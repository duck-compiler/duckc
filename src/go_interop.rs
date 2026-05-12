use std::{
    collections::HashMap,
    env, fs,
    path::PathBuf,
    process::Command,
    sync::{Arc, Mutex, OnceLock},
};

use serde::{Deserialize, Deserializer};

// JSON mirror of extractor output

// Handles both absent fields and explicit JSON `null` values (serde `default` alone
// only covers absent fields; null causes a deserialization error for Vec<T>).
fn null_as_default<'de, D, T>(d: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: Default + Deserialize<'de>,
{
    Option::<T>::deserialize(d).map(|o| o.unwrap_or_default())
}

#[derive(Debug, Clone, Deserialize)]
struct ParamJSON {
    #[serde(default)]
    name: String,
    #[serde(rename = "type")]
    type_str: String,
    #[serde(default)]
    variadic: bool,
}

#[derive(Debug, Clone, Deserialize)]
struct FuncJSON {
    name: String,
    #[serde(default, deserialize_with = "null_as_default")]
    params: Vec<ParamJSON>,
    #[serde(default, deserialize_with = "null_as_default")]
    results: Vec<ParamJSON>,
}

#[derive(Debug, Clone, Deserialize)]
struct FieldJSON {
    name: String,
    #[serde(rename = "type")]
    type_str: String,
}

#[derive(Debug, Clone, Deserialize)]
struct TypeJSON {
    name: String,
    kind: String,
    #[serde(default, deserialize_with = "null_as_default")]
    fields: Vec<FieldJSON>,
    #[serde(default, deserialize_with = "null_as_default")]
    methods: Vec<FuncJSON>,
}

#[derive(Debug, Clone, Deserialize)]
struct VarJSON {
    name: String,
    #[serde(rename = "type")]
    type_str: String,
}

#[derive(Debug, Clone, Deserialize)]
struct PackageJSON {
    import_path: String,
    #[serde(default, deserialize_with = "null_as_default")]
    functions: Vec<FuncJSON>,
    #[serde(default, deserialize_with = "null_as_default")]
    types: Vec<TypeJSON>,
    #[serde(default, deserialize_with = "null_as_default")]
    vars: Vec<VarJSON>,
    #[serde(default, deserialize_with = "null_as_default")]
    consts: Vec<VarJSON>,
}

// Public types

#[derive(Debug, Clone)]
pub struct GoParam {
    pub name: String,
    pub type_str: String,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub struct GoFuncInfo {
    pub params: Vec<GoParam>,
    /// Return type strings (empty = void, 1 = single return, 2+ = multi-return).
    pub results: Vec<String>,
}

impl GoFuncInfo {
    /// Number of required (non-variadic) parameters.
    pub fn min_args(&self) -> usize {
        if self.params.last().map(|p| p.variadic).unwrap_or(false) {
            self.params.len().saturating_sub(1)
        } else {
            self.params.len()
        }
    }

    pub fn is_variadic(&self) -> bool {
        self.params.last().map(|p| p.variadic).unwrap_or(false)
    }
}

#[derive(Debug, Clone)]
pub struct GoFieldInfo {
    pub type_str: String,
}

#[derive(Debug, Clone)]
pub struct GoTypeInfo {
    pub kind: String, // "struct", "interface", "alias", "other"
    pub fields: HashMap<String, GoFieldInfo>,
    pub methods: HashMap<String, GoFuncInfo>,
}

#[derive(Debug, Clone)]
pub struct GoPackageData {
    pub import_path: String,
    pub short_name: String,
    pub funcs: HashMap<String, GoFuncInfo>,
    pub types: HashMap<String, GoTypeInfo>,
    /// Package-level variables: name -> Go type string.
    pub vars: HashMap<String, String>,
    /// Package-level constants: name -> Go type string.
    pub consts: HashMap<String, String>,
}

impl GoPackageData {
    fn from_json(pkg: PackageJSON) -> Self {
        let short_name = pkg
            .import_path
            .split('/')
            .next_back()
            .unwrap_or(&pkg.import_path)
            .to_string();

        let funcs = pkg
            .functions
            .into_iter()
            .map(|f| {
                let info = GoFuncInfo {
                    params: f
                        .params
                        .into_iter()
                        .map(|p| GoParam {
                            name: p.name,
                            type_str: p.type_str,
                            variadic: p.variadic,
                        })
                        .collect(),
                    results: f.results.into_iter().map(|r| r.type_str).collect(),
                };
                (f.name, info)
            })
            .collect();

        let types = pkg
            .types
            .into_iter()
            .map(|t| {
                let fields = t
                    .fields
                    .into_iter()
                    .map(|f| {
                        (
                            f.name,
                            GoFieldInfo {
                                type_str: f.type_str,
                            },
                        )
                    })
                    .collect();
                let methods = t
                    .methods
                    .into_iter()
                    .map(|m| {
                        let info = GoFuncInfo {
                            params: m
                                .params
                                .into_iter()
                                .map(|p| GoParam {
                                    name: p.name,
                                    type_str: p.type_str,
                                    variadic: p.variadic,
                                })
                                .collect(),
                            results: m.results.into_iter().map(|r| r.type_str).collect(),
                        };
                        (m.name, info)
                    })
                    .collect();
                (
                    t.name,
                    GoTypeInfo {
                        kind: t.kind,
                        fields,
                        methods,
                    },
                )
            })
            .collect();

        let vars = pkg.vars.into_iter().map(|v| (v.name, v.type_str)).collect();
        let consts = pkg
            .consts
            .into_iter()
            .map(|c| (c.name, c.type_str))
            .collect();

        GoPackageData {
            import_path: pkg.import_path,
            short_name,
            funcs,
            types,
            vars,
            consts,
        }
    }

    /// Look up any exported member by name: function, var, or const.
    pub fn lookup_member(&self, name: &str) -> GoMember<'_> {
        if let Some(f) = self.funcs.get(name) {
            return GoMember::Func(f);
        }
        if let Some(ty) = self.vars.get(name).or_else(|| self.consts.get(name)) {
            return GoMember::Var(ty.as_str());
        }
        if self.types.contains_key(name) {
            return GoMember::Type;
        }
        GoMember::Unknown
    }
}

pub enum GoMember<'a> {
    Func(&'a GoFuncInfo),
    Var(&'a str),
    Type,
    Unknown,
}

// Global caches

static PKG_CACHE: OnceLock<Mutex<HashMap<String, Arc<GoPackageData>>>> = OnceLock::new();
/// Maps short package name -> full import path (e.g. "http" -> "net/http").
static PKG_REGISTRY: OnceLock<Mutex<HashMap<String, String>>> = OnceLock::new();

fn pkg_cache() -> &'static Mutex<HashMap<String, Arc<GoPackageData>>> {
    PKG_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn pkg_registry() -> &'static Mutex<HashMap<String, String>> {
    PKG_REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Resolve a short package name (e.g. "http") to its full import path ("net/http").
/// Falls back to treating the short name as the full path (works for single-segment stdlib).
pub fn resolve_short_name(short: &str) -> String {
    let reg = pkg_registry().lock().unwrap();
    reg.get(short).cloned().unwrap_or_else(|| short.to_string())
}

fn register_pkg(data: &GoPackageData) {
    let mut reg = pkg_registry().lock().unwrap();
    reg.insert(data.short_name.clone(), data.import_path.clone());
}

// Extractor binary management

const EXTRACTOR_SRC: &str = include_str!("../tools/go-type-info/main.go");
/// Bump this when the source changes to force a rebuild.
const EXTRACTOR_VERSION: &str = "3";

fn duck_dir() -> Option<PathBuf> {
    let home = env::var_os("HOME").or_else(|| env::var_os("USERPROFILE"))?;
    let mut p = PathBuf::from(home);
    p.push(".duck");
    Some(p)
}

fn goroot() -> Option<PathBuf> {
    if let Ok(r) = env::var("GOROOT") {
        let p = PathBuf::from(r);
        if p.exists() {
            return Some(p);
        }
    }
    let mut p = duck_dir()?;
    p.push("go-compiler");
    if p.exists() { Some(p) } else { None }
}

fn go_binary() -> Option<PathBuf> {
    let mut p = goroot()?;
    p.push("bin");
    if cfg!(windows) {
        p.push("go.exe")
    } else {
        p.push("go")
    }
    if p.exists() { Some(p) } else { None }
}

fn extractor_path() -> Option<PathBuf> {
    let mut p = duck_dir()?;
    p.push("bin");
    if cfg!(windows) {
        p.push("go-type-info.exe")
    } else {
        p.push("go-type-info")
    }
    Some(p)
}

fn type_cache_path(import_path: &str) -> Option<PathBuf> {
    let mut p = duck_dir()?;
    p.push("type-cache");
    p.push(format!("{}.json", import_path.replace('/', "__")));
    Some(p)
}

/// Build the extractor binary if it doesn't exist or is out-of-date.
/// Returns the path to the binary, or `None` if Go toolchain is unavailable.
pub fn ensure_extractor() -> Option<PathBuf> {
    let ext_path = extractor_path()?;
    let go = go_binary()?;

    // Version stamp lives next to the binary.
    let stamp = ext_path.with_extension("ver");
    let current = ext_path.exists()
        && stamp.exists()
        && fs::read_to_string(&stamp).unwrap_or_default().trim() == EXTRACTOR_VERSION;
    if current {
        return Some(ext_path);
    }

    // Write source + go.mod to a temp build directory.
    let tmp = env::temp_dir().join("duckc-type-extractor-build");
    fs::create_dir_all(&tmp).ok()?;
    fs::write(tmp.join("main.go"), EXTRACTOR_SRC).ok()?;
    fs::write(tmp.join("go.mod"), "module duckc/go-type-info\n\ngo 1.21\n").ok()?;

    if let Some(parent) = ext_path.parent() {
        fs::create_dir_all(parent).ok()?;
    }

    let status = Command::new(&go)
        .args(["build", "-o"])
        .arg(&ext_path)
        .arg(".")
        .current_dir(&tmp)
        .env("GOROOT", goroot().unwrap())
        .output()
        .ok()?;

    if !status.status.success() {
        return None;
    }

    fs::write(&stamp, EXTRACTOR_VERSION).ok();
    Some(ext_path)
}

// Public entry point

/// Scan a Go package and return its full type information.
/// Tries memory cache -> disk cache -> extractor binary, in that order.
pub fn scan_package(import_path: &str) -> Option<Arc<GoPackageData>> {
    // Memory cache hit.
    {
        let c = pkg_cache().lock().unwrap();
        if let Some(d) = c.get(import_path) {
            return Some(d.clone());
        }
    }

    // Disk cache hit.
    if let Some(path) = type_cache_path(import_path) {
        if let Ok(json) = fs::read_to_string(&path) {
            if let Ok(pkg_json) = serde_json::from_str::<PackageJSON>(&json) {
                let data = Arc::new(GoPackageData::from_json(pkg_json));
                register_pkg(&data);
                pkg_cache()
                    .lock()
                    .unwrap()
                    .insert(import_path.to_string(), data.clone());
                return Some(data);
            }
        }
    }

    // Run extractor binary.
    let ext = ensure_extractor()?;
    let output = Command::new(&ext)
        .arg(import_path)
        .env("GOROOT", goroot()?)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let json_str = std::str::from_utf8(&output.stdout).ok()?;
    let pkg_json: PackageJSON = serde_json::from_str(json_str).ok()?;
    let data = Arc::new(GoPackageData::from_json(pkg_json));
    register_pkg(&data);

    // Persist to disk cache.
    if let Some(path) = type_cache_path(import_path) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).ok();
        }
        fs::write(&path, json_str).ok();
    }

    pkg_cache()
        .lock()
        .unwrap()
        .insert(import_path.to_string(), data.clone());
    Some(data)
}

/// Scan an external Go package using a specific working directory for module resolution.
fn scan_package_with_dir(
    import_path: &str,
    working_dir: &std::path::Path,
) -> Option<Arc<GoPackageData>> {
    // Memory cache hit.
    {
        let c = pkg_cache().lock().unwrap();
        if let Some(d) = c.get(import_path) {
            return Some(d.clone());
        }
    }

    // Disk cache hit.
    if let Some(path) = type_cache_path(import_path) {
        if let Ok(json) = fs::read_to_string(&path) {
            if let Ok(pkg_json) = serde_json::from_str::<PackageJSON>(&json) {
                let data = Arc::new(GoPackageData::from_json(pkg_json));
                register_pkg(&data);
                pkg_cache()
                    .lock()
                    .unwrap()
                    .insert(import_path.to_string(), data.clone());
                return Some(data);
            }
        }
    }

    // Run extractor binary with the working directory as second argument.
    let ext = ensure_extractor()?;
    let output = Command::new(&ext)
        .arg(import_path)
        .arg(working_dir)
        .env("GOROOT", goroot()?)
        .output()
        .ok()?;

    if !output.status.success() {
        eprintln!(
            "[go-type-info] failed for {}: {}",
            import_path,
            std::str::from_utf8(&output.stderr).unwrap_or("?")
        );
        return None;
    }

    let json_str = std::str::from_utf8(&output.stdout).ok()?;
    let pkg_json: PackageJSON = serde_json::from_str(json_str).ok()?;
    let data = Arc::new(GoPackageData::from_json(pkg_json));
    register_pkg(&data);

    // Persist to disk cache.
    if let Some(path) = type_cache_path(import_path) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).ok();
        }
        fs::write(&path, json_str).ok();
    }

    pkg_cache()
        .lock()
        .unwrap()
        .insert(import_path.to_string(), data.clone());
    Some(data)
}

/// Download and scan external Go packages into the type cache. Initializes go.mod if missing.
pub fn prepare_external_packages(import_paths: &[String], working_dir: &std::path::Path) {
    if import_paths.is_empty() {
        return;
    }
    let Some(go) = go_binary() else { return };
    let Some(goroot_path) = goroot() else { return };

    // Ensure go.mod exists in the working directory.
    if !working_dir.join("go.mod").exists() {
        let _ = Command::new(&go)
            .args(["mod", "init", "duck_out"])
            .current_dir(working_dir)
            .env("GOROOT", &goroot_path)
            .output();
    }

    for pkg in import_paths {
        // Skip if already in cache (memory or disk).
        {
            let c = pkg_cache().lock().unwrap();
            if c.contains_key(pkg.as_str()) {
                continue;
            }
        }
        if let Some(p) = type_cache_path(pkg) {
            if p.exists() {
                // Load from disk cache - if it works, no extraction needed.
                if scan_package(pkg).is_some() {
                    continue;
                }
            }
        }

        // Download the package into the module cache.
        let _ = Command::new(&go)
            .args(["get", pkg.as_str()])
            .current_dir(working_dir)
            .env("GOROOT", &goroot_path)
            .output();

        // Extract and cache type information.
        scan_package_with_dir(pkg, working_dir);
    }
}
