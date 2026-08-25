use std::{
    collections::{HashMap, hash_map::DefaultHasher},
    hash::{Hash, Hasher},
    path::PathBuf,
    process::Command,
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

const RESOLVER_SOURCE: &str = include_str!("../../../tools/go_resolve/main.go");

#[derive(Debug, Clone, serde::Deserialize, PartialEq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RawType {
    Basic { basic: String },
    Named { r#ref: String },
    Pointer { elem: Box<RawType> },
    Slice { elem: Box<RawType> },
    Array { elem: Box<RawType>, len: i64 },
    Map { key: Box<RawType>, value: Box<RawType> },
    Struct { #[serde(default)] fields: Vec<RawField> },
    Interface { #[serde(default)] methods: Vec<RawMethod> },
    Any,
    Chan { elem: Box<RawType>, dir: String },
    Func { signature: Box<RawFuncType> },
    TypeParam { name: String, #[serde(default)] constraint: String },
    Unsupported,
}

#[derive(Debug, Clone, serde::Deserialize, PartialEq)]
pub struct RawField {
    pub name: String,
    #[serde(rename = "type")]
    pub type_: RawType,
    pub exported: bool,
    pub embedded: bool,
    #[serde(default)]
    pub tag: String,
}

#[derive(Debug, Clone, serde::Deserialize, PartialEq)]
pub struct RawMethod {
    pub name: String,
    #[serde(rename = "type")]
    pub type_: RawFuncType,
}

#[derive(Debug, Clone, serde::Deserialize, PartialEq)]
pub struct RawTypeParam {
    pub name: String,
    #[serde(default)]
    pub constraint: String,
}

#[derive(Debug, Clone, serde::Deserialize, PartialEq)]
pub struct RawFuncType {
    #[serde(default)]
    pub type_params: Vec<RawTypeParam>,
    pub variadic: bool,
    pub params: Vec<RawType>,
    pub results: Vec<RawType>,
}

#[derive(serde::Deserialize)]
struct RawOutput {
    #[allow(dead_code)]
    package: String,
    types: HashMap<String, RawType>,
    functions: HashMap<String, RawFuncType>,
}

struct PackageInfo {
    functions: HashMap<String, RawFuncType>,
    types: Rc<HashMap<String, RawType>>,
}

pub struct GoResolver {
    resolver: Option<Result<PathBuf, String>>,
    cache: HashMap<String, Result<PackageInfo, String>>,
}

impl GoResolver {
    pub fn new() -> Self {
        Self {
            resolver: None,
            cache: HashMap::new()
        }
    }

    pub fn lookup(&mut self, package: &str, name: &str) -> Result<&RawFuncType, String> {
        self.ensure_loaded(package);

        match self.cache.get(package).expect("just inserted") {
            Ok(info) => info.functions.get(name).ok_or_else(|| {
                format!("no exported function `{name}` found in package `{package}`")
            }),
            Err(reason) => Err(reason.clone()),
        }
    }

    pub fn types_of(&mut self, package: &str) -> Result<Rc<HashMap<String, RawType>>, String> {
        self.ensure_loaded(package);

        match self.cache.get(package).expect("just inserted") {
            Ok(info) => Ok(Rc::clone(&info.types)),
            Err(reason) => Err(reason.clone()),
        }
    }

    fn ensure_loaded(&mut self, package: &str) {
        if !self.cache.contains_key(package) {
            let loaded = self.load_package(package);
            self.cache.insert(package.to_string(), loaded);
        }
    }

    fn load_package(&mut self, package: &str) -> Result<PackageInfo, String> {
        let helper = self.ensure_go_resolver()?;

        let output = Command::new(&helper)
            .arg(package)
            .output()
            .map_err(|err| format!("failed to run go_resolve helper: {err}"))?;

        if !output.status.success() {
            return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
        }

        let raw: RawOutput = serde_json::from_slice(&output.stdout)
            .map_err(|err| format!("failed to parse go_resolve output: {err}"))?;

        Ok(PackageInfo { functions: raw.functions, types: Rc::new(raw.types) })
    }

    fn ensure_go_resolver(&mut self) -> Result<PathBuf, String> {
        if let Some(result) = &self.resolver {
            return result.clone();
        }

        let resolver = build_go_resolver();
        self.resolver = Some(resolver.clone());
        resolver
    }
}

// todo: store in duck dir and use provided go version
fn build_go_resolver() -> Result<PathBuf, String> {
    let mut hasher = DefaultHasher::new();
    RESOLVER_SOURCE.hash(&mut hasher);

    let hash = hasher.finish();

    let mut target = std::env::temp_dir();
    target.push(format!("duck-go-resolve-{hash:x}"));

    if target.exists() {
        return Ok(target);
    }

    static UNIQUE: AtomicU64 = AtomicU64::new(0);
    let unique = format!("{}-{}", std::process::id(), UNIQUE.fetch_add(1, Ordering::Relaxed));

    let mut source_path = std::env::temp_dir();
    source_path.push(format!("duckc-go-resolve-{hash:x}-{unique}.go"));
    std::fs::write(&source_path, RESOLVER_SOURCE)
        .map_err(|err| format!("failed to write go_resolve source file: {err}"))?;

    let mut tmp_target = std::env::temp_dir();
    tmp_target.push(format!("duck-go-resolve-{hash:x}-{unique}.tmp"));

    let output = Command::new(crate::driver::resolve_go_binary())
        .arg("build")
        .arg("-o")
        .arg(&tmp_target)
        .arg(&source_path)
        .output();

    let _ = std::fs::remove_file(&source_path);

    let output = match output {
        Ok(output) => output,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
            return Err("go not found".to_string());
        }
        Err(err) => return Err(format!("failed to run `go build`: {err}")),
    };

    if !output.status.success() {
        let _ = std::fs::remove_file(&tmp_target);
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
    }

    match std::fs::rename(&tmp_target, &target) {
        Ok(()) => Ok(target),
        Err(_) if target.exists() => {
            let _ = std::fs::remove_file(&tmp_target);
            Ok(target)
        }
        Err(err) => Err(format!("failed to install go_resolve helper binary: {err}")),
    }
}
