use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

const MODULE_REGISTRY: &str = r#"var __duck_modules = {};
var __duck_loaded = {};
function __duck_define(name, factory) { __duck_modules[name] = factory; }
function __duck_require(name) {
  if (__duck_loaded[name] !== undefined) return __duck_loaded[name];
  var m = { exports: {} };
  __duck_loaded[name] = m.exports;
  if (!__duck_modules[name]) return {};
  __duck_modules[name](m, m.exports, __duck_require);
  __duck_loaded[name] = m.exports;
  return m.exports;
}"#;

pub struct BundleInput {
    /// (pkg_name, binding_name) pairs from `use ts "pkg"` declarations.
    pub ts_packages: Vec<(String, String)>,
    /// Compiled Duck client JS source.
    pub client_js: String,
    /// Names of all `client fn` functions (used to build island map).
    pub client_fn_names: Vec<String>,
    /// Path to node_modules to search for CJS dists. If None, auto-detected.
    pub node_modules: Option<PathBuf>,
}

/// Bundle the client code + npm CJS packages into a single self-contained JS file.
pub fn bundle(input: BundleInput) -> String {
    let node_modules = input.node_modules.or_else(find_node_modules);
    let mut out = String::new();

    out.push_str(MODULE_REGISTRY);
    out.push_str("\n\n");

    let mut inlined: HashSet<String> = HashSet::new();
    for (pkg_name, binding) in &input.ts_packages {
        if inlined.contains(pkg_name) {
            continue;
        }
        inlined.insert(pkg_name.clone());

        if let Some(nm) = &node_modules {
            match find_cjs_dist(pkg_name, nm) {
                Some(cjs_path) => match fs::read_to_string(&cjs_path) {
                    Ok(cjs) => {
                        out.push_str(&format!(
                            "__duck_define({pkg_name:?}, function(module, exports, require) {{\n{cjs}\n}});\n"
                        ));
                        out.push_str(&format!(
                            "var {binding} = __duck_require({pkg_name:?});\n\n"
                        ));
                    }
                    Err(_) => {
                        out.push_str(&format!(
                            "// WARNING: could not read CJS dist for {pkg_name}\nvar {binding} = {{}};\n\n"
                        ));
                    }
                },
                None => {
                    out.push_str(&format!(
                        "// WARNING: no CJS dist found for {pkg_name}\nvar {binding} = {{}};\n\n"
                    ));
                }
            }
        } else {
            out.push_str(&format!(
                "// WARNING: node_modules not found — {pkg_name} unavailable\nvar {binding} = {{}};\n\n"
            ));
        }
    }

    // Expose Preact's h for JSX transforms (generated h() calls need it in scope).
    if input.ts_packages.iter().any(|(pkg, _)| pkg == "preact") {
        out.push_str("var h = preact.h;\n\n");
    }

    out.push_str(&input.client_js);
    out.push('\n');

    if !input.client_fn_names.is_empty() {
        let island_map = input
            .client_fn_names
            .iter()
            .map(|n| format!("{n:?}: {n}"))
            .collect::<Vec<_>>()
            .join(", ");

        out.push_str(&format!(
            r#"
(function() {{
  var __duck_islands = {{{island_map}}};
  function __duck_hydrate() {{
    document.querySelectorAll('[data-duck-island]').forEach(function(el) {{
      var name = el.getAttribute('data-duck-island');
      var comp = __duck_islands[name];
      if (!comp) return;
      var propsStr = el.getAttribute('data-duck-props');
      var props = propsStr ? JSON.parse(propsStr) : {{}};
      var preactMod = __duck_require('preact');
      preactMod.render(preactMod.h(comp, props), el);
    }});
  }}
  if (document.readyState === 'loading') {{
    document.addEventListener('DOMContentLoaded', __duck_hydrate);
  }} else {{
    __duck_hydrate();
  }}
}})();
"#
        ));
    }

    out
}

/// Walk upward from cwd looking for a node_modules directory.
fn find_node_modules() -> Option<PathBuf> {
    let cwd = std::env::current_dir().ok()?;
    let mut dir: &Path = cwd.as_path();
    loop {
        let nm = dir.join("node_modules");
        if nm.is_dir() {
            return Some(nm);
        }
        dir = dir.parent()?;
    }
}

/// Find the main CJS dist file for an npm package.
///
/// For a package like "preact/hooks" the package dir is node_modules/preact/hooks/.
fn find_cjs_dist(pkg_name: &str, node_modules: &Path) -> Option<PathBuf> {
    let pkg_dir = node_modules.join(pkg_name);
    let pkg_json_path = pkg_dir.join("package.json");
    let pkg_json = fs::read_to_string(&pkg_json_path).ok()?;
    let json: serde_json::Value = serde_json::from_str(&pkg_json).ok()?;

    // Try "main" field first.
    if let Some(main) = json.get("main").and_then(|v| v.as_str()) {
        let dist = pkg_dir.join(main);
        if dist.exists() {
            return Some(dist);
        }
    }

    // Try "exports" -> "." -> "require" or "default".
    if let Some(exports) = json.get("exports") {
        let dot = exports.get(".").unwrap_or(exports);
        let candidates = [
            dot.get("require").and_then(|v| v.as_str()),
            dot.get("default").and_then(|v| v.as_str()),
        ];
        for candidate in candidates.into_iter().flatten() {
            let dist = pkg_dir.join(candidate);
            if dist.exists() {
                return Some(dist);
            }
        }
    }

    None
}
