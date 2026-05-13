use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use crate::parser2::{
    parser::{
        Expr, ExprKind, FunctionDecl, Item, Parsed, SourceFile, Span, StructDecl, TypeAliasDecl,
        TypeDescription, UseDecl, WithSpan,
    },
    tokenizer::tokenize_no_comments,
};

/// Items from one loaded module, with names already mangled (e.g. fn println -> fn io__println).
pub struct LoadedModule {
    pub items: Vec<Item<Parsed>>,
    /// Short member name -> index into `items`.
    /// Also contains composite keys like `"Opt__some"` for static methods.
    pub members: HashMap<String, usize>,
    /// Go import paths pulled from `use go "..."` lines inside the module files.
    pub go_imports: Vec<String>,
}

/// Holds all modules loaded for a single compilation.
pub struct ModuleStore {
    /// Key is the canonical path joined with "::", e.g. "std::io".
    pub modules: HashMap<String, LoadedModule>,
    /// file_id -> (display name, source content) for every loaded std file.
    pub sources: HashMap<u16, (String, String)>,
    std_root: PathBuf,
    /// Next file_id to allocate; 0=unused, 1=user file, 2+ are std files.
    next_file_id: u16,
}

impl ModuleStore {
    pub fn new(std_root: PathBuf) -> Self {
        Self {
            modules: HashMap::new(),
            sources: HashMap::new(),
            std_root,
            next_file_id: 2,
        }
    }

    fn alloc_file_id(&mut self) -> u16 {
        let id = self.next_file_id;
        self.next_file_id += 1;
        id
    }

    /// Scan `items` for UseDecl::Duck entries and direct module path expressions,
    /// load all referenced modules into the store.
    /// Returns parse/load error strings (non-fatal - caller decides what to do with them).
    pub fn load_needed(&mut self, items: &[Item<Parsed>]) -> Vec<String> {
        let mut to_load: Vec<(Vec<String>, Option<String>)> = Vec::new();

        for item in items {
            match item {
                Item::Use(UseDecl::Duck(segs, _, _)) => {
                    if segs.len() < 2 {
                        continue;
                    }
                    let path: Vec<String> = segs.iter().map(|s| s.value.clone()).collect();
                    let (mod_path, member) = self.split_path(&path);
                    if !mod_path.is_empty() {
                        to_load.push((mod_path, member));
                    }
                }
                Item::Function(f) => {
                    // scan body for direct module-path expressions like std::io::println(...)
                    let mut paths: Vec<Vec<String>> = Vec::new();
                    collect_scope_res_paths(&f.body, &mut paths);
                    for path in paths {
                        let (mod_path, member) = self.split_path(&path);
                        if !mod_path.is_empty() {
                            to_load.push((mod_path, member));
                        }
                    }
                }
                _ => {}
            }
        }

        to_load.sort_by(|a, b| a.0.cmp(&b.0));
        to_load.dedup_by(|a, b| a.0 == b.0);

        let mut errors = Vec::new();
        for (mod_path, _member) in to_load {
            let key = mod_path.join("::");
            if self.modules.contains_key(&key) {
                continue;
            }
            // load the full module so intra-module cross-file refs resolve
            if let Err(e) = self.load_one(&mod_path, None) {
                errors.push(e);
            }
        }
        errors
    }

    /// Returns all mangled item names across all loaded modules.
    /// Used by the resolver to identify which DefIds are module-origin.
    pub fn all_mangled_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        for module in self.modules.values() {
            for item in &module.items {
                if let Some(n) = item_name(item) {
                    names.insert(n);
                }
            }
        }
        names
    }

    /// Given a UseDecl::Duck path, return (module_key, Option<member_name>).
    /// If the full path is a module dir, member is None (namespace import).
    /// If all-but-last is a module dir, last is the member name (direct import).
    pub fn split_path(&self, path: &[String]) -> (Vec<String>, Option<String>) {
        if path.len() < 2 {
            return (vec![], None);
        }

        // a bare file (e.g. std/error/panic.duck) is a module member, not a sub-module dir
        if self.path_is_dir(path) {
            return (path.to_vec(), None);
        }

        // Try progressively shorter prefixes as the module path, longest first.
        // This also handles brace-expansion paths like ["std","opt","Opt","Some"]
        // where the module is ["std","opt"] and the member(s) follow.
        for prefix_len in (2..path.len()).rev() {
            let mod_part = &path[..prefix_len];
            if self.path_exists(mod_part) {
                // Join remaining segments with "__" (handles multi-member brace imports).
                let member = path[prefix_len..].join("__");
                return (mod_part.to_vec(), Some(member));
            }
        }

        (vec![], None)
    }

    /// Returns true if `path` corresponds to a directory (module namespace).
    fn path_is_dir(&self, path: &[String]) -> bool {
        let rel: PathBuf = path[1..].iter().collect();
        let dir = self.std_root.join(&rel);
        self.path_entry_exists(&dir) && dir.is_dir()
    }

    fn path_exists(&self, path: &[String]) -> bool {
        // path[0] is the namespace root (e.g. "std"), rest is the sub-path.
        let rel: PathBuf = path[1..].iter().collect();
        let dir = self.std_root.join(&rel);
        let file = self.std_root.join(rel.with_extension("duck"));
        // Use exact-case directory listing to avoid Windows case-insensitive false positives.
        // On Windows, Path::is_file/is_dir match case-insensitively, so "Opt.duck" would
        // match "opt.duck". We compare file_name bytes directly instead.
        self.path_entry_exists(&dir) || self.path_entry_exists(&file)
    }

    fn path_entry_exists(&self, full_path: &Path) -> bool {
        let parent = match full_path.parent() {
            Some(p) if !p.as_os_str().is_empty() => p,
            _ => return full_path.exists(),
        };
        let name = match full_path.file_name() {
            Some(n) => n,
            None => return false,
        };
        std::fs::read_dir(parent)
            .map(|entries| entries.flatten().any(|e| e.file_name() == name))
            .unwrap_or(false)
    }

    fn load_one(&mut self, path: &[String], member_filter: Option<&str>) -> Result<(), String> {
        let rel: PathBuf = path[1..].iter().collect();
        let dir = self.std_root.join(&rel);
        let single = self.std_root.join(format!("{}.duck", rel.display()));

        let sources: Vec<(PathBuf, String)> = if dir.is_dir() {
            read_dir_duck_files(&dir)?
        } else if single.is_file() {
            let src = fs::read_to_string(&single)
                .map_err(|e| format!("cannot read {}: {}", single.display(), e))?;
            vec![(single, src)]
        } else {
            return Err(format!("module {} not found", path.join("::")));
        };

        // Prefix for name mangling: "io" for std::io, "col" for std::col, etc.
        let prefix = path[1..].join("__");

        let mut items: Vec<Item<Parsed>> = Vec::new();
        let mut members: HashMap<String, usize> = HashMap::new();
        let mut go_imports: Vec<String> = Vec::new();
        // Transitive dep paths found in UseDecl items of this module's source files.
        let mut transitive_deps: Vec<Vec<String>> = Vec::new();

        for (file_path, src) in &sources {
            let fid = self.alloc_file_id();
            self.sources
                .insert(fid, (file_path.display().to_string(), src.clone()));

            let (tokens, lex_errors) = tokenize_no_comments(src, fid);
            if !lex_errors.is_empty() {
                continue;
            }
            let (sf, _parse_errors) = crate::parser2::parser::parse(tokens, fid);

            // Collect transitive dependencies declared via `use` in this std file.
            for dep_item in &sf.items {
                if let Item::Use(UseDecl::Duck(segs, is_global, _)) = dep_item {
                    if segs.len() >= 2 {
                        let raw: Vec<String> = segs.iter().map(|s| s.value.clone()).collect();
                        // Try as-is first (already starts with "std").
                        let (mod_path, _) = self.split_path(&raw);
                        if !mod_path.is_empty() {
                            transitive_deps.push(mod_path);
                        } else if *is_global || raw[0] != "std" {
                            // Global `::opt::Opt` means std-root-relative; prepend "std".
                            let std_path: Vec<String> = std::iter::once("std".to_string())
                                .chain(raw.into_iter())
                                .collect();
                            let (mod_path, _) = self.split_path(&std_path);
                            if !mod_path.is_empty() {
                                transitive_deps.push(mod_path);
                            }
                        }
                    }
                }
            }

            // Also scan function and extension-method bodies for direct module-path
            // expressions (e.g. `std::error::unreachable(...)` in iter.duck bodies).
            for dep_item in &sf.items {
                let bodies: Vec<&Expr<Parsed>> = match dep_item {
                    Item::Function(f) => vec![&f.body],
                    Item::Extension(e) => e.methods.iter().map(|m| &m.body).collect(),
                    _ => vec![],
                };
                for body in bodies {
                    let mut paths: Vec<Vec<String>> = Vec::new();
                    collect_scope_res_paths(body, &mut paths);
                    for path in paths {
                        let (mod_path, _) = self.split_path(&path);
                        if !mod_path.is_empty() {
                            transitive_deps.push(mod_path);
                        }
                    }
                }
            }

            // When loading a specific member, only include files that contain it.
            if let Some(filter) = member_filter {
                let has_member = sf
                    .items
                    .iter()
                    .any(|item| item_name(item).as_deref() == Some(filter));
                if !has_member {
                    continue;
                }
            }

            absorb_items(sf, &prefix, &mut items, &mut members, &mut go_imports);
        }

        // Add self-import use declarations for every member of this module so that
        // within the module's own Extension methods, short names (e.g. `Opt`) resolve
        // to their mangled counterparts (e.g. `opt__Opt`).
        // These are appended AFTER all struct/function items so collect_top_level's
        // second pass sees them after all definitions are already registered.
        {
            let path_segs: Vec<WithSpan<String>> = path
                .iter()
                .map(|s| WithSpan::new(s.clone(), Span::dummy()))
                .collect();
            // collect short member names first to avoid borrowing members while mutating items
            let short_names: Vec<String> = members
                .keys()
                .filter(|k| !k.contains("__")) // skip composite "Opt__some" keys
                .cloned()
                .collect();
            for short_name in short_names {
                let mut import_segs = path_segs.clone();
                import_segs.push(WithSpan::new(short_name, Span::dummy()));
                items.push(Item::Use(UseDecl::Duck(import_segs, false, Span::dummy())));
            }
        }

        let key = path.join("::");
        // Insert the current module before loading transitive deps to prevent re-entry.
        self.modules.insert(
            key,
            LoadedModule {
                items,
                members,
                go_imports,
            },
        );

        for dep_path in transitive_deps {
            let dep_key = dep_path.join("::");
            if !self.modules.contains_key(&dep_key) {
                let _ = self.load_one(&dep_path, None);
            }
        }

        Ok(())
    }
}

/// Consume a parsed SourceFile, mangling item names with `prefix__` and appending to `out_*`.
fn absorb_items(
    sf: SourceFile<Parsed>,
    prefix: &str,
    out_items: &mut Vec<Item<Parsed>>,
    out_members: &mut HashMap<String, usize>,
    out_go_imports: &mut Vec<String>,
) {
    for item in sf.items {
        match item {
            Item::Use(UseDecl::Go(path_segs, _span)) => {
                if let Some(p) = path_segs.first() {
                    out_go_imports.push(p.value.clone());
                }
            }
            Item::Use(UseDecl::Duck(segs, is_global, span)) => {
                // Normalize global (leading `::`) paths from std files by prepending "std".
                // prepend "std" so the resolver can create proper name bindings
                let normalized: Vec<WithSpan<String>> =
                    if is_global && segs.first().map_or(true, |s| s.value != "std") {
                        let std_seg = WithSpan::new("std".to_string(), span);
                        std::iter::once(std_seg).chain(segs.into_iter()).collect()
                    } else {
                        segs
                    };
                out_items.push(Item::Use(UseDecl::Duck(normalized, false, span)));
            }
            Item::Use(_) => {}
            Item::Function(f) => {
                let short = f.name.value.clone();
                let mangled = format!("{}__{}", prefix, short);
                let idx = out_items.len();
                out_members.insert(short, idx);
                out_items.push(Item::Function(FunctionDecl {
                    name: WithSpan::new(mangled, f.name.span),
                    ..f
                }));
            }
            Item::Struct(s) => {
                let short = s.name.value.clone();
                let mangled = format!("{}__{}", prefix, short);
                let idx = out_items.len();
                out_members.insert(short, idx);
                out_items.push(Item::Struct(StructDecl {
                    name: WithSpan::new(mangled, s.name.span),
                    ..s
                }));
            }
            Item::TypeAlias(a) => {
                let short = a.name.value.clone();
                let mangled = format!("{}__{}", prefix, short);
                let idx = out_items.len();
                out_members.insert(short, idx);
                out_items.push(Item::TypeAlias(TypeAliasDecl {
                    name: WithSpan::new(mangled, a.name.span),
                    ..a
                }));
            }
            Item::Extension(e) => {
                // Register each `static fn` as a free FunctionDecl so they are resolvable
                // via 2-segment `TypeName::method` syntax (e.g. `Opt::some`, `ArrayList::new`).
                if let TypeDescription::TypeName { ref type_ref, .. } = e.target.desc {
                    if let Some(struct_short) = type_ref.path.last().cloned() {
                        for m in &e.methods {
                            if m.is_static {
                                let composite_key = format!("{}__{}", struct_short, m.name.value);
                                let mangled_name =
                                    format!("{}__{}__{}", prefix, struct_short, m.name.value);
                                let idx = out_items.len();
                                out_members.insert(composite_key, idx);
                                out_items.push(Item::Function(FunctionDecl {
                                    name: WithSpan::new(mangled_name, m.name.span),
                                    ..m.clone()
                                }));
                            }
                        }
                    }
                }
                out_items.push(Item::Extension(e));
            }
        }
    }
}

fn try_scope_res_path(expr: &Expr<Parsed>) -> Option<Vec<String>> {
    match &expr.kind {
        ExprKind::ScopeRes { base, member } => {
            let mut path = try_scope_res_path(base)?;
            path.push(member.value.clone());
            Some(path)
        }
        ExprKind::Ident(ident) if ident.segments.len() == 1 => {
            Some(vec![ident.segments[0].value.clone()])
        }
        _ => None,
    }
}

fn collect_scope_res_paths(expr: &Expr<Parsed>, out: &mut Vec<Vec<String>>) {
    match &expr.kind {
        ExprKind::Ident(ident) => {
            let segs: Vec<String> = ident.segments.iter().map(|s| s.value.clone()).collect();
            if ident.is_global && segs.len() >= 2 {
                // global paths are std-root-relative - prepend "std"
                let mut full = vec!["std".to_string()];
                full.extend(segs);
                out.push(full);
            } else if !ident.is_global && segs.len() >= 3 {
                out.push(segs);
            }
        }
        ExprKind::ScopeRes { base, member: _ } => {
            if let Some(path) = try_scope_res_path(expr) {
                if path.len() >= 3 {
                    out.push(path);
                    return; // don't recurse - subpath is covered by the full path
                }
            }
            collect_scope_res_paths(base, out);
        }
        ExprKind::Call { callee, args, .. } => {
            collect_scope_res_paths(callee, out);
            for a in args {
                collect_scope_res_paths(a, out);
            }
        }
        ExprKind::Block(stmts) => {
            for s in stmts {
                collect_scope_res_paths(s, out);
            }
        }
        ExprKind::Return(Some(v)) => collect_scope_res_paths(v, out),
        ExprKind::Let { value, .. } | ExprKind::Const { value, .. } => {
            collect_scope_res_paths(value, out);
        }
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_scope_res_paths(condition, out);
            collect_scope_res_paths(then_branch, out);
            if let Some(e) = else_branch {
                collect_scope_res_paths(e, out);
            }
        }
        ExprKind::While { condition, body } => {
            collect_scope_res_paths(condition, out);
            collect_scope_res_paths(body, out);
        }
        ExprKind::For { iterable, body, .. } => {
            collect_scope_res_paths(iterable, out);
            collect_scope_res_paths(body, out);
        }
        ExprKind::Match {
            value,
            arms,
            else_arm,
        } => {
            collect_scope_res_paths(value, out);
            for arm in arms {
                collect_scope_res_paths(&arm.body, out);
            }
            if let Some(e) = else_arm {
                collect_scope_res_paths(e, out);
            }
        }
        ExprKind::Lambda { body, .. } => {
            collect_scope_res_paths(body, out);
        }
        ExprKind::Add(a, b)
        | ExprKind::Sub(a, b)
        | ExprKind::Mul(a, b)
        | ExprKind::Div(a, b)
        | ExprKind::Eq(a, b)
        | ExprKind::Neq(a, b)
        | ExprKind::And(a, b)
        | ExprKind::Or(a, b) => {
            collect_scope_res_paths(a, out);
            collect_scope_res_paths(b, out);
        }
        _ => {}
    }
}

fn item_name(item: &Item<Parsed>) -> Option<String> {
    match item {
        Item::Function(f) => Some(f.name.value.clone()),
        Item::Struct(s) => Some(s.name.value.clone()),
        Item::TypeAlias(a) => Some(a.name.value.clone()),
        _ => None,
    }
}

fn read_dir_duck_files(dir: &Path) -> Result<Vec<(PathBuf, String)>, String> {
    let mut out = Vec::new();
    for entry in
        fs::read_dir(dir).map_err(|e| format!("cannot read dir {}: {}", dir.display(), e))?
    {
        let entry = entry.map_err(|e| e.to_string())?;
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) == Some("duck") {
            let src = fs::read_to_string(&path)
                .map_err(|e| format!("cannot read {}: {}", path.display(), e))?;
            out.push((path, src));
        }
    }
    Ok(out)
}
