use std::collections::{HashMap, HashSet};

use crate::dargo::module_loader::ModuleStore;
use crate::parser2::parser::{
    DefId, DefKind, DuckType, Expr, ExprKind, ExtensionDecl, Field, FmtPart, FunTypeParam,
    FunctionDecl, Generic, Item, JsxAttr, JsxAttrValue, JsxNode, MatchArm, Param, Parsed, Resolved,
    SourceFile, Span, StructDecl, SymbolDef, SymbolTable, TypeAliasDecl, TypeDescription, TypeExpr,
    Typed, UnresolvedIdent, UnresolvedTypeRef, WithSpan,
};

#[derive(Debug, Clone)]
pub struct ResolveError {
    pub msg: String,
    pub span: Span,
}

// DefId(0) is reserved as the poison sentinel - inserted once at init.
const POISON: DefId = DefId(0);

/// Try to flatten a nested ScopeRes + member into a path vec.
/// `std::io` base + "println" member -> `["std", "io", "println"]`
fn collect_scope_res_path(base: &Expr<Parsed>, final_member: &str) -> Option<Vec<String>> {
    fn collect(expr: &Expr<Parsed>, out: &mut Vec<String>) -> bool {
        match &expr.kind {
            ExprKind::ScopeRes { base, member } => {
                if !collect(base, out) {
                    return false;
                }
                out.push(member.value.clone());
                true
            }
            ExprKind::Ident(ident) if ident.segments.len() == 1 => {
                out.push(ident.segments[0].value.clone());
                true
            }
            _ => false,
        }
    }
    let mut path = Vec::new();
    if collect(base, &mut path) {
        path.push(final_member.to_string());
        Some(path)
    } else {
        None
    }
}

struct Resolver<'a> {
    symbols: SymbolTable,
    // innermost scope is last; each scope maps name -> DefId
    scopes: Vec<HashMap<String, DefId>>,
    errors: Vec<ResolveError>,
    // set of DefIds that came from loaded module items (used for selective emit)
    module_def_ids: HashSet<DefId>,
    // names of all mangled module items, set before collect_top_level runs
    module_item_names: &'a HashSet<String>,
    // module store for building Module namespace bindings
    module_store: &'a ModuleStore,
}

impl<'a> Resolver<'a> {
    fn new(module_item_names: &'a HashSet<String>, module_store: &'a ModuleStore) -> Self {
        let mut symbols = SymbolTable::default();
        symbols.insert(SymbolDef {
            kind: DefKind::Poison,
            name: "<error>".into(),
            span: Span::dummy(),
            scope_depth: 0,
            ty: None,
        });
        Self {
            symbols,
            scopes: vec![HashMap::new()],
            errors: Vec::new(),
            module_def_ids: HashSet::new(),
            module_item_names,
            module_store,
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &WithSpan<String>, kind: DefKind) -> DefId {
        let scope_depth = (self.scopes.len() - 1) as u32;
        let id = self.symbols.insert(SymbolDef {
            kind,
            name: name.value.clone(),
            span: name.span,
            scope_depth,
            ty: None,
        });
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.value.clone(), id);
        // track which DefIds come from module-loaded items
        if self.module_item_names.contains(&name.value) {
            self.module_def_ids.insert(id);
        }
        id
    }

    fn lookup(&self, name: &str) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&id) = scope.get(name) {
                return Some(id);
            }
        }
        None
    }

    fn error(&mut self, msg: impl Into<String>, span: Span) {
        self.errors.push(ResolveError {
            msg: msg.into(),
            span,
        });
    }

    fn poison(&mut self, msg: impl Into<String>, span: Span) -> DefId {
        self.error(msg, span);
        POISON
    }

    fn collect_top_level(&mut self, items: &[Item<Parsed>]) {
        // Pass 1: define all named items (functions, structs, type aliases, Go/Ts packages).
        // This must complete before pass 2 so that UseDecl::Duck bindings can look up any
        // already-mangled name regardless of declaration order.
        for item in items {
            match item {
                Item::Function(f) => {
                    self.define(
                        &f.name,
                        DefKind::Function {
                            is_static: f.is_static,
                            is_client: f.is_client,
                        },
                    );
                }
                Item::TypeAlias(t) => {
                    self.define(&t.name, DefKind::TypeAlias);
                }
                Item::Struct(s) => {
                    self.define(&s.name, DefKind::Struct);
                }
                Item::Use(crate::parser2::parser::UseDecl::Ts(pkg_name_str, span)) => {
                    let short_name = pkg_name_str
                        .split('/')
                        .next_back()
                        .unwrap_or(pkg_name_str.as_str())
                        .to_string();
                    if !short_name.is_empty() {
                        let ts_path = format!("ts:{}", pkg_name_str);
                        let name_ws = WithSpan::new(short_name, *span);
                        let id = self.define(
                            &name_ws,
                            DefKind::GoPackage {
                                import_path: ts_path.clone(),
                            },
                        );
                        self.symbols.get_mut(id).ty = Some(TypeExpr {
                            desc: TypeDescription::GoPackage(ts_path),
                            span: *span,
                        });
                    }
                }
                Item::Use(crate::parser2::parser::UseDecl::Go(path, span)) => {
                    let import_path = path[0].value.clone();
                    let pkg_name = if path.len() >= 2 {
                        path[1].value.clone()
                    } else {
                        import_path
                            .split('/')
                            .last()
                            .unwrap_or(&import_path)
                            .to_string()
                    };
                    if !pkg_name.is_empty() {
                        let name_ws = WithSpan::new(pkg_name, *span);
                        let id = self.define(
                            &name_ws,
                            DefKind::GoPackage {
                                import_path: import_path.clone(),
                            },
                        );
                        self.symbols.get_mut(id).ty = Some(TypeExpr {
                            desc: TypeDescription::GoPackage(import_path),
                            span: *span,
                        });
                    }
                }
                Item::Use(crate::parser2::parser::UseDecl::Duck(_, _, _)) | Item::Extension(_) => {}
            }
        }

        // Pass 2: process UseDecl::Duck imports now that all names are defined.
        for item in items {
            if let Item::Use(crate::parser2::parser::UseDecl::Duck(segs, _, span)) = item {
                self.register_duck_import(segs, *span);
            }
        }
    }

    /// Handle a `use path::to::thing;` import by building a Module binding or
    /// a direct alias depending on whether the path identifies a module dir or a member.
    fn register_duck_import(&mut self, segs: &[WithSpan<String>], span: Span) {
        if segs.len() < 2 {
            return;
        }
        let path: Vec<String> = segs.iter().map(|s| s.value.clone()).collect();
        let (mod_path, member_opt) = self.module_store.split_path(&path);

        if mod_path.is_empty() {
            // no matching module on disk - report when we fail to resolve references
            return;
        }

        let mod_key = mod_path.join("::");
        let Some(loaded) = self.module_store.modules.get(&mod_key) else {
            return;
        };

        // prefix used during loading, e.g. "io" for std::io
        let prefix = mod_path[1..].join("__");

        match member_opt {
            None => {
                // namespace import - `use std::io;` -> bind `io` as Module
                let mut members = HashMap::new();
                for (short_name, _) in &loaded.members {
                    let mangled = format!("{}__{}", prefix, short_name);
                    if let Some(&def_id) = self.scopes[0].get(&mangled) {
                        members.insert(short_name.clone(), def_id);
                    }
                }
                let binding_name = segs.last().unwrap();
                self.define(binding_name, DefKind::Module { members });
            }
            Some(member_name) => {
                // direct import - `use std::io::{println}` -> bind `println` directly.
                // `member_name` may contain "__" when the source used brace-expansion like
                // `use ::opt::{Opt, Some}` which the parser flattens into one path and
                // split_path joins trailing segments with "__". In that case split and
                // register each name individually.
                let names: Vec<&str> = if member_name.contains("__") {
                    member_name.split("__").collect()
                } else {
                    vec![member_name.as_str()]
                };
                for name in names {
                    let mangled = format!("{}__{}", prefix, name);
                    if let Some(&def_id) = self.scopes[0].get(&mangled) {
                        self.scopes
                            .last_mut()
                            .unwrap()
                            .insert(name.to_string(), def_id);
                    }
                    // Silently skip names not found; they come from brace-expansion and the
                    // module's own self-imports will create the bindings separately.
                }
            }
        }
    }

    fn resolve_source_file(&mut self, sf: SourceFile<Parsed>) -> SourceFile<Resolved> {
        self.collect_top_level(&sf.items);
        let items = sf
            .items
            .into_iter()
            .map(|item| self.resolve_item(item))
            .collect();
        SourceFile { items }
    }

    fn resolve_item(&mut self, item: Item<Parsed>) -> Item<Resolved> {
        match item {
            Item::Function(f) => Item::Function(self.resolve_function(f)),
            Item::TypeAlias(t) => Item::TypeAlias(self.resolve_type_alias(t)),
            Item::Struct(s) => Item::Struct(self.resolve_struct(s)),
            Item::Use(u) => Item::Use(u),
            Item::Extension(e) => Item::Extension(self.resolve_extension(e)),
        }
    }

    fn resolve_generics(
        &mut self,
        generics: Vec<WithSpan<Generic<Parsed>>>,
    ) -> Vec<WithSpan<Generic<Resolved>>> {
        generics
            .into_iter()
            .map(|g| {
                let span = g.span;
                let Generic { name, constraint } = g.value;
                let constraint = constraint.map(|c| self.resolve_type_expr(c));
                self.define(&name, DefKind::GenericParam);
                WithSpan::new(Generic { name, constraint }, span)
            })
            .collect()
    }

    fn resolve_param(&mut self, param: Param<Parsed>) -> Param<Resolved> {
        let type_expr = self.resolve_type_expr(param.type_expr);
        let id = self.define(
            &param.name,
            DefKind::Param {
                is_mut: param.is_mut,
            },
        );
        self.symbols.get_mut(id).ty = Some(type_expr_to_typed(type_expr.clone()));
        Param {
            name: param.name,
            type_expr,
            is_mut: param.is_mut,
        }
    }

    fn resolve_field(&mut self, field: Field<Parsed>) -> Field<Resolved> {
        Field {
            name: field.name,
            type_expr: self.resolve_type_expr(field.type_expr),
        }
    }

    fn resolve_function(&mut self, func: FunctionDecl<Parsed>) -> FunctionDecl<Resolved> {
        self.push_scope();
        let generics = self.resolve_generics(func.generics);
        let params: Vec<Param<Resolved>> = func
            .params
            .into_iter()
            .map(|p| self.resolve_param(p))
            .collect();
        let return_type = func.return_type.map(|t| self.resolve_type_expr(t));
        let body = self.resolve_expr(func.body);
        self.pop_scope();
        FunctionDecl {
            name: func.name,
            generics,
            params,
            return_type,
            body,
            is_static: func.is_static,
            is_client: func.is_client,
            span: func.span,
        }
    }

    fn resolve_type_alias(&mut self, alias: TypeAliasDecl<Parsed>) -> TypeAliasDecl<Resolved> {
        self.push_scope();
        let generics = self.resolve_generics(alias.generics);
        let type_expr = self.resolve_type_expr(alias.type_expr);
        self.pop_scope();
        TypeAliasDecl {
            name: alias.name,
            generics,
            type_expr,
            span: alias.span,
        }
    }

    fn resolve_struct(&mut self, s: StructDecl<Parsed>) -> StructDecl<Resolved> {
        self.push_scope();
        let generics = self.resolve_generics(s.generics);
        let fields = s
            .fields
            .into_iter()
            .map(|f| self.resolve_field(f))
            .collect();
        self.pop_scope();
        StructDecl {
            name: s.name,
            generics,
            fields,
            span: s.span,
        }
    }

    fn resolve_extension(&mut self, ext: ExtensionDecl<Parsed>) -> ExtensionDecl<Resolved> {
        self.push_scope();

        // Bring the struct's generic type parameters (T, U, …) into scope so that
        // method signatures and bodies can reference them.
        // The target type is e.g. `ArrayList<T>` where T is stored as TemplParam.
        if let TypeDescription::TypeName {
            ref type_params, ..
        } = ext.target.desc
        {
            for tp in type_params {
                if let TypeDescription::TemplParam(ref name) = tp.desc {
                    let ws = WithSpan::new(name.clone(), tp.span);
                    self.define(&ws, DefKind::GenericParam);
                }
            }
        }

        // `self` is the implicit receiver in extension instance methods.
        let self_ws = WithSpan::new("self".to_string(), ext.target.span);
        self.define(&self_ws, DefKind::Local { is_mut: true });

        let target = self.resolve_type_expr(ext.target);
        let methods = ext
            .methods
            .into_iter()
            .map(|m| self.resolve_function(m))
            .collect();

        self.pop_scope();
        ExtensionDecl {
            target,
            methods,
            span: ext.span,
        }
    }

    fn resolve_jsx_node(&mut self, node: JsxNode<Parsed>) -> JsxNode<Resolved> {
        match node {
            JsxNode::Text(s) => JsxNode::Text(s),
            JsxNode::Expr(e) => JsxNode::Expr(Box::new(self.resolve_expr(*e))),
            JsxNode::Element {
                tag,
                attrs,
                children,
            } => JsxNode::Element {
                tag,
                attrs: attrs
                    .into_iter()
                    .map(|a| JsxAttr {
                        name: a.name,
                        value: match a.value {
                            JsxAttrValue::Bool => JsxAttrValue::Bool,
                            JsxAttrValue::Str(s) => JsxAttrValue::Str(s),
                            JsxAttrValue::Expr(e) => {
                                JsxAttrValue::Expr(Box::new(self.resolve_expr(*e)))
                            }
                        },
                    })
                    .collect(),
                children: children
                    .into_iter()
                    .map(|c| self.resolve_jsx_node(c))
                    .collect(),
            },
        }
    }

    fn resolve_type_expr(&mut self, te: TypeExpr<Parsed>) -> TypeExpr<Resolved> {
        let desc = self.resolve_type_desc(te.desc, te.span);
        TypeExpr {
            desc,
            span: te.span,
        }
    }

    fn resolve_type_desc(
        &mut self,
        desc: TypeDescription<Parsed>,
        span: Span,
    ) -> TypeDescription<Resolved> {
        match desc {
            TypeDescription::Int => TypeDescription::Int,
            TypeDescription::UInt => TypeDescription::UInt,
            TypeDescription::Float => TypeDescription::Float,
            TypeDescription::Bool(v) => TypeDescription::Bool(v),
            TypeDescription::Char => TypeDescription::Char,
            TypeDescription::Byte => TypeDescription::Byte,
            TypeDescription::String(v) => TypeDescription::String(v),
            TypeDescription::Statement => TypeDescription::Statement,
            TypeDescription::Never => TypeDescription::Never,
            TypeDescription::Html => TypeDescription::Html,
            TypeDescription::Any => TypeDescription::Any,
            TypeDescription::Tag(s) => TypeDescription::Tag(s),
            TypeDescription::TypeOf(s) => TypeDescription::TypeOf(s),
            TypeDescription::TemplParam(s) => TypeDescription::TemplParam(s),
            TypeDescription::Go(s) => TypeDescription::Go(s),

            TypeDescription::KeyOf(inner) => {
                TypeDescription::KeyOf(Box::new(self.resolve_type_expr(*inner)))
            }
            TypeDescription::Tuple(ts) => {
                TypeDescription::Tuple(ts.into_iter().map(|t| self.resolve_type_expr(t)).collect())
            }
            TypeDescription::Array(inner) => {
                TypeDescription::Array(Box::new(self.resolve_type_expr(*inner)))
            }
            TypeDescription::Indexed { base, index } => TypeDescription::Indexed {
                base: Box::new(self.resolve_type_expr(*base)),
                index: Box::new(self.resolve_type_expr(*index)),
            },
            TypeDescription::Or(ts) => {
                TypeDescription::Or(ts.into_iter().map(|t| self.resolve_type_expr(t)).collect())
            }
            TypeDescription::And(ts) => {
                TypeDescription::And(ts.into_iter().map(|t| self.resolve_type_expr(t)).collect())
            }
            TypeDescription::Fun {
                params,
                return_type,
                is_mut,
                is_variadic,
            } => TypeDescription::Fun {
                params: params
                    .into_iter()
                    .map(|p| FunTypeParam {
                        label: p.label,
                        type_expr: self.resolve_type_expr(p.type_expr),
                    })
                    .collect(),
                return_type: Box::new(self.resolve_type_expr(*return_type)),
                is_mut,
                is_variadic,
            },
            TypeDescription::Ref(inner) => {
                TypeDescription::Ref(Box::new(self.resolve_type_expr(*inner)))
            }
            TypeDescription::RefMut(inner) => {
                TypeDescription::RefMut(Box::new(self.resolve_type_expr(*inner)))
            }
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } => {
                let def_id = self.resolve_type_ref(&type_ref, span);
                let type_params = type_params
                    .into_iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect();
                TypeDescription::TypeName {
                    type_ref: def_id,
                    type_params,
                }
            }
            TypeDescription::Duck(duck) => TypeDescription::Duck(DuckType {
                fields: duck
                    .fields
                    .into_iter()
                    .map(|f| self.resolve_field(f))
                    .collect(),
            }),
            TypeDescription::NamedDuck { name, type_params } => TypeDescription::NamedDuck {
                name,
                type_params: type_params
                    .into_iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect(),
            },
            TypeDescription::Struct { name, type_params } => TypeDescription::Struct {
                name,
                type_params: type_params
                    .into_iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect(),
            },
            TypeDescription::GoPackage(s) => TypeDescription::GoPackage(s),
            TypeDescription::GoNamed(s) => TypeDescription::GoNamed(s),
        }
    }

    fn resolve_type_ref(&mut self, type_ref: &UnresolvedTypeRef, span: Span) -> DefId {
        match type_ref.path.len() {
            0 => self.poison("empty type path", span),
            1 => {
                let name = &type_ref.path[0];
                match self.lookup(name) {
                    Some(id) => id,
                    None => self.poison(format!("undefined type `{name}`"), span),
                }
            }
            _ => {
                // Global paths (is_global=true, e.g. `::col::Iter`) are relative to the
                // std root; look them up directly in the module store.
                if type_ref.is_global && type_ref.path.len() >= 2 {
                    let std_mod_key = format!("std::{}", type_ref.path[0]);
                    let member = &type_ref.path[1];
                    if self.module_store.modules.contains_key(&std_mod_key) {
                        let mangled = format!("{}__{}", type_ref.path[0], member);
                        if let Some(def_id) = self.lookup(&mangled) {
                            return def_id;
                        }
                    }
                }

                // two-segment: treat first as module namespace binding, second as member
                let ns = &type_ref.path[0];
                let member = &type_ref.path[1];
                if let Some(ns_id) = self.lookup(ns) {
                    if let DefKind::Module { members } = self.symbols.get(ns_id).kind.clone() {
                        if let Some(&def_id) = members.get(member.as_str()) {
                            return def_id;
                        }
                        return self
                            .poison(format!("no type `{}` in module `{}`", member, ns), span);
                    }
                }
                let path = type_ref.path.join("::");
                self.poison(format!("unresolved module path `{path}`"), span)
            }
        }
    }

    fn resolve_ident(&mut self, ident: &UnresolvedIdent, span: Span) -> DefId {
        if ident.segments.len() == 1 {
            let name = &ident.segments[0].value;
            match self.lookup(name) {
                Some(id) => id,
                None => self.poison(format!("undefined name `{name}`"), span),
            }
        } else if ident.segments.len() >= 3 {
            let segs: Vec<&str> = ident.segments.iter().map(|s| s.value.as_str()).collect();

            // Global `::mod::Struct::method` paths (is_global=true, first seg is std module).
            // E.g. `::col::Iter::from` → module "std::col", composite key "Iter__from".
            if ident.is_global {
                let std_mod_key = format!("std::{}", segs[0]);
                if let Some(module) = self.module_store.modules.get(&std_mod_key) {
                    let composite = segs[1..].join("__");
                    if let Some(&idx) = module.members.get(&composite) {
                        if let Item::Function(f) = &module.items[idx] {
                            if let Some(def_id) = self.lookup(&f.name.value) {
                                return def_id;
                            }
                        }
                    }
                    // Also try last segment as a plain function in the module.
                    let prefix = segs[0];
                    let mangled = format!("{}__{}", prefix, segs[segs.len() - 1]);
                    if let Some(def_id) = self.lookup(&mangled) {
                        return def_id;
                    }
                }
                return self.poison(format!("unresolved path `::{}`", segs.join("::")), span);
            }

            // Non-global 3+ segment path like `std::io::println`.
            let mod_key = segs[..segs.len() - 1].join("::");
            let member_name = segs[segs.len() - 1];
            if let Some(_module) = self.module_store.modules.get(&mod_key) {
                let prefix = segs[1..segs.len() - 1].join("__");
                let mangled = format!("{}__{}", prefix, member_name);
                if let Some(def_id) = self.lookup(&mangled) {
                    return def_id;
                }
                self.poison(
                    format!("no member `{}` in module `{}`", member_name, mod_key),
                    span,
                )
            } else {
                let path = segs.join("::");
                self.poison(format!("unresolved path `{}`", path), span)
            }
        } else {
            let first = ident.segments[0].value.as_str();
            let second = ident.segments[1].value.as_str();

            // Global 2-segment path: `::module::member` (is_global=true, e.g. `::error::panic`).
            // These are std-root-relative; look up "std::{first}" module directly.
            if ident.is_global {
                let std_mod_key = format!("std::{}", first);
                if self.module_store.modules.contains_key(&std_mod_key) {
                    let mangled = format!("{}__{}", first, second);
                    if let Some(def_id) = self.lookup(&mangled) {
                        return def_id;
                    }
                    // Also check composite keys for static methods.
                    if let Some(module) = self.module_store.modules.get(&std_mod_key) {
                        let composite = format!("{}__{}", first, second);
                        if let Some(&idx) = module.members.get(&composite) {
                            if let Item::Function(f) = &module.items[idx] {
                                if let Some(def_id) = self.lookup(&f.name.value) {
                                    return def_id;
                                }
                            }
                        }
                    }
                }
                return self.poison(format!("unresolved path `::{first}::{second}`"), span);
            }

            // Non-global 2-segment: try as `StructName::static_method` dispatch.
            // Static methods are registered in the module store with composite keys
            // like "Opt__some" → mangled function name "opt__Opt__some".
            let composite = format!("{}__{}", first, second);
            for module in self.module_store.modules.values() {
                if let Some(&idx) = module.members.get(&composite) {
                    if let Item::Function(f) = &module.items[idx] {
                        if let Some(def_id) = self.lookup(&f.name.value) {
                            return def_id;
                        }
                    }
                }
            }
            let path = format!("{}::{}", first, second);
            self.poison(format!("unresolved path `{path}`"), span)
        }
    }

    fn resolve_expr(&mut self, expr: Expr<Parsed>) -> Expr<Resolved> {
        let kind = self.resolve_expr_kind(expr.kind, expr.span);
        Expr {
            kind,
            ty: (),
            span: expr.span,
        }
    }

    fn resolve_expr_kind(&mut self, kind: ExprKind<Parsed>, span: Span) -> ExprKind<Resolved> {
        match kind {
            ExprKind::Int(v) => ExprKind::Int(v),
            ExprKind::Float(v) => ExprKind::Float(v),
            ExprKind::Bool(v) => ExprKind::Bool(v),
            ExprKind::Char(v) => ExprKind::Char(v),
            ExprKind::String(v) => ExprKind::String(v),
            ExprKind::Tag(v) => ExprKind::Tag(v),
            ExprKind::InlineGo(v) => ExprKind::InlineGo(v),
            ExprKind::Jsx(node) => ExprKind::Jsx(Box::new(self.resolve_jsx_node(*node))),
            ExprKind::Break => ExprKind::Break,
            ExprKind::Continue => ExprKind::Continue,

            ExprKind::FmtString(parts) => ExprKind::FmtString(
                parts
                    .into_iter()
                    .map(|p| match p {
                        FmtPart::Literal(s) => FmtPart::Literal(s),
                        FmtPart::Expr(e) => FmtPart::Expr(self.resolve_expr(e)),
                    })
                    .collect(),
            ),

            ExprKind::Ident(ident) => ExprKind::Ident(self.resolve_ident(&ident, span)),

            ExprKind::Block(stmts) => {
                self.push_scope();
                let stmts = stmts.into_iter().map(|s| self.resolve_expr(s)).collect();
                self.pop_scope();
                ExprKind::Block(stmts)
            }

            ExprKind::Let {
                is_mut,
                name,
                type_ann,
                value,
            } => {
                let type_ann = type_ann.map(|t| self.resolve_type_expr(t));
                let value = Box::new(self.resolve_expr(*value));
                let id = self.define(&name, DefKind::Local { is_mut });
                if let Some(ref ann) = type_ann {
                    self.symbols.get_mut(id).ty = Some(type_expr_to_typed(ann.clone()));
                }
                ExprKind::Let {
                    is_mut,
                    name,
                    type_ann,
                    value,
                }
            }

            ExprKind::LetTuple { names, value } => {
                let value = Box::new(self.resolve_expr(*value));
                for name in &names {
                    self.define(name, DefKind::Local { is_mut: false });
                }
                ExprKind::LetTuple { names, value }
            }

            ExprKind::Const {
                name,
                type_ann,
                value,
            } => {
                let type_ann = type_ann.map(|t| self.resolve_type_expr(t));
                let value = Box::new(self.resolve_expr(*value));
                let id = self.define(&name, DefKind::Const);
                if let Some(ref ann) = type_ann {
                    self.symbols.get_mut(id).ty = Some(type_expr_to_typed(ann.clone()));
                }
                ExprKind::Const {
                    name,
                    type_ann,
                    value,
                }
            }

            ExprKind::Assign { target, value } => ExprKind::Assign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::AddAssign { target, value } => ExprKind::AddAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::SubAssign { target, value } => ExprKind::SubAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::MulAssign { target, value } => ExprKind::MulAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::DivAssign { target, value } => ExprKind::DivAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::ModAssign { target, value } => ExprKind::ModAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::ShrAssign { target, value } => ExprKind::ShrAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },
            ExprKind::ShlAssign { target, value } => ExprKind::ShlAssign {
                target: Box::new(self.resolve_expr(*target)),
                value: Box::new(self.resolve_expr(*value)),
            },

            ExprKind::Add(a, b) => {
                ExprKind::Add(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Sub(a, b) => {
                ExprKind::Sub(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Mul(a, b) => {
                ExprKind::Mul(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Div(a, b) => {
                ExprKind::Div(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Mod(a, b) => {
                ExprKind::Mod(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }

            ExprKind::BitAnd(a, b) => {
                ExprKind::BitAnd(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::BitOr(a, b) => {
                ExprKind::BitOr(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::BitXor(a, b) => {
                ExprKind::BitXor(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::BitNot(a) => ExprKind::BitNot(b!(self.resolve_expr(*a))),
            ExprKind::Shl(a, b) => {
                ExprKind::Shl(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Shr(a, b) => {
                ExprKind::Shr(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }

            ExprKind::Eq(a, b) => {
                ExprKind::Eq(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Neq(a, b) => {
                ExprKind::Neq(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Lt(a, b) => {
                ExprKind::Lt(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Lte(a, b) => {
                ExprKind::Lte(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Gt(a, b) => {
                ExprKind::Gt(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Gte(a, b) => {
                ExprKind::Gte(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }

            ExprKind::And(a, b) => {
                ExprKind::And(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Or(a, b) => {
                ExprKind::Or(b!(self.resolve_expr(*a)), b!(self.resolve_expr(*b)))
            }
            ExprKind::Not(a) => ExprKind::Not(b!(self.resolve_expr(*a))),

            ExprKind::Neg(a) => ExprKind::Neg(b!(self.resolve_expr(*a))),
            ExprKind::Ref(a) => ExprKind::Ref(b!(self.resolve_expr(*a))),
            ExprKind::RefMut(a) => ExprKind::RefMut(b!(self.resolve_expr(*a))),
            ExprKind::Deref(a) => ExprKind::Deref(b!(self.resolve_expr(*a))),

            ExprKind::Field { base, field } => ExprKind::Field {
                base: b!(self.resolve_expr(*base)),
                field,
            },
            ExprKind::Index { base, index } => ExprKind::Index {
                base: b!(self.resolve_expr(*base)),
                index: b!(self.resolve_expr(*index)),
            },

            // Desugar `module::member` into a direct Ident when the base is a Module binding.
            // This removes the need for type inference to understand module namespaces.
            ExprKind::ScopeRes { base, member } => {
                // 2-segment: `io::member` where `io` is a bound Module namespace
                if let ExprKind::Ident(ref ident) = base.kind {
                    if ident.segments.len() == 1 {
                        let ns_name = &ident.segments[0].value;
                        if let Some(ns_id) = self.lookup(ns_name) {
                            if let DefKind::Module { members } =
                                self.symbols.get(ns_id).kind.clone()
                            {
                                return if let Some(&fn_id) = members.get(member.value.as_str()) {
                                    ExprKind::Ident(fn_id)
                                } else {
                                    ExprKind::Ident(self.poison(
                                        format!(
                                            "no member `{}` in module `{}`",
                                            member.value, ns_name
                                        ),
                                        member.span,
                                    ))
                                };
                            }
                        }
                    }
                }

                // multi-segment: `std::io::member` - collect the full path then look up
                if let Some(path) = collect_scope_res_path(&base, &member.value) {
                    if path.len() >= 3 {
                        let mod_key = path[..path.len() - 1].join("::");
                        let member_name = path.last().unwrap().as_str();
                        if let Some(module) = self.module_store.modules.get(&mod_key) {
                            let prefix = path[1..path.len() - 1].join("__");
                            let mangled = format!("{}__{}", prefix, member_name);
                            return if let Some(def_id) = self.lookup(&mangled) {
                                ExprKind::Ident(def_id)
                            } else if module.members.contains_key(member_name) {
                                ExprKind::Ident(self.poison(
                                    format!("no member `{}` in module `{}`", member_name, mod_key),
                                    member.span,
                                ))
                            } else {
                                ExprKind::Ident(self.poison(
                                    format!("no member `{}` in module `{}`", member_name, mod_key),
                                    member.span,
                                ))
                            };
                        } else {
                            // module not loaded - clear error instead of "undefined name std"
                            return ExprKind::Ident(self.poison(
                                format!("unresolved path `{}`", path.join("::")),
                                member.span,
                            ));
                        }
                    }
                }

                ExprKind::ScopeRes {
                    base: b!(self.resolve_expr(*base)),
                    member,
                }
            }

            ExprKind::Call {
                callee,
                type_params,
                args,
            } => ExprKind::Call {
                callee: b!(self.resolve_expr(*callee)),
                type_params: type_params
                    .into_iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect(),
                args: args.into_iter().map(|a| self.resolve_expr(a)).collect(),
            },

            ExprKind::As { value, type_expr } => ExprKind::As {
                value: b!(self.resolve_expr(*value)),
                type_expr: self.resolve_type_expr(type_expr),
            },

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => ExprKind::If {
                condition: b!(self.resolve_expr(*condition)),
                then_branch: b!(self.resolve_expr(*then_branch)),
                else_branch: else_branch.map(|e| b!(self.resolve_expr(*e))),
            },

            ExprKind::While { condition, body } => ExprKind::While {
                condition: b!(self.resolve_expr(*condition)),
                body: b!(self.resolve_expr(*body)),
            },

            ExprKind::For {
                binding,
                is_mut,
                iterable,
                body,
            } => {
                let iterable = b!(self.resolve_expr(*iterable));
                self.push_scope();
                self.define(&binding, DefKind::Local { is_mut });
                let body = b!(self.resolve_expr(*body));
                self.pop_scope();
                ExprKind::For {
                    binding,
                    is_mut,
                    iterable,
                    body,
                }
            }

            ExprKind::Return(v) => ExprKind::Return(v.map(|e| b!(self.resolve_expr(*e)))),

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => ExprKind::Match {
                value: b!(self.resolve_expr(*value)),
                arms: arms
                    .into_iter()
                    .map(|arm| self.resolve_match_arm(arm))
                    .collect(),
                else_arm: else_arm.map(|e| b!(self.resolve_expr(*e))),
            },

            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } => {
                let def_id = self.resolve_ident(&name, span);
                let type_params = type_params
                    .into_iter()
                    .map(|t| self.resolve_type_expr(t))
                    .collect();
                let fields = fields
                    .into_iter()
                    .map(|(label, val)| (label, self.resolve_expr(val)))
                    .collect();
                ExprKind::StructLit {
                    name: def_id,
                    type_params,
                    fields,
                }
            }

            ExprKind::DuckLit(fields) => ExprKind::DuckLit(
                fields
                    .into_iter()
                    .map(|(label, val)| (label, self.resolve_expr(val)))
                    .collect(),
            ),

            ExprKind::Array(elems) => {
                ExprKind::Array(elems.into_iter().map(|e| self.resolve_expr(e)).collect())
            }
            ExprKind::Tuple(elems) => {
                ExprKind::Tuple(elems.into_iter().map(|e| self.resolve_expr(e)).collect())
            }

            ExprKind::Lambda {
                is_mut,
                params,
                return_type,
                body,
            } => {
                self.push_scope();
                let params: Vec<Param<Resolved>> =
                    params.into_iter().map(|p| self.resolve_param(p)).collect();
                let return_type = return_type.map(|t| self.resolve_type_expr(t));
                let body = b!(self.resolve_expr(*body));
                self.pop_scope();
                ExprKind::Lambda {
                    is_mut,
                    params,
                    return_type,
                    body,
                }
            }

            ExprKind::Async(e) => ExprKind::Async(b!(self.resolve_expr(*e))),
            ExprKind::Defer(e) => ExprKind::Defer(b!(self.resolve_expr(*e))),
        }
    }

    fn resolve_match_arm(&mut self, arm: MatchArm<Parsed>) -> MatchArm<Resolved> {
        let pattern = self.resolve_type_expr(arm.pattern);
        let base = arm.base.map(|t| self.resolve_type_expr(t));
        self.push_scope();
        if let Some(ref binding) = arm.binding {
            self.define(binding, DefKind::Local { is_mut: false });
        }
        let guard = arm.guard.map(|g| b!(self.resolve_expr(*g)));
        let body = self.resolve_expr(arm.body);
        self.pop_scope();
        MatchArm {
            pattern,
            base,
            binding: arm.binding,
            guard,
            body,
            span: arm.span,
        }
    }
}

pub fn type_expr_to_typed(te: TypeExpr<Resolved>) -> TypeExpr<Typed> {
    TypeExpr {
        desc: type_desc_to_typed(te.desc),
        span: te.span,
    }
}

fn type_desc_to_typed(desc: TypeDescription<Resolved>) -> TypeDescription<Typed> {
    match desc {
        TypeDescription::Int => TypeDescription::Int,
        TypeDescription::UInt => TypeDescription::UInt,
        TypeDescription::Float => TypeDescription::Float,
        TypeDescription::Bool(v) => TypeDescription::Bool(v),
        TypeDescription::Char => TypeDescription::Char,
        TypeDescription::Byte => TypeDescription::Byte,
        TypeDescription::String(v) => TypeDescription::String(v),
        TypeDescription::Statement => TypeDescription::Statement,
        TypeDescription::Never => TypeDescription::Never,
        TypeDescription::Html => TypeDescription::Html,
        TypeDescription::Any => TypeDescription::Any,
        TypeDescription::Tag(s) => TypeDescription::Tag(s),
        TypeDescription::TypeOf(s) => TypeDescription::TypeOf(s),
        TypeDescription::TemplParam(s) => TypeDescription::TemplParam(s),
        TypeDescription::Go(s) => TypeDescription::Go(s),
        TypeDescription::KeyOf(inner) => {
            TypeDescription::KeyOf(Box::new(type_expr_to_typed(*inner)))
        }
        TypeDescription::Tuple(ts) => {
            TypeDescription::Tuple(ts.into_iter().map(type_expr_to_typed).collect())
        }
        TypeDescription::Array(inner) => {
            TypeDescription::Array(Box::new(type_expr_to_typed(*inner)))
        }
        TypeDescription::Indexed { base, index } => TypeDescription::Indexed {
            base: Box::new(type_expr_to_typed(*base)),
            index: Box::new(type_expr_to_typed(*index)),
        },
        TypeDescription::Or(ts) => {
            TypeDescription::Or(ts.into_iter().map(type_expr_to_typed).collect())
        }
        TypeDescription::And(ts) => {
            TypeDescription::And(ts.into_iter().map(type_expr_to_typed).collect())
        }
        TypeDescription::Fun {
            params,
            return_type,
            is_mut,
            is_variadic,
        } => TypeDescription::Fun {
            params: params
                .into_iter()
                .map(|p| FunTypeParam {
                    label: p.label,
                    type_expr: type_expr_to_typed(p.type_expr),
                })
                .collect(),
            return_type: Box::new(type_expr_to_typed(*return_type)),
            is_mut,
            is_variadic,
        },
        TypeDescription::Ref(inner) => TypeDescription::Ref(Box::new(type_expr_to_typed(*inner))),
        TypeDescription::RefMut(inner) => {
            TypeDescription::RefMut(Box::new(type_expr_to_typed(*inner)))
        }
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => TypeDescription::TypeName {
            type_ref,
            type_params: type_params.into_iter().map(type_expr_to_typed).collect(),
        },
        TypeDescription::Duck(duck) => TypeDescription::Duck(DuckType {
            fields: duck
                .fields
                .into_iter()
                .map(|f| Field {
                    name: f.name,
                    type_expr: type_expr_to_typed(f.type_expr),
                })
                .collect(),
        }),
        TypeDescription::NamedDuck { name, type_params } => TypeDescription::NamedDuck {
            name,
            type_params: type_params.into_iter().map(type_expr_to_typed).collect(),
        },
        TypeDescription::Struct { name, type_params } => TypeDescription::Struct {
            name,
            type_params: type_params.into_iter().map(type_expr_to_typed).collect(),
        },
        TypeDescription::GoPackage(s) => TypeDescription::GoPackage(s),
        TypeDescription::GoNamed(s) => TypeDescription::GoNamed(s),
    }
}

// shorthand to reduce Box::new(...) noise in the binary op arms
macro_rules! b {
    ($e:expr) => {
        Box::new($e)
    };
}
use b;

#[derive(Debug)]
pub struct ResolveOutput {
    pub source_file: SourceFile<Resolved>,
    pub symbols: SymbolTable,
    pub global_scope: HashMap<String, DefId>,
    pub errors: Vec<ResolveError>,
    /// DefIds of items that were loaded from Duck modules (not user code).
    pub module_def_ids: HashSet<DefId>,
}

/// Empty store used in tests and any context without module loading.
static EMPTY_STORE: std::sync::OnceLock<ModuleStore> = std::sync::OnceLock::new();

fn empty_store() -> &'static ModuleStore {
    EMPTY_STORE.get_or_init(|| ModuleStore::new(PathBuf::new()))
}

use std::path::PathBuf;

pub fn resolve(source_file: SourceFile<Parsed>) -> ResolveOutput {
    resolve_with_modules(source_file, &Default::default(), empty_store())
}

pub fn resolve_with_modules(
    source_file: SourceFile<Parsed>,
    module_item_names: &HashSet<String>,
    module_store: &ModuleStore,
) -> ResolveOutput {
    let mut r = Resolver::new(module_item_names, module_store);
    let resolved = r.resolve_source_file(source_file);
    let global_scope = r.scopes.into_iter().next().unwrap_or_default();
    let module_def_ids = r.module_def_ids;
    ResolveOutput {
        source_file: resolved,
        symbols: r.symbols,
        global_scope,
        errors: r.errors,
        module_def_ids,
    }
}
