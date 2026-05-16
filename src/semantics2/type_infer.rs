use std::collections::{HashMap, HashSet};

use crate::parser2::parser::{
    DefId, DefKind, DuckType, Expr, ExprKind, ExtensionDecl, Field, FmtPart, FunTypeParam,
    FunctionDecl, Generic, Item, JsxAttr, JsxAttrValue, JsxNode, MatchArm, Param, Resolved,
    SourceFile, Span, StructDecl, SymbolTable, TypeAliasDecl, TypeDescription, TypeExpr, Typed,
    WithSpan,
};
use crate::semantics2::resolver::{ResolveOutput, type_expr_to_typed};

#[derive(Debug, Clone)]
pub struct TypeError {
    pub msg: String,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct InferOutput {
    pub source_file: SourceFile<Typed>,
    pub symbols: SymbolTable,
    pub errors: Vec<TypeError>,
    /// Forwarded from ResolveOutput - DefIds of items loaded from Duck modules.
    pub module_def_ids: std::collections::HashSet<crate::parser2::parser::DefId>,
    /// Global name->DefId map forwarded from the resolver, used by selective emit.
    pub global_scope: std::collections::HashMap<String, crate::parser2::parser::DefId>,
    // generic type aliases keyed by DefId, populated by monomorphize
    pub generic_type_aliases: HashMap<DefId, TypeAliasDecl<Typed>>,
}

struct Inferencer {
    symbols: SymbolTable,
    errors: Vec<TypeError>,
    current_return_type: Option<TypeExpr<Typed>>,
    /// Local DefIds grouped by name, in definition order.
    local_queues: HashMap<String, Vec<DefId>>,
    used_locals: HashSet<DefId>,
    /// Struct field types keyed by the struct's DefId, populated during collect_signatures.
    struct_fields: HashMap<DefId, Vec<Field<Typed>>>,
    /// Extension method types keyed by struct DefId then method name to Fun type.
    extension_methods: HashMap<DefId, HashMap<String, TypeExpr<Typed>>>,
    /// Ordered generic param names per struct DefId (extracted from extension targets).
    struct_generic_params: HashMap<DefId, Vec<String>>,
    /// DefIds of items that came from loaded Duck modules - suppress type errors for these.
    module_def_ids: HashSet<DefId>,
    /// Global name→DefId map, used to identify module items in infer_item.
    global_scope: HashMap<String, DefId>,
    /// When true, calls to error() are silently dropped (used while inferring module bodies).
    suppress_errors: bool,
    /// Derived traits per struct DefId, populated during collect_signatures.
    struct_derived: HashMap<DefId, HashSet<crate::parse::struct_parser::DerivableInterface>>,
    /// TypeAlias target types keyed by DefId, populated during collect_signatures.
    /// Tuple: (ordered generic param names, unexpanded RHS type expression).
    type_aliases: HashMap<DefId, (Vec<String>, TypeExpr<Typed>)>,
}

fn any(span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(TypeDescription::Any, span)
}

/// Expand a stored type alias by substituting its generic params with the provided type args.
/// If the alias has no generics or the arg count doesn't match, returns the stored body unchanged.
fn expand_alias_desc(
    expanded: &TypeExpr<Typed>,
    generic_names: &[String],
    type_params: &[TypeExpr<Typed>],
    symbols: &SymbolTable,
) -> TypeDescription<Typed> {
    if !generic_names.is_empty() && type_params.len() == generic_names.len() {
        let subs: HashMap<String, TypeExpr<Typed>> = generic_names
            .iter()
            .zip(type_params.iter())
            .map(|(n, t)| (n.clone(), t.clone()))
            .collect();
        super::mono::subst_type(expanded, &subs, symbols).desc
    } else {
        expanded.desc.clone()
    }
}

fn stmt(span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(TypeDescription::Statement, span)
}

fn bool_ty(span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(TypeDescription::Bool(None), span)
}

fn never(span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(TypeDescription::Never, span)
}

/// Human-readable name for a type, used in error messages.
fn type_name(te: &TypeExpr<Typed>) -> String {
    match &te.desc {
        TypeDescription::Int => "Int".into(),
        TypeDescription::UInt => "UInt".into(),
        TypeDescription::Float => "Float".into(),
        TypeDescription::Bool(_) => "Bool".into(),
        TypeDescription::Char => "Char".into(),
        TypeDescription::Byte => "Byte".into(),
        TypeDescription::String(_) => "String".into(),
        TypeDescription::Any => "Any".into(),
        TypeDescription::Never => "Never".into(),
        TypeDescription::Statement => "()".into(),
        TypeDescription::Tag(t) => format!(".{t}"),
        TypeDescription::Array(e) => format!("[]{}", type_name(e)),
        TypeDescription::Ref(e) => format!("&{}", type_name(e)),
        TypeDescription::RefMut(e) => format!("&mut {}", type_name(e)),
        TypeDescription::Tuple(es) => {
            let inner = es.iter().map(type_name).collect::<Vec<_>>().join(", ");
            format!("({inner})")
        }
        TypeDescription::Fun {
            params,
            return_type,
            ..
        } => {
            let ps = params
                .iter()
                .map(|p| type_name(&p.type_expr))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({ps}) -> {}", type_name(return_type))
        }
        TypeDescription::Or(vs) => vs.iter().map(type_name).collect::<Vec<_>>().join(" | "),
        TypeDescription::Duck(_) => "{ ... }".into(),
        TypeDescription::GoPackage(n) => format!("package {n}"),
        TypeDescription::GoNamed(s) => s.clone(),
        TypeDescription::TypeName { type_ref, .. } => format!("#{}", type_ref.0),
        _ => "?".into(),
    }
}

/// Like type_name but resolves TypeName to the actual struct name via the symbol table.
fn type_name_sym(te: &TypeExpr<Typed>, symbols: &SymbolTable) -> String {
    match &te.desc {
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            let name = symbols.get(*type_ref).name.clone();
            if type_params.is_empty() {
                name
            } else {
                let params = type_params
                    .iter()
                    .map(|tp| type_name_sym(tp, symbols))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}<{params}>")
            }
        }
        _ => type_name(te),
    }
}

// TypeExpr::PartialEq includes spans, so structurally equal types at different positions
// compare unequal - compare descriptions only
fn type_desc_eq(a: &TypeDescription<Typed>, b: &TypeDescription<Typed>) -> bool {
    match (a, b) {
        (
            TypeDescription::TypeName {
                type_ref: ra,
                type_params: tpa,
            },
            TypeDescription::TypeName {
                type_ref: rb,
                type_params: tpb,
            },
        ) => {
            ra == rb
                && tpa.len() == tpb.len()
                && tpa
                    .iter()
                    .zip(tpb.iter())
                    .all(|(ta, tb)| type_desc_eq(&ta.desc, &tb.desc))
        }
        (TypeDescription::Or(va), TypeDescription::Or(vb))
        | (TypeDescription::And(va), TypeDescription::And(vb)) => {
            va.len() == vb.len()
                && va
                    .iter()
                    .zip(vb.iter())
                    .all(|(ta, tb)| type_desc_eq(&ta.desc, &tb.desc))
        }
        (TypeDescription::Ref(ia), TypeDescription::Ref(ib))
        | (TypeDescription::RefMut(ia), TypeDescription::RefMut(ib))
        | (TypeDescription::Array(ia), TypeDescription::Array(ib)) => {
            type_desc_eq(&ia.desc, &ib.desc)
        }
        (
            TypeDescription::Fun {
                params: pa,
                return_type: ra,
                ..
            },
            TypeDescription::Fun {
                params: pb,
                return_type: rb,
                ..
            },
        ) => {
            type_desc_eq(&ra.desc, &rb.desc)
                && pa.len() == pb.len()
                && pa
                    .iter()
                    .zip(pb.iter())
                    .all(|(a, b)| type_desc_eq(&a.type_expr.desc, &b.type_expr.desc))
        }
        (TypeDescription::Tuple(va), TypeDescription::Tuple(vb)) => {
            va.len() == vb.len()
                && va
                    .iter()
                    .zip(vb.iter())
                    .all(|(a, b)| type_desc_eq(&a.desc, &b.desc))
        }
        // Primitives and unit-like variants: just compare directly (they have no TypeExpr children)
        _ => a == b,
    }
}

/// Collect all free GenericParam DefIds from a type in order of first appearance.
/// Used to match call-site type_params to the function's generic parameters.
fn collect_free_generic_params(te: &TypeExpr<Typed>, symbols: &SymbolTable, out: &mut Vec<DefId>) {
    match &te.desc {
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            if matches!(symbols.get(*type_ref).kind, DefKind::GenericParam)
                && !out.contains(type_ref)
            {
                out.push(*type_ref);
            }
            for tp in type_params {
                collect_free_generic_params(tp, symbols, out);
            }
        }
        TypeDescription::Fun {
            params,
            return_type,
            ..
        } => {
            for p in params {
                collect_free_generic_params(&p.type_expr, symbols, out);
            }
            collect_free_generic_params(return_type, symbols, out);
        }
        TypeDescription::Array(e) => collect_free_generic_params(e, symbols, out),
        TypeDescription::Ref(e) | TypeDescription::RefMut(e) => {
            collect_free_generic_params(e, symbols, out)
        }
        TypeDescription::Or(vs) | TypeDescription::And(vs) => {
            for v in vs {
                collect_free_generic_params(v, symbols, out);
            }
        }
        TypeDescription::Tuple(es) => {
            for e in es {
                collect_free_generic_params(e, symbols, out);
            }
        }
        _ => {}
    }
}

/// Whether a type is numeric (valid for arithmetic and negation).
fn is_numeric(te: &TypeExpr<Typed>) -> bool {
    matches!(
        te.desc,
        TypeDescription::Int
            | TypeDescription::UInt
            | TypeDescription::Float
            | TypeDescription::Byte
            | TypeDescription::Any
    )
}

/// Whether a type is an integer (valid for bitwise ops and shifts).
fn is_int(te: &TypeExpr<Typed>) -> bool {
    matches!(
        te.desc,
        TypeDescription::Int | TypeDescription::UInt | TypeDescription::Byte | TypeDescription::Any
    )
}

/// Structural type compatibility. Returns `true` when a value of type `got` can be
/// used where `expected` is required. `Any` on either side silences the check.
fn types_compatible(expected: &TypeDescription<Typed>, got: &TypeDescription<Typed>) -> bool {
    // Any is a universal escape hatch.
    if matches!(expected, TypeDescription::Any) || matches!(got, TypeDescription::Any) {
        return true;
    }
    // Never and Statement propagate without error.
    if matches!(
        expected,
        TypeDescription::Statement | TypeDescription::Never
    ) || matches!(got, TypeDescription::Statement | TypeDescription::Never)
    {
        return true;
    }
    // Go packages are opaque silence compat errors against them.
    if matches!(expected, TypeDescription::GoPackage(_))
        || matches!(got, TypeDescription::GoPackage(_))
    {
        return true;
    }
    // GoNamed types: check interface satisfaction when both sides are Go named types.
    // Primitives are never compatible with GoNamed (e.g. int does not implement io.Writer).
    // For other combinations (structs, arrays, unknown types) stay permissive - Go catches them.
    let is_primitive = |t: &TypeDescription<Typed>| {
        matches!(
            t,
            TypeDescription::Int
                | TypeDescription::UInt
                | TypeDescription::Byte
                | TypeDescription::Float
                | TypeDescription::Char
                | TypeDescription::Bool(_)
                | TypeDescription::String(_)
        )
    };
    match (expected, got) {
        (TypeDescription::GoNamed(a), TypeDescription::GoNamed(b)) => {
            return a == b || go_named_implements(b, a);
        }
        (TypeDescription::GoNamed(_), got) if is_primitive(got) => return false,
        (expected, TypeDescription::GoNamed(_)) if is_primitive(expected) => return false,
        (TypeDescription::GoNamed(_), _) | (_, TypeDescription::GoNamed(_)) => return true,
        _ => {}
    }
    match (expected, got) {
        // Primitives exact match, with Int/UInt interchangeable.
        (
            TypeDescription::Int,
            TypeDescription::Int | TypeDescription::UInt | TypeDescription::Byte,
        )
        | (
            TypeDescription::UInt,
            TypeDescription::UInt | TypeDescription::Int | TypeDescription::Byte,
        )
        | (
            TypeDescription::Byte,
            TypeDescription::Byte | TypeDescription::Int | TypeDescription::UInt,
        )
        | (TypeDescription::Float, TypeDescription::Float)
        | (TypeDescription::Char, TypeDescription::Char)
        | (TypeDescription::Bool(_), TypeDescription::Bool(_))
        | (TypeDescription::String(_), TypeDescription::String(_)) => true,
        // Nominal types same DefId.
        (
            TypeDescription::TypeName { type_ref: a, .. },
            TypeDescription::TypeName { type_ref: b, .. },
        ) => a == b,
        // Tags same label.
        (TypeDescription::Tag(a), TypeDescription::Tag(b)) => a == b,
        // Composite.
        (TypeDescription::Array(a), TypeDescription::Array(b)) => {
            types_compatible(&a.desc, &b.desc)
        }
        (TypeDescription::Ref(a), TypeDescription::Ref(b))
        | (TypeDescription::RefMut(a), TypeDescription::RefMut(b))
        | (TypeDescription::Ref(a), TypeDescription::RefMut(b)) => {
            types_compatible(&a.desc, &b.desc)
        }
        (TypeDescription::Tuple(a_elems), TypeDescription::Tuple(b_elems)) => {
            a_elems.len() == b_elems.len()
                && a_elems
                    .iter()
                    .zip(b_elems.iter())
                    .all(|(a, b)| types_compatible(&a.desc, &b.desc))
        }
        (
            TypeDescription::Fun {
                params: pa,
                return_type: ra,
                ..
            },
            TypeDescription::Fun {
                params: pb,
                return_type: rb,
                ..
            },
        ) => {
            pa.len() == pb.len()
                && pa
                    .iter()
                    .zip(pb.iter())
                    .all(|(a, b)| types_compatible(&a.type_expr.desc, &b.type_expr.desc))
                && types_compatible(&ra.desc, &rb.desc)
        }
        // Union types compatible if `got` matches any variant, or if `expected` has
        // a variant that is compatible with `got`.
        (TypeDescription::Or(variants), got) => {
            variants.iter().any(|v| types_compatible(&v.desc, got))
        }
        (expected, TypeDescription::Or(variants)) => {
            variants.iter().any(|v| types_compatible(expected, &v.desc))
        }
        // Duck structural subtyping: every field required by `expected` must exist in
        // `got` with a compatible type.
        (TypeDescription::Duck(exp_duck), TypeDescription::Duck(got_duck)) => {
            exp_duck.fields.iter().all(|ef| {
                got_duck.fields.iter().any(|gf| {
                    gf.name.value == ef.name.value
                        && types_compatible(&ef.type_expr.desc, &gf.type_expr.desc)
                })
            })
        }
        _ => false,
    }
}

// Go type string helpers

/// Convert a Go type string (as produced by go/types) to a Duck TypeDescription.
fn go_type_str_to_type_desc(s: &str, span: Span) -> TypeDescription<Typed> {
    let s = s.trim();
    match s {
        "string" => TypeDescription::String(None),
        "int" | "int8" | "int16" | "int32" | "int64" | "rune" => TypeDescription::Int,
        "uint" | "uint16" | "uint32" | "uint64" | "uintptr" => TypeDescription::UInt,
        "uint8" | "byte" => TypeDescription::Byte,
        "float32" | "float64" => TypeDescription::Float,
        "bool" => TypeDescription::Bool(None),
        "any" | "interface{}" => TypeDescription::Any,
        _ => {
            if let Some(inner) = s.strip_prefix("[]") {
                return TypeDescription::Array(Box::new(TypeExpr::new(
                    go_type_str_to_type_desc(inner, span),
                    span,
                )));
            }
            TypeDescription::GoNamed(s.to_string())
        }
    }
}

fn go_type_str_to_expr(s: &str, span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(go_type_str_to_type_desc(s, span), span)
}

/// Convert a TypeScript type string to a Duck TypeDescription.
fn ts_type_str_to_type_desc(s: &str, span: Span) -> TypeDescription<Typed> {
    let s = s.trim();
    // Handle union: `A | B | C` - split on top-level `|`.
    // Fast path: if no `|` at top level, skip the union check.
    let variants = split_ts_union(s);
    if variants.len() > 1 {
        let filtered: Vec<&str> = variants
            .iter()
            .map(|v| v.trim())
            .filter(|v| !matches!(*v, "null" | "undefined"))
            .collect();
        if filtered.len() == 1 {
            return ts_type_str_to_type_desc(filtered[0], span);
        }
        let or_variants: Vec<TypeExpr<Typed>> = filtered
            .iter()
            .map(|v| TypeExpr::new(ts_type_str_to_type_desc(v, span), span))
            .collect();
        if !or_variants.is_empty() {
            return TypeDescription::Or(or_variants);
        }
    }
    // Strip trailing `[]` for arrays.
    if s.ends_with("[]") {
        let inner = &s[..s.len() - 2];
        return TypeDescription::Array(Box::new(TypeExpr::new(
            ts_type_str_to_type_desc(inner, span),
            span,
        )));
    }
    // `Array<T>` or `ReadonlyArray<T>`
    if let Some(inner) = s
        .strip_prefix("Array<")
        .and_then(|s| s.strip_suffix('>'))
        .or_else(|| {
            s.strip_prefix("ReadonlyArray<")
                .and_then(|s| s.strip_suffix('>'))
        })
    {
        return TypeDescription::Array(Box::new(TypeExpr::new(
            ts_type_str_to_type_desc(inner, span),
            span,
        )));
    }
    match s {
        "string" => TypeDescription::String(None),
        "number" | "bigint" => TypeDescription::Float,
        "boolean" => TypeDescription::Bool(None),
        "void" | "undefined" | "never" => TypeDescription::Statement,
        "null" | "any" | "unknown" | "object" => TypeDescription::Any,
        _ => TypeDescription::GoNamed(s.to_string()),
    }
}

fn ts_type_str_to_expr(s: &str, span: Span) -> TypeExpr<Typed> {
    TypeExpr::new(ts_type_str_to_type_desc(s, span), span)
}

/// Split a TypeScript type string on top-level `|` separators.
fn split_ts_union(s: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in s.char_indices() {
        match c {
            '<' | '(' | '[' | '{' => depth += 1,
            '>' | ')' | ']' | '}' => depth -= 1,
            '|' if depth == 0 => {
                parts.push(s[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    parts.push(s[start..].trim());
    parts
}

/// Build a Fun TypeExpr from a GoFuncInfo (required params only; variadic flag propagated).
fn go_func_info_to_fun_type(
    func_info: &crate::go_interop::GoFuncInfo,
    span: Span,
) -> TypeExpr<Typed> {
    let is_variadic = func_info.is_variadic();
    let min = func_info.min_args();
    let params: Vec<FunTypeParam<Typed>> = func_info.params[..min]
        .iter()
        .map(|p| FunTypeParam {
            label: None,
            type_expr: go_type_str_to_expr(&p.type_str, span),
        })
        .collect();
    let return_ty = match func_info.results.len() {
        0 => TypeExpr::new(TypeDescription::Statement, span),
        1 => go_type_str_to_expr(&func_info.results[0], span),
        _ => TypeExpr::new(
            TypeDescription::Tuple(
                func_info
                    .results
                    .iter()
                    .map(|r| go_type_str_to_expr(r, span))
                    .collect(),
            ),
            span,
        ),
    };
    TypeExpr::new(
        TypeDescription::Fun {
            params,
            return_type: Box::new(return_ty),
            is_mut: false,
            is_variadic,
        },
        span,
    )
}

/// Check whether concrete Go type `concrete_str` satisfies interface `interface_str`.
/// Uses the method sets loaded from the package data. Permissive on failure.
fn go_named_implements(concrete_str: &str, interface_str: &str) -> bool {
    let iface = interface_str.trim_start_matches('*').trim();
    let Some(idot) = iface.rfind('.') else {
        return true;
    };
    let iface_pkg = crate::go_interop::resolve_short_name(&iface[..idot]);
    let iface_type = &iface[idot + 1..];

    let Some(iface_data) = crate::go_interop::scan_package(&iface_pkg) else {
        return true;
    };
    let Some(iface_info) = iface_data.types.get(iface_type) else {
        return true;
    };
    if iface_info.kind != "interface" {
        return true;
    }
    if iface_info.methods.is_empty() {
        return true;
    }

    let concrete = concrete_str.trim_start_matches('*').trim();
    let Some(cdot) = concrete.rfind('.') else {
        return true;
    };
    let concrete_pkg = crate::go_interop::resolve_short_name(&concrete[..cdot]);
    let concrete_type = &concrete[cdot + 1..];

    let Some(concrete_data) = crate::go_interop::scan_package(&concrete_pkg) else {
        return true;
    };
    let Some(concrete_info) = concrete_data.types.get(concrete_type) else {
        return true;
    };

    iface_info
        .methods
        .keys()
        .all(|m| concrete_info.methods.contains_key(m))
}

impl Inferencer {
    fn new(
        symbols: SymbolTable,
        module_def_ids: HashSet<DefId>,
        global_scope: HashMap<String, DefId>,
    ) -> Self {
        let mut local_queues: HashMap<String, Vec<DefId>> = HashMap::new();
        for (id, def) in symbols.iter() {
            if matches!(
                def.kind,
                DefKind::Local { .. } | DefKind::Param { .. } | DefKind::Const
            ) {
                local_queues.entry(def.name.clone()).or_default().push(id);
            }
        }
        Self {
            symbols,
            errors: Vec::new(),
            current_return_type: None,
            local_queues,
            used_locals: HashSet::new(),
            struct_fields: HashMap::new(),
            extension_methods: HashMap::new(),
            struct_generic_params: HashMap::new(),
            module_def_ids,
            global_scope,
            suppress_errors: false,
            struct_derived: HashMap::new(),
            type_aliases: HashMap::new(),
        }
    }

    fn claim_local(&mut self, name: &str) -> Option<DefId> {
        let queue = self.local_queues.get(name)?;
        // module pass only claims module-origin slots (file_id != 1) to avoid
        // clobbering user-code DefIds with stdlib function bodies
        let id = *queue.iter().find(|id| {
            if self.used_locals.contains(id) {
                return false;
            }
            let is_user = self.symbols.get(**id).span.file_id == 1;
            if self.suppress_errors {
                !is_user
            } else {
                is_user
            }
        })?;
        self.used_locals.insert(id);
        Some(id)
    }

    fn error(&mut self, msg: impl Into<String>, span: Span) {
        if self.suppress_errors {
            return;
        }
        self.errors.push(TypeError {
            msg: msg.into(),
            span,
        });
    }

    /// Returns true if the type contains a generic type parameter (`DefKind::GenericParam`).
    /// Used to suppress compat checks involving unsubstituted generic type variables.
    fn type_contains_generic_param(&self, te: &TypeExpr<Typed>) -> bool {
        match &te.desc {
            TypeDescription::TypeName { type_ref, .. } => {
                matches!(self.symbols.get(*type_ref).kind, DefKind::GenericParam)
            }
            TypeDescription::Array(inner) => self.type_contains_generic_param(inner),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.type_contains_generic_param(inner)
            }
            TypeDescription::Or(variants) => {
                variants.iter().any(|v| self.type_contains_generic_param(v))
            }
            TypeDescription::Tuple(elems) => {
                elems.iter().any(|e| self.type_contains_generic_param(e))
            }
            TypeDescription::Fun {
                params,
                return_type,
                ..
            } => {
                self.type_contains_generic_param(return_type)
                    || params
                        .iter()
                        .any(|p| self.type_contains_generic_param(&p.type_expr))
            }
            _ => false,
        }
    }

    /// Extends types_compatible with duck-structural checks for named struct types.
    fn is_compatible(
        &self,
        expected: &TypeDescription<Typed>,
        got: &TypeDescription<Typed>,
    ) -> bool {
        // Expand type aliases on both sides before checking.
        if let TypeDescription::TypeName {
            type_ref,
            type_params,
        } = expected
        {
            if matches!(self.symbols.get(*type_ref).kind, DefKind::TypeAlias) {
                if let Some((generic_names, expanded)) = self.type_aliases.get(type_ref) {
                    let expanded_desc =
                        expand_alias_desc(expanded, generic_names, type_params, &self.symbols);
                    return self.is_compatible(&expanded_desc, got);
                }
            }
        }
        if let TypeDescription::TypeName {
            type_ref,
            type_params,
        } = got
        {
            if matches!(self.symbols.get(*type_ref).kind, DefKind::TypeAlias) {
                if let Some((generic_names, expanded)) = self.type_aliases.get(type_ref) {
                    let expanded_desc =
                        expand_alias_desc(expanded, generic_names, type_params, &self.symbols);
                    return self.is_compatible(expected, &expanded_desc);
                }
            }
        }
        // And type: got must satisfy all intersection members.
        if let TypeDescription::And(variants) = expected {
            return variants.iter().all(|v| self.is_compatible(&v.desc, got));
        }
        if let TypeDescription::Duck(exp_duck) = expected {
            // empty duck {} is the any type
            if exp_duck.fields.is_empty() {
                return true;
            }
            if let TypeDescription::TypeName { type_ref, .. } = got {
                if let Some(struct_fields) = self.struct_fields.get(type_ref) {
                    return exp_duck.fields.iter().all(|ef| {
                        struct_fields.iter().any(|gf| {
                            gf.name.value == ef.name.value
                                && types_compatible(&ef.type_expr.desc, &gf.type_expr.desc)
                        })
                    });
                }
            }
            // Duck vs Duck: check that got has all required fields.
            if let TypeDescription::Duck(got_duck) = got {
                return exp_duck.fields.iter().all(|ef| {
                    got_duck.fields.iter().any(|gf| {
                        gf.name.value == ef.name.value
                            && self.is_compatible(&ef.type_expr.desc, &gf.type_expr.desc)
                    })
                });
            }
        }
        types_compatible(expected, got)
    }

    /// Emit a type error if `got` is not compatible with `expected`.
    /// `ctx` names what is being checked (e.g. "argument", "return value").
    fn check_compat(
        &mut self,
        expected: &TypeExpr<Typed>,
        got: &TypeExpr<Typed>,
        span: Span,
        ctx: &str,
    ) {
        // The inferencer doesn't substitute type params, so skip checks where either
        // side contains an unresolved generic type variable.
        if self.type_contains_generic_param(expected) || self.type_contains_generic_param(got) {
            return;
        }
        let compatible = self.is_compatible(&expected.desc, &got.desc);
        if !compatible {
            self.error(
                format!(
                    "type mismatch in {ctx}: expected `{}`, got `{}`",
                    type_name_sym(expected, &self.symbols),
                    type_name_sym(got, &self.symbols)
                ),
                span,
            );
        }
    }

    fn sym_ty(&self, id: DefId, span: Span) -> TypeExpr<Typed> {
        self.symbols.get(id).ty.clone().unwrap_or_else(|| any(span))
    }

    fn collect_signatures(
        &mut self,
        items: &[Item<Resolved>],
        global_scope: &HashMap<String, DefId>,
    ) {
        for item in items {
            match item {
                Item::Function(f) => {
                    let Some(&fn_id) = global_scope.get(&f.name.value) else {
                        continue;
                    };

                    let param_types: Vec<FunTypeParam<Typed>> = f
                        .params
                        .iter()
                        .map(|p| FunTypeParam {
                            label: Some(p.name.clone()),
                            type_expr: type_expr_to_typed(p.type_expr.clone()),
                        })
                        .collect();

                    let return_type = f
                        .return_type
                        .as_ref()
                        .map(|te| type_expr_to_typed(te.clone()))
                        .unwrap_or_else(|| stmt(f.span));

                    let fn_ty = TypeExpr::new(
                        TypeDescription::Fun {
                            params: param_types,
                            return_type: Box::new(return_type),
                            is_mut: false,
                            is_variadic: false,
                        },
                        f.span,
                    );

                    self.symbols.get_mut(fn_id).ty = Some(fn_ty);
                }
                Item::Struct(s) => {
                    let Some(&struct_id) = global_scope.get(&s.name.value) else {
                        continue;
                    };
                    let fields = s
                        .fields
                        .iter()
                        .map(|f| Field {
                            name: f.name.clone(),
                            type_expr: type_expr_to_typed(f.type_expr.clone()),
                        })
                        .collect();
                    self.struct_fields.insert(struct_id, fields);
                    self.struct_derived.insert(struct_id, s.derived.clone());
                }
                Item::Extension(ext) => {
                    // Resolve the target type to a struct DefId so we can index by it.
                    let target_typed = type_expr_to_typed(ext.target.clone());
                    let (struct_id, target_type_params) = match &target_typed.desc {
                        TypeDescription::TypeName {
                            type_ref,
                            type_params,
                        } => (*type_ref, type_params.clone()),
                        _ => continue,
                    };
                    // Record ordered generic param names so lookup_field_type can substitute.
                    if !target_type_params.is_empty() {
                        let param_names: Vec<String> = target_type_params
                            .iter()
                            .filter_map(|tp| match &tp.desc {
                                // After resolution the target type_params are still TemplParam("T").
                                TypeDescription::TemplParam(name) => Some(name.clone()),
                                // Fallback: if already resolved to a GenericParam TypeName.
                                TypeDescription::TypeName {
                                    type_ref,
                                    type_params,
                                } if type_params.is_empty() => {
                                    if matches!(
                                        self.symbols.get(*type_ref).kind,
                                        DefKind::GenericParam
                                    ) {
                                        Some(self.symbols.get(*type_ref).name.clone())
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .collect();
                        self.struct_generic_params
                            .entry(struct_id)
                            .or_insert(param_names);
                    }
                    let map = self.extension_methods.entry(struct_id).or_default();
                    for m in &ext.methods {
                        let param_types: Vec<FunTypeParam<Typed>> = m
                            .params
                            .iter()
                            .map(|p| FunTypeParam {
                                label: Some(p.name.clone()),
                                type_expr: type_expr_to_typed(p.type_expr.clone()),
                            })
                            .collect();
                        let return_type = m
                            .return_type
                            .as_ref()
                            .map(|te| type_expr_to_typed(te.clone()))
                            .unwrap_or_else(|| stmt(m.span));
                        let fn_ty = TypeExpr::new(
                            TypeDescription::Fun {
                                params: param_types,
                                return_type: Box::new(return_type),
                                is_mut: false,
                                is_variadic: false,
                            },
                            m.span,
                        );
                        map.insert(m.name.value.clone(), fn_ty);
                    }
                }
                Item::TypeAlias(a) => {
                    if let Some(&alias_id) = global_scope.get(&a.name.value) {
                        let generic_names: Vec<String> = a
                            .generics
                            .iter()
                            .map(|g| g.value.name.value.clone())
                            .collect();
                        self.type_aliases.insert(
                            alias_id,
                            (generic_names, type_expr_to_typed(a.type_expr.clone())),
                        );
                    }
                }
                _ => {}
            }
        }
    }

    fn lookup_field_type(&self, ty: &TypeExpr<Typed>, field_name: &str) -> Option<TypeExpr<Typed>> {
        match &ty.desc {
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } => {
                if let Some(fields) = self.struct_fields.get(type_ref) {
                    if let Some(field) = fields.iter().find(|f| f.name.value == field_name) {
                        return Some(field.type_expr.clone());
                    }
                }
                let method_ty = self
                    .extension_methods
                    .get(type_ref)
                    .and_then(|m| m.get(field_name))
                    .cloned();
                if let Some(ty) = method_ty {
                    if !type_params.is_empty() {
                        if let Some(param_names) = self.struct_generic_params.get(type_ref).cloned()
                        {
                            let subs: HashMap<String, TypeExpr<Typed>> = param_names
                                .iter()
                                .zip(type_params.iter())
                                .map(|(name, concrete)| (name.clone(), concrete.clone()))
                                .collect();
                            return Some(super::mono::subst_type(&ty, &subs, &self.symbols));
                        }
                    }
                    return Some(ty);
                }
                if let Some(derived) = self.struct_derived.get(type_ref) {
                    use crate::parse::struct_parser::DerivableInterface;
                    let span = ty.span;
                    let self_ty = TypeExpr::new(
                        TypeDescription::TypeName {
                            type_ref: *type_ref,
                            type_params: type_params.clone(),
                        },
                        span,
                    );
                    let fun = |params: Vec<FunTypeParam<Typed>>, ret: TypeExpr<Typed>| {
                        TypeExpr::new(
                            TypeDescription::Fun {
                                params,
                                return_type: Box::new(ret),
                                is_mut: false,
                                is_variadic: false,
                            },
                            span,
                        )
                    };
                    let other_param = || FunTypeParam {
                        label: Some(WithSpan::new("other".into(), span)),
                        type_expr: any(span),
                    };
                    match field_name {
                        "clone" if derived.contains(&DerivableInterface::Clone) => {
                            return Some(fun(vec![], self_ty));
                        }
                        "to_string" if derived.contains(&DerivableInterface::ToString) => {
                            return Some(fun(
                                vec![],
                                TypeExpr::new(TypeDescription::String(None), span),
                            ));
                        }
                        "to_json" if derived.contains(&DerivableInterface::ToJson) => {
                            return Some(fun(
                                vec![],
                                TypeExpr::new(TypeDescription::String(None), span),
                            ));
                        }
                        "eq" if derived.contains(&DerivableInterface::Eq) => {
                            return Some(fun(
                                vec![other_param()],
                                TypeExpr::new(TypeDescription::Bool(None), span),
                            ));
                        }
                        "hash" if derived.contains(&DerivableInterface::Hash) => {
                            return Some(fun(vec![], TypeExpr::new(TypeDescription::Int, span)));
                        }
                        "ord" if derived.contains(&DerivableInterface::Ord) => {
                            return Some(fun(vec![other_param()], any(span)));
                        }
                        _ => {}
                    }
                }
                // Unresolved generic param: to_json/to_string are always available.
                if matches!(self.symbols.get(*type_ref).kind, DefKind::GenericParam) {
                    let span = ty.span;
                    return match field_name {
                        "to_string" | "to_json" => Some(TypeExpr::new(
                            TypeDescription::Fun {
                                params: vec![],
                                return_type: Box::new(TypeExpr::new(
                                    TypeDescription::String(None),
                                    span,
                                )),
                                is_mut: false,
                                is_variadic: false,
                            },
                            span,
                        )),
                        _ => None,
                    };
                }
                // Type alias: expand and recurse.
                if matches!(self.symbols.get(*type_ref).kind, DefKind::TypeAlias) {
                    if let Some((generic_names, expanded)) = self.type_aliases.get(type_ref) {
                        let expanded_desc =
                            expand_alias_desc(expanded, generic_names, type_params, &self.symbols);
                        let expanded_te = TypeExpr::new(expanded_desc, ty.span);
                        return self.lookup_field_type(&expanded_te, field_name);
                    }
                }
                None
            }
            TypeDescription::Tuple(elems) => {
                let span = ty.span;
                match field_name {
                    "to_string" | "to_json" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(
                                TypeDescription::String(None),
                                span,
                            )),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    _ => field_name
                        .parse::<usize>()
                        .ok()
                        .and_then(|i| elems.get(i))
                        .cloned(),
                }
            }
            TypeDescription::Duck(duck) => {
                let span = ty.span;
                match field_name {
                    "to_string" | "to_json" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(
                                TypeDescription::String(None),
                                span,
                            )),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    _ => duck
                        .fields
                        .iter()
                        .find(|f| f.name.value == field_name)
                        .map(|f| f.type_expr.clone()),
                }
            }
            TypeDescription::GoPackage(import_path) => {
                let span = ty.span;
                // TypeScript package: import_path starts with "ts:".
                if let Some(ts_pkg) = import_path.strip_prefix("ts:") {
                    if let Some(data) = crate::ts_interop::get_package(ts_pkg) {
                        use crate::go_interop::GoMember;
                        return match data.lookup_member(field_name) {
                            GoMember::Func(f) => Some(go_func_info_to_fun_type(f, span)),
                            GoMember::Var(type_str) => Some(ts_type_str_to_expr(type_str, span)),
                            GoMember::Type => Some(any(span)),
                            GoMember::Unknown => None,
                        };
                    }
                    return None; // Package not found - strict: let the Field handler report the error.
                }
                match crate::go_interop::scan_package(import_path) {
                    Some(data) => {
                        use crate::go_interop::GoMember;
                        match data.lookup_member(field_name) {
                            GoMember::Func(f) => Some(go_func_info_to_fun_type(f, span)),
                            GoMember::Var(type_str) => Some(go_type_str_to_expr(type_str, span)),
                            GoMember::Type => Some(any(span)),
                            GoMember::Unknown => None, // Field handler will emit an error.
                        }
                    }
                    // Extractor unavailable be permissive.
                    None => Some(any(span)),
                }
            }
            TypeDescription::GoNamed(type_str) => {
                let span = ty.span;
                // Parse "*pkg.TypeName" or "pkg.TypeName" to find struct fields / methods.
                let inner = type_str.trim_start_matches('*').trim();
                if let Some(dot) = inner.rfind('.') {
                    let pkg_short = &inner[..dot];
                    let type_name_str = &inner[dot + 1..];
                    let import_path = crate::go_interop::resolve_short_name(pkg_short);
                    if let Some(data) = crate::go_interop::scan_package(&import_path) {
                        if let Some(type_info) = data.types.get(type_name_str) {
                            if let Some(field) = type_info.fields.get(field_name) {
                                return Some(go_type_str_to_expr(&field.type_str, span));
                            }
                            if let Some(method) = type_info.methods.get(field_name) {
                                return Some(go_func_info_to_fun_type(method, span));
                            }
                            // Known type, unknown member - return None so the Field handler reports an error.
                            return None;
                        }
                    }
                }
                // Type not resolved be permissive.
                Some(any(span))
            }
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.lookup_field_type(inner, field_name)
            }
            TypeDescription::Array(_) => {
                let span = ty.span;
                match field_name {
                    "len" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(TypeDescription::Int, span)),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    "to_string" | "to_json" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(
                                TypeDescription::String(None),
                                span,
                            )),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    _ => None,
                }
            }
            TypeDescription::Int
            | TypeDescription::UInt
            | TypeDescription::Float
            | TypeDescription::Bool(_)
            | TypeDescription::String(_)
            | TypeDescription::Char
            | TypeDescription::Byte => {
                let span = ty.span;
                match field_name {
                    "to_string" | "to_json" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(
                                TypeDescription::String(None),
                                span,
                            )),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    _ => None,
                }
            }
            TypeDescription::Or(_) | TypeDescription::And(_) => {
                let span = ty.span;
                match field_name {
                    "to_string" | "to_json" => Some(TypeExpr::new(
                        TypeDescription::Fun {
                            params: vec![],
                            return_type: Box::new(TypeExpr::new(
                                TypeDescription::String(None),
                                span,
                            )),
                            is_mut: false,
                            is_variadic: false,
                        },
                        span,
                    )),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn infer_source_file(
        &mut self,
        sf: SourceFile<Resolved>,
        global_scope: &HashMap<String, DefId>,
    ) -> SourceFile<Typed> {
        self.collect_signatures(&sf.items, global_scope);
        let items = sf
            .items
            .into_iter()
            .map(|item| self.infer_item(item))
            .collect();
        SourceFile { items }
    }

    fn is_module_item(&self, item: &Item<Resolved>) -> bool {
        match item {
            Item::Function(f) => self
                .global_scope
                .get(&f.name.value)
                .map_or(false, |id| self.module_def_ids.contains(id)),
            Item::Extension(e) => {
                // Extensions from module files are always module items regardless of
                // what their target type resolves to (which may differ if a user-defined
                // symbol shadowed the module name).
                if e.span.file_id != 1 {
                    return true;
                }
                let target_typed =
                    crate::semantics2::resolver::type_expr_to_typed(e.target.clone());
                match &target_typed.desc {
                    TypeDescription::TypeName { type_ref, .. } => {
                        self.module_def_ids.contains(type_ref)
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    fn infer_item(&mut self, item: Item<Resolved>) -> Item<Typed> {
        let was_suppressed = self.suppress_errors;
        if self.is_module_item(&item) {
            self.suppress_errors = true;
        }
        let result = self.infer_item_inner(item);
        self.suppress_errors = was_suppressed;
        result
    }

    fn infer_item_inner(&mut self, item: Item<Resolved>) -> Item<Typed> {
        match item {
            Item::Function(f) => Item::Function(self.infer_function(f)),
            Item::TypeAlias(a) => Item::TypeAlias(TypeAliasDecl {
                name: a.name,
                generics: infer_generics(a.generics),
                type_expr: type_expr_to_typed(a.type_expr),
                span: a.span,
            }),
            Item::Struct(s) => Item::Struct(StructDecl {
                name: s.name,
                generics: infer_generics(s.generics),
                fields: s
                    .fields
                    .into_iter()
                    .map(|f| Field {
                        name: f.name,
                        type_expr: type_expr_to_typed(f.type_expr),
                    })
                    .collect(),
                derived: s.derived,
                span: s.span,
            }),
            Item::Use(u) => Item::Use(u),
            Item::Extension(e) => Item::Extension(self.infer_extension(e)),
        }
    }

    fn infer_function(&mut self, f: FunctionDecl<Resolved>) -> FunctionDecl<Typed> {
        let saved = self.current_return_type.take();
        self.current_return_type = f
            .return_type
            .as_ref()
            .map(|te| type_expr_to_typed(te.clone()));

        let generics = infer_generics(f.generics);
        let params: Vec<Param<Typed>> = f
            .params
            .into_iter()
            .map(|p| Param {
                name: p.name,
                type_expr: type_expr_to_typed(p.type_expr),
                is_mut: p.is_mut,
            })
            .collect();
        // Consume param slots so nested lambdas with same-named params claim the right DefIds.
        for p in &params {
            self.claim_local(&p.name.value);
        }
        let return_type = f.return_type.map(type_expr_to_typed);
        let body = self.infer_expr(f.body);

        // Check body expression type against declared return type.
        // Only check non-statement bodies (expression-returning functions).
        if let Some(ret_ty) = &return_type {
            let ret_ty = ret_ty.clone();
            if !matches!(
                body.ty.desc,
                TypeDescription::Statement | TypeDescription::Never
            ) {
                self.check_compat(&ret_ty, &body.ty, body.span, "function body");
            }
        }

        self.current_return_type = saved;

        FunctionDecl {
            name: f.name,
            generics,
            params,
            return_type,
            body,
            is_static: f.is_static,
            is_client: f.is_client,
            span: f.span,
        }
    }

    fn infer_extension(&mut self, ext: ExtensionDecl<Resolved>) -> ExtensionDecl<Typed> {
        ExtensionDecl {
            target: type_expr_to_typed(ext.target),
            methods: ext
                .methods
                .into_iter()
                .map(|m| self.infer_function(m))
                .collect(),
            span: ext.span,
        }
    }

    fn infer_jsx_node(&mut self, node: JsxNode<Resolved>) -> JsxNode<Typed> {
        match node {
            JsxNode::Text(s) => JsxNode::Text(s),
            JsxNode::Expr(e) => JsxNode::Expr(Box::new(self.infer_expr(*e))),
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
                                JsxAttrValue::Expr(Box::new(self.infer_expr(*e)))
                            }
                        },
                    })
                    .collect(),
                children: children
                    .into_iter()
                    .map(|c| self.infer_jsx_node(c))
                    .collect(),
            },
        }
    }

    fn infer_expr(&mut self, e: Expr<Resolved>) -> Expr<Typed> {
        let span = e.span;
        let (kind, ty) = self.infer_expr_kind(e.kind, span);
        Expr { kind, ty, span }
    }

    fn infer_expr_kind(
        &mut self,
        kind: ExprKind<Resolved>,
        span: Span,
    ) -> (ExprKind<Typed>, TypeExpr<Typed>) {
        match kind {
            ExprKind::Int(v) => (ExprKind::Int(v), TypeExpr::new(TypeDescription::Int, span)),
            ExprKind::Float(v) => (
                ExprKind::Float(v),
                TypeExpr::new(TypeDescription::Float, span),
            ),
            ExprKind::Bool(v) => (ExprKind::Bool(v), bool_ty(span)),
            ExprKind::Char(v) => (
                ExprKind::Char(v),
                TypeExpr::new(TypeDescription::Char, span),
            ),
            ExprKind::String(v) => (
                ExprKind::String(v),
                TypeExpr::new(TypeDescription::String(None), span),
            ),
            ExprKind::Tag(v) => (
                ExprKind::Tag(v.clone()),
                TypeExpr::new(TypeDescription::Tag(v), span),
            ),
            ExprKind::InlineGo(v) => (ExprKind::InlineGo(v), any(span)),
            ExprKind::Jsx(node) => (
                ExprKind::Jsx(Box::new(self.infer_jsx_node(*node))),
                any(span),
            ),
            ExprKind::Break => (ExprKind::Break, never(span)),
            ExprKind::Continue => (ExprKind::Continue, never(span)),

            ExprKind::FmtString(parts) => {
                let parts = parts
                    .into_iter()
                    .map(|p| match p {
                        FmtPart::Literal(s) => FmtPart::Literal(s),
                        FmtPart::Expr(e) => FmtPart::Expr(self.infer_expr(e)),
                    })
                    .collect();
                (
                    ExprKind::FmtString(parts),
                    TypeExpr::new(TypeDescription::String(None), span),
                )
            }

            ExprKind::Ident(id) => {
                let ty = if matches!(self.symbols.get(id).kind, DefKind::Struct) {
                    TypeExpr::new(
                        TypeDescription::TypeName {
                            type_ref: id,
                            type_params: vec![],
                        },
                        span,
                    )
                } else {
                    self.sym_ty(id, span)
                };
                (ExprKind::Ident(id), ty)
            }

            ExprKind::Block(stmts) => {
                let stmts: Vec<Expr<Typed>> =
                    stmts.into_iter().map(|s| self.infer_expr(s)).collect();
                // propagate Never if any stmt diverges - trailing unit sentinel is dead code
                let ty = if stmts
                    .iter()
                    .any(|e| matches!(e.ty.desc, TypeDescription::Never))
                {
                    never(span)
                } else {
                    stmts
                        .last()
                        .map(|e| e.ty.clone())
                        .unwrap_or_else(|| stmt(span))
                };
                (ExprKind::Block(stmts), ty)
            }

            ExprKind::Let {
                is_mut,
                name,
                type_ann,
                value,
            } => {
                let type_ann = type_ann.map(type_expr_to_typed);
                // Use the annotation as a hint when inferring lambdas.
                let value = Box::new(self.infer_expr_with_expected(*value, type_ann.as_ref()));
                // Check annotation against actual inferred type.
                if let Some(ann) = &type_ann {
                    self.check_compat(ann, &value.ty, value.span, "variable declaration");
                }
                let inferred = type_ann.clone().unwrap_or_else(|| value.ty.clone());
                if let Some(id) = self.claim_local(&name.value) {
                    self.symbols.get_mut(id).ty = Some(inferred);
                }
                (
                    ExprKind::Let {
                        is_mut,
                        name,
                        type_ann,
                        value,
                    },
                    stmt(span),
                )
            }

            ExprKind::LetTuple { names, value } => {
                let value = Box::new(self.infer_expr(*value));
                if let TypeDescription::Tuple(elems) = &value.ty.desc {
                    for (name, elem_ty) in names.iter().zip(elems.iter()) {
                        if let Some(id) = self.claim_local(&name.value) {
                            self.symbols.get_mut(id).ty = Some(elem_ty.clone());
                        }
                    }
                } else {
                    // fallback: assign Any to each name
                    for name in &names {
                        if let Some(id) = self.claim_local(&name.value) {
                            self.symbols.get_mut(id).ty = Some(any(span));
                        }
                    }
                }
                (ExprKind::LetTuple { names, value }, stmt(span))
            }

            ExprKind::Const {
                name,
                type_ann,
                value,
            } => {
                let value = Box::new(self.infer_expr(*value));
                let type_ann = type_ann.map(type_expr_to_typed);
                let inferred = type_ann.clone().unwrap_or_else(|| value.ty.clone());
                if let Some(id) = self.claim_local(&name.value) {
                    self.symbols.get_mut(id).ty = Some(inferred);
                }
                (
                    ExprKind::Const {
                        name,
                        type_ann,
                        value,
                    },
                    stmt(span),
                )
            }

            ExprKind::Assign { target, value } => {
                let target = Box::new(self.infer_expr(*target));
                let value = Box::new(self.infer_expr(*value));
                self.check_compat(&target.ty, &value.ty, value.span, "assignment");
                (ExprKind::Assign { target, value }, stmt(span))
            }
            ExprKind::AddAssign { target, value } => (
                ExprKind::AddAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::SubAssign { target, value } => (
                ExprKind::SubAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::MulAssign { target, value } => (
                ExprKind::MulAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::DivAssign { target, value } => (
                ExprKind::DivAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::ModAssign { target, value } => (
                ExprKind::ModAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::ShrAssign { target, value } => (
                ExprKind::ShrAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),
            ExprKind::ShlAssign { target, value } => (
                ExprKind::ShlAssign {
                    target: Box::new(self.infer_expr(*target)),
                    value: Box::new(self.infer_expr(*value)),
                },
                stmt(span),
            ),

            ExprKind::Add(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                // Allow numeric + numeric, or String + String (concatenation).
                let is_addable = |te: &TypeExpr<Typed>| {
                    is_numeric(te) || matches!(te.desc, TypeDescription::String(_))
                };
                if !is_addable(&a.ty) {
                    self.error(
                        format!(
                            "`+` requires a numeric type or String, got `{}`",
                            type_name(&a.ty)
                        ),
                        a.span,
                    );
                }
                if !is_addable(&b.ty) {
                    self.error(
                        format!(
                            "`+` requires a numeric type or String, got `{}`",
                            type_name(&b.ty)
                        ),
                        b.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Add(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::Sub(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_numeric(&a.ty) {
                    self.error(
                        format!("`-` requires a numeric type, got `{}`", type_name(&a.ty)),
                        a.span,
                    );
                }
                if !is_numeric(&b.ty) {
                    self.error(
                        format!("`-` requires a numeric type, got `{}`", type_name(&b.ty)),
                        b.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Sub(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::Mul(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_numeric(&a.ty) {
                    self.error(
                        format!("`*` requires a numeric type, got `{}`", type_name(&a.ty)),
                        a.span,
                    );
                }
                if !is_numeric(&b.ty) {
                    self.error(
                        format!("`*` requires a numeric type, got `{}`", type_name(&b.ty)),
                        b.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Mul(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::Div(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_numeric(&a.ty) {
                    self.error(
                        format!("`/` requires a numeric type, got `{}`", type_name(&a.ty)),
                        a.span,
                    );
                }
                if !is_numeric(&b.ty) {
                    self.error(
                        format!("`/` requires a numeric type, got `{}`", type_name(&b.ty)),
                        b.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Div(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::Mod(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_numeric(&a.ty) {
                    self.error(
                        format!("`%` requires a numeric type, got `{}`", type_name(&a.ty)),
                        a.span,
                    );
                }
                if !is_numeric(&b.ty) {
                    self.error(
                        format!("`%` requires a numeric type, got `{}`", type_name(&b.ty)),
                        b.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Mod(Box::new(a), Box::new(b)), ty)
            }

            ExprKind::BitAnd(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_int(&a.ty) {
                    self.error("bitwise `&` requires Int or UInt", a.span);
                }
                if !is_int(&b.ty) {
                    self.error("bitwise `&` requires Int or UInt", b.span);
                }
                let ty = a.ty.clone();
                (ExprKind::BitAnd(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::BitOr(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_int(&a.ty) {
                    self.error("bitwise `|` requires Int or UInt", a.span);
                }
                if !is_int(&b.ty) {
                    self.error("bitwise `|` requires Int or UInt", b.span);
                }
                let ty = a.ty.clone();
                (ExprKind::BitOr(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::BitXor(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_int(&a.ty) {
                    self.error("bitwise `^` requires Int or UInt", a.span);
                }
                if !is_int(&b.ty) {
                    self.error("bitwise `^` requires Int or UInt", b.span);
                }
                let ty = a.ty.clone();
                (ExprKind::BitXor(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::BitNot(a) => {
                let a = self.infer_expr(*a);
                if !is_int(&a.ty) {
                    self.error("bitwise `~` requires Int or UInt", a.span);
                }
                let ty = a.ty.clone();
                (ExprKind::BitNot(Box::new(a)), ty)
            }
            ExprKind::Shl(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_int(&a.ty) {
                    self.error("shift `<<` requires Int or UInt", a.span);
                }
                if !is_int(&b.ty) {
                    self.error("shift amount must be Int or UInt", b.span);
                }
                let ty = a.ty.clone();
                (ExprKind::Shl(Box::new(a), Box::new(b)), ty)
            }
            ExprKind::Shr(a, b) => {
                let a = self.infer_expr(*a);
                let b = self.infer_expr(*b);
                if !is_int(&a.ty) {
                    self.error("shift `>>` requires Int or UInt", a.span);
                }
                if !is_int(&b.ty) {
                    self.error("shift amount must be Int or UInt", b.span);
                }
                let ty = a.ty.clone();
                (ExprKind::Shr(Box::new(a), Box::new(b)), ty)
            }

            ExprKind::Eq(a, b) => (
                ExprKind::Eq(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Neq(a, b) => (
                ExprKind::Neq(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Lt(a, b) => (
                ExprKind::Lt(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Lte(a, b) => (
                ExprKind::Lte(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Gt(a, b) => (
                ExprKind::Gt(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Gte(a, b) => (
                ExprKind::Gte(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),

            ExprKind::And(a, b) => (
                ExprKind::And(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Or(a, b) => (
                ExprKind::Or(Box::new(self.infer_expr(*a)), Box::new(self.infer_expr(*b))),
                bool_ty(span),
            ),
            ExprKind::Not(a) => (ExprKind::Not(Box::new(self.infer_expr(*a))), bool_ty(span)),

            ExprKind::Neg(a) => {
                let a = self.infer_expr(*a);
                if !is_numeric(&a.ty) {
                    self.error(
                        format!(
                            "cannot negate `{}`: expected Int, UInt, or Float",
                            type_name(&a.ty)
                        ),
                        a.span,
                    );
                }
                let ty = a.ty.clone();
                (ExprKind::Neg(Box::new(a)), ty)
            }
            ExprKind::Ref(a) => {
                let a = self.infer_expr(*a);
                let ty = TypeExpr::new(TypeDescription::Ref(Box::new(a.ty.clone())), span);
                (ExprKind::Ref(Box::new(a)), ty)
            }
            ExprKind::RefMut(a) => {
                let a = self.infer_expr(*a);
                let ty = TypeExpr::new(TypeDescription::RefMut(Box::new(a.ty.clone())), span);
                (ExprKind::RefMut(Box::new(a)), ty)
            }
            ExprKind::Deref(a) => {
                let a = self.infer_expr(*a);
                let ty = match &a.ty.desc {
                    TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => *inner.clone(),
                    _ => any(span),
                };
                (ExprKind::Deref(Box::new(a)), ty)
            }

            ExprKind::Field { base, field } => {
                let base = Box::new(self.infer_expr(*base));
                let field_ty = match self.lookup_field_type(&base.ty, &field.value) {
                    Some(ty) => ty,
                    None => {
                        // Suppress only for fully-opaque types (no package data available).
                        // GoNamed stays in the error path when we have package data (lookup returns None).
                        if !matches!(
                            base.ty.desc,
                            TypeDescription::Any | TypeDescription::Statement
                        ) {
                            let msg = match &base.ty.desc {
                                TypeDescription::GoPackage(pkg) => {
                                    if let Some(ts_pkg) = pkg.strip_prefix("ts:") {
                                        if crate::ts_interop::get_package(ts_pkg).is_none() {
                                            format!(
                                                "TypeScript package `{ts_pkg}` has no type info loaded - is it installed in node_modules?"
                                            )
                                        } else {
                                            format!(
                                                "TypeScript package `{ts_pkg}` has no exported member `{}`",
                                                field.value
                                            )
                                        }
                                    } else {
                                        format!(
                                            "package `{pkg}` has no exported member `{}`",
                                            field.value
                                        )
                                    }
                                }
                                _ => format!(
                                    "type `{}` has no field `{}`",
                                    type_name(&base.ty),
                                    field.value
                                ),
                            };
                            self.error(msg, field.span);
                        }
                        any(span)
                    }
                };
                (ExprKind::Field { base, field }, field_ty)
            }
            ExprKind::Index { base, index } => {
                let base = Box::new(self.infer_expr(*base));
                let ty = match &base.ty.desc {
                    TypeDescription::Array(elem) => *elem.clone(),
                    _ => any(span),
                };
                let index = Box::new(self.infer_expr(*index));
                (ExprKind::Index { base, index }, ty)
            }
            ExprKind::ScopeRes { base, member } => {
                let base = Box::new(self.infer_expr(*base));
                let member_ty = self
                    .lookup_field_type(&base.ty, &member.value)
                    .unwrap_or_else(|| any(span));
                (ExprKind::ScopeRes { base, member }, member_ty)
            }

            ExprKind::Call {
                callee,
                type_params,
                args,
            } => {
                let callee = Box::new(self.infer_expr(*callee));
                let type_params: Vec<TypeExpr<Typed>> =
                    type_params.into_iter().map(type_expr_to_typed).collect();
                let mut is_variadic_call = false;
                let (mut return_ty, mut callee_param_tys) = match &callee.ty.desc {
                    TypeDescription::Fun {
                        return_type,
                        params,
                        is_variadic,
                        ..
                    } => {
                        is_variadic_call = *is_variadic;
                        let param_tys = params.iter().map(|p| p.type_expr.clone()).collect();
                        (*return_type.clone(), param_tys)
                    }
                    // Any and GoPackage are opaque allow calls without validation.
                    TypeDescription::Any | TypeDescription::GoPackage(_) => (any(span), vec![]),
                    other => {
                        self.error(
                            format!(
                                "type `{}` is not callable",
                                type_name(&TypeExpr::new(other.clone(), span))
                            ),
                            callee.span,
                        );
                        (any(span), vec![])
                    }
                };
                // If the callee is a generic function (has free GenericParam DefIds) and
                // call-site type_params are provided, substitute them in the return and param types.
                if !type_params.is_empty() {
                    let mut generic_ids: Vec<DefId> = Vec::new();
                    collect_free_generic_params(&callee.ty, &self.symbols, &mut generic_ids);
                    if generic_ids.len() == type_params.len() {
                        let subs: HashMap<String, TypeExpr<Typed>> = generic_ids
                            .iter()
                            .zip(type_params.iter())
                            .map(|(id, tp)| (self.symbols.get(*id).name.clone(), tp.clone()))
                            .collect();
                        return_ty = super::mono::subst_type(&return_ty, &subs, &self.symbols);
                        callee_param_tys = callee_param_tys
                            .iter()
                            .map(|p| super::mono::subst_type(p, &subs, &self.symbols))
                            .collect();
                    }
                }
                // Arg count check (only when we know the expected count).
                let n_expected = callee_param_tys.len();
                let n_got = args.len();
                if !callee_param_tys.is_empty() {
                    if is_variadic_call {
                        if n_got < n_expected {
                            self.error(
                                format!("expected at least {n_expected} argument(s), got {n_got}"),
                                span,
                            );
                        }
                    } else if n_got != n_expected {
                        self.error(
                            format!("expected {n_expected} argument(s), got {n_got}"),
                            span,
                        );
                    }
                }
                let args: Vec<Expr<Typed>> = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let inferred = self.infer_expr_with_expected(a, callee_param_tys.get(i));
                        if let Some(expected_ty) = callee_param_tys.get(i) {
                            let expected_ty = expected_ty.clone();
                            self.check_compat(
                                &expected_ty,
                                &inferred.ty,
                                inferred.span,
                                "argument",
                            );
                        }
                        inferred
                    })
                    .collect();
                (
                    ExprKind::Call {
                        callee,
                        type_params,
                        args,
                    },
                    return_ty,
                )
            }

            ExprKind::As { value, type_expr } => {
                let value = Box::new(self.infer_expr(*value));
                let type_expr = type_expr_to_typed(type_expr);
                let ty = type_expr.clone();
                (ExprKind::As { value, type_expr }, ty)
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = Box::new(self.infer_expr(*condition));
                let then_branch = Box::new(self.infer_expr(*then_branch));
                let ty = then_branch.ty.clone();
                let else_branch = else_branch.map(|e| Box::new(self.infer_expr(*e)));
                (
                    ExprKind::If {
                        condition,
                        then_branch,
                        else_branch,
                    },
                    ty,
                )
            }

            ExprKind::While { condition, body } => {
                let condition = Box::new(self.infer_expr(*condition));
                let body = Box::new(self.infer_expr(*body));
                (ExprKind::While { condition, body }, stmt(span))
            }

            ExprKind::For {
                binding,
                is_mut,
                iterable,
                body,
            } => {
                let iterable = Box::new(self.infer_expr(*iterable));
                let body = Box::new(self.infer_expr(*body));
                (
                    ExprKind::For {
                        binding,
                        is_mut,
                        iterable,
                        body,
                    },
                    stmt(span),
                )
            }

            ExprKind::Return(v) => {
                let v = v.map(|e| Box::new(self.infer_expr(*e)));
                if let (Some(val), Some(ret_ty)) = (&v, self.current_return_type.clone()) {
                    self.check_compat(&ret_ty, &val.ty, val.span, "return value");
                }
                (ExprKind::Return(v), never(span))
            }

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => {
                let value = Box::new(self.infer_expr(*value));
                let arms: Vec<MatchArm<Typed>> = arms
                    .into_iter()
                    .map(|arm| self.infer_match_arm(arm))
                    .collect();
                let else_arm = else_arm.map(|e| Box::new(self.infer_expr(*e)));
                let is_never = |ty: &TypeExpr<Typed>| {
                    matches!(ty.desc, TypeDescription::Never | TypeDescription::Statement)
                };
                let non_never: Vec<TypeExpr<Typed>> = arms
                    .iter()
                    .map(|a| &a.body.ty)
                    .chain(else_arm.as_ref().map(|e| &e.ty))
                    .filter(|t| !is_never(t))
                    .cloned()
                    .collect();
                let ty = match non_never.as_slice() {
                    [] => any(span),
                    [single] => single.clone(),
                    [first, rest @ ..] => {
                        if rest.iter().all(|t| type_desc_eq(&t.desc, &first.desc)) {
                            first.clone()
                        } else {
                            any(span)
                        }
                    }
                };
                (
                    ExprKind::Match {
                        value,
                        arms,
                        else_arm,
                    },
                    ty,
                )
            }

            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } => {
                let type_params: Vec<TypeExpr<Typed>> =
                    type_params.into_iter().map(type_expr_to_typed).collect();
                // Snapshot struct field definitions before mutable borrows for inference.
                let def_fields: Option<Vec<Field<Typed>>> = self.struct_fields.get(&name).cloned();
                let fields: Vec<(WithSpan<String>, Expr<Typed>)> = fields
                    .into_iter()
                    .map(|(label, val)| (label, self.infer_expr(val)))
                    .collect();
                if let Some(def_fields) = &def_fields {
                    let expected = def_fields.len();
                    let got = fields.len();
                    if expected != got {
                        self.error(format!("struct has {expected} field(s), got {got}"), span);
                    }
                    for (label, val) in &fields {
                        match def_fields.iter().find(|f| f.name.value == label.value) {
                            Some(def_field) => {
                                let def_ty = def_field.type_expr.clone();
                                self.check_compat(
                                    &def_ty,
                                    &val.ty,
                                    val.span,
                                    &format!("field `{}`", label.value),
                                );
                            }
                            None => {
                                self.error(format!("unknown field `{}`", label.value), label.span);
                            }
                        }
                    }
                }
                let ty = TypeExpr::new(
                    TypeDescription::TypeName {
                        type_ref: name,
                        type_params: type_params.clone(),
                    },
                    span,
                );
                (
                    ExprKind::StructLit {
                        name,
                        type_params,
                        fields,
                    },
                    ty,
                )
            }

            ExprKind::DuckLit(fields) => {
                let inferred: Vec<(WithSpan<String>, Expr<Typed>)> = fields
                    .into_iter()
                    .map(|(label, val)| (label, self.infer_expr(val)))
                    .collect();
                // Build the Duck type from actual inferred field types, sorted alphabetically
                // so that two structurally identical duck literals produce the same type.
                let mut duck_fields: Vec<Field<Typed>> = inferred
                    .iter()
                    .map(|(label, val)| Field {
                        name: label.clone(),
                        type_expr: val.ty.clone(),
                    })
                    .collect();
                duck_fields.sort_by(|a, b| a.name.value.cmp(&b.name.value));
                let ty = TypeExpr::new(
                    TypeDescription::Duck(DuckType {
                        fields: duck_fields,
                    }),
                    span,
                );
                (ExprKind::DuckLit(inferred), ty)
            }

            ExprKind::Array(elems) => {
                let elems: Vec<Expr<Typed>> =
                    elems.into_iter().map(|e| self.infer_expr(e)).collect();
                let elem_ty = elems
                    .first()
                    .map(|e| e.ty.clone())
                    .unwrap_or_else(|| any(span));
                let ty = TypeExpr::new(TypeDescription::Array(Box::new(elem_ty)), span);
                (ExprKind::Array(elems), ty)
            }

            ExprKind::Tuple(elems) => {
                let elems: Vec<Expr<Typed>> =
                    elems.into_iter().map(|e| self.infer_expr(e)).collect();
                let elem_tys = elems.iter().map(|e| e.ty.clone()).collect();
                let ty = TypeExpr::new(TypeDescription::Tuple(elem_tys), span);
                (ExprKind::Tuple(elems), ty)
            }

            ExprKind::Lambda {
                is_mut,
                params,
                return_type,
                body,
            } => {
                let params: Vec<Param<Typed>> = params
                    .into_iter()
                    .map(|p| Param {
                        name: p.name,
                        type_expr: type_expr_to_typed(p.type_expr),
                        is_mut: p.is_mut,
                    })
                    .collect();
                // Consume param slots so any nested lambdas claim the right DefIds.
                for p in &params {
                    self.claim_local(&p.name.value);
                }
                let return_type = return_type.map(type_expr_to_typed);
                let body = Box::new(self.infer_expr(*body));

                let param_types: Vec<FunTypeParam<Typed>> = params
                    .iter()
                    .map(|p| FunTypeParam {
                        label: Some(p.name.clone()),
                        type_expr: p.type_expr.clone(),
                    })
                    .collect();
                let ret = return_type.clone().unwrap_or_else(|| body.ty.clone());
                let ty = TypeExpr::new(
                    TypeDescription::Fun {
                        params: param_types,
                        return_type: Box::new(ret),
                        is_mut,
                        is_variadic: false,
                    },
                    span,
                );
                (
                    ExprKind::Lambda {
                        is_mut,
                        params,
                        return_type,
                        body,
                    },
                    ty,
                )
            }

            ExprKind::Async(e) => {
                let e = self.infer_expr(*e);
                let ty = e.ty.clone();
                (ExprKind::Async(Box::new(e)), ty)
            }
            ExprKind::Defer(e) => (ExprKind::Defer(Box::new(self.infer_expr(*e))), stmt(span)),
        }
    }

    fn infer_expr_with_expected(
        &mut self,
        e: Expr<Resolved>,
        expected: Option<&TypeExpr<Typed>>,
    ) -> Expr<Typed> {
        let span = e.span;
        match e.kind {
            ExprKind::Lambda {
                is_mut,
                params,
                return_type,
                body,
            } => {
                let expected_params = expected.and_then(|te| match &te.desc {
                    TypeDescription::Fun { params, .. } => Some(params),
                    _ => None,
                });
                let mut typed_params: Vec<Param<Typed>> = Vec::with_capacity(params.len());
                for (i, p) in params.into_iter().enumerate() {
                    let te = type_expr_to_typed(p.type_expr);
                    let type_expr = if matches!(te.desc, TypeDescription::Any) {
                        expected_params
                            .and_then(|fps| fps.get(i))
                            .map(|fp| fp.type_expr.clone())
                            .unwrap_or(te)
                    } else {
                        te
                    };

                    if let Some(id) = self.claim_local(&p.name.value) {
                        self.symbols.get_mut(id).ty = Some(type_expr.clone());
                    }
                    typed_params.push(Param {
                        name: p.name,
                        type_expr,
                        is_mut: p.is_mut,
                    });
                }
                let params = typed_params;
                let return_type = return_type.map(type_expr_to_typed);
                let body = Box::new(self.infer_expr(*body));
                let param_types: Vec<FunTypeParam<Typed>> = params
                    .iter()
                    .map(|p| FunTypeParam {
                        label: Some(p.name.clone()),
                        type_expr: p.type_expr.clone(),
                    })
                    .collect();
                // When no explicit return type, prefer the expected return type from
                // the call-site context (e.g. fn() passed where fn() -> T | .tag expected).
                let ret = return_type.clone().unwrap_or_else(|| {
                    if let Some(exp) = expected {
                        if let TypeDescription::Fun {
                            return_type: exp_ret,
                            ..
                        } = &exp.desc
                        {
                            return *exp_ret.clone();
                        }
                    }
                    body.ty.clone()
                });
                let ty = TypeExpr::new(
                    TypeDescription::Fun {
                        params: param_types,
                        return_type: Box::new(ret),
                        is_mut,
                        is_variadic: false,
                    },
                    span,
                );
                Expr {
                    kind: ExprKind::Lambda {
                        is_mut,
                        params,
                        return_type,
                        body,
                    },
                    ty,
                    span,
                }
            }
            kind => self.infer_expr(Expr { kind, ty: (), span }),
        }
    }

    fn infer_match_arm(&mut self, arm: MatchArm<Resolved>) -> MatchArm<Typed> {
        let pattern = type_expr_to_typed(arm.pattern);
        let base = arm.base.map(type_expr_to_typed);
        if let Some(ref binding_name) = arm.binding {
            if let Some(id) = self.claim_local(&binding_name.value) {
                self.symbols.get_mut(id).ty = Some(pattern.clone());
            }
        }
        let guard = arm.guard.map(|g| Box::new(self.infer_expr(*g)));
        let body = self.infer_expr(arm.body);
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

fn infer_generics(generics: Vec<WithSpan<Generic<Resolved>>>) -> Vec<WithSpan<Generic<Typed>>> {
    generics
        .into_iter()
        .map(|g| {
            WithSpan::new(
                Generic {
                    name: g.value.name,
                    constraint: g.value.constraint.map(type_expr_to_typed),
                },
                g.span,
            )
        })
        .collect()
}

pub fn infer(resolve_output: ResolveOutput) -> InferOutput {
    let mut inferencer = Inferencer::new(
        resolve_output.symbols,
        resolve_output.module_def_ids.clone(),
        resolve_output.global_scope.clone(),
    );
    let source_file =
        inferencer.infer_source_file(resolve_output.source_file, &resolve_output.global_scope);
    InferOutput {
        source_file,
        symbols: inferencer.symbols,
        errors: inferencer.errors,
        module_def_ids: resolve_output.module_def_ids,
        global_scope: resolve_output.global_scope,
        generic_type_aliases: HashMap::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::infer;
    use crate::parser2::parser::parse;
    use crate::parser2::tokenizer::tokenize;
    use crate::semantics2::resolver::resolve;

    fn type_errors(src: &str) -> Vec<String> {
        let (tokens, _) = tokenize(src, 1);
        let (ast, _) = parse(tokens, 1);
        let resolve_out = resolve(ast);
        let infer_out = infer(resolve_out);
        infer_out.errors.into_iter().map(|e| e.msg).collect()
    }

    fn extractor_ok() -> bool {
        crate::go_interop::ensure_extractor().is_some()
    }

    #[test]
    fn errorf_zero_args_fails() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Errorf() }"#);
        assert!(
            !errs.is_empty(),
            "expected a type error for fmt.Errorf() with 0 args, got none"
        );
        assert!(
            errs.iter().any(|e| e.contains("at least")),
            "expected 'at least' in error, got: {errs:?}"
        );
    }

    #[test]
    fn errorf_int_arg_fails() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Errorf(1) }"#);
        assert!(
            !errs.is_empty(),
            "expected a type error for fmt.Errorf(1), got none"
        );
        assert!(
            errs.iter()
                .any(|e| e.contains("String") || e.contains("string")),
            "expected String mismatch, got: {errs:?}"
        );
    }

    #[test]
    fn errorf_one_arg_passes() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Errorf("oops") }"#);
        assert!(errs.is_empty(), "expected no type errors, got: {errs:?}");
    }

    #[test]
    fn println_zero_args_passes() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Println() }"#);
        assert!(
            errs.is_empty(),
            "expected no type errors for variadic fmt.Println(), got: {errs:?}"
        );
    }

    #[test]
    fn unknown_member_fails() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Pr() }"#);
        assert!(
            !errs.is_empty(),
            "expected a type error for unknown fmt.Pr, got none"
        );
    }

    #[test]
    fn os_stdout_is_recognized() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "os"; fn main() { let _ = os.Stdout }"#);
        assert!(
            errs.is_empty(),
            "expected os.Stdout to be a known member, got: {errs:?}"
        );
    }

    #[test]
    fn struct_field_access_typed() {
        if !extractor_ok() {
            return;
        }
        // http.Request.Method is a string field passing it to Errorf should work
        let errs = type_errors(
            r#"use go "net/http"; use go "fmt"; fn main(r: http.Request) { fmt.Errorf(r.Method) }"#,
        );
        assert!(
            errs.is_empty(),
            "expected no errors for r.Method as string, got: {errs:?}"
        );
    }

    #[test]
    fn fprintf_int_as_writer_fails() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(r#"use go "fmt"; fn main() { fmt.Fprintf(1, "hi") }"#);
        assert!(
            !errs.is_empty(),
            "expected a type error for int passed as io.Writer, got none"
        );
    }

    #[test]
    fn fprintf_stdout_passes() {
        if !extractor_ok() {
            return;
        }
        let errs = type_errors(
            r#"use go "fmt"; use go "os"; fn main() { fmt.Fprintf(os.Stdout, "hi\n") }"#,
        );
        assert!(
            errs.is_empty(),
            "expected no errors for os.Stdout as io.Writer, got: {errs:?}"
        );
    }
}
