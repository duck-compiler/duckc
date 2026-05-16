use std::collections::{HashMap, HashSet};

const JSON_SCAN_UTILS: &str = include_str!("../emit/json_util.go");

const PRIMITIVE_FROM_JSON: &str = r#"
func int_FromJson(json_str string) (int, error) {
    var b int
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return 0, err }
    return b, nil
}
func uint_FromJson(json_str string) (uint, error) {
    var b uint
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return 0, err }
    return b, nil
}
func byte_FromJson(json_str string) (byte, error) {
    var b byte
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return 0, err }
    return b, nil
}
func float64_FromJson(json_str string) (float64, error) {
    var b float64
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return 0, err }
    return b, nil
}
func bool_FromJson(json_str string) (bool, error) {
    var b bool
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return false, err }
    return b, nil
}
func string_FromJson(json_str string) (string, error) {
    var b string
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return "", err }
    return b, nil
}
func rune_FromJson(json_str string) (rune, error) {
    var b string
    err := json.Unmarshal([]byte(json_str), &b)
    if err != nil { return 0, err }
    r := []rune(b)
    if len(r) == 0 { return 0, errors.New("empty rune") }
    return r[0], nil
}
"#;

use crate::parser2::parser::{
    DefId, DefKind, Expr, ExprKind, ExtensionDecl, FmtPart, FunctionDecl, Item, JsxAttr,
    JsxAttrValue, JsxNode, MatchArm, StructDecl, SymbolTable, TypeAliasDecl, TypeDescription,
    TypeExpr, Typed, UseDecl,
};
use crate::semantics2::type_infer::InferOutput;

use super::go_ir::*;

// Go keywords these are prefixed to avoid identifier conflicts
const ANY_HELPERS: &str = r#"
func __any_to_str(v any) string {
    if v == nil { return "()" }
    type _ts interface { to_string() string }
    if s, ok := v.(_ts); ok { return s.to_string() }
    switch u := v.(type) {
    case int: return strconv.Itoa(u)
    case uint64: return strconv.FormatUint(u, 10)
    case float64: return fmt.Sprintf("%f", u)
    case bool: return strconv.FormatBool(u)
    case string: return u
    default: return fmt.Sprintf("%v", u)
    }
}
func __any_to_json(v any) string {
    if v == nil { return "[]" }
    type _tj interface { to_json() string }
    if s, ok := v.(_tj); ok { return s.to_json() }
    switch u := v.(type) {
    case int: return strconv.Itoa(u)
    case uint64: return strconv.FormatUint(u, 10)
    case float64: return fmt.Sprintf("%f", u)
    case bool: return strconv.FormatBool(u)
    case string: return fmt.Sprintf("%q", u)
    default: return "null"
    }
}
"#;

const GO_KEYWORDS: &[&str] = &[
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "map",
    "package",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var",
];

fn escape(name: &str) -> String {
    if GO_KEYWORDS.contains(&name) {
        format!("Δ{name}")
    } else {
        name.to_string()
    }
}

fn lower_type_param_name(desc: &TypeDescription<Typed>) -> String {
    match desc {
        TypeDescription::Int => "Int".into(),
        TypeDescription::UInt => "UInt".into(),
        TypeDescription::Float => "Float".into(),
        TypeDescription::Bool(_) => "Bool".into(),
        TypeDescription::Char => "Char".into(),
        TypeDescription::Byte => "Byte".into(),
        TypeDescription::String(_) => "String".into(),
        TypeDescription::Any => "Any".into(),
        TypeDescription::Tuple(elems) if !elems.is_empty() => {
            let parts: Vec<String> = elems
                .iter()
                .map(|e| lower_type_param_name(&e.desc))
                .collect();
            format!("Tup_{}", parts.join("_"))
        }
        TypeDescription::TypeName {
            type_ref,
            type_params,
        } => {
            if type_params.is_empty() {
                format!("N{}", type_ref.0)
            } else {
                let inner: Vec<String> = type_params
                    .iter()
                    .map(|tp| lower_type_param_name(&tp.desc))
                    .collect();
                format!("N{}_{}", type_ref.0, inner.join("_"))
            }
        }
        _ => "Any".into(),
    }
}

fn prim_ext_prefix(desc: &TypeDescription<Typed>) -> Option<&'static str> {
    match desc {
        TypeDescription::String(_) => Some("string"),
        TypeDescription::Int => Some("int"),
        TypeDescription::UInt => Some("uint"),
        TypeDescription::Float => Some("float"),
        TypeDescription::Bool(_) => Some("bool"),
        TypeDescription::Char => Some("char"),
        TypeDescription::Byte => Some("byte"),
        _ => None,
    }
}

fn is_go_addressable(e: &GoExpr) -> bool {
    match e {
        GoExpr::Ident(_) | GoExpr::Deref(_) | GoExpr::Index { .. } => true,
        GoExpr::Field { base, .. } => is_go_addressable(base),
        _ => false,
    }
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().to_string() + c.as_str(),
    }
}

fn has_interface_name(field_name: &str) -> String {
    format!("Has{}", capitalize(field_name))
}

fn go_type_mangled(ty: &GoType) -> String {
    match ty {
        GoType::Int64 => "int64".into(),
        GoType::Uint64 => "uint64".into(),
        GoType::Float64 => "float64".into(),
        GoType::Bool => "bool".into(),
        GoType::String => "string".into(),
        GoType::Rune => "rune".into(),
        GoType::Byte => "byte".into(),
        GoType::Any => "any".into(),
        GoType::Named(n) => n.replace(['*', '[', ']', ' '], "_"),
        GoType::Slice(t) => format!("Slice_{}", go_type_mangled(t)),
        GoType::Ptr(t) => format!("Ptr_{}", go_type_mangled(t)),
        GoType::Func { .. } => "Func".into(),
        GoType::DuckInterface(constraints) => {
            if constraints.is_empty() {
                "EmptyDuck".into()
            } else {
                let parts: Vec<String> = constraints
                    .iter()
                    .flat_map(|(name, ty)| [name.clone(), go_type_mangled(ty)])
                    .collect();
                format!("Duck_{}", parts.join("_"))
            }
        }
    }
}

fn go_type_str(ty: &GoType) -> String {
    match ty {
        GoType::Int64 => "int".into(),
        GoType::Uint64 => "uint64".into(),
        GoType::Float64 => "float64".into(),
        GoType::Bool => "bool".into(),
        GoType::String => "string".into(),
        GoType::Rune => "rune".into(),
        GoType::Byte => "byte".into(),
        GoType::Slice(elem) => format!("[]{}", go_type_str(elem)),
        GoType::Ptr(inner) => format!("*{}", go_type_str(inner)),
        GoType::Named(name) => name.clone(),
        GoType::DuckInterface(_) => "any".into(),
        _ => "any".into(),
    }
}

fn duck_interface_type_str(constraints: &[(String, GoType)]) -> String {
    let parts: Vec<String> = constraints
        .iter()
        .map(|(name, ty)| format!("{}[{}]", name, go_type_str(ty)))
        .collect();
    if parts.is_empty() {
        "interface{}".into()
    } else {
        format!("interface{{ {} }}", parts.join("; "))
    }
}

fn duck_struct_name(fields: &[(String, GoType)]) -> String {
    let parts: Vec<String> = fields
        .iter()
        .flat_map(|(name, ty)| [name.clone(), go_type_mangled(ty)])
        .collect();
    format!("Duck_{}", parts.join("_"))
}

fn tuple_struct_name(types: &[GoType]) -> String {
    let parts: Vec<String> = types.iter().map(go_type_mangled).collect();
    format!("Tup_{}", parts.join("_"))
}

struct Lowerer<'a> {
    symbols: &'a SymbolTable,
    names: HashMap<DefId, String>,
    local_queues: HashMap<String, Vec<DefId>>,
    used_locals: HashSet<DefId>,
    tmp: u32,
    imports: HashSet<String>,
    duck_structs: HashMap<String, Vec<GoField>>,
    new_duck_field_names: std::collections::BTreeSet<String>,
    tuple_structs: HashMap<String, Vec<GoField>>,
    used_tags: std::collections::BTreeSet<String>,
    // typed elems for tuple structs, parallel to tuple_structs
    tuple_struct_typed_elems: HashMap<String, Vec<TypeExpr<Typed>>>,
    // typed fields for duck structs, parallel to duck_structs
    duck_struct_typed_fields: HashMap<String, Vec<(String, TypeExpr<Typed>)>>,
    // DefId -> expanded TypeDescription for type aliases
    type_alias_expansions: HashMap<DefId, TypeDescription<Typed>>,
    // DefId -> ordered generic param names for generic type aliases
    type_alias_param_names: HashMap<DefId, Vec<String>>,
}

impl<'a> Lowerer<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        let mut names: HashMap<DefId, String> = HashMap::new();
        let mut local_queues: HashMap<String, Vec<DefId>> = HashMap::new();

        for (id, def) in symbols.iter() {
            if matches!(def.kind, DefKind::Poison) {
                continue;
            }

            let go_name = match &def.kind {
                DefKind::Local { .. } => {
                    local_queues.entry(def.name.clone()).or_default().push(id);
                    escape(&def.name)
                }
                _ => escape(&def.name),
            };

            names.insert(id, go_name);
        }

        Self {
            symbols,
            names,
            local_queues,
            used_locals: HashSet::new(),
            tmp: 0,
            imports: HashSet::new(),
            duck_structs: HashMap::new(),
            new_duck_field_names: std::collections::BTreeSet::new(),
            tuple_structs: HashMap::new(),
            used_tags: std::collections::BTreeSet::new(),
            tuple_struct_typed_elems: HashMap::new(),
            duck_struct_typed_fields: HashMap::new(),
            type_alias_expansions: HashMap::new(),
            type_alias_param_names: HashMap::new(),
        }
    }

    fn fresh(&mut self) -> String {
        let n = self.tmp;

        self.tmp += 1;

        format!("_t{n}")
    }

    // spill non-addressable exprs (e.g. call results) into a fresh local
    fn ensure_addressable(&mut self, expr: Expr<Typed>, out: &mut Vec<GoStmt>) -> GoExpr {
        let go = self.lower_as_value(expr, out);
        if is_go_addressable(&go) {
            go
        } else {
            let tmp = self.fresh();
            out.push(GoStmt::DeclareAssign {
                name: tmp.clone(),
                value: go,
            });
            GoExpr::Ident(tmp)
        }
    }

    // lower lvalue: duck fields use GetX/GetPtrX depending on whether the field
    // type is a pointer in Go (tuple/duck) or a plain value (primitives)
    fn lower_as_lvalue(&mut self, expr: Expr<Typed>, out: &mut Vec<GoStmt>) -> GoExpr {
        let field_ty_desc = expr.ty.desc.clone();
        match expr.kind {
            ExprKind::Field { base, field } => {
                let field_name = field.value;
                let is_duck_base = matches!(
                    &base.ty.desc,
                    TypeDescription::Duck(_) | TypeDescription::NamedDuck { .. }
                ) || if let TypeDescription::TypeName {
                    type_ref,
                    type_params,
                } = &base.ty.desc
                {
                    type_params.is_empty()
                        && self
                            .type_alias_expansions
                            .get(type_ref)
                            .map_or(false, |exp| {
                                matches!(exp, TypeDescription::Duck(_) | TypeDescription::And(_))
                            })
                } else {
                    false
                };
                let is_tuple_idx = matches!(&base.ty.desc, TypeDescription::Tuple(_))
                    && field_name.parse::<usize>().is_ok();

                if is_duck_base {
                    let go_base = self.ensure_addressable(*base, out);
                    let field_is_ptr = matches!(
                        field_ty_desc,
                        TypeDescription::Tuple(_)
                            | TypeDescription::Duck(_)
                            | TypeDescription::NamedDuck { .. }
                    );
                    if field_is_ptr {
                        GoExpr::Call {
                            callee: Box::new(GoExpr::Field {
                                base: Box::new(go_base),
                                field: format!("Get{}", capitalize(&field_name)),
                            }),
                            args: vec![],
                        }
                    } else {
                        GoExpr::Deref(Box::new(GoExpr::Call {
                            callee: Box::new(GoExpr::Field {
                                base: Box::new(go_base),
                                field: format!("GetPtr{}", capitalize(&field_name)),
                            }),
                            args: vec![],
                        }))
                    }
                } else if is_tuple_idx {
                    let idx: usize = field_name.parse().unwrap();
                    let go_base = self.lower_as_lvalue(*base, out);
                    GoExpr::Field {
                        base: Box::new(go_base),
                        field: format!("field_{idx}"),
                    }
                } else {
                    let go_base = self.lower_as_value(*base, out);
                    GoExpr::Field {
                        base: Box::new(go_base),
                        field: escape(&field_name),
                    }
                }
            }
            _ => self.lower_as_value(expr, out),
        }
    }

    fn name(&self, id: DefId) -> String {
        self.names
            .get(&id)
            .cloned()
            .unwrap_or_else(|| format!("_unknown{}", id.0))
    }

    fn claim_local(&mut self, raw_name: &str) -> Option<DefId> {
        let queue = self.local_queues.get(raw_name)?;
        let id = *queue.iter().find(|id| !self.used_locals.contains(id))?;

        self.used_locals.insert(id);

        Some(id)
    }

    fn lower_type(&mut self, te: &TypeExpr<Typed>) -> GoType {
        self.lower_type_desc(&te.desc.clone())
    }

    fn lower_type_for_switch_arm(&mut self, te: &TypeExpr<Typed>) -> GoType {
        // tag patterns use *__DuckTag_X so type switches never collide with plain `string`
        if let TypeDescription::Tag(name) = &te.desc {
            self.used_tags.insert(name.clone());
            return GoType::Ptr(Box::new(GoType::Named(format!("__DuckTag_{name}"))));
        }
        let go_ty = self.lower_type(te);
        match go_ty {
            GoType::Named(name) => {
                // duck/union aliases are interfaces - no *Ptr wrap, type switch needs `case IfaceType:`
                let is_iface_alias = if let TypeDescription::TypeName {
                    type_ref,
                    type_params,
                } = &te.desc
                {
                    type_params.is_empty()
                        && self
                            .type_alias_expansions
                            .get(type_ref)
                            .map_or(false, |exp| {
                                matches!(
                                    exp,
                                    TypeDescription::Duck(_)
                                        | TypeDescription::Or(_)
                                        | TypeDescription::And(_)
                                )
                            })
                } else {
                    false
                };
                if is_iface_alias {
                    GoType::Named(name)
                } else {
                    GoType::Ptr(Box::new(GoType::Named(name)))
                }
            }
            other => other,
        }
    }

    fn lower_type_desc(&mut self, desc: &TypeDescription<Typed>) -> GoType {
        match desc {
            TypeDescription::Int => GoType::Int64,
            TypeDescription::UInt => GoType::Uint64,
            TypeDescription::Float => GoType::Float64,
            TypeDescription::Bool(_) => GoType::Bool,
            TypeDescription::Char => GoType::Rune,
            TypeDescription::Byte => GoType::Byte,
            TypeDescription::String(_) => GoType::String,
            TypeDescription::Array(elem) => GoType::Slice(Box::new(self.lower_type(elem))),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                // Don't double-wrap: struct TypeNames with type_params already lower
                // to Ptr(Named(...)), so adding another Ptr here would give **Type.
                let lowered = self.lower_type(inner);
                match lowered {
                    GoType::Ptr(_) => lowered,
                    other => GoType::Ptr(Box::new(other)),
                }
            }
            TypeDescription::Fun {
                params,
                return_type,
                ..
            } => GoType::Func {
                params: params
                    .iter()
                    .map(|p| self.lower_type(&p.type_expr))
                    .collect(),
                ret: if Self::is_void(return_type) {
                    None
                } else {
                    Some(Box::new(self.lower_type(return_type)))
                },
            },
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } => {
                if type_params.is_empty() {
                    match self.symbols.get(*type_ref).kind {
                        DefKind::GenericParam => GoType::Any,
                        DefKind::Struct => {
                            GoType::Ptr(Box::new(GoType::Named(self.name(*type_ref))))
                        }
                        _ => GoType::Named(self.name(*type_ref)),
                    }
                } else if matches!(self.symbols.get(*type_ref).kind, DefKind::TypeAlias) {
                    // expand generic type alias with substituted params
                    if let (Some(body), Some(param_names)) = (
                        self.type_alias_expansions.get(type_ref).cloned(),
                        self.type_alias_param_names.get(type_ref).cloned(),
                    ) {
                        if param_names.len() == type_params.len() {
                            let subs: std::collections::HashMap<String, TypeExpr<Typed>> =
                                param_names
                                    .iter()
                                    .zip(type_params.iter())
                                    .map(|(n, t)| (n.clone(), t.clone()))
                                    .collect();
                            use crate::parser2::parser::Span;
                            let expanded_body = TypeExpr::new(body, Span::dummy());
                            let expanded = crate::semantics2::mono::subst_type(
                                &expanded_body,
                                &subs,
                                self.symbols,
                            );
                            return self.lower_type(&expanded);
                        }
                    }
                    // Fallback: treat as opaque (same as struct case).
                    let base = self.name(*type_ref);
                    let parts: Vec<String> = type_params
                        .iter()
                        .map(|tp| lower_type_param_name(&tp.desc))
                        .collect();
                    let mangled = format!("{}__{}", base, parts.join("__"));
                    GoType::Ptr(Box::new(GoType::Named(mangled)))
                } else {
                    let base = self.name(*type_ref);
                    let parts: Vec<String> = type_params
                        .iter()
                        .map(|tp| lower_type_param_name(&tp.desc))
                        .collect();
                    let mangled = format!("{}__{}", base, parts.join("__"));
                    GoType::Ptr(Box::new(GoType::Named(mangled)))
                }
            }
            TypeDescription::NamedDuck { name, .. } => GoType::Named(format!("Interface_{name}")),
            TypeDescription::Duck(duck) => {
                let mut constraints: Vec<(String, GoType)> = duck
                    .fields
                    .iter()
                    .map(|f| {
                        // nested duck fields stored as any - mirrors lower_duck_lit
                        let go_ty = match self.lower_type(&f.type_expr) {
                            GoType::DuckInterface(_) => GoType::Any,
                            other => other,
                        };
                        (has_interface_name(&f.name.value), go_ty)
                    })
                    .collect();
                constraints.sort_by(|a, b| a.0.cmp(&b.0));
                GoType::DuckInterface(constraints)
            }
            TypeDescription::Tuple(elems) => {
                if elems.is_empty() {
                    return GoType::Any;
                }
                let elems_cloned: Vec<TypeExpr<Typed>> = elems.to_vec();
                let elem_tys: Vec<GoType> =
                    elems_cloned.iter().map(|e| self.lower_type(e)).collect();
                let sname = tuple_struct_name(&elem_tys);
                self.tuple_structs.entry(sname.clone()).or_insert_with(|| {
                    elem_tys
                        .iter()
                        .enumerate()
                        .map(|(i, t)| GoField {
                            name: format!("field_{i}"),
                            ty: t.clone(),
                        })
                        .collect()
                });
                self.tuple_struct_typed_elems
                    .entry(sname.clone())
                    .or_insert_with(|| elems_cloned.clone());
                GoType::Ptr(Box::new(GoType::Named(sname)))
            }
            TypeDescription::Or(_) | TypeDescription::And(_) => GoType::Any,
            TypeDescription::GoPackage(_) => GoType::Any,
            // Emit GoNamed as a Go named type (the string is already valid Go syntax).
            TypeDescription::GoNamed(s) => GoType::Named(s.clone()),
            _ => GoType::Any,
        }
    }

    fn is_void(te: &TypeExpr<Typed>) -> bool {
        match &te.desc {
            TypeDescription::Tuple(v) => v.is_empty(),
            TypeDescription::Statement | TypeDescription::Never | TypeDescription::Any => true,
            _ => false,
        }
    }

    fn lower_as_value(&mut self, expr: Expr<Typed>, out: &mut Vec<GoStmt>) -> GoExpr {
        let span_ty = expr.ty.clone();
        match expr.kind {
            ExprKind::Int(v) => GoExpr::Int(v as i64),
            ExprKind::Float(v) => GoExpr::Float(v),
            ExprKind::Bool(v) => GoExpr::Bool(v),
            ExprKind::Char(v) => GoExpr::Char(v),
            ExprKind::String(v) => GoExpr::Str(v),
            ExprKind::Tag(v) => {
                self.used_tags.insert(v.clone());
                GoExpr::Raw(format!("&__DuckTag_{}{{}}", v))
            }
            ExprKind::InlineGo(v) => GoExpr::Raw(v),
            ExprKind::Jsx(node) => self.lower_server_jsx(*node, out),
            ExprKind::Break => {
                out.push(GoStmt::Break);
                GoExpr::Nil
            }
            ExprKind::Continue => {
                out.push(GoStmt::Continue);
                GoExpr::Nil
            }

            ExprKind::FmtString(parts) => {
                self.imports.insert("fmt".into());
                let mut fmt_str = String::new();
                let mut args: Vec<GoExpr> = Vec::new();
                for part in parts {
                    match part {
                        FmtPart::Literal(s) => fmt_str.push_str(&s.replace('%', "%%")),
                        FmtPart::Expr(e) => {
                            fmt_str.push_str("%v");
                            args.push(self.lower_as_value(e, out));
                        }
                    }
                }
                let mut call_args = vec![GoExpr::Str(fmt_str)];
                call_args.extend(args);
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("fmt".into())),
                        field: "Sprintf".into(),
                    }),
                    args: call_args,
                }
            }

            ExprKind::Ident(id) => GoExpr::Ident(self.name(id)),

            ExprKind::Block(stmts) => {
                let n = stmts.len();
                if n == 0 {
                    return GoExpr::Nil;
                }
                for (i, s) in stmts.into_iter().enumerate() {
                    if i < n - 1 {
                        self.lower_as_stmts(s, out);
                    } else {
                        return self.lower_as_value(s, out);
                    }
                }
                GoExpr::Nil
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.lower_as_value(*condition, out);
                let tmp = self.fresh();
                let go_ty = self.lower_type(&span_ty);
                out.push(GoStmt::Declare {
                    name: tmp.clone(),
                    ty: go_ty,
                });

                let mut then_stmts = Vec::new();
                let then_val = self.lower_as_value(*then_branch, &mut then_stmts);
                then_stmts.push(GoStmt::Assign {
                    target: GoExpr::Ident(tmp.clone()),
                    value: then_val,
                });

                let else_stmts = else_branch.map(|e| {
                    let mut s = Vec::new();
                    let v = self.lower_as_value(*e, &mut s);
                    s.push(GoStmt::Assign {
                        target: GoExpr::Ident(tmp.clone()),
                        value: v,
                    });
                    s
                });

                out.push(GoStmt::If {
                    cond,
                    then: then_stmts,
                    else_: else_stmts,
                });
                GoExpr::Ident(tmp)
            }

            ExprKind::Add(a, b) => self.bin(GoBinOp::Add, *a, *b, out),
            ExprKind::Sub(a, b) => self.bin(GoBinOp::Sub, *a, *b, out),
            ExprKind::Mul(a, b) => self.bin(GoBinOp::Mul, *a, *b, out),
            ExprKind::Div(a, b) => self.bin(GoBinOp::Div, *a, *b, out),
            ExprKind::Mod(a, b) => self.bin(GoBinOp::Mod, *a, *b, out),
            ExprKind::BitAnd(a, b) => self.bin(GoBinOp::BitAnd, *a, *b, out),
            ExprKind::BitOr(a, b) => self.bin(GoBinOp::BitOr, *a, *b, out),
            ExprKind::BitXor(a, b) => self.bin(GoBinOp::BitXor, *a, *b, out),
            ExprKind::Shl(a, b) => self.bin(GoBinOp::Shl, *a, *b, out),
            ExprKind::Shr(a, b) => self.bin(GoBinOp::Shr, *a, *b, out),
            ExprKind::Eq(a, b) => self.bin(GoBinOp::Eq, *a, *b, out),
            ExprKind::Neq(a, b) => self.bin(GoBinOp::Neq, *a, *b, out),
            ExprKind::Lt(a, b) => self.bin(GoBinOp::Lt, *a, *b, out),
            ExprKind::Lte(a, b) => self.bin(GoBinOp::Lte, *a, *b, out),
            ExprKind::Gt(a, b) => self.bin(GoBinOp::Gt, *a, *b, out),
            ExprKind::Gte(a, b) => self.bin(GoBinOp::Gte, *a, *b, out),
            ExprKind::And(a, b) => self.bin(GoBinOp::And, *a, *b, out),
            ExprKind::Or(a, b) => self.bin(GoBinOp::Or, *a, *b, out),
            ExprKind::Not(a) => GoExpr::UnaryOp {
                op: GoUnaryOp::Not,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            ExprKind::Neg(a) => GoExpr::UnaryOp {
                op: GoUnaryOp::Neg,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            ExprKind::BitNot(a) => GoExpr::UnaryOp {
                op: GoUnaryOp::BitNot,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            ExprKind::Ref(a) | ExprKind::RefMut(a) => {
                GoExpr::Ref(Box::new(self.lower_as_value(*a, out)))
            }
            ExprKind::Deref(a) => GoExpr::Deref(Box::new(self.lower_as_value(*a, out))),

            ExprKind::Field { base, field } => {
                let field_name = field.value.clone();

                // Go package member access and Go named type field/method: emit verbatim.
                if matches!(
                    &base.ty.desc,
                    TypeDescription::GoPackage(_) | TypeDescription::GoNamed(_)
                ) {
                    let go_base = self.lower_as_value(*base, out);
                    return GoExpr::Field {
                        base: Box::new(go_base),
                        field: field_name,
                    };
                }

                // Tuple field access: t.0 -> t.field_0
                let is_tuple_access = matches!(&base.ty.desc, TypeDescription::Tuple(_))
                    && field_name.parse::<usize>().is_ok();
                if is_tuple_access {
                    let idx = field_name.parse::<usize>().unwrap();
                    let go_base = self.lower_as_value(*base, out);
                    return GoExpr::Field {
                        base: Box::new(go_base),
                        field: format!("field_{idx}"),
                    };
                }

                let is_duck = matches!(
                    &base.ty.desc,
                    TypeDescription::Duck(_) | TypeDescription::NamedDuck { .. }
                ) || if let TypeDescription::TypeName {
                    type_ref,
                    type_params,
                } = &base.ty.desc
                {
                    type_params.is_empty()
                        && self
                            .type_alias_expansions
                            .get(type_ref)
                            .map_or(false, |exp| {
                                matches!(exp, TypeDescription::Duck(_) | TypeDescription::And(_))
                            })
                } else {
                    false
                };
                // Extension methods are emitted with their original lowercase Go name.
                let is_method = matches!(&span_ty.desc, TypeDescription::Fun { .. });
                let go_base = self.lower_as_value(*base, out);
                if is_duck {
                    let call_expr = GoExpr::Call {
                        callee: Box::new(GoExpr::Field {
                            base: Box::new(go_base),
                            field: format!("Get{}", capitalize(&field_name)),
                        }),
                        args: vec![],
                    };
                    // duck-typed field stored as any - assert to duck interface before chaining
                    let result_go_ty = self.lower_type(&span_ty);
                    if matches!(&result_go_ty, GoType::DuckInterface(_)) {
                        GoExpr::TypeAssert {
                            value: Box::new(call_expr),
                            ty: result_go_ty,
                        }
                    } else {
                        call_expr
                    }
                } else if is_method {
                    GoExpr::Field {
                        base: Box::new(go_base),
                        field: escape(&field_name),
                    }
                } else {
                    // Struct fields are lowercase (unexported) in the new pipeline.
                    GoExpr::Field {
                        base: Box::new(go_base),
                        field: escape(&field_name),
                    }
                }
            }
            ExprKind::Index { base, index } => GoExpr::Index {
                base: Box::new(self.lower_as_value(*base, out)),
                index: Box::new(self.lower_as_value(*index, out)),
            },
            ExprKind::ScopeRes { base, member } => GoExpr::Field {
                base: Box::new(self.lower_as_value(*base, out)),
                field: member.value,
            },

            ExprKind::Call {
                callee,
                args,
                type_params,
            } => {
                fn peel_ref(desc: &TypeDescription<Typed>) -> &TypeDescription<Typed> {
                    match desc {
                        TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                            peel_ref(&inner.desc)
                        }
                        other => other,
                    }
                }
                let is_prim_intrinsic = match &callee.kind {
                    ExprKind::Field { base, field } => {
                        let m = field.value.as_str();
                        match peel_ref(&base.ty.desc) {
                            TypeDescription::Array(_) => {
                                matches!(m, "len" | "to_string" | "to_json")
                            }
                            // Or/And: any in Go, no methods, must inline a type switch.
                            TypeDescription::Or(_) | TypeDescription::And(_) => {
                                matches!(m, "to_string" | "to_json")
                            }
                            TypeDescription::Int
                            | TypeDescription::UInt
                            | TypeDescription::Float
                            | TypeDescription::Bool(_)
                            | TypeDescription::String(_)
                            | TypeDescription::Char
                            | TypeDescription::Byte => matches!(m, "to_string" | "to_json"),
                            // empty tuple is any in Go
                            TypeDescription::Tuple(elems) if elems.is_empty() => {
                                matches!(m, "to_string" | "to_json")
                            }
                            TypeDescription::Duck(_) => matches!(m, "to_string" | "to_json"),
                            // duck/union alias: generate inline IIFE
                            TypeDescription::TypeName {
                                type_ref,
                                type_params,
                            } if type_params.is_empty() => {
                                matches!(m, "to_string" | "to_json")
                                    && self.type_alias_expansions.get(type_ref).map_or(
                                        false,
                                        |exp| {
                                            matches!(
                                                exp,
                                                TypeDescription::Duck(_)
                                                    | TypeDescription::Or(_)
                                                    | TypeDescription::And(_)
                                            )
                                        },
                                    )
                            }
                            _ => false,
                        }
                    }
                    _ => false,
                };
                if is_prim_intrinsic {
                    let Expr {
                        kind: ExprKind::Field { base, field },
                        ..
                    } = *callee
                    else {
                        unreachable!()
                    };
                    let base_ty = peel_ref(&base.ty.desc).clone();
                    let needs_deref = matches!(
                        &base.ty.desc,
                        TypeDescription::Ref(_) | TypeDescription::RefMut(_)
                    );
                    let mut go_base = self.lower_as_value(*base, out);
                    if needs_deref {
                        go_base = GoExpr::Deref(Box::new(go_base));
                    }
                    return self.lower_intrinsic_call(&field.value, &base_ty, go_base);
                }
                let is_prim_ext = match &callee.kind {
                    ExprKind::Field { base, field } => {
                        let m = field.value.as_str();
                        prim_ext_prefix(peel_ref(&base.ty.desc)).is_some()
                            && !matches!(m, "to_string" | "to_json")
                    }
                    _ => false,
                };
                if is_prim_ext {
                    let Expr {
                        kind: ExprKind::Field { base, field },
                        ..
                    } = *callee
                    else {
                        unreachable!()
                    };
                    let prefix = prim_ext_prefix(peel_ref(&base.ty.desc)).unwrap();
                    let fn_name = format!("{}__{}", prefix, field.value);
                    let needs_deref = matches!(
                        &base.ty.desc,
                        TypeDescription::Ref(_) | TypeDescription::RefMut(_)
                    );
                    let mut go_base = self.lower_as_value(*base, out);
                    if needs_deref {
                        go_base = GoExpr::Deref(Box::new(go_base));
                    }
                    let mut call_args = vec![go_base];
                    call_args.extend(args.into_iter().map(|a| self.lower_as_value(a, out)));
                    return GoExpr::Call {
                        callee: Box::new(GoExpr::Ident(fn_name)),
                        args: call_args,
                    };
                }
                // Extract parameter types so duck literals can be coerced to the right struct.
                let param_tys: Vec<Option<Vec<(String, TypeExpr<Typed>)>>> = match &callee.ty.desc {
                    TypeDescription::Fun { params, .. } => params
                        .iter()
                        .map(|p| match &p.type_expr.desc {
                            TypeDescription::Duck(d) => Some(
                                d.fields
                                    .iter()
                                    .map(|f| (f.name.value.clone(), f.type_expr.clone()))
                                    .collect(),
                            ),
                            _ => None,
                        })
                        .collect(),
                    _ => vec![],
                };
                let lowered_callee = self.lower_as_value(*callee, out);
                let args = args
                    .into_iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let Expr { kind, ty, span } = a;
                        match kind {
                            ExprKind::DuckLit(fields)
                                if param_tys.get(i).and_then(|x| x.as_ref()).is_some() =>
                            {
                                let target_types = param_tys[i].as_ref().unwrap();
                                self.lower_duck_lit_with_target_types(fields, target_types, out)
                            }
                            kind => self.lower_as_value(Expr { kind, ty, span }, out),
                        }
                    })
                    .collect();
                let callee = if !type_params.is_empty() {
                    let parts: Vec<String> = type_params
                        .iter()
                        .map(|tp| lower_type_param_name(&tp.desc))
                        .collect();
                    let suffix = parts.join("__");
                    match lowered_callee {
                        GoExpr::Ident(name) => GoExpr::Ident(format!("{name}__{suffix}")),
                        GoExpr::Field { base, field } => GoExpr::Field {
                            base,
                            field: format!("{field}__{suffix}"),
                        },
                        other => other,
                    }
                } else {
                    lowered_callee
                };
                GoExpr::Call {
                    callee: Box::new(callee),
                    args,
                }
            }

            ExprKind::As { value, type_expr } => {
                if let ExprKind::InlineGo(s) = value.kind {
                    // when target type is Never, emit statements only - no temp var
                    if Self::is_void(&type_expr) {
                        for line in s.lines() {
                            let trimmed = line.trim();
                            if !trimmed.is_empty() {
                                out.push(GoStmt::Expr(GoExpr::Raw(trimmed.to_string())));
                            }
                        }
                        return GoExpr::Nil;
                    }
                    let tmp = self.fresh();
                    let go_ty = self.lower_type(&type_expr);
                    out.push(GoStmt::Declare {
                        name: tmp.clone(),
                        ty: go_ty,
                    });
                    let replaced = s.replace('$', &tmp);
                    for line in replaced.lines() {
                        let trimmed = line.trim();
                        if !trimmed.is_empty() {
                            out.push(GoStmt::Expr(GoExpr::Raw(trimmed.to_string())));
                        }
                    }
                    GoExpr::Ident(tmp)
                } else {
                    let lowered_ty = self.lower_type(&type_expr);
                    let lowered_val = self.lower_as_value(*value, out);
                    // `[] as T[]` - Go can't cast []any{} to []T directly
                    if let GoType::Slice(ref elem_ty) = lowered_ty {
                        if let GoExpr::SliceLit { ref elems, .. } = lowered_val {
                            if elems.is_empty() {
                                return GoExpr::SliceLit {
                                    ty: *elem_ty.clone(),
                                    elems: vec![],
                                };
                            }
                        }
                    }
                    GoExpr::Cast {
                        ty: lowered_ty,
                        value: Box::new(lowered_val),
                    }
                }
            }

            ExprKind::Array(elems) => {
                let elem_ty = match &span_ty.desc {
                    TypeDescription::Array(e) => self.lower_type(e),
                    _ => GoType::Any,
                };
                let elems = elems
                    .into_iter()
                    .map(|e| self.lower_as_value(e, out))
                    .collect();
                GoExpr::SliceLit { ty: elem_ty, elems }
            }

            ExprKind::Tuple(elems) => {
                if elems.is_empty() {
                    return GoExpr::Nil;
                }
                let lowered: Vec<(GoExpr, GoType, TypeExpr<Typed>)> = elems
                    .into_iter()
                    .map(|e| {
                        let go_ty = self.lower_type(&e.ty);
                        let orig_ty = e.ty.clone();
                        let go_val = self.lower_as_value(e, out);
                        (go_val, go_ty, orig_ty)
                    })
                    .collect();
                let elem_tys: Vec<GoType> = lowered.iter().map(|(_, t, _)| t.clone()).collect();
                let struct_name = tuple_struct_name(&elem_tys);
                self.tuple_structs
                    .entry(struct_name.clone())
                    .or_insert_with(|| {
                        lowered
                            .iter()
                            .enumerate()
                            .map(|(i, (_, t, _))| GoField {
                                name: format!("field_{i}"),
                                ty: t.clone(),
                            })
                            .collect()
                    });
                self.tuple_struct_typed_elems
                    .entry(struct_name.clone())
                    .or_insert_with(|| lowered.iter().map(|(_, _, ty)| ty.clone()).collect());
                let fields: Vec<(String, GoExpr)> = lowered
                    .into_iter()
                    .enumerate()
                    .map(|(i, (v, _, _))| (format!("field_{i}"), v))
                    .collect();
                GoExpr::Ref(Box::new(GoExpr::StructLit {
                    ty: struct_name,
                    fields,
                }))
            }

            ExprKind::StructLit {
                name,
                type_params,
                fields,
            } => {
                let ty_name = if type_params.is_empty() {
                    self.name(name)
                } else {
                    let base = self.name(name);
                    let parts: Vec<String> = type_params
                        .iter()
                        .map(|tp| lower_type_param_name(&tp.desc))
                        .collect();
                    format!("{}__{}", base, parts.join("__"))
                };
                let fields = fields
                    .into_iter()
                    .map(|(label, val)| (escape(&label.value), self.lower_as_value(val, out)))
                    .collect();
                GoExpr::Ref(Box::new(GoExpr::StructLit {
                    ty: ty_name,
                    fields,
                }))
            }

            ExprKind::DuckLit(fields) => {
                // Lower each field value and compute its Go type from the typed expr.
                let mut lowered: Vec<(String, GoExpr, GoType, TypeExpr<Typed>)> = fields
                    .into_iter()
                    .map(|(label, val)| {
                        // duck-typed fields stored as any to satisfy HasField[any]
                        let go_ty = match self.lower_type(&val.ty) {
                            GoType::DuckInterface(_) => GoType::Any,
                            other => other,
                        };
                        let orig_ty = val.ty.clone();
                        let cap = capitalize(&label.value);
                        let go_val = self.lower_as_value(val, out);
                        (cap, go_val, go_ty, orig_ty)
                    })
                    .collect();
                // Sort alphabetically so the struct name is canonical.
                lowered.sort_by(|a, b| a.0.cmp(&b.0));

                let name_fields: Vec<(String, GoType)> = lowered
                    .iter()
                    .map(|(n, _, t, _)| (n.clone(), t.clone()))
                    .collect();
                let struct_name = duck_struct_name(&name_fields);

                // Register the duck struct (once per unique shape).
                self.duck_structs
                    .entry(struct_name.clone())
                    .or_insert_with(|| {
                        let go_fields: Vec<GoField> = lowered
                            .iter()
                            .map(|(n, _, t, _)| GoField {
                                name: n.clone(),
                                ty: t.clone(),
                            })
                            .collect();
                        for (n, _) in &name_fields {
                            self.new_duck_field_names.insert(n.clone());
                        }
                        go_fields
                    });
                self.duck_struct_typed_fields
                    .entry(struct_name.clone())
                    .or_insert_with(|| {
                        lowered
                            .iter()
                            .map(|(n, _, _, ty)| (n.clone(), ty.clone()))
                            .collect()
                    });

                let struct_fields: Vec<(String, GoExpr)> =
                    lowered.into_iter().map(|(n, v, _, _)| (n, v)).collect();
                GoExpr::Ref(Box::new(GoExpr::StructLit {
                    ty: struct_name,
                    fields: struct_fields,
                }))
            }

            ExprKind::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                let go_params: Vec<GoParam> = params
                    .iter()
                    .map(|p| GoParam {
                        name: escape(&p.name.value),
                        ty: self.lower_type(&p.type_expr),
                    })
                    .collect();
                let effective_ret: Option<&TypeExpr<Typed>> = return_type.as_ref().or_else(|| {
                    if let TypeDescription::Fun { return_type, .. } = &span_ty.desc {
                        Some(return_type.as_ref())
                    } else {
                        None
                    }
                });
                let is_void = effective_ret.map(|t| Self::is_void(t)).unwrap_or(true);
                let ret = effective_ret.and_then(|t| {
                    if Self::is_void(t) {
                        None
                    } else {
                        Some(self.lower_type(t))
                    }
                });
                let body_stmts = self.lower_fn_body(*body, is_void);
                GoExpr::Closure {
                    params: go_params,
                    ret,
                    body: body_stmts,
                }
            }

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => self.lower_match_as_value(value, arms, else_arm, &span_ty, out),

            ExprKind::Return(v) => {
                let ret = v.map(|e| self.lower_as_value(*e, out));
                out.push(GoStmt::Return(ret));
                GoExpr::Nil
            }

            ExprKind::Async(e) => self.lower_as_value(*e, out),
            ExprKind::Defer(e) => {
                let inner = self.lower_as_value(*e, out);
                out.push(GoStmt::Defer(inner));
                GoExpr::Nil
            }

            ExprKind::Let { .. }
            | ExprKind::LetTuple { .. }
            | ExprKind::Const { .. }
            | ExprKind::Assign { .. }
            | ExprKind::AddAssign { .. }
            | ExprKind::SubAssign { .. }
            | ExprKind::MulAssign { .. }
            | ExprKind::DivAssign { .. }
            | ExprKind::ModAssign { .. }
            | ExprKind::ShrAssign { .. }
            | ExprKind::ShlAssign { .. }
            | ExprKind::While { .. }
            | ExprKind::For { .. } => {
                self.lower_as_stmts(
                    Expr {
                        kind: expr.kind,
                        ty: span_ty,
                        span: expr.span,
                    },
                    out,
                );
                GoExpr::Nil
            }
        }
    }

    fn bin(
        &mut self,
        op: GoBinOp,
        a: Expr<Typed>,
        b: Expr<Typed>,
        out: &mut Vec<GoStmt>,
    ) -> GoExpr {
        let lhs = Box::new(self.lower_as_value(a, out));
        let rhs = Box::new(self.lower_as_value(b, out));
        GoExpr::BinOp { op, lhs, rhs }
    }

    fn lower_match_as_value(
        &mut self,
        value: Box<Expr<Typed>>,
        arms: Vec<MatchArm<Typed>>,
        else_arm: Option<Box<Expr<Typed>>>,
        result_ty: &TypeExpr<Typed>,
        out: &mut Vec<GoStmt>,
    ) -> GoExpr {
        let tmp = self.fresh();
        let go_ty = self.lower_type(result_ty);
        out.push(GoStmt::Declare {
            name: tmp.clone(),
            ty: go_ty,
        });

        // Cast to any so Go accepts the type switch on non-interface values.
        let val_go_ty = self.lower_type(&value.ty);
        let switch_val = self.lower_as_value(*value, out);
        let switch_val = match val_go_ty {
            GoType::Any => switch_val,
            _ => GoExpr::Cast {
                ty: GoType::Any,
                value: Box::new(switch_val),
            },
        };

        let switch_arms: Vec<TypeSwitchArm> = arms
            .into_iter()
            .map(|arm| {
                let binding = if let Some(b) = &arm.binding {
                    self.claim_local(&b.value)
                        .map(|id| self.names[&id].clone())
                        .unwrap_or_else(|| escape(&b.value))
                } else {
                    String::new()
                };
                let ty = self.lower_type_for_switch_arm(&arm.pattern);
                let mut body = Vec::new();
                let arm_is_never = Self::is_void(&arm.body.ty);
                if arm_is_never {
                    self.lower_as_stmts(arm.body, &mut body);
                } else {
                    let val = self.lower_as_value(arm.body, &mut body);
                    body.push(GoStmt::Assign {
                        target: GoExpr::Ident(tmp.clone()),
                        value: val,
                    });
                }
                TypeSwitchArm { binding, ty, body }
            })
            .collect();

        let default = else_arm.map(|e| {
            let mut body = Vec::new();
            let else_is_never = Self::is_void(&e.ty);
            if else_is_never {
                self.lower_as_stmts(*e, &mut body);
            } else {
                let val = self.lower_as_value(*e, &mut body);
                body.push(GoStmt::Assign {
                    target: GoExpr::Ident(tmp.clone()),
                    value: val,
                });
            }
            body
        });

        out.push(GoStmt::TypeSwitch {
            value: switch_val,
            arms: switch_arms,
            default,
        });
        GoExpr::Ident(tmp)
    }

    fn lower_duck_lit_with_target_types(
        &mut self,
        fields: Vec<(crate::parser2::parser::WithSpan<String>, Expr<Typed>)>,
        target_types: &[(String, TypeExpr<Typed>)],
        out: &mut Vec<GoStmt>,
    ) -> GoExpr {
        let mut lowered: Vec<(String, GoExpr, GoType, TypeExpr<Typed>)> = fields
            .into_iter()
            .map(|(label, val)| {
                let go_ty = match target_types
                    .iter()
                    .find(|(n, _)| *n == label.value)
                    .map(|(_, ty)| self.lower_type(ty))
                    .unwrap_or_else(|| self.lower_type(&val.ty))
                {
                    GoType::DuckInterface(_) => GoType::Any,
                    other => other,
                };
                let orig_ty = target_types
                    .iter()
                    .find(|(n, _)| *n == label.value)
                    .map(|(_, ty)| ty.clone())
                    .unwrap_or_else(|| val.ty.clone());
                let cap = capitalize(&label.value);
                let go_val = self.lower_as_value(val, out);
                (cap, go_val, go_ty, orig_ty)
            })
            .collect();
        lowered.sort_by(|a, b| a.0.cmp(&b.0));
        let name_fields: Vec<(String, GoType)> = lowered
            .iter()
            .map(|(n, _, t, _)| (n.clone(), t.clone()))
            .collect();
        let struct_name = duck_struct_name(&name_fields);
        self.duck_structs
            .entry(struct_name.clone())
            .or_insert_with(|| {
                let go_fields: Vec<GoField> = lowered
                    .iter()
                    .map(|(n, _, t, _)| GoField {
                        name: n.clone(),
                        ty: t.clone(),
                    })
                    .collect();
                for (n, _) in &name_fields {
                    self.new_duck_field_names.insert(n.clone());
                }
                go_fields
            });
        self.duck_struct_typed_fields
            .entry(struct_name.clone())
            .or_insert_with(|| {
                lowered
                    .iter()
                    .map(|(n, _, _, ty)| (n.clone(), ty.clone()))
                    .collect()
            });
        let struct_fields: Vec<(String, GoExpr)> =
            lowered.into_iter().map(|(n, v, _, _)| (n, v)).collect();
        GoExpr::Ref(Box::new(GoExpr::StructLit {
            ty: struct_name,
            fields: struct_fields,
        }))
    }

    fn lower_as_stmts(&mut self, expr: Expr<Typed>, out: &mut Vec<GoStmt>) {
        match expr.kind {
            ExprKind::Block(stmts) => {
                for s in stmts {
                    self.lower_as_stmts(s, out);
                }
            }

            ExprKind::Let {
                name,
                type_ann,
                value,
                ..
            } => {
                let value = *value;
                let go_name = self
                    .claim_local(&name.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| escape(&name.value));

                // use annotation field types so the struct satisfies the declared HasField[T] interface
                let go_val = match (type_ann.as_ref(), value.kind) {
                    (Some(ann), ExprKind::DuckLit(fields))
                        if matches!(&ann.desc, TypeDescription::Duck(_)) =>
                    {
                        let target_types: Vec<(String, TypeExpr<Typed>)> = match &ann.desc {
                            TypeDescription::Duck(d) => d
                                .fields
                                .iter()
                                .map(|f| (f.name.value.clone(), f.type_expr.clone()))
                                .collect(),
                            _ => unreachable!(),
                        };
                        self.lower_duck_lit_with_target_types(fields, &target_types, out)
                    }
                    (_, kind) => self.lower_as_value(
                        Expr {
                            kind,
                            ty: value.ty,
                            span: value.span,
                        },
                        out,
                    ),
                };

                match type_ann {
                    Some(ann) => {
                        let go_ty = self.lower_type(&ann);
                        out.push(GoStmt::Declare {
                            name: go_name.clone(),
                            ty: go_ty,
                        });
                        out.push(GoStmt::Assign {
                            target: GoExpr::Ident(go_name.clone()),
                            value: go_val,
                        });
                    }
                    None => out.push(GoStmt::DeclareAssign {
                        name: go_name.clone(),
                        value: go_val,
                    }),
                }

                out.push(GoStmt::Assign {
                    target: GoExpr::Ident("_".into()),
                    value: GoExpr::Ident(go_name),
                });
            }

            ExprKind::LetTuple { names, value } => {
                let go_val = self.lower_as_value(*value, out);
                let go_names: Vec<String> = names
                    .iter()
                    .map(|n| {
                        self.claim_local(&n.value)
                            .map(|id| self.names[&id].clone())
                            .unwrap_or_else(|| escape(&n.value))
                    })
                    .collect();
                out.push(GoStmt::MultiDeclareAssign {
                    names: go_names.clone(),
                    value: go_val,
                });
                // suppress "declared and not used" for each name
                for go_name in go_names {
                    out.push(GoStmt::Assign {
                        target: GoExpr::Ident("_".into()),
                        value: GoExpr::Ident(go_name),
                    });
                }
            }

            ExprKind::Const {
                name,
                type_ann: _,
                value,
                ..
            } => {
                let go_val = self.lower_as_value(*value, out);
                let go_name = self
                    .claim_local(&name.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| escape(&name.value));

                out.push(GoStmt::DeclareAssign {
                    name: go_name.clone(),
                    value: go_val,
                });
                out.push(GoStmt::Assign {
                    target: GoExpr::Ident("_".into()),
                    value: GoExpr::Ident(go_name),
                });
            }

            ExprKind::Assign { target, value } => {
                let t = self.lower_as_lvalue(*target, out);
                let v = self.lower_as_value(*value, out);
                out.push(GoStmt::Assign {
                    target: t,
                    value: v,
                });
            }

            ExprKind::AddAssign { target, value } => {
                self.compound_assign(GoBinOp::Add, *target, *value, out)
            }
            ExprKind::SubAssign { target, value } => {
                self.compound_assign(GoBinOp::Sub, *target, *value, out)
            }
            ExprKind::MulAssign { target, value } => {
                self.compound_assign(GoBinOp::Mul, *target, *value, out)
            }
            ExprKind::DivAssign { target, value } => {
                self.compound_assign(GoBinOp::Div, *target, *value, out)
            }
            ExprKind::ModAssign { target, value } => {
                self.compound_assign(GoBinOp::Mod, *target, *value, out)
            }
            ExprKind::ShrAssign { target, value } => {
                self.compound_assign(GoBinOp::Shr, *target, *value, out)
            }
            ExprKind::ShlAssign { target, value } => {
                self.compound_assign(GoBinOp::Shl, *target, *value, out)
            }

            ExprKind::Return(v) => {
                let ret = v.map(|e| self.lower_as_value(*e, out));
                out.push(GoStmt::Return(ret));
            }
            ExprKind::Break => out.push(GoStmt::Break),
            ExprKind::Continue => out.push(GoStmt::Continue),

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.lower_as_value(*condition, out);
                let mut then_stmts = Vec::new();
                self.lower_as_stmts(*then_branch, &mut then_stmts);
                let else_stmts = else_branch.map(|e| {
                    let mut s = Vec::new();
                    self.lower_as_stmts(*e, &mut s);
                    s
                });
                out.push(GoStmt::If {
                    cond,
                    then: then_stmts,
                    else_: else_stmts,
                });
            }

            ExprKind::While { condition, body } => {
                let mut cond_setup: Vec<GoStmt> = Vec::new();
                let cond = self.lower_as_value(*condition, &mut cond_setup);
                let mut body_stmts = cond_setup.clone();
                self.lower_as_stmts(*body, &mut body_stmts);
                // re-evaluate condition at end of loop for while semantics
                if !cond_setup.is_empty() {
                    body_stmts.extend(cond_setup);
                }
                out.push(GoStmt::ForCond {
                    cond,
                    body: body_stmts,
                });
            }

            ExprKind::For {
                binding,
                is_mut: _,
                iterable,
                body,
            } => {
                let iter = self.lower_as_value(*iterable, out);
                let binding_name = self
                    .claim_local(&binding.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| escape(&binding.value));
                let mut body_stmts = Vec::new();
                self.lower_as_stmts(*body, &mut body_stmts);
                out.push(GoStmt::ForRange {
                    val: binding_name,
                    iter,
                    body: body_stmts,
                });
            }

            ExprKind::Defer(e) => {
                let inner = self.lower_as_value(*e, out);
                out.push(GoStmt::Defer(inner));
            }

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => {
                let val_go_ty = self.lower_type(&value.ty);
                let switch_val = self.lower_as_value(*value, out);
                let switch_val = match val_go_ty {
                    GoType::Any => switch_val,
                    _ => GoExpr::Cast {
                        ty: GoType::Any,
                        value: Box::new(switch_val),
                    },
                };
                let switch_arms: Vec<TypeSwitchArm> = arms
                    .into_iter()
                    .map(|arm| {
                        let binding = if let Some(b) = &arm.binding {
                            self.claim_local(&b.value)
                                .map(|id| self.names[&id].clone())
                                .unwrap_or_else(|| escape(&b.value))
                        } else {
                            String::new()
                        };
                        let ty = self.lower_type_for_switch_arm(&arm.pattern);
                        let mut body = Vec::new();
                        self.lower_as_stmts(arm.body, &mut body);
                        TypeSwitchArm { binding, ty, body }
                    })
                    .collect();
                let default = else_arm.map(|e| {
                    let mut body = Vec::new();
                    self.lower_as_stmts(*e, &mut body);
                    body
                });
                out.push(GoStmt::TypeSwitch {
                    value: switch_val,
                    arms: switch_arms,
                    default,
                });
            }

            ExprKind::Async(e) => self.lower_as_stmts(*e, out),

            ExprKind::InlineGo(s) => {
                for line in s.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        out.push(GoStmt::Expr(GoExpr::Raw(trimmed.to_string())));
                    }
                }
            }

            ExprKind::Jsx(node) => {
                let val = self.lower_server_jsx(*node, out);
                out.push(GoStmt::Expr(val));
            }

            _ => {
                let val = self.lower_as_value(
                    Expr {
                        kind: expr.kind,
                        ty: expr.ty,
                        span: expr.span,
                    },
                    out,
                );
                if !matches!(val, GoExpr::Nil) {
                    out.push(GoStmt::Expr(val));
                }
            }
        }
    }

    fn compound_assign(
        &mut self,
        op: GoBinOp,
        target: Expr<Typed>,
        value: Expr<Typed>,
        out: &mut Vec<GoStmt>,
    ) {
        let t = self.lower_as_value(target, out);
        let v = self.lower_as_value(value, out);
        out.push(GoStmt::Assign {
            target: t.clone(),
            value: GoExpr::BinOp {
                op,
                lhs: Box::new(t),
                rhs: Box::new(v),
            },
        });
    }

    fn lower_fn_body(&mut self, body: Expr<Typed>, is_void: bool) -> Vec<GoStmt> {
        let mut stmts = Vec::new();
        if is_void {
            self.lower_as_stmts(body, &mut stmts);
            return stmts;
        }
        match body.kind {
            ExprKind::Block(exprs) => {
                let n = exprs.len();
                for (i, e) in exprs.into_iter().enumerate() {
                    if i < n - 1 {
                        self.lower_as_stmts(e, &mut stmts);
                    } else if matches!(e.kind, ExprKind::Return(_)) {
                        self.lower_as_stmts(e, &mut stmts);
                    } else {
                        let val = self.lower_as_value(e, &mut stmts);
                        if !matches!(val, GoExpr::Nil) {
                            stmts.push(GoStmt::Return(Some(val)));
                        }
                    }
                }
            }
            ExprKind::Return(_) | ExprKind::InlineGo(_) => self.lower_as_stmts(body, &mut stmts),
            _ => {
                let val = self.lower_as_value(body, &mut stmts);
                if !matches!(val, GoExpr::Nil) {
                    stmts.push(GoStmt::Return(Some(val)));
                }
            }
        }
        stmts
    }

    fn lower_server_jsx(&mut self, node: JsxNode<Typed>, out: &mut Vec<GoStmt>) -> GoExpr {
        match node {
            JsxNode::Text(s) => GoExpr::Str(s),
            JsxNode::Expr(e) => self.lower_as_value(*e, out),
            JsxNode::Element {
                tag,
                attrs,
                children,
            } => {
                if tag.chars().next().map_or(false, |c| c.is_uppercase()) {
                    // Client component island
                    let props_json = build_props_json(&tag, attrs, self, out);
                    GoExpr::Call {
                        callee: Box::new(GoExpr::Ident(format!("DuckIsland_{tag}"))),
                        args: vec![props_json],
                    }
                } else {
                    // Plain HTML element - render to string
                    let (open_tag, dynamic_attrs) = build_html_open_tag(&tag, &attrs, self, out);
                    let mut parts: Vec<GoExpr> = vec![GoExpr::Str(open_tag)];
                    parts.extend(dynamic_attrs);

                    if children.is_empty() {
                        parts.push(GoExpr::Str(format!("</{tag}>")));
                    } else {
                        for child in children {
                            let child_expr = self.lower_server_jsx(child, out);
                            if !matches!(child_expr, GoExpr::Str(ref s) if s.is_empty()) {
                                parts.push(child_expr);
                            }
                        }
                        parts.push(GoExpr::Str(format!("</{tag}>")));
                    }

                    go_str_concat(parts)
                }
            }
        }
    }

    fn lower_function(&mut self, f: FunctionDecl<Typed>, receiver: Option<GoParam>) -> GoDecl {
        let is_void = f.return_type.as_ref().map(Self::is_void).unwrap_or(true);
        let ret = f.return_type.as_ref().and_then(|t| {
            if Self::is_void(t) {
                None
            } else {
                Some(self.lower_type(t))
            }
        });
        let params: Vec<GoParam> = f
            .params
            .iter()
            .map(|p| GoParam {
                name: escape(&p.name.value),
                ty: self.lower_type(&p.type_expr),
            })
            .collect();
        let body = self.lower_fn_body(f.body, is_void);
        GoDecl::Func {
            name: escape(&f.name.value),
            receiver,
            params,
            ret,
            body,
        }
    }

    fn type_has_generic_param(&self, te: &TypeExpr<Typed>) -> bool {
        match &te.desc {
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } => {
                matches!(self.symbols.get(*type_ref).kind, DefKind::GenericParam)
                    || type_params.iter().any(|tp| self.type_has_generic_param(tp))
            }
            TypeDescription::Array(elem) => self.type_has_generic_param(elem),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.type_has_generic_param(inner)
            }
            TypeDescription::Fun {
                params,
                return_type,
                ..
            } => {
                self.type_has_generic_param(return_type)
                    || params
                        .iter()
                        .any(|p| self.type_has_generic_param(&p.type_expr))
            }
            TypeDescription::Or(vs) | TypeDescription::And(vs) => {
                vs.iter().any(|v| self.type_has_generic_param(v))
            }
            TypeDescription::Tuple(elems) => elems.iter().any(|e| self.type_has_generic_param(e)),
            _ => false,
        }
    }

    fn lower_extension(&mut self, ext: ExtensionDecl<Typed>) -> Vec<GoDecl> {
        let target_generic = self.type_has_generic_param(&ext.target);
        let methods_generic = ext.methods.iter().any(|m| {
            m.return_type
                .as_ref()
                .map_or(false, |rt| self.type_has_generic_param(rt))
                || m.params
                    .iter()
                    .any(|p| self.type_has_generic_param(&p.type_expr))
        });
        if target_generic || methods_generic {
            return vec![];
        }

        if let Some(prefix) = prim_ext_prefix(&ext.target.desc) {
            let self_ty = self.lower_type(&ext.target);
            return ext
                .methods
                .into_iter()
                .filter(|m| m.generics.is_empty() && !m.is_static)
                .map(|m| {
                    let free_name = format!("{}__{}", prefix, escape(&m.name.value));
                    let decl = self.lower_function(m, None);
                    if let GoDecl::Func {
                        params, ret, body, ..
                    } = decl
                    {
                        let mut all_params = vec![GoParam {
                            name: "self".into(),
                            ty: self_ty.clone(),
                        }];
                        all_params.extend(params);
                        GoDecl::Func {
                            name: free_name,
                            receiver: None,
                            params: all_params,
                            ret,
                            body,
                        }
                    } else {
                        unreachable!()
                    }
                })
                .collect();
        }

        // Structs already lower to *T; don't double-wrap.
        let receiver_ty = match self.lower_type(&ext.target) {
            GoType::Ptr(inner) => GoType::Ptr(inner),
            other => GoType::Ptr(Box::new(other)),
        };
        ext.methods
            .into_iter()
            .filter(|m| m.generics.is_empty())
            .map(|m| {
                let recv = GoParam {
                    name: "self".into(),
                    ty: receiver_ty.clone(),
                };
                self.lower_function(m, Some(recv))
            })
            .collect()
    }

    fn lower_struct(&mut self, s: StructDecl<Typed>) -> Vec<GoDecl> {
        let struct_name = escape(&s.name.value);
        let go_fields: Vec<GoField> = s
            .fields
            .iter()
            .map(|f| GoField {
                name: escape(&f.name.value),
                ty: self.lower_type(&f.type_expr),
            })
            .collect();

        let mut decls = vec![GoDecl::Struct {
            name: struct_name.clone(),
            fields: go_fields.clone(),
        }];
        let recv_ty = GoType::Ptr(Box::new(GoType::Named(struct_name.clone())));

        for go_field in &go_fields {
            let cap = capitalize(&go_field.name);
            let recv = GoParam {
                name: "self".into(),
                ty: recv_ty.clone(),
            };
            let self_field = GoExpr::Field {
                base: Box::new(GoExpr::Ident("self".into())),
                field: go_field.name.clone(),
            };

            decls.push(GoDecl::Func {
                name: format!("Get{cap}"),
                receiver: Some(recv.clone()),
                params: vec![],
                ret: Some(go_field.ty.clone()),
                body: vec![GoStmt::Return(Some(self_field.clone()))],
            });

            decls.push(GoDecl::Func {
                name: format!("GetPtr{cap}"),
                receiver: Some(recv.clone()),
                params: vec![],
                ret: Some(GoType::Ptr(Box::new(go_field.ty.clone()))),
                body: vec![GoStmt::Return(Some(GoExpr::Ref(Box::new(
                    self_field.clone(),
                ))))],
            });

            decls.push(GoDecl::Func {
                name: format!("Set{cap}"),
                receiver: Some(recv.clone()),
                params: vec![GoParam {
                    name: "param".into(),
                    ty: go_field.ty.clone(),
                }],
                ret: None,
                body: vec![GoStmt::Assign {
                    target: self_field,
                    value: GoExpr::Ident("param".into()),
                }],
            });
        }

        use crate::parse::struct_parser::DerivableInterface;

        if s.derived.contains(&DerivableInterface::ToJson) {
            self.imports.insert("fmt".into());
            let mut parts: Vec<String> = Vec::new();
            for (f, gf) in s.fields.iter().zip(go_fields.iter()) {
                let val = format!("self.{}", gf.name);
                let json_expr = self.field_to_json_code(&f.type_expr.desc, &val);
                parts.push(format!("\"\\\"{}\\\": \"+({json_expr})", gf.name));
            }
            let body = if parts.is_empty() {
                "return \"{}\"".to_string()
            } else {
                format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
            };
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_json() string {{ {body} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::ToString)
            || s.derived.contains(&DerivableInterface::EmitJs)
        {
            self.imports.insert("fmt".into());
            let mut parts: Vec<String> = Vec::new();
            for (f, gf) in s.fields.iter().zip(go_fields.iter()) {
                let val = format!("self.{}", gf.name);
                let str_expr = self.field_to_string_code(&f.type_expr.desc, &val);
                parts.push(format!("\"{}: \"+({str_expr})", gf.name));
            }
            let inner = if parts.is_empty() {
                String::new()
            } else {
                parts.join("+\", \"+")
            };
            let body = format!("return fmt.Sprintf(\"{struct_name}{{%s}}\", {inner})");
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_string() string {{ {body} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::Eq) {
            let mut comparisons: Vec<String> = s
                .fields
                .iter()
                .zip(go_fields.iter())
                .map(|(f, gf)| {
                    let a = format!("self.{}", gf.name);
                    let b = format!("(*other).{}", gf.name);
                    self.field_eq_code(&f.type_expr.desc, &a, &b)
                })
                .collect();
            if comparisons.is_empty() {
                comparisons.push("true".to_string());
            }
            let body = format!("return {}", comparisons.join(" && "));
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) eq(other **{struct_name}) bool {{ {body} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::Clone) {
            let field_clones: Vec<String> = s
                .fields
                .iter()
                .zip(go_fields.iter())
                .map(|(f, gf)| {
                    let val = format!("self.{}", gf.name);
                    let clone_expr = self.field_clone_code(&f.type_expr.desc, &val);
                    format!("{}: {clone_expr}", gf.name)
                })
                .collect();
            let fields_str = field_clones.join(", ");
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) clone() *{struct_name} {{ return &{struct_name}{{{fields_str}}} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::Hash) {
            let mut go_code = String::from("var _res int\n_res = 1\n");
            for (f, gf) in s.fields.iter().zip(go_fields.iter()) {
                let val = format!("self.{}", gf.name);
                let hash_expr = self.field_hash_code(&f.type_expr.desc, &val);
                go_code.push_str(&format!("_res = (31 * _res) + ({hash_expr})\n"));
            }
            go_code.push_str("return _res");
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) hash() int {{ {go_code} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::Ord) {
            self.used_tags.insert("equal".into());
            self.used_tags.insert("greater".into());
            self.used_tags.insert("smaller".into());
            let mut go_code = String::from("var _r any\n_r = &__DuckTag_equal{}\n");
            for (f, gf) in s.fields.iter().zip(go_fields.iter()) {
                let a = format!("self.{}", gf.name);
                let b = format!("(*other).{}", gf.name);
                let ord_expr = self.field_ord_code(&f.type_expr.desc, &a, &b);
                go_code.push_str(&format!(
                    "_r = {ord_expr}\nswitch _r.(type) {{\ncase *__DuckTag_greater: return &__DuckTag_greater{{}}\ncase *__DuckTag_smaller: return &__DuckTag_smaller{{}}\n}}\n"
                ));
            }
            go_code.push_str("return _r");
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) ord(other **{struct_name}) any {{ {go_code} }}"
            )));
        }

        if s.derived.contains(&DerivableInterface::FromJson) {
            self.imports.insert("errors".into());
            let mut go_code = format!(
                "var _res *{struct_name}\n_obj, _, _err := scan_json_struct_parts(_json_str)\nif _err != nil {{ return _res, _err }}\n"
            );
            let mut all_vars: Vec<(String, String)> = Vec::new();
            for (f, gf) in s.fields.iter().zip(go_fields.iter()) {
                let key = &gf.name;
                let var = format!("_field_{key}");
                let call =
                    self.field_from_json_code(&f.type_expr.desc, &format!("_obj[\"{key}\"]"));
                go_code.push_str(&format!(
                    "{var}, _err := {call}\n_ = {var}\nif _err != nil {{ return _res, _err }}\n"
                ));
                all_vars.push((key.clone(), var));
            }
            let field_inits: String = all_vars
                .iter()
                .map(|(k, v)| format!("{k}: {v}"))
                .collect::<Vec<_>>()
                .join(", ");
            go_code.push_str(&format!("return &{struct_name}{{{field_inits}}}, nil"));
            decls.push(GoDecl::Raw(format!(
                "func {struct_name}_FromJson(_json_str string) (*{struct_name}, error) {{ {go_code} }}"
            )));
        }

        decls
    }

    fn lower_type_alias(&mut self, a: TypeAliasDecl<Typed>) -> GoDecl {
        // And type aliases get a merged interface instead of any
        if let TypeDescription::And(variants) = &a.type_expr.desc.clone() {
            let all_fields = self.collect_and_duck_fields(variants);
            if let Some(all_fields) = all_fields {
                let constraints: Vec<(String, GoType)> = all_fields
                    .iter()
                    .map(|(name, ty_expr)| {
                        let cap = capitalize(name);
                        let go_ty = match self.lower_type(ty_expr) {
                            GoType::DuckInterface(_) => GoType::Any,
                            t => t,
                        };
                        (format!("Has{cap}"), go_ty)
                    })
                    .collect();
                return GoDecl::TypeAlias {
                    name: escape(&a.name.value),
                    ty: GoType::DuckInterface(constraints),
                };
            }
        }
        GoDecl::TypeAlias {
            name: escape(&a.name.value),
            ty: self.lower_type(&a.type_expr),
        }
    }

    fn lower_intrinsic_call(
        &mut self,
        method: &str,
        base_ty: &TypeDescription<Typed>,
        go_base: GoExpr,
    ) -> GoExpr {
        match (base_ty, method) {
            (TypeDescription::Array(_), "len") => GoExpr::Call {
                callee: Box::new(GoExpr::Ident("len".into())),
                args: vec![go_base],
            },
            (_, "to_string") => self.lower_to_string(base_ty, go_base),
            (_, "to_json") => self.lower_to_json(base_ty, go_base),
            _ => unreachable!("unregistered intrinsic {method}"),
        }
    }

    fn gen_type_to_json_code(
        &mut self,
        ty: &TypeDescription<Typed>,
        val: &str,
        depth: usize,
    ) -> String {
        match ty {
            TypeDescription::String(_) => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%q\", ({val}))")
            }
            TypeDescription::Char => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%q\", string({val}))")
            }
            TypeDescription::Int => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::UInt => {
                self.imports.insert("strconv".into());
                format!("strconv.FormatUint(uint64({val}), 10)")
            }
            TypeDescription::Byte => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::Float => {
                self.imports.insert("strconv".into());
                format!("fmt.Sprintf(\"%f\", float64({val}))")
            }
            TypeDescription::Bool(_) => {
                self.imports.insert("strconv".into());
                format!("strconv.FormatBool({val})")
            }
            TypeDescription::Tag(t) => format!("\"\\\"{}\\\"\"", t),
            TypeDescription::Array(elem_ty) => {
                self.imports.insert("strings".into());
                let elem_go_ty = self.lower_type(elem_ty);
                let elem_ty_str = go_type_str(&elem_go_ty);
                let vv = format!("__v{depth}");
                let iv = format!("__i{depth}");
                let parts = format!("__parts{depth}");
                let arr = format!("__arr{depth}");
                let elem_code = self.gen_type_to_json_code(&elem_ty.desc, &vv, depth + 1);
                let range_vv = if elem_code.contains(&vv) {
                    vv.as_str()
                } else {
                    "_"
                };
                format!(
                    "func({arr} []{elem_ty_str}) string {{ {parts} := make([]string, len({arr})); for {iv}, {range_vv} := range {arr} {{ {parts}[{iv}] = {elem_code} }}; return \"[\" + strings.Join({parts}, \", \") + \"]\" }}({val})"
                )
            }
            TypeDescription::Or(variants) | TypeDescription::And(variants) => {
                if matches!(ty, TypeDescription::And(_)) {
                    let all_fields = self.collect_and_duck_fields(variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let constraints: Vec<(String, GoType)> = all_fields
                            .iter()
                            .map(|(name, ty_expr)| {
                                let cap = capitalize(name);
                                let go_ty = match self.lower_type(ty_expr) {
                                    GoType::DuckInterface(_) => GoType::Any,
                                    t => t,
                                };
                                (format!("Has{cap}"), go_ty)
                            })
                            .collect();
                        let iface_str = duck_interface_type_str(&constraints);
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg{depth}.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code =
                                self.gen_type_to_json_code(&field_ty.desc, &inner_val, depth + 1);
                            parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                        }
                        let body = if parts.is_empty() {
                            "return \"{}\"".to_string()
                        } else {
                            format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                        };
                        return format!(
                            "func(__duck_arg{depth} {iface_str}) string {{ {body} }}({val})"
                        );
                    }
                }
                // Or-style (or And fallback when variants aren't all-duck)
                let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
                let or_var = format!("__or{depth}");
                let tmp = format!("__tmp{depth}");
                let ok = format!("__ok{depth}");
                let mut body = format!("var {or_var} any = ({val}); ");
                for variant in &variants_cloned {
                    let go_ty = self.lower_type(variant);
                    let ty_str = match &variant.desc {
                        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
                        _ => match &go_ty {
                            GoType::DuckInterface(constraints) => {
                                duck_interface_type_str(constraints)
                            }
                            _ => go_type_str(&go_ty),
                        },
                    };
                    let inner = if matches!(&go_ty, GoType::Ptr(_)) {
                        format!("(*{tmp})")
                    } else {
                        tmp.clone()
                    };
                    let elem_code = self.gen_type_to_json_code(&variant.desc, &inner, depth + 1);
                    body.push_str(&format!(
                        "if {tmp}, {ok} := ({or_var}).({ty_str}); {ok} {{ _ = {tmp}; return {elem_code} }}; "
                    ));
                }
                body.push_str("return \"null\"");
                format!("func() string {{ {body} }}()")
            }
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.gen_type_to_json_code(&inner.desc, &format!("(*{val})"), depth)
            }
            TypeDescription::Tuple(elems) if elems.is_empty() => "\"[]\"".into(),
            // TypeAlias for duck: generate IIFE using interface getter methods.
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } if type_params.is_empty() => {
                let type_ref_id = *type_ref;
                let expansion = self.type_alias_expansions.get(&type_ref_id).cloned();
                if let Some(TypeDescription::Duck(duck)) = expansion {
                    let alias_name = self.name(type_ref_id);
                    self.imports.insert("fmt".into());
                    let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                        .fields
                        .iter()
                        .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                        .collect();
                    sorted.sort_by(|a, b| a.0.cmp(&b.0));
                    let mut parts: Vec<String> = Vec::new();
                    for (field_name, field_ty) in &sorted {
                        let cap = capitalize(field_name);
                        let getter = format!("__duck_arg{depth}.Get{cap}()");
                        let code = self.gen_type_to_json_code(field_ty, &getter, depth + 1);
                        parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                    }
                    let body = if parts.is_empty() {
                        "return \"{}\"".to_string()
                    } else {
                        format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                    };
                    format!("func(__duck_arg{depth} {alias_name}) string {{ {body} }}({val})")
                } else if let Some(TypeDescription::And(variants)) = expansion {
                    let alias_name = self.name(type_ref_id);
                    let all_fields = self.collect_and_duck_fields(&variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg{depth}.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code =
                                self.gen_type_to_json_code(&field_ty.desc, &inner_val, depth + 1);
                            parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                        }
                        let body = if parts.is_empty() {
                            "return \"{}\"".to_string()
                        } else {
                            format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                        };
                        format!("func(__duck_arg{depth} {alias_name}) string {{ {body} }}({val})")
                    } else {
                        self.gen_type_to_json_code(&TypeDescription::And(variants), val, depth)
                    }
                } else if let Some(TypeDescription::Or(variants)) = expansion {
                    self.gen_type_to_json_code(&TypeDescription::Or(variants), val, depth)
                } else {
                    format!("({val}).to_json()")
                }
            }
            // duck type stored as any - type-assert before calling getters
            TypeDescription::Duck(duck) => {
                let go_ty = self.lower_type_desc(ty);
                let iface_str = match &go_ty {
                    GoType::DuckInterface(constraints) => duck_interface_type_str(constraints),
                    other => go_type_str(other),
                };
                let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                    .fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                    .collect();
                sorted.sort_by(|a, b| a.0.cmp(&b.0));
                let mut parts: Vec<String> = Vec::new();
                for (field_name, field_ty) in &sorted {
                    let cap = capitalize(field_name);
                    let getter = format!("__duck_arg{depth}.Get{cap}()");
                    let code = self.gen_type_to_json_code(field_ty, &getter, depth + 1);
                    parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                }
                let body = if parts.is_empty() {
                    "return \"{}\"".to_string()
                } else {
                    format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                };
                self.imports.insert("fmt".into());
                format!(
                    "func(__duck_any{depth} any) string {{ __duck_arg{depth} := __duck_any{depth}.({iface_str}); {body} }}({val})"
                )
            }
            // Named structs, non-empty tuples all have to_json() methods.
            _ => format!("({val}).to_json()"),
        }
    }

    fn gen_type_to_string_code(
        &mut self,
        ty: &TypeDescription<Typed>,
        val: &str,
        depth: usize,
    ) -> String {
        match ty {
            TypeDescription::String(_) => format!("({val})"),
            TypeDescription::Char => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%c\", ({val}))")
            }
            TypeDescription::Int => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::UInt => {
                self.imports.insert("strconv".into());
                format!("strconv.FormatUint(uint64({val}), 10)")
            }
            TypeDescription::Byte => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::Float => {
                self.imports.insert("strconv".into());
                format!("fmt.Sprintf(\"%f\", float64({val}))")
            }
            TypeDescription::Bool(_) => {
                self.imports.insert("strconv".into());
                format!("strconv.FormatBool({val})")
            }
            TypeDescription::Tag(t) => format!("\"{}\"", t),
            TypeDescription::Array(elem_ty) => {
                self.imports.insert("strings".into());
                let elem_go_ty = self.lower_type(elem_ty);
                let elem_ty_str = go_type_str(&elem_go_ty);
                let vv = format!("__v{depth}");
                let iv = format!("__i{depth}");
                let parts = format!("__parts{depth}");
                let arr = format!("__arr{depth}");
                let elem_code = self.gen_type_to_string_code(&elem_ty.desc, &vv, depth + 1);
                let range_vv = if elem_code.contains(&vv) {
                    vv.as_str()
                } else {
                    "_"
                };
                format!(
                    "func({arr} []{elem_ty_str}) string {{ {parts} := make([]string, len({arr})); for {iv}, {range_vv} := range {arr} {{ {parts}[{iv}] = {elem_code} }}; return \"[\" + strings.Join({parts}, \", \") + \"]\" }}({val})"
                )
            }
            TypeDescription::Or(variants) | TypeDescription::And(variants) => {
                if matches!(ty, TypeDescription::And(_)) {
                    let all_fields = self.collect_and_duck_fields(variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let constraints: Vec<(String, GoType)> = all_fields
                            .iter()
                            .map(|(name, ty_expr)| {
                                let cap = capitalize(name);
                                let go_ty = match self.lower_type(ty_expr) {
                                    GoType::DuckInterface(_) => GoType::Any,
                                    t => t,
                                };
                                (format!("Has{cap}"), go_ty)
                            })
                            .collect();
                        let iface_str = duck_interface_type_str(&constraints);
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg{depth}.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code =
                                self.gen_type_to_string_code(&field_ty.desc, &inner_val, depth + 1);
                            parts.push(format!("\"{}: \"+({code})", field_name));
                        }
                        let str_inner = parts.join("+\",\"+");
                        let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                        return format!(
                            "func(__duck_arg{depth} {iface_str}) string {{ {body} }}({val})"
                        );
                    }
                }
                // Or-style (or And fallback)
                let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
                let or_var = format!("__or{depth}");
                let tmp = format!("__tmp{depth}");
                let ok = format!("__ok{depth}");
                let mut body = format!("var {or_var} any = ({val}); ");
                for variant in &variants_cloned {
                    let go_ty = self.lower_type(variant);
                    let ty_str = match &variant.desc {
                        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
                        _ => match &go_ty {
                            GoType::DuckInterface(constraints) => {
                                duck_interface_type_str(constraints)
                            }
                            _ => go_type_str(&go_ty),
                        },
                    };
                    let inner = if matches!(&go_ty, GoType::Ptr(_)) {
                        format!("(*{tmp})")
                    } else {
                        tmp.clone()
                    };
                    let elem_code = self.gen_type_to_string_code(&variant.desc, &inner, depth + 1);
                    body.push_str(&format!(
                        "if {tmp}, {ok} := ({or_var}).({ty_str}); {ok} {{ _ = {tmp}; return {elem_code} }}; "
                    ));
                }
                body.push_str("return \"?\"");
                format!("func() string {{ {body} }}()")
            }
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.gen_type_to_string_code(&inner.desc, &format!("(*{val})"), depth)
            }
            TypeDescription::Tuple(elems) if elems.is_empty() => "\"()\"".into(),
            // TypeAlias for duck: generate IIFE using interface getter methods.
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } if type_params.is_empty() => {
                let type_ref_id = *type_ref;
                let expansion = self.type_alias_expansions.get(&type_ref_id).cloned();
                if let Some(TypeDescription::Duck(duck)) = expansion {
                    let alias_name = self.name(type_ref_id);
                    self.imports.insert("fmt".into());
                    let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                        .fields
                        .iter()
                        .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                        .collect();
                    sorted.sort_by(|a, b| a.0.cmp(&b.0));
                    let mut parts: Vec<String> = Vec::new();
                    for (field_name, field_ty) in &sorted {
                        let cap = capitalize(field_name);
                        let getter = format!("__duck_arg{depth}.Get{cap}()");
                        let code = self.gen_type_to_string_code(field_ty, &getter, depth + 1);
                        parts.push(format!("\"{}: \"+({code})", field_name));
                    }
                    let str_inner = parts.join("+\",\"+");
                    let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                    format!("func(__duck_arg{depth} {alias_name}) string {{ {body} }}({val})")
                } else if let Some(TypeDescription::And(variants)) = expansion {
                    let alias_name = self.name(type_ref_id);
                    let all_fields = self.collect_and_duck_fields(&variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg{depth}.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code =
                                self.gen_type_to_string_code(&field_ty.desc, &inner_val, depth + 1);
                            parts.push(format!("\"{}: \"+({code})", field_name));
                        }
                        let str_inner = parts.join("+\",\"+");
                        let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                        format!("func(__duck_arg{depth} {alias_name}) string {{ {body} }}({val})")
                    } else {
                        self.gen_type_to_string_code(&TypeDescription::And(variants), val, depth)
                    }
                } else if let Some(TypeDescription::Or(variants)) = expansion {
                    self.gen_type_to_string_code(&TypeDescription::Or(variants), val, depth)
                } else {
                    format!("({val}).to_string()")
                }
            }
            // duck type stored as any - type-assert before calling getters
            TypeDescription::Duck(duck) => {
                let go_ty = self.lower_type_desc(ty);
                let iface_str = match &go_ty {
                    GoType::DuckInterface(constraints) => duck_interface_type_str(constraints),
                    other => go_type_str(other),
                };
                let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                    .fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                    .collect();
                sorted.sort_by(|a, b| a.0.cmp(&b.0));
                let mut parts: Vec<String> = Vec::new();
                for (field_name, field_ty) in &sorted {
                    let cap = capitalize(field_name);
                    let getter = format!("__duck_arg{depth}.Get{cap}()");
                    let code = self.gen_type_to_string_code(field_ty, &getter, depth + 1);
                    parts.push(format!("\"{}: \"+({code})", field_name));
                }
                let str_inner = parts.join("+\",\"+");
                let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                self.imports.insert("fmt".into());
                format!(
                    "func(__duck_any{depth} any) string {{ __duck_arg{depth} := __duck_any{depth}.({iface_str}); {body} }}({val})"
                )
            }
            // Named structs, non-empty tuples all have to_string() methods.
            _ => format!("({val}).to_string()"),
        }
    }

    fn lower_to_string(&mut self, ty: &TypeDescription<Typed>, val: GoExpr) -> GoExpr {
        match ty {
            TypeDescription::String(_) => val,
            TypeDescription::Char => GoExpr::Cast {
                ty: GoType::String,
                value: Box::new(val),
            },
            TypeDescription::Int => {
                self.imports.insert("strconv".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("strconv".into())),
                        field: "Itoa".into(),
                    }),
                    args: vec![val],
                }
            }
            TypeDescription::UInt => {
                self.imports.insert("strconv".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("strconv".into())),
                        field: "FormatUint".into(),
                    }),
                    args: vec![val, GoExpr::Int(10)],
                }
            }
            TypeDescription::Byte => {
                self.imports.insert("strconv".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("strconv".into())),
                        field: "FormatUint".into(),
                    }),
                    args: vec![
                        GoExpr::Cast {
                            ty: GoType::Uint64,
                            value: Box::new(val),
                        },
                        GoExpr::Int(10),
                    ],
                }
            }
            TypeDescription::Float => {
                self.imports.insert("fmt".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("fmt".into())),
                        field: "Sprintf".into(),
                    }),
                    args: vec![
                        GoExpr::Str("%f".into()),
                        GoExpr::Cast {
                            ty: GoType::Float64,
                            value: Box::new(val),
                        },
                    ],
                }
            }
            TypeDescription::Bool(_) => {
                self.imports.insert("strconv".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("strconv".into())),
                        field: "FormatBool".into(),
                    }),
                    args: vec![val],
                }
            }
            TypeDescription::Array(elem_ty) => {
                self.imports.insert("strings".into());
                let elem_go_ty = self.lower_type(elem_ty);
                let elem_ty_str = go_type_str(&elem_go_ty);
                let elem_code = self.gen_type_to_string_code(&elem_ty.desc, "__v", 1);
                let lambda = format!(
                    "func(__arr_arg []{elem_ty_str}) string {{ __parts := make([]string, len(__arr_arg)); for __i, __v := range __arr_arg {{ __parts[__i] = {elem_code} }}; return \"[\" + strings.Join(__parts, \", \") + \"]\" }}"
                );
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(lambda)),
                    args: vec![val],
                }
            }
            TypeDescription::Or(variants) | TypeDescription::And(variants) => {
                if matches!(ty, TypeDescription::And(_)) {
                    let all_fields = self.collect_and_duck_fields(variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let constraints: Vec<(String, GoType)> = all_fields
                            .iter()
                            .map(|(name, ty_expr)| {
                                let cap = capitalize(name);
                                let go_ty = match self.lower_type(ty_expr) {
                                    GoType::DuckInterface(_) => GoType::Any,
                                    t => t,
                                };
                                (format!("Has{cap}"), go_ty)
                            })
                            .collect();
                        let iface_str = duck_interface_type_str(&constraints);
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code = self.gen_type_to_string_code(&field_ty.desc, &inner_val, 1);
                            parts.push(format!("\"{}: \"+({code})", field_name));
                        }
                        let str_inner = parts.join("+\",\"+");
                        let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                        return GoExpr::Call {
                            callee: Box::new(GoExpr::Raw(format!(
                                "func(__duck_arg {iface_str}) string {{ {body} }}"
                            ))),
                            args: vec![val],
                        };
                    }
                }
                // Or-style (or And fallback)
                let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
                let mut body = String::new();
                for variant in &variants_cloned {
                    let go_ty = self.lower_type(variant);
                    let ty_str = match &variant.desc {
                        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
                        _ => match &go_ty {
                            GoType::DuckInterface(constraints) => {
                                duck_interface_type_str(constraints)
                            }
                            _ => go_type_str(&go_ty),
                        },
                    };
                    let inner = if matches!(&go_ty, GoType::Ptr(_)) {
                        "(*__tmp)".to_string()
                    } else {
                        "__tmp".to_string()
                    };
                    let elem_code = self.gen_type_to_string_code(&variant.desc, &inner, 1);
                    body.push_str(&format!(
                        "if __tmp, __ok := __or_val.({ty_str}); __ok {{ _ = __tmp; return {elem_code} }}; "
                    ));
                }
                body.push_str("return \"?\"");
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(format!(
                        "func(__or_val any) string {{ {body} }}"
                    ))),
                    args: vec![val],
                }
            }
            // Duck types: generate IIFE using interface getter methods (Go interface has no to_string).
            TypeDescription::Duck(duck) => {
                let duck = duck.clone();
                let go_ty = self.lower_type_desc(ty);
                let param_ty_str = match &go_ty {
                    GoType::DuckInterface(constraints) => duck_interface_type_str(constraints),
                    other => go_type_str(other),
                };
                self.imports.insert("fmt".into());
                let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                    .fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                    .collect();
                sorted.sort_by(|a, b| a.0.cmp(&b.0));
                let mut parts: Vec<String> = Vec::new();
                for (field_name, field_ty) in &sorted {
                    let cap = capitalize(field_name);
                    let getter = format!("__duck_arg.Get{cap}()");
                    let code = self.gen_type_to_string_code(field_ty, &getter, 1);
                    parts.push(format!("\"{}: \"+({code})", field_name));
                }
                let str_inner = parts.join("+\",\"+");
                let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(format!(
                        "func(__duck_arg {param_ty_str}) string {{ {body} }}"
                    ))),
                    args: vec![val],
                }
            }
            // TypeAlias for duck type: generate IIFE using interface getter methods.
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } if type_params.is_empty() => {
                let type_ref_id = *type_ref;
                let expansion = self.type_alias_expansions.get(&type_ref_id).cloned();
                let alias_name = self.name(type_ref_id);
                match expansion {
                    Some(TypeDescription::Duck(duck)) => {
                        self.imports.insert("fmt".into());
                        let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                            .fields
                            .iter()
                            .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                            .collect();
                        sorted.sort_by(|a, b| a.0.cmp(&b.0));
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &sorted {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg.Get{cap}()");
                            let code = self.gen_type_to_string_code(field_ty, &getter, 1);
                            parts.push(format!("\"{}: \"+({code})", field_name));
                        }
                        let str_inner = parts.join("+\",\"+");
                        let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                        GoExpr::Call {
                            callee: Box::new(GoExpr::Raw(format!(
                                "func(__duck_arg {alias_name}) string {{ {body} }}"
                            ))),
                            args: vec![val],
                        }
                    }
                    Some(TypeDescription::And(variants)) => {
                        let all_fields = self.collect_and_duck_fields(&variants);
                        if let Some(all_fields) = all_fields {
                            self.imports.insert("fmt".into());
                            let mut parts: Vec<String> = Vec::new();
                            for (field_name, field_ty) in &all_fields {
                                let cap = capitalize(field_name);
                                let getter = format!("__duck_arg.Get{cap}()");
                                let go_ty = self.lower_type(field_ty);
                                let inner_val = match &go_ty {
                                    GoType::Ptr(_) => format!("(*{getter})"),
                                    _ => getter,
                                };
                                let code =
                                    self.gen_type_to_string_code(&field_ty.desc, &inner_val, 1);
                                parts.push(format!("\"{}: \"+({code})", field_name));
                            }
                            let str_inner = parts.join("+\",\"+");
                            let body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
                            GoExpr::Call {
                                callee: Box::new(GoExpr::Raw(format!(
                                    "func(__duck_arg {alias_name}) string {{ {body} }}"
                                ))),
                                args: vec![val],
                            }
                        } else {
                            self.lower_to_string(&TypeDescription::And(variants), val)
                        }
                    }
                    Some(expanded) => self.lower_to_string(&expanded, val),
                    None => GoExpr::Call {
                        callee: Box::new(GoExpr::Field {
                            base: Box::new(val),
                            field: "to_string".into(),
                        }),
                        args: vec![],
                    },
                }
            }
            // Empty tuple () lowers to `any`; inline the literal string.
            TypeDescription::Tuple(elems) if elems.is_empty() => GoExpr::Str("()".into()),
            _ => unreachable!("to_string on unsupported type: {:?}", ty),
        }
    }

    fn lower_to_json(&mut self, ty: &TypeDescription<Typed>, val: GoExpr) -> GoExpr {
        match ty {
            TypeDescription::String(_) => {
                self.imports.insert("fmt".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("fmt".into())),
                        field: "Sprintf".into(),
                    }),
                    args: vec![GoExpr::Str("%q".into()), val],
                }
            }
            TypeDescription::Char => {
                self.imports.insert("fmt".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("fmt".into())),
                        field: "Sprintf".into(),
                    }),
                    args: vec![
                        GoExpr::Str("%q".into()),
                        GoExpr::Cast {
                            ty: GoType::String,
                            value: Box::new(val),
                        },
                    ],
                }
            }
            TypeDescription::Array(elem_ty) => {
                self.imports.insert("strings".into());
                let elem_go_ty = self.lower_type(elem_ty);
                let elem_ty_str = go_type_str(&elem_go_ty);
                let elem_code = self.gen_type_to_json_code(&elem_ty.desc, "__v", 1);
                let lambda = format!(
                    "func(__arr_arg []{elem_ty_str}) string {{ __parts := make([]string, len(__arr_arg)); for __i, __v := range __arr_arg {{ __parts[__i] = {elem_code} }}; return \"[\" + strings.Join(__parts, \", \") + \"]\" }}"
                );
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(lambda)),
                    args: vec![val],
                }
            }
            TypeDescription::Or(variants) | TypeDescription::And(variants) => {
                if matches!(ty, TypeDescription::And(_)) {
                    let all_fields = self.collect_and_duck_fields(variants);
                    if let Some(all_fields) = all_fields {
                        self.imports.insert("fmt".into());
                        let constraints: Vec<(String, GoType)> = all_fields
                            .iter()
                            .map(|(name, ty_expr)| {
                                let cap = capitalize(name);
                                let go_ty = match self.lower_type(ty_expr) {
                                    GoType::DuckInterface(_) => GoType::Any,
                                    t => t,
                                };
                                (format!("Has{cap}"), go_ty)
                            })
                            .collect();
                        let iface_str = duck_interface_type_str(&constraints);
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &all_fields {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg.Get{cap}()");
                            let go_ty = self.lower_type(field_ty);
                            let inner_val = match &go_ty {
                                GoType::Ptr(_) => format!("(*{getter})"),
                                _ => getter,
                            };
                            let code = self.gen_type_to_json_code(&field_ty.desc, &inner_val, 1);
                            parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                        }
                        let body = if parts.is_empty() {
                            "return \"{}\"".to_string()
                        } else {
                            format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                        };
                        return GoExpr::Call {
                            callee: Box::new(GoExpr::Raw(format!(
                                "func(__duck_arg {iface_str}) string {{ {body} }}"
                            ))),
                            args: vec![val],
                        };
                    }
                }
                // Or-style (or And fallback)
                let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
                let mut body = String::new();
                for variant in &variants_cloned {
                    let go_ty = self.lower_type(variant);
                    let ty_str = match &variant.desc {
                        TypeDescription::Tag(name) => format!("*__DuckTag_{name}"),
                        _ => match &go_ty {
                            GoType::DuckInterface(constraints) => {
                                duck_interface_type_str(constraints)
                            }
                            _ => go_type_str(&go_ty),
                        },
                    };
                    let inner = if matches!(&go_ty, GoType::Ptr(_)) {
                        "(*__tmp)".to_string()
                    } else {
                        "__tmp".to_string()
                    };
                    let elem_code = self.gen_type_to_json_code(&variant.desc, &inner, 1);
                    body.push_str(&format!(
                        "if __tmp, __ok := __or_val.({ty_str}); __ok {{ _ = __tmp; return {elem_code} }}; "
                    ));
                }
                body.push_str("return \"null\"");
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(format!(
                        "func(__or_val any) string {{ {body} }}"
                    ))),
                    args: vec![val],
                }
            }
            // Duck types: generate IIFE using interface getter methods (Go interface has no to_json).
            TypeDescription::Duck(duck) => {
                let duck = duck.clone();
                let go_ty = self.lower_type_desc(ty);
                let param_ty_str = match &go_ty {
                    GoType::DuckInterface(constraints) => duck_interface_type_str(constraints),
                    other => go_type_str(other),
                };
                self.imports.insert("fmt".into());
                let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                    .fields
                    .iter()
                    .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                    .collect();
                sorted.sort_by(|a, b| a.0.cmp(&b.0));
                let mut parts: Vec<String> = Vec::new();
                for (field_name, field_ty) in &sorted {
                    let cap = capitalize(field_name);
                    let getter = format!("__duck_arg.Get{cap}()");
                    let code = self.gen_type_to_json_code(field_ty, &getter, 1);
                    parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                }
                let body = if parts.is_empty() {
                    "return \"{}\"".to_string()
                } else {
                    format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                };
                GoExpr::Call {
                    callee: Box::new(GoExpr::Raw(format!(
                        "func(__duck_arg {param_ty_str}) string {{ {body} }}"
                    ))),
                    args: vec![val],
                }
            }
            // TypeAlias for duck type: generate IIFE using interface getter methods.
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } if type_params.is_empty() => {
                let type_ref_id = *type_ref;
                let expansion = self.type_alias_expansions.get(&type_ref_id).cloned();
                let alias_name = self.name(type_ref_id);
                match expansion {
                    Some(TypeDescription::Duck(duck)) => {
                        self.imports.insert("fmt".into());
                        let mut sorted: Vec<(String, TypeDescription<Typed>)> = duck
                            .fields
                            .iter()
                            .map(|f| (f.name.value.clone(), f.type_expr.desc.clone()))
                            .collect();
                        sorted.sort_by(|a, b| a.0.cmp(&b.0));
                        let mut parts: Vec<String> = Vec::new();
                        for (field_name, field_ty) in &sorted {
                            let cap = capitalize(field_name);
                            let getter = format!("__duck_arg.Get{cap}()");
                            let code = self.gen_type_to_json_code(field_ty, &getter, 1);
                            parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                        }
                        let body = if parts.is_empty() {
                            "return \"{}\"".to_string()
                        } else {
                            format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                        };
                        GoExpr::Call {
                            callee: Box::new(GoExpr::Raw(format!(
                                "func(__duck_arg {alias_name}) string {{ {body} }}"
                            ))),
                            args: vec![val],
                        }
                    }
                    Some(TypeDescription::And(variants)) => {
                        let all_fields = self.collect_and_duck_fields(&variants);
                        if let Some(all_fields) = all_fields {
                            self.imports.insert("fmt".into());
                            let mut parts: Vec<String> = Vec::new();
                            for (field_name, field_ty) in &all_fields {
                                let cap = capitalize(field_name);
                                let getter = format!("__duck_arg.Get{cap}()");
                                let go_ty = self.lower_type(field_ty);
                                let inner_val = match &go_ty {
                                    GoType::Ptr(_) => format!("(*{getter})"),
                                    _ => getter,
                                };
                                let code =
                                    self.gen_type_to_json_code(&field_ty.desc, &inner_val, 1);
                                parts.push(format!("\"\\\"{}\\\": \"+({code})", field_name));
                            }
                            let body = if parts.is_empty() {
                                "return \"{}\"".to_string()
                            } else {
                                format!("return fmt.Sprintf(\"{{%s}}\", {})", parts.join("+\",\"+"))
                            };
                            GoExpr::Call {
                                callee: Box::new(GoExpr::Raw(format!(
                                    "func(__duck_arg {alias_name}) string {{ {body} }}"
                                ))),
                                args: vec![val],
                            }
                        } else {
                            self.lower_to_json(&TypeDescription::And(variants), val)
                        }
                    }
                    Some(expanded) => self.lower_to_json(&expanded, val),
                    None => self.lower_to_string(ty, val),
                }
            }
            // Empty tuple () lowers to `any`; inline the literal string.
            TypeDescription::Tuple(elems) if elems.is_empty() => GoExpr::Str("[]".into()),
            _ => self.lower_to_string(ty, val),
        }
    }

    fn field_to_json_code(&mut self, ty: &TypeDescription<Typed>, val: &str) -> String {
        match ty {
            TypeDescription::String(_) => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%q\", ({val}))")
            }
            TypeDescription::Char => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%q\", string({val}))")
            }
            TypeDescription::Int => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa({val})")
            }
            TypeDescription::UInt => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%d\", ({val}))")
            }
            TypeDescription::Byte => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::Float => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%f\", ({val}))")
            }
            TypeDescription::Bool(_) => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%t\", ({val}))")
            }
            TypeDescription::Tag(t) => format!("\"\\\"{}\\\"\"", t),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_to_json_code(&inner.desc, &format!("(*{val})"))
            }
            _ => self.gen_type_to_json_code(ty, val, 0),
        }
    }

    fn field_to_string_code(&mut self, ty: &TypeDescription<Typed>, val: &str) -> String {
        match ty {
            TypeDescription::String(_) => format!("({val})"),
            TypeDescription::Int => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa({val})")
            }
            TypeDescription::UInt => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%d\", ({val}))")
            }
            TypeDescription::Byte => {
                self.imports.insert("strconv".into());
                format!("strconv.Itoa(int({val}))")
            }
            TypeDescription::Float => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%f\", ({val}))")
            }
            TypeDescription::Bool(_) => {
                self.imports.insert("strconv".into());
                format!("strconv.FormatBool({val})")
            }
            TypeDescription::Char => {
                self.imports.insert("fmt".into());
                format!("fmt.Sprintf(\"%c\", ({val}))")
            }
            TypeDescription::Tag(t) => format!("\"{}\"", t),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_to_string_code(&inner.desc, &format!("(*{val})"))
            }
            _ => self.gen_type_to_string_code(ty, val, 0),
        }
    }

    fn field_eq_code(&self, ty: &TypeDescription<Typed>, a: &str, b: &str) -> String {
        match ty {
            TypeDescription::Int
            | TypeDescription::UInt
            | TypeDescription::Float
            | TypeDescription::Bool(_)
            | TypeDescription::Char
            | TypeDescription::Byte
            | TypeDescription::String(_)
            | TypeDescription::Tag(_) => format!("({a}) == ({b})"),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_eq_code(&inner.desc, &format!("(*{a})"), &format!("(*{b})"))
            }
            _ => format!("({a}).eq(&({b}))"),
        }
    }

    fn field_hash_code(&mut self, ty: &TypeDescription<Typed>, val: &str) -> String {
        match ty {
            TypeDescription::Int | TypeDescription::Char => format!("({val})"),
            TypeDescription::UInt | TypeDescription::Byte => format!("int({val})"),
            TypeDescription::Float => {
                self.imports.insert("math".into());
                format!("int(math.Float64bits({val}))")
            }
            TypeDescription::Bool(_) => {
                format!("func() int {{ if {val} {{ return 1 }}; return 0 }}()")
            }
            TypeDescription::String(_) => {
                self.imports.insert("hash/maphash".into());
                format!(
                    "func() int {{ var _h maphash.Hash; _h.WriteString({val}); return int(_h.Sum64()) }}()"
                )
            }
            TypeDescription::Tag(_) => "1".to_string(),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_hash_code(&inner.desc, &format!("(*{val})"))
            }
            _ => format!("({val}).hash()"),
        }
    }

    fn field_clone_code(&self, ty: &TypeDescription<Typed>, val: &str) -> String {
        match ty {
            TypeDescription::Int
            | TypeDescription::UInt
            | TypeDescription::Float
            | TypeDescription::Bool(_)
            | TypeDescription::Char
            | TypeDescription::Byte
            | TypeDescription::String(_)
            | TypeDescription::Tag(_) => format!("({val})"),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_clone_code(&inner.desc, &format!("(*{val})"))
            }
            _ => format!("*({val}).clone()"),
        }
    }

    fn field_ord_code(&mut self, ty: &TypeDescription<Typed>, a: &str, b: &str) -> String {
        self.used_tags.insert("equal".into());
        self.used_tags.insert("greater".into());
        self.used_tags.insert("smaller".into());
        match ty {
            TypeDescription::Int
            | TypeDescription::UInt
            | TypeDescription::Float
            | TypeDescription::Char
            | TypeDescription::Byte => format!(
                "func() any {{ if ({a}) < ({b}) {{ return &__DuckTag_smaller{{}} }} else if ({a}) > ({b}) {{ return &__DuckTag_greater{{}} }}; return &__DuckTag_equal{{}} }}()"
            ),
            TypeDescription::String(_) => {
                self.imports.insert("strings".into());
                format!(
                    "func() any {{ _cmp := strings.Compare({a}, {b}); if _cmp < 0 {{ return &__DuckTag_smaller{{}} }} else if _cmp > 0 {{ return &__DuckTag_greater{{}} }}; return &__DuckTag_equal{{}} }}()"
                )
            }
            TypeDescription::Bool(_) => format!(
                "func() any {{ if !({a}) && ({b}) {{ return &__DuckTag_smaller{{}} }} else if ({a}) && !({b}) {{ return &__DuckTag_greater{{}} }}; return &__DuckTag_equal{{}} }}()"
            ),
            TypeDescription::Tag(_) => "(&__DuckTag_equal{})".to_string(),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_ord_code(&inner.desc, &format!("(*{a})"), &format!("(*{b})"))
            }
            _ => format!("({a}).ord(&({b}))"),
        }
    }

    // collect all (field_name, TypeExpr) from And variants; None if any variant is not a duck type
    fn collect_and_duck_fields(
        &mut self,
        variants: &[TypeExpr<Typed>],
    ) -> Option<Vec<(String, TypeExpr<Typed>)>> {
        let mut all_fields: Vec<(String, TypeExpr<Typed>)> = Vec::new();
        for variant in variants {
            match &variant.desc.clone() {
                TypeDescription::Duck(duck) => {
                    all_fields.extend(
                        duck.fields
                            .iter()
                            .map(|f| (f.name.value.clone(), f.type_expr.clone())),
                    );
                }
                TypeDescription::TypeName {
                    type_ref,
                    type_params,
                } if type_params.is_empty() => {
                    let expansion = self.type_alias_expansions.get(type_ref).cloned();
                    match expansion {
                        Some(TypeDescription::Duck(duck)) => {
                            all_fields.extend(
                                duck.fields
                                    .iter()
                                    .map(|f| (f.name.value.clone(), f.type_expr.clone())),
                            );
                        }
                        _ => return None,
                    }
                }
                _ => return None,
            }
        }
        if all_fields.is_empty() {
            return None;
        }
        all_fields.sort_by(|a, b| a.0.cmp(&b.0));
        let mut seen = std::collections::HashSet::new();
        all_fields.retain(|(name, _)| seen.insert(name.clone()));
        Some(all_fields)
    }

    fn field_from_json_code(&mut self, ty: &TypeDescription<Typed>, val: &str) -> String {
        match ty {
            TypeDescription::Int => format!("int_FromJson({val})"),
            TypeDescription::UInt => format!(
                "func() (uint64, error) {{ _v, _e := uint_FromJson({val}); return uint64(_v), _e }}()"
            ),
            TypeDescription::Byte => format!("byte_FromJson({val})"),
            TypeDescription::Float => format!("float64_FromJson({val})"),
            TypeDescription::Bool(_) => format!("bool_FromJson({val})"),
            TypeDescription::String(_) => format!("string_FromJson({val})"),
            TypeDescription::Char => format!("rune_FromJson({val})"),
            TypeDescription::TypeName {
                type_ref,
                type_params,
            } if type_params.is_empty() => {
                if matches!(self.symbols.get(*type_ref).kind, DefKind::GenericParam) {
                    self.imports.insert("errors".into());
                    format!(
                        "func() (any, error) {{ return nil, errors.New(\"unsupported type\") }}()"
                    )
                } else {
                    let name = self.name(*type_ref);
                    format!("{name}_FromJson({val})")
                }
            }
            TypeDescription::Struct { name, .. } => format!("{name}_FromJson({val})"),
            TypeDescription::Ref(inner) | TypeDescription::RefMut(inner) => {
                self.field_from_json_code(&inner.desc, val)
            }
            TypeDescription::Tuple(elems) if elems.is_empty() => {
                format!("func() (any, error) {{ return nil, nil }}()")
            }
            TypeDescription::Tuple(elems) => {
                let elems_cloned: Vec<TypeExpr<Typed>> = elems.to_vec();
                let elem_tys: Vec<GoType> =
                    elems_cloned.iter().map(|e| self.lower_type(e)).collect();
                let struct_name = tuple_struct_name(&elem_tys);
                format!("{struct_name}_FromJson({val})")
            }
            TypeDescription::Array(inner) => {
                self.imports.insert("errors".into());
                let elem_go_ty = self.lower_type(inner);
                let elem_ty_str = go_type_str(&elem_go_ty);
                let inner_call = self.field_from_json_code(&inner.desc, "_afi_parts[_afi_i]");
                format!(
                    "func() ([]{elem_ty_str}, error) {{ var _afi_res []{elem_ty_str}; _afi_parts, _, _afi_err := scan_json_array_parts({val}); if _afi_err != nil {{ return _afi_res, _afi_err }}; _afi_res = make([]{elem_ty_str}, len(_afi_parts)); for _afi_i := range _afi_res {{ _afi_tmp, _afi_err2 := {inner_call}; if _afi_err2 != nil {{ return _afi_res, _afi_err2 }}; _afi_res[_afi_i] = _afi_tmp }}; return _afi_res, nil }}()"
                )
            }
            TypeDescription::Duck(duck) => {
                // on-demand duck struct; duck-typed fields stored as any
                let fields_cloned: Vec<_> = duck.fields.iter().cloned().collect();
                let name_tys: Vec<(String, GoType)> = fields_cloned
                    .iter()
                    .map(|f| {
                        let go_ty = match self.lower_type(&f.type_expr) {
                            GoType::DuckInterface(_) => GoType::Any,
                            other => other,
                        };
                        (capitalize(&f.name.value), go_ty)
                    })
                    .collect();
                let mut sorted_name_tys = name_tys.clone();
                sorted_name_tys.sort_by(|a, b| a.0.cmp(&b.0));
                let struct_name = duck_struct_name(&sorted_name_tys);
                let go_fields: Vec<GoField> = sorted_name_tys
                    .iter()
                    .map(|(n, t)| GoField {
                        name: n.clone(),
                        ty: t.clone(),
                    })
                    .collect();
                self.duck_structs
                    .entry(struct_name.clone())
                    .or_insert_with(|| go_fields);
                for (n, _) in &sorted_name_tys {
                    self.new_duck_field_names.insert(n.clone());
                }
                let typed: Vec<(String, TypeExpr<Typed>)> = fields_cloned
                    .iter()
                    .map(|f| (capitalize(&f.name.value), f.type_expr.clone()))
                    .collect();
                let mut sorted_typed = typed;
                sorted_typed.sort_by(|a, b| a.0.cmp(&b.0));
                self.duck_struct_typed_fields
                    .entry(struct_name.clone())
                    .or_insert_with(|| sorted_typed);
                format!("{struct_name}_FromJson({val})")
            }
            TypeDescription::Tag(name) => {
                let name = name.clone();
                self.used_tags.insert(name.clone());
                format!(
                    "func() (*__DuckTag_{name}, error) {{ return &__DuckTag_{name}{{}}, nil }}()"
                )
            }
            TypeDescription::Or(variants) | TypeDescription::And(variants) => {
                let variants_cloned: Vec<TypeExpr<Typed>> = variants.iter().cloned().collect();
                self.imports.insert("errors".into());
                let mut body = String::new();
                for (i, variant) in variants_cloned.iter().enumerate() {
                    match &variant.desc {
                        TypeDescription::Tag(tag_name) => {
                            let tag_name = tag_name.clone();
                            self.used_tags.insert(tag_name.clone());
                            body.push_str(&format!(
                                "if _sv{i}, _se{i} := string_FromJson({val}); _se{i} == nil && _sv{i} == \"{tag_name}\" {{ return &__DuckTag_{tag_name}{{}}, nil }}; "
                            ));
                        }
                        TypeDescription::Tuple(elems) if elems.is_empty() => {
                            // empty tuple is any - try last so other variants match first
                            body.push_str(&format!("{{ _ = {val}; return nil, nil }}; "));
                        }
                        _ => {
                            let inner = self.field_from_json_code(&variant.desc, val);
                            body.push_str(&format!(
                                "if _v{i}, _e{i} := ({inner}); _e{i} == nil {{ return _v{i}, nil }}; "
                            ));
                        }
                    }
                }
                body.push_str("return nil, errors.New(\"no variant matched\")");
                format!("func() (any, error) {{ {body} }}()")
            }
            _ => {
                self.imports.insert("errors".into());
                format!("func() (any, error) {{ return nil, errors.New(\"unsupported type\") }}()")
            }
        }
    }
}

/// Concatenate a list of Go string expressions with `+`.
fn go_str_concat(parts: Vec<GoExpr>) -> GoExpr {
    parts
        .into_iter()
        .reduce(|a, b| GoExpr::BinOp {
            op: GoBinOp::Add,
            lhs: Box::new(a),
            rhs: Box::new(b),
        })
        .unwrap_or(GoExpr::Str(String::new()))
}

/// Build the opening HTML tag string plus any dynamic attribute expressions.
/// Returns (static_prefix, dynamic_parts) where dynamic_parts are runtime values
/// interleaved between static string fragments.
fn build_html_open_tag(
    tag: &str,
    attrs: &[JsxAttr<Typed>],
    lowerer: &mut Lowerer,
    out: &mut Vec<GoStmt>,
) -> (String, Vec<GoExpr>) {
    let mut static_buf = format!("<{tag}");
    let mut dynamic: Vec<GoExpr> = Vec::new();

    for attr in attrs {
        match &attr.value {
            JsxAttrValue::Bool => {
                static_buf.push(' ');
                static_buf.push_str(&attr.name);
            }
            JsxAttrValue::Str(s) => {
                static_buf.push(' ');
                static_buf.push_str(&attr.name);
                static_buf.push_str("=\"");
                static_buf.push_str(&s.replace('"', "&quot;"));
                static_buf.push('"');
            }
            JsxAttrValue::Expr(e) => {
                // Close current static fragment, emit dynamic value, continue
                static_buf.push(' ');
                static_buf.push_str(&attr.name);
                static_buf.push_str("=\"");
                let flushed = std::mem::replace(&mut static_buf, String::new());
                dynamic.push(GoExpr::Str(flushed));
                let val = lowerer.lower_as_value(*e.clone(), out);
                dynamic.push(val);
                static_buf.push('"');
            }
        }
    }

    static_buf.push('>');
    (static_buf, dynamic)
}

/// Build the `props` JSON argument for a `DuckIsland_<Name>` call.
/// Literal attrs -> compile-time JSON string.
/// Expression attrs -> runtime string concatenation.
fn build_props_json(
    _tag: &str,
    attrs: Vec<JsxAttr<Typed>>,
    lowerer: &mut Lowerer,
    out: &mut Vec<GoStmt>,
) -> GoExpr {
    if attrs.is_empty() {
        return GoExpr::Str("{}".to_string());
    }

    // Build a list of GoExpr parts that together form the JSON object string.
    let mut parts: Vec<GoExpr> = vec![GoExpr::Str("{".to_string())];
    for (i, attr) in attrs.iter().enumerate() {
        if i > 0 {
            parts.push(GoExpr::Str(",".to_string()));
        }
        let key_fragment = format!("\"{}\":", attr.name);
        match &attr.value {
            JsxAttrValue::Bool => {
                parts.push(GoExpr::Str(format!("{key_fragment}true")));
            }
            JsxAttrValue::Str(s) => {
                let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
                parts.push(GoExpr::Str(format!("{key_fragment}\"{escaped}\"")));
            }
            JsxAttrValue::Expr(e) => {
                parts.push(GoExpr::Str(key_fragment));
                // Use fmt.Sprintf to serialize the value. Strings need %q, others %v.
                let val = lowerer.lower_as_value(*e.clone(), out);
                let is_string = matches!(e.ty.desc, TypeDescription::String(_));
                let fmt_verb = if is_string { "%q" } else { "%v" };
                parts.push(GoExpr::Call {
                    callee: Box::new(GoExpr::Raw("fmt.Sprintf".to_string())),
                    args: vec![GoExpr::Str(fmt_verb.to_string()), val],
                });
            }
        }
    }
    parts.push(GoExpr::Str("}".to_string()));
    go_str_concat(parts)
}

/// BFS from non-module items outward: returns DefIds of module items that are reachable.
fn collect_used_module_defs(
    items: &[Item<Typed>],
    module_def_ids: &HashSet<DefId>,
    global_scope: &HashMap<String, DefId>,
) -> HashSet<DefId> {
    // build lookup: DefId -> item reference for module items only
    let module_items: HashMap<DefId, &Item<Typed>> = items
        .iter()
        .filter_map(|item| {
            let name = item_top_name(item)?;
            let id = global_scope.get(name)?;
            if module_def_ids.contains(id) {
                Some((*id, item))
            } else {
                None
            }
        })
        .collect();

    let mut used: HashSet<DefId> = HashSet::new();
    let mut queue: Vec<DefId> = Vec::new();

    // seed from user items; extensions have no name so always emit - seed their deps too
    for item in items {
        let name = match item_top_name(item) {
            Some(n) => n,
            None => {
                if matches!(item, Item::Extension(_)) {
                    collect_idents_in_item(item, &mut queue);
                }
                continue;
            }
        };
        let id = match global_scope.get(name) {
            Some(&id) => id,
            None => continue,
        };
        if !module_def_ids.contains(&id) {
            collect_idents_in_item(item, &mut queue);
        }
    }

    // BFS
    while let Some(id) = queue.pop() {
        if !used.insert(id) {
            continue;
        }
        if let Some(&item) = module_items.get(&id) {
            collect_idents_in_item(item, &mut queue);
        }
    }

    used
}

fn item_top_name(item: &Item<Typed>) -> Option<&str> {
    match item {
        Item::Function(f) => Some(&f.name.value),
        Item::Struct(s) => Some(&s.name.value),
        Item::TypeAlias(a) => Some(&a.name.value),
        _ => None,
    }
}

fn collect_idents_in_item(item: &Item<Typed>, out: &mut Vec<DefId>) {
    match item {
        Item::Function(f) => collect_idents_in_expr(&f.body, out),
        Item::Extension(e) => {
            for m in &e.methods {
                collect_idents_in_expr(&m.body, out);
            }
        }
        _ => {}
    }
}

fn collect_idents_in_expr(expr: &Expr<Typed>, out: &mut Vec<DefId>) {
    match &expr.kind {
        ExprKind::Ident(id) => out.push(*id),
        ExprKind::Block(stmts) => stmts.iter().for_each(|s| collect_idents_in_expr(s, out)),
        ExprKind::Let { value, .. }
        | ExprKind::Const { value, .. }
        | ExprKind::Return(Some(value))
        | ExprKind::Not(value)
        | ExprKind::Neg(value)
        | ExprKind::Ref(value)
        | ExprKind::RefMut(value)
        | ExprKind::Deref(value)
        | ExprKind::BitNot(value)
        | ExprKind::Field { base: value, .. }
        | ExprKind::ScopeRes { base: value, .. }
        | ExprKind::Async(value)
        | ExprKind::Defer(value) => collect_idents_in_expr(value, out),
        ExprKind::Return(None) | ExprKind::Break | ExprKind::Continue => {}
        ExprKind::Assign { target, value }
        | ExprKind::AddAssign { target, value }
        | ExprKind::SubAssign { target, value }
        | ExprKind::MulAssign { target, value }
        | ExprKind::DivAssign { target, value }
        | ExprKind::ModAssign { target, value }
        | ExprKind::ShrAssign { target, value }
        | ExprKind::ShlAssign { target, value }
        | ExprKind::Add(target, value)
        | ExprKind::Sub(target, value)
        | ExprKind::Mul(target, value)
        | ExprKind::Div(target, value)
        | ExprKind::Mod(target, value)
        | ExprKind::BitAnd(target, value)
        | ExprKind::BitOr(target, value)
        | ExprKind::BitXor(target, value)
        | ExprKind::Shl(target, value)
        | ExprKind::Shr(target, value)
        | ExprKind::Eq(target, value)
        | ExprKind::Neq(target, value)
        | ExprKind::Lt(target, value)
        | ExprKind::Lte(target, value)
        | ExprKind::Gt(target, value)
        | ExprKind::Gte(target, value)
        | ExprKind::And(target, value)
        | ExprKind::Or(target, value)
        | ExprKind::Index {
            base: target,
            index: value,
        } => {
            collect_idents_in_expr(target, out);
            collect_idents_in_expr(value, out);
        }
        ExprKind::Call { callee, args, .. } => {
            collect_idents_in_expr(callee, out);
            args.iter().for_each(|a| collect_idents_in_expr(a, out));
        }
        ExprKind::As { value, .. } => collect_idents_in_expr(value, out),
        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_idents_in_expr(condition, out);
            collect_idents_in_expr(then_branch, out);
            if let Some(e) = else_branch {
                collect_idents_in_expr(e, out);
            }
        }
        ExprKind::While { condition, body } => {
            collect_idents_in_expr(condition, out);
            collect_idents_in_expr(body, out);
        }
        ExprKind::For { iterable, body, .. } => {
            collect_idents_in_expr(iterable, out);
            collect_idents_in_expr(body, out);
        }
        ExprKind::Match {
            value,
            arms,
            else_arm,
        } => {
            collect_idents_in_expr(value, out);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    collect_idents_in_expr(g, out);
                }
                collect_idents_in_expr(&arm.body, out);
            }
            if let Some(e) = else_arm {
                collect_idents_in_expr(e, out);
            }
        }
        ExprKind::StructLit { fields, .. } | ExprKind::DuckLit(fields) => {
            fields
                .iter()
                .for_each(|(_, v)| collect_idents_in_expr(v, out));
        }
        ExprKind::Array(elems) | ExprKind::Tuple(elems) => {
            elems.iter().for_each(|e| collect_idents_in_expr(e, out));
        }
        ExprKind::Lambda { body, .. } => collect_idents_in_expr(body, out),
        ExprKind::FmtString(parts) => {
            for p in parts {
                if let FmtPart::Expr(e) = p {
                    collect_idents_in_expr(e, out);
                }
            }
        }
        ExprKind::Jsx(node) => collect_idents_in_jsx(node, out),
        ExprKind::LetTuple { value, .. } => collect_idents_in_expr(value, out),
        _ => {}
    }
}

fn collect_idents_in_jsx(node: &JsxNode<Typed>, out: &mut Vec<DefId>) {
    match node {
        JsxNode::Text(_) => {}
        JsxNode::Expr(e) => collect_idents_in_expr(e, out),
        JsxNode::Element {
            attrs, children, ..
        } => {
            for attr in attrs {
                if let JsxAttrValue::Expr(e) = &attr.value {
                    collect_idents_in_expr(e, out);
                }
            }
            children.iter().for_each(|c| collect_idents_in_jsx(c, out));
        }
    }
}

pub fn lower(out: InferOutput, package: &str) -> GoFile {
    let used_module_defs = if out.module_def_ids.is_empty() {
        HashSet::new()
    } else {
        collect_used_module_defs(
            &out.source_file.items,
            &out.module_def_ids,
            &out.global_scope,
        )
    };

    let needs_json_utils = out.source_file.items.iter().any(|item| match item {
        Item::Struct(s) => s
            .derived
            .contains(&crate::parse::struct_parser::DerivableInterface::FromJson),
        // parse_json<T> specializations have mangled names starting with "parse_json__"
        Item::Function(f) => f.name.value.starts_with("parse_json__"),
        _ => false,
    });

    let mut l = Lowerer::new(&out.symbols);

    // build type alias expansion map before consuming items
    let mut type_aliases_for_from_json: Vec<(String, TypeExpr<Typed>)> = Vec::new();
    for item in &out.source_file.items {
        if let Item::TypeAlias(a) = item {
            if let Some(&id) = out.global_scope.get(a.name.value.as_str()) {
                l.type_alias_expansions.insert(id, a.type_expr.desc.clone());
                if !a.generics.is_empty() {
                    let param_names: Vec<String> = a
                        .generics
                        .iter()
                        .map(|g| g.value.name.value.clone())
                        .collect();
                    l.type_alias_param_names.insert(id, param_names);
                }
            }
            type_aliases_for_from_json.push((escape(&a.name.value), a.type_expr.clone()));
        }
    }
    // generic aliases are filtered from source_file.items by monomorphize - use the dedicated map
    for (&id, a) in &out.generic_type_aliases {
        if !a.generics.is_empty() {
            l.type_alias_expansions.insert(id, a.type_expr.desc.clone());
            let param_names: Vec<String> = a
                .generics
                .iter()
                .map(|g| g.value.name.value.clone())
                .collect();
            l.type_alias_param_names.insert(id, param_names);
        }
    }

    let mut decls: Vec<GoDecl> = Vec::new();
    let mut go_imports: Vec<String> = Vec::new();
    let mut client_fn_names: Vec<String> = Vec::new();

    let mut field_names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for item in &out.source_file.items {
        if let Item::Struct(s) = item {
            // skip unreachable module structs
            if let Some(&id) = out.global_scope.get(s.name.value.as_str()) {
                if out.module_def_ids.contains(&id) && !used_module_defs.contains(&id) {
                    continue;
                }
            }
            for f in &s.fields {
                field_names.insert(capitalize(&f.name.value));
            }
        }
    }

    for cap in &field_names {
        let t = GoType::Named("T".into());
        decls.push(GoDecl::Interface {
            name: format!("Has{cap}"),
            methods: vec![
                GoInterfaceMethod {
                    name: format!("Get{cap}"),
                    params: vec![],
                    ret: Some(t.clone()),
                },
                GoInterfaceMethod {
                    name: format!("GetPtr{cap}"),
                    params: vec![],
                    ret: Some(GoType::Ptr(Box::new(t.clone()))),
                },
                GoInterfaceMethod {
                    name: format!("Set{cap}"),
                    params: vec![GoParam {
                        name: "param".into(),
                        ty: t,
                    }],
                    ret: None,
                },
            ],
        });
    }

    for item in out.source_file.items {
        // skip module items that are not reachable from user code
        if let Some(name) = item_top_name(&item) {
            if let Some(&id) = out.global_scope.get(name) {
                if out.module_def_ids.contains(&id) && !used_module_defs.contains(&id) {
                    continue;
                }
            }
        }

        match item {
            Item::Function(f) => {
                // pre-register duck structs from parse_json return types for _FromJson gen
                if let Some(ret_ty) = &f.return_type {
                    if let TypeDescription::Or(variants) = &ret_ty.desc {
                        for v in variants.iter() {
                            if matches!(&v.desc, TypeDescription::Duck(_)) {
                                let _ = l.field_from_json_code(&v.desc, "_ph");
                            }
                        }
                    }
                }
                if f.is_client {
                    client_fn_names.push(f.name.value.clone());
                } else {
                    decls.push(l.lower_function(f, None));
                }
            }
            Item::Struct(s) => decls.extend(l.lower_struct(s)),
            Item::TypeAlias(a) => decls.push(l.lower_type_alias(a)),
            Item::Extension(e) => {
                // If the target is a module-origin struct that was DCE'd, its type definition
                // won't be in the output — skip to avoid generating methods on undefined types.
                let skip = if let TypeDescription::TypeName { ref type_ref, .. } = e.target.desc {
                    out.module_def_ids.contains(type_ref) && !used_module_defs.contains(type_ref)
                } else {
                    false
                };
                if !skip {
                    decls.extend(l.lower_extension(e));
                }
            }
            Item::Use(UseDecl::Go(path, _)) => {
                // path[0] is always the full Go import path; path[1] (optional) is the alias
                let import = path[0].value.clone();
                if !go_imports.contains(&import) {
                    go_imports.push(import);
                }
            }
            Item::Use(UseDecl::Duck(_, _, _)) | Item::Use(UseDecl::Ts(_, _)) => {}
        }
    }

    // Emit HasField interfaces for fields introduced only by duck literals.
    for cap in &l.new_duck_field_names {
        if !field_names.contains(cap) {
            let t = GoType::Named("T".into());
            decls.push(GoDecl::Interface {
                name: format!("Has{cap}"),
                methods: vec![
                    GoInterfaceMethod {
                        name: format!("Get{cap}"),
                        params: vec![],
                        ret: Some(t.clone()),
                    },
                    GoInterfaceMethod {
                        name: format!("GetPtr{cap}"),
                        params: vec![],
                        ret: Some(GoType::Ptr(Box::new(t.clone()))),
                    },
                    GoInterfaceMethod {
                        name: format!("Set{cap}"),
                        params: vec![GoParam {
                            name: "param".into(),
                            ty: t,
                        }],
                        ret: None,
                    },
                ],
            });
            field_names.insert(cap.clone());
        }
    }

    // emit duck structs - loop iterates because _FromJson may register more on-demand
    let mut seen_duck_structs: HashSet<String> = HashSet::new();
    let mut duck_structs: Vec<(String, Vec<GoField>)> = l.duck_structs.drain().collect();
    for (name, _) in &duck_structs {
        seen_duck_structs.insert(name.clone());
    }
    let had_duck_structs = !duck_structs.is_empty();
    let mut duck_typed: HashMap<String, Vec<(String, TypeExpr<Typed>)>> =
        l.duck_struct_typed_fields.drain().collect();
    let mut duck_struct_i = 0;
    while duck_struct_i < duck_structs.len() {
        let (struct_name, go_fields) = duck_structs[duck_struct_i].clone();
        duck_struct_i += 1;
        decls.push(GoDecl::Struct {
            name: struct_name.clone(),
            fields: go_fields.clone(),
        });
        let recv_ty = GoType::Ptr(Box::new(GoType::Named(struct_name.clone())));
        for go_field in &go_fields {
            let cap = &go_field.name;
            let recv = GoParam {
                name: "self".into(),
                ty: recv_ty.clone(),
            };
            let self_field = GoExpr::Field {
                base: Box::new(GoExpr::Ident("self".into())),
                field: cap.clone(),
            };
            decls.push(GoDecl::Func {
                name: format!("Get{cap}"),
                receiver: Some(recv.clone()),
                params: vec![],
                ret: Some(go_field.ty.clone()),
                body: vec![GoStmt::Return(Some(self_field.clone()))],
            });
            decls.push(GoDecl::Func {
                name: format!("GetPtr{cap}"),
                receiver: Some(recv.clone()),
                params: vec![],
                ret: Some(GoType::Ptr(Box::new(go_field.ty.clone()))),
                body: vec![GoStmt::Return(Some(GoExpr::Ref(Box::new(
                    self_field.clone(),
                ))))],
            });
            decls.push(GoDecl::Func {
                name: format!("Set{cap}"),
                receiver: Some(recv.clone()),
                params: vec![GoParam {
                    name: "param".into(),
                    ty: go_field.ty.clone(),
                }],
                ret: None,
                body: vec![GoStmt::Assign {
                    target: self_field,
                    value: GoExpr::Ident("param".into()),
                }],
            });
        }
        if let Some(typed_fields) = duck_typed.get(&struct_name).cloned() {
            l.imports.insert("fmt".into());
            let mut json_parts: Vec<String> = Vec::new();
            let mut str_parts: Vec<String> = Vec::new();
            for (field_name, field_ty) in &typed_fields {
                let val = format!("self.{field_name}");
                // field_name is capitalized, use lowercase for JSON/display keys
                let duck_key: String = field_name
                    .chars()
                    .enumerate()
                    .map(|(i, c)| {
                        if i == 0 {
                            c.to_lowercase().next().unwrap_or(c)
                        } else {
                            c
                        }
                    })
                    .collect();
                let json_expr = l.gen_type_to_json_code(&field_ty.desc, &val, 0);
                json_parts.push(format!("\"\\\"{}\\\": \"+({json_expr})", duck_key));
                let str_expr = l.gen_type_to_string_code(&field_ty.desc, &val, 0);
                str_parts.push(format!("\"{}: \"+({str_expr})", duck_key));
            }
            let json_body = if json_parts.is_empty() {
                "return \"{}\"".to_string()
            } else {
                format!(
                    "return fmt.Sprintf(\"{{%s}}\", {})",
                    json_parts.join("+\",\"+")
                )
            };
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_json() string {{ {json_body} }}"
            )));
            let str_inner = str_parts.join("+\",\"+");
            let str_body = format!("return fmt.Sprintf(\"{{%s}}\", {str_inner})");
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_string() string {{ {str_body} }}"
            )));
            l.imports.insert("errors".into());
            let mut fj_code = format!(
                "var _res *{struct_name}\n_obj, _, _err := scan_json_struct_parts(_json_str)\nif _err != nil {{ return _res, _err }}\n_ = _obj\n"
            );
            let mut fj_inits: Vec<String> = Vec::new();
            for (field_name, field_ty) in &typed_fields {
                let lower_field_name = field_name.to_lowercase();
                let call = l
                    .field_from_json_code(&field_ty.desc, &format!("_obj[\"{lower_field_name}\"]"));
                fj_code.push_str(&format!(
                    "_f_{field_name}, _fj_err := {call}\n_ = _f_{field_name}\nif _fj_err != nil {{ return _res, _fj_err }}\n"
                ));
                fj_inits.push(format!("{field_name}: _f_{field_name}"));
            }
            let inits = fj_inits.join(", ");
            fj_code.push_str(&format!("return &{struct_name}{{{inits}}}, nil"));
            decls.push(GoDecl::Raw(format!(
                "func {struct_name}_FromJson(_json_str string) (*{struct_name}, error) {{ {fj_code} }}"
            )));
        }
        // drain newly registered on-demand structs, skip already-seen
        for (name, fields) in l.duck_structs.drain() {
            if seen_duck_structs.insert(name.clone()) {
                duck_structs.push((name, fields));
            }
        }
        for (k, v) in l.duck_struct_typed_fields.drain() {
            duck_typed.entry(k).or_insert(v);
        }
    }
    // emit HasField interfaces for field names from on-demand structs
    for cap in &l.new_duck_field_names {
        if !field_names.contains(cap) {
            let t = GoType::Named("T".into());
            decls.push(GoDecl::Interface {
                name: format!("Has{cap}"),
                methods: vec![
                    GoInterfaceMethod {
                        name: format!("Get{cap}"),
                        params: vec![],
                        ret: Some(t.clone()),
                    },
                    GoInterfaceMethod {
                        name: format!("GetPtr{cap}"),
                        params: vec![],
                        ret: Some(GoType::Ptr(Box::new(t.clone()))),
                    },
                    GoInterfaceMethod {
                        name: format!("Set{cap}"),
                        params: vec![GoParam {
                            name: "param".into(),
                            ty: t,
                        }],
                        ret: None,
                    },
                ],
            });
            field_names.insert(cap.clone());
        }
    }

    // Generate _FromJson for type aliases that expand to duck or And types.
    {
        for (alias_name, alias_type) in type_aliases_for_from_json {
            match alias_type.desc.clone() {
                TypeDescription::Duck(duck) => {
                    let mut name_tys: Vec<(String, GoType)> = duck
                        .fields
                        .iter()
                        .map(|f| (capitalize(&f.name.value), l.lower_type(&f.type_expr)))
                        .collect();
                    name_tys.sort_by(|a, b| a.0.cmp(&b.0));
                    let struct_name = duck_struct_name(&name_tys);
                    decls.push(GoDecl::Raw(format!(
                        "func {alias_name}_FromJson(_json_str string) ({alias_name}, error) {{ return {struct_name}_FromJson(_json_str) }}"
                    )));
                }
                TypeDescription::And(variants) => {
                    let mut merged: std::collections::BTreeMap<String, GoType> =
                        std::collections::BTreeMap::new();
                    let mut all_duck = true;
                    for v in &variants {
                        let duck_fields: Option<Vec<_>> = match &v.desc {
                            TypeDescription::Duck(duck) => {
                                Some(duck.fields.iter().cloned().collect())
                            }
                            TypeDescription::TypeName {
                                type_ref,
                                type_params,
                            } if type_params.is_empty() => {
                                if let Some(TypeDescription::Duck(duck)) =
                                    l.type_alias_expansions.get(type_ref).cloned()
                                {
                                    Some(duck.fields.iter().cloned().collect())
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        };
                        if let Some(fields) = duck_fields {
                            for f in &fields {
                                let cap = capitalize(&f.name.value);
                                let go_ty = match l.lower_type(&f.type_expr) {
                                    GoType::DuckInterface(_) => GoType::Any,
                                    other => other,
                                };
                                merged.insert(cap, go_ty);
                            }
                        } else {
                            all_duck = false;
                            break;
                        }
                    }
                    if all_duck && !merged.is_empty() {
                        let sorted: Vec<(String, GoType)> = merged.into_iter().collect();
                        let struct_name = duck_struct_name(&sorted);
                        decls.push(GoDecl::Raw(format!(
                            "func {alias_name}_FromJson(_json_str string) ({alias_name}, error) {{ return {struct_name}_FromJson(_json_str) }}"
                        )));
                    }
                }
                _ => {}
            }
        }
    }

    // Emit tuple struct types (one per unique element-type signature).
    let tuple_structs: Vec<(String, Vec<GoField>)> = l.tuple_structs.drain().collect();
    let had_tuple_structs = !tuple_structs.is_empty();
    let tuple_typed: HashMap<String, Vec<TypeExpr<Typed>>> =
        l.tuple_struct_typed_elems.drain().collect();
    let mut needs_any_dispatch_helpers = false;
    for (struct_name, go_fields) in tuple_structs {
        decls.push(GoDecl::Struct {
            name: struct_name.clone(),
            fields: go_fields,
        });
        if let Some(typed_elems) = tuple_typed.get(&struct_name) {
            l.imports.insert("fmt".into());
            let mut json_parts: Vec<String> = Vec::new();
            let mut str_parts: Vec<String> = Vec::new();
            let mut needs_any_helpers_local = false;
            for (i, elem_ty) in typed_elems.iter().enumerate() {
                let val = format!("self.field_{i}");
                let is_any_field =
                    matches!(&elem_ty.desc, TypeDescription::Tuple(e) if e.is_empty());
                let json_expr = if is_any_field {
                    needs_any_helpers_local = true;
                    format!("__any_to_json({val})")
                } else {
                    l.gen_type_to_json_code(&elem_ty.desc, &val, 0)
                };
                json_parts.push(json_expr);
                let str_expr = if is_any_field {
                    format!("__any_to_str({val})")
                } else {
                    l.gen_type_to_string_code(&elem_ty.desc, &val, 0)
                };
                str_parts.push(str_expr);
            }
            if needs_any_helpers_local {
                needs_any_dispatch_helpers = true;
            }
            let json_body = format!("return \"[\" + {} + \"]\"", json_parts.join("+\", \"+"));
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_json() string {{ {json_body} }}"
            )));
            let str_body = format!("return \"(\" + {} + \")\"", str_parts.join("+\", \"+"));
            decls.push(GoDecl::Raw(format!(
                "func (self *{struct_name}) to_string() string {{ {str_body} }}"
            )));
            l.imports.insert("errors".into());
            let n = typed_elems.len();
            let elems_for_fj: Vec<TypeExpr<Typed>> = typed_elems.clone();
            let mut fj_code = format!(
                "var _res *{struct_name}\n_parts, _, _err := scan_json_array_parts(_json_str)\nif _err != nil {{ return _res, _err }}\nif len(_parts) != {n} {{ return _res, errors.New(\"tuple length mismatch\") }}\n"
            );
            let mut fj_inits: Vec<String> = Vec::new();
            for (i, elem_ty) in elems_for_fj.iter().enumerate() {
                let call = l.field_from_json_code(&elem_ty.desc, &format!("_parts[{i}]"));
                fj_code.push_str(&format!(
                    "_tf{i}, _fj_err{i} := {call}\nif _fj_err{i} != nil {{ return _res, _fj_err{i} }}\n"
                ));
                let needs_addr = matches!(
                    &elem_ty.desc,
                    TypeDescription::Ref(_) | TypeDescription::RefMut(_)
                );
                let init_val = if needs_addr {
                    format!("&_tf{i}")
                } else {
                    format!("_tf{i}")
                };
                fj_inits.push(format!("field_{i}: {init_val}"));
            }
            let inits = fj_inits.join(", ");
            fj_code.push_str(&format!("return &{struct_name}{{{inits}}}, nil"));
            decls.push(GoDecl::Raw(format!(
                "func {struct_name}_FromJson(_json_str string) (*{struct_name}, error) {{ {fj_code} }}"
            )));
        }
    }

    let mut imports: Vec<String> = go_imports;
    for imp in l.imports {
        if !imports.contains(&imp) {
            imports.push(imp);
        }
    }

    // Emit island companion functions and bundle handler for client fns.
    if !client_fn_names.is_empty() {
        for fn_name in &client_fn_names {
            let raw = format!(
                "func DuckIsland_{fn_name}(props string) string {{\n\treturn \"<div data-duck-island=\\\"{fn_name}\\\" data-duck-props='\" + props + \"'></div>\"\n}}"
            );
            decls.push(GoDecl::Raw(raw));
        }

        decls.push(GoDecl::Raw(
            "func DuckServeClientBundle() http.Handler {\n\
             \treturn http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {\n\
             \t\tdata, err := os.ReadFile(\".dargo/client.js\")\n\
             \t\tif err != nil {\n\
             \t\t\thttp.Error(w, \"bundle not found\", 404)\n\
             \t\t\treturn\n\
             \t\t}\n\
             \t\tw.Header().Set(\"Content-Type\", \"application/javascript\")\n\
             \t\tw.Write(data)\n\
             \t})\n\
             }"
            .to_string(),
        ));

        for pkg in &["net/http", "os"] {
            if !imports.contains(&pkg.to_string()) {
                imports.push(pkg.to_string());
            }
        }
    }

    let needs_json_utils = needs_json_utils || had_tuple_structs || had_duck_structs;

    if needs_json_utils {
        for pkg in &["errors", "encoding/json", "unicode/utf8", "strings"] {
            if !imports.contains(&pkg.to_string()) {
                imports.push(pkg.to_string());
            }
        }
        decls.push(GoDecl::Raw(JSON_SCAN_UTILS.to_string()));
        decls.push(GoDecl::Raw(PRIMITIVE_FROM_JSON.to_string()));
    }
    if needs_any_dispatch_helpers {
        for pkg in &["fmt", "strconv"] {
            if !imports.contains(&pkg.to_string()) {
                imports.push(pkg.to_string());
            }
        }
        decls.push(GoDecl::Raw(ANY_HELPERS.to_string()));
    }

    imports.sort();

    let mut tag_decls: Vec<GoDecl> = l
        .used_tags
        .iter()
        .map(|name| GoDecl::Struct {
            name: format!("__DuckTag_{name}"),
            fields: vec![],
        })
        .collect();
    tag_decls.extend(decls);
    let decls = tag_decls;

    GoFile {
        package: package.to_string(),
        imports,
        decls,
    }
}
