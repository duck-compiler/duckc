use std::collections::{HashMap, HashSet};

use crate::parser2::parser::{
    DefId, DefKind, Expr, ExprKind, ExtensionDecl, FmtPart, FunctionDecl, Item, JsxAttr,
    JsxAttrValue, JsxNode, MatchArm, StructDecl, SymbolTable, TypeAliasDecl, TypeDescription,
    TypeExpr, Typed, UseDecl,
};
use crate::semantics2::type_infer::InferOutput;

use super::go_ir::*;

// Go keywords these are prefixed to avoid identifier conflicts
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
        GoType::DuckInterface(_) => "Duck".into(),
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
        }
    }

    fn fresh(&mut self) -> String {
        let n = self.tmp;

        self.tmp += 1;

        format!("_t{n}")
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

    fn lower_type(&self, te: &TypeExpr<Typed>) -> GoType {
        self.lower_type_desc(&te.desc)
    }

    fn lower_type_for_switch_arm(&mut self, te: &TypeExpr<Typed>) -> GoType {
        // tag patterns use *__DuckTag_X so type switches never collide with plain `string`
        if let TypeDescription::Tag(name) = &te.desc {
            self.used_tags.insert(name.clone());
            return GoType::Ptr(Box::new(GoType::Named(format!("__DuckTag_{name}"))));
        }
        let go_ty = self.lower_type(te);
        match go_ty {
            GoType::Named(name) => GoType::Ptr(Box::new(GoType::Named(name))),
            other => other,
        }
    }

    fn lower_type_desc(&self, desc: &TypeDescription<Typed>) -> GoType {
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
                    // bare GenericParam TypeName becomes `any`
                    if matches!(self.symbols.get(*type_ref).kind, DefKind::GenericParam) {
                        GoType::Any
                    } else {
                        GoType::Named(self.name(*type_ref))
                    }
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
                        (
                            has_interface_name(&f.name.value),
                            self.lower_type(&f.type_expr),
                        )
                    })
                    .collect();
                constraints.sort_by(|a, b| a.0.cmp(&b.0));
                GoType::DuckInterface(constraints)
            }
            TypeDescription::Tuple(elems) => {
                if elems.is_empty() {
                    return GoType::Any;
                }
                let elem_tys: Vec<GoType> = elems.iter().map(|e| self.lower_type(e)).collect();
                GoType::Ptr(Box::new(GoType::Named(tuple_struct_name(&elem_tys))))
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
                );
                // Extension methods are emitted with their original lowercase Go name.
                let is_method = matches!(&span_ty.desc, TypeDescription::Fun { .. });
                let go_base = self.lower_as_value(*base, out);
                if is_duck {
                    GoExpr::Call {
                        callee: Box::new(GoExpr::Field {
                            base: Box::new(go_base),
                            field: format!("Get{}", capitalize(&field_name)),
                        }),
                        args: vec![],
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
                            TypeDescription::Array(_) => m == "len",
                            TypeDescription::Int
                            | TypeDescription::UInt
                            | TypeDescription::Float
                            | TypeDescription::Bool(_)
                            | TypeDescription::String(_)
                            | TypeDescription::Char
                            | TypeDescription::Byte => matches!(m, "to_string" | "to_json"),
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
                let lowered_callee = self.lower_as_value(*callee, out);
                let args = args
                    .into_iter()
                    .map(|a| self.lower_as_value(a, out))
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
                let lowered: Vec<(GoExpr, GoType)> = elems
                    .into_iter()
                    .map(|e| {
                        let go_ty = self.lower_type(&e.ty);
                        let go_val = self.lower_as_value(e, out);
                        (go_val, go_ty)
                    })
                    .collect();
                let elem_tys: Vec<GoType> = lowered.iter().map(|(_, t)| t.clone()).collect();
                let struct_name = tuple_struct_name(&elem_tys);
                self.tuple_structs
                    .entry(struct_name.clone())
                    .or_insert_with(|| {
                        lowered
                            .iter()
                            .enumerate()
                            .map(|(i, (_, t))| GoField {
                                name: format!("field_{i}"),
                                ty: t.clone(),
                            })
                            .collect()
                    });
                let fields: Vec<(String, GoExpr)> = lowered
                    .into_iter()
                    .enumerate()
                    .map(|(i, (v, _))| (format!("field_{i}"), v))
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
                let mut lowered: Vec<(String, GoExpr, GoType)> = fields
                    .into_iter()
                    .map(|(label, val)| {
                        let go_ty = self.lower_type(&val.ty);
                        let cap = capitalize(&label.value);
                        let go_val = self.lower_as_value(val, out);
                        (cap, go_val, go_ty)
                    })
                    .collect();
                // Sort alphabetically so the struct name is canonical.
                lowered.sort_by(|a, b| a.0.cmp(&b.0));

                let name_fields: Vec<(String, GoType)> = lowered
                    .iter()
                    .map(|(n, _, t)| (n.clone(), t.clone()))
                    .collect();
                let struct_name = duck_struct_name(&name_fields);

                // Register the duck struct (once per unique shape).
                self.duck_structs
                    .entry(struct_name.clone())
                    .or_insert_with(|| {
                        let go_fields: Vec<GoField> = lowered
                            .iter()
                            .map(|(n, _, t)| GoField {
                                name: n.clone(),
                                ty: t.clone(),
                            })
                            .collect();
                        for (n, _) in &name_fields {
                            self.new_duck_field_names.insert(n.clone());
                        }
                        go_fields
                    });

                let struct_fields: Vec<(String, GoExpr)> =
                    lowered.into_iter().map(|(n, v, _)| (n, v)).collect();
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
                let go_val = self.lower_as_value(*value, out);
                let go_name = self
                    .claim_local(&name.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| escape(&name.value));

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
                let t = self.lower_as_value(*target, out);
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
            ExprKind::Return(_) => self.lower_as_stmts(body, &mut stmts),
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

        let receiver_ty = GoType::Ptr(Box::new(self.lower_type(&ext.target)));
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

    fn lower_struct(&self, s: StructDecl<Typed>) -> Vec<GoDecl> {
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

        decls
    }

    fn lower_type_alias(&self, a: TypeAliasDecl<Typed>) -> GoDecl {
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
                self.imports.insert("strconv".into());
                GoExpr::Call {
                    callee: Box::new(GoExpr::Field {
                        base: Box::new(GoExpr::Ident("strconv".into())),
                        field: "FormatFloat".into(),
                    }),
                    args: vec![val, GoExpr::Char('f'), GoExpr::Int(-1), GoExpr::Int(64)],
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
            _ => unreachable!("to_string on unsupported type"),
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
            _ => self.lower_to_string(ty, val),
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

    let mut l = Lowerer::new(&out.symbols);
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
                if f.is_client {
                    client_fn_names.push(f.name.value.clone());
                } else {
                    decls.push(l.lower_function(f, None));
                }
            }
            Item::Struct(s) => decls.extend(l.lower_struct(s)),
            Item::TypeAlias(a) => decls.push(l.lower_type_alias(a)),
            Item::Extension(e) => decls.extend(l.lower_extension(e)),
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
        }
    }

    // Emit anonymous duck literal struct types with getter/setter methods.
    let duck_structs: Vec<(String, Vec<GoField>)> = l.duck_structs.drain().collect();
    for (struct_name, go_fields) in duck_structs {
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
    }

    // Emit tuple struct types (one per unique element-type signature).
    let tuple_structs: Vec<(String, Vec<GoField>)> = l.tuple_structs.drain().collect();
    for (struct_name, go_fields) in tuple_structs {
        decls.push(GoDecl::Struct {
            name: struct_name,
            fields: go_fields,
        });
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
