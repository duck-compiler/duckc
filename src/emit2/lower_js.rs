use std::collections::{HashMap, HashSet};

use crate::parser2::parser::{
    DefId, DefKind, Expr, ExprKind, ExtensionDecl, FmtPart, FunctionDecl, Item, JsxAttrValue,
    JsxNode, MatchArm, StructDecl, SymbolTable, TypeDescription, TypeExpr, Typed,
};
use crate::semantics2::type_infer::InferOutput;

use super::js_ir::*;

fn js_ident(name: &str) -> String {
    // Reserve `$tag` for our own use; escape JS keywords.
    const JS_KEYWORDS: &[&str] = &[
        "break",
        "case",
        "catch",
        "class",
        "const",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "else",
        "export",
        "extends",
        "false",
        "finally",
        "for",
        "function",
        "if",
        "import",
        "in",
        "instanceof",
        "let",
        "new",
        "null",
        "of",
        "return",
        "static",
        "super",
        "switch",
        "this",
        "throw",
        "true",
        "try",
        "typeof",
        "undefined",
        "var",
        "void",
        "while",
        "with",
        "yield",
    ];
    if JS_KEYWORDS.contains(&name) {
        format!("_{name}")
    } else {
        name.to_string()
    }
}

struct Lowerer<'a> {
    symbols: &'a SymbolTable,
    names: HashMap<DefId, String>,
    local_queues: HashMap<String, Vec<DefId>>,
    used_locals: HashSet<DefId>,
    tmp: u32,
}

impl<'a> Lowerer<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        let mut names: HashMap<DefId, String> = HashMap::new();
        let mut local_queues: HashMap<String, Vec<DefId>> = HashMap::new();
        // Track how many times each base name has been assigned, to generate
        // unique names only when there is an actual collision (same raw name used
        // more than once in a single function/scope).
        let mut name_counts: HashMap<String, u32> = HashMap::new();

        for (id, def) in symbols.iter() {
            if matches!(def.kind, DefKind::Poison) {
                continue;
            }
            let base = js_ident(&def.name);
            let js_name = match &def.kind {
                DefKind::Local { .. } => {
                    local_queues.entry(def.name.clone()).or_default().push(id);
                    let count = name_counts.entry(base.clone()).or_insert(0);
                    // First use of this name: keep it clean (no suffix) so that
                    // JSX expressions like `{myVar}` work without knowing the
                    // mangled name. Only suffix on subsequent uses.
                    let js_name = if *count == 0 {
                        base
                    } else {
                        format!("{base}_{count}")
                    };
                    *count += 1;
                    js_name
                }
                _ => base,
            };
            names.insert(id, js_name);
        }

        Self {
            symbols,
            names,
            local_queues,
            used_locals: HashSet::new(),
            tmp: 0,
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

    /// Lower a typed expression to a `JsExpr`, emitting any needed preamble statements into
    /// `out`. The result is always a single JS expression (never multi-statement).
    fn lower_as_value(&mut self, expr: Expr<Typed>, out: &mut Vec<JsStmt>) -> JsExpr {
        let span_ty = expr.ty.clone();
        match expr.kind {
            ExprKind::Int(v) => JsExpr::Int(v as i64),
            ExprKind::Float(v) => JsExpr::Float(v),
            ExprKind::Bool(v) => JsExpr::Bool(v),
            ExprKind::Char(v) => JsExpr::Str(v.to_string()),
            ExprKind::String(v) => JsExpr::Str(v),
            ExprKind::Tag(v) => JsExpr::Str(format!(".{v}")),
            ExprKind::InlineGo(v) => JsExpr::Raw(v),
            ExprKind::Jsx(node) => self.lower_jsx_node(*node, out),

            ExprKind::Break => {
                out.push(JsStmt::Break);
                JsExpr::Undefined
            }
            ExprKind::Continue => {
                out.push(JsStmt::Continue);
                JsExpr::Undefined
            }

            ExprKind::FmtString(parts) => {
                let tpl_parts: Vec<JsTemplatePart> = parts
                    .into_iter()
                    .map(|p| match p {
                        FmtPart::Literal(s) => JsTemplatePart::Str(s),
                        FmtPart::Expr(e) => JsTemplatePart::Expr(self.lower_as_value(e, out)),
                    })
                    .collect();
                JsExpr::Template { parts: tpl_parts }
            }

            ExprKind::Ident(id) => JsExpr::Ident(self.name(id)),

            ExprKind::Block(stmts) => {
                let n = stmts.len();
                if n == 0 {
                    return JsExpr::Undefined;
                }
                for (i, s) in stmts.into_iter().enumerate() {
                    if i < n - 1 {
                        self.lower_as_stmts(s, out);
                    } else {
                        return self.lower_as_value(s, out);
                    }
                }
                JsExpr::Undefined
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond = self.lower_as_value(*condition, out);
                let tmp = self.fresh();
                out.push(JsStmt::Let {
                    name: tmp.clone(),
                    value: None,
                });

                let mut then_stmts = Vec::new();
                let then_val = self.lower_as_value(*then_branch, &mut then_stmts);
                then_stmts.push(JsStmt::Assign {
                    target: JsExpr::Ident(tmp.clone()),
                    value: then_val,
                });

                let else_stmts = else_branch.map(|e| {
                    let mut s = Vec::new();
                    let v = self.lower_as_value(*e, &mut s);
                    s.push(JsStmt::Assign {
                        target: JsExpr::Ident(tmp.clone()),
                        value: v,
                    });
                    s
                });

                out.push(JsStmt::If {
                    cond,
                    then: then_stmts,
                    else_: else_stmts,
                });
                JsExpr::Ident(tmp)
            }

            ExprKind::Add(a, b) => self.bin(JsBinOp::Add, *a, *b, out),
            ExprKind::Sub(a, b) => self.bin(JsBinOp::Sub, *a, *b, out),
            ExprKind::Mul(a, b) => self.bin(JsBinOp::Mul, *a, *b, out),
            ExprKind::Div(a, b) => self.bin(JsBinOp::Div, *a, *b, out),
            ExprKind::Mod(a, b) => self.bin(JsBinOp::Mod, *a, *b, out),
            ExprKind::BitAnd(a, b) => self.bin(JsBinOp::BitAnd, *a, *b, out),
            ExprKind::BitOr(a, b) => self.bin(JsBinOp::BitOr, *a, *b, out),
            ExprKind::BitXor(a, b) => self.bin(JsBinOp::BitXor, *a, *b, out),
            ExprKind::Shl(a, b) => self.bin(JsBinOp::Shl, *a, *b, out),
            ExprKind::Shr(a, b) => self.bin(JsBinOp::Shr, *a, *b, out),
            ExprKind::Eq(a, b) => self.bin(JsBinOp::StrictEq, *a, *b, out),
            ExprKind::Neq(a, b) => self.bin(JsBinOp::StrictNeq, *a, *b, out),
            ExprKind::Lt(a, b) => self.bin(JsBinOp::Lt, *a, *b, out),
            ExprKind::Lte(a, b) => self.bin(JsBinOp::Lte, *a, *b, out),
            ExprKind::Gt(a, b) => self.bin(JsBinOp::Gt, *a, *b, out),
            ExprKind::Gte(a, b) => self.bin(JsBinOp::Gte, *a, *b, out),
            ExprKind::And(a, b) => self.bin(JsBinOp::And, *a, *b, out),
            ExprKind::Or(a, b) => self.bin(JsBinOp::Or, *a, *b, out),
            ExprKind::Not(a) => JsExpr::UnaryOp {
                op: JsUnaryOp::Not,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            ExprKind::Neg(a) => JsExpr::UnaryOp {
                op: JsUnaryOp::Neg,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            ExprKind::BitNot(a) => JsExpr::UnaryOp {
                op: JsUnaryOp::BitNot,
                operand: Box::new(self.lower_as_value(*a, out)),
            },
            // References and derefs are no-ops in JS (objects are reference types).
            ExprKind::Ref(a) | ExprKind::RefMut(a) | ExprKind::Deref(a) => {
                self.lower_as_value(*a, out)
            }

            ExprKind::Field { base, field } => {
                let field_name = field.value.clone();
                let js_base = self.lower_as_value(*base, out);
                // Numeric field names (tuple index) must use bracket notation in JS.
                if let Ok(idx) = field_name.parse::<u64>() {
                    JsExpr::Index {
                        base: Box::new(js_base),
                        index: Box::new(JsExpr::Int(idx as i64)),
                    }
                } else {
                    JsExpr::Field {
                        base: Box::new(js_base),
                        field: js_ident(&field_name),
                    }
                }
            }

            ExprKind::Index { base, index } => JsExpr::Index {
                base: Box::new(self.lower_as_value(*base, out)),
                index: Box::new(self.lower_as_value(*index, out)),
            },

            ExprKind::ScopeRes { base, member } => JsExpr::Field {
                base: Box::new(self.lower_as_value(*base, out)),
                field: js_ident(&member.value),
            },

            ExprKind::Call { callee, args, .. } => {
                let callee = Box::new(self.lower_as_value(*callee, out));
                let args = args
                    .into_iter()
                    .map(|a| self.lower_as_value(a, out))
                    .collect();
                JsExpr::Call { callee, args }
            }

            ExprKind::As { value, .. } => self.lower_as_value(*value, out),

            ExprKind::Array(elems) => JsExpr::Array {
                elems: elems
                    .into_iter()
                    .map(|e| self.lower_as_value(e, out))
                    .collect(),
            },

            ExprKind::Tuple(elems) => {
                if elems.is_empty() {
                    return JsExpr::Undefined;
                }
                JsExpr::Array {
                    elems: elems
                        .into_iter()
                        .map(|e| self.lower_as_value(e, out))
                        .collect(),
                }
            }

            ExprKind::StructLit { name, fields, .. } => {
                let struct_name = self.name(name);
                let mut obj_fields: Vec<(String, JsExpr)> =
                    vec![("$tag".into(), JsExpr::Str(struct_name))];
                for (label, val) in fields {
                    obj_fields.push((js_ident(&label.value), self.lower_as_value(val, out)));
                }
                JsExpr::Object { fields: obj_fields }
            }

            ExprKind::DuckLit(fields) => JsExpr::Object {
                fields: fields
                    .into_iter()
                    .map(|(label, val)| (js_ident(&label.value), self.lower_as_value(val, out)))
                    .collect(),
            },

            ExprKind::Lambda {
                params,
                return_type,
                body,
                ..
            } => {
                let js_params: Vec<String> =
                    params.iter().map(|p| js_ident(&p.name.value)).collect();
                let is_void = return_type.as_ref().map(Self::is_void).unwrap_or(true);
                let body_stmts = self.lower_fn_body(*body, is_void);
                JsExpr::Arrow {
                    params: js_params,
                    body: JsArrowBody::Block(body_stmts),
                    is_async: false,
                }
            }

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => self.lower_match_as_value(value, arms, else_arm, out),

            ExprKind::Return(v) => {
                let ret = v.map(|e| self.lower_as_value(*e, out));
                out.push(JsStmt::Return(ret));
                JsExpr::Undefined
            }

            ExprKind::Async(e) => {
                // Wrap in an immediately-invoked async arrow if needed.
                // For now, just lower the inner expression.
                self.lower_as_value(*e, out)
            }

            ExprKind::Defer(e) => {
                // No direct equivalent; emit as a call expression at end of scope.
                // Best effort: emit it as a side effect expression.
                let inner = self.lower_as_value(*e, out);
                out.push(JsStmt::Expr(inner));
                JsExpr::Undefined
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
                JsExpr::Undefined
            }
        }
    }

    fn bin(
        &mut self,
        op: JsBinOp,
        a: Expr<Typed>,
        b: Expr<Typed>,
        out: &mut Vec<JsStmt>,
    ) -> JsExpr {
        let lhs = Box::new(self.lower_as_value(a, out));
        let rhs = Box::new(self.lower_as_value(b, out));
        JsExpr::BinOp { op, lhs, rhs }
    }

    fn is_void(te: &TypeExpr<Typed>) -> bool {
        matches!(
            te.desc,
            TypeDescription::Statement | TypeDescription::Never | TypeDescription::Any
        )
    }

    /// Build a `$tag === "Name"` check for match discrimination.
    fn tag_check(val: &JsExpr, struct_name: &str) -> JsExpr {
        JsExpr::BinOp {
            op: JsBinOp::StrictEq,
            lhs: Box::new(JsExpr::Field {
                base: Box::new(val.clone()),
                field: "$tag".into(),
            }),
            rhs: Box::new(JsExpr::Str(struct_name.to_string())),
        }
    }

    fn arm_cond(val: &JsExpr, arm: &MatchArm<Typed>, symbols: &SymbolTable) -> JsExpr {
        match &arm.pattern.desc {
            TypeDescription::TypeName { type_ref, .. } => {
                let name = symbols.get(*type_ref).name.clone();
                Self::tag_check(val, &name)
            }
            TypeDescription::Tag(s) => JsExpr::BinOp {
                op: JsBinOp::StrictEq,
                lhs: Box::new(val.clone()),
                rhs: Box::new(JsExpr::Str(format!(".{s}"))),
            },
            TypeDescription::Bool(Some(b)) => JsExpr::BinOp {
                op: JsBinOp::StrictEq,
                lhs: Box::new(val.clone()),
                rhs: Box::new(JsExpr::Bool(*b)),
            },
            TypeDescription::String(Some(s)) => JsExpr::BinOp {
                op: JsBinOp::StrictEq,
                lhs: Box::new(val.clone()),
                rhs: Box::new(JsExpr::Str(s.clone())),
            },
            // For unknown patterns, default to a truthy check (best-effort).
            _ => JsExpr::Bool(true),
        }
    }

    fn lower_match_as_value(
        &mut self,
        value: Box<Expr<Typed>>,
        arms: Vec<MatchArm<Typed>>,
        else_arm: Option<Box<Expr<Typed>>>,
        out: &mut Vec<JsStmt>,
    ) -> JsExpr {
        let tmp = self.fresh();
        let val_tmp = self.fresh();
        let val_expr = self.lower_as_value(*value, out);
        out.push(JsStmt::Const {
            name: val_tmp.clone(),
            value: val_expr,
        });
        out.push(JsStmt::Let {
            name: tmp.clone(),
            value: None,
        });

        // Build a chain of if/else if.
        let arms_with_cond: Vec<(JsExpr, Vec<JsStmt>)> = arms
            .into_iter()
            .map(|arm| {
                let val_ref = JsExpr::Ident(val_tmp.clone());
                let cond = Self::arm_cond(&val_ref, &arm, self.symbols);
                let binding_name = arm.binding.as_ref().map(|b| {
                    self.claim_local(&b.value)
                        .map(|id| self.names[&id].clone())
                        .unwrap_or_else(|| js_ident(&b.value))
                });
                let mut body_stmts = Vec::new();
                if let Some(bname) = binding_name {
                    body_stmts.push(JsStmt::Const {
                        name: bname,
                        value: JsExpr::Ident(val_tmp.clone()),
                    });
                }
                let result = self.lower_as_value(arm.body, &mut body_stmts);
                body_stmts.push(JsStmt::Assign {
                    target: JsExpr::Ident(tmp.clone()),
                    value: result,
                });
                (cond, body_stmts)
            })
            .collect();

        let default_stmts = else_arm.map(|e| {
            let mut body = Vec::new();
            let v = self.lower_as_value(*e, &mut body);
            body.push(JsStmt::Assign {
                target: JsExpr::Ident(tmp.clone()),
                value: v,
            });
            body
        });

        // Collapse into nested if/else if.
        let if_chain = build_if_chain(arms_with_cond, default_stmts);
        if let Some(stmt) = if_chain {
            out.push(stmt);
        }
        JsExpr::Ident(tmp)
    }

    fn lower_jsx_node(&mut self, node: JsxNode<Typed>, out: &mut Vec<JsStmt>) -> JsExpr {
        match node {
            JsxNode::Text(s) => {
                let s = s.trim().to_string();
                if s.is_empty() {
                    JsExpr::Undefined
                } else {
                    JsExpr::Str(s)
                }
            }
            JsxNode::Expr(e) => self.lower_as_value(*e, out),
            JsxNode::Element {
                tag,
                attrs,
                children,
            } => {
                let tag_arg = if tag.chars().next().map_or(false, |c| c.is_uppercase()) {
                    JsExpr::Ident(tag)
                } else {
                    JsExpr::Str(tag)
                };

                let attrs_arg = if attrs.is_empty() {
                    JsExpr::Null
                } else {
                    let fields = attrs
                        .into_iter()
                        .map(|a| {
                            let key = if a.name == "class" {
                                "className".to_string()
                            } else {
                                a.name
                            };
                            let val = match a.value {
                                JsxAttrValue::Bool => JsExpr::Bool(true),
                                JsxAttrValue::Str(s) => JsExpr::Str(s),
                                JsxAttrValue::Expr(e) => self.lower_as_value(*e, out),
                            };
                            (key, val)
                        })
                        .collect();
                    JsExpr::Object { fields }
                };

                let child_args: Vec<JsExpr> = children
                    .into_iter()
                    .map(|c| self.lower_jsx_node(c, out))
                    .filter(|e| !matches!(e, JsExpr::Undefined))
                    .collect();

                let mut args = vec![tag_arg, attrs_arg];
                args.extend(child_args);

                JsExpr::Call {
                    callee: Box::new(JsExpr::Ident("h".to_string())),
                    args,
                }
            }
        }
    }

    fn lower_as_stmts(&mut self, expr: Expr<Typed>, out: &mut Vec<JsStmt>) {
        match expr.kind {
            ExprKind::Block(stmts) => {
                for s in stmts {
                    self.lower_as_stmts(s, out);
                }
            }

            ExprKind::Let { name, value, .. } => {
                let js_val = self.lower_as_value(*value, out);
                let js_name = self
                    .claim_local(&name.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| js_ident(&name.value));
                out.push(JsStmt::Const {
                    name: js_name,
                    value: js_val,
                });
            }

            ExprKind::LetTuple { names, value } => {
                let js_val = self.lower_as_value(*value, out);
                let js_names: Vec<String> = names
                    .iter()
                    .map(|n| {
                        self.claim_local(&n.value)
                            .map(|id| self.names[&id].clone())
                            .unwrap_or_else(|| js_ident(&n.value))
                    })
                    .collect();
                out.push(JsStmt::MultiConst {
                    names: js_names,
                    value: js_val,
                });
            }

            ExprKind::Const { name, value, .. } => {
                let js_val = self.lower_as_value(*value, out);
                let js_name = self
                    .claim_local(&name.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| js_ident(&name.value));
                out.push(JsStmt::Const {
                    name: js_name,
                    value: js_val,
                });
            }

            ExprKind::Assign { target, value } => {
                let t = self.lower_as_value(*target, out);
                let v = self.lower_as_value(*value, out);
                out.push(JsStmt::Assign {
                    target: t,
                    value: v,
                });
            }

            ExprKind::AddAssign { target, value } => {
                self.compound_assign(JsBinOp::Add, *target, *value, out)
            }
            ExprKind::SubAssign { target, value } => {
                self.compound_assign(JsBinOp::Sub, *target, *value, out)
            }
            ExprKind::MulAssign { target, value } => {
                self.compound_assign(JsBinOp::Mul, *target, *value, out)
            }
            ExprKind::DivAssign { target, value } => {
                self.compound_assign(JsBinOp::Div, *target, *value, out)
            }
            ExprKind::ModAssign { target, value } => {
                self.compound_assign(JsBinOp::Mod, *target, *value, out)
            }
            ExprKind::ShrAssign { target, value } => {
                self.compound_assign(JsBinOp::Shr, *target, *value, out)
            }
            ExprKind::ShlAssign { target, value } => {
                self.compound_assign(JsBinOp::Shl, *target, *value, out)
            }

            ExprKind::Return(v) => {
                let ret = v.map(|e| self.lower_as_value(*e, out));
                out.push(JsStmt::Return(ret));
            }
            ExprKind::Break => out.push(JsStmt::Break),
            ExprKind::Continue => out.push(JsStmt::Continue),

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
                out.push(JsStmt::If {
                    cond,
                    then: then_stmts,
                    else_: else_stmts,
                });
            }

            ExprKind::While { condition, body } => {
                let cond = self.lower_as_value(*condition, out);
                let mut body_stmts = Vec::new();
                self.lower_as_stmts(*body, &mut body_stmts);
                out.push(JsStmt::While {
                    cond,
                    body: body_stmts,
                });
            }

            ExprKind::For {
                binding,
                iterable,
                body,
                ..
            } => {
                let iter = self.lower_as_value(*iterable, out);
                let binding_name = self
                    .claim_local(&binding.value)
                    .map(|id| self.names[&id].clone())
                    .unwrap_or_else(|| js_ident(&binding.value));
                let mut body_stmts = Vec::new();
                self.lower_as_stmts(*body, &mut body_stmts);
                out.push(JsStmt::ForOf {
                    val: binding_name,
                    iter,
                    body: body_stmts,
                });
            }

            ExprKind::Match {
                value,
                arms,
                else_arm,
            } => {
                let val_tmp = self.fresh();
                let val_expr = self.lower_as_value(*value, out);
                out.push(JsStmt::Const {
                    name: val_tmp.clone(),
                    value: val_expr,
                });

                let arms_with_cond: Vec<(JsExpr, Vec<JsStmt>)> = arms
                    .into_iter()
                    .map(|arm| {
                        let val_ref = JsExpr::Ident(val_tmp.clone());
                        let cond = Self::arm_cond(&val_ref, &arm, self.symbols);
                        let binding_name = arm.binding.as_ref().map(|b| {
                            self.claim_local(&b.value)
                                .map(|id| self.names[&id].clone())
                                .unwrap_or_else(|| js_ident(&b.value))
                        });
                        let mut body_stmts = Vec::new();
                        if let Some(bname) = binding_name {
                            body_stmts.push(JsStmt::Const {
                                name: bname,
                                value: JsExpr::Ident(val_tmp.clone()),
                            });
                        }
                        self.lower_as_stmts(arm.body, &mut body_stmts);
                        (cond, body_stmts)
                    })
                    .collect();

                let default_stmts = else_arm.map(|e| {
                    let mut body = Vec::new();
                    self.lower_as_stmts(*e, &mut body);
                    body
                });

                if let Some(stmt) = build_if_chain(arms_with_cond, default_stmts) {
                    out.push(stmt);
                }
            }

            ExprKind::Async(e) => self.lower_as_stmts(*e, out),

            ExprKind::Defer(e) => {
                let inner = self.lower_as_value(*e, out);
                out.push(JsStmt::Expr(inner));
            }

            ExprKind::InlineGo(s) => {
                for line in s.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        out.push(JsStmt::Expr(JsExpr::Raw(trimmed.to_string())));
                    }
                }
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
                if !matches!(val, JsExpr::Undefined) {
                    out.push(JsStmt::Expr(val));
                }
            }
        }
    }

    fn compound_assign(
        &mut self,
        op: JsBinOp,
        target: Expr<Typed>,
        value: Expr<Typed>,
        out: &mut Vec<JsStmt>,
    ) {
        let t = self.lower_as_value(target, out);
        let v = self.lower_as_value(value, out);
        out.push(JsStmt::Assign {
            target: t.clone(),
            value: JsExpr::BinOp {
                op,
                lhs: Box::new(t),
                rhs: Box::new(v),
            },
        });
    }

    fn lower_fn_body(&mut self, body: Expr<Typed>, is_void: bool) -> Vec<JsStmt> {
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
                        if !matches!(val, JsExpr::Undefined) {
                            stmts.push(JsStmt::Return(Some(val)));
                        }
                    }
                }
            }
            ExprKind::Return(_) => self.lower_as_stmts(body, &mut stmts),
            _ => {
                let val = self.lower_as_value(body, &mut stmts);
                if !matches!(val, JsExpr::Undefined) {
                    stmts.push(JsStmt::Return(Some(val)));
                }
            }
        }
        stmts
    }

    fn lower_function(&mut self, f: FunctionDecl<Typed>) -> JsDecl {
        let is_void = f.return_type.as_ref().map(Self::is_void).unwrap_or(true);
        let params: Vec<String> = f.params.iter().map(|p| js_ident(&p.name.value)).collect();
        let body = self.lower_fn_body(f.body, is_void);
        JsDecl::Function {
            name: js_ident(&f.name.value),
            params,
            is_async: false,
            body,
        }
    }

    fn lower_extension(&mut self, ext: ExtensionDecl<Typed>) -> Vec<JsDecl> {
        ext.methods
            .into_iter()
            .filter(|m| m.generics.is_empty())
            .map(|m| {
                let is_void = m.return_type.as_ref().map(Self::is_void).unwrap_or(true);
                let mut params = vec!["self".to_string()];
                params.extend(m.params.iter().map(|p| js_ident(&p.name.value)));
                let body = self.lower_fn_body(m.body, is_void);
                JsDecl::Function {
                    name: js_ident(&m.name.value),
                    params,
                    is_async: false,
                    body,
                }
            })
            .collect()
    }

    fn lower_struct(&self, s: StructDecl<Typed>) -> JsDecl {
        let name = js_ident(&s.name.value);
        let field_names: Vec<String> = s.fields.iter().map(|f| js_ident(&f.name.value)).collect();

        // Emit a constructor function: `function Foo(x, y) { return {$tag:"Foo",x,y}; }`
        let obj_fields: Vec<(String, JsExpr)> =
            std::iter::once(("$tag".into(), JsExpr::Str(name.clone())))
                .chain(
                    field_names
                        .iter()
                        .map(|f| (f.clone(), JsExpr::Ident(f.clone()))),
                )
                .collect();

        JsDecl::Function {
            name: name.clone(),
            params: field_names,
            is_async: false,
            body: vec![JsStmt::Return(Some(JsExpr::Object { fields: obj_fields }))],
        }
    }
}

/// Build a nested if/else-if chain from a list of (condition, body) pairs plus an optional
/// default (else) block. Returns `None` when the input is empty.
fn build_if_chain(
    mut arms: Vec<(JsExpr, Vec<JsStmt>)>,
    default: Option<Vec<JsStmt>>,
) -> Option<JsStmt> {
    if arms.is_empty() {
        return default.map(|d| JsStmt::Block(d));
    }
    let (cond, then) = arms.remove(0);
    let else_ = build_if_chain(arms, default).map(|s| vec![s]);
    Some(JsStmt::If { cond, then, else_ })
}

/// Compile a typed Duck source file to a JS file.
pub fn lower_js(out: InferOutput) -> JsFile {
    let mut l = Lowerer::new(&out.symbols);
    let mut decls: Vec<JsDecl> = Vec::new();
    let mut ts_packages: Vec<(String, String)> = Vec::new();
    let mut client_fn_names: Vec<String> = Vec::new();

    for item in out.source_file.items {
        match item {
            Item::Function(f) => {
                if f.is_client {
                    client_fn_names.push(f.name.value.clone());
                    decls.push(l.lower_function(f));
                }
            }
            Item::Struct(s) => decls.push(l.lower_struct(s)),
            Item::TypeAlias(_) => {}
            Item::Extension(e) => decls.extend(l.lower_extension(e)),
            Item::Use(crate::parser2::parser::UseDecl::Ts(pkg_name, _)) => {
                let binding = pkg_name
                    .split('/')
                    .next_back()
                    .unwrap_or(&pkg_name)
                    .to_string();
                ts_packages.push((pkg_name, binding));
            }
            Item::Use(_) => {}
        }
    }

    JsFile {
        decls,
        ts_packages,
        client_fn_names,
    }
}
