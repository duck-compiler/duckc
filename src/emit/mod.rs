use std::collections::HashSet;

use crate::{
    parse::{
        Spanned,
        function_parser::LambdaFunctionExpr,
        type_parser::{Duck, TypeExpr},
        value_parser::ValueExpr,
    },
    semantics::type_resolve::{TypeEnv, trav_type_expr, trav_value_expr},
};

pub mod duckx_component;
pub mod function;
pub mod ir;
pub mod jsx_component;
pub mod schema_def;
pub mod source_file;
pub mod types;
pub mod value;

pub fn is_identifier_blocked_by_go(ident: &str) -> bool {
    match ident {
        "go" | "map" | "chan" | "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8"
        | "uint16" | "uint32" | "uint64" | "string" | "float" | "float32" | "float64"
        | "select" | "package" | "import" | "interface" | "var" | "rune" | "bool" | "complex64"
        | "range" | "goto" | "break" | "fallthrough" | "default" | "complex128" => true,
        _ => false,
    }
}

pub fn fix_ident_for_go(ident: &str, _imports: &HashSet<String>) -> String {
    if is_identifier_blocked_by_go(ident)
    /*|| imports.contains(ident)*/
    {
        format!("Δ{ident}")
    } else {
        ident.to_string()
    }
}

pub fn fix_all_idents_type_expr(
    t: &mut Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
    imports: &'static HashSet<String>,
) {
    trav_type_expr(
        |t, _| match &mut t.0 {
            TypeExpr::Struct {
                name,
                type_params: _,
            } => *name = fix_ident_for_go(name, imports),
            TypeExpr::Duck(Duck { fields }) => {
                for field in fields.iter_mut() {
                    field.name = fix_ident_for_go(&field.name, imports);
                }
            }
            TypeExpr::NamedDuck {
                name,
                type_params: _,
            } => *name = fix_ident_for_go(name, imports),
            TypeExpr::TypeOf(z) => *z = fix_ident_for_go(z, imports),
            TypeExpr::Fun(params, _, _) => {
                for p in params {
                    if let Some(p) = p.0.as_mut() {
                        *p = fix_ident_for_go(p, imports);
                    }
                }
            }
            TypeExpr::TypeName(_, name, _) => {
                *name = fix_ident_for_go(name, imports);
            }
            _ => {}
        },
        t,
        type_env,
    );
}

pub fn fix_all_idents_value_expr(
    v: &mut Spanned<ValueExpr>,
    type_env: &mut TypeEnv,
    imports: &'static HashSet<String>,
) {
    trav_value_expr(
        |t, _| match &mut t.0 {
            TypeExpr::Struct {
                name,
                type_params: _,
            } => *name = fix_ident_for_go(name, imports),
            TypeExpr::Duck(Duck { fields }) => {
                for field in fields.iter_mut() {
                    field.name = fix_ident_for_go(&field.name, imports);
                }
            }
            TypeExpr::NamedDuck {
                name,
                type_params: _,
            } => *name = fix_ident_for_go(name, imports),
            TypeExpr::TypeOf(z) => *z = fix_ident_for_go(z, imports),
            TypeExpr::Fun(params, _, _) => {
                for p in params {
                    if let Some(p) = p.0.as_mut() {
                        *p = fix_ident_for_go(p, imports);
                    }
                }
            }
            TypeExpr::TypeName(_, name, _) => {
                *name = fix_ident_for_go(name, imports);
            }
            _ => {}
        },
        |v, _| match &mut v.0 {
            ValueExpr::Struct {
                name,
                fields,
                type_params: _,
            } => {
                *name = fix_ident_for_go(name, imports);
                for f in fields {
                    f.0 = fix_ident_for_go(&f.0, imports);
                }
            }
            ValueExpr::Lambda(expr) => {
                let LambdaFunctionExpr {
                    is_mut: _,
                    params,
                    return_type: _,
                    value_expr: _,
                } = expr.as_mut();
                for p in params {
                    p.0 = fix_ident_for_go(&p.0, imports);
                }
            }
            ValueExpr::Duck(fields) => {
                for f in fields {
                    f.0 = fix_ident_for_go(&f.0, imports);
                }
            }
            ValueExpr::Variable(_, name, _, _, _) => {
                *name = fix_ident_for_go(name, imports);
            }
            ValueExpr::VarDecl(decl) => decl.0.name = fix_ident_for_go(&decl.0.name, imports),
            ValueExpr::For {
                ident,
                target: _,
                block: _,
            } => ident.0 = fix_ident_for_go(&ident.0, imports),
            ValueExpr::Match {
                value_expr: _,
                arms,
                else_arm,
                span: _,
            } => {
                for arm in arms {
                    if let Some(ident) = arm.identifier_binding.as_mut() {
                        *ident = fix_ident_for_go(ident, imports);
                    }
                }

                if let Some(arm) = else_arm
                    && let Some(ident) = arm.identifier_binding.as_mut()
                {
                    *ident = fix_ident_for_go(ident, imports);
                }
            }
            _ => {}
        },
        v,
        type_env,
    );
}
