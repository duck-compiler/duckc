use crate::{
    emit::value::{IrInstruction, IrValue},
    parse::type_parser::{Duck, Field, Struct, TypeExpr},
    semantics::typechecker::TypeEnv,
};

pub fn emit_type_definitions(type_env: &mut TypeEnv) -> Vec<IrInstruction> {
    let summary = type_env.summarize();

    fn interface_implementations(
        typename: String,
        type_expr: &TypeExpr,
        type_env: &mut TypeEnv,
    ) -> Vec<IrInstruction> {
        return match type_expr {
            TypeExpr::Duck(duck) => duck
                .fields
                .iter()
                .map(|field| {
                    IrInstruction::FunDef(
                        format!("Get{}", field.name),
                        Some(("self".into(), typename.clone())),
                        vec![],
                        Some(field.type_expr.0.as_go_concrete_annotation(type_env)),
                        vec![IrInstruction::Return(Some(IrValue::FieldAccess(
                            IrValue::Var("self".into()).into(),
                            field.name.clone(),
                        )))],
                    )
                })
                .collect::<Vec<_>>(),
            TypeExpr::Struct(r#_struct) => todo!(),
            _ => vec![],
        };
    }

    let interface_defs = summary
        .param_names_used
        .iter()
        .map(|param_name| {
            IrInstruction::InterfaceDef(
                format!("Has{param_name}"),
                vec![("T".into(), "any".into())],
                vec![(format!("Get{param_name}"), "T".into())],
            )
        })
        .collect::<Vec<_>>();

    summary
        .types_used
        .iter()
        .filter(|type_expr| type_expr.is_object_like())
        .map(|type_expr| {
            let type_name = type_expr.as_clean_go_type_name(type_env);

            let mut i = interface_implementations(type_name.clone(), type_expr, type_env);

            i.push(match type_expr {
                TypeExpr::Tuple(t) => IrInstruction::StructDef(
                    type_name,
                    t.iter()
                        .enumerate()
                        .map(|(i, x)| (format!("field_{i}"), x.0.as_clean_go_type_name(type_env)))
                        .collect::<Vec<_>>(),
                ),
                TypeExpr::Struct(Struct { fields }) | TypeExpr::Duck(Duck { fields }) => {
                    IrInstruction::StructDef(
                        type_name,
                        fields
                            .iter()
                            .map(
                                |Field {
                                     name,
                                     type_expr: (type_expr, _),
                                 }| {
                                    (name.clone(), type_expr.as_clean_go_type_name(type_env))
                                },
                            )
                            .collect::<Vec<_>>(),
                    )
                }
                _ => panic!("cant create for {type_name}"),
            });
            i
        })
        .chain(vec![interface_defs].into_iter())
        .fold(Vec::new(), |mut acc, x| {
            acc.extend(x);
            acc
        })
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(_, name) => type_env
                .resolve_type_alias(name)
                .as_go_type_annotation(type_env),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name}: {}", type_expr.0.as_go_type_annotation(type_env)),
                        None => type_expr.0.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
                        .0
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Struct(r#struct) => format!(
                "struct {{\n{}\n}}",
                r#struct
                    .fields
                    .iter()
                    .map(|field| format!(
                        "   {} {}",
                        field.name,
                        field.type_expr.0.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            TypeExpr::Duck(duck) => {
                let mut fields = duck.fields.clone();
                fields.sort_by_key(|field| field.name.clone());

                format!(
                    "interface {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|field| format!(
                            "   Has{}[{}]",
                            field.name,
                            field.type_expr.0.as_go_type_annotation(type_env)
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TypeExpr::Tuple(fields) => {
                format!(
                    "struct {{\n{}\n}}",
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, type_expr)| format!(
                            "field_{i} {}",
                            type_expr.0.as_go_type_annotation(type_env).to_string()
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    pub fn as_go_concrete_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::TypeName(_, name) => type_env
                .resolve_type_alias(name)
                .as_go_concrete_annotation(type_env),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                            format!("{name}: {}", type_expr.0.as_go_type_annotation(type_env)),
                        None => type_expr.0.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
                        .0
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => format!(
                "struct {{\n{}\n}}",
                fields
                    .iter()
                    .map(|field| format!(
                        "   {} {}",
                        field.name,
                        field.type_expr.0.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "struct {{\n{}\n}}",
                    fields
                        .iter()
                        .enumerate()
                        .map(|(i, type_expr)| format!(
                            "field_{i} {}",
                            type_expr.0.as_go_type_annotation(type_env).to_string()
                        ))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    pub fn as_clean_go_type_name(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(_, name) => name.clone(),
            TypeExpr::InlineGo => "InlineGo".to_string(),
            TypeExpr::Fun(params, return_type) => format!(
                "Fun_From_{}{}",
                params
                    .iter()
                    .map(|(name, type_expr)| format!(
                        "{}_{}",
                        name.clone().unwrap_or_else(|| "".to_string()),
                        type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_"),
                return_type
                    .as_ref()
                    .map(|type_expr| format!("_To_{}", type_expr.0.as_clean_go_type_name(type_env)))
                    .unwrap_or_else(|| "".to_string())
            ),
            TypeExpr::Struct(r#struct) if r#struct.fields.is_empty() => "Any".to_string(),
            TypeExpr::Struct(r#struct) => format!(
                "Struct_{}",
                r#struct
                    .fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!(
                        "{}_{}",
                        field.name,
                        field.type_expr.0.as_clean_go_type_name(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tuple_{}",
                    fields
                        .iter()
                        .map(|type_expr| type_expr.0.as_clean_go_type_name(type_env).to_string())
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    /// First return is go code repr, second is id
    /// First go code and second is identifier of self
    pub fn emit(&self) -> (String, String) {
        match self {
            TypeExpr::Tuple(types) => (
                [
                    "struct {\n",
                    &types
                        .iter()
                        .enumerate()
                        .map(|(i, x)| format!("field_{i} {}\n", x.0.emit().0))
                        .collect::<Vec<_>>()
                        .join(""),
                    "}",
                ]
                .join(""),
                format!(
                    "Tup{}",
                    types
                        .iter()
                        .map(|x| format!("_{}", x.0.emit().1))
                        .collect::<Vec<_>>()
                        .join("")
                ),
            ),
            TypeExpr::Duck(Duck { fields }) => {
                let mut fields = fields.clone();
                fields.sort_by_key(|x| x.name.clone());
                if fields.is_empty() {
                    ("interface{}".to_string(), "Any".to_string())
                } else {
                    let name = format!(
                        "Duck{}",
                        fields
                            .into_iter()
                            .map(|x| format!("_Has{}_{}", x.name, x.type_expr.0.emit().1))
                            .collect::<Vec<_>>()
                            .join("")
                    );
                    (name.clone(), name)
                }
            }
            TypeExpr::Struct(Struct { fields }) => (
                [
                    "struct",
                    &fields
                        .iter()
                        .map(|field| format!("_With{}{}", field.name, field.type_expr.0.emit().1))
                        .collect::<Vec<_>>()
                        .join(""),
                    "{\n",
                    &fields
                        .iter()
                        .map(|field| format!("   {} {}", field.name, field.type_expr.0.emit().1))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    "\n}\n",
                ]
                .join(""),
                format!(
                    "Struct_{}",
                    fields
                        .iter()
                        .map(|field| format!("_With{}{}", field.name, field.type_expr.0.emit().1))
                        .collect::<Vec<_>>()
                        .join("")
                ),
            ),
            TypeExpr::Go(x) => (x.clone(), format!("Go{}", x.replace(".", "_"))),
            TypeExpr::TypeName(_, x) => (x.clone(), x.clone()),
            TypeExpr::Int => ("Int".to_string(), "Int".to_string()),
            TypeExpr::Float => ("Float".to_string(), "Float".to_string()),
            TypeExpr::Bool => ("Bool".to_string(), "Bool".to_string()),
            TypeExpr::String => ("String".to_string(), "String".to_string()),
            TypeExpr::Char => ("Char".to_string(), "Char".to_string()),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{
        lexer::lexer, make_input, type_parser::type_expression_parser, value_parser::empty_range,
    };
    use chumsky::prelude::*;

    #[test]
    fn test_emit() {
        let test_cases = vec![
            ("()", ("struct {\n}", "Tup")),
            (
                "((), {})",
                (
                    "struct {\nfield_0 struct {\n}\nfield_1 interface{}\n}",
                    "Tup_Tup_Any",
                ),
            ),
            ("{}", ("interface{}", "Any")),
            ("({})", ("struct {\nfield_0 interface{}\n}", "Tup_Any")),
            (
                "{x: String, y: ({}, Int)}",
                (
                    "Duck_Hasx_String_Hasy_Tup_Any_Int",
                    "Duck_Hasx_String_Hasy_Tup_Any_Int",
                ),
            ),
        ];

        for (src, exp) in test_cases {
            let lex = lexer("test", src).parse(src).unwrap();
            let parse = type_expression_parser()
                .parse(make_input(empty_range(), &lex))
                .unwrap()
                .0
                .emit();
            let exp = (exp.0.to_string(), exp.1.to_string());
            assert_eq!(parse, exp, "{src}");
        }
    }
}
