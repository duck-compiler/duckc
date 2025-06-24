use crate::{
    parse::type_parser::{Duck, Struct, TypeExpr},
    semantics::typechecker::TypeEnv,
};

pub fn emit_type_definitions(type_env: &mut TypeEnv) -> String {
    let summary = type_env.summarize();

    let interface_defs = summary.param_names_used.iter()
        .map(|param_name| [
            format!("type Has{}[T any] interface {{", param_name),
            format!("   Get{}() T", param_name),
            "}".to_string(),
        ].join("\n"))
        .collect::<Vec<_>>()
        .join("\n");

    let type_defs = summary.types_used.iter()
        .filter(|type_expr| type_expr.is_object_like())
        .map(|type_expr| format!("type {} {};", type_expr.as_clean_go_type_name(type_env), type_expr.as_go_type_annotation(type_env)))
        .collect::<Vec<_>>()
        .join("\n");

    return format!(
        "{}\n{}",
        interface_defs,
        type_defs,
    )
}

impl TypeExpr {
    pub fn as_go_type_annotation(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(name) => name.to_string(),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                        format!("{name}: {}", type_expr.as_go_type_annotation(type_env)),
                        None => type_expr.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
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
                        field.type_expr.as_go_type_annotation(type_env)
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
                            field.type_expr.as_go_type_annotation(type_env)
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
                        .map(|type_expr| format!("{}", type_expr.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>()
                        .join("_")
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
            TypeExpr::TypeName(name) => name.to_string(),
            TypeExpr::Fun(params, return_type) => format!(
                "func({}) {}",
                params
                    .iter()
                    .map(|(name, type_expr)| match name {
                        Some(name) =>
                        format!("{name}: {}", type_expr.as_go_type_annotation(type_env)),
                        None => type_expr.as_go_type_annotation(type_env),
                    })
                    .collect::<Vec<_>>()
                    .join(","),
                return_type
                    .clone()
                    .map_or("".to_string(), |return_type| return_type
                        .as_go_type_annotation(type_env))
            ),
            TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => format!(
                "struct {{\n{}\n}}",
                fields
                    .iter()
                    .map(|field| format!(
                        "   {} {}",
                        field.name,
                        field.type_expr.as_go_type_annotation(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "struct {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|type_expr| format!("{}", type_expr.as_go_type_annotation(type_env)))
                        .collect::<Vec<_>>()
                        .join("_")
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
            TypeExpr::TypeName(name) => name.clone(),
            TypeExpr::Fun(params, return_type) => format!(
                "Fun_From_{}{}",
                params.iter()
                    .map(|(name, type_expr)| format!("{}_{}", name.clone().unwrap_or_else(|| "".to_string()), type_expr.as_clean_go_type_name(type_env)))
                    .collect::<Vec<_>>()
                    .join("_"),
                return_type.as_ref()
                    .map(|type_expr| format!("_To_{}", type_expr.as_clean_go_type_name(type_env)))
                    .unwrap_or_else(|| "".to_string())
            ),
            TypeExpr::Struct(r#struct) if r#struct.fields.is_empty() => format!("Any"),
            TypeExpr::Struct(r#struct) => format!(
                "Struct_{}",
                r#struct.fields
                    .iter()
                    .map(|field| format!("{}_{}", field.name, field.type_expr.as_clean_go_type_name(type_env)))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Duck(duck) => format!(
                "Duck_{}",
                duck.fields
                    .iter()
                    .map(|field| format!("{}_{}", field.name, field.type_expr.as_clean_go_type_name(type_env)))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Tuple(fields) => {
                format!(
                    "struct {{\n{}\n}}",
                    fields
                        .iter()
                        .map(|type_expr| format!("{}", type_expr.as_go_type_annotation(type_env)))
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
                        .map(|(i, x)| format!("field_{i} {}\n", x.emit().0))
                        .collect::<Vec<_>>()
                        .join(""),
                    "}",
                    ]
                    .join(""),
                format!(
                    "Tup{}",
                    types
                        .iter()
                        .map(|x| format!("_{}", x.emit().1))
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
                            .map(|x| format!("_Has{}_{}", x.name, x.type_expr.emit().1))
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
                        .map(|field| format!("_With{}{}", field.name, field.type_expr.emit().1))
                        .collect::<Vec<_>>()
                        .join(""),
                    "{\n",
                    &fields
                        .iter()
                        .map(|field| format!("   {} {}", field.name, field.type_expr.emit().1))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    "\n}\n",
                    ]
                    .join(""),
                format!(
                    "Struct_{}",
                    fields
                        .iter()
                        .map(|field| format!("_With{}{}", field.name, field.type_expr.emit().1))
                        .collect::<Vec<_>>()
                        .join("")
                ),
            ),
            TypeExpr::Go(x) => (x.clone(), format!("Go{}", x.replace(".", "_"))),
            TypeExpr::TypeName(x) => (x.clone(), x.clone()),
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
    use crate::parse::{lexer::lexer, type_parser::type_expression_parser};
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
            let lex = lexer().parse(src).unwrap();
            let parse = type_expression_parser().parse(&lex).unwrap().emit();
            let exp = (exp.0.to_string(), exp.1.to_string());
            assert_eq!(parse, exp, "{src}");
        }
    }
}
