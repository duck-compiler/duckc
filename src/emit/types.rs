use crate::{
    parse::type_parser::{Duck, Struct, TypeExpr},
    semantics::typechecker::TypeEnv,
};

impl TypeExpr {
    pub fn as_go_implementation(&self, type_env: &mut TypeEnv) -> String {
        match self {
            TypeExpr::Int
            | TypeExpr::Char
            | TypeExpr::Bool
            | TypeExpr::String
            | TypeExpr::Float
            | TypeExpr::TypeName(..)
            | TypeExpr::Go(..)
            | TypeExpr::Any => self.to_go_type_str(type_env),
            TypeExpr::Tuple(_types) => [
                "type".to_string(),
                self.to_go_type_str(type_env),
                "struct".to_string(),
                "{\n".to_string(),
                "\n}\n".to_string(),
            ]
            .join(" "),
            TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => [
                "type".to_string(),
                self.to_go_type_str(type_env),
                "struct".to_string(),
                "{\n".to_string(),
                fields
                    .iter()
                    .map(|(field_name, type_expr)| {
                        format!("    {} {}", field_name, type_expr.to_go_type_str(type_env))
                    })
                    .collect::<Vec<String>>()
                    .join("\n"),
                "\n}\n".to_string(),
            ]
            .join(" "),
            TypeExpr::Or(..) => todo!(),
            TypeExpr::Fun(_param_types, _return_type) => ["func("].join(" "),
        }
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
                fields.sort_by_key(|x| x.0.clone());
                if fields.is_empty() {
                    ("interface{}".to_string(), "Any".to_string())
                } else {
                    let name = format!(
                        "Duck{}",
                        fields
                            .into_iter()
                            .map(|x| format!("_Has{}_{}", x.0, x.1.emit().1))
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
                        .map(|field| format!("_With{}{}", field.0, field.1.emit().1))
                        .collect::<Vec<_>>()
                        .join(""),
                    "{\n",
                    &fields
                        .iter()
                        .map(|(field_name, field_type)| {
                            format!("   {field_name} {}", field_type.emit().1)
                        })
                        .collect::<Vec<_>>()
                        .join("\n"),
                    "\n}\n",
                ]
                .join(""),
                format!(
                    "Struct_{}",
                    fields
                        .iter()
                        .map(|field| format!("_With{}{}", field.0, field.1.emit().1))
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
    use crate::parse::{lexer::lexer, make_no_span_input, type_parser::type_expression_parser};
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
            let parse = type_expression_parser().parse(make_no_span_input(&lex)).unwrap().emit();
            let exp = (exp.0.to_string(), exp.1.to_string());
            assert_eq!(parse, exp, "{src}");
        }
    }
}
