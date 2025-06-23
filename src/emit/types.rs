use crate::{emit::value::{EmitEnvironment, GoMethodDef, GoTypeDef}, parse::type_parser::{Duck, Struct, TypeExpr}};

impl TypeExpr {
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
            _ => todo!()
        }
    }

    pub fn emit_into_env(&self, env: EmitEnvironment) {
        match self {
            TypeExpr::Tuple(params) => {
                for param in params {
                    param.emit_into_env(env.clone());
                }
            }
            TypeExpr::Struct(Struct { fields }) => {
                let mut type_fields = Vec::new();
                let mut go_type_name = "Struct".to_string();

                let mut methods = Vec::new();

                for (field_name, field_type) in fields {
                    field_type.emit_into_env(env.clone());
                    let go_type_name = field_type.emit();
                    type_fields.push((field_name.clone(), go_type_name.clone()));
                    methods.push(GoMethodDef {
                        name: format!("Duck_Get{field_name}"),
                        body: vec![format!("return self.{field_name}")],
                        params: vec![],
                        return_type: Some(go_type_name.1),
                    });
                }

                for (field_name, field_type) in &type_fields {
                    go_type_name.push_str(&format!(
                        "_Has{field_name}_{}",
                        field_type.1,
                    ));
                }

                let mut go_interface_name = "Duck".to_string();

                let mut cloned_type_fields = type_fields.clone();
                cloned_type_fields.sort();
                for (field_name, field_type) in &type_fields {
                    go_interface_name.push_str(&format!(
                        "_Has{field_name}_{}",
                        field_type.1,
                    ));
                }

                let go_struct = GoTypeDef::Struct {
                    name: go_type_name.clone(),
                    fields: type_fields.into_iter().map(|(field_name, (as_go, _))| (field_name, as_go)).collect(),
                    methods: methods.clone(),
                };
                let go_interface = GoTypeDef::Interface {
                    name: go_interface_name,
                    methods: methods.clone(),
                };

                env.push_types([go_struct, go_interface].into_iter());
            }
            TypeExpr::Duck(Duck { fields }) => {
                let mut type_fields = Vec::new();
                let mut go_type_name = "Duck".to_string();

                let mut methods = Vec::new();

                for (field_name, field_type) in fields {
                    field_type.emit_into_env(env.clone());
                    let go_type_name = field_type.emit();
                    type_fields.push((field_name.clone(), go_type_name.clone()));
                    methods.push(GoMethodDef {
                        name: format!("Duck_Get{field_name}"),
                        body: vec![format!("return self.{field_name}")],
                        params: vec![],
                        return_type: Some(go_type_name.0),
                    });
                }

                type_fields.sort();

                for (field_name, field_type) in &type_fields {
                    go_type_name.push_str(&format!(
                        "_Has{field_name}_{}",
                        field_type.1,
                    ));
                }

                let go_interface_name = go_type_name.clone();

                go_type_name.push_str("_Struct");

                let go_struct = GoTypeDef::Struct {
                    name: go_type_name.clone(),
                    fields: type_fields.into_iter().map(|(field_name, (as_go, _))| (field_name, as_go)).collect(),
                    methods: methods.clone(),
                };
                let go_interface = GoTypeDef::Interface {
                    name: go_interface_name,
                    methods: methods.clone(),
                };

                env.push_types([go_struct, go_interface].into_iter());
            }
            _ => {}
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
