use std::{collections::HashMap, process};

use crate::parse::{
    function_parser::FunctionDefintion, source_file_parser::SourceFile, type_parser::{Duck, Struct, TypeExpr}, value_parser::ValueExpr
};

#[derive(Debug, Clone)]
pub struct TypeEnv {
    identifier_types: Vec<HashMap<String, TypeExpr>>,
    type_aliases: Vec<HashMap<String, TypeExpr>>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],
        }
    }
}

impl TypeEnv {
    pub fn push_type_aliases(&mut self) {
        let cloned_hash_map = self.type_aliases.last().expect("Expect at least one env.").clone();
        self.type_aliases.push(cloned_hash_map);
    }

    pub fn pop_type_aliases(&mut self) {
        self.identifier_types.pop();
    }

    pub fn push_identifier_types(&mut self) {
        let cloned_hash_map = self.identifier_types.last().expect("Expect at least one env.").clone();
        self.identifier_types.push(cloned_hash_map);
    }

    pub fn pop_identifier_types(&mut self) {
        self.identifier_types.pop();
    }

    pub fn insert_identifier_type(&mut self, identifier: String, type_expr: TypeExpr) {
        self.identifier_types
            .last_mut()
            .expect("At least one env should exist. :(")
            .insert(identifier, type_expr);
    }

    pub fn get_identifier_type(&self, identifier: String) -> Option<TypeExpr> {
        let env = self
            .identifier_types
            .last()
            .expect("At least one env should exist. :(");

        if !env.contains_key(&identifier) {
            return None;
        }

        return env.get(&identifier).cloned();
    }

    pub fn insert_type_alias(&mut self, alias: String, type_expr: TypeExpr) {
         self.type_aliases
            .last_mut()
            .expect("At least one type aliases hashmap should exist. :(")
            .insert(alias, type_expr);
    }

    pub fn resolve_type_alias(&mut self, alias: String) -> TypeExpr {
        let type_aliases = self
            .type_aliases
            .last()
            .expect("At least one type aliases hashmap should exist. :(");

        type_aliases
            .get(&alias)
            .expect("Couldn't resolve type alias {alias}")
            .clone()
    }
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| type_env.insert_type_alias(type_definition.name.clone(), type_definition.type_expression.clone()));

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| typeresolve_function_definition(function_definition, type_env));

    type_env.pop_type_aliases();

    fn typeresolve_function_definition(function_definition: &mut FunctionDefintion, type_env: &mut TypeEnv) {
        let fn_type_expr = TypeExpr::Fun(
            function_definition.params
                .as_ref()
                .or(Some(&vec![]))
                .unwrap()
                .iter()
                .map(|(_, type_expr)| type_expr.clone())
                .collect::<Vec<_>>(),
            function_definition.return_type.as_ref().map(|x| Box::new(x.clone()))
        );

        type_env.insert_identifier_type(function_definition.name.clone(), fn_type_expr);
        type_env.push_identifier_types();

        typeresolve_value_expr(&mut function_definition.value_expr, type_env);
    }

    fn typeresolve_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
        match value_expr {
            ValueExpr::InlineGo(..) => {},
            ValueExpr::Lambda(lambda_expr) => {
                // typeresolve_value_expr(lambda_expr., type_env);
            },
            ValueExpr::FunctionCall { target, params } => {
                typeresolve_value_expr(target, type_env);
                params.iter_mut().for_each(|param| typeresolve_value_expr(param, type_env));
            },
            ValueExpr::Variable(identifier, type_expr_opt) => {
                let type_expr = type_env.get_identifier_type(identifier.clone())
                    .expect("Couldn't resolve type of identifier {identifier}");

                *type_expr_opt = Some(type_expr)
            },
            ValueExpr::If { condition, then, r#else } => {
                typeresolve_value_expr(condition, type_env);

                type_env.push_identifier_types();
                typeresolve_value_expr(then, type_env);
                type_env.pop_identifier_types();

                typeresolve_value_expr(r#else, type_env);
            },
            ValueExpr::While { condition, body } => {
                typeresolve_value_expr(condition, type_env);
                type_env.push_identifier_types();
                typeresolve_value_expr(body, type_env);
                type_env.pop_identifier_types();
            },
            ValueExpr::Tuple(value_exprs) => value_exprs.iter_mut().for_each(|value_expr| typeresolve_value_expr(value_expr, type_env)),
            ValueExpr::Block(value_exprs) => {
                type_env.push_identifier_types();
                value_exprs.iter_mut().for_each(|value_expr| typeresolve_value_expr(value_expr, type_env));
                type_env.pop_identifier_types();
            },
            ValueExpr::Duck(items) => items.iter_mut().for_each(|(_, value_expr)| typeresolve_value_expr(value_expr, type_env)),
            ValueExpr::Struct(items) => items.iter_mut().for_each(|(_, value_expr)| typeresolve_value_expr(value_expr, type_env)),
            ValueExpr::FieldAccess { target_obj, .. } => {
                typeresolve_value_expr(target_obj, type_env);
            },
            ValueExpr::Return(Some(value_expr)) => typeresolve_value_expr(value_expr, type_env),
            ValueExpr::VarAssign(assignment) => {
                typeresolve_value_expr(&mut assignment.value_expr, type_env);
            },
            ValueExpr::VarDecl(declaration) => {
                type_env.insert_identifier_type(declaration.name.clone(), declaration.type_expr.clone());
                if let Some(type_expr) = &mut declaration.initializer {
                    typeresolve_value_expr(type_expr, type_env);
                }
            },
            ValueExpr::Add(lhs, rhs) => {
                typeresolve_value_expr(lhs, type_env);
                typeresolve_value_expr(rhs, type_env);
            },
            ValueExpr::Mul(lhs, rhs) => {
                typeresolve_value_expr(lhs, type_env);
                typeresolve_value_expr(rhs, type_env);
            },
            ValueExpr::Equals(lhs, rhs) => {
                typeresolve_value_expr(lhs, type_env);
                typeresolve_value_expr(rhs, type_env);
            },
            ValueExpr::BoolNegate(value_expr) => {
                typeresolve_value_expr(value_expr, type_env);
            },
            ValueExpr::Int(_)
            | ValueExpr::String(_)
            | ValueExpr::Bool(_)
            | ValueExpr::Float(_)
            | ValueExpr::Char(_)
            | ValueExpr::Break
            | ValueExpr::Return(None)
            | ValueExpr::Continue => {},
        }
    }
}

impl TypeExpr {
    fn to_go_type_str(&self, type_env: &mut TypeEnv) -> String {
        return match self {
            TypeExpr::Any => "interface{}".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "rune".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(name) => (type_env.resolve_type_alias(name.clone())).to_go_type_str(type_env),
            TypeExpr::Fun(param_types, return_type) => format!(
                "func({}) {}",
                param_types
                    .iter()
                    .map(|type_expr| type_expr.to_go_type_str(type_env))
                    .collect::<Vec<_>>()
                    .join(","),
                return_type.clone().map_or("".to_string(), |return_type| return_type.to_go_type_str(type_env))
            ),
            TypeExpr::Struct(r#struct) => format!(
                "Struct{}",
                r#struct
                    .fields
                    .iter()
                    .map(|(field_name, type_expr)| format!(
                        "{field_name}_{}",
                        (*type_expr).to_go_type_str(type_env)
                    ))
                    .collect::<Vec<_>>()
                    .join("_")
            ),
            TypeExpr::Duck(duck) => {
                let mut fields = duck.fields.clone();
                fields.sort_by_key(|(field_name, _)| field_name.clone());

                format!(
                    "Duck{}",
                    fields
                        .iter()
                        .map(|(field_name, type_expr)| format!(
                            "{field_name}_{}",
                            type_expr.to_go_type_str(type_env)
                        ))
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Tuple(fields) => {
                format!(
                    "Tuple{}",
                    fields
                        .iter()
                        .map(|type_expr| format!("{}", type_expr.to_go_type_str(type_env)))
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Or(_variants) => todo!("implement variants"),
        };
    }

    pub fn from_value_expr(value_expr: &ValueExpr, type_env: &mut TypeEnv) -> TypeExpr {
        return match value_expr {
            ValueExpr::Lambda(lambda_expr) => TypeExpr::Fun(
                lambda_expr.params.iter().map(|(_, type_expr)| type_expr.clone()).collect(),
                lambda_expr.return_type.clone().map(|i| Box::new(i)),
            ),
            ValueExpr::InlineGo(..) => todo!("type for inline go"),
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Bool(..) => TypeExpr::Bool,
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(..) => TypeExpr::String,
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => TypeExpr::from_value_expr(value_expr, type_env),
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::Struct(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| {
                        (
                            field_name.to_string(),
                            TypeExpr::from_value_expr(value_expr, type_env),
                        )
                    })
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Struct(Struct { fields: types })
            }
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr, type_env))
                    .collect::<Vec<TypeExpr>>();

                TypeExpr::Tuple(types)
            }
            ValueExpr::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| {
                        (
                            field_name.to_string(),
                            TypeExpr::from_value_expr(value_expr, type_env),
                        )
                    })
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Duck(Duck { fields: types })
            }
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Addition '+' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.to_go_type_str(type_env),
                        right_type_expr.to_go_type_str(type_env)
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Multiplication '*' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.to_go_type_str(type_env),
                        right_type_expr.to_go_type_str(type_env)
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::FunctionCall {..} => {
                todo!("Return Type of Function");
            }
            ValueExpr::Block(value_exprs) => TypeExpr::from(
                value_exprs
                    .last()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr, type_env))
                    .expect("Block Expressions must be at least one expression long."),
            ),
            ValueExpr::Variable(.., type_expr) => type_expr.as_ref().expect("Expected type but didn't get one").clone(),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(&TypeExpr::from_value_expr(bool_expr, type_env), &TypeExpr::Bool, type_env);
                TypeExpr::Bool
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition, type_env);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool, type_env);

                let _then_type_expr = TypeExpr::from_value_expr(then, type_env);
                let _else_type_expr = TypeExpr::from_value_expr(r#else, type_env);

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                todo!("combine then and else, then return combined type");
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let target_obj_type_expr = TypeExpr::from_value_expr(target_obj, type_env);
                require(
                    target_obj_type_expr.is_object_like(),
                    format!(
                        "the target of a field access must be of type duck, struct or tuple. Got {}",
                        target_obj_type_expr.to_go_type_str(type_env)
                    ),
                );
                require(
                    target_obj_type_expr.has_field_by_name(field_name.clone()),
                    format!(
                        "{} doesn't have a field with name {}",
                        target_obj_type_expr.to_go_type_str(type_env),
                        field_name
                    ),
                );

                let target_field_type_expr =
                    target_obj_type_expr.typeof_field(field_name.to_string());

                target_field_type_expr
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition, type_env);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool, type_env);

                let _body_type_expr = TypeExpr::from_value_expr(body, type_env);

                return TypeExpr::Tuple(vec![]);
            }
        };
    }

    fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..) | Self::Duck(..) | Self::Struct(..) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    fn has_field(&self, field: (String, TypeExpr)) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .any(|struct_field| *struct_field == field),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field == field),
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .any(|struct_field| *struct_field.0 == name),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field.0 == name),
            _ => false,
        }
    }

    fn typeof_field(&self, field_name: String) -> TypeExpr {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .find(|struct_field| *struct_field.0 == field_name)
                .expect("Tried to access field that doesn't exist")
                .1
                .clone(),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .find(|struct_field| *struct_field.0 == field_name)
                .expect("Tried to access field that doesn't exist")
                .1
                .clone(),
            _ => panic!("Tried to access field on non object-like type."),
        }
    }

    fn is_number(&self) -> bool {
        return *self == TypeExpr::Int || *self == TypeExpr::Float;
    }
}

fn require(condition: bool, fail_message: String) {
    if !condition {
        println!("TypeError: {}", fail_message);
        process::exit(2);
    }
}

fn check_type_compatability(one: &TypeExpr, two: &TypeExpr, type_env: &mut TypeEnv) {
    if one.is_number() {
        if !two.is_number() {
            println!(
                "Types {} and {} are not compatible.",
                one.to_go_type_str(type_env),
                two.clone().to_go_type_str(type_env)
            );
            process::exit(2);
        }

        return;
    }

    if *one != *two {
        println!(
            "Types {} and {} are not compatible.",
            one.to_go_type_str(type_env),
            two.to_go_type_str(type_env)
        );
        process::exit(2);
    }
}

#[cfg(test)]
mod test {
    use crate::parse::{function_parser::FunctionDefintion, lexer::lexer, value_parser::value_expr_parser};
    use chumsky::prelude::*;

    use super::*;

    #[test]
    fn test_typechecking() {
        let src_and_expected_type_vec = vec![
            ("4 + 4", TypeExpr::Int),
            ("\"Hallo\"", TypeExpr::String),
            (
                "{ x: \"hallo\", }",
                TypeExpr::Duck(Duck {
                    fields: vec![("x".to_string(), TypeExpr::String)],
                }),
            ),
            ("0.5", TypeExpr::Float),
            ("0.1 + 0.4", TypeExpr::Float),
            ("0 + 0.4", TypeExpr::Int),
            ("0.4 + 0", TypeExpr::Float),
            (
                "(0, 2)",
                TypeExpr::Tuple(vec![TypeExpr::Int, TypeExpr::Int]),
            ),
            (
                "(0, 2 + 2)",
                TypeExpr::Tuple(vec![TypeExpr::Int, TypeExpr::Int]),
            ),
            (
                "(0, (2 + 2, 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int,
                    TypeExpr::Tuple(vec![TypeExpr::Int, TypeExpr::Int]),
                ]),
            ),
            (
                "(0, (\"Hallo, Welt\", 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int,
                    TypeExpr::Tuple(vec![TypeExpr::String, TypeExpr::Int]),
                ]),
            ),
            ("{ 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; x }", TypeExpr::Int),
            ("{ let x: Int = 5; x * x }", TypeExpr::Int),
        ];

        for (src, expected_type_expr) in src_and_expected_type_vec {
            println!("lexing {src}");
            let lexer_parse_result = lexer().parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {src}");
            let value_expr_parse_result = value_expr_parser().parse(tokens.as_slice());
            assert_eq!(value_expr_parse_result.has_errors(), false);
            assert_eq!(value_expr_parse_result.has_output(), true);

            let value_expr = value_expr_parse_result.into_output().unwrap();
            let mut source_file = SourceFile {
                function_definitions: vec![FunctionDefintion { name: "main".to_string(), params: None, return_type: None, value_expr: value_expr }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let type_expr = TypeExpr::from_value_expr(&source_file.function_definitions.get(0).unwrap().value_expr, &mut type_env);
            assert_eq!(type_expr, expected_type_expr);
        }
    }
}
