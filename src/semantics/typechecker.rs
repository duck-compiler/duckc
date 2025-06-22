#[allow(dead_code)]

use std::{collections::HashMap, process};

use crate::parse::{
    function_parser::FunctionDefintion, source_file_parser::SourceFile, type_parser::{Duck, Struct, TypeExpr}, value_parser::ValueExpr
};

#[derive(Debug, Clone)]
pub struct TypeEnv {
    identifier_types: Vec<HashMap<String, TypeExpr>>,
    type_aliases: HashMap<String, TypeExpr>,
}

impl TypeEnv {
    pub fn env_push(&mut self) {
        let cloned_hash_map = self.identifier_types.last().expect("Expect at least one env.").clone();
        self.identifier_types.push(cloned_hash_map);
    }

    pub fn env_pop(&mut self) {
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
            .insert(alias, type_expr)
            .expect("Couldn't insert type alias.");
    }

    pub fn resolve_type_alias(&mut self, alias: String) -> TypeExpr {
        self.type_aliases
            .get(&alias)
            .expect("Couldn't resolve type alias {alias}")
            .clone()
    }
}

pub fn typeresolve_source_file(source_file: &mut SourceFile) {
    let mut type_env: TypeEnv = TypeEnv { identifier_types: vec![HashMap::new()], type_aliases: HashMap::new() };

    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| type_env.insert_type_alias(type_definition.name.clone(), type_definition.type_expression.clone()));

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| typeresolve_function_definition(function_definition, &mut type_env));

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
        type_env.env_push();

        typeresolve_value_expr(&mut function_definition.value_expr, type_env);
    }

    fn typeresolve_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
        match value_expr {
            ValueExpr::InlineGo(..) => {},
            ValueExpr::Lambda(..) => todo!("lambda expressions"),
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

                type_env.env_push();
                typeresolve_value_expr(then, type_env);
                type_env.env_pop();

                typeresolve_value_expr(r#else, type_env);
            },
            ValueExpr::While { condition, body } => {
                typeresolve_value_expr(condition, type_env);
                type_env.env_push();
                typeresolve_value_expr(body, type_env);
                type_env.env_pop();
            },
            ValueExpr::Tuple(value_exprs) => value_exprs.iter_mut().for_each(|value_expr| typeresolve_value_expr(value_expr, type_env)),
            ValueExpr::Block(value_exprs) => {
                type_env.env_push();
                value_exprs.iter_mut().for_each(|value_expr| typeresolve_value_expr(value_expr, type_env));
                type_env.env_pop();
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
    fn to_ref_string(&self) -> String {
        return match self {
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "char".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(name) => name.clone(),
            TypeExpr::Fun(param_types, return_type) => todo!(),
            TypeExpr::Struct(r#struct) => format!(
                "Struct{}",
                r#struct
                    .fields
                    .iter()
                    .map(|(field_name, type_expr)| format!(
                        "{field_name}_{}",
                        (*type_expr).to_ref_string()
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
                            type_expr.to_ref_string()
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
                        .map(|type_expr| format!("{}", type_expr.to_ref_string()))
                        .collect::<Vec<_>>()
                        .join("_")
                )
            }
            TypeExpr::Or(variants) => todo!("implement variants"),
        };
    }

    pub fn from_value_expr(value_expr: &ValueExpr) -> TypeExpr {
        return match value_expr {
            ValueExpr::Lambda(..) => todo!("type for lambda"),
            ValueExpr::InlineGo(..) => todo!("type for inline go"),
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Bool(..) => TypeExpr::Bool,
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(..) => TypeExpr::String,
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => TypeExpr::from_value_expr(value_expr),
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::Struct(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| {
                        (
                            field_name.to_string(),
                            TypeExpr::from_value_expr(value_expr),
                        )
                    })
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Struct(Struct { fields: types })
            }
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr))
                    .collect::<Vec<TypeExpr>>();

                TypeExpr::Tuple(types)
            }
            ValueExpr::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| {
                        (
                            field_name.to_string(),
                            TypeExpr::from_value_expr(value_expr),
                        )
                    })
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Duck(Duck { fields: types })
            }
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Addition '+' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.to_ref_string(),
                        right_type_expr.to_ref_string()
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            }
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            }
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Multiplication '*' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.to_ref_string(),
                        right_type_expr.to_ref_string()
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            }
            ValueExpr::FunctionCall { target, params } => {
                todo!("Return Type of Function");
            }
            ValueExpr::Block(value_exprs) => TypeExpr::from(
                value_exprs
                    .last()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr))
                    .expect("Block Expressions must be at least one expression long."),
            ),
            ValueExpr::Variable(identifier, type_expr) => type_expr.as_ref().expect("Expected type but didn't get one").clone(),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(&TypeExpr::from_value_expr(bool_expr), &TypeExpr::Bool);
                TypeExpr::Bool
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool);

                let then_type_expr = TypeExpr::from_value_expr(then);
                let else_type_expr = TypeExpr::from_value_expr(r#else);

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                todo!("combine then and else, then return combined type");
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let target_obj_type_expr = TypeExpr::from_value_expr(target_obj);
                require(
                    target_obj_type_expr.is_object_like(),
                    format!(
                        "the target of a field access must be of type duck, struct or tuple. Got {}",
                        target_obj_type_expr.to_ref_string()
                    ),
                );
                require(
                    target_obj_type_expr.has_field_by_name(field_name.clone()),
                    format!(
                        "{} doesn't have a field with name {}",
                        target_obj_type_expr.to_ref_string(),
                        field_name
                    ),
                );

                let target_field_type_expr =
                    target_obj_type_expr.typeof_field(field_name.to_string());

                target_field_type_expr
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool);

                let body_type_expr = TypeExpr::from_value_expr(body);

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

fn check_type_compatability(one: &TypeExpr, two: &TypeExpr) {
    if one.is_number() {
        if !two.is_number() {
            println!(
                "Types {} and {} are not compatible.",
                one.to_ref_string(),
                two.clone().to_ref_string()
            );
            process::exit(2);
        }

        return;
    }

    if *one != *two {
        println!(
            "Types {} and {} are not compatible.",
            one.to_ref_string(),
            two.to_ref_string()
        );
        process::exit(2);
    }
}

#[cfg(test)]
mod test {
    use crate::parse::{lexer::lexer, value_parser::value_expr_parser};
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
                use_statements: vec![],
                function_definitions: vec![FunctionDefintion { name: "main".to_string(), params: None, return_type: None, value_expr: value_expr }],
                type_definitions: vec![],
            };

            typeresolve_source_file(&mut source_file);

            let type_expr = TypeExpr::from_value_expr(&source_file.function_definitions.get(0).unwrap().value_expr);
            assert_eq!(type_expr, expected_type_expr);
        }
    }
}
