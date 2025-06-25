use std::{collections::HashMap, process};

use crate::parse::{
    Spanned,
    function_parser::FunctionDefintion,
    source_file_parser::SourceFile,
    type_parser::{Duck, Field, Struct, TypeExpr},
    value_parser::ValueExpr,
};

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub identifier_types: Vec<HashMap<String, TypeExpr>>,
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub all_types: Vec<TypeExpr>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        let mut identifier_type_map = HashMap::<String, TypeExpr>::new();

        identifier_type_map.insert(
            "println".to_string(),
            TypeExpr::Fun(vec![(None, TypeExpr::String.into_empty_span())], None),
        );

        let identifier_types = vec![identifier_type_map];

        Self {
            identifier_types,
            type_aliases: vec![HashMap::new()],
            all_types: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub enum GoTypeDefinition {
    TypaAlias {
        name: String,
        target: String,
    },
    Struct {
        name: String,
        fields: Vec<(String, String)>,
        methods: Vec<String>,
    },
    Interface {
        name: String,
        methods: Vec<String>,
    },
}

#[derive(Clone, Debug)]
pub struct TypesSummary {
    // these are primitive types filtered
    pub types_used: Vec<TypeExpr>,
    pub param_names_used: Vec<String>,
}

impl TypeEnv {
    pub fn push_type_aliases(&mut self) {
        let cloned_hash_map = self
            .type_aliases
            .last()
            .expect("Expect at least one env.")
            .clone();
        self.type_aliases.push(cloned_hash_map);
    }

    pub fn pop_type_aliases(&mut self) {
        self.identifier_types.pop();
    }

    pub fn push_identifier_types(&mut self) {
        let cloned_hash_map = self
            .identifier_types
            .last()
            .expect("Expect at least one env.")
            .clone();
        self.identifier_types.push(cloned_hash_map);
    }

    pub fn pop_identifier_types(&mut self) {
        self.identifier_types.pop();
    }

    pub fn insert_identifier_type(&mut self, identifier: String, type_expr: TypeExpr) {
        self.insert_type(type_expr.clone());
        self.identifier_types
            .last_mut()
            .expect("At least one env should exist. :(")
            .insert(identifier, type_expr);
    }

    pub fn insert_type(&mut self, type_expr: TypeExpr) {
        self.all_types.push(type_expr);
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

    pub fn resolve_type_alias(&mut self, alias: &String) -> TypeExpr {
        let type_aliases = self
            .type_aliases
            .last()
            .expect("At least one type aliases hashmap should exist. :(");

        println!("Try to resolve type alias {alias}");
        type_aliases
            .get(alias)
            .expect("Couldn't resolve type alias {alias}")
            .clone()
    }

    fn flatten_types(
        &mut self,
        type_expr: &mut TypeExpr,
        param_names_used: &mut Vec<String>,
    ) -> Vec<TypeExpr> {
        let mut found = vec![];

        match type_expr {
            TypeExpr::Duck(duck) => duck.fields.iter_mut().for_each(|field| {
                param_names_used.push(field.name.clone());
                if !field.type_expr.0.is_object_like() {
                    found.push(field.type_expr.0.clone());
                    return;
                }

                let mut clone = field.type_expr.0.clone();
                let mut flattens_from_clone = self.flatten_types(&mut clone, param_names_used);

                found.push(clone);
                found.append(&mut flattens_from_clone);

                field.type_expr = (
                    TypeExpr::TypeName(field.type_expr.0.as_clean_go_type_name(self)),
                    field.type_expr.1.clone(),
                );
            }),
            TypeExpr::Struct(duck) => duck.fields.iter_mut().for_each(|field| {
                param_names_used.push(field.name.clone());
                if !field.type_expr.0.is_object_like() {
                    found.push(field.type_expr.0.clone());
                    return;
                }

                let mut clone = field.type_expr.0.clone();
                let mut flattens_from_clone = self.flatten_types(&mut clone, param_names_used);

                found.push(clone);
                found.append(&mut flattens_from_clone);

                field.type_expr = (
                    TypeExpr::TypeName(field.type_expr.0.as_clean_go_type_name(self)),
                    field.type_expr.1.clone(),
                );
            }),
            TypeExpr::Tuple(types) => types.iter_mut().for_each(|type_expr| {
                found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
            }),
            TypeExpr::Fun(params, return_type) => {
                params.iter_mut().for_each(|param| {
                    found.extend(self.flatten_types(&mut param.1.0, param_names_used))
                });

                if let Some(type_expr) = return_type.as_mut() {
                    found.extend(self.flatten_types(&mut (*type_expr).0, param_names_used));
                }
            }
            _ => {
                found.push(type_expr.clone());
            }
        }

        found
    }

    pub fn summarize(&mut self) -> TypesSummary {
        let mut all_types = self.all_types.clone();
        let mut param_names_used = Vec::new();

        let mut to_push = Vec::new();
        all_types.iter_mut().for_each(|type_expr| {
            to_push.append(&mut self.flatten_types(type_expr, &mut param_names_used));
        });

        all_types.append(&mut to_push);
        all_types.sort_by_key(|type_expr| type_expr.as_clean_go_type_name(self));
        all_types.dedup_by_key(|type_expr| dbg!(type_expr.as_clean_go_type_name(self)));

        let all_types = dbg!(all_types);

        let mut param_names_used = dbg!(param_names_used);
        param_names_used.dedup();

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| {
            type_env.insert_type_alias(
                type_definition.name.clone(),
                type_definition.type_expression.0.clone(),
            )
        });

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            typeresolve_function_definition(function_definition, type_env)
        });

    type_env.pop_type_aliases();

    fn typeresolve_function_definition(
        function_definition: &mut FunctionDefintion,
        type_env: &mut TypeEnv,
    ) {
        let fn_type_expr = TypeExpr::Fun(
            function_definition
                .params
                .as_ref()
                .unwrap_or(&Vec::new())
                .iter()
                .map(|(identifier, type_expr)| {
                    type_env.insert_identifier_type(identifier.clone(), type_expr.0.clone());
                    (Some(identifier.clone()), type_expr.clone())
                })
                .collect::<Vec<_>>(),
            function_definition
                .return_type
                .as_ref()
                .map(|x| Box::new(x.clone())),
        );

        type_env.insert_identifier_type(function_definition.name.clone(), fn_type_expr);
        type_env.push_identifier_types();

        typeresolve_value_expr(&mut function_definition.value_expr.0, type_env);
    }

    fn typeresolve_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
        match value_expr {
            ValueExpr::InlineGo(..) => {}
            ValueExpr::Lambda(_lambda_expr) => {
                // typeresolve_value_expr(lambda_expr., type_env);
            }
            ValueExpr::FunctionCall { target, params } => {
                typeresolve_value_expr(&mut target.0, type_env);
                params
                    .iter_mut()
                    .for_each(|param| typeresolve_value_expr(&mut param.0, type_env));
            }
            ValueExpr::Variable(identifier, type_expr_opt) => {
                println!("try resolving identifier {identifier}");
                let type_expr = type_env
                    .get_identifier_type(identifier.clone())
                    .expect("Couldn't resolve type of identifier {identifier}");

                *type_expr_opt = Some(type_expr)
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                typeresolve_value_expr(&mut condition.0, type_env);

                type_env.push_identifier_types();
                typeresolve_value_expr(&mut then.0, type_env);
                type_env.pop_identifier_types();

                if let Some(r#else) = r#else {
                    typeresolve_value_expr(&mut r#else.0, type_env);
                }
            }
            ValueExpr::While { condition, body } => {
                typeresolve_value_expr(&mut condition.0, type_env);
                type_env.push_identifier_types();
                typeresolve_value_expr(&mut body.0, type_env);
                type_env.pop_identifier_types();
            }
            ValueExpr::Tuple(value_exprs) => value_exprs
                .iter_mut()
                .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env)),
            ValueExpr::Block(value_exprs) => {
                type_env.push_identifier_types();
                value_exprs
                    .iter_mut()
                    .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env));
                type_env.pop_identifier_types();
            }
            ValueExpr::Duck(items) => items
                .iter_mut()
                .for_each(|(_, value_expr)| typeresolve_value_expr(&mut value_expr.0, type_env)),
            ValueExpr::Struct(items) => items
                .iter_mut()
                .for_each(|(_, value_expr)| typeresolve_value_expr(&mut value_expr.0, type_env)),
            ValueExpr::FieldAccess { target_obj, .. } => {
                typeresolve_value_expr(&mut target_obj.0, type_env);
            }
            ValueExpr::Return(Some(value_expr)) => {
                typeresolve_value_expr(&mut value_expr.0, type_env)
            }
            ValueExpr::VarAssign(assignment) => {
                typeresolve_value_expr(&mut assignment.0.value_expr.0, type_env);
            }
            ValueExpr::VarDecl(declaration) => {
                let declaration = &mut declaration.0;

                if let (TypeExpr::TypeName(type_name), span) = &declaration.type_expr {
                    let type_expr = type_env.resolve_type_alias(type_name);
                    // mutate
                    declaration.type_expr = (type_expr, span.clone());
                }

                type_env.insert_identifier_type(
                    declaration.name.clone(),
                    declaration.type_expr.0.clone(),
                );

                if let Some(type_expr) = &mut declaration.initializer {
                    typeresolve_value_expr(&mut type_expr.0, type_env);
                }
            }
            ValueExpr::Add(lhs, rhs) => {
                typeresolve_value_expr(&mut lhs.0, type_env);
                typeresolve_value_expr(&mut rhs.0, type_env);
            }
            ValueExpr::Mul(lhs, rhs) => {
                typeresolve_value_expr(&mut lhs.0, type_env);
                typeresolve_value_expr(&mut rhs.0, type_env);
            }
            ValueExpr::Equals(lhs, rhs) => {
                typeresolve_value_expr(&mut lhs.0, type_env);
                typeresolve_value_expr(&mut rhs.0, type_env);
            }
            ValueExpr::BoolNegate(value_expr) => {
                typeresolve_value_expr(&mut value_expr.0, type_env);
            }
            ValueExpr::String(..) => type_env.insert_type(TypeExpr::String),
            ValueExpr::Bool(..) => type_env.insert_type(TypeExpr::Bool),
            ValueExpr::Float(..) => type_env.insert_type(TypeExpr::Float),
            ValueExpr::Char(..) => type_env.insert_type(TypeExpr::Char),
            ValueExpr::Int(..) => type_env.insert_type(TypeExpr::Int), // this is so that only the used primitive types are in the type env
            ValueExpr::Break | ValueExpr::Return(None) | ValueExpr::Continue => {}
        }
    }
}

impl TypeExpr {
    pub fn from_value_expr(value_expr: &ValueExpr, type_env: &mut TypeEnv) -> TypeExpr {
        return match value_expr {
            ValueExpr::Lambda(lambda_expr) => TypeExpr::Fun(
                lambda_expr
                    .params
                    .iter()
                    .map(|(name, type_expr)| (Some(name.clone()), type_expr.clone()))
                    .collect(),
                lambda_expr.return_type.clone().map(Box::new),
            ),
            ValueExpr::InlineGo(..) => todo!("type for inline go"),
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Bool(..) => TypeExpr::Bool,
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(..) => TypeExpr::String,
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => {
                TypeExpr::from_value_expr(&value_expr.0, type_env)
            }
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::Struct(fields) => {
                let types = fields
                    .iter()
                    .map(|field| {
                        Field::new(
                            field.0.to_string(),
                            TypeExpr::from_value_expr(value_expr, type_env).into_empty_span(),
                        )
                    })
                    .collect::<Vec<Field>>();

                TypeExpr::Struct(Struct { fields: types })
            }
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .iter()
                    .map(|value_expr| {
                        TypeExpr::from_value_expr(&value_expr.0, type_env).into_empty_span()
                    })
                    .collect::<Vec<Spanned<TypeExpr>>>();

                TypeExpr::Tuple(types)
            }
            ValueExpr::Duck(fields) => {
                let types = fields
                    .iter()
                    .map(|(name, value_expr)| {
                        Field::new(
                            name.to_string(),
                            TypeExpr::from_value_expr(&value_expr.0, type_env).into_empty_span(),
                        )
                    })
                    .collect::<Vec<Field>>();

                TypeExpr::Duck(Duck { fields: types })
            }
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Addition '+' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

                require(
                    left_type_expr.is_number(),
                    format!(
                        "Multiplication '*' is only allowed for numbers. You've used {} + {}.",
                        left_type_expr.as_go_type_annotation(type_env),
                        right_type_expr.as_go_type_annotation(type_env)
                    ),
                );
                check_type_compatability(&left_type_expr, &right_type_expr, type_env);

                left_type_expr
            }
            ValueExpr::FunctionCall { .. } => {
                todo!("Return Type of Function");
            }
            ValueExpr::Block(value_exprs) => value_exprs
                .last()
                .map(|value_expr| TypeExpr::from_value_expr(&value_expr.0, type_env))
                .expect("Block Expressions must be at least one expression long."),

            ValueExpr::Variable(.., type_expr) => type_expr
                .as_ref()
                .expect("Expected type but didn't get one")
                .clone(),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(
                    &TypeExpr::from_value_expr(&bool_expr.0, type_env),
                    &TypeExpr::Bool,
                    type_env,
                );
                TypeExpr::Bool
            }
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                let condition_type_expr = TypeExpr::from_value_expr(&condition.0, type_env);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool, type_env);

                let _then_type_expr = TypeExpr::from_value_expr(&then.0, type_env);
                if let Some(r#else) = r#else {
                    let _else_type_expr = TypeExpr::from_value_expr(&r#else.0, type_env);
                }

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                todo!("combine then and else, then return combined type");
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                let target_obj_type_expr = TypeExpr::from_value_expr(&target_obj.0, type_env);
                require(
                    target_obj_type_expr.is_object_like(),
                    format!(
                        "the target of a field access must be of type duck, struct or tuple. Got {}",
                        target_obj_type_expr.as_go_type_annotation(type_env)
                    ),
                );
                require(
                    target_obj_type_expr.has_field_by_name(field_name.clone()),
                    format!(
                        "{} doesn't have a field with name {}",
                        target_obj_type_expr.as_go_type_annotation(type_env),
                        field_name
                    ),
                );

                target_obj_type_expr.typeof_field(field_name.to_string())
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(&condition.0, type_env);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool, type_env);

                let _body_type_expr = TypeExpr::from_value_expr(&body.0, type_env);

                return TypeExpr::Tuple(vec![]);
            }
        };
    }

    pub fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..) | Self::Duck(..) | Self::Struct(..) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn has_field(&self, field: Field) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct.fields.contains(&field),
            Self::Duck(duck) => duck.fields.contains(&field),
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .any(|struct_field| *struct_field.name == name),
            _ => false,
        }
    }

    fn typeof_field(&self, field_name: String) -> TypeExpr {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct
                .fields
                .iter()
                .find(|struct_field| *struct_field.name == field_name)
                .expect("Tried to access field that doesn't exist")
                .type_expr
                .0
                .clone(),
            Self::Duck(duck) => duck
                .fields
                .iter()
                .find(|struct_field| *struct_field.name == field_name)
                .expect("Tried to access field that doesn't exist")
                .type_expr
                .0
                .clone(),
            _ => panic!("Tried to access field on non object-like type."),
        }
    }

    pub fn is_number(&self) -> bool {
        return *self == TypeExpr::Int || *self == TypeExpr::Float;
    }
}

fn require(condition: bool, fail_message: String) {
    if !condition {
        println!("TypeError: {fail_message}");
        process::exit(2);
    }
}

fn check_type_compatability(one: &TypeExpr, two: &TypeExpr, type_env: &mut TypeEnv) {
    if one.is_number() {
        if !two.is_number() {
            println!(
                "Types {} and {} are not compatible.",
                one.as_go_type_annotation(type_env),
                two.clone().as_go_type_annotation(type_env)
            );
            process::exit(2);
        }

        return;
    }

    if *one != *two {
        println!(
            "Types {} and {} are not compatible.",
            one.as_go_type_annotation(type_env),
            two.as_go_type_annotation(type_env)
        );
        process::exit(2);
    }
}

#[cfg(test)]
mod test {
    use crate::parse::{
        function_parser::FunctionDefintion,
        lexer::lexer,
        make_input,
        type_parser::Field,
        value_parser::{empty_range, value_expr_parser},
    };
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
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String.into_empty_span(),
                    )],
                }),
            ),
            ("0.5", TypeExpr::Float),
            ("0.1 + 0.4", TypeExpr::Float),
            ("0 + 0.4", TypeExpr::Int),
            ("0.4 + 0", TypeExpr::Float),
            (
                "(0, 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Int.into_empty_span(),
                ]),
            ),
            (
                "(0, 2 + 2)",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Int.into_empty_span(),
                ]),
            ),
            (
                "(0, (2 + 2, 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int.into_empty_span(),
                        TypeExpr::Int.into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            (
                "(0, (\"Hallo, Welt\", 5))",
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::Tuple(vec![
                        TypeExpr::String.into_empty_span(),
                        TypeExpr::Int.into_empty_span(),
                    ])
                    .into_empty_span(),
                ]),
            ),
            ("{ 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; x }", TypeExpr::Int),
            ("{ let x: Int = 5; x * x }", TypeExpr::Int),
        ];

        for (src, expected_type_expr) in src_and_expected_type_vec {
            println!("lexing {src}");
            let lexer_parse_result = lexer("test").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {src}");
            let value_expr_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(value_expr_parse_result.has_errors(), false);
            assert_eq!(value_expr_parse_result.has_output(), true);

            let value_expr = value_expr_parse_result.into_output().unwrap();
            let mut source_file = SourceFile {
                function_definitions: vec![FunctionDefintion {
                    name: "main".to_string(),
                    params: None,
                    return_type: None,
                    value_expr: value_expr,
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let type_expr = TypeExpr::from_value_expr(
                &source_file
                    .function_definitions
                    .get(0)
                    .unwrap()
                    .value_expr
                    .0,
                &mut type_env,
            );
            assert_eq!(type_expr, expected_type_expr);
        }
    }

    #[test]
    fn test_type_summary() {
        // the summary always holds the type of main function
        let src_and_summary_check_funs: Vec<(&str, Box<dyn FnOnce(&TypesSummary)>)> = vec![
            (
                "{ let y: { x: String, y: Int } = \"Hallo\"; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 4);
                }),
            ),
            (
                "{ let y: { x: String, y: Int, a: { b: { c: { d: { e: String }}}} } = \"Hallo\"; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 8);
                }),
            ),
            (
                "{ let x: Int = 5; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 2);
                }),
            ),
            (
                "{ let x: { a: Char, b: Char }; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 3);
                }),
            ),
            (
                "{ let y: { x: { y: Int } } = 4; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 4);
                }),
            ),
            (
                "{ let y: { x: Int } = 4; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 3);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int } } = 4; }",
                Box::new(|summary: &TypesSummary| {
                    dbg!(summary);
                    assert_eq!(summary.types_used.len(), 5);
                }),
            ),
        ];

        for (src, summary_check_fun) in src_and_summary_check_funs {
            let lexer_parse_result = lexer("test").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false, "Couldn't lex {src}");
            assert_eq!(lexer_parse_result.has_output(), true, "Couldn't lex {src}");

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            let value_expr_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(
                value_expr_parse_result.has_errors(),
                false,
                "Couldn't parse value expr {src}"
            );
            assert_eq!(
                value_expr_parse_result.has_output(),
                true,
                "Couldn't parse value expr {src}"
            );

            let value_expr = value_expr_parse_result.into_output().unwrap();
            let mut source_file = SourceFile {
                function_definitions: vec![FunctionDefintion {
                    name: "main".to_string(),
                    params: None,
                    return_type: None,
                    value_expr: value_expr,
                }],
                ..Default::default()
            };

            let mut type_env = TypeEnv::default();
            typeresolve_source_file(&mut source_file, &mut type_env);

            let summary = type_env.summarize();

            summary_check_fun(&summary);
        }
    }
}
