use std::{collections::HashMap, process};

use crate::parse::{
    SS, Spanned, failure,
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
            TypeExpr::Fun(vec![(None, TypeExpr::Any.into_empty_span())], None),
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

    pub fn insert_type(&mut self, type_expr: TypeExpr) -> TypeExpr {
        if let TypeExpr::TypeName(_, ident) = &type_expr {
            let resolved = self.resolve_type_alias(ident);
            self.all_types.push(resolved.clone());
            resolved
        } else {
            self.all_types.push(type_expr.clone());
            type_expr
        }
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
            .unwrap_or_else(|| panic!("Couldn't resolve type alias {alias}"))
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

                let mut type_expr = field.type_expr.0.clone();
                let mut flattened_types_from_type_expr =
                    self.flatten_types(&mut type_expr, param_names_used);

                found.push(type_expr.clone());
                found.append(&mut flattened_types_from_type_expr);

                if type_expr.is_object_like() {
                    let type_name = type_expr.as_clean_go_type_name(self);
                    let is_duck = matches!(type_expr, TypeExpr::Duck(_));
                    field.type_expr = (
                        if is_duck {
                            type_expr
                        } else {
                            TypeExpr::TypeNameInternal(type_name)
                        },
                        field.type_expr.1,
                    );
                }
            }),
            TypeExpr::Struct(duck) => duck.fields.iter_mut().for_each(|field| {
                param_names_used.push(field.name.clone());
                if !field.type_expr.0.is_object_like() {
                    found.push(field.type_expr.0.clone());
                    return;
                }

                let mut type_expr = field.type_expr.0.clone();
                let mut flattened_types_from_type_expr =
                    self.flatten_types(&mut type_expr, param_names_used);

                found.push(type_expr.clone());
                found.append(&mut flattened_types_from_type_expr);

                if type_expr.is_object_like() {
                    let type_name = type_expr.as_clean_go_type_name(self);
                    let is_duck = matches!(type_expr, TypeExpr::Duck(_));
                    field.type_expr = (
                        if is_duck {
                            type_expr
                        } else {
                            TypeExpr::TypeNameInternal(type_name)
                        },
                        field.type_expr.1,
                    );
                }
            }),
            TypeExpr::Tuple(types) => types.iter_mut().for_each(|type_expr| {
                found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
            }),
            TypeExpr::Fun(params, return_type) => {
                params.iter_mut().for_each(|param| {
                    found.extend(self.flatten_types(&mut param.1.0, param_names_used))
                });

                if let Some(type_expr) = return_type.as_mut() {
                    found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
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
        all_types.dedup_by_key(|type_expr| type_expr.as_clean_go_type_name(self));

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
                    .map(|spanned_type_expr| {
                        Box::new((
                            type_env.insert_type(spanned_type_expr.0.clone()),
                            spanned_type_expr.1,
                        ))
                    }),
            );
            type_env.insert_identifier_type(function_definition.name.clone(), fn_type_expr);
        });

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            typeresolve_function_definition(function_definition, type_env);

            let function_definition = function_definition;
            let explicit_return_type = function_definition.return_type.clone()
                .unwrap_or_else(|| TypeExpr::Tuple(vec![]).into_empty_span());

            let implicit_return_type = resolve_implicit_function_return_type(&function_definition, type_env).unwrap();

            check_type_compatability(&explicit_return_type, &implicit_return_type.into_empty_span(), type_env);
        });

    // mvmo - 03.07.2025: is this required? If not we should remove the type aliases stack at all
    // type_env.pop_type_aliases();

    fn typeresolve_function_definition(
        function_definition: &mut FunctionDefintion,
        type_env: &mut TypeEnv,
    ) {
        if function_definition.name == "main" {
            let valid_return_type = match function_definition
                .return_type
                .clone()
                .map(|return_type| return_type.0)
            {
                Some(TypeExpr::Int) => true,
                Some(TypeExpr::Tuple(types)) if types.is_empty() => true,
                None => true,
                _ => false,
            };

            if !valid_return_type {
                let span = function_definition.return_type.as_ref().unwrap().1;
                failure(
                    function_definition.value_expr.1.context.file_name,
                    "Tried to return non-int value from main function".to_string(),
                    (
                        "This is the type you've declared the main function to return".to_string(),
                        span,
                    ),
                    vec![(
                        "The main function can only return either Nothing or Int".to_string(),
                        function_definition.value_expr.1,
                    )],
                    function_definition.value_expr.1.context.file_contents,
                )
            }
        }

        if let Some((return_type, _)) = &mut function_definition.return_type {
            *return_type = type_env.insert_type(return_type.clone());
        }

        type_env.push_identifier_types();
        typeresolve_value_expr(&mut function_definition.value_expr.0, type_env);
        type_env.pop_identifier_types();
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
            ValueExpr::Variable(_, identifier, type_expr_opt) => {
                println!("try resolving identifier {identifier}");
                let type_expr = type_env
                    .get_identifier_type(identifier.clone())
                    .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {identifier}"));

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
            ValueExpr::Tuple(value_exprs) => {
                value_exprs
                    .iter_mut()
                    .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env));
                let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
                type_env.insert_type(ty);
            }
            ValueExpr::Block(value_exprs) => {
                type_env.push_identifier_types();
                value_exprs
                    .iter_mut()
                    .for_each(|value_expr| typeresolve_value_expr(&mut value_expr.0, type_env));
                type_env.pop_identifier_types();
            }
            ValueExpr::Duck(items) => {
                items.iter_mut().for_each(|(_, value_expr)| {
                    typeresolve_value_expr(&mut value_expr.0, type_env)
                });
                let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
                type_env.insert_type(ty);
            }
            ValueExpr::Struct(items) => {
                items.iter_mut().for_each(|(_, value_expr)| {
                    typeresolve_value_expr(&mut value_expr.0, type_env)
                });
                let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
                type_env.insert_type(ty);
            }
            ValueExpr::FieldAccess { target_obj, .. } => {
                typeresolve_value_expr(&mut target_obj.0, type_env);
            }
            ValueExpr::Return(Some(value_expr)) => {
                typeresolve_value_expr(&mut value_expr.0, type_env)
            }
            ValueExpr::VarAssign(assignment) => {
                typeresolve_value_expr(&mut assignment.0.target.0, type_env);
                typeresolve_value_expr(&mut assignment.0.value_expr.0, type_env);
            }
            ValueExpr::VarDecl(declaration) => {
                let declaration = &mut declaration.0;

                if let (TypeExpr::TypeName(_, type_name), span) = &declaration.type_expr {
                    let type_expr = type_env.resolve_type_alias(type_name);
                    // mutate
                    declaration.type_expr = (type_expr, *span);
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
            ValueExpr::String(..) => {
                type_env.insert_type(TypeExpr::String);
            }
            ValueExpr::Bool(..) => {
                type_env.insert_type(TypeExpr::Bool);
            }
            ValueExpr::Float(..) => {
                type_env.insert_type(TypeExpr::Float);
            }
            ValueExpr::Char(..) => {
                type_env.insert_type(TypeExpr::Char);
            }
            ValueExpr::Int(..) => {
                type_env.insert_type(TypeExpr::Int);
            } // this is so that only the used primitive types are in the type env
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
            ValueExpr::InlineGo(..) => TypeExpr::InlineGo,
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
            ValueExpr::VarAssign(_assignment) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(decl) => {
                let decl = decl.as_ref();
                if let Some(init) = &decl.0.initializer {
                    check_type_compatability(
                        &decl.0.type_expr,
                        &(TypeExpr::from_value_expr(&init.0, type_env), init.1),
                        type_env,
                    );
                }

                TypeExpr::Tuple(vec![])
            }
            ValueExpr::Struct(fields) => {
                let types = fields
                    .iter()
                    .map(|(name, (value_expr, _))| {
                        Field::new(
                            name.to_string(),
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

                check_type_compatability(
                    &(left_type_expr.clone(), left.as_ref().1),
                    &(right_type_expr, right.as_ref().1),
                    type_env,
                );

                left_type_expr
            }
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(&left.0, type_env);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(&right.0, type_env);

                check_type_compatability(
                    &(left_type_expr.clone(), left.1),
                    &(right_type_expr, right.1),
                    type_env,
                );

                TypeExpr::Bool
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

                check_type_compatability(
                    &(left_type_expr.clone(), left.1),
                    &(right_type_expr, right.1),
                    type_env,
                );

                left_type_expr
            }
            ValueExpr::FunctionCall { target, params } => {
                let in_param_types = params
                    .iter()
                    .map(|param| (TypeExpr::from_value_expr(&param.0, type_env), param.1))
                    .collect::<Vec<_>>();

                let target_type = TypeExpr::from_value_expr(&target.as_ref().0, type_env);
                if let TypeExpr::Fun(param_types, return_type) = target_type {
                    param_types
                        .iter()
                        .enumerate()
                        .for_each(|(index, param_type)| {
                            if matches!(param_type.1.0, TypeExpr::Any) {
                                return;
                            }

                            check_type_compatability(
                                in_param_types.get(index).unwrap(),
                                &param_type.1,
                                type_env,
                            )
                        });

                    return return_type.map_or(TypeExpr::Tuple(vec![]), |x| x.as_ref().0.clone());
                }

                failure(
                    target.as_ref().1.context.file_name,
                    "Tried to invoke a non-function value".to_string(),
                    (
                        "This is the value you tried to invoke as a function.".to_string(),
                        target.as_ref().1,
                    ),
                    vec![
                        (
                            format!(
                                "the thing you tried to invoke is of type {}",
                                TypeExpr::from_value_expr(&target.as_ref().0, type_env)
                                    .as_clean_go_type_name(type_env)
                            ),
                            target.as_ref().1,
                        ),
                        (
                            format!(
                                "{} cannot be called as it's not of type function!",
                                TypeExpr::from_value_expr(&target.as_ref().0, type_env)
                                    .as_clean_go_type_name(type_env)
                            ),
                            target.as_ref().1,
                        ),
                    ],
                    target.as_ref().1.context.file_contents,
                )
            }
            ValueExpr::Block(value_exprs) => {
                let mut ty = TypeExpr::Tuple(vec![]);
                value_exprs.iter().for_each(|value_expr| {
                    ty = TypeExpr::from_value_expr(&value_expr.0, type_env);
                });

                // TODO: add correct return type of block
                // 26.06.2025: Return type of last expression as type of block?

                return ty;
            }
            ValueExpr::Variable(_, ident, type_expr) => type_expr
                .as_ref()
                .cloned()
                .or(type_env.get_identifier_type(ident.clone()))
                .expect("Expected type but didn't get one")
                .clone(),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(
                    &(
                        TypeExpr::from_value_expr(&bool_expr.0, type_env),
                        bool_expr.1,
                    ),
                    &TypeExpr::Bool.into_empty_span(),
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
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool.into_empty_span(),
                    type_env,
                );

                let _then_type_expr = TypeExpr::from_value_expr(&then.0, type_env);
                if let Some(r#else) = r#else {
                    let _else_type_expr = TypeExpr::from_value_expr(&r#else.0, type_env);
                }

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                // todo!("combine then and else, then return combined type");

                _then_type_expr
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
                        "{:?} {} doesn't have a field with name {}",
                        &target_obj_type_expr,
                        target_obj_type_expr.as_go_type_annotation(type_env),
                        field_name
                    ),
                );

                target_obj_type_expr.typeof_field(field_name.to_string())
            }
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(&condition.0, type_env);
                check_type_compatability(
                    &(condition_type_expr, condition.1),
                    &TypeExpr::Bool.into_empty_span(),
                    type_env,
                );

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
            Self::Tuple(fields) => fields.len() > field.name.parse::<usize>().unwrap(),
            Self::Struct(r#struct) => r#struct.fields.contains(&field),
            Self::Duck(duck) => duck.fields.contains(&field),
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String) -> bool {
        match self {
            Self::Tuple(fields) => fields.len() > name.parse::<usize>().unwrap(),
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
            Self::Tuple(fields) => fields[field_name.parse::<usize>().unwrap()].0.clone(),
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

#[derive(Debug)]
enum TypeCheckErrKind {
    TypeResolve
}

fn resolve_implicit_function_return_type(
    fun_def: &FunctionDefintion,
    type_env: &mut TypeEnv
) -> Result<TypeExpr, String> {
    // check against annotated return type
    fn flatten_returns(value_expr: &ValueExpr, return_types_found: &mut Vec<TypeExpr>, type_env: &mut TypeEnv) {
        let value_expr = value_expr;
        match value_expr {
            ValueExpr::FunctionCall {..}
            | ValueExpr::Int(..)
            | ValueExpr::InlineGo(..)
            | ValueExpr::String(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Float(..)
            | ValueExpr::Char(..)
            | ValueExpr::Tuple(..)
            | ValueExpr::Break
            | ValueExpr::Continue
            | ValueExpr::Duck(..)
            | ValueExpr::Struct(..)
            | ValueExpr::FieldAccess { .. }
            | ValueExpr::Lambda(..)
            | ValueExpr::Variable(..) => {}
            ValueExpr::If { condition, then, r#else } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&then.as_ref().0, return_types_found, type_env);
                r#else.as_ref().inspect(|r#else| {
                    flatten_returns(&r#else.as_ref().0, return_types_found, type_env);
                });
            },
            ValueExpr::While { condition, body } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&body.as_ref().0, return_types_found, type_env)
            },
            ValueExpr::Block(items) => items.iter().for_each(|item| flatten_returns(&item.0, return_types_found, type_env)),
            ValueExpr::Return(Some(value_expr)) => {
                return_types_found.push(TypeExpr::from_value_expr(&value_expr.0, type_env));
            },
            ValueExpr::Return(None) => {
                return_types_found.push(TypeExpr::Tuple(vec![]));
            },
            ValueExpr::VarAssign(assignment) => {
                flatten_returns(&assignment.as_ref().0.value_expr.0, return_types_found, type_env);
            },
            ValueExpr::VarDecl(declaration) => {
                declaration.as_ref().0.initializer.as_ref().inspect(|initializer| {
                    flatten_returns(&initializer.0, return_types_found, type_env);
                });
            },
            ValueExpr::Add(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            },
            ValueExpr::Mul(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            },
            ValueExpr::BoolNegate(value_expr) => {
                flatten_returns(&value_expr.as_ref().0, return_types_found, type_env);
            },
            ValueExpr::Equals(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            },
        }
    }

    let mut return_types_found = Vec::new();
    flatten_returns(&fun_def.value_expr.0, &mut return_types_found, type_env);

    return_types_found.sort_by_key(|type_expr| type_expr.as_clean_go_type_name(type_env));
    return_types_found.dedup();

    if return_types_found.is_empty() {
        return Ok(TypeExpr::Tuple(vec![]));
    }

    if return_types_found.len() == 1 {
        return Ok(return_types_found.first().unwrap().clone())
    }

    // TODO add spans
    return Ok(TypeExpr::Or(return_types_found
        .iter()
        .map(|type_expr| type_expr.clone().into_empty_span())
        .collect::<Vec<_>>()
    ))
}

fn require(condition: bool, fail_message: String) {
    if !condition {
        println!("TypeError: {fail_message}");
        process::exit(2);
    }
}

fn check_type_compatability(
    one: &Spanned<TypeExpr>,
    two: &Spanned<TypeExpr>,
    type_env: &mut TypeEnv,
) {
    if one.0.is_number() {
        if !two.0.is_number() {
            failure(
                one.1.context.file_name,
                "Incompatible Types".to_string(),
                (
                    format!(
                        "This expression is of type {}, which is a number.",
                        one.0.as_go_type_annotation(type_env)
                    ),
                    one.1,
                ),
                vec![
                    (
                        "because of this, the second operand also needs to be of type number."
                            .to_string(),
                        two.1,
                    ),
                    (
                        format!(
                            "but it is of type {}.",
                            two.0.as_go_type_annotation(type_env)
                        ),
                        two.1,
                    ),
                ],
                one.1.context.file_contents,
            )
        }

        return;
    }

    if one.0.as_clean_go_type_name(type_env) != two.0.as_clean_go_type_name(type_env) {
        let smaller = if one.1.start > two.1.start {
            two.1
        } else {
            one.1
        };
        let larger = if one.1.start < two.1.start {
            two.1
        } else {
            one.1
        };

        let combined_span = SS {
            start: smaller.start,
            end: larger.end,
            context: one.1.context,
        };

        failure(
            one.1.context.file_name,
            "Incompatible Types".to_string(),
            (
                format!("this is of type {}", one.0.as_go_type_annotation(type_env)),
                one.1,
            ),
            vec![
                (
                    format!("this is of type {}", two.0.as_go_type_annotation(type_env)),
                    two.1,
                ),
                (
                    "These two types are not not compatible".to_string(),
                    combined_span,
                ),
            ],
            one.1.context.file_contents,
        );
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
            ("{ let x: Int = 5; 5 }", TypeExpr::Int),
            ("{ let x: Int = 5; x }", TypeExpr::Int),
            ("{ let x: Int = 5; x * x }", TypeExpr::Int),
        ];

        for (src, expected_type_expr) in src_and_expected_type_vec {
            println!("lexing {src}");
            let lexer_parse_result = lexer("test", "").parse(src);
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
                "{ let y: { x: String, y: Int }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 5);
                }),
            ),
            (
                "{ let y: { x: String, y: Int, a: { b: { c: { d: { e: String }}}} }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 9);
                }),
            ),
            (
                "{ let x: Int = 5; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 3);
                }),
            ),
            (
                "{ let x: { a: Char, b: Char }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 4);
                }),
            ),
            (
                "{ let y: { x: { y: Int } }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 5);
                }),
            ),
            (
                "{ let y: { x: Int }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 4);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int } }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 6);
                }),
            ),
            (
                "{ let y: { x: Int, y: String, z: { x: Int }, w: () }; }",
                Box::new(|summary: &TypesSummary| {
                    assert_eq!(summary.types_used.len(), 6);
                }),
            ),
        ];

        for (src, summary_check_fun) in src_and_summary_check_funs {
            let lexer_parse_result = lexer("test", "").parse(src);
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
