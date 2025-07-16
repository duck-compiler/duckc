use std::collections::HashMap;

use crate::{
    parse::{
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        source_file_parser::SourceFile,
        type_parser::{Duck, Struct, TypeDefinition, TypeExpr},
        value_parser::{Assignment, Declaration, ValFmtStringContents, ValueExpr}, Spanned,
    },
    semantics::ident_mangler::mangle,
    tags::Tag,
};

#[derive(Debug, Clone)]
pub enum GenericDefinition {
    Function(FunctionDefintion),
    Type(TypeDefinition),
}

impl GenericDefinition {
    pub fn typename_with_given_typeparams(
        &self,
        env: &mut TypeEnv,
        type_params: &Vec<(String, Spanned<TypeExpr>)>,
    ) -> TypeExpr {
        let typename = match self {
            Self::Function(function_def) => format!(
                "{}{}",
                function_def.name,
                type_params.iter()
                    .map(|(generic_name, type_expr)| format!("{generic_name}_as_{}", type_expr.0.as_clean_go_type_name(env)))
                    .collect::<Vec<_>>()
                    .join("____")
            ),
            Self::Type(type_def) => format!(
                "{}{}",
                type_def.name,
                type_params.iter()
                    .map(|(generic_name, type_expr)| format!("{generic_name}_as_{}", type_expr.0.as_clean_go_type_name(env)))
                    .collect::<Vec<_>>()
                    .join("____")
            )
        };

        TypeExpr::TypeNameInternal(typename)
    }

    pub fn generics_names(
        &self,
    ) -> Vec<String> {
        return match self {
            Self::Function(function_def) => function_def
                .generics
                .as_ref()
                .expect("FunctionDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>(),
            Self::Type(type_def) => type_def
                .generics
                .as_ref()
                .expect("TypeDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>()
        };
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub identifier_types: Vec<HashMap<String, TypeExpr>>,
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub all_types: Vec<TypeExpr>,
    pub generic_definitions: HashMap<String, GenericDefinition>,
    pub generics_used: Vec<(GenericDefinition, Vec<(String, Spanned<TypeExpr>)>)>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],
            all_types: vec![],
            generic_definitions: HashMap::new(),
            generics_used: vec![],
        }
    }
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
        self.type_aliases.pop();
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

        match &type_expr {
            TypeExpr::TypeName(_, ident, None) => {
                let resolved = self.resolve_type_alias(ident);
                self.all_types.push(resolved.clone());
                resolved
            },
            TypeExpr::TypeName(_, ident, Some(generics)) => {
                TypeExpr::TypeNameInternal(ident.clone())
            },
            _ => {
                self.all_types.push(type_expr.clone());
                type_expr
            }
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

    pub fn insert_generic_definition(&mut self, alias: String, definition: GenericDefinition) {
        self.generic_definitions
            .insert(alias, definition);
    }

    pub fn resolve_generic_type_definition(&mut self, alias: String) -> GenericDefinition {
        self.generic_definitions
            .iter()
            .find(|(key, _)| **key == alias)
            .expect("expect")
            .1
            .clone()
    }

    pub fn use_generic_definition(&mut self, definition: GenericDefinition, with: &Vec<(String, Spanned<TypeExpr>)>) -> TypeExpr {
        self.generics_used
            .push((definition.clone(), with.clone()));

        return definition.typename_with_given_typeparams(self, with)
    }

    pub fn resolve_type_alias(&self, alias: &String) -> TypeExpr {
        let type_aliases = self
            .type_aliases
            .last()
            .expect("At least one type aliases hashmap should exist. :(");

        println!("Try to resolve type alias {alias}");

        let mut res = type_aliases
            .get(alias)
            .unwrap_or_else(|| panic!("Couldn't resolve type alias {alias}"))
            .clone();

        if let TypeExpr::Or(types) = &mut res {
            for ty in types {
                // todo: type params
                if let TypeExpr::TypeName(_, name, _type_params) = &ty.0 {
                    ty.0 = self.resolve_type_alias(name);
                }
            }
        }

        res
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

                if !field.type_expr.0.has_subtypes() {
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
            TypeExpr::Or(types) => types.iter_mut().for_each(|(type_expr, ..)| {
                found.push(type_expr.clone());
                found.extend(self.flatten_types(type_expr, param_names_used));
            }),
            _ => {
                found.push(type_expr.clone());
            }
        }

        found
    }

    pub fn summarize(&mut self) -> TypesSummary {
        let mut all_types = self.all_types.clone();
        all_types.extend(TypeExpr::primitives());
        all_types.push(TypeExpr::Tuple(vec![]));
        let mut param_names_used = Vec::new();

        let mut to_push = Vec::new();
        all_types.iter_mut().for_each(|type_expr| {
            to_push.append(&mut self.flatten_types(type_expr, &mut param_names_used));
        });

        all_types.append(&mut to_push);
        all_types.sort_by_key(|type_expr| type_expr.as_clean_go_type_name(self));
        all_types.dedup_by_key(|type_expr| type_expr.as_clean_go_type_name(self));

        param_names_used.dedup();

        dbg!(&self.generic_definitions);
        dbg!(&self.generics_used);

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

fn resolve_all_aliases_type_expr(expr: &mut TypeExpr, env: &mut TypeEnv) {
    match expr {
        TypeExpr::GenericToBeReplaced(..) => panic!(),
        TypeExpr::RawTypeName(_, typename, _) => {
            if typename.len() != 1 {
                panic!()
            }

            let type_expr = env.resolve_type_alias(typename.first().unwrap());
            dbg!(type_expr);
        },
        TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                resolve_all_aliases_type_expr(&mut field.type_expr.0, env);
            }
        }
        TypeExpr::Array(d) => resolve_all_aliases_type_expr(&mut d.0, env),
        TypeExpr::Fun(params, r) => {
            if let Some(r) = r {
                resolve_all_aliases_type_expr(&mut r.0, env);
            }
            params
                .iter_mut()
                .for_each(|(_, x)| resolve_all_aliases_type_expr(&mut x.0, env));
        }
        TypeExpr::Or(exprs) => {
            for expr in exprs {
                resolve_all_aliases_type_expr(&mut expr.0, env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for field in fields {
                resolve_all_aliases_type_expr(&mut field.0, env);
            }
        }
        TypeExpr::TypeName(_, name, type_params) => {
            if let Some(type_params) = type_params {
                type_params
                    .iter_mut()
                    .for_each(|type_param| resolve_all_aliases_type_expr(&mut type_param.0, env));

                let generic_definition = env.resolve_generic_type_definition(name.to_string());
                let generics_names = generic_definition.generics_names();

                let with = type_params.iter()
                    .enumerate()
                    .map(|(index, type_expr)| (
                        generics_names
                            .get(index)
                            .expect("TODO: message")
                            .clone(),
                        type_expr.clone()
                    ))
                    .collect::<Vec<_>>();

                let type_expr = env.use_generic_definition(generic_definition, &with);

                *expr = type_expr;
                return;
            }

            *expr = env.resolve_type_alias(name);
        }
        TypeExpr::Any
        | TypeExpr::Bool
        | TypeExpr::BoolLiteral(_)
        | TypeExpr::Char
        | TypeExpr::Float
        | TypeExpr::Go(_)
        | TypeExpr::InlineGo
        | TypeExpr::Int
        | TypeExpr::IntLiteral(_)
        | TypeExpr::String
        | TypeExpr::StringLiteral(_)
        | TypeExpr::TypeNameInternal(..) => {}
    }
}

fn resolve_all_aliases_value_expr(expr: &mut ValueExpr, env: &mut TypeEnv) {
    match expr {
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                resolve_all_aliases_type_expr(&mut ty.0, env);
            }
            for expr in exprs {
                resolve_all_aliases_value_expr(&mut expr.0, env);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                name: _,
                type_expr,
                initializer,
            } = &mut d.0;
            resolve_all_aliases_type_expr(&mut type_expr.0, env);
            if let Some(e) = initializer {
                resolve_all_aliases_value_expr(&mut e.0, env);
            }
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **l;
            if let Some(return_type) = return_type {
                resolve_all_aliases_type_expr(&mut return_type.0, env);
            }
            for (_, p) in params {
                resolve_all_aliases_type_expr(&mut p.0, env);
            }
            resolve_all_aliases_value_expr(&mut value_expr.0, env);
        }
        ValueExpr::Add(l, r) | ValueExpr::Mul(l, r) | ValueExpr::Equals(l, r) => {
            resolve_all_aliases_value_expr(&mut l.0, env);
            resolve_all_aliases_value_expr(&mut r.0, env);
        }
        ValueExpr::ArrayAccess(target, idx) => {
            resolve_all_aliases_value_expr(&mut target.0, env);
            resolve_all_aliases_value_expr(&mut idx.0, env);
        }
        ValueExpr::Block(exprs) => {
            for expr in exprs {
                resolve_all_aliases_value_expr(&mut expr.0, env);
            }
        }
        ValueExpr::BoolNegate(e) => resolve_all_aliases_value_expr(&mut e.0, env),
        ValueExpr::Duck(init) => {
            for i in init {
                resolve_all_aliases_value_expr(&mut i.1.0, env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            resolve_all_aliases_value_expr(&mut target_obj.0, env);
        }
        ValueExpr::FormattedString(content) => {
            for c in content {
                if let ValFmtStringContents::Expr(e) = c {
                    resolve_all_aliases_value_expr(&mut e.0, env);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            resolve_all_aliases_value_expr(&mut target.0, env);

            if let Some(type_params) = type_params {
                type_params
                    .iter_mut()
                    .for_each(|type_param| resolve_all_aliases_type_expr(&mut type_param.0, env));
            }

            params
                .iter_mut()
                .for_each(|param| resolve_all_aliases_value_expr(&mut param.0, env));
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            resolve_all_aliases_value_expr(&mut condition.0, env);
            resolve_all_aliases_value_expr(&mut then.0, env);
            if let Some(r#else) = r#else {
                resolve_all_aliases_value_expr(&mut r#else.0, env);
            }
        }
        ValueExpr::Match { value_expr, arms } => {
            resolve_all_aliases_value_expr(&mut value_expr.0, env);
            for arm in arms {
                resolve_all_aliases_type_expr(&mut arm.type_case.0, env);
                resolve_all_aliases_value_expr(&mut arm.value_expr.0, env);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(r) = r {
                resolve_all_aliases_value_expr(&mut r.0, env);
            }
        }
        ValueExpr::Struct(fields) => {
            for field in fields {
                resolve_all_aliases_value_expr(&mut field.1.0, env);
            }
        }
        ValueExpr::Tuple(fields) => {
            for field in fields {
                resolve_all_aliases_value_expr(&mut field.0, env);
            }
        }
        ValueExpr::VarAssign(a) => {
            let Assignment { target, value_expr } = &mut a.0;
            resolve_all_aliases_value_expr(&mut target.0, env);
            resolve_all_aliases_value_expr(&mut value_expr.0, env);
        }
        ValueExpr::While { condition, body } => {
            resolve_all_aliases_value_expr(&mut condition.0, env);
            resolve_all_aliases_value_expr(&mut body.0, env);
        }
        ValueExpr::Break
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Continue
        | ValueExpr::String(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Bool(..) => {}
    }
}

fn sort_fields_value_expr(expr: &mut ValueExpr) {
    match expr {
        ValueExpr::Array(ty, exprs) => {
            if let Some(ty) = ty {
                sort_fields_type_expr(&mut ty.0);
            }
            for expr in exprs {
                sort_fields_value_expr(&mut expr.0);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                name: _,
                type_expr,
                initializer,
            } = &mut d.0;
            sort_fields_type_expr(&mut type_expr.0);
            if let Some(e) = initializer {
                sort_fields_value_expr(&mut e.0);
            }
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **l;
            if let Some(return_type) = return_type {
                sort_fields_type_expr(&mut return_type.0);
            }
            for (_, p) in params {
                sort_fields_type_expr(&mut p.0);
            }
            sort_fields_value_expr(&mut value_expr.0);
        }
        ValueExpr::Add(l, r) | ValueExpr::Mul(l, r) | ValueExpr::Equals(l, r) => {
            sort_fields_value_expr(&mut l.0);
            sort_fields_value_expr(&mut r.0);
        }
        ValueExpr::ArrayAccess(target, idx) => {
            sort_fields_value_expr(&mut target.0);
            sort_fields_value_expr(&mut idx.0);
        }
        ValueExpr::Block(exprs) => {
            for expr in exprs {
                sort_fields_value_expr(&mut expr.0);
            }
        }
        ValueExpr::BoolNegate(e) => sort_fields_value_expr(&mut e.0),
        ValueExpr::Duck(init) => {
            for i in init {
                sort_fields_value_expr(&mut i.1.0);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            sort_fields_value_expr(&mut target_obj.0);
        }
        ValueExpr::FormattedString(content) => {
            for c in content {
                if let ValFmtStringContents::Expr(e) = c {
                    sort_fields_value_expr(&mut e.0);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params: _,
        } => {
            // todo: type_params
            sort_fields_value_expr(&mut target.0);
            for p in params {
                sort_fields_value_expr(&mut p.0);
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            sort_fields_value_expr(&mut condition.0);
            sort_fields_value_expr(&mut then.0);
            if let Some(r#else) = r#else {
                sort_fields_value_expr(&mut r#else.0);
            }
        }
        ValueExpr::Match { value_expr, arms } => {
            sort_fields_value_expr(&mut value_expr.0);
            for arm in arms {
                sort_fields_type_expr(&mut arm.type_case.0);
                sort_fields_value_expr(&mut arm.value_expr.0);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(r) = r {
                sort_fields_value_expr(&mut r.0);
            }
        }
        ValueExpr::Struct(fields) => {
            for field in fields {
                sort_fields_value_expr(&mut field.1.0);
            }
        }
        ValueExpr::Tuple(fields) => {
            for field in fields {
                sort_fields_value_expr(&mut field.0);
            }
        }
        ValueExpr::VarAssign(a) => {
            let Assignment { target, value_expr } = &mut a.0;
            sort_fields_value_expr(&mut target.0);
            sort_fields_value_expr(&mut value_expr.0);
        }
        ValueExpr::While { condition, body } => {
            sort_fields_value_expr(&mut condition.0);
            sort_fields_value_expr(&mut body.0);
        }
        ValueExpr::Break
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Continue
        | ValueExpr::String(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Bool(..) => {}
    }
}

fn sort_fields_type_expr(expr: &mut TypeExpr) {
    match expr {
        TypeExpr::GenericToBeReplaced(..) => panic!(),
        TypeExpr::RawTypeName(..) => {}
        TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                sort_fields_type_expr(&mut field.type_expr.0);
            }
        }
        TypeExpr::Array(d) => sort_fields_type_expr(&mut d.0),
        TypeExpr::Fun(params, r) => {
            if let Some(r) = r {
                sort_fields_type_expr(&mut r.0);
            }
            params
                .iter_mut()
                .for_each(|(_, x)| sort_fields_type_expr(&mut x.0));
        }
        TypeExpr::Or(exprs) => {
            for expr in exprs {
                sort_fields_type_expr(&mut expr.0);
            }
        }
        TypeExpr::Tuple(fields) => {
            for field in fields {
                sort_fields_type_expr(&mut field.0);
            }
        }
        TypeExpr::Any
        | TypeExpr::Bool
        | TypeExpr::BoolLiteral(_)
        | TypeExpr::Char
        | TypeExpr::Float
        | TypeExpr::Go(_)
        | TypeExpr::InlineGo
        | TypeExpr::Int
        | TypeExpr::IntLiteral(_)
        | TypeExpr::String
        | TypeExpr::StringLiteral(_)
        | TypeExpr::TypeName(..)
        | TypeExpr::TypeNameInternal(..) => {}
    }
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    // Step 1: Sort fields
    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_definition| {
            sort_fields_type_expr(&mut type_definition.type_expression.0);
        });

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            sort_fields_value_expr(&mut function_definition.value_expr.0);
        });

    // Step 2: Insert type definitions
    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| {
            if type_definition.generics.is_some() {
                type_env.insert_generic_definition(
                    type_definition.name.clone(),
                    GenericDefinition::Type(type_definition.clone()),
                );

                return;
            }

            type_env.insert_type_alias(
                type_definition.name.clone(),
                type_definition.type_expression.0.clone(),
            )
        });

    // Step 3: Resolve all aliases
    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_definition| {
            if let Some(generics) = type_definition.generics.clone() {
                // todo: create some kind of type which indicates that this type needs to be replaced later on.
                type_env.push_type_aliases();

                generics.iter()
                    .for_each(|generic| {
                        println!("Inserting generic {}", generic.0.name.clone());
                        type_env.insert_type_alias(generic.0.name.clone(), TypeExpr::GenericToBeReplaced(generic.0.name.clone()))
                    });

                resolve_all_aliases_type_expr(&mut type_definition.type_expression.0, type_env);

                type_env.pop_type_aliases();
                return;
            }

            resolve_all_aliases_type_expr(&mut type_definition.type_expression.0, type_env);
        });

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            if let Some(params) = function_definition.params.as_mut() {
                for (_, p) in params {
                    resolve_all_aliases_type_expr(&mut p.0, type_env);
                }
            }
            if let Some(r) = function_definition.return_type.as_mut() {
                resolve_all_aliases_type_expr(&mut r.0, type_env);
            }
            resolve_all_aliases_value_expr(&mut function_definition.value_expr.0, type_env);
        });

    dbg!(&source_file);

    // Step 5: Map function names available
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
                    .map(|(identifier, type_expr)| (Some(identifier.clone()), type_expr.clone()))
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

    // Step 6: Type Check functions
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            typeresolve_function_definition(function_definition, type_env);

            // let explicit_return_type = function_definition
            //     .return_type
            //     .clone()
            //     .unwrap_or_else(|| TypeExpr::Tuple(vec![]).into_empty_span());

            // let implicit_return_type =
            //     resolve_implicit_function_return_type(function_definition, type_env).unwrap();

            // check_type_compatability(
            // &explicit_return_type,
            // &implicit_return_type.into_empty_span(),
            // type_env,
            // );
        });

    // mvmo - 03.07.2025: is this required? If not we should remove the type aliases stack at all
    // type_env.pop_type_aliases();

    fn typeresolve_function_definition(
        function_definition: &mut FunctionDefintion,
        type_env: &mut TypeEnv,
    ) {
        if function_definition.name == "main" {
            // let valid_return_type = match function_definition
            //     .return_type
            //     .clone()
            //     .map(|return_type| return_type.0)
            // {
            //     Some(TypeExpr::Int) => true,
            //     Some(TypeExpr::Tuple(types)) if types.is_empty() => true,
            //     None => true,
            //     _ => false,
            // };

            // todo(@mvmo): move checks from typeresolve to typecheck

            // if !valid_return_type {
            //     let span = function_definition.return_type.as_ref().unwrap().1;
            //     // failure(
            //     //     function_definition.value_expr.1.context.file_name,
            //     //     "Tried to return non-int value from main function".to_string(),
            //     //     (
            //     //         "This is the type you've declared the main function to return".to_string(),
            //     //         span,
            //     //     ),
            //     //     vec![(
            //     //         "The main function can only return either Nothing or Int".to_string(),
            //     //         function_definition.value_expr.1,
            //     //     )],
            //     //     function_definition.value_expr.1.context.file_contents,
            //     // )
            // }
        }

        if let Some((return_type, _)) = &mut function_definition.return_type {
            *return_type = type_env.insert_type(return_type.clone());
        }

        type_env.push_identifier_types();

        if let Some(params) = function_definition.params.clone().as_mut() {
            for p in params {
                type_env.insert_identifier_type(p.0.clone(), p.1.0.clone());
            }
        }

        typeresolve_value_expr(&mut function_definition.value_expr.0, type_env);
        type_env.pop_identifier_types();
    }

    fn typeresolve_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
        match value_expr {
            ValueExpr::RawVariable(_, path) => {
                let ident = mangle(path);

                let type_expr = type_env
                    .get_identifier_type(ident.clone())
                    .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {ident}"));

                println!(
                    "{}{}{}trying to resolve raw variable {:?}",
                    Tag::Dargo,
                    Tag::Build,
                    Tag::Err,
                    &value_expr
                );

                *value_expr = ValueExpr::Variable(true, ident, Some(type_expr));
            }
            ValueExpr::FormattedString(contents) => {
                for c in contents {
                    if let ValFmtStringContents::Expr(e) = c {
                        typeresolve_value_expr(&mut e.0, type_env);
                    }
                }
            }
            ValueExpr::ArrayAccess(target, idx) => {
                typeresolve_value_expr(&mut target.0, type_env);
                typeresolve_value_expr(&mut idx.0, type_env);
            }
            ValueExpr::Array(_, exprs) => {
                for expr in exprs {
                    typeresolve_value_expr(&mut expr.0, type_env);
                }
                let ty = TypeExpr::from_value_expr(value_expr as &ValueExpr, type_env);
                type_env.insert_type(ty);
            }
            ValueExpr::InlineGo(..) => {}
            ValueExpr::Lambda(b) => {
                let LambdaFunctionExpr {
                    params: _,
                    return_type: _,
                    value_expr,
                } = &mut **b;

                type_env.push_identifier_types();
                typeresolve_value_expr(&mut value_expr.0, type_env);
                type_env.pop_identifier_types();
            }
            ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
            } => {
                // todo: type_params
                typeresolve_value_expr(&mut target.0, type_env);
                params
                    .iter_mut()
                    .for_each(|param| typeresolve_value_expr(&mut param.0, type_env));
            }
            ValueExpr::Variable(_, identifier, type_expr_opt) => {
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

                // todo: type params
                if let (TypeExpr::TypeName(_, type_name, _type_params), span) =
                    &declaration.type_expr
                {
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
            ValueExpr::Match { value_expr, arms } => {
                typeresolve_value_expr(&mut value_expr.0, type_env);
                arms.iter_mut().for_each(|arm| {
                    type_env.push_identifier_types();

                    type_env.insert_identifier_type(
                        arm.bound_to_identifier.clone(),
                        arm.type_case.0.clone(),
                    );
                    typeresolve_value_expr(&mut arm.value_expr.0, type_env);

                    type_env.pop_identifier_types();
                });
            }
            ValueExpr::String(..)
            | ValueExpr::Int(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Char(..)
            | ValueExpr::Float(..)
            | ValueExpr::Break
            | ValueExpr::Return(None)
            | ValueExpr::Continue => {}
        }
    }
}

// todo(@mvmo): typeinference
// resolve implicit type and attach it to variables for type inference
#[allow(dead_code)]
fn resolve_implicit_function_return_type(
    fun_def: &FunctionDefintion,
    type_env: &mut TypeEnv,
) -> Result<TypeExpr, String> {
    // check against annotated return type
    fn flatten_returns(
        value_expr: &ValueExpr,
        return_types_found: &mut Vec<TypeExpr>,
        type_env: &mut TypeEnv,
    ) {
        match value_expr {
            ValueExpr::FormattedString(contents) => {
                for c in contents {
                    if let ValFmtStringContents::Expr(e) = c {
                        flatten_returns(&e.0, return_types_found, type_env);
                    }
                }
            }
            ValueExpr::Array(_, exprs) => {
                for expr in exprs {
                    flatten_returns(&expr.0, return_types_found, type_env);
                }
            }
            ValueExpr::FunctionCall { .. }
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
            | ValueExpr::Variable(..)
            | ValueExpr::RawVariable(..)
            | ValueExpr::Match { .. }
            | ValueExpr::ArrayAccess(..) => {}
            ValueExpr::If {
                condition,
                then,
                r#else,
            } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&then.as_ref().0, return_types_found, type_env);
                r#else.as_ref().inspect(|r#else| {
                    flatten_returns(&r#else.as_ref().0, return_types_found, type_env);
                });
            }
            ValueExpr::While { condition, body } => {
                flatten_returns(&condition.as_ref().0, return_types_found, type_env);
                flatten_returns(&body.as_ref().0, return_types_found, type_env)
            }
            ValueExpr::Block(items) => items
                .iter()
                .for_each(|item| flatten_returns(&item.0, return_types_found, type_env)),
            ValueExpr::Return(Some(value_expr)) => {
                return_types_found.push(TypeExpr::from_value_expr(&value_expr.0, type_env));
            }
            ValueExpr::Return(None) => {
                return_types_found.push(TypeExpr::Tuple(vec![]));
            }
            ValueExpr::VarAssign(assignment) => {
                flatten_returns(
                    &assignment.as_ref().0.value_expr.0,
                    return_types_found,
                    type_env,
                );
            }
            ValueExpr::VarDecl(declaration) => {
                declaration
                    .as_ref()
                    .0
                    .initializer
                    .as_ref()
                    .inspect(|initializer| {
                        flatten_returns(&initializer.0, return_types_found, type_env);
                    });
            }
            ValueExpr::Add(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::Mul(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::BoolNegate(value_expr) => {
                flatten_returns(&value_expr.as_ref().0, return_types_found, type_env);
            }
            ValueExpr::Equals(left, right) => {
                flatten_returns(&left.as_ref().0, return_types_found, type_env);
                flatten_returns(&right.as_ref().0, return_types_found, type_env);
            }
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
        return Ok(return_types_found.first().unwrap().clone());
    }

    // TODO add spans
    return Ok(TypeExpr::Or(
        return_types_found
            .iter()
            .map(|type_expr| type_expr.clone().into_empty_span())
            .collect::<Vec<_>>(),
    ));
}
