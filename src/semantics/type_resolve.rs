use std::{collections::HashMap, process};

use crate::{
    parse::{
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        source_file_parser::SourceFile,
        type_parser::{Duck, Struct, TypeDefinition, TypeExpr},
        value_parser::{empty_range, Assignment, Declaration, ValFmtStringContents, ValueExpr},
        Spanned, SS,
    }, semantics::ident_mangler::mangle, tags::Tag
};

#[derive(Debug, Clone)]
pub enum GenericDefinition {
    Function(FunctionDefintion),
    Type(TypeDefinition),
}

impl GenericDefinition {
    pub fn generics_names(&self) -> Vec<String> {
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
                .collect::<Vec<_>>(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub identifier_types: Vec<HashMap<String, TypeExpr>>,
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub all_types: Vec<TypeExpr>,
    pub generic_definitions: HashMap<String, GenericDefinition>,
    pub generic_fns_generated: HashMap<String, (FunctionDefintion, TypeExpr)>
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],
            all_types: vec![],
            generic_definitions: HashMap::new(),
            generic_fns_generated: HashMap::new(),
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
            }
            TypeExpr::TypeName(_, ident, Some(_)) => TypeExpr::TypeNameInternal(ident.clone()),
            _ => {
                self.all_types.push(type_expr.clone());
                type_expr
            }
        }
    }

    pub fn get_identifier_type(&self, identifier: String) -> Option<TypeExpr> {
        self.identifier_types
            .last()
            .expect("At least one env should exist. :(")
            .get(&identifier)
            .cloned()
    }

    pub fn insert_type_alias(&mut self, alias: String, type_expr: TypeExpr) {
        self.type_aliases
            .last_mut()
            .expect("At least one type aliases hashmap should exist. :(")
            .insert(alias, type_expr);
    }

    pub fn insert_generic_definition(&mut self, alias: String, definition: GenericDefinition) {
        self.generic_definitions.insert(alias, definition);
    }

    fn instantiate_generic_function(
        &mut self,
        name: &String,
        type_params: &[Spanned<TypeExpr>],
        _span: SS,
    ) -> (FunctionDefintion, TypeExpr) {
        let generic_def = self.generic_definitions.get(name).cloned().unwrap_or_else(|| {
            println!(
                "{}{}{}Generic function '{}' not found.",
                Tag::Dargo,
                Tag::Build,
                Tag::Err,
                name
            );
            process::exit(1);
        });

        let mut fn_def = match generic_def {
            GenericDefinition::Function(def) => def,
            GenericDefinition::Type(_) => {
                println!(
                    "{}{}{}Expected '{}' to be a generic function, but it's a type.",
                    Tag::Dargo,
                    Tag::Build,
                    Tag::Err,
                    name
                );
                process::exit(1);
            }
        };

        let generics = fn_def.generics.as_ref().unwrap();
        if generics.len() != type_params.len() {
            println!(
                "{}{}{}Expected {} type arguments for generic function '{}', but got {}.",
                Tag::Dargo,
                Tag::Build,
                Tag::Err,
                generics.len(),
                name,
                type_params.len()
            );
            process::exit(1);
        }

        let mangled_name = format!(
            "{}_{}",
            fn_def.name,
            type_params
                .iter()
                .map(|type_param| type_param.0.as_clean_go_type_name(self))
                .collect::<Vec<_>>()
                .join("_")
        );

        if self.generic_fns_generated.contains_key(&mangled_name) {
            return self.generic_fns_generated.get(&mangled_name).unwrap().clone();
        }

        self.push_type_aliases();

        for (generic_param, concrete_type) in generics.iter().zip(type_params.iter()) {
            self.insert_type_alias(generic_param.0.name.clone(), concrete_type.0.clone());
        }

        if let Some(params) = fn_def.params.as_mut() {
            for (param_name, param_type_expr) in params {
                resolve_all_aliases_type_expr(&mut param_type_expr.0, self);
                self.insert_identifier_type(param_name.clone(), param_type_expr.0.clone());
            }
        }

        if let Some(return_type) = fn_def.return_type.as_mut() {
            resolve_all_aliases_type_expr(&mut return_type.0, self);
        }

        typeresolve_value_expr(&mut fn_def.value_expr.0, self);

        fn_def.name = mangled_name;
        fn_def.generics = None;

        let fn_type_expr = TypeExpr::Fun(
            fn_def
                .params
                .as_ref()
                .unwrap_or(&Vec::new())
                .iter()
                .map(|(identifier, type_expr)| (Some(identifier.clone()), type_expr.clone()))
                .collect::<Vec<_>>(),
            fn_def
                .return_type
                .as_ref()
                .map(|spanned_type_expr| {
                    Box::new((
                        self.insert_type(spanned_type_expr.0.clone()),
                        spanned_type_expr.1,
                    ))
                }),
        );

        self.pop_type_aliases();

        self.insert_identifier_type(fn_def.name.clone(), fn_type_expr.clone());

        self.generic_fns_generated.insert(fn_def.name.clone(), (fn_def.clone(), fn_type_expr.clone()));
        return (fn_def, fn_type_expr);
    }

    fn instantiate_generic_type(
        &mut self,
        name: &String,
        type_params: &mut [Spanned<TypeExpr>],
        _span: SS,
    ) -> TypeExpr {
        let generic_def = self.generic_definitions.get(name).cloned().unwrap_or_else(|| {
            println!(
                "{}{}{}Generic type '{}' not found.",
                Tag::Dargo,
                Tag::Build,
                Tag::Err,
                name
            );
            process::exit(1);
        });

        let type_def = match generic_def {
            GenericDefinition::Type(td) => td,
            GenericDefinition::Function(_) => {
                println!(
                    "{}{}{}Expected '{}' to be a generic type, but it's a function.",
                    Tag::Dargo,
                    Tag::Build,
                    Tag::Err,
                    name
                );
                process::exit(1);
            }
        };

        let generics = type_def.generics.as_ref().unwrap();
        if generics.len() != type_params.len() {
            println!(
                "{}{}{}Expected {} type arguments for generic type '{}', but got {}.",
                Tag::Dargo,
                Tag::Build,
                Tag::Err,
                generics.len(),
                name,
                type_params.len()
            );
            process::exit(1);
        }

        let mangled_name = format!(
            "{}_{}",
            type_def.name,
            type_params
                .iter()
                .map(|tp| tp.0.as_clean_go_type_name(self))
                .collect::<Vec<_>>()
                .join("_")
        );

        if self.type_aliases.last().unwrap().contains_key(&mangled_name) {
            return self.resolve_type_alias(&mangled_name);
        }

        self.push_type_aliases(); // Create a new scope for this instantiation

        for (generic_param, concrete_type) in generics.iter().zip(type_params.iter()) {
            self.insert_type_alias(generic_param.0.name.clone(), concrete_type.0.clone());
        }

        let mut concrete_type_expr = type_def.type_expression.0.clone();

        resolve_all_aliases_type_expr(&mut concrete_type_expr, self);

        self.pop_type_aliases();

        self.insert_type_alias(mangled_name.clone(), concrete_type_expr.clone());
        self.all_types.push(concrete_type_expr.clone());

        concrete_type_expr
    }

    pub fn resolve_type_alias(&self, alias: &String) -> TypeExpr {
        for scope in self.type_aliases.iter().rev() {
            if let Some(type_expr) = scope.get(alias) {
                let mut res = type_expr.clone();
                if let TypeExpr::Or(types) = &mut res {
                    for ty in types {
                        if let TypeExpr::TypeName(_, name, _type_params) = &ty.0 {
                            ty.0 = self.resolve_type_alias(name);
                        }
                    }
                }

                return res;
            }
        }

        panic!("Couldn't resolve type alias {alias} on stack #{}", self.type_aliases.len());
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

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

fn resolve_all_aliases_type_expr(expr: &mut TypeExpr, env: &mut TypeEnv) {
    match expr {
        TypeExpr::GenericToBeReplaced(name) => {
            if let Some(resolved) = env.type_aliases.last().unwrap().get(name) {
                *expr = resolved.clone();
            }
        }
        TypeExpr::RawTypeName(_, typename, _) => {
            if typename.len() != 1 {
                panic!()
            }

            *expr = env.resolve_type_alias(typename.first().unwrap());
        }
        TypeExpr::Duck(Duck { fields }) | TypeExpr::Struct(Struct { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                resolve_all_aliases_type_expr(&mut field.type_expr.0, env);
            }
        }
        TypeExpr::Array(d) => resolve_all_aliases_type_expr(&mut d.0, env),
        TypeExpr::Fun(params, return_type) => {
            if let Some(r) = return_type {
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
        TypeExpr::TypeName(_, name, type_params_opt) => {
            if let Some(type_params) = type_params_opt {
                for type_param in type_params.iter_mut() {
                    resolve_all_aliases_type_expr(&mut type_param.0, env);
                }
                if env.generic_definitions.contains_key(name) {
                    // todo: add span for generic types
                    *expr = env.instantiate_generic_type(name, type_params, empty_range());
                }
            } else if !env.generic_definitions.contains_key(name) {
                *expr = env.resolve_type_alias(name);
            }
        }
        _ => {}
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
        | TypeExpr::ConstBool(_)
        | TypeExpr::Char
        | TypeExpr::Float
        | TypeExpr::Go(_)
        | TypeExpr::InlineGo
        | TypeExpr::Int
        | TypeExpr::ConstInt(_)
        | TypeExpr::String
        | TypeExpr::ConstString(_)
        | TypeExpr::TypeName(..)
        | TypeExpr::TypeNameInternal(..) => {}
    }
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    println!(
        "{} sort fields",
        Tag::TypeResolve,
    );

    // Step 1: Sort fields
    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_definition| {
            sort_fields_type_expr(&mut type_definition.type_expression.0);
        });

    // todo: check if we'd rather sort after generic generation
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            sort_fields_value_expr(&mut function_definition.value_expr.0);
        });

    println!(
        "{} insert type definitions",
        Tag::TypeResolve,
    );

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
            } else {
                let mut rhs = type_definition.type_expression.0.clone();
                resolve_all_aliases_type_expr(&mut rhs, type_env);
                type_env.insert_type_alias(type_definition.name.clone(), rhs)
            }
        });

    println!(
        "{} resolve aliases in function signatures and prepare function types",
        Tag::TypeResolve,
    );

    // Step 3: Resolve aliases in function signatures and prepare function types
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            if function_definition.generics.is_some() {
                type_env.insert_generic_definition(function_definition.name.clone(), GenericDefinition::Function(function_definition.clone()));
                return;
            }

            if let Some(params) = function_definition.params.as_mut() {
                for (_, p) in params {
                    resolve_all_aliases_type_expr(&mut p.0, type_env);
                }
            }

            if let Some(r) = function_definition.return_type.as_mut() {
                resolve_all_aliases_type_expr(&mut r.0, type_env);
            }

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

    println!(
        "{} typeresolve functions",
        Tag::TypeResolve,
    );

    // Step 4: Instantiate all generic functions
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            find_generic_fn_instantiations(function_definition, type_env);
        });

    println!(
        "{} final resolve of all functions",
        Tag::TypeResolve,
    );

    // Step 5: Final resolve of all functions
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_defintion| {
            typeresolve_function_definition(function_defintion, type_env);
        });
}

type ResultingDefinitions = Vec<FunctionDefintion>;
fn find_generic_fn_instantiations(function_definition: &mut FunctionDefintion, type_env: &mut TypeEnv) -> ResultingDefinitions {
    fn in_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) -> ResultingDefinitions {
        println!("\n\n\n\n-------------------------------");
        println!("in value expr {value_expr:?}");
        match value_expr {
            ValueExpr::FunctionCall { target, params, type_params } => {
                let mut instantiations = vec![];

                params
                    .iter_mut()
                    .for_each(|param| instantiations.extend(in_value_expr(&mut param.0, type_env)));

                // if it's a generic it must be a variable
                let span = target.as_ref().1;
                let target = &mut target.as_mut().0;
                if let ValueExpr::Variable(is_global, identifier, r#type) = target
                    && type_env.generic_definitions.contains_key(identifier) {
                        let (fn_def, fn_type) = type_env.instantiate_generic_function(
                            identifier,
                            &type_params.clone().unwrap_or_else(std::vec::Vec::new),
                            span,
                        );

                        *r#type = Some(fn_type.clone());
                        *type_params = None;
                        *target = ValueExpr::Variable(*is_global, fn_def.name.clone(), Some(fn_type));

                        instantiations.push(fn_def.clone());
                        println!("return\n\t{:?} \n\tfor {:?}", &instantiations, &value_expr);
                        return instantiations;
                    }
            },
            ValueExpr::Variable(_, identifier, _) => {
                // if it's a generic it must be a variable
                if type_env.generic_definitions.contains_key(identifier) {
                }
            },
            ValueExpr::Int(_) => {},
            ValueExpr::String(_) => {},
            ValueExpr::Bool(_) => {},
            ValueExpr::Float(_) => {},
            ValueExpr::Char(_) => {},
            ValueExpr::RawVariable(_, _) => {},
            ValueExpr::If { condition, then, r#else } => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut condition.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut then.as_mut().0, type_env));
                instantiations.extend(
                    r#else.as_mut()
                        .map(|boxed| in_value_expr(&mut boxed.as_mut().0, type_env))
                        .unwrap_or_else(std::vec::Vec::new)
                );

                return instantiations
            },
            ValueExpr::While { condition, body } => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut condition.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut body.as_mut().0, type_env));

                return instantiations
            },
            ValueExpr::Tuple(items) => {
                let mut instantiations = vec![];
                for item in items {
                    instantiations.extend(in_value_expr(&mut item.0, type_env))
                }

                return instantiations
            },
            ValueExpr::Block(items) => {
                let mut instantiations = vec![];
                for item in items {
                    instantiations.extend(in_value_expr(&mut item.0, type_env))
                }

                return instantiations
            },
            ValueExpr::Duck(items) => {
                let mut instantiations = vec![];
                for item in items {
                    instantiations.extend(in_value_expr(&mut item.1.0, type_env))
                }

                return instantiations
            },
            ValueExpr::Struct(items) => {
                let mut instantiations = vec![];
                for item in items {
                    instantiations.extend(in_value_expr(&mut item.1.0, type_env))
                }

                return instantiations
            },
            ValueExpr::Array(_, items) => {
                let mut instantiations = vec![];
                for item in items {
                    instantiations.extend(in_value_expr(&mut item.0, type_env))
                }

                return instantiations
            },
            ValueExpr::VarAssign(boxed_assignment) => {
                let assignment = &mut boxed_assignment.as_mut().0;

                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut assignment.target.0, type_env));
                instantiations.extend(in_value_expr(&mut assignment.value_expr.0, type_env));

                return instantiations
            },
            ValueExpr::FieldAccess { target_obj, field_name: _ } => return in_value_expr(&mut target_obj.as_mut().0, type_env),
            ValueExpr::Return(Some(boxed_value_expr_and_span)) => return in_value_expr(&mut boxed_value_expr_and_span.as_mut().0, type_env),
            ValueExpr::Return(None) => return vec![],
            ValueExpr::VarDecl(boxed_declaration) if boxed_declaration.as_ref().0.initializer.is_some() => {
                return in_value_expr(&mut boxed_declaration.as_mut().0.initializer.as_mut().unwrap().0, type_env)
            },
            ValueExpr::VarDecl(..) => return vec![],
            ValueExpr::Add(lhs, rhs) => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut lhs.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut rhs.as_mut().0, type_env));

                return instantiations
            },
            ValueExpr::Mul(lhs, rhs) => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut lhs.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut rhs.as_mut().0, type_env));

                return instantiations
            },
            ValueExpr::Equals(lhs, rhs) => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut lhs.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut rhs.as_mut().0, type_env));

                return instantiations
            },
            ValueExpr::BoolNegate(target) => return in_value_expr(&mut target.as_mut().0, type_env),
            // todo: discuss if we enable generic functions being called from go and how
            ValueExpr::InlineGo(_) => return vec![],
            ValueExpr::Lambda(lambda_function_expr) => return in_value_expr(&mut lambda_function_expr.value_expr.0, type_env),
            ValueExpr::ArrayAccess(target, index) => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut target.as_mut().0, type_env));
                instantiations.extend(in_value_expr(&mut index.as_mut().0, type_env));

                return instantiations
            },
            ValueExpr::Match { value_expr, arms } => {
                let mut instantiations = vec![];
                instantiations.extend(in_value_expr(&mut value_expr.as_mut().0, type_env));
                for arm in arms {
                    instantiations.extend(in_value_expr(&mut arm.value_expr.0, type_env));
                }

                return instantiations
            },
            ValueExpr::FormattedString(items) => {
                let mut instantiations = vec![];
                for item in items {
                    if let ValFmtStringContents::Expr(value_expr) = item {
                        instantiations.extend(in_value_expr(&mut value_expr.0, type_env));
                    }
                }

                return instantiations
            },
            ValueExpr::Continue
            | ValueExpr::Break => {},
        }

        println!("return none for {value_expr:?}");
        vec![]
    }

    let x = in_value_expr(&mut function_definition.value_expr.0, type_env);
    println!("{}", function_definition.name.clone());
    return x
}

fn typeresolve_function_definition(
    function_definition: &mut FunctionDefintion,
    type_env: &mut TypeEnv,
) {
    if let Some(generics) = function_definition.generics.as_mut() {
        generics
            .iter()
            .for_each(|(generic, _)| {
                type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
            });
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
            *value_expr = ValueExpr::Variable(true, ident, Some(type_expr));
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;

            // Resolve the type expression on the declaration
            resolve_all_aliases_type_expr(&mut declaration.type_expr.0, type_env);

            type_env.insert_identifier_type(
                declaration.name.clone(),
                declaration.type_expr.0.clone(),
            );

            if let Some(initializer) = &mut declaration.initializer {
                typeresolve_value_expr(&mut initializer.0, type_env);
            }
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
            type_params,
        } => {
            // todo: let's discuss if we should use asserts for compiler internal errors.
            // example right below this issue
            assert_eq!(
                *type_params,
                None,
                "type_params should be omitted by now"
            );

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
        ValueExpr::String(str) => {
            type_env.insert_type(TypeExpr::ConstString(str.clone()));
        }
        ValueExpr::Int(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Break
        | ValueExpr::Return(None)
        | ValueExpr::Continue => {}
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

pub fn replace_generics_in_value_expr(
    value_expr: &mut ValueExpr,
    generics_to_concrete_type_map: &HashMap<String, &TypeExpr>,
) {
    match value_expr {
        ValueExpr::Array(ty, exprs) => {
            if let Some(t) = ty {
                replace_generics_in_type_expr(&mut t.0, generics_to_concrete_type_map);
            }
            for expr in exprs {
                replace_generics_in_value_expr(&mut expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                type_expr,
                initializer,
                ..
            } = &mut d.0;
            replace_generics_in_type_expr(&mut type_expr.0, generics_to_concrete_type_map);
            if let Some(init) = initializer {
                replace_generics_in_value_expr(&mut init.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                params,
                return_type,
                value_expr,
            } = &mut **l;
            if let Some(rt) = return_type {
                replace_generics_in_type_expr(&mut rt.0, generics_to_concrete_type_map);
            }
            for (_, p) in params {
                replace_generics_in_type_expr(&mut p.0, generics_to_concrete_type_map);
            }
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
        }
        ValueExpr::Add(l, r)
        | ValueExpr::Mul(l, r)
        | ValueExpr::Equals(l, r)
        | ValueExpr::ArrayAccess(l, r) => {
            replace_generics_in_value_expr(&mut l.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut r.0, generics_to_concrete_type_map);
        }
        ValueExpr::Block(exprs) | ValueExpr::Tuple(exprs) => {
            for expr in exprs {
                replace_generics_in_value_expr(&mut expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Duck(fields) | ValueExpr::Struct(fields) => {
            for (_, field_val) in fields {
                replace_generics_in_value_expr(&mut field_val.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::BoolNegate(e) => {
            replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            replace_generics_in_value_expr(&mut target_obj.0, generics_to_concrete_type_map);
        }
        ValueExpr::FormattedString(contents) => {
            for content in contents {
                if let ValFmtStringContents::Expr(e) = content {
                    replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            replace_generics_in_value_expr(&mut target.0, generics_to_concrete_type_map);
            for p in params {
                replace_generics_in_value_expr(&mut p.0, generics_to_concrete_type_map);
            }
            if let Some(t_params) = type_params {
                for tp in t_params {
                    replace_generics_in_type_expr(&mut tp.0, generics_to_concrete_type_map);
                }
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            replace_generics_in_value_expr(&mut condition.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut then.0, generics_to_concrete_type_map);
            if let Some(e) = r#else {
                replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Match { value_expr, arms } => {
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
            for arm in arms {
                replace_generics_in_type_expr(&mut arm.type_case.0, generics_to_concrete_type_map);
                replace_generics_in_value_expr(&mut arm.value_expr.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(e) = r {
                replace_generics_in_value_expr(&mut e.0, generics_to_concrete_type_map);
            }
        }
        ValueExpr::VarAssign(a) => {
            let Assignment { target, value_expr } = &mut a.0;
            replace_generics_in_value_expr(&mut target.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut value_expr.0, generics_to_concrete_type_map);
        }
        ValueExpr::While { condition, body } => {
            replace_generics_in_value_expr(&mut condition.0, generics_to_concrete_type_map);
            replace_generics_in_value_expr(&mut body.0, generics_to_concrete_type_map);
        }

        ValueExpr::Break
        | ValueExpr::Continue
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..) => {}
    }
}

pub fn replace_generics_in_type_expr(
    type_expr: &mut TypeExpr,
    generics_to_concrete_type_map: &HashMap<String, &TypeExpr>,
) {
    match type_expr {
        TypeExpr::GenericToBeReplaced(name) | TypeExpr::TypeName(_, name, _) => {
            if let Some(concrete_type) = generics_to_concrete_type_map.get(name) {
                *type_expr = (*concrete_type).clone();
            }
        }
        TypeExpr::Struct(Struct { fields }) | TypeExpr::Duck(Duck { fields }) => {
            for field in fields {
                replace_generics_in_type_expr(&mut field.type_expr.0, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Tuple(elements) | TypeExpr::Or(elements) => {
            for (element_expr, _) in elements {
                replace_generics_in_type_expr(element_expr, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Fun(params, return_type) => {
            for (_, param_type_expr) in params {
                replace_generics_in_type_expr(&mut param_type_expr.0, generics_to_concrete_type_map);
            }
            if let Some(rt_box) = return_type {
                replace_generics_in_type_expr(&mut rt_box.0, generics_to_concrete_type_map);
            }
        }
        TypeExpr::Array(element_type_expr) => {
            replace_generics_in_type_expr(&mut element_type_expr.0, generics_to_concrete_type_map);
        }
        TypeExpr::RawTypeName(_, _, type_params) => {
            if let Some(params) = type_params {
                for (param_expr, _) in params {
                    replace_generics_in_type_expr(param_expr, generics_to_concrete_type_map);
                }
            }
        }
        TypeExpr::Any
        | TypeExpr::InlineGo
        | TypeExpr::Go(_)
        | TypeExpr::TypeNameInternal(_)
        | TypeExpr::ConstString(_)
        | TypeExpr::ConstInt(_)
        | TypeExpr::ConstBool(_)
        | TypeExpr::String
        | TypeExpr::Int
        | TypeExpr::Bool
        | TypeExpr::Char
        | TypeExpr::Float => {}
    }
}
