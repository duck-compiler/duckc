use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
    sync::mpsc::Sender,
};

use chumsky::container::Container;
use colored::Colorize;

use crate::{
    parse::{
        SS, Spanned, SpannedMutRef,
        duckx_component_parser::DuckxComponent,
        extensions_def_parser::ExtensionsDef,
        failure_with_occurence,
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        source_file_parser::SourceFile,
        struct_parser::StructDefinition,
        test_parser::TestCase,
        tsx_component_parser::{
            Edit, TsxComponent, TsxComponentDependencies, TsxSourceUnit, do_edits,
        },
        type_parser::{Duck, TypeDefinition, TypeExpr},
        value_parser::{
            Assignment, Declaration, ValFmtStringContents, ValHtmlStringContents, ValueExpr,
        },
    },
    semantics::{
        ident_mangler::mangle,
        typechecker::{check_type_compatability, check_type_compatability_full},
    },
    tags::Tag,
};

#[derive(Debug, Clone)]
pub enum GenericDefinition {
    Function(FunctionDefintion),
    Type(TypeDefinition),
    Struct(StructDefinition),
}

fn typeresolve_duckx_component(c: &mut DuckxComponent, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    type_env.insert_identifier_type("props".to_string(), c.props_type.0.clone(), false);
    type_env.all_types.push(c.props_type.0.clone());
    typeresolve_value_expr((&mut c.value_expr.0, c.value_expr.1), type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_extensions_def(extensions_def: &mut ExtensionsDef, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    type_env.insert_identifier_type(
        "self".to_string(),
        extensions_def.target_type_expr.0.clone(),
        false,
    );

    type_env
        .all_types
        .push(extensions_def.target_type_expr.0.clone());
    let type_expr = extensions_def.target_type_expr.clone();
    for extension_method in &mut extensions_def.function_definitions {
        let extension_function_name = type_expr
            .0
            .build_extension_access_function_name(&extension_method.0.name.clone(), type_env);
        if type_env
            .extension_functions
            .iter()
            .any(|(existing, _)| *existing == extension_function_name)
        {
            continue;
        }

        let underlying_fn_type = extension_method.0.type_expr().0;

        let access_fn_type = TypeExpr::Fun(
            vec![(Some("self".to_string()), type_expr.clone())],
            Some(Box::new(extension_method.0.type_expr())),
            // todo: mutable extension fns?
            false,
        );

        type_env.extension_functions.insert(
            extension_function_name,
            (underlying_fn_type.clone(), access_fn_type.clone()),
        );

        type_env.insert_type(access_fn_type);
        type_env.insert_type(underlying_fn_type);

        typeresolve_function_definition(&mut extension_method.0, type_env);
    }
}

fn typeresolve_test_case(test_case: &mut TestCase, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    typeresolve_value_expr((&mut test_case.body.0, test_case.body.1), type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_tsx_component(c: &mut TsxComponent, type_env: &mut TypeEnv) {
    type_env.all_types.push(c.props_type.0.clone());
    let units = c.find_units();
    let mut edits = Vec::new();
    for (range, unit) in units.iter() {
        match unit {
            TsxSourceUnit::Jsx => {
                edits.push((range.start_byte, Edit::Insert("html`".to_string())));
                edits.push((range.end_byte, Edit::Insert("`".to_string())));
            }
            TsxSourceUnit::OpeningJsx => edits.push((range.start_byte, Edit::Delete(2))),
            TsxSourceUnit::ClosingJsx => edits.push((range.start_byte, Edit::Delete(3))),
            TsxSourceUnit::Expression => {
                if range.start_byte > 0
                    && &c.typescript_source.0[range.start_byte - 1..(range.start_byte)] != "$"
                {
                    edits.push((range.start_byte, Edit::Insert("$".to_string())))
                }
            }
            TsxSourceUnit::Ident => {
                // here we could implement rpc calls
                let ident = &c.typescript_source.0[range.start_byte..range.end_byte];

                if let Some(found_comp) = type_env.get_component(ident) {
                    let found_name = found_comp.name.clone();
                    println!("{} -> {}", c.name, found_name);
                    type_env
                        .get_component_dependencies(c.name.clone())
                        .client_components
                        .push(found_name);
                }
            }
        }
    }
    do_edits(&mut c.typescript_source.0, &mut edits);
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
            Self::Struct(struct_def) => struct_def
                .generics
                .as_ref()
                .expect("StructDefinition shouldn't be wrapped inside a GenericDefinition, when it doesn't have any generics.")
                .iter()
                .map(|generic| generic.0.name.clone())
                .collect::<Vec<_>>(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct FunHeader {
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Option<Spanned<TypeExpr>>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'a> {
    pub identifier_types: Vec<HashMap<String, (TypeExpr, bool)>>,
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub all_types: Vec<TypeExpr>,
    pub extension_functions: HashMap<String, (TypeExpr, TypeExpr)>, // key = extension function name, (actual_fn_type, access_fn_type)

    pub function_headers: HashMap<String, FunHeader>,
    pub function_definitions: Vec<FunctionDefintion>,
    pub tsx_components: Vec<TsxComponent>,
    pub duckx_components: Vec<DuckxComponent>,
    pub tsx_component_dependencies: HashMap<String, TsxComponentDependencies>,
    pub struct_definitions: Vec<StructDefinition>,
    pub generic_fns_generated: Vec<FunctionDefintion>,
    pub generic_structs_generated: Vec<StructDefinition>,
    pub generic_methods_generated: HashMap<String, Vec<FunctionDefintion>>,
    pub prevent_struct_generation: HashSet<String>,
    pub tailwind_sender: Option<&'a Sender<String>>,
}

impl Default for TypeEnv<'_> {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],
            all_types: vec![],
            extension_functions: HashMap::new(),
            tsx_components: Vec::new(),
            duckx_components: Vec::new(),
            tsx_component_dependencies: HashMap::new(),
            function_headers: HashMap::new(),
            function_definitions: Vec::new(),
            struct_definitions: Vec::new(),
            generic_fns_generated: Vec::new(),
            generic_structs_generated: Vec::new(),
            generic_methods_generated: HashMap::new(),
            prevent_struct_generation: HashSet::new(),
            tailwind_sender: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypesSummary {
    pub types_used: Vec<TypeExpr>,
    pub param_names_used: Vec<String>,
}

impl TypeEnv<'_> {
    pub fn check_for_tailwind(&self, s: &String) {
        if let Some(sender) = self.tailwind_sender.as_ref() {
            sender.send(s.to_owned()).expect("tailwind channel closed");
        }
    }

    pub fn has_component(&self, name: &str) -> bool {
        self.tsx_components.iter().any(|x| x.name.as_str() == name)
    }

    pub fn get_component_dependencies(&mut self, name: String) -> &mut TsxComponentDependencies {
        self.tsx_component_dependencies.entry(name).or_default()
    }

    pub fn get_duckx_component(&self, name: &str) -> Option<&DuckxComponent> {
        self.duckx_components.iter().find(|x| x.name == name)
    }

    pub fn get_full_component_dependencies(&mut self, name: String) -> HashSet<String> {
        let mut out = self
            .tsx_component_dependencies
            .entry(name.clone())
            .or_default()
            .client_components
            .clone()
            .into_iter()
            .flat_map(|dep| {
                let mut v = self.get_full_component_dependencies(dep.clone());
                v.push(dep.clone());
                v.into_iter()
            })
            .collect::<HashSet<_>>();
        if self.get_component(name.as_str()).is_some() {
            out.insert(name);
        }
        out
    }

    pub fn get_component(&self, name: &str) -> Option<&TsxComponent> {
        self.tsx_components.iter().find(|x| x.name.as_str() == name)
    }

    pub fn has_method_header(&self, name: &str) -> bool {
        self.function_headers.contains_key(name)
    }

    pub fn get_method_header(&self, name: &str) -> FunHeader {
        self.function_headers
            .get(name)
            .cloned()
            .or_else(|| {
                self.function_definitions
                    .iter()
                    .find(|x| x.name.as_str() == name)
                    .map(|x| x.to_header())
            })
            .or_else(|| {
                self.generic_fns_generated
                    .iter()
                    .find(|x| x.name.as_str() == name)
                    .map(|x| x.to_header())
            })
            .unwrap()
    }

    pub fn get_struct_def_opt<'a>(&'a self, name: &str) -> Option<&'a StructDefinition> {
        self.struct_definitions
            .iter()
            .chain(self.generic_structs_generated.iter())
            .find(|x| x.name.as_str() == name)
    }

    pub fn get_struct_def<'a>(&'a self, name: &str) -> &'a StructDefinition {
        self.get_struct_def_opt(name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
    }

    pub fn get_struct_def_mut<'a>(&'a mut self, name: &str) -> &'a mut StructDefinition {
        self.struct_definitions
            .iter_mut()
            .find(|x| x.name.as_str() == name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
    }

    pub fn get_generic_methods(&mut self, type_name: String) -> &mut Vec<FunctionDefintion> {
        self.generic_methods_generated.entry(type_name).or_default()
    }

    pub fn has_generic_method(&self, type_name: &str, method_name: &str) -> bool {
        self.generic_methods_generated
            .get(type_name)
            .is_some_and(|x| x.iter().any(|x| x.name == method_name))
    }

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

    pub fn insert_identifier_type(
        &mut self,
        identifier: String,
        type_expr: TypeExpr,
        is_const: bool,
    ) {
        self.insert_type(type_expr.clone());
        self.identifier_types
            .last_mut()
            .expect("At least one env should exist. :(")
            .insert(identifier, (type_expr, is_const));
    }

    pub fn insert_type(&mut self, type_expr: TypeExpr) -> TypeExpr {
        self.all_types.push(type_expr.clone());
        return type_expr;
    }

    pub fn get_identifier_type(&self, identifier: String) -> Option<TypeExpr> {
        self.identifier_types
            .last()
            .expect("At least one env should exist. :(")
            .get(&identifier)
            .cloned()
            .map(|(ty, _)| ty)
    }

    pub fn get_identifier_type_and_const(&self, identifier: String) -> Option<(TypeExpr, bool)> {
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

    pub fn try_resolve_type_expr(&self, expr: &TypeExpr) -> TypeExpr {
        match expr {
            TypeExpr::TypeName(_, name, _) | TypeExpr::TypeNameInternal(name) => self
                .try_resolve_type_alias(name)
                .unwrap_or_else(|| expr.clone()),
            _ => expr.clone(),
        }
    }

    pub fn try_resolve_type_alias(&self, alias: &String) -> Option<TypeExpr> {
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

                return Some(res);
            }
        }

        if let Some(def) = self
            .generic_structs_generated
            .iter()
            .find(|x| x.name.as_str() == alias.as_str())
        {
            return Some(TypeExpr::Struct(def.name.clone()));
        }

        None
    }

    pub fn resolve_type_alias(&self, alias: &String) -> TypeExpr {
        self.try_resolve_type_alias(alias).unwrap_or_else(|| {
            panic!(
                "Couldn't resolve type alias {alias} on stack #{}",
                self.type_aliases.len()
            )
        })
    }

    fn flatten_types(
        &mut self,
        type_expr: &mut TypeExpr,
        param_names_used: &mut Vec<String>,
    ) -> Vec<TypeExpr> {
        let mut found = vec![];

        match type_expr {
            TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
                found.extend(self.flatten_types(&mut t.0, param_names_used));
            }
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
            TypeExpr::Tuple(types) => types.iter_mut().for_each(|type_expr| {
                found.extend(self.flatten_types(&mut type_expr.0, param_names_used));
            }),
            TypeExpr::Fun(params, return_type, _) => {
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
        // dbg!(all_types.iter().filter(|x| x.is_struct() && x.type_id(self).contains("Xyz")).collect::<Vec<_>>());
        all_types.extend(TypeExpr::primitives());
        all_types.extend(
            self.tsx_components
                .iter()
                .map(|x| x.props_type.0.clone())
                .collect::<Vec<_>>(),
        );
        all_types.extend(
            self.duckx_components
                .iter()
                .map(|x| x.props_type.0.clone())
                .collect::<Vec<_>>(),
        );
        all_types.push(TypeExpr::Tuple(vec![]));
        let mut param_names_used = Vec::new();

        let mut to_push = Vec::new();
        all_types.iter_mut().for_each(|type_expr| {
            to_push.append(&mut self.flatten_types(type_expr, &mut param_names_used));
        });

        all_types.append(&mut to_push);

        // todo: fix this panic workaround
        let org = std::panic::take_hook();
        std::panic::set_hook(Box::new(|_| {}));
        all_types.retain(|e| {
            let mut cloned = self.clone();
            std::panic::catch_unwind(move || {
                e.type_id(&mut cloned);
                e.as_clean_go_type_name(&mut cloned);
                e.as_go_type_annotation(&mut cloned);
                e.as_clean_user_faced_type_name();
            })
            .is_ok()
        });

        std::panic::set_hook(org);

        all_types.sort_by_key(|type_expr| type_expr.type_id(self));
        all_types.dedup_by_key(|type_expr| type_expr.type_id(self));

        param_names_used.dedup();

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

fn resolve_all_aliases_type_expr(expr: &mut TypeExpr, env: &mut TypeEnv) {
    match expr {
        TypeExpr::RawTypeName(_, typename, generic_params) => {
            if typename.len() != 1 {
                panic!()
            }

            if let Some(generic_params) = generic_params {
                for (g, _) in generic_params {
                    resolve_all_aliases_type_expr(g, env);
                }
            }

            *expr = env.resolve_type_alias(typename.first().unwrap());
        }
        TypeExpr::Duck(Duck { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                resolve_all_aliases_type_expr(&mut field.type_expr.0, env);
            }
        }
        TypeExpr::Array(d) => resolve_all_aliases_type_expr(&mut d.0, env),
        TypeExpr::Fun(params, return_type, _) => {
            if let Some(r) = return_type {
                resolve_all_aliases_type_expr(&mut r.0, env);
            }

            params.iter_mut().for_each(|(_, x)| {
                resolve_all_aliases_type_expr(&mut x.0, env);
            });
        }
        TypeExpr::Or(exprs) => {
            for expr in exprs {
                resolve_all_aliases_type_expr(&mut expr.0, env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for field in fields.iter_mut() {
                resolve_all_aliases_type_expr(&mut field.0, env);
            }
        }
        TypeExpr::TypeName(_, name, _) => {
            *expr = env.resolve_type_alias(name);
        }
        TypeExpr::TypeOf(identifier) => {
            let type_expr = env
                .identifier_types
                .last()
                .expect("expected at least one identifiert types map to be on the stack")
                .get(identifier)
                .expect("sorry bro didn't work :(");

            *expr = type_expr.0.clone()
        }
        TypeExpr::KeyOf(type_expr) => {
            resolve_all_aliases_type_expr(&mut type_expr.0, env);
            let span = type_expr.as_ref().1;
            let type_expr: &mut TypeExpr = &mut type_expr.as_mut().0;

            fn do_it(type_expr: &TypeExpr, span: &SS, env: &TypeEnv) -> TypeExpr {
                match &type_expr {
                    TypeExpr::Duck(duck) => {
                        let fields = duck
                            .fields
                            .iter()
                            .map(|field| (TypeExpr::Tag(field.name.clone()), field.type_expr.1))
                            .collect::<Vec<_>>();

                        return TypeExpr::Or(fields);
                    }
                    TypeExpr::Struct(struct_name) => {
                        let struct_def = env.get_struct_def(struct_name);
                        let fields = struct_def
                            .fields
                            .iter()
                            .map(|field| (TypeExpr::Tag(field.name.clone()), field.type_expr.1))
                            .collect::<Vec<_>>();

                        return TypeExpr::Or(fields);
                    }
                    TypeExpr::RawTypeName(_, typename, _) => {
                        let resolved_type = env.resolve_type_alias(typename.first().unwrap());
                        return do_it(&resolved_type, span, env);
                    }
                    TypeExpr::Alias(alias) => {
                        return do_it(&alias.type_expression.0, span, env);
                    }
                    TypeExpr::Array(arr) => {
                        return TypeExpr::Array(Box::new((
                            do_it(&arr.as_ref().0, span, env),
                            *span,
                        )));
                    }
                    TypeExpr::Or(variants) => {
                        let keyof_variants = variants
                            .iter()
                            .map(|(variant, variant_span)| {
                                (do_it(variant, span, env), *variant_span)
                            })
                            .collect::<Vec<_>>();
                        return TypeExpr::Or(keyof_variants);
                    }
                    e => {
                        panic!("compiler error: didn't match {e:?} in process_keyof_in_typ_expr")
                    }
                };
            }
            let mut final_type = do_it(type_expr, &span, env);
            resolve_all_aliases_type_expr(&mut final_type, env);
            *expr = final_type;
        }
        _ => {}
    }
}

fn process_keyof_in_type_expr(expr: &mut TypeExpr, type_env: &mut TypeEnv) {
    match expr {
        TypeExpr::KeyOf(type_expr) => {
            let span = type_expr.as_ref().1;
            let type_expr: &mut TypeExpr = &mut type_expr.as_mut().0;

            fn do_it(type_expr: &TypeExpr, span: &SS, type_env: &mut TypeEnv) -> TypeExpr {
                match &type_expr {
                    TypeExpr::Duck(duck) => {
                        let fields = duck
                            .fields
                            .iter()
                            .map(|field| (TypeExpr::Tag(field.name.clone()), field.type_expr.1))
                            .collect::<Vec<_>>();

                        return TypeExpr::Or(fields);
                    }
                    TypeExpr::Struct(struct_name) => {
                        let struct_def = type_env.get_struct_def(struct_name);
                        let fields = struct_def
                            .fields
                            .iter()
                            .map(|field| (TypeExpr::Tag(field.name.clone()), field.type_expr.1))
                            .collect::<Vec<_>>();

                        return TypeExpr::Or(fields);
                    }
                    TypeExpr::RawTypeName(_, typename, _) => {
                        let resolved_type = type_env.resolve_type_alias(typename.first().unwrap());
                        return do_it(&resolved_type, span, type_env);
                    }
                    TypeExpr::Alias(alias) => {
                        return do_it(&alias.type_expression.0, span, type_env);
                    }
                    TypeExpr::Array(arr) => {
                        return TypeExpr::Array(Box::new((
                            do_it(&arr.as_ref().0, span, type_env),
                            *span,
                        )));
                    }
                    e => {
                        panic!("compiler error: didn't match {e:?} in process_keyof_in_typ_expr")
                    }
                };
            }
            let final_type = do_it(type_expr, &span, type_env);
            *expr = final_type;
        }
        TypeExpr::Array(t) => {
            process_keyof_in_type_expr(&mut t.0, type_env);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                process_keyof_in_type_expr(&mut f.type_expr.0, type_env);
            }
        }
        TypeExpr::Alias(alias) => {
            process_keyof_in_type_expr(&mut alias.type_expression.0, type_env);
        }
        _ => {}
    }
}

fn replace_generics_in_struct_definition(
    def: &mut StructDefinition,
    generics: &HashMap<String, TypeExpr>,
) {
    for f in def.fields.iter_mut() {
        replace_generics_in_type_expr(&mut f.type_expr.0, generics);
    }

    for m in def.methods.iter_mut() {
        for t in m.return_type.iter_mut().map(|x| &mut x.0).chain(
            m.params
                .iter_mut()
                .flat_map(|x| x.iter_mut().map(|x| &mut x.1.0)),
        ) {
            replace_generics_in_type_expr(t, generics);
        }
        replace_generics_in_value_expr(&mut m.value_expr.0, generics);
    }
}

fn instantiate_generics_type_expr(expr: &mut Spanned<TypeExpr>, type_env: &mut TypeEnv) {
    match &mut expr.0 {
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
            instantiate_generics_type_expr(t.as_mut(), type_env)
        }
        TypeExpr::Html => {}
        // todo: support generics in typeof
        TypeExpr::TypeOf(..) => {}
        TypeExpr::KeyOf(type_expr) => {
            instantiate_generics_type_expr(type_expr.as_mut(), type_env);
        }
        TypeExpr::Alias(alias) => {
            instantiate_generics_type_expr(&mut alias.type_expression, type_env);
        }
        TypeExpr::Array(t) => {
            instantiate_generics_type_expr(t.as_mut(), type_env);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                instantiate_generics_type_expr(&mut f.type_expr, type_env);
            }
        }
        TypeExpr::Fun(params, ret, _) => {
            for p in params {
                instantiate_generics_type_expr(&mut p.1, type_env);
            }
            if let Some(ret) = ret {
                instantiate_generics_type_expr(ret.as_mut(), type_env);
            }
        }
        TypeExpr::Or(contents) => {
            for c in contents {
                instantiate_generics_type_expr(c, type_env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                instantiate_generics_type_expr(f, type_env);
            }
        }
        TypeExpr::TypeName(_, name, type_params) => {
            let (mut new_type, mut new_type_params) =
                resolve_by_string(name.as_str(), type_params.as_ref().cloned(), type_env);

            let type_params = new_type_params.clone();

            if let Some(e) = new_type.as_mut() {
                if let TypeExpr::Struct(def) = e {
                    if let Some(new_type_params) = new_type_params.as_mut() {
                        for new_param in new_type_params.iter_mut() {
                            instantiate_generics_type_expr(new_param, type_env);
                        }
                    } else {
                        new_type_params = Some(vec![]);
                    }

                    let def = type_env.get_struct_def(def.as_str()).clone();

                    let span = &expr.1;

                    if let Some(type_params) = &type_params {
                        if let Some(def_type_params) = def.generics.as_ref() {
                            if type_params.len() != def_type_params.len() {
                                failure_with_occurence(
                                    format!(
                                        "{name} takes {} type parameters. You provided {}",
                                        def_type_params.len(),
                                        type_params.len()
                                    ),
                                    *span,
                                    [if type_params.len() < def_type_params.len() {
                                        ("Add missing type parameters".to_string(), *span)
                                    } else {
                                        ("Remove extra type parameters".to_string(), *span)
                                    }],
                                );
                            }

                            for (provided_type, def_type) in
                                type_params.iter().zip(def_type_params.iter())
                            {
                                if let Some(constraint) = def_type.0.constraint.as_ref() {
                                    check_type_compatability(constraint, provided_type, type_env);
                                }
                            }
                        } else {
                            failure_with_occurence(
                                format!("{name} does not take type parameters"),
                                *span,
                                [("Remove the type parameters".to_string(), *span)],
                            );
                        }
                    } else if def.generics.is_some() {
                        failure_with_occurence(
                            format!("A {name} takes type parameters and you provided none"),
                            *span,
                            [("Provide type parameters".to_string(), *span)],
                        );
                    }

                    let mangled_name = mangle_generics_name(
                        def.name.as_str(),
                        new_type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                        type_env,
                    );

                    if !type_env.prevent_struct_generation.contains(&mangled_name) {
                        type_env
                            .prevent_struct_generation
                            .push(mangled_name.clone());
                        let generics_instance = def
                            .generics
                            .as_ref()
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|x| &x.0.name)
                            .zip(
                                new_type_params
                                    .as_ref()
                                    .unwrap_or(&vec![])
                                    .iter()
                                    .map(|x| &x.0),
                            )
                            .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                                acc.insert(param_name.clone(), param_inst.clone());
                                acc
                            });

                        let mut cloned_def = def.clone();
                        cloned_def.name = mangled_name.clone();
                        cloned_def.generics = None;
                        replace_generics_in_struct_definition(&mut cloned_def, &generics_instance);

                        // println!("AAAAA replaced: {cloned_def:?}");
                        type_env.generic_structs_generated.push(cloned_def.clone());

                        for f in &mut cloned_def.fields {
                            instantiate_generics_type_expr(&mut f.type_expr, type_env);
                        }

                        for m in &mut cloned_def.methods {
                            if m.generics.is_some() {
                                continue;
                            }
                            for ty in m.return_type.iter_mut().chain(
                                m.params
                                    .iter_mut()
                                    .flat_map(|x| x.iter_mut())
                                    .map(|x| &mut x.1),
                            ) {
                                instantiate_generics_type_expr(ty, type_env);
                            }
                            instantiate_generics_value_expr(&mut m.value_expr, type_env);
                        }

                        // println!("AAAA  pushing {cloned_def:?}");
                        type_env
                            .generic_structs_generated
                            .retain(|x| x.name != mangled_name);
                        type_env.generic_structs_generated.push(cloned_def.clone());
                        type_env.struct_definitions.push(cloned_def);
                        *e = TypeExpr::Struct(mangled_name);
                    } else {
                        *e = TypeExpr::Struct(mangled_name);
                    }
                }
                expr.0 = e.clone();
                instantiate_generics_type_expr(expr, type_env);
            }
        }
        TypeExpr::Any
        | TypeExpr::Char
        | TypeExpr::Bool(..)
        | TypeExpr::Int(..)
        | TypeExpr::String(..)
        | TypeExpr::Float
        | TypeExpr::Tag(..)
        | TypeExpr::Go(..)
        | TypeExpr::RawTypeName(..)
        | TypeExpr::TypeNameInternal(..)
        | TypeExpr::Struct(..)
        | TypeExpr::InlineGo => {}
        TypeExpr::And(variants) => {
            for variant in variants.iter_mut() {
                instantiate_generics_type_expr(variant, type_env);
            }
        }
    }
}

fn replace_generics_in_value_expr(expr: &mut ValueExpr, set_params: &HashMap<String, TypeExpr>) {
    match expr {
        ValueExpr::As(v, t) => {
            replace_generics_in_value_expr(&mut v.0, set_params);
            replace_generics_in_type_expr(&mut t.0, set_params);
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            replace_generics_in_value_expr(&mut target.0, set_params);
            replace_generics_in_value_expr(&mut block.0, set_params);
        }
        ValueExpr::Deref(t) | ValueExpr::Ref(t) | ValueExpr::RefMut(t) => {
            replace_generics_in_value_expr(&mut t.0, set_params)
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            replace_generics_in_value_expr(&mut lhs.0, set_params);
            replace_generics_in_value_expr(&mut rhs.0, set_params);
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    replace_generics_in_value_expr(&mut e.0, set_params);
                }
            }
        }
        ValueExpr::BoolNegate(e) | ValueExpr::Return(Some(e)) => {
            replace_generics_in_value_expr(&mut e.0, set_params)
        }
        ValueExpr::Array(exprs) => {
            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params);
            }
        }
        ValueExpr::ArrayAccess(target, index) => {
            replace_generics_in_value_expr(&mut target.0, set_params);
            replace_generics_in_value_expr(&mut index.0, set_params);
        }
        ValueExpr::Block(exprs) => {
            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params);
            }
        }
        ValueExpr::Duck(def) => {
            for (_, expr) in def {
                replace_generics_in_value_expr(&mut expr.0, set_params);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            replace_generics_in_value_expr(&mut target_obj.0, set_params);
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    replace_generics_in_value_expr(&mut e.0, set_params);
                }
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            ..
        } => {
            for v in [&mut target.0]
                .into_iter()
                .chain(params.iter_mut().map(|x| &mut x.0))
            {
                replace_generics_in_value_expr(v, set_params);
            }
            for t in type_params.iter_mut().flat_map(|x| x.iter_mut()) {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }
        }
        ValueExpr::ExtensionAccess { target_obj, .. } => {
            replace_generics_in_value_expr(&mut target_obj.as_mut().0, set_params);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            replace_generics_in_value_expr(&mut condition.0, set_params);
            replace_generics_in_value_expr(&mut then.0, set_params);
            if let Some(r#else) = r#else {
                replace_generics_in_value_expr(&mut r#else.0, set_params);
            }
        }
        ValueExpr::Lambda(def) => {
            for p in &mut def.params {
                replace_generics_in_type_expr(&mut p.1.0, set_params);
            }
            if let Some(return_type) = def.return_type.as_mut() {
                replace_generics_in_type_expr(&mut return_type.0, set_params);
            }
            replace_generics_in_value_expr(&mut def.value_expr.0, set_params);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            if let Some(replacement) = set_params.get(name) {
                match replacement {
                    TypeExpr::TypeName(_, new_name, _new_params) => {
                        *name = new_name.clone();
                        // *type_params = new_params.as_ref().cloned();
                    }
                    _ => panic!("invalid"),
                }
            }

            for f in fields {
                replace_generics_in_value_expr(&mut f.1.0, set_params);
            }

            for t in type_params.as_mut().unwrap_or(&mut vec![]).iter_mut() {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }
        }
        ValueExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_value_expr(&mut f.0, set_params);
            }
        }
        ValueExpr::While { condition, body } => {
            replace_generics_in_value_expr(&mut condition.0, set_params);
            replace_generics_in_value_expr(&mut body.0, set_params);
        }
        ValueExpr::VarDecl(decl) => {
            if let Some(type_expr) = &mut decl.0.type_expr {
                replace_generics_in_type_expr(&mut type_expr.0, set_params);
            }
            replace_generics_in_value_expr(&mut decl.0.initializer.0, set_params);
        }
        ValueExpr::VarAssign(a) => {
            replace_generics_in_value_expr(&mut a.0.target.0, set_params);
            replace_generics_in_value_expr(&mut a.0.value_expr.0, set_params);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            replace_generics_in_value_expr(&mut value_expr.0, set_params);
            for arm in arms {
                replace_generics_in_type_expr(&mut arm.type_case.0, set_params);
                if let Some(condition) = &mut arm.condition {
                    replace_generics_in_value_expr(&mut condition.0, set_params);
                }
                replace_generics_in_value_expr(&mut arm.value_expr.0, set_params);
            }

            if let Some(arm) = else_arm {
                replace_generics_in_type_expr(&mut arm.type_case.0, set_params);
                if let Some(condition) = &mut arm.condition {
                    replace_generics_in_value_expr(&mut condition.0, set_params);
                }
                replace_generics_in_value_expr(&mut arm.value_expr.0, set_params);
            }
        }
        ValueExpr::Bool(..)
        | ValueExpr::Break
        | ValueExpr::Char(..)
        | ValueExpr::String(..)
        | ValueExpr::Continue
        | ValueExpr::Float(..)
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Return(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Variable(..) => {}
    }
}

fn replace_generics_in_type_expr(expr: &mut TypeExpr, set_params: &HashMap<String, TypeExpr>) {
    match expr {
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
            replace_generics_in_type_expr(&mut t.0, set_params)
        }
        TypeExpr::Html => {}
        TypeExpr::TypeOf(..) => {}
        TypeExpr::KeyOf(type_expr) => {
            replace_generics_in_type_expr(&mut type_expr.as_mut().0, set_params);
        }
        TypeExpr::Alias(alias) => {
            replace_generics_in_type_expr(&mut alias.type_expression.0, set_params);
        }
        TypeExpr::Array(t) => {
            replace_generics_in_type_expr(&mut t.0, set_params);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                replace_generics_in_type_expr(&mut f.type_expr.0, set_params);
            }
        }
        TypeExpr::Fun(params, ret, _) => {
            for p in params {
                replace_generics_in_type_expr(&mut p.1.0, set_params);
            }
            if let Some(ret) = ret {
                replace_generics_in_type_expr(&mut ret.0, set_params);
            }
        }
        TypeExpr::Or(contents) => {
            for c in contents {
                replace_generics_in_type_expr(&mut c.0, set_params);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_type_expr(&mut f.0, set_params);
            }
        }
        TypeExpr::TypeName(_, name, generics) => {
            if let Some(generics) = generics {
                for (g, _) in generics {
                    replace_generics_in_type_expr(g, set_params);
                }
            }
            if let Some(replacement) = set_params.get(name) {
                *expr = replacement.clone();
            }
        }
        TypeExpr::Any
        | TypeExpr::Char
        | TypeExpr::Bool(..)
        | TypeExpr::Int(..)
        | TypeExpr::Float
        | TypeExpr::Go(..)
        | TypeExpr::String(..)
        | TypeExpr::Struct(..)
        | TypeExpr::TypeNameInternal(..)
        | TypeExpr::Tag(..)
        | TypeExpr::InlineGo => {}
        TypeExpr::RawTypeName(_, typename, _) => {
            if typename.len() == 1
                && let Some(replacement) = set_params.get(&typename[0])
            {
                *expr = replacement.clone();
            }
        }
        TypeExpr::And(variants) => {
            for variant in variants.iter_mut() {
                replace_generics_in_type_expr(&mut variant.0, set_params);
            }
        }
    }
}

fn resolve_by_string(
    s: &str,
    mut type_params: Option<Vec<(TypeExpr, SS)>>,
    type_env: &mut TypeEnv,
) -> (Option<TypeExpr>, Option<Vec<(TypeExpr, SS)>>) {
    let mut resolved = type_env.try_resolve_type_alias(&s.to_string());
    while let Some(TypeExpr::Alias(ref def)) = resolved {
        let mut gen_instance_map = HashMap::new();
        if let Some(generics) = &def.generics
            && let Some(ref user_generics) = type_params
        {
            for (generic, val_to_set) in generics.iter().map(|x| &x.0).zip(user_generics.iter()) {
                let generic_name = generic.name.clone();
                if let Some(constraint) = &generic.constraint {
                    check_type_compatability(constraint, val_to_set, type_env);
                }

                gen_instance_map.insert(generic_name, val_to_set.0.clone());
            }
        }
        let mut rhs = def.type_expression.0.clone();
        // println!("def {rhs:?} {gen_instance_map:?}");
        replace_generics_in_type_expr(&mut rhs, &gen_instance_map);
        // println!("replaced def {rhs:?}");
        match &rhs {
            TypeExpr::TypeName(_, typename_name, typename_generics) => {
                let next = type_env.try_resolve_type_alias(typename_name);
                if let Some(next) = next {
                    match next {
                        TypeExpr::Alias(..) => {
                            type_params = typename_generics.as_ref().cloned();
                            resolved = Some(next);
                            println!("case alias");
                        }
                        _ => {
                            return (Some(next), typename_generics.as_ref().cloned());
                        }
                    }
                } else {
                    break;
                }
            }
            _ => {
                return (Some(rhs), type_params);
            }
        }
    }
    (resolved, type_params)
}

fn mangle_generics_name(
    base: &str,
    params: &[Spanned<TypeExpr>],
    type_env: &mut TypeEnv,
) -> String {
    format!("{base}{}", {
        let r = params
            .iter()
            .map(|x| x.0.as_clean_go_type_name(type_env))
            .collect::<Vec<_>>()
            .join("_");
        if r.is_empty() { r } else { format!("_{r}") }
    })
}

fn instantiate_generics_value_expr(expr: &mut Spanned<ValueExpr>, type_env: &mut TypeEnv) {
    match &mut expr.0 {
        ValueExpr::As(v, t) => {
            instantiate_generics_value_expr(v.as_mut(), type_env);
            instantiate_generics_type_expr(t, type_env);
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            instantiate_generics_value_expr(target.as_mut(), type_env);
            instantiate_generics_value_expr(block.as_mut(), type_env);
        }
        ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            instantiate_generics_value_expr(v.as_mut(), type_env)
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            instantiate_generics_value_expr(lhs.as_mut(), type_env);
            instantiate_generics_value_expr(rhs.as_mut(), type_env);
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    instantiate_generics_value_expr(e, type_env);
                }
            }
        }
        ValueExpr::BoolNegate(e) | ValueExpr::Return(Some(e)) => {
            instantiate_generics_value_expr(e.as_mut(), type_env)
        }
        ValueExpr::Array(exprs) => {
            for e in exprs {
                instantiate_generics_value_expr(e, type_env);
            }
        }
        ValueExpr::ArrayAccess(target, index) => {
            instantiate_generics_value_expr(target.as_mut(), type_env);
            instantiate_generics_value_expr(index.as_mut(), type_env);
        }
        ValueExpr::Block(exprs) => {
            for e in exprs {
                instantiate_generics_value_expr(e, type_env);
            }
        }
        ValueExpr::Duck(def) => {
            for (_, expr) in def {
                instantiate_generics_value_expr(expr, type_env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            instantiate_generics_value_expr(target_obj.as_mut(), type_env);
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    instantiate_generics_value_expr(e, type_env);
                }
            }
        }
        ValueExpr::ExtensionAccess { target_obj, .. } => {
            instantiate_generics_value_expr(target_obj.as_mut(), type_env);
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            ..
        } => {
            instantiate_generics_value_expr(target.as_mut(), type_env);
            for t in type_params.iter_mut().flat_map(|x| x.iter_mut()) {
                instantiate_generics_type_expr(t, type_env);
            }
            for p in params {
                instantiate_generics_value_expr(p, type_env);
            }

            if let ValueExpr::Variable(_, var_name, _, _) = &mut target.0 {
                let mangled_name = mangle_generics_name(
                    var_name.as_str(),
                    type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                    type_env,
                );
                let fn_def = type_env
                    .function_definitions
                    .iter()
                    .find(|x| x.name.as_str() == var_name.as_str());
                if let Some(fn_def) = fn_def {
                    if !type_env.prevent_struct_generation.contains(&mangled_name) {
                        type_env
                            .prevent_struct_generation
                            .push(mangled_name.clone());

                        let generics_instance = fn_def
                            .generics
                            .as_ref()
                            .unwrap_or(&vec![])
                            .iter()
                            .map(|x| &x.0.name)
                            .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                            .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                                acc.insert(param_name.clone(), param_inst.clone());
                                acc
                            });

                        let mut cloned_def = fn_def.clone();
                        cloned_def.name = mangled_name.clone();
                        cloned_def.generics = None;

                        for t in cloned_def
                            .params
                            .iter_mut()
                            .flat_map(|x| x.iter_mut().map(|x| &mut x.1))
                            .chain(cloned_def.return_type.iter_mut())
                        {
                            replace_generics_in_type_expr(&mut t.0, &generics_instance);
                            instantiate_generics_type_expr(t, type_env);
                            process_keyof_in_type_expr(&mut t.0, type_env);
                        }

                        replace_generics_in_value_expr(
                            &mut cloned_def.value_expr.0,
                            &generics_instance,
                        );
                        instantiate_generics_value_expr(&mut cloned_def.value_expr, type_env);

                        for m in type_env.identifier_types.iter_mut() {
                            m.insert(
                                cloned_def.name.clone(),
                                (
                                    TypeExpr::Fun(
                                        cloned_def
                                            .params
                                            .clone()
                                            .map(|x| {
                                                x.iter()
                                                    .map(|x| (Some(x.0.clone()), x.1.clone()))
                                                    .collect()
                                            })
                                            .unwrap_or_default(),
                                        cloned_def.return_type.clone().map(Box::new),
                                        true,
                                    ),
                                    true,
                                ),
                            );
                        }

                        type_env.generic_fns_generated.push(cloned_def);
                        *var_name = mangled_name;
                        *type_params = None;
                    } else {
                        *var_name = mangled_name;
                        *type_params = None;
                    }
                }
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            instantiate_generics_value_expr(condition.as_mut(), type_env);
            instantiate_generics_value_expr(then.as_mut(), type_env);
            if let Some(r#else) = r#else {
                instantiate_generics_value_expr(r#else.as_mut(), type_env);
            }
        }
        ValueExpr::Lambda(def) => {
            for p in &mut def.params {
                instantiate_generics_type_expr(&mut p.1, type_env);
            }
            if let Some(return_type) = def.return_type.as_mut() {
                instantiate_generics_type_expr(return_type, type_env);
            }
            instantiate_generics_value_expr(&mut def.value_expr, type_env);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            let (new_type, mut new_type_params) =
                resolve_by_string(name.as_str(), type_params.clone(), type_env);

            if let Some(new_type_params) = new_type_params.as_mut() {
                for new_param in new_type_params.iter_mut() {
                    instantiate_generics_type_expr(new_param, type_env);
                }
                *type_params = Some(new_type_params.clone());
            }

            for field in fields {
                instantiate_generics_value_expr(&mut field.1, type_env);
            }

            if let Some(TypeExpr::Struct(ref def)) = new_type {
                let def = type_env.get_struct_def(def.as_str()).clone();
                let span = &expr.1;

                if let Some(type_params) = &type_params {
                    if let Some(def_type_params) = def.generics.as_ref() {
                        if type_params.len() != def_type_params.len() {
                            failure_with_occurence(
                                format!(
                                    "{name} takes {} type parameters. You provided {}",
                                    def_type_params.len(),
                                    type_params.len()
                                ),
                                *span,
                                [if type_params.len() < def_type_params.len() {
                                    ("Add missing type parameters".to_string(), *span)
                                } else {
                                    ("Remove extra type parameters".to_string(), *span)
                                }],
                            );
                        }

                        for (provided_type, def_type) in
                            type_params.iter().zip(def_type_params.iter())
                        {
                            if let Some(constraint) = def_type.0.constraint.as_ref() {
                                check_type_compatability(constraint, provided_type, type_env);
                            }
                        }
                    } else {
                        failure_with_occurence(
                            format!("{name} does not take type parameters"),
                            *span,
                            [("Remove the type parameters".to_string(), *span)],
                        );
                    }
                } else if def.generics.is_some() {
                    failure_with_occurence(
                        format!("B {name} takes type parameters and you provided none"),
                        *span,
                        [("Provide type parameters".to_string(), *span)],
                    );
                }

                let mangled_name = mangle_generics_name(
                    def.name.as_str(),
                    new_type_params.as_ref().unwrap_or(&vec![]).as_slice(),
                    type_env,
                );
                if !type_env.prevent_struct_generation.contains(&mangled_name) {
                    type_env
                        .prevent_struct_generation
                        .push(mangled_name.clone());
                    let generics_instance = def
                        .generics
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|x| &x.0.name)
                        .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                        .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                            acc.insert(param_name.clone(), param_inst.clone());
                            acc
                        });

                    let mut cloned_def = def.clone();
                    cloned_def.name = mangled_name.clone();
                    cloned_def.generics = None;
                    replace_generics_in_struct_definition(&mut cloned_def, &generics_instance);

                    // println!("replaced: {cloned_def:?}");

                    type_env.generic_structs_generated.push(cloned_def.clone());
                    for f in &mut cloned_def.fields {
                        instantiate_generics_type_expr(&mut f.type_expr, type_env);
                    }

                    for m in &mut cloned_def.methods {
                        if m.generics.is_some() {
                            continue;
                        }
                        for ty in m.return_type.iter_mut().chain(
                            m.params
                                .iter_mut()
                                .flat_map(|x| x.iter_mut())
                                .map(|x| &mut x.1),
                        ) {
                            instantiate_generics_type_expr(ty, type_env);
                        }
                        instantiate_generics_value_expr(&mut m.value_expr, type_env);
                    }

                    // println!("pushing {cloned_def:?}");
                    type_env
                        .generic_structs_generated
                        .retain(|x| x.name != mangled_name);
                    type_env.generic_structs_generated.push(cloned_def.clone());
                    type_env.struct_definitions.push(cloned_def);
                    *name = mangled_name.clone();
                    *type_params = None;
                } else {
                    *name = mangled_name;
                    *type_params = None;
                }
            }

            println!("got {name:?} and {type_params:?}");
            // std::process::exit(0);
        }
        ValueExpr::Tuple(fields) => {
            for f in fields {
                instantiate_generics_value_expr(f, type_env);
            }
        }
        ValueExpr::While { condition, body } => {
            instantiate_generics_value_expr(condition.as_mut(), type_env);
            instantiate_generics_value_expr(body.as_mut(), type_env);
        }
        ValueExpr::VarDecl(decl) => {
            if let Some(type_expr) = &mut decl.0.type_expr {
                instantiate_generics_type_expr(type_expr, type_env);
            }

            instantiate_generics_value_expr(&mut decl.0.initializer, type_env);
        }
        ValueExpr::VarAssign(a) => {
            instantiate_generics_value_expr(&mut a.0.target, type_env);
            instantiate_generics_value_expr(&mut a.0.value_expr, type_env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            instantiate_generics_value_expr(value_expr.as_mut(), type_env);
            for arm in arms {
                instantiate_generics_type_expr(&mut arm.type_case, type_env);
                instantiate_generics_value_expr(&mut arm.value_expr, type_env);
            }

            if let Some(arm) = else_arm {
                instantiate_generics_type_expr(&mut arm.type_case, type_env);
                instantiate_generics_value_expr(&mut arm.value_expr, type_env);
            }
        }
        ValueExpr::Bool(..)
        | ValueExpr::Break
        | ValueExpr::Char(..)
        | ValueExpr::String(..)
        | ValueExpr::Continue
        | ValueExpr::Float(..)
        | ValueExpr::InlineGo(..)
        | ValueExpr::Int(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Return(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Variable(..) => {}
    }
}

pub fn sort_fields_value_expr(expr: &mut ValueExpr) {
    match expr {
        ValueExpr::As(v, t) => {
            sort_fields_value_expr(&mut v.0);
            sort_fields_type_expr(&mut t.0);
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            sort_fields_value_expr(&mut target.0);
            sort_fields_value_expr(&mut block.0);
        }
        ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            sort_fields_value_expr(&mut v.0)
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    sort_fields_value_expr(&mut e.0);
                }
            }
        }
        ValueExpr::Array(exprs) => {
            for expr in exprs {
                sort_fields_value_expr(&mut expr.0);
            }
        }
        ValueExpr::VarDecl(d) => {
            let Declaration {
                name: _,
                type_expr,
                initializer,
                is_const: _,
            } = &mut d.0;
            if let Some(type_expr) = type_expr {
                sort_fields_type_expr(&mut type_expr.0);
            }

            sort_fields_value_expr(&mut initializer.0);
        }
        ValueExpr::Lambda(l) => {
            let LambdaFunctionExpr {
                is_mut: _,
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
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            sort_fields_value_expr(&mut lhs.0);
            sort_fields_value_expr(&mut rhs.0);
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
        ValueExpr::ExtensionAccess { target_obj, .. } => {
            sort_fields_value_expr(&mut target_obj.as_mut().0);
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params: _,
            ..
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
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            sort_fields_value_expr(&mut value_expr.0);
            for arm in arms {
                sort_fields_type_expr(&mut arm.type_case.0);
                if let Some(condition) = &mut arm.condition {
                    sort_fields_value_expr(&mut condition.0);
                }
                sort_fields_value_expr(&mut arm.value_expr.0);
            }

            if let Some(arm) = else_arm {
                sort_fields_type_expr(&mut arm.type_case.0);
                if let Some(condition) = &mut arm.condition {
                    sort_fields_value_expr(&mut condition.0);
                }
                sort_fields_value_expr(&mut arm.value_expr.0);
            }
        }
        ValueExpr::Return(r) => {
            if let Some(r) = r {
                sort_fields_value_expr(&mut r.0);
            }
        }
        ValueExpr::Struct { fields, .. } => {
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
        | ValueExpr::Tag(..)
        | ValueExpr::Bool(..) => {}
    }
}

pub fn is_const_var(v: &ValueExpr) -> bool {
    matches!(v, ValueExpr::Variable(_, _, _, Some(true)))
}

pub fn sort_fields_type_expr(expr: &mut TypeExpr) {
    match expr {
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => sort_fields_type_expr(&mut t.0),
        TypeExpr::Html => {}
        TypeExpr::TypeOf(..) => {}
        TypeExpr::KeyOf(type_expr) => {
            sort_fields_type_expr(&mut type_expr.0);
        }
        TypeExpr::Alias(t) => {
            sort_fields_type_expr(&mut t.type_expression.0);
        }
        TypeExpr::RawTypeName(..) => {}
        TypeExpr::Duck(Duck { fields }) => {
            fields.sort_by_key(|x| x.name.clone());
            for field in fields {
                sort_fields_type_expr(&mut field.type_expr.0);
            }
        }
        TypeExpr::Array(d) => sort_fields_type_expr(&mut d.0),
        TypeExpr::Fun(params, r, _) => {
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
        | TypeExpr::Bool(_)
        | TypeExpr::Char
        | TypeExpr::Float
        | TypeExpr::Go(_)
        | TypeExpr::InlineGo
        | TypeExpr::Int(_)
        | TypeExpr::String(_)
        | TypeExpr::TypeName(..)
        | TypeExpr::Struct(..)
        | TypeExpr::Tag(..)
        | TypeExpr::TypeNameInternal(..) => {}
        TypeExpr::And(variants) => {
            for variant in variants.iter_mut() {
                sort_fields_type_expr(&mut variant.0);
            }
        }
    }
}

pub fn typeresolve_struct_def(def: &mut StructDefinition, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();
    type_env.insert_type_alias("Self".to_string(), TypeExpr::Struct(def.name.clone()));

    for f in &mut def.fields {
        resolve_all_aliases_type_expr(&mut f.type_expr.0, type_env);
    }

    for m in &mut def.methods {
        if m.generics.is_some() {
            continue;
        }
        if let Some(return_type) = &mut m.return_type {
            resolve_all_aliases_type_expr(&mut return_type.0, type_env);
        }

        if let Some(params) = m.params.clone().as_mut() {
            for p in params {
                resolve_all_aliases_type_expr(&mut p.1.0, type_env);
            }
        }
    }

    for m in &mut def.methods {
        if m.generics.is_some() {
            continue;
        }
        type_env.push_identifier_types();

        if let Some(params) = m.params.clone().as_mut() {
            for p in params {
                type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false);
            }
        }

        type_env.insert_identifier_type(
            "self".to_string(),
            if def.mut_methods.contains(&m.name) {
                TypeExpr::RefMut(TypeExpr::Struct(def.name.clone()).into_empty_span().into())
            } else {
                TypeExpr::Ref(TypeExpr::Struct(def.name.clone()).into_empty_span().into())
            },
            true,
        );
        typeresolve_value_expr((&mut m.value_expr.0, m.value_expr.1), type_env);
        if m.name == "fisch" {
            println!(
                "method resolve {} {} {:?}",
                def.name, m.name, m.value_expr.0
            );
        }
        type_env.pop_identifier_types();
    }

    type_env.pop_type_aliases();
    let ty_expr = TypeExpr::Struct(def.name.clone());
    type_env.insert_type_alias(def.name.clone(), ty_expr.clone());
    type_env.all_types.insert(0, ty_expr);
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    println!("{} sort fields", Tag::TypeResolve);

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

    source_file
        .tsx_components
        .iter_mut()
        .for_each(|function_definition| {
            sort_fields_type_expr(&mut function_definition.props_type.0);
        });

    source_file
        .duckx_components
        .iter_mut()
        .for_each(|function_definition| {
            sort_fields_type_expr(&mut function_definition.props_type.0);
            sort_fields_value_expr(&mut function_definition.value_expr.0);
        });

    source_file
        .test_cases
        .iter_mut()
        .for_each(|test_case| sort_fields_value_expr(&mut test_case.body.0));

    println!("{} insert type definitions", Tag::TypeResolve);

    // Step 2: Insert type definitions
    source_file
        .struct_definitions
        .iter()
        .for_each(|struct_definition| {
            type_env.struct_definitions.push(struct_definition.clone());
            type_env.insert_type_alias(
                struct_definition.name.clone(),
                TypeExpr::Struct(struct_definition.name.clone()),
            );
        });

    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_def| {
            if let TypeExpr::And(_) = &mut type_def.type_expression.0 {
                type_def.type_expression.0 =
                    translate_interception_to_duck(&type_def.type_expression.0);
            }
        });

    source_file
        .type_definitions
        .iter()
        .for_each(|type_definition| {
            type_env.insert_type_alias(
                type_definition.name.clone(),
                TypeExpr::Alias(Box::new(type_definition.clone())),
            );
        });

    source_file.tsx_components.iter().for_each(|tsx_component| {
        type_env.insert_identifier_type(
            tsx_component.name.clone(),
            TypeExpr::Fun(
                vec![(Some("props".to_string()), tsx_component.props_type.clone())],
                Some(Box::new((
                    TypeExpr::Tuple(vec![
                        (TypeExpr::String(None), tsx_component.typescript_source.1),
                        (TypeExpr::String(None), tsx_component.typescript_source.1),
                    ]),
                    tsx_component.typescript_source.1,
                ))),
                true,
            ),
            true,
        );
    });

    source_file
        .duckx_components
        .iter()
        .for_each(|duckx_component| {
            type_env.insert_identifier_type(
                duckx_component.name.clone(),
                TypeExpr::Fun(
                    vec![(
                        Some("props".to_string()),
                        duckx_component.props_type.clone(),
                    )],
                    Some(Box::new((TypeExpr::Html, duckx_component.value_expr.1))),
                    true,
                ),
                true,
            );
        });

    for fn_def in &source_file.function_definitions {
        type_env.function_definitions.push(fn_def.clone());
        let mut function_type = fn_def.type_expr().0;

        if let TypeExpr::Fun(_, return_type, _) = &mut function_type
            && let Some(return_type_box) = return_type
            && let TypeExpr::And(_) = &return_type_box.0
        {
            return_type_box.0 = translate_interception_to_duck(&return_type_box.0);
        }

        type_env.insert_identifier_type(fn_def.name.clone(), function_type, true);
    }

    for comp in &source_file.tsx_components {
        type_env.tsx_components.push(comp.clone());
        type_env.check_for_tailwind(&comp.typescript_source.0);
    }

    for comp in &source_file.duckx_components {
        type_env.duckx_components.push(comp.clone());
    }

    println!(
        "{} resolve aliases in function signatures and prepare function types",
        Tag::TypeResolve,
    );

    // Step 3: Find generic instantiations
    source_file.function_definitions.iter_mut().for_each(|x| {
        if x.generics.is_some() {
            return;
        }
        for t in x
            .params
            .iter_mut()
            .flat_map(|x| x.iter_mut().map(|x| &mut x.1))
            .chain(x.return_type.iter_mut())
        {
            instantiate_generics_type_expr(t, type_env);
        }

        instantiate_generics_value_expr(&mut x.value_expr, type_env);
    });

    for func in &type_env.generic_fns_generated {
        source_file.function_definitions.push(func.clone());
    }

    // Step 3: Resolve aliases in function signatures and prepare function types
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            if function_definition.generics.is_some() {
                return;
            }

            if let Some(params) = function_definition.params.as_mut() {
                for (_, p) in params {
                    instantiate_generics_type_expr(p, type_env);
                    resolve_all_aliases_type_expr(&mut p.0, type_env);
                    process_keyof_in_type_expr(&mut p.0, type_env);
                }
            }

            if let Some(r) = function_definition.return_type.as_mut() {
                instantiate_generics_type_expr(r, type_env);
                resolve_all_aliases_type_expr(&mut r.0, type_env);
                process_keyof_in_type_expr(&mut r.0, type_env);
            }

            let mut fn_type_expr = TypeExpr::Fun(
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
                true,
            );

            if let TypeExpr::Fun(_, return_type, _) = &mut fn_type_expr
                && let Some(return_type_box) = return_type
                && let TypeExpr::And(_) = &return_type_box.0
            {
                return_type_box.0 = translate_interception_to_duck(&return_type_box.0);
            }

            type_env.insert_identifier_type(function_definition.name.clone(), fn_type_expr, true);
        });
    println!("{} typeresolve functions", Tag::TypeResolve);
    println!("{} final resolve of all functions", Tag::TypeResolve);

    for s in &mut source_file.tsx_components {
        typeresolve_tsx_component(s, type_env);
    }

    for s in &mut source_file.duckx_components {
        typeresolve_duckx_component(s, type_env);
    }

    // TODO: typeresolve for tests can be disabled when not in test mode
    for test_case in &mut source_file.test_cases {
        typeresolve_test_case(test_case, type_env);
    }

    type_env.tsx_components = source_file.tsx_components.clone();
    type_env.duckx_components = source_file.duckx_components.clone();

    for extensions_def in &mut source_file.extensions_defs {
        typeresolve_extensions_def(extensions_def, type_env);
    }

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_defintion| {
            if function_defintion.generics.is_some() {
                return;
            }
            typeresolve_function_definition(function_defintion, type_env);
        });

    for s in &type_env.generic_structs_generated {
        source_file.struct_definitions.push(s.clone());
    }

    let mut cloned = type_env.generic_methods_generated.clone();

    for (struct_name, methods) in cloned.iter_mut() {
        for m in methods.iter_mut() {
            type_env.push_identifier_types();
            let StructDefinition {
                name: _,
                fields: _,
                methods: _,
                mut_methods,
                generics: _,
            } = type_env.get_struct_def(struct_name);
            type_env.insert_identifier_type(
                "self".to_string(),
                if mut_methods.contains(&m.name) {
                    TypeExpr::RefMut(
                        TypeExpr::Struct(struct_name.clone())
                            .into_empty_span()
                            .into(),
                    )
                } else {
                    TypeExpr::Ref(
                        TypeExpr::Struct(struct_name.clone())
                            .into_empty_span()
                            .into(),
                    )
                },
                true,
            );
            typeresolve_function_definition(m, type_env);
            type_env.pop_identifier_types();
        }
    }

    type_env.generic_methods_generated = cloned;

    let mut cloned = type_env.generic_fns_generated.clone();

    for m in cloned.iter_mut() {
        typeresolve_function_definition(m, type_env);
    }

    type_env.generic_fns_generated = cloned;

    // Step 5: Final resolve of all functions
    source_file
        .struct_definitions
        .iter_mut()
        .for_each(|struct_definition| {
            if struct_definition.generics.is_some() {
                return;
            }
            typeresolve_struct_def(struct_definition, type_env);
        });

    for x in &type_env.generic_fns_generated {
        if !source_file
            .function_definitions
            .iter()
            .any(|y| x.name.as_str() == y.name.as_str())
        {
            source_file.function_definitions.push(x.clone());
        }
    }

    type_env.struct_definitions = source_file.struct_definitions.clone();
    type_env.function_definitions = source_file.function_definitions.clone();
}

fn typeresolve_function_definition(
    function_definition: &mut FunctionDefintion,
    type_env: &mut TypeEnv,
) {
    if let Some(generics) = function_definition.generics.as_mut() {
        generics.iter().for_each(|(generic, _)| {
            type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
        });
    }

    if let Some((return_type, _span)) = &mut function_definition.return_type {
        if let TypeExpr::And(_) = &return_type {
            *return_type = translate_interception_to_duck(return_type);
        }
        *return_type = type_env.insert_type(return_type.clone());
    }

    type_env.push_identifier_types();

    if let Some(params) = function_definition.params.clone().as_mut() {
        for p in params {
            type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false);
        }
    }

    typeresolve_value_expr(
        (
            &mut function_definition.value_expr.0,
            function_definition.value_expr.1,
        ),
        type_env,
    );

    type_env.pop_identifier_types();
}

pub fn translate_interception_to_duck(interception_type: &TypeExpr) -> TypeExpr {
    match interception_type {
        TypeExpr::And(variants) => {
            let mut all_fields = Vec::new();

            for variant in variants.iter() {
                match &variant.0 {
                    TypeExpr::Duck(duck) => {
                        for field in &duck.fields {
                            all_fields.push(field.clone());
                        }
                    }
                    _ => {
                        continue;
                    }
                }
            }

            let mut unique_fields = Vec::new();
            let mut seen_names = HashSet::new();

            for field in all_fields.into_iter().rev() {
                if seen_names.insert(field.name.clone()) {
                    unique_fields.push(field);
                }
            }

            unique_fields.reverse();
            TypeExpr::Duck(Duck {
                fields: unique_fields,
            })
        }
        _ => interception_type.clone(),
    }
}

fn typeresolve_value_expr(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let span = &value_expr.1;
    let value_expr = value_expr.0;
    match value_expr {
        ValueExpr::As(v, t) => {
            type_env.insert_type(t.0.clone());
            typeresolve_value_expr((&mut v.0, v.1), type_env);
            let _ = TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env);
        }
        ValueExpr::For {
            ident: (ident, is_const, ty),
            target,
            block,
        } => {
            typeresolve_value_expr((&mut target.0, target.1), type_env);
            let mut target_type = TypeExpr::from_value_expr_resolved_type_name(target, type_env);

            let mut current = &mut target_type;
            loop {
                match current {
                    TypeExpr::Ref(t) | TypeExpr::RefMut(t) => current = &mut t.0,
                    other => {
                        if let TypeExpr::Array(content_type) = other {
                            *other = content_type.0.clone();
                            break;
                        } else {
                            panic!("can only use range on array");
                        }
                    }
                }
            }

            type_env.push_identifier_types();
            *ty = Some(target_type.clone());
            type_env.insert_identifier_type(ident.clone(), target_type, *is_const);
            typeresolve_value_expr((&mut block.0, block.1), type_env);
            type_env.pop_identifier_types();
        }
        ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            typeresolve_value_expr((&mut v.0, v.1), type_env)
        }

        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    typeresolve_value_expr((&mut e.0, e.1), type_env);
                }

                if let ValHtmlStringContents::String(s) = c {
                    type_env.check_for_tailwind(s);
                }
            }
        }
        ValueExpr::RawVariable(_, path) => {
            let ident = mangle(path);
            let (mut type_expr, is_const) = type_env
                .get_identifier_type_and_const(ident.clone())
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {ident}"));
            resolve_all_aliases_type_expr(&mut type_expr, type_env);
            *value_expr = ValueExpr::Variable(true, ident, Some(type_expr), Some(is_const));
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;

            typeresolve_value_expr(
                (&mut declaration.initializer.0, declaration.initializer.1),
                type_env,
            );

            // Resolve the type expression on the declaration
            if let Some(type_expr) = &mut declaration.type_expr {
                if let TypeExpr::And(_) = &type_expr.0 {
                    type_expr.0 = translate_interception_to_duck(&type_expr.0);
                }
                resolve_all_aliases_type_expr(&mut type_expr.0, type_env);
                type_env.insert_identifier_type(
                    declaration.name.clone(),
                    type_expr.0.clone(),
                    declaration.is_const,
                );
            } else {
                let type_expr = TypeExpr::from_value_expr(&declaration.initializer, type_env);
                declaration.type_expr = Some((type_expr.clone(), declaration.initializer.1));
                type_env.insert_identifier_type(
                    declaration.name.clone(),
                    type_expr,
                    declaration.is_const,
                );
            }
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                match c {
                    ValFmtStringContents::Expr(e) => {
                        typeresolve_value_expr((&mut e.0, e.1), type_env)
                    }
                    ValFmtStringContents::String(s) => {
                        type_env.insert_type(TypeExpr::String(Some(s.clone())));
                        type_env.check_for_tailwind(s);
                    }
                }
            }
        }
        ValueExpr::ArrayAccess(target, idx) => {
            let target = target.as_mut();
            let idx = idx.as_mut();
            typeresolve_value_expr((&mut target.0, target.1), type_env);
            typeresolve_value_expr((&mut idx.0, target.1), type_env);
        }
        ValueExpr::Array(exprs) => {
            if exprs.is_empty() {
                return;
            }
            for expr in exprs {
                typeresolve_value_expr((&mut expr.0, expr.1), type_env);
            }
            let ty = TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::InlineGo(..) => {}
        ValueExpr::Lambda(b) => {
            let LambdaFunctionExpr {
                is_mut,
                params,
                return_type,
                value_expr,
            } = &mut **b;

            let captured = type_env.identifier_types[1..]
                .iter()
                .flat_map(|v| v.iter())
                .fold(HashMap::new(), |mut acc, (name, (ty, is_const))| {
                    if !type_env.identifier_types[0].contains_key(name)  // don't capture top level identifiers like functions
                    && !params.iter().any(|(param_name, _)| param_name == name)
                    // don't capture shadowed variables
                    {
                        acc.insert(name.clone(), (ty.clone(), *is_mut && !*is_const));
                    }
                    acc
                });

            type_env.push_identifier_types();

            for (name, (ty, capture_as_mut)) in captured {
                type_env.insert_identifier_type(name, ty, !capture_as_mut);
            }

            for (name, ty) in params {
                type_env.insert_identifier_type(name.to_owned(), ty.0.clone(), false);
                resolve_all_aliases_type_expr(&mut ty.0, type_env);
            }

            if let Some(return_type) = return_type {
                resolve_all_aliases_type_expr(&mut return_type.0, type_env);
            }

            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
            type_env.pop_identifier_types();
        }
        ValueExpr::ExtensionAccess { target_obj, .. } => {
            let target = target_obj.as_mut();
            typeresolve_value_expr((&mut target.0, target.1), type_env);
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            ..
        } => {
            typeresolve_value_expr((&mut target.0, target.1), type_env);

            #[allow(unused_variables)]
            let header: FunHeader;

            // generic method call
            if let Some(type_params_vec) = type_params
                && let ValueExpr::FieldAccess {
                    target_obj,
                    field_name,
                } = &mut target.0
            {
                let target_ty =
                    TypeExpr::from_value_expr_resolved_type_name_dereferenced(target_obj, type_env);

                let TypeExpr::Struct(struct_name) = target_ty else {
                    panic!()
                };

                let StructDefinition {
                    name,
                    fields: _,
                    methods,
                    mut_methods: _,
                    generics,
                } = type_env.get_struct_def(struct_name.as_str()).clone();

                assert!(generics.is_none());

                let mangled_name =
                    mangle_generics_name(field_name.as_str(), type_params_vec.as_slice(), type_env);

                let mangled_name_to_check = format!("{name}_{mangled_name}");

                let method_def = methods
                    .iter()
                    .find(|m| m.name.as_str() == field_name.as_str());

                let method_def = match method_def {
                    Some(method) => method,
                    None => {
                        let span = target_obj.as_ref().1;
                        failure_with_occurence(
                            "Invalid Field Access".to_string(),
                            {
                                let mut span = span;
                                span.end += 2;
                                span
                            },
                            vec![(
                                format!(
                                    "this is of type {} and it has no field '{}'",
                                    struct_name.bright_yellow(),
                                    field_name.bright_blue()
                                ),
                                span,
                            )],
                        );
                    }
                };

                if !type_env.has_method_header(mangled_name_to_check.as_str()) {
                    let generics_instance = method_def
                        .generics
                        .as_ref()
                        .unwrap_or(&vec![])
                        .iter()
                        .map(|x| &x.0.name)
                        .zip(type_params.as_ref().unwrap_or(&vec![]).iter().map(|x| &x.0))
                        .fold(HashMap::new(), |mut acc, (param_name, param_inst)| {
                            acc.insert(param_name.clone(), param_inst.clone());
                            acc
                        });

                    let mut cloned_def = method_def.clone();
                    cloned_def.name = mangled_name.clone();
                    cloned_def.generics = None;

                    for t in cloned_def
                        .params
                        .iter_mut()
                        .flat_map(|x| x.iter_mut().map(|x| &mut x.1))
                        .chain(cloned_def.return_type.iter_mut())
                    {
                        replace_generics_in_type_expr(&mut t.0, &generics_instance);
                        instantiate_generics_type_expr(t, type_env);
                    }

                    replace_generics_in_value_expr(
                        &mut cloned_def.value_expr.0,
                        &generics_instance,
                    );
                    instantiate_generics_value_expr(&mut cloned_def.value_expr, type_env);

                    type_env
                        .function_headers
                        .insert(mangled_name_to_check.clone(), cloned_def.to_header());

                    let mut cloned = type_env.generic_structs_generated.clone();
                    for s in cloned.iter_mut() {
                        typeresolve_struct_def(s, type_env);
                    }
                    type_env.generic_structs_generated = cloned;

                    let mut cloned = type_env.generic_fns_generated.clone();
                    for s in cloned.iter_mut() {
                        typeresolve_function_definition(s, type_env);
                    }
                    type_env.generic_fns_generated = cloned;

                    let mut cloned = type_env.generic_methods_generated.clone();
                    let StructDefinition {
                        name,
                        fields: _,
                        methods: _,
                        mut_methods,
                        generics: _,
                    } = type_env.get_struct_def(struct_name.as_str()).clone();
                    for (s_name, values) in cloned.iter_mut() {
                        let StructDefinition {
                            name: _,
                            fields: _,
                            methods: _,
                            mut_methods,
                            generics: _,
                        } = type_env.get_struct_def(s_name.as_str()).clone();
                        for v in values {
                            type_env.push_identifier_types();

                            type_env.insert_identifier_type(
                                "self".to_string(),
                                if mut_methods.contains(&v.name) {
                                    TypeExpr::RefMut(
                                        TypeExpr::Struct(struct_name.clone())
                                            .into_empty_span()
                                            .into(),
                                    )
                                } else {
                                    TypeExpr::Ref(
                                        TypeExpr::Struct(struct_name.clone())
                                            .into_empty_span()
                                            .into(),
                                    )
                                },
                                true,
                            );
                            typeresolve_function_definition(v, type_env);
                            type_env.pop_identifier_types();
                        }
                    }
                    type_env.generic_methods_generated = cloned;

                    type_env.push_identifier_types();
                    type_env.insert_identifier_type(
                        "self".to_string(),
                        if mut_methods.contains(&cloned_def.name) {
                            TypeExpr::RefMut(
                                TypeExpr::Struct(struct_name.clone())
                                    .into_empty_span()
                                    .into(),
                            )
                        } else {
                            TypeExpr::Ref(
                                TypeExpr::Struct(struct_name.clone())
                                    .into_empty_span()
                                    .into(),
                            )
                        },
                        true,
                    );
                    typeresolve_function_definition(&mut cloned_def, type_env);
                    type_env.pop_identifier_types();

                    header = cloned_def.to_header();

                    type_env
                        .generic_methods_generated
                        .entry(name.clone())
                        .or_default()
                        .push(cloned_def);

                    *field_name = mangled_name;
                    *type_params = None;
                } else {
                    header = type_env.get_method_header(mangled_name_to_check.as_str());
                    *field_name = mangled_name;
                    *type_params = None;
                }
            } else {
                let TypeExpr::Fun(params, ret, _) = TypeExpr::from_value_expr(target, type_env)
                else {
                    panic!("not a func?? {target:?}")
                };
                header = FunHeader {
                    params: params.iter().map(|x| x.1.clone()).collect(),
                    return_type: ret.as_ref().map(|x| x.deref().clone()),
                };
            }

            let _header = header;

            assert!(
                type_params.is_none(),
                "type_params should be omitted by now"
            );

            params
                .iter_mut()
                .for_each(|param| typeresolve_value_expr((&mut param.0, param.1), type_env));
        }
        ValueExpr::Variable(_, identifier, type_expr_opt, const_opt) => {
            // if let Some(type_expr) = type_expr_opt {
            //     resolve_all_aliases_type_expr(type_expr, type_env);
            //     return;
            // }
            let (type_expr, is_const) = type_env
                .get_identifier_type_and_const(identifier.clone())
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {identifier}"));

            //resolve_all_aliases_type_expr(&mut type_expr, type_env, generics_to_ignore);
            *type_expr_opt = Some(type_expr);
            *const_opt = Some(is_const);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            typeresolve_value_expr((&mut condition.0, condition.1), type_env);
            type_env.push_identifier_types();
            typeresolve_value_expr((&mut then.0, then.1), type_env);
            type_env.pop_identifier_types();
            if let Some(r#else) = r#else {
                type_env.push_identifier_types();
                typeresolve_value_expr((&mut r#else.0, r#else.1), type_env);
                type_env.pop_identifier_types();
            }
        }
        ValueExpr::While { condition, body } => {
            typeresolve_value_expr((&mut condition.0, condition.1), type_env);
            type_env.push_identifier_types();
            typeresolve_value_expr((&mut body.0, body.1), type_env);
            type_env.pop_identifier_types();
        }
        ValueExpr::Tuple(value_exprs) => {
            value_exprs.iter_mut().for_each(|value_expr| {
                typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
            });
            let ty = TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::Block(value_exprs) => {
            type_env.push_identifier_types();
            value_exprs.iter_mut().for_each(|value_expr| {
                typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
            });
            type_env.pop_identifier_types();
        }
        ValueExpr::Duck(items) => {
            items.iter_mut().for_each(|(_, value_expr)| {
                typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
            });
            let ty = TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params: _,
        } => {
            if let Some(TypeExpr::Struct(s)) = type_env
                .try_resolve_type_alias(name)
                .map(|e| type_env.try_resolve_type_expr(&e))
            {
                *name = s;
            }

            let def = type_env.get_struct_def(name.as_str()).clone();

            // if let Some(type_params) = &type_params {
            //     if let Some(def_type_params) = def.generics.as_ref() {
            //         if type_params.len() != def_type_params.len() {
            //             failure_with_occurence(
            //                 format!(
            //                     "{name} takes {} type parameters. You provided {}",
            //                     def_type_params.len(),
            //                     type_params.len()
            //                 ),
            //                 *span,
            //                 [if type_params.len() < def_type_params.len() {
            //                     ("Add missing type parameters".to_string(), *span)
            //                 } else {
            //                     ("Remove extra type parameters".to_string(), *span)
            //                 }],
            //             );
            //         }

            //         for (provided_type, def_type) in type_params.iter().zip(def_type_params.iter())
            //         {
            //             if let Some(constraint) = def_type.0.constraint.as_ref() {
            //                 check_type_compatability(constraint, provided_type, type_env);
            //             }
            //         }
            //     } else {
            //         failure_with_occurence(
            //             format!("{name} does not take type parameters"),
            //             *span,
            //             [("Remove the type parameters".to_string(), *span)],
            //         );
            //     }
            // } else if def.generics.is_some() {
            //     failure_with_occurence(
            //         format!("{name} takes type parameters and you provided none"),
            //         *span,
            //         [("Provide type parameters".to_string(), *span)],
            //     );
            // }

            fields
                .iter_mut()
                .zip(def.fields.iter())
                .for_each(|((_field_name, value_expr), _)| {
                    typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
                });

            let ty = TypeExpr::from_value_expr(&(value_expr.clone(), *span), type_env);
            type_env.insert_type(ty);
        }
        ValueExpr::FieldAccess { target_obj, .. } => {
            let target_obj = target_obj.as_mut();
            typeresolve_value_expr((&mut target_obj.0, target_obj.1), type_env);
        }
        ValueExpr::Return(Some(value_expr)) => {
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
        }
        ValueExpr::VarAssign(assignment) => {
            typeresolve_value_expr(
                (&mut assignment.0.target.0, assignment.0.target.1),
                type_env,
            );
            let target_type = TypeExpr::from_value_expr(&assignment.0.target, type_env);
            typeresolve_value_expr(
                (&mut assignment.0.value_expr.0, assignment.0.value_expr.1),
                type_env,
            );

            check_type_compatability_full(
                &(target_type, assignment.0.target.1),
                &(
                    TypeExpr::from_value_expr(&assignment.0.value_expr, type_env),
                    assignment.0.value_expr.1,
                ),
                type_env,
                is_const_var(&assignment.0.target.0),
            );
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs) => {
            typeresolve_value_expr((&mut lhs.0, lhs.1), type_env);
            typeresolve_value_expr((&mut rhs.0, lhs.1), type_env);
        }
        ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            typeresolve_value_expr((&mut lhs.0, lhs.1), type_env);
            typeresolve_value_expr((&mut rhs.0, lhs.1), type_env);
        }
        ValueExpr::BoolNegate(value_expr) => {
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
            arms.iter_mut().for_each(|arm| {
                if let TypeExpr::And(_) = &arm.type_case.0 {
                    arm.type_case.0 = translate_interception_to_duck(&arm.type_case.0);
                }

                type_env.push_identifier_types();
                if let Some(identifier) = &arm.identifier_binding {
                    type_env.insert_identifier_type(
                        identifier.clone(),
                        arm.type_case.0.clone(),
                        false,
                    );
                    if let Some(condition) = &mut arm.condition {
                        typeresolve_value_expr((&mut condition.0, condition.1), type_env);
                    }
                }
                typeresolve_value_expr((&mut arm.value_expr.0, arm.value_expr.1), type_env);
                type_env.pop_identifier_types();
            });

            if let Some(arm) = else_arm {
                type_env.push_identifier_types();
                if let Some(identifier) = &arm.identifier_binding {
                    if let Some(condition) = &mut arm.condition {
                        typeresolve_value_expr((&mut condition.0, condition.1), type_env);
                    }
                    type_env.insert_identifier_type(
                        identifier.clone(),
                        arm.type_case.0.clone(),
                        false,
                    );
                }
                typeresolve_value_expr((&mut arm.value_expr.0, arm.value_expr.1), type_env);
                type_env.pop_identifier_types();
            }
        }
        ValueExpr::String(str, _) => {
            type_env.check_for_tailwind(str);
            type_env.insert_type(TypeExpr::String(Some(str.clone())));
        }
        ValueExpr::Tag(tag) => {
            type_env.insert_type(TypeExpr::Tag(tag.clone()));
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
