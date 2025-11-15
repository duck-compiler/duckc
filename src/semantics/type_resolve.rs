use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex, mpsc::Sender},
};

use chumsky::container::Container;

use crate::{
    emit::{types::interface_implementations, value::ToIr},
    parse::{
        Field, SS, Spanned, SpannedMutRef,
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
        ident_mangler::{MANGLE_SEP, mangle},
        typechecker::{check_type_compatability, check_type_compatability_full},
    },
    tags::Tag,
};

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
    pub type_definitions: Vec<TypeDefinition>,

    pub generic_fns_generated: Vec<FunctionDefintion>,
    pub generic_structs_generated: Vec<StructDefinition>,
    pub generic_methods_generated: HashMap<String, Vec<FunctionDefintion>>,
    pub prevent_generic_generation: HashSet<String>,
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
            type_definitions: Vec::new(),

            generic_fns_generated: Vec::new(),
            generic_structs_generated: Vec::new(),
            generic_methods_generated: HashMap::new(),
            prevent_generic_generation: HashSet::new(),
            tailwind_sender: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypesSummary {
    pub types_used: Vec<TypeExpr>,
    pub param_names_used: Vec<String>,
}

pub fn build_struct_generic_id(
    struct_name: &str,
    type_params: &[Spanned<TypeExpr>],
    type_env: &mut TypeEnv,
) -> Box<str> {
    let mut res = struct_name.to_string();
    for t in type_params {
        res.push('_');
        res.push_str(&t.0.as_clean_go_type_name(type_env));
    }
    res.into_boxed_str()
}

impl TypeEnv<'_> {
    pub fn get_struct_def_with_type_params_mut<'a>(
        &'a mut self,
        name: &str,
        type_params: &[Spanned<TypeExpr>],
        span: SS,
    ) -> &'a mut StructDefinition {
        let name = name;
        let type_params = type_params;

        let generic_id = build_struct_generic_id(name, type_params, self);

        if !type_params.is_empty()
            && let Some((idx, _def)) = self
                .generic_structs_generated
                .iter()
                .enumerate()
                .find(|(_, stored_def)| stored_def.name.as_str() == generic_id.as_ref())
        {
            return &mut self.generic_structs_generated[idx];
        }

        if type_params.is_empty() {
            return self
                .struct_definitions
                .iter_mut()
                .find(|d| d.name == name)
                .unwrap();
        }

        let cloned_def = self
            .struct_definitions
            .iter()
            .chain(self.generic_structs_generated.iter())
            .find(|user_struct_definition| user_struct_definition.name.as_str() == name)
            .cloned();

        if let Some(mut cloned_def) = cloned_def {
            let generic_arguments = cloned_def
                .generics
                .iter()
                .map(|(x, _)| x)
                .zip(type_params.iter())
                .fold(HashMap::new(), |mut acc, (def, arg)| {
                    if let Some(c) = def.constraint.as_ref() {
                        check_type_compatability(c, arg, self);
                    }
                    acc.insert(def.name.clone(), arg.0.clone());
                    acc
                });

            let new_struct_name = [cloned_def.name.clone()]
                .into_iter()
                .chain(
                    generic_arguments
                        .iter()
                        .map(|(_, x)| x.as_clean_go_type_name(self)),
                )
                .collect::<Vec<_>>()
                .join(MANGLE_SEP);

            cloned_def.generics = vec![];

            if self
                .prevent_generic_generation
                .insert(new_struct_name.clone())
            {
                replace_generics_in_struct_definition(&mut cloned_def, &generic_arguments);
                cloned_def.name = new_struct_name.clone();
                self.generic_structs_generated.push(cloned_def.clone());
                cloned_def.name = name.to_string();
                typeresolve_struct_def(&mut cloned_def, type_params.to_vec(), self);
                self.generic_structs_generated
                    .retain(|f| f.name.as_str() != new_struct_name.as_str());
                cloned_def.name = new_struct_name.clone();
                self.generic_structs_generated.push(cloned_def);
            }
            self.generic_structs_generated
                .iter_mut()
                .find(|f| f.name.as_str() == new_struct_name.as_str())
                .unwrap()
        } else {
            failure_with_occurence(
                format!("This struct does not exist {name}"),
                span,
                [(format!("Struct {name} does not exist"), span)],
            );
        }
    }

    pub fn check_for_tailwind(&self, s: &str) {
        if let Some(sender) = self.tailwind_sender.as_ref() {
            sender.send(s.to_string()).expect("tailwind channel closed");
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

    pub fn get_full_component_dependencies(&mut self, name: &str) -> HashSet<String> {
        let mut out = self
            .tsx_component_dependencies
            .entry(name.to_string())
            .or_default()
            .client_components
            .clone()
            .into_iter()
            .flat_map(|dep| {
                let mut v = self.get_full_component_dependencies(&dep);
                v.push(dep.clone());
                v.into_iter()
            })
            .collect::<HashSet<_>>();
        if self.get_component(name).is_some() {
            out.insert(name.to_string());
        }
        out
    }

    pub fn get_component<'a>(&'a self, name: &str) -> Option<&'a TsxComponent> {
        self.tsx_components.iter().find(|x| x.name.as_str() == name)
    }

    pub fn has_method_header(&self, name: &str) -> bool {
        self.function_headers.contains_key(name)
    }

    pub fn get_method_header(&self, name: &str) -> FunHeader {
        self.function_headers
            .get(name)
            .cloned()
            .expect(&format!("{:?}\nSearched for {name}", self.function_headers))
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
            .chain(self.generic_structs_generated.iter_mut())
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
        self.type_aliases.push(HashMap::new());
    }

    pub fn pop_type_aliases(&mut self) {
        self.type_aliases.pop();
    }

    pub fn push_identifier_types(&mut self) {
        self.identifier_types.push(HashMap::new());
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

    pub fn get_identifier_type(&self, identifier: &str) -> Option<TypeExpr> {
        self.get_identifier_type_and_const(identifier)
            .map(|(x, _)| x)
    }

    pub fn get_identifier_type_and_const(&self, identifier: &str) -> Option<(TypeExpr, bool)> {
        for i in self.identifier_types.iter().rev() {
            let r = i.get(identifier).cloned();
            if r.is_some() {
                return r;
            }
        }
        None
    }

    pub fn insert_type_alias(&mut self, alias: String, type_expr: TypeExpr) {
        self.type_aliases
            .last_mut()
            .expect("At least one type aliases hashmap should exist. :(")
            .insert(alias, type_expr);
    }

    pub fn get_type_alias(&self, alias: &str) -> Option<TypeExpr> {
        for i in self.type_aliases.iter().rev() {
            let r = i.get(alias).cloned();
            if r.is_some() {
                return r;
            }
        }
        None
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

        let hook = std::panic::take_hook();

        std::panic::set_hook(hook);

        let mut to_push = Vec::new();
        all_types.iter_mut().for_each(|type_expr| {
            to_push.append(&mut self.flatten_types(type_expr, &mut param_names_used));
        });

        all_types.append(&mut to_push);

        // let hook = std::panic::take_hook();
        // std::panic::set_hook(Box::new(|_| {}));

        all_types.retain(|f| {
            let mut cloned_env = self.clone();
            if !std::panic::catch_unwind(move || {
                interface_implementations(
                    f.as_clean_go_type_name(&mut cloned_env),
                    f,
                    &mut cloned_env,
                    &mut ToIr::default(),
                );
                f.as_go_concrete_annotation(&mut cloned_env);
                f.as_go_type_annotation(&mut cloned_env);
                f.type_id(&mut cloned_env);
            })
            .is_ok()
            {
                println!("removing {f:?}");
                false
            } else {
                true
            }
        });

        // std::panic::set_hook(hook);

        all_types.sort_by_key(|type_expr| type_expr.type_id(self));
        all_types.dedup_by_key(|type_expr| type_expr.type_id(self));

        param_names_used.dedup();

        return TypesSummary {
            types_used: all_types,
            param_names_used,
        };
    }
}

fn trav_type_expr<F1>(mut f_t: F1, v: &mut Spanned<TypeExpr>, env: &mut TypeEnv)
where
    F1: FnMut(&mut Spanned<TypeExpr>, &mut TypeEnv) + Clone,
{
    f_t(v, env);
    match &mut v.0 {
        TypeExpr::Duck(fields) => {
            for f in &mut fields.fields {
                trav_type_expr(f_t.clone(), &mut f.type_expr, env);
            }
        }
        TypeExpr::Array(a) | TypeExpr::KeyOf(a) | TypeExpr::Ref(a) | TypeExpr::RefMut(a) => {
            trav_type_expr(f_t, a, env)
        }
        TypeExpr::Bool(..)
        | TypeExpr::Tag(..)
        | TypeExpr::TypeOf(..)
        | TypeExpr::Int(..)
        | TypeExpr::String(..)
        | TypeExpr::Float
        | TypeExpr::Char
        | TypeExpr::Html
        | TypeExpr::Go(..)
        | TypeExpr::Any
        | TypeExpr::InlineGo => {}
        TypeExpr::And(types) | TypeExpr::Or(types) | TypeExpr::Tuple(types) => {
            for t in types {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
        TypeExpr::Fun(params, ret, _) => {
            for p in params {
                trav_type_expr(f_t.clone(), &mut p.1, env);
            }
            if let Some(ret) = ret.as_mut() {
                trav_type_expr(f_t.clone(), ret, env);
            }
        }
        TypeExpr::RawTypeName(_, _, type_params)
        | TypeExpr::TypeName(_, _, type_params)
        | TypeExpr::Struct {
            name: _,
            type_params,
        } => {
            for t in type_params {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
    }
}

pub fn trav_value_expr<F1, F2>(
    mut f_t: F1,
    mut f_vv: F2,
    v: &mut Spanned<ValueExpr>,
    env: &mut TypeEnv,
) where
    F1: FnMut(&mut Spanned<TypeExpr>, &mut TypeEnv) + Clone,
    F2: FnMut(&mut Spanned<ValueExpr>, &mut TypeEnv) + Clone,
{
    f_vv(v, env);

    match &mut v.0 {
        ValueExpr::Lambda(l) => {
            for v in &mut l.params {
                if let Some(t) = v.1.as_mut() {
                    trav_type_expr(f_t.clone(), t, env);
                }
            }
            if let Some(ret) = l.return_type.as_mut() {
                trav_type_expr(f_t.clone(), ret, env);
            }
        }
        ValueExpr::Return(e) => {
            if let Some(e) = e {
                trav_value_expr(f_t, f_vv, e, env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name,
        } => {
            trav_value_expr(f_t, f_vv, target_obj, env);
        }
        ValueExpr::ExtensionAccess {
            target_obj,
            extension_name,
        } => {
            trav_value_expr(f_t, f_vv, target_obj, env);
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    trav_value_expr(f_t.clone(), f_vv.clone(), e, env);
                }
            }
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    trav_value_expr(f_t.clone(), f_vv.clone(), e, env);
                }
            }
        }
        ValueExpr::Block(exprs) | ValueExpr::Tuple(exprs) => {
            for v in exprs {
                trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
            }
        }
        ValueExpr::Duck(v) => {
            for v in v {
                trav_value_expr(f_t.clone(), f_vv.clone(), &mut v.1, env);
            }
        }
        ValueExpr::Struct {
            name,
            fields,
            type_params,
        } => {
            for t in type_params {
                trav_type_expr(f_t.clone(), t, env);
            }
            for v in fields {
                trav_value_expr(f_t.clone(), f_vv.clone(), &mut v.1, env);
            }
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), lhs, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), rhs, env);
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
            is_extension_call: _,
        } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), target, env);
            for p in params {
                trav_value_expr(f_t.clone(), f_vv.clone(), p, env);
            }
            for t in type_params {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
        ValueExpr::For {
            ident: (_, _, _ident_type),
            target,
            block,
        } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), target, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), block, env);
        }
        ValueExpr::ArrayAccess(target, idx) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), target, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), idx, env);
        }
        ValueExpr::As(target, t) => {
            trav_value_expr(f_t.clone(), f_vv, target, env);
            trav_type_expr(f_t.clone(), t, env);
        }
        ValueExpr::Defer(v) => {
            trav_value_expr(f_t, f_vv, v, env);
        }
        ValueExpr::Array(exprs) => {
            for v in exprs {
                trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
            }
        }
        ValueExpr::VarAssign(assign) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), &mut assign.0.target, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), &mut assign.0.value_expr, env);
        }
        ValueExpr::VarDecl(decl) => {
            if let Some(t) = decl.0.type_expr.as_mut() {
                trav_type_expr(f_t.clone(), t, env);
            }
            trav_value_expr(f_t.clone(), f_vv.clone(), &mut decl.0.initializer, env);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), condition, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), then, env);
            if let Some(e) = r#else {
                trav_value_expr(f_t.clone(), f_vv.clone(), e, env);
            }
        }
        ValueExpr::While { condition, body } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), condition, env);
            trav_value_expr(f_t.clone(), f_vv.clone(), body, env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span,
        } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), value_expr, env);
            for arm in arms {
                if let Some(c) = arm.condition.as_mut() {
                    trav_value_expr(f_t.clone(), f_vv.clone(), c, env);
                }
                trav_type_expr(f_t.clone(), &mut arm.type_case, env);
                trav_value_expr(f_t.clone(), f_vv.clone(), &mut arm.value_expr, env);
            }
            if let Some(e) = else_arm.as_mut() {
                trav_value_expr(f_t.clone(), f_vv.clone(), &mut e.value_expr, env);
            }
        }
        ValueExpr::Int(..)
        | ValueExpr::String(..)
        | ValueExpr::Float(..)
        | ValueExpr::Char(..)
        | ValueExpr::Bool(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Break
        | ValueExpr::Continue
        | ValueExpr::Variable(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::InlineGo(..) => {}

        ValueExpr::BoolNegate(v)
        | ValueExpr::Deref(v)
        | ValueExpr::Ref(v)
        | ValueExpr::RefMut(v) => {
            trav_value_expr(f_t, f_vv, v, env);
        }
    }
}

fn resolve_all_aliases_type_expr(expr: &mut Spanned<TypeExpr>, env: &mut TypeEnv) {
    trav_type_expr(
        |f, env| match &mut f.0 {
            TypeExpr::TypeName(..) => {
                *f = resolve_type_expr(f, env);
            }
            TypeExpr::RawTypeName(_, v, _) => {
                assert_eq!(1, v.len(), "should be mangled");
                *f = resolve_type_expr(f, env);
            }
            _ => {}
        },
        expr,
        env,
    );
}

fn resolve_all_aliases_value_expr(expr: &mut Spanned<ValueExpr>, env: &mut TypeEnv) {
    trav_value_expr(
        |f, env| match &mut f.0 {
            TypeExpr::TypeName(..) => {
                *f = resolve_type_expr(f, env);
            }
            TypeExpr::RawTypeName(_, v, _) => {
                assert_eq!(1, v.len(), "should be mangled");
                *f = resolve_type_expr(f, env);
            }
            _ => {}
        },
        |f, env| {},
        expr,
        env,
    );
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
                    TypeExpr::Struct {
                        name: struct_name,
                        type_params,
                    } => {
                        let struct_def = type_env.get_struct_def_with_type_params_mut(
                            struct_name,
                            type_params,
                            *span,
                        );
                        let fields = struct_def
                            .fields
                            .iter()
                            .map(|field| (TypeExpr::Tag(field.name.clone()), field.type_expr.1))
                            .collect::<Vec<_>>();

                        return TypeExpr::Or(fields);
                    }
                    TypeExpr::RawTypeName(_, typename, _) => {
                        let resolved_type = todo!("repalce type name");
                        return do_it(&resolved_type, span, type_env);
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

fn replace_generics_in_function_definition(
    t: &mut FunctionDefintion,
    set_params: &HashMap<String, TypeExpr>,
) {
    for t in t
        .params
        .iter_mut()
        .flat_map(|v| v.iter_mut().map(|(_, x)| x))
        .chain(t.return_type.iter_mut())
    {
        replace_generics_in_type_expr(&mut t.0, set_params);
    }
    replace_generics_in_value_expr(&mut t.value_expr.0, set_params);
}

fn replace_generics_in_value_expr(expr: &mut ValueExpr, set_params: &HashMap<String, TypeExpr>) {
    match expr {
        ValueExpr::Defer(d) => replace_generics_in_value_expr(&mut d.0, set_params),
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
            for t in type_params.iter_mut() {
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
            for (_, p) in &mut def.params {
                if let Some(p) = p.as_mut() {
                    replace_generics_in_type_expr(&mut p.0, set_params);
                }
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

            for t in type_params {
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
            for (g, _) in generics {
                replace_generics_in_type_expr(g, set_params);
            }
            if let Some(replacement) = set_params.get(name) {
                *expr = replacement.clone();
            }
        }
        TypeExpr::Struct { name, type_params } => {
            if let Some(TypeExpr::TypeName(_, new_name, g)) = set_params.get(name)
                && g.is_empty()
            {
                *name = new_name.clone();
            }

            for t in type_params {
                replace_generics_in_type_expr(&mut t.0, set_params);
            }
        }
        TypeExpr::Any
        | TypeExpr::Char
        | TypeExpr::Bool(..)
        | TypeExpr::Int(..)
        | TypeExpr::Float
        | TypeExpr::Go(..)
        | TypeExpr::String(..)
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

pub fn resolve_type_expr(t: &Spanned<TypeExpr>, env: &mut TypeEnv) -> Spanned<TypeExpr> {
    let mut res = t.clone();

    loop {
        let (name, generics) = match &res.0 {
            TypeExpr::RawTypeName(_, v, generics) => {
                assert_eq!(v.len(), 1, "should be mangled");
                let name = &v[0];
                (name, generics)
            }
            TypeExpr::TypeName(_, name, generics) => (name, generics),
            TypeExpr::Struct { name, type_params } => (name, type_params),
            _ => break,
        };

        if let Some(s_def) = env.get_struct_def_opt(name)
            && !matches!(res.0, TypeExpr::Struct { .. })
        {
            res.0 = TypeExpr::Struct {
                name: s_def.name.clone(),
                type_params: generics.to_vec(),
            };
            continue;
        }

        if let Some(simple_resolved) = env.get_type_alias(name)
            && generics.is_empty()
            && res.0 != simple_resolved
        {
            res = simple_resolved.into_empty_span();
            continue;
        }

        if let Some(def) = env
            .type_definitions
            .iter()
            .find(|d| d.name.as_str() == name.as_str())
            .cloned()
        {
            if generics.len() != def.generics.len() {
                failure_with_occurence(
                    "Wrong number generic arguments",
                    res.1,
                    [("Wrong number of generic arguments", res.1)],
                );
            }

            let arguments = def
                .generics
                .iter()
                .map(|g| (g.0.name.clone(), g.0.constraint.clone()))
                .zip(generics.clone().into_iter())
                .fold(HashMap::new(), |mut acc, ((name, constraint), exp)| {
                    if let Some(constraint) = constraint {
                        check_type_compatability(&constraint, &exp, env);
                    }
                    acc.insert(name, exp.0.clone());
                    acc
                });

            let mut r = def.type_expression.clone();
            replace_generics_in_type_expr(&mut r.0, &arguments);
            res = r;
            continue;
        }

        break;
    }

    res
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

pub fn sort_fields_value_expr(expr: &mut ValueExpr) {
    match expr {
        ValueExpr::Defer(d) => sort_fields_value_expr(&mut d.0),
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
                if let Some(p) = p.as_mut() {
                    sort_fields_type_expr(&mut p.0);
                }
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
        | TypeExpr::Struct { .. }
        | TypeExpr::Tag(..) => {}
        TypeExpr::And(variants) => {
            for variant in variants.iter_mut() {
                sort_fields_type_expr(&mut variant.0);
            }
        }
    }
}

pub fn typeresolve_struct_def(
    def: &mut StructDefinition,
    type_params: Vec<Spanned<TypeExpr>>,
    type_env: &mut TypeEnv,
) {
    type_env.push_type_aliases();

    let self_type = TypeExpr::Struct {
        name: def.name.clone(),
        type_params: type_params.clone(),
    };

    type_env.insert_type_alias("Self".to_string(), self_type.clone());

    for f in &mut def.fields {
        resolve_all_aliases_type_expr(&mut f.type_expr, type_env);
    }

    for m in &mut def.methods {
        if let Some(return_type) = &mut m.return_type {
            resolve_all_aliases_type_expr(return_type, type_env);
        }

        if let Some(params) = m.params.clone().as_mut() {
            for p in params {
                resolve_all_aliases_type_expr(&mut p.1, type_env);
            }
        }
    }

    let new_struct_name = [def.name.clone()]
        .into_iter()
        .chain(
            type_params
                .iter()
                .map(|(x, _)| x.as_clean_go_type_name(type_env)),
        )
        .collect::<Vec<_>>()
        .join(MANGLE_SEP);

    let mut cloned = def.clone();
    cloned.name = new_struct_name.clone();
    type_env
        .generic_structs_generated
        .retain(|f| f.name.as_str() != new_struct_name.as_str());
    type_env.generic_structs_generated.push(cloned);

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
                TypeExpr::RefMut(self_type.clone().into_empty_span().into())
            } else {
                TypeExpr::Ref(self_type.clone().into_empty_span().into())
            },
            true,
        );
        typeresolve_value_expr((&mut m.value_expr.0, m.value_expr.1), type_env);
        type_env.pop_identifier_types();
    }

    type_env.pop_type_aliases();
    type_env.all_types.insert(0, self_type.clone());
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
            if false && struct_definition.generics.is_empty() {
                type_env.insert_type_alias(
                    struct_definition.name.clone(),
                    TypeExpr::Struct {
                        name: struct_definition.name.clone(),
                        type_params: vec![],
                    },
                );
            }
            type_env.struct_definitions.push(struct_definition.clone());
        });

    source_file
        .type_definitions
        .iter_mut()
        .for_each(|type_def| {
            if let TypeExpr::And(_) = &mut type_def.type_expression.0 {
                type_def.type_expression.0 =
                    translate_intersection_to_duck(&type_def.type_expression.0);
            }
            type_env.type_definitions.push(type_def.clone());
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
            type_env.duckx_components.push(duckx_component.clone());
        });

    for comp in &source_file.tsx_components {
        type_env.tsx_components.push(comp.clone());
        type_env.check_for_tailwind(&comp.typescript_source.0);
    }

    println!(
        "{} resolve aliases in function signatures and prepare function types",
        Tag::TypeResolve,
    );

    // Step 3: Resolve aliases in function signatures and prepare function types
    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_definition| {
            if let Some(params) = function_definition.params.as_mut() {
                for (_, p) in params {
                    resolve_all_aliases_type_expr(p, type_env);
                    process_keyof_in_type_expr(&mut p.0, type_env);
                }
            }

            if let Some(r) = function_definition.return_type.as_mut() {
                resolve_all_aliases_type_expr(r, type_env);
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
                return_type_box.0 = translate_intersection_to_duck(&return_type_box.0);
            }

            if function_definition.name.starts_with("gimme") {
                // dbg!(&function_definition.value_expr);
            }
            resolve_all_aliases_value_expr(&mut function_definition.value_expr, type_env);
            if function_definition.name.starts_with("gimme") {
                // dbg!(&function_definition.value_expr);
            }

            if function_definition.generics.is_none() {
                type_env.insert_identifier_type(
                    function_definition.name.clone(),
                    fn_type_expr,
                    true,
                );
            }
            type_env
                .function_definitions
                .push(function_definition.clone());
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
            if function_defintion.generics.is_none() {
                typeresolve_function_definition(function_defintion, type_env);
            }
        });

    source_file
        .struct_definitions
        .iter_mut()
        .for_each(|struct_definition| {
            if struct_definition.generics.is_empty() {
                typeresolve_struct_def(struct_definition, vec![], type_env);
            }
        });

    type_env.struct_definitions = source_file.struct_definitions.clone();
    type_env.function_definitions = source_file.function_definitions.clone();
}

fn typeresolve_method_definition(
    function_definition: &mut FunctionDefintion,
    self_type: Spanned<TypeExpr>,
    is_mut: bool,
    type_env: &mut TypeEnv,
) {
    if let Some(generics) = function_definition.generics.as_mut() {
        generics.iter().for_each(|(generic, _)| {
            type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
        });
    }

    if let Some((return_type, _span)) = &mut function_definition.return_type {
        if let TypeExpr::And(_) = &return_type {
            *return_type = translate_intersection_to_duck(return_type);
        }
        *return_type = type_env.insert_type(return_type.clone());
    }

    type_env.push_identifier_types();

    if let Some(params) = function_definition.params.clone().as_mut() {
        for p in params {
            type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false);
        }
    }

    type_env.insert_identifier_type(
        "self".to_string(),
        if is_mut {
            TypeExpr::RefMut(self_type.into())
        } else {
            TypeExpr::Ref(self_type.into())
        },
        true,
    );

    typeresolve_value_expr(
        (
            &mut function_definition.value_expr.0,
            function_definition.value_expr.1,
        ),
        type_env,
    );

    type_env.pop_identifier_types();
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
            *return_type = translate_intersection_to_duck(return_type);
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

pub fn translate_intersection_to_duck(interception_type: &TypeExpr) -> TypeExpr {
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

fn infer_against(v: &mut Spanned<ValueExpr>, req: &Spanned<TypeExpr>, type_env: &TypeEnv) {
    match &mut v.0 {
        ValueExpr::Ref(target) | ValueExpr::RefMut(target) => {
            if let TypeExpr::Ref(next_ty) | TypeExpr::RefMut(next_ty) = &req.0 {
                infer_against(target, next_ty, type_env);
            }
        }
        ValueExpr::Array(..) => {
            if let TypeExpr::Array(..) = &req.0 {
                let cloned = v.clone();
                v.0 = ValueExpr::As(Box::new(cloned), req.clone());
            }
        }
        ValueExpr::Tuple(exprs) => {
            if let TypeExpr::Tuple(fields) = &req.0 {
                for (e, ty) in exprs.iter_mut().zip(fields.iter()) {
                    infer_against(e, ty, type_env);
                }
            }
        }
        ValueExpr::Duck(exprs) => {
            if let TypeExpr::Duck(def_fields) = &req.0 {
                for (
                    (_, e),
                    Field {
                        name: _,
                        type_expr: ty,
                    },
                ) in exprs.iter_mut().zip(def_fields.fields.iter())
                {
                    infer_against(e, ty, type_env);
                }
            }
        }
        ValueExpr::Block(exprs) => exprs
            .last_mut()
            .iter_mut()
            .for_each(|v| infer_against(v, req, type_env)),
        ValueExpr::Lambda(expr) => {
            if let TypeExpr::Fun(params, ret_type, _) = &req.0 {
                for (expr_type, def_type) in expr
                    .params
                    .iter_mut()
                    .map(|(_, ty)| ty)
                    .zip(params.iter().map(|(_, x)| x))
                {
                    if expr_type.is_none() {
                        *expr_type = Some(def_type.clone());
                    }
                }

                if expr.return_type.is_none() {
                    expr.return_type = ret_type.as_deref().cloned();
                }
            }
        }
        ValueExpr::If {
            condition: _,
            then,
            r#else,
        } => {
            infer_against(then, req, type_env);
            if let Some(r#else) = r#else {
                infer_against(r#else, req, type_env);
            }
        }
        _ => {}
    }
}

fn typeresolve_value_expr(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let span = &value_expr.1;
    let value_expr = value_expr.0;
    match value_expr {
        ValueExpr::Defer(inner) => {
            typeresolve_value_expr((&mut inner.0, inner.1), type_env);
            if !matches!(inner.0, ValueExpr::FunctionCall { .. }) {
                let msg = "Can only defer a function call".to_string();
                failure_with_occurence(msg.clone(), *span, [(msg.clone(), inner.1)]);
            }
        }
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
            let mut target_type = TypeExpr::from_value_expr(target, type_env);

            let mut current = &mut target_type;
            let mut ref_type = 0;
            loop {
                match current {
                    TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
                        ref_type = if matches!(t.0, TypeExpr::Ref(..)) {
                            1
                        } else {
                            2
                        };
                        current = &mut t.0;
                    }
                    other => {
                        if let TypeExpr::Array(content_type) = other {
                            if ref_type > 0 {
                                target_type = if ref_type == 1 {
                                    TypeExpr::Ref(content_type.clone())
                                } else {
                                    TypeExpr::RefMut(content_type.clone())
                                };
                            } else {
                                *other = content_type.0.clone();
                            }
                            break;
                        } else {
                            let msg = "Can only use for on array".to_string();
                            failure_with_occurence(msg.clone(), target.1, [(msg.clone(), target.1)])
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
                .get_identifier_type_and_const(&ident)
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {ident}"));
            resolve_all_aliases_type_expr(&mut type_expr.clone().into_empty_span(), type_env);
            *value_expr = ValueExpr::Variable(true, ident, Some(type_expr), Some(is_const));
        }
        ValueExpr::VarDecl(declaration) => {
            let declaration = &mut declaration.0;

            // Resolve the type expression on the declaration
            if let Some(type_expr) = &mut declaration.type_expr {
                if let TypeExpr::And(_) = &type_expr.0 {
                    type_expr.0 = translate_intersection_to_duck(&type_expr.0);
                }

                resolve_all_aliases_type_expr(type_expr, type_env);
                infer_against(&mut declaration.initializer, type_expr, type_env);

                typeresolve_value_expr(
                    (&mut declaration.initializer.0, declaration.initializer.1),
                    type_env,
                );
            } else {
                typeresolve_value_expr(
                    (&mut declaration.initializer.0, declaration.initializer.1),
                    type_env,
                );

                let type_expr = TypeExpr::from_value_expr(&declaration.initializer, type_env);
                declaration.type_expr = Some((type_expr.clone(), declaration.initializer.1));
            }

            type_env.insert_identifier_type(
                declaration.name.clone(),
                declaration
                    .type_expr
                    .as_ref()
                    .cloned()
                    .expect("should be unreachable")
                    .0,
                declaration.is_const,
            );
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
                let ty = ty.as_mut().unwrap();
                type_env.insert_identifier_type(name.to_owned(), ty.0.clone(), false);
                resolve_all_aliases_type_expr(ty, type_env);
            }

            if let Some(return_type) = return_type {
                resolve_all_aliases_type_expr(return_type, type_env);
            }

            if let Some(return_type) = return_type.as_ref() {
                infer_against(value_expr, return_type, type_env);
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
            let header: FunHeader;

            if type_params.is_empty() {
                typeresolve_value_expr((&mut target.0, target.1), type_env);

                let target_type = TypeExpr::from_value_expr(target, type_env);

                let TypeExpr::Fun(def_params, def_ret, _) = target_type else {
                    failure_with_occurence(
                        "Can only call functions",
                        *span,
                        [("Can only call functions", *span)],
                    )
                };
                header = FunHeader {
                    params: def_params.into_iter().map(|(_, x)| x).collect(),
                    return_type: def_ret.as_deref().cloned(),
                };
            } else {
                match &mut target.0 {
                    ValueExpr::Variable(_, name, ty, _) => {
                        let fn_def = type_env
                            .function_definitions
                            .iter()
                            .find(|x| name.as_str() == x.name.as_str())
                            .cloned()
                            .expect(&format!("could not find {name}"));

                        if type_params.len()
                            != fn_def.generics.as_ref().map(|v| v.len()).unwrap_or(0)
                        {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, *span, [(msg, *span)])
                        }

                        let generic_arguments = fn_def
                            .generics
                            .as_ref()
                            .unwrap()
                            .iter()
                            .map(|(x, _)| x)
                            .zip(type_params.iter())
                            .fold(HashMap::new(), |mut acc, (def, arg)| {
                                if let Some(c) = def.constraint.as_ref() {
                                    check_type_compatability(c, arg, type_env);
                                }
                                acc.insert(def.name.clone(), arg.0.clone());
                                acc
                            });

                        let mut cloned_fn_def = fn_def.clone();

                        let new_fn_name = [cloned_fn_def.name.clone()]
                            .into_iter()
                            .chain(
                                generic_arguments
                                    .iter()
                                    .map(|(_, t)| t.as_clean_go_type_name(type_env)),
                            )
                            .collect::<Vec<_>>()
                            .join(MANGLE_SEP);

                        cloned_fn_def.name = new_fn_name.clone();
                        cloned_fn_def.generics = None;

                        if type_env
                            .prevent_generic_generation
                            .insert(new_fn_name.clone())
                        {
                            replace_generics_in_function_definition(
                                &mut cloned_fn_def,
                                &generic_arguments,
                            );
                            type_env.function_headers.insert(
                                new_fn_name.clone(),
                                FunHeader {
                                    params: cloned_fn_def
                                        .params
                                        .as_ref()
                                        .cloned()
                                        .unwrap()
                                        .into_iter()
                                        .map(|(_, x)| x)
                                        .collect(),
                                    return_type: cloned_fn_def.return_type.as_ref().cloned(),
                                },
                            );
                            typeresolve_function_definition(&mut cloned_fn_def, type_env);
                            type_env.generic_fns_generated.push(cloned_fn_def.clone());
                        }
                        header = type_env.get_method_header(&new_fn_name);
                        *ty = Some(TypeExpr::Fun(
                            cloned_fn_def
                                .params
                                .clone()
                                .unwrap_or_default()
                                .into_iter()
                                .map(|x| (Some(x.0), x.1))
                                .collect(),
                            cloned_fn_def.return_type.clone().map(Box::new),
                            true,
                        ));
                    }
                    ValueExpr::FieldAccess {
                        target_obj,
                        field_name,
                    } => {
                        typeresolve_value_expr((&mut target_obj.0, target_obj.1), type_env);

                        let self_type =
                            TypeExpr::from_value_expr_dereferenced(target_obj, type_env);

                        let TypeExpr::Struct {
                            name: struct_name,
                            type_params: struct_type_params,
                        } = TypeExpr::from_value_expr_dereferenced(&target_obj, type_env)
                        else {
                            let msg = "Can only generic method call a struct";
                            failure_with_occurence(msg, *span, [(msg, *span)]);
                        };

                        let mut_struct_def = type_env.get_struct_def_with_type_params_mut(
                            &struct_name,
                            &struct_type_params,
                            *span,
                        );

                        let replaced_struct_name = mut_struct_def.name.clone();

                        let method = mut_struct_def
                            .methods
                            .iter()
                            .find(|m| m.name.as_str() == field_name.as_str())
                            .cloned()
                            .unwrap();

                        if method
                            .generics
                            .as_ref()
                            .is_none_or(|v2| type_params.len() != v2.len())
                        {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, *span, [(msg, *span)])
                        }

                        let is_mut = mut_struct_def.mut_methods.contains(&method.name);

                        let generic_arguments = method
                            .generics
                            .as_ref()
                            .unwrap()
                            .iter()
                            // .chain(mut_struct_def.generics.clone().iter())
                            .map(|(x, _)| x)
                            .zip(type_params.iter()) //.chain(struct_type_params.iter()))
                            .fold(HashMap::new(), |mut acc, (def, arg)| {
                                if let Some(c) = def.constraint.as_ref() {
                                    check_type_compatability(c, arg, type_env);
                                }
                                acc.insert(def.name.clone(), arg.0.clone());
                                acc
                            });

                        let mut cloned_fn_def = method.clone();

                        let (new_fn_name, global_generic_generation_id) = {
                            let new_method_name = [cloned_fn_def.name.clone()]
                                .into_iter()
                                .chain(
                                    generic_arguments
                                        .iter()
                                        .map(|(_, t)| t.as_clean_go_type_name(type_env)),
                                )
                                .collect::<Vec<_>>();

                            let mut gen_id = new_method_name.clone();
                            gen_id.insert(0, replaced_struct_name.clone());
                            (new_method_name.join(MANGLE_SEP), gen_id.join(MANGLE_SEP))
                        };

                        cloned_fn_def.name = new_fn_name.clone();
                        cloned_fn_def.generics = None;

                        if type_env
                            .prevent_generic_generation
                            .insert(global_generic_generation_id.clone())
                        {
                            replace_generics_in_function_definition(
                                &mut cloned_fn_def,
                                &generic_arguments,
                            );
                            type_env.function_headers.insert(
                                global_generic_generation_id.clone(),
                                FunHeader {
                                    params: cloned_fn_def
                                        .params
                                        .as_ref()
                                        .cloned()
                                        .unwrap()
                                        .into_iter()
                                        .map(|(_, x)| x)
                                        .collect(),
                                    return_type: cloned_fn_def.return_type.as_ref().cloned(),
                                },
                            );
                            typeresolve_method_definition(
                                &mut cloned_fn_def,
                                self_type.into_empty_span(),
                                is_mut,
                                type_env,
                            );
                            type_env
                                .get_generic_methods(replaced_struct_name)
                                .push(cloned_fn_def);
                        }
                        header = type_env.get_method_header(&global_generic_generation_id);
                    }
                    _ => {
                        let msg = "Can only use generics on methods or global functions";
                        failure_with_occurence(msg, *span, [(msg, *span)]);
                    }
                }
            }

            params
                .iter_mut()
                .zip(header.params.iter())
                .for_each(|(param_expr, param_def)| {
                    infer_against(param_expr, param_def, type_env);
                    typeresolve_value_expr((&mut param_expr.0, param_expr.1), type_env)
                });
        }
        ValueExpr::Variable(_, identifier, type_expr_opt, const_opt) => {
            // if let Some(type_expr) = type_expr_opt {
            //     resolve_all_aliases_type_expr(type_expr, type_env);
            //     return;
            // }
            let (type_expr, is_const) = type_env
                .get_identifier_type_and_const(&identifier)
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
            type_params,
        } => {
            let og_def = type_env.get_struct_def(name);

            if type_params.len() != og_def.generics.len() {
                let msg = "Wrong number of type parameters A";
                failure_with_occurence(msg, *span, [(msg, *span)])
            }

            let def = type_env
                .get_struct_def_with_type_params_mut(name.as_str(), type_params.as_slice(), *span)
                .clone();

            fields.iter_mut().zip(def.fields.iter()).for_each(
                |((_field_name, value_expr), def_field)| {
                    infer_against(value_expr, &def_field.type_expr, type_env);
                    typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
                },
            );

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
                    arm.type_case.0 = translate_intersection_to_duck(&arm.type_case.0);
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
