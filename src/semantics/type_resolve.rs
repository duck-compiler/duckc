use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::mpsc::Sender,
};

use chumsky::container::Container;
use indexmap::IndexMap;

use crate::{
    parse::{
        Field, SS, Spanned, SpannedMutRef,
        duckx_component_parser::DuckxComponent,
        extensions_def_parser::ExtensionsDef,
        failure_with_occurence,
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        jsx_component_parser::{
            Edit, JsxComponent, JsxComponentDependencies, JsxSourceUnit, do_edits,
        },
        schema_def_parser::SchemaDefinition,
        source_file_parser::SourceFile,
        struct_parser::{NamedDuckDefinition, StructDefinition},
        test_parser::TestCase,
        type_parser::{Duck, TypeDefinition, TypeExpr},
        value_parser::{
            Assignment, Declaration, ValFmtStringContents, ValHtmlStringContents, ValueExpr,
        },
    },
    semantics::{
        ident_mangler::{MANGLE_SEP, mangle, unmangle},
        typechecker::{check_type_compatability, check_type_compatability_full},
    },
    tags::Tag,
};

fn typeresolve_duckx_component(c: &mut DuckxComponent, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    type_env.insert_identifier_type("props".to_string(), c.props_type.0.clone(), false, false);
    typeresolve_value_expr((&mut c.value_expr.0, c.value_expr.1), type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_extensions_def(extensions_def: &mut ExtensionsDef, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    type_env.insert_identifier_type(
        "self".to_string(),
        extensions_def.target_type_expr.0.clone(),
        false,
        false,
    );

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

        let underlying_fn_type = extension_method.0.type_expr();

        let access_fn_type = TypeExpr::Fun(
            vec![(Some("self".to_string()), type_expr.clone())],
            Box::new(extension_method.0.type_expr()),
            // todo: mutable extension fns?
            false,
        );

        type_env.extension_functions.insert(
            extension_function_name,
            (underlying_fn_type.clone(), access_fn_type.clone()),
        );

        typeresolve_function_definition(&mut extension_method.0, type_env);
    }
}

fn typeresolve_test_case(test_case: &mut TestCase, type_env: &mut TypeEnv) {
    type_env.push_identifier_types();
    typeresolve_value_expr((&mut test_case.body.0, test_case.body.1), type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_jsx_component(c: &mut JsxComponent, type_env: &mut TypeEnv) {
    let units = c.find_units();
    let mut edits = Vec::new();
    for (range, unit) in units.iter() {
        match unit {
            JsxSourceUnit::Jsx => {
                edits.push((range.start_byte, Edit::Insert("html`".to_string())));
                edits.push((range.end_byte, Edit::Insert("`".to_string())));
            }
            JsxSourceUnit::OpeningJsx => edits.push((range.start_byte, Edit::Delete(2))),
            JsxSourceUnit::ClosingJsx => edits.push((range.start_byte, Edit::Delete(3))),
            JsxSourceUnit::Expression => {
                if range.start_byte > 0
                    && &c.javascript_source.0[range.start_byte - 1..(range.start_byte)] != "$"
                {
                    edits.push((range.start_byte, Edit::Insert("$".to_string())))
                }
            }
            JsxSourceUnit::Ident => {
                // here we could implement rpc calls
                let ident = &c.javascript_source.0[range.start_byte..range.end_byte];

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
    do_edits(&mut c.javascript_source.0, &mut edits);
}

#[derive(Debug, Clone)]
pub struct FunHeader {
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct TypeEnv<'a> {
    pub identifier_types: Vec<HashMap<String, (TypeExpr, bool, bool)>>, // (type_expr, is_const, is_schema)
    pub type_aliases: Vec<HashMap<String, TypeExpr>>,
    pub extension_functions: HashMap<String, (Spanned<TypeExpr>, TypeExpr)>, // key = extension function name, (actual_fn_type, access_fn_type)

    pub function_headers: HashMap<String, FunHeader>,
    pub function_definitions: Vec<FunctionDefintion>,
    pub jsx_components: Vec<JsxComponent>,
    pub duckx_components: Vec<DuckxComponent>,
    pub jsx_component_dependencies: HashMap<String, JsxComponentDependencies>,
    pub struct_definitions: Vec<StructDefinition>,
    pub schema_defs: Vec<SchemaDefinition>,
    pub named_duck_definitions: Vec<NamedDuckDefinition>,
    pub type_definitions: Vec<TypeDefinition>,

    pub generic_fns_generated: Vec<FunctionDefintion>,
    pub generic_structs_generated: Vec<StructDefinition>,
    pub generic_ducks_generated: Vec<NamedDuckDefinition>,
    pub generic_methods_generated: HashMap<String, Vec<FunctionDefintion>>,
    pub resolved_methods: HashMap<String, HashSet<String>>,
    pub prevent_generic_generation: HashSet<String>,
    pub tailwind_sender: Option<&'a Sender<String>>,
    pub is_recursive_type_alias: HashSet<String>,
    pub total_structs_resolved: HashSet<String>,

    pub all_go_imports: &'static HashSet<String>,
}

impl Default for TypeEnv<'_> {
    fn default() -> Self {
        Self {
            identifier_types: vec![HashMap::new()],
            type_aliases: vec![HashMap::new()],

            extension_functions: HashMap::new(),
            jsx_components: Vec::new(),
            duckx_components: Vec::new(),
            jsx_component_dependencies: HashMap::new(),
            function_headers: HashMap::new(),
            function_definitions: Vec::new(),
            struct_definitions: Vec::new(),
            schema_defs: Vec::new(),
            named_duck_definitions: Vec::new(),
            type_definitions: Vec::new(),

            resolved_methods: Default::default(),
            total_structs_resolved: Default::default(),
            generic_fns_generated: Vec::new(),
            generic_structs_generated: Vec::new(),
            generic_ducks_generated: Vec::new(),
            generic_methods_generated: HashMap::new(),
            prevent_generic_generation: HashSet::new(),
            tailwind_sender: None,
            is_recursive_type_alias: HashSet::new(),

            all_go_imports: Box::leak(Box::new(HashSet::new())),
        }
    }
}

#[derive(Clone, Debug)]
pub enum NeedsSearchResult {
    Duck { fields: Vec<Field> },
    Tuple { fields: Vec<Spanned<TypeExpr>> },
    Tag { name: String },
    Array { type_expr: Spanned<TypeExpr> },
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
    /// Returns whether it was resolved before
    pub fn mark_resolved(&mut self, type_name: &str, method_name: &str) -> bool {
        self.resolved_methods
            .entry(type_name.to_string())
            .or_default()
            .insert(method_name.to_string())
    }

    pub fn is_resolved(&self, type_name: &str, method_name: &str) -> bool {
        self.resolved_methods
            .get(type_name)
            .unwrap_or(&Default::default())
            .contains(method_name)
    }

    pub fn get_duck_def_with_type_params_mut<'a>(
        &'a mut self,
        name: &str,
        type_params: &[Spanned<TypeExpr>],
        span: SS,
    ) -> &'a mut NamedDuckDefinition {
        let generic_id = build_struct_generic_id(name, type_params, self);

        if !type_params.is_empty()
            && let Some((idx, _def)) = self
                .generic_ducks_generated
                .iter()
                .enumerate()
                .find(|(_, stored_def)| stored_def.name.as_str() == generic_id.as_ref())
        {
            return &mut self.generic_ducks_generated[idx];
        }

        if type_params.is_empty() {
            return self
                .named_duck_definitions
                .iter_mut()
                .find(|d| d.name == name)
                .unwrap();
        }

        let cloned_def = self
            .named_duck_definitions
            .iter()
            .chain(self.generic_ducks_generated.iter())
            .find(|user_struct_definition| user_struct_definition.name.as_str() == name)
            .cloned();

        if let Some(mut cloned_def) = cloned_def {
            let generic_arguments = cloned_def
                .generics
                .iter()
                .map(|(x, _)| x)
                .zip(type_params.iter())
                .fold(IndexMap::new(), |mut acc, (def, arg)| {
                    if let Some(c) = def.constraint.as_ref() {
                        check_type_compatability(c, arg, self);
                    }
                    acc.insert(def.name.clone(), arg.0.clone());
                    acc
                });

            let new_duck_name = [cloned_def.name.clone()]
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
                .insert(new_duck_name.clone())
            {
                replace_generics_in_named_duck_def(&mut cloned_def, &generic_arguments, self);
                cloned_def.name = new_duck_name.clone();
                for f in &mut cloned_def.fields {
                    resolve_all_aliases_type_expr(&mut f.type_expr, self);
                }
                self.generic_ducks_generated.push(cloned_def.clone());
            }
            self.generic_ducks_generated
                .iter_mut()
                .find(|f| f.name.as_str() == new_duck_name.as_str())
                .unwrap()
        } else {
            failure_with_occurence(
                format!("This duck does not exist {name}"),
                span,
                [(format!("duck {name} does not exist"), span)],
            );
        }
    }

    pub fn get_struct_def_with_type_params_mut<'a>(
        &'a mut self,
        name: &str,
        type_params: &[Spanned<TypeExpr>],
        span: SS,
    ) -> &'a mut StructDefinition {
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
                .unwrap_or_else(|| {
                    failure_with_occurence(
                        format!("Unknown Struct"),
                        span,
                        [(format!("Struct {name} does not exist"), span)],
                    );
                });
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
                .fold(IndexMap::new(), |mut acc, (def, arg)| {
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
                replace_generics_in_struct_definition(&mut cloned_def, &generic_arguments, self);
                cloned_def.name = new_struct_name.clone();
                self.generic_structs_generated.push(cloned_def.clone());
                cloned_def.name = name.to_string();
                typeresolve_struct_def(&mut cloned_def, type_params.to_vec(), self, false);
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
                format!("Unkown Struct"),
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
        self.jsx_components.iter().any(|x| x.name.as_str() == name)
    }

    pub fn get_component_dependencies(&mut self, name: String) -> &mut JsxComponentDependencies {
        self.jsx_component_dependencies.entry(name).or_default()
    }

    pub fn get_duckx_component(&self, name: &str) -> Option<&DuckxComponent> {
        self.duckx_components.iter().find(|x| x.name == name)
    }

    pub fn get_full_component_dependencies(&mut self, name: &str) -> HashSet<String> {
        let mut out = self
            .jsx_component_dependencies
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

    pub fn get_component<'a>(&'a self, name: &str) -> Option<&'a JsxComponent> {
        self.jsx_components.iter().find(|x| x.name.as_str() == name)
    }

    pub fn has_method_header(&self, name: &str) -> bool {
        self.function_headers.contains_key(name)
    }

    pub fn get_method_header(&self, name: &str) -> FunHeader {
        self.function_headers
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("{:?}\nSearched for {name}", self.function_headers))
    }

    pub fn get_struct_def_opt<'a>(&'a self, name: &str) -> Option<&'a StructDefinition> {
        self.struct_definitions
            .iter()
            .chain(self.generic_structs_generated.iter())
            .find(|x| x.name.as_str() == name)
    }

    pub fn get_schema_def_opt<'a>(&'a self, name: &str) -> Option<&'a SchemaDefinition> {
        self.schema_defs.iter().find(|x| x.name.as_str() == name)
    }

    pub fn get_duck_def_opt<'a>(&'a self, name: &str) -> Option<&'a NamedDuckDefinition> {
        self.named_duck_definitions
            .iter()
            .chain(self.generic_ducks_generated.iter())
            .find(|x| x.name.as_str() == name)
    }

    pub fn get_schema_def<'a>(&'a self, name: &str) -> &'a SchemaDefinition {
        self.get_schema_def_opt(name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
    }

    pub fn get_schema_def_mut<'a>(&'a mut self, name: &str) -> &'a mut SchemaDefinition {
        self.schema_defs
            .iter_mut()
            .find(|x| x.name.as_str() == name)
            .unwrap_or_else(|| panic!("Could not find struct {name}"))
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
        is_schema: bool,
    ) {
        self.identifier_types
            .last_mut()
            .expect("At least one env should exist. :(")
            .insert(identifier, (type_expr, is_const, is_schema));
    }

    pub fn get_identifier_type(&self, identifier: &str) -> Option<TypeExpr> {
        self.get_identifier_type_and_const(identifier)
            .map(|(x, _, _)| x)
    }

    pub fn get_identifier_type_in_typeof(&self, identifier: &str) -> Option<TypeExpr> {
        self.get_identifier_type_and_const(identifier)
            .map(|(x, _, schema)| {
                if schema {
                    if let TypeExpr::Fun(_, return_duck, _) = x
                        && let TypeExpr::Duck(duck) = &return_duck.as_ref().0
                    {
                        let fields = &duck.fields;
                        let from_json_fun = fields
                            .iter()
                            .find(|field| field.name == "from_json")
                            .expect("compiler error: schema without from json in object");

                        dbg!(&from_json_fun);

                        if let TypeExpr::Fun(_, return_type, _) = &from_json_fun.type_expr.0 {
                            return return_type.as_ref().clone().0;
                        }

                        panic!("compiler error: schema without function")
                    } else {
                        panic!("compiler error: schema without function")
                    }
                } else {
                    x
                }
            })
    }

    pub fn get_identifier_type_and_const(
        &self,
        identifier: &str,
    ) -> Option<(TypeExpr, bool, bool)> {
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

    pub fn find_ducks_and_tuples(&mut self) -> Vec<NeedsSearchResult> {
        let mut result = Vec::new();

        let cloned_resolve = self.resolved_methods.clone();

        for fun_def in self
            .function_definitions
            .clone()
            .iter_mut()
            .chain(self.generic_fns_generated.clone().iter_mut())
            .chain(
                self.generic_methods_generated
                    .clone()
                    .values_mut()
                    .flat_map(|v| v.iter_mut()),
            )
            .chain(
                self.struct_definitions
                    .clone()
                    .iter_mut()
                    .filter(|s| s.generics.is_empty())
                    .flat_map(|s| {
                        let leaked_name = s.name.clone().leak() as &'static str;
                        s.methods.iter_mut().filter(|m| {
                            cloned_resolve
                                .get(leaked_name)
                                .unwrap_or(&HashSet::default())
                                .contains(&m.name)
                        })
                    }),
            )
            .chain(
                self.generic_structs_generated
                    .clone()
                    .iter_mut()
                    .filter(|s| s.generics.is_empty())
                    .flat_map(|s| {
                        let leaked_name = s.name.clone().leak() as &'static str;
                        s.methods.iter_mut().filter(|m| {
                            cloned_resolve
                                .get(leaked_name)
                                .unwrap_or(&HashSet::default())
                                .contains(&m.name)
                        })
                    }),
            )
            .filter(|f| f.generics.is_empty())
        {
            for t in fun_def
                .params
                .iter_mut()
                .map(|v| &mut v.1)
                .chain([&mut fun_def.return_type].into_iter())
            {
                self.find_ducks_and_tuples_type_expr(t, &mut result);
            }

            // dbg!(&fun_def.name);
            // dbg!(&cloned_resolve);
            self.find_tuples_and_ducks_value_expr(&mut fun_def.value_expr, &mut result);
        }

        // todo: check we should probably not clone here
        let mut extension_functions = self.extension_functions.clone();
        for extension_fun in extension_functions.iter_mut() {
            self.find_ducks_and_tuples_type_expr(&mut extension_fun.1.0, &mut result);
        }

        let mut schema_defs = self.schema_defs.clone();
        for schema_def in &mut schema_defs {
            for schema_field in &mut schema_def.fields {
                self.find_ducks_and_tuples_type_expr(&mut schema_field.type_expr, &mut result);
            }

            if let Some(out_type) = &mut schema_def.out_type {
                self.find_ducks_and_tuples_type_expr(out_type, &mut result);
            }

            if let Some(function_type) = &mut schema_def.schema_fn_type {
                self.find_ducks_and_tuples_type_expr(function_type, &mut result);
            }
        }

        for s in self
            .struct_definitions
            .clone()
            .iter_mut()
            .chain(self.generic_structs_generated.clone().iter_mut())
            .filter(|s| s.generics.is_empty())
        {
            for m in &mut s.fields {
                self.find_ducks_and_tuples_type_expr(&mut m.type_expr, &mut result);
            }
        }

        for s in self
            .named_duck_definitions
            .clone()
            .iter_mut()
            .chain(self.generic_ducks_generated.clone().iter_mut())
            .filter(|s| s.generics.is_empty())
        {
            for m in &mut s.fields {
                self.find_ducks_and_tuples_type_expr(&mut m.type_expr, &mut result);
            }
        }

        result.sort_by_key(|e| match e {
            NeedsSearchResult::Duck { fields } => TypeExpr::Duck(Duck {
                fields: fields.clone(),
            })
            .as_clean_go_type_name(self),
            NeedsSearchResult::Tuple { fields } => {
                TypeExpr::Tuple(fields.clone()).as_clean_go_type_name(self)
            }
            NeedsSearchResult::Tag { name } => {
                TypeExpr::Tag(name.clone()).as_clean_go_type_name(self)
            }
            NeedsSearchResult::Array { type_expr: t } => {
                TypeExpr::Array(t.clone().into()).as_clean_go_type_name(self)
            }
        });

        result.dedup_by_key(|e| match e {
            NeedsSearchResult::Duck { fields } => TypeExpr::Duck(Duck {
                fields: fields.clone(),
            })
            .as_clean_go_type_name(self),
            NeedsSearchResult::Tuple { fields } => {
                TypeExpr::Tuple(fields.clone()).as_clean_go_type_name(self)
            }
            NeedsSearchResult::Tag { name } => {
                TypeExpr::Tag(name.clone()).as_clean_go_type_name(self)
            }
            NeedsSearchResult::Array { type_expr: t } => {
                TypeExpr::Array(t.clone().into()).as_clean_go_type_name(self)
            }
        });

        result
    }

    fn find_ducks_and_tuples_type_expr(
        &mut self,
        v: &mut Spanned<TypeExpr>,
        out: &mut Vec<NeedsSearchResult>,
    ) {
        let out = Rc::new(RefCell::new(out));
        trav_type_expr(build_tuples_and_ducks_type_expr_trav_fn(out), v, self);
    }

    fn find_tuples_and_ducks_value_expr(
        &mut self,
        v: &mut Spanned<ValueExpr>,
        out: &mut Vec<NeedsSearchResult>,
    ) {
        let out = Rc::new(RefCell::new(out));
        trav_value_expr(
            build_tuples_and_ducks_type_expr_trav_fn(out.clone()),
            build_tuples_and_ducks_value_expr_trav_fn(out.clone()),
            v,
            self,
        );
    }
}

fn build_tuples_and_ducks_type_expr_trav_fn(
    out: Rc<RefCell<&mut Vec<NeedsSearchResult>>>,
) -> impl Fn(&mut Spanned<TypeExpr>, &mut TypeEnv<'_>) + Clone {
    move |t, env| match &t.0 {
        TypeExpr::Tuple(fields) => {
            out.borrow_mut().push(NeedsSearchResult::Tuple {
                fields: fields.clone(),
            });
        }
        TypeExpr::Duck(fields) => {
            out.borrow_mut().push(NeedsSearchResult::Duck {
                fields: fields.fields.clone(),
            });
        }
        TypeExpr::Tag(name) => {
            out.borrow_mut()
                .push(NeedsSearchResult::Tag { name: name.clone() });
        }
        TypeExpr::Struct { name, type_params } => {
            env.get_struct_def_with_type_params_mut(name, type_params, t.1);
        }
        TypeExpr::Array(t) => {
            out.borrow_mut().push(NeedsSearchResult::Array {
                type_expr: t.as_ref().clone(),
            });
        }
        _ => {}
    }
}

fn build_tuples_and_ducks_value_expr_trav_fn(
    out: Rc<RefCell<&mut Vec<NeedsSearchResult>>>,
) -> impl Fn(&mut Spanned<ValueExpr>, &mut TypeEnv<'_>) + Clone {
    move |v, env| match &v.0 {
        ValueExpr::Tag(name) => {
            out.borrow_mut()
                .push(NeedsSearchResult::Tag { name: name.clone() });
        }
        ValueExpr::Duck(..) => {
            let TypeExpr::Duck(Duck { fields }) = TypeExpr::from_value_expr(v, env) else {
                panic!()
            };
            out.borrow_mut().push(NeedsSearchResult::Duck { fields });
        }
        ValueExpr::Tuple(..) => {
            let TypeExpr::Tuple(fields) = TypeExpr::from_value_expr(v, env) else {
                panic!()
            };
            out.borrow_mut().push(NeedsSearchResult::Tuple { fields });
        }
        ValueExpr::Array(exprs) if !exprs.is_empty() => {
            let TypeExpr::Array(t) = TypeExpr::from_value_expr(v, env) else {
                panic!()
            };
            out.borrow_mut()
                .push(NeedsSearchResult::Array { type_expr: *t });
        }
        _ => {}
    }
}

pub fn trav_type_expr<F1>(f_t: F1, v: &mut Spanned<TypeExpr>, env: &mut TypeEnv)
where
    F1: Fn(&mut Spanned<TypeExpr>, &mut TypeEnv) + Clone,
{
    f_t(v, env);
    match &mut v.0 {
        TypeExpr::Never | TypeExpr::Statement => {}
        TypeExpr::NamedDuck {
            name: _,
            type_params,
        } => {
            for t in type_params {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
        TypeExpr::Duck(fields) => {
            for f in &mut fields.fields {
                trav_type_expr(f_t.clone(), &mut f.type_expr, env);
            }
        }
        TypeExpr::Array(a) | TypeExpr::KeyOf(a) | TypeExpr::Ref(a) | TypeExpr::RefMut(a) => {
            trav_type_expr(f_t.clone(), a, env)
        }
        TypeExpr::Bool(..)
        | TypeExpr::Tag(..)
        | TypeExpr::TypeOf(..)
        | TypeExpr::Int
        | TypeExpr::UInt
        | TypeExpr::String(..)
        | TypeExpr::Float
        | TypeExpr::Char
        | TypeExpr::Html
        | TypeExpr::Go(..)
        | TypeExpr::Any
        | TypeExpr::TemplParam(..) => {}
        TypeExpr::And(types) | TypeExpr::Or(types) | TypeExpr::Tuple(types) => {
            for t in types {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
        TypeExpr::Fun(params, ret, _) => {
            for p in params {
                trav_type_expr(f_t.clone(), &mut p.1, env);
            }
            trav_type_expr(f_t.clone(), ret, env);
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

pub fn trav_value_expr<F1, F2>(f_t: F1, f_vv: F2, v: &mut Spanned<ValueExpr>, env: &mut TypeEnv)
where
    F1: Fn(&mut Spanned<TypeExpr>, &mut TypeEnv) + Clone,
    F2: Fn(&mut Spanned<ValueExpr>, &mut TypeEnv) + Clone,
{
    f_vv(v, env);

    match &mut v.0 {
        ValueExpr::Negate(v) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
        }
        ValueExpr::RawStruct {
            is_global: _,
            name: _,
            fields,
            type_params,
        } => {
            for field in fields {
                trav_value_expr(f_t.clone(), f_vv.clone(), &mut field.1, env);
            }
            for t in type_params {
                trav_type_expr(f_t.clone(), t, env);
            }
        }
        ValueExpr::Lambda(l) => {
            for v in &mut l.params {
                if let Some(t) = v.1.as_mut() {
                    trav_type_expr(f_t.clone(), t, env);
                }
            }
            if let Some(ret) = l.return_type.as_mut() {
                trav_type_expr(f_t.clone(), ret, env);
            }

            trav_value_expr(f_t, f_vv, &mut l.value_expr, env);
        }
        ValueExpr::Return(e) => {
            if let Some(e) = e {
                trav_value_expr(f_t.clone(), f_vv.clone(), e, env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            trav_value_expr(f_t.clone(), f_vv.clone(), target_obj, env);
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
            name: _,
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
            trav_value_expr(f_t.clone(), f_vv.clone(), target, env);
            trav_type_expr(f_t.clone(), t, env);
        }
        ValueExpr::Defer(v) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
        }
        ValueExpr::Async(v) => {
            trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
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
            if let Some(initializer) = decl.0.initializer.as_mut() {
                trav_value_expr(f_t.clone(), f_vv.clone(), initializer, env);
            }
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
            span: _,
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
            trav_value_expr(f_t.clone(), f_vv.clone(), v, env);
        }
    }
}

fn resolve_all_aliases_type_expr(expr: &mut Spanned<TypeExpr>, env: &mut TypeEnv) {
    let span = expr.1;
    trav_type_expr(
        |node, env| match &mut node.0 {
            TypeExpr::TypeOf(identifier) => {
                let type_expr = env.get_identifier_type_in_typeof(identifier);
                *node = resolve_type_expr(
                    &(type_expr.expect("couldn't find identifier type"), span),
                    env,
                );
            }
            TypeExpr::KeyOf(type_expr) => {
                let type_expr = type_expr.as_mut();
                *type_expr = resolve_type_expr(type_expr, env);
            }
            TypeExpr::TypeName(..) => {
                *node = resolve_type_expr(node, env);
            }
            TypeExpr::RawTypeName(_, v, _) => {
                assert_eq!(1, v.len(), "should be mangled");
                *node = resolve_type_expr(node, env);
            }
            _ => {}
        },
        expr,
        env,
    );
}

fn resolve_all_aliases_value_expr(expr: &mut Spanned<ValueExpr>, env: &mut TypeEnv) {
    trav_value_expr(
        |node, env| match &mut node.0 {
            TypeExpr::TypeOf(_identifier) => {
                // let type_expr = env.get_identifier_type(identifier);
                // *node = resolve_type_expr(&(type_expr.expect("couldn't find identifier type"), node.1), env);
            }
            TypeExpr::KeyOf(type_expr) => {
                let type_expr = type_expr.as_mut();
                *type_expr = resolve_type_expr(type_expr, env);
            }
            TypeExpr::TypeName(..) => {
                *node = resolve_type_expr(node, env);
            }
            TypeExpr::RawTypeName(_, v, _) => {
                assert_eq!(1, v.len(), "should be mangled");
                *node = resolve_type_expr(node, env);
            }
            _ => {}
        },
        |f, env| {
            if let ValueExpr::Struct {
                name,
                fields: _,
                type_params,
            } = &mut f.0
                && let TypeExpr::Struct {
                    name: new_name,
                    type_params: new_type_params,
                } = resolve_type_expr(
                    &TypeExpr::Struct {
                        name: name.clone(),
                        type_params: type_params.clone(),
                    }
                    .into_empty_span(),
                    env,
                )
                .0
            {
                *name = new_name;
                *type_params = new_type_params;
            }
        },
        expr,
        env,
    );
}

fn process_keyof_in_value_expr(expr: &mut Spanned<ValueExpr>, type_env: &mut TypeEnv) {
    trav_value_expr(
        |f, env| {
            process_keyof_in_type_expr(&mut f.0, env);
        },
        |_, _| {},
        expr,
        type_env,
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
                    TypeExpr::RawTypeName(_, _typename, _) => {
                        todo!();
                        // let resolved_type = resolve_type_expr(&(type_expr.clone(), *span), type_env);
                        // return do_it(&resolved_type.0, span, type_env);
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
    generics: &IndexMap<String, TypeExpr>,
    type_env: &mut TypeEnv<'_>,
) {
    for f in def.fields.iter_mut() {
        replace_generics_in_type_expr(&mut f.type_expr.0, generics, type_env);
    }

    for m in def.methods.iter_mut() {
        for t in [&mut m.return_type.0]
            .into_iter()
            .chain(m.params.iter_mut().map(|x| &mut x.1.0))
        {
            replace_generics_in_type_expr(t, generics, type_env);
        }
        replace_generics_in_value_expr(&mut m.value_expr.0, generics, type_env);
    }
}

fn replace_generics_in_function_definition(
    t: &mut FunctionDefintion,
    set_params: &IndexMap<String, TypeExpr>,
    type_env: &mut TypeEnv<'_>,
) {
    for t in t
        .params
        .iter_mut()
        .map(|(_, x)| x)
        .chain([&mut t.return_type].into_iter())
    {
        replace_generics_in_type_expr(&mut t.0, set_params, type_env);
    }
    replace_generics_in_value_expr(&mut t.value_expr.0, set_params, type_env);
}

fn replace_generics_in_value_expr(
    expr: &mut ValueExpr,
    set_params: &IndexMap<String, TypeExpr>,
    type_env: &mut TypeEnv<'_>,
) {
    match expr {
        ValueExpr::RawStruct {
            is_global,
            name,
            fields: _,
            type_params: _,
        } => {
            panic!("Compiler Bug: raw struct sholdnt be here {name:?} {is_global}")
        }
        ValueExpr::Async(d) | ValueExpr::Defer(d) => {
            replace_generics_in_value_expr(&mut d.0, set_params, type_env)
        }
        ValueExpr::As(v, t) => {
            replace_generics_in_value_expr(&mut v.0, set_params, type_env);
            replace_generics_in_type_expr(&mut t.0, set_params, type_env);
        }
        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            replace_generics_in_value_expr(&mut target.0, set_params, type_env);
            replace_generics_in_value_expr(&mut block.0, set_params, type_env);
        }
        ValueExpr::Deref(t) | ValueExpr::Ref(t) | ValueExpr::RefMut(t) => {
            replace_generics_in_value_expr(&mut t.0, set_params, type_env)
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
            replace_generics_in_value_expr(&mut lhs.0, set_params, type_env);
            replace_generics_in_value_expr(&mut rhs.0, set_params, type_env);
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    replace_generics_in_value_expr(&mut e.0, set_params, type_env);
                }
            }
        }
        ValueExpr::Negate(e) | ValueExpr::BoolNegate(e) | ValueExpr::Return(Some(e)) => {
            replace_generics_in_value_expr(&mut e.0, set_params, type_env)
        }
        ValueExpr::Array(exprs) => {
            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params, type_env);
            }
        }
        ValueExpr::ArrayAccess(target, index) => {
            replace_generics_in_value_expr(&mut target.0, set_params, type_env);
            replace_generics_in_value_expr(&mut index.0, set_params, type_env);
        }
        ValueExpr::Block(exprs) => {
            for e in exprs {
                replace_generics_in_value_expr(&mut e.0, set_params, type_env);
            }
        }
        ValueExpr::Duck(def) => {
            for (_, expr) in def {
                replace_generics_in_value_expr(&mut expr.0, set_params, type_env);
            }
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            replace_generics_in_value_expr(&mut target_obj.0, set_params, type_env);
        }
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                if let ValFmtStringContents::Expr(e) = c {
                    replace_generics_in_value_expr(&mut e.0, set_params, type_env);
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
                replace_generics_in_value_expr(v, set_params, type_env);
            }
            for t in type_params.iter_mut() {
                replace_generics_in_type_expr(&mut t.0, set_params, type_env);
            }
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            replace_generics_in_value_expr(&mut condition.0, set_params, type_env);
            replace_generics_in_value_expr(&mut then.0, set_params, type_env);
            if let Some(r#else) = r#else {
                replace_generics_in_value_expr(&mut r#else.0, set_params, type_env);
            }
        }
        ValueExpr::Lambda(def) => {
            for (_, p) in &mut def.params {
                if let Some(p) = p.as_mut() {
                    replace_generics_in_type_expr(&mut p.0, set_params, type_env);
                }
            }
            if let Some(return_type) = def.return_type.as_mut() {
                replace_generics_in_type_expr(&mut return_type.0, set_params, type_env);
            }
            replace_generics_in_value_expr(&mut def.value_expr.0, set_params, type_env);
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
                replace_generics_in_value_expr(&mut f.1.0, set_params, type_env);
            }

            for t in type_params {
                replace_generics_in_type_expr(&mut t.0, set_params, type_env);
            }
        }
        ValueExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_value_expr(&mut f.0, set_params, type_env);
            }
        }
        ValueExpr::While { condition, body } => {
            replace_generics_in_value_expr(&mut condition.0, set_params, type_env);
            replace_generics_in_value_expr(&mut body.0, set_params, type_env);
        }
        ValueExpr::VarDecl(decl) => {
            if let Some(type_expr) = &mut decl.0.type_expr {
                replace_generics_in_type_expr(&mut type_expr.0, set_params, type_env);
            }
            if let Some(initializer) = decl.0.initializer.as_mut() {
                replace_generics_in_value_expr(&mut initializer.0, set_params, type_env);
            }
        }
        ValueExpr::VarAssign(a) => {
            replace_generics_in_value_expr(&mut a.0.target.0, set_params, type_env);
            replace_generics_in_value_expr(&mut a.0.value_expr.0, set_params, type_env);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span: _,
        } => {
            replace_generics_in_value_expr(&mut value_expr.0, set_params, type_env);
            for arm in arms {
                replace_generics_in_type_expr(&mut arm.type_case.0, set_params, type_env);
                if let Some(condition) = &mut arm.condition {
                    replace_generics_in_value_expr(&mut condition.0, set_params, type_env);
                }
                replace_generics_in_value_expr(&mut arm.value_expr.0, set_params, type_env);
            }

            if let Some(arm) = else_arm {
                replace_generics_in_type_expr(&mut arm.type_case.0, set_params, type_env);
                if let Some(condition) = &mut arm.condition {
                    replace_generics_in_value_expr(&mut condition.0, set_params, type_env);
                }
                replace_generics_in_value_expr(&mut arm.value_expr.0, set_params, type_env);
            }
        }
        ValueExpr::InlineGo(go_src, ty) => {
            if let Some(ty) = ty {
                replace_generics_in_type_expr(&mut ty.0, set_params, type_env);
            }
            let mut to_replace = go_src.to_string();
            loop {
                let start_idx = to_replace.find("<<<");
                if let Some(start_idx) = start_idx {
                    let end = to_replace.find(">>>");
                    if let Some(end_idx) = end {
                        let mut name = &to_replace[start_idx + 3..end_idx];
                        let mut concrete_type = false;
                        if name.starts_with("@") {
                            let replaced_name = name.replace("@", "");
                            name = replaced_name.leak();

                            concrete_type = true
                        }

                        let replacement = set_params.get(name);
                        if let Some(replacement) = replacement {
                            let type_anno = if concrete_type {
                                replacement.as_clean_go_type_name(type_env)
                            } else {
                                replacement.as_go_type_annotation(type_env)
                            };
                            to_replace.replace_range(start_idx..end_idx + 3, type_anno.as_str());
                        }
                        continue;
                    }
                }
                break;
            }
            *go_src = to_replace;
        }
        ValueExpr::Bool(..)
        | ValueExpr::Break
        | ValueExpr::Char(..)
        | ValueExpr::String(..)
        | ValueExpr::Continue
        | ValueExpr::Float(..)
        | ValueExpr::Int(..)
        | ValueExpr::RawVariable(..)
        | ValueExpr::Return(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Variable(..) => {}
    }
}

fn replace_generics_in_named_duck_def(
    def: &mut NamedDuckDefinition,
    set_params: &IndexMap<String, TypeExpr>,
    type_env: &mut TypeEnv<'_>,
) {
    for f in &mut def.fields {
        replace_generics_in_type_expr(&mut f.type_expr.0, set_params, type_env);
    }
}

fn replace_generics_in_type_expr(
    expr: &mut TypeExpr,
    set_params: &IndexMap<String, TypeExpr>,
    type_env: &mut TypeEnv<'_>,
) {
    match expr {
        TypeExpr::UInt => {}
        TypeExpr::Statement | TypeExpr::Never => {}
        TypeExpr::TemplParam(name) => {
            if let Some(replacement) = set_params.get(name).cloned() {
                *expr = replacement;
            }
        }
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => {
            replace_generics_in_type_expr(&mut t.0, set_params, type_env)
        }
        TypeExpr::Html => {}
        TypeExpr::TypeOf(..) => {}
        TypeExpr::KeyOf(type_expr) => {
            replace_generics_in_type_expr(&mut type_expr.as_mut().0, set_params, type_env);
        }
        TypeExpr::Array(t) => {
            replace_generics_in_type_expr(&mut t.0, set_params, type_env);
        }
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                replace_generics_in_type_expr(&mut f.type_expr.0, set_params, type_env);
            }
        }
        TypeExpr::Fun(params, ret, _) => {
            for p in params {
                replace_generics_in_type_expr(&mut p.1.0, set_params, type_env);
            }
            replace_generics_in_type_expr(&mut ret.0, set_params, type_env);
        }
        TypeExpr::Or(contents) => {
            for c in contents {
                replace_generics_in_type_expr(&mut c.0, set_params, type_env);
            }
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                replace_generics_in_type_expr(&mut f.0, set_params, type_env);
            }
        }
        TypeExpr::TypeName(_, name, generics) => {
            for (g, _) in generics {
                replace_generics_in_type_expr(g, set_params, type_env);
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
                replace_generics_in_type_expr(&mut t.0, set_params, type_env);
            }
        }
        TypeExpr::NamedDuck { name, type_params } => {
            if let Some(TypeExpr::TypeName(_, new_name, g)) = set_params.get(name)
                && g.is_empty()
            {
                *name = new_name.clone();
            }

            for t in type_params {
                replace_generics_in_type_expr(&mut t.0, set_params, type_env);
            }
        }
        TypeExpr::Go(go_type) => {
            let mut out = String::new();
            let chars = go_type.chars().collect::<Vec<_>>();
            let mut i = 0;

            while i < chars.len() {
                let current = chars[i];

                if current == '{' {
                    let mut other_curly_brace = i + 1;
                    let mut inner_buf = String::new();
                    while other_curly_brace < chars.len() && chars[other_curly_brace] != '}' {
                        inner_buf.push(chars[other_curly_brace]);
                        other_curly_brace += 1;
                    }
                    if other_curly_brace < chars.len() && chars[other_curly_brace] == '}' {
                        let replacement = set_params.get(&inner_buf);
                        if let Some(replacement) = replacement {
                            out.push_str(&replacement.as_go_type_annotation(type_env));
                            i = other_curly_brace;
                        }
                    }
                } else {
                    out.push(current);
                }
                i += 1;
            }
            *go_type = out;
        }
        TypeExpr::Any
        | TypeExpr::Char
        | TypeExpr::Bool(..)
        | TypeExpr::Int
        | TypeExpr::Float
        | TypeExpr::String(..)
        | TypeExpr::Tag(..) => {}
        TypeExpr::RawTypeName(_, typename, _) => {
            if typename.len() == 1
                && let Some(replacement) = set_params.get(&typename[0])
            {
                *expr = replacement.clone();
            }
        }
        TypeExpr::And(variants) => {
            for variant in variants.iter_mut() {
                replace_generics_in_type_expr(&mut variant.0, set_params, type_env);
            }
        }
    }
}

pub fn resolve_type_expr(type_expr: &Spanned<TypeExpr>, env: &mut TypeEnv) -> Spanned<TypeExpr> {
    let mut res = type_expr.clone();

    loop {
        let (name, generics) = match &res.0 {
            TypeExpr::RawTypeName(_, name_segments, generics) => {
                assert_eq!(name_segments.len(), 1, "should be mangled");
                let name = &name_segments[0];
                (name, generics)
            }
            TypeExpr::TypeName(_, name, generics) => (name, generics),
            TypeExpr::Struct { name, type_params } => (name, type_params),
            TypeExpr::NamedDuck { name, type_params } => (name, type_params),
            _ => break,
        };

        if let Some(s_def) = env.get_duck_def_opt(name)
            && !matches!(res.0, TypeExpr::NamedDuck { .. })
        {
            res.0 = TypeExpr::NamedDuck {
                name: s_def.name.clone(),
                type_params: generics.to_vec(),
            };
            continue;
        }

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
                .zip(generics.clone())
                .fold(IndexMap::new(), |mut acc, ((name, constraint), exp)| {
                    if let Some(constraint) = constraint {
                        check_type_compatability(&constraint, &exp, env);
                    }
                    acc.insert(name, exp.0.clone());
                    acc
                });

            let mut r = def.type_expression.clone();
            replace_generics_in_type_expr(&mut r.0, &arguments, env);
            res = r;
            continue;
        }

        break;
    }

    res
}

// fn mangle_generics_name(
//     base: &str,
//     params: &[Spanned<TypeExpr>],
//     type_env: &mut TypeEnv,
// ) -> String {
//     format!("{base}{}", {
//         let r = params
//             .iter()
//             .map(|x| x.0.as_clean_go_type_name(type_env))
//             .collect::<Vec<_>>()
//             .join("_");
//         if r.is_empty() { r } else { format!("_{r}") }
//     })
// }

pub fn sort_fields_value_expr(expr: &mut ValueExpr) {
    match expr {
        ValueExpr::RawStruct {
            is_global: _,
            name: _,
            fields,
            type_params,
        } => {
            for field in fields {
                sort_fields_value_expr(&mut field.1.0);
            }
            for t in type_params {
                sort_fields_type_expr(&mut t.0);
            }
        }
        ValueExpr::Async(d) | ValueExpr::Defer(d) => sort_fields_value_expr(&mut d.0),
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

            if let Some(initializer) = initializer.as_mut() {
                sort_fields_value_expr(&mut initializer.0);
            }
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
        ValueExpr::Negate(e) | ValueExpr::BoolNegate(e) => sort_fields_value_expr(&mut e.0),
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
    matches!(v, ValueExpr::Variable(_, _, _, Some(true), _))
}

pub fn sort_fields_type_expr(expr: &mut TypeExpr) {
    match expr {
        TypeExpr::UInt => {}
        TypeExpr::Statement | TypeExpr::Never => {}
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => sort_fields_type_expr(&mut t.0),
        TypeExpr::Html => {}
        TypeExpr::TypeOf(..) => {}
        TypeExpr::TemplParam(..) => {}
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
            sort_fields_type_expr(&mut r.0);
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
        | TypeExpr::Int
        | TypeExpr::String(_)
        | TypeExpr::TypeName(..)
        | TypeExpr::NamedDuck { .. }
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
    method_bodies: bool,
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
        resolve_all_aliases_type_expr(&mut m.return_type, type_env);

        for p in &mut m.params {
            resolve_all_aliases_type_expr(&mut p.1, type_env);
        }

        resolve_all_aliases_value_expr(&mut m.value_expr, type_env);
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
        .struct_definitions
        .retain(|f| f.name.as_str() != new_struct_name.as_str());
    type_env
        .generic_structs_generated
        .retain(|f| f.name.as_str() != new_struct_name.as_str());
    if type_params.is_empty() {
        type_env.struct_definitions.push(cloned);
    } else {
        type_env.generic_structs_generated.push(cloned);
    }

    if method_bodies {
        for m in &mut def.methods {
            if !m.generics.is_empty() {
                continue;
            }
            if type_env.mark_resolved(&new_struct_name, &m.name) {
                type_env.push_identifier_types();

                for p in &mut m.params {
                    type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false, false);
                }

                type_env.insert_identifier_type(
                    "self".to_string(),
                    if def.mut_methods.contains(&m.name) {
                        TypeExpr::RefMut(self_type.clone().into_empty_span().into())
                    } else {
                        TypeExpr::Ref(self_type.clone().into_empty_span().into())
                    },
                    true,
                    false,
                );
                typeresolve_value_expr((&mut m.value_expr.0, m.value_expr.1), type_env);
                type_env.pop_identifier_types();
            }
        }
    }

    type_env.pop_type_aliases();
}

pub fn typeresolve_schema_def(schema_def: &mut SchemaDefinition, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    for schema_field in &mut schema_def.fields {
        if let Some(branch) = &mut schema_field.if_branch {
            resolve_all_aliases_value_expr(&mut branch.0.condition, type_env);
            if let Some(value_expr) = &mut branch.0.value_expr {
                resolve_all_aliases_value_expr(value_expr, type_env)
            }
        }

        if let Some(value_expr) = &mut schema_field.else_branch_value_expr {
            resolve_all_aliases_value_expr(value_expr, type_env);
        }

        resolve_all_aliases_type_expr(&mut schema_field.type_expr, type_env);
    }

    if let Some(out_type) = &mut schema_def.out_type {
        resolve_all_aliases_type_expr(out_type, type_env);
    }

    if let Some(fn_type) = &mut schema_def.schema_fn_type {
        resolve_all_aliases_type_expr(fn_type, type_env);
    }

    type_env.schema_defs.push(schema_def.clone());

    type_env.pop_type_aliases();
}

pub fn typeresolve_source_file(source_file: &mut SourceFile, type_env: &mut TypeEnv) {
    type_env.push_type_aliases();

    println!("{} sort fields", Tag::TypeResolve);

    source_file.schema_defs.iter_mut().for_each(|schema_def| {
        for field in &mut schema_def.fields {
            sort_fields_type_expr(&mut field.type_expr.0);
            dbg!(&schema_def.out_type);
        }
    });

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
        .jsx_compontents
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
        .global_var_decls
        .iter_mut()
        .for_each(|global_var_decl| {
            sort_fields_type_expr(&mut global_var_decl.type_expr.0);
            sort_fields_value_expr(&mut global_var_decl.initializer.0);
        });

    source_file.schema_defs.iter_mut().for_each(|schema_def| {
        for schema_field in &mut schema_def.fields {
            sort_fields_type_expr(&mut schema_field.type_expr.0);
            if let Some(branch) = &mut schema_field.if_branch {
                sort_fields_value_expr(&mut branch.0.condition.0);
                if let Some((value_expr, _)) = &mut branch.0.value_expr {
                    sort_fields_value_expr(value_expr);
                }
            }

            if let Some(value_expr) = &mut schema_field.else_branch_value_expr {
                sort_fields_value_expr(&mut value_expr.0);
            }
        }
    });

    source_file
        .test_cases
        .iter_mut()
        .for_each(|test_case| sort_fields_value_expr(&mut test_case.body.0));

    println!("{} insert type definitions", Tag::TypeResolve);

    let mut to_remove_from_type_defs = Vec::new();

    source_file
        .type_definitions
        .iter_mut()
        .enumerate()
        .for_each(|(idx, type_def)| {
            if let TypeExpr::Duck(..) = &type_def.type_expression.0 {
                let to_find = &type_def.name;
                let is_recursive = Cell::new(false);
                trav_type_expr(
                    |t: &mut Spanned<TypeExpr>, _: &mut TypeEnv<'_>| {
                        if let TypeExpr::TypeName(_, n, type_params) = &t.0
                            && n == to_find
                        {
                            *t = (
                                TypeExpr::NamedDuck {
                                    name: n.clone(),
                                    type_params: type_params.clone(),
                                },
                                t.1,
                            );
                            is_recursive.set(true);
                        }
                    },
                    &mut type_def.type_expression,
                    type_env,
                );
                if is_recursive.get() {
                    let TypeExpr::Duck(Duck { mut fields }) = type_def.type_expression.0.clone()
                    else {
                        unreachable!()
                    };
                    for f in &mut fields {
                        resolve_all_aliases_type_expr(&mut f.type_expr, type_env);
                    }
                    type_env.named_duck_definitions.push(NamedDuckDefinition {
                        name: to_find.clone(),
                        fields: fields.clone(),
                        generics: type_def.generics.clone(),
                    });
                    to_remove_from_type_defs.push(idx);
                    return;
                }
            }

            if let TypeExpr::And(_) = &mut type_def.type_expression.0 {
                type_def.type_expression.0 =
                    translate_intersection_to_duck(&type_def.type_expression.0);
            }
            type_env.type_definitions.push(type_def.clone());
        });

    // Step 2: Insert type definitions
    source_file
        .struct_definitions
        .iter_mut()
        .for_each(|struct_definition| {
            type_env.struct_definitions.push(struct_definition.clone());

            type_env.push_type_aliases();

            for (g, _) in &struct_definition.generics {
                type_env.insert_type_alias(g.name.clone(), TypeExpr::TemplParam(g.name.clone()));
            }

            for field in &mut struct_definition.fields {
                resolve_all_aliases_type_expr(&mut field.type_expr, type_env);
            }
            for fun_def in &mut struct_definition.methods {
                for type_expr in fun_def
                    .params
                    .iter_mut()
                    .map(|c| &mut c.1)
                    .chain([&mut fun_def.return_type].into_iter())
                {
                    resolve_all_aliases_type_expr(type_expr, type_env);
                }
                resolve_all_aliases_value_expr(&mut fun_def.value_expr, type_env);
            }

            type_env.pop_type_aliases();
            type_env
                .struct_definitions
                .retain(|s| s.name.as_str() != struct_definition.name.as_str());
            type_env.struct_definitions.push(struct_definition.clone());
        });

    source_file.schema_defs.iter_mut().for_each(|schema_def| {
        let mut fields_with_type: Vec<(String, Spanned<TypeExpr>)> = vec![];

        for schema_field in &mut schema_def.fields {
            let mut potential_types = vec![];

            resolve_all_aliases_type_expr(&mut schema_field.type_expr, type_env);
            potential_types.push((schema_field.type_expr.0.clone(), schema_field.type_expr.1));

            if let Some(branch) = &mut schema_field.if_branch {
                resolve_all_aliases_value_expr(&mut branch.0.condition, type_env);
                if let Some(value_expr) = &mut branch.0.value_expr {
                    resolve_all_aliases_value_expr(value_expr, type_env);
                    potential_types.push((
                        TypeExpr::from_value_expr(value_expr, type_env),
                        value_expr.1,
                    ));
                }
            }

            if let Some(value_expr) = &mut schema_field.else_branch_value_expr {
                resolve_all_aliases_value_expr(value_expr, type_env);
                potential_types.push((
                    TypeExpr::from_value_expr(value_expr, type_env),
                    value_expr.1,
                ));
            }

            resolve_all_aliases_type_expr(&mut schema_field.type_expr, type_env);

            let field_type_expr = if potential_types.is_empty() {
                unreachable!("compiler error: schemas should not allow no typings?")
            } else if potential_types.len() == 1 {
                potential_types.first().unwrap()
            } else {
                // todo: concat spans
                &(
                    TypeExpr::Or(potential_types.clone()),
                    potential_types.first().unwrap().1,
                )
            };

            fields_with_type.push((schema_field.name.clone(), field_type_expr.clone()));
        }

        let mut schema_fn_return_type = (
            TypeExpr::Duck(Duck {
                fields: vec![Field {
                    name: "from_json".to_string(),
                    type_expr: (
                        TypeExpr::Fun(
                            vec![(None, (TypeExpr::String(None), schema_def.span))],
                            Box::new((
                                TypeExpr::Or(vec![
                                    (
                                        TypeExpr::Duck(Duck {
                                            fields: fields_with_type
                                                .iter()
                                                .map(|(name, type_expr)| Field {
                                                    name: name.clone(),
                                                    type_expr: type_expr.clone(),
                                                })
                                                .collect::<Vec<_>>(),
                                        }),
                                        schema_def.span,
                                    ),
                                    (TypeExpr::Tag("err".to_string()), schema_def.span),
                                ]),
                                schema_def.span,
                            )),
                            false,
                        ),
                        schema_def.span,
                    ),
                }],
            }),
            schema_def.span,
        );

        let mut schema_fn_type =
            TypeExpr::Fun(vec![], Box::new(schema_fn_return_type.clone()), false);

        sort_fields_type_expr(&mut schema_fn_return_type.0);
        sort_fields_type_expr(&mut schema_fn_type);

        schema_def.out_type = Some(schema_fn_return_type);
        schema_def.schema_fn_type = Some((schema_fn_type.clone(), schema_def.span));

        type_env.insert_identifier_type(schema_def.name.clone(), schema_fn_type, true, true);
    });

    source_file
        .jsx_compontents
        .iter_mut()
        .for_each(|jsx_component| {
            resolve_all_aliases_type_expr(&mut jsx_component.props_type, type_env);
            type_env.insert_identifier_type(
                jsx_component.name.clone(),
                TypeExpr::Fun(
                    vec![(Some("props".to_string()), jsx_component.props_type.clone())],
                    Box::new((
                        TypeExpr::Tuple(vec![
                            (TypeExpr::String(None), jsx_component.javascript_source.1),
                            (TypeExpr::String(None), jsx_component.javascript_source.1),
                        ]),
                        jsx_component.javascript_source.1,
                    )),
                    true,
                ),
                true,
                false,
            );
        });

    for global in &mut source_file.global_var_decls {
        resolve_all_aliases_type_expr(&mut global.type_expr, type_env);
        resolve_all_aliases_value_expr(&mut global.initializer, type_env);
        type_env.insert_identifier_type(
            global.name.clone(),
            global.type_expr.0.clone(),
            !global.is_mut,
            false,
        );
    }

    source_file
        .duckx_components
        .iter_mut()
        .for_each(|duckx_component| {
            resolve_all_aliases_type_expr(&mut duckx_component.props_type, type_env);
            resolve_all_aliases_value_expr(&mut duckx_component.value_expr, type_env);

            type_env.insert_identifier_type(
                duckx_component.name.clone(),
                TypeExpr::Fun(
                    vec![(
                        Some("props".to_string()),
                        duckx_component.props_type.clone(),
                    )],
                    Box::new((TypeExpr::Html, duckx_component.value_expr.1)),
                    true,
                ),
                true,
                false,
            );
            type_env.duckx_components.push(duckx_component.clone());
        });

    for comp in &source_file.jsx_compontents {
        type_env.jsx_components.push(comp.clone());
        type_env.check_for_tailwind(&comp.javascript_source.0);
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
            for (_, p) in &mut function_definition.params {
                resolve_all_aliases_type_expr(p, type_env);
                if function_definition.generics.is_empty() {
                    process_keyof_in_type_expr(&mut p.0, type_env);
                }
            }

            type_env.push_type_aliases();

            for type_param in function_definition
                .generics
                .iter_mut()
                .flat_map(|g| g.0.constraint.iter_mut())
            {
                // println!("replacing type params in {}", function_definition.name);
                resolve_all_aliases_type_expr(type_param, type_env);
            }

            for (g, _) in function_definition.generics.iter() {
                type_env.insert_type_alias(g.name.clone(), TypeExpr::TemplParam(g.name.clone()));
            }

            resolve_all_aliases_type_expr(&mut function_definition.return_type, type_env);
            if function_definition.generics.is_empty() {
                process_keyof_in_type_expr(&mut function_definition.return_type.0, type_env);
            }

            let mut fn_type_expr = TypeExpr::Fun(
                function_definition
                    .params
                    .iter()
                    .map(|(identifier, type_expr)| (Some(identifier.clone()), type_expr.clone()))
                    .collect::<Vec<_>>(),
                Box::new((
                    function_definition.return_type.0.clone(),
                    function_definition.return_type.1,
                )),
                true,
            );

            if let TypeExpr::Fun(_, return_type, _) = &mut fn_type_expr
                && let TypeExpr::And(_) = &return_type.0
            {
                return_type.0 = translate_intersection_to_duck(&return_type.0);
            }

            if function_definition.name.starts_with("gimme") {
                // dbg!(&function_definition);
            }
            resolve_all_aliases_value_expr(&mut function_definition.value_expr, type_env);
            if function_definition.name.starts_with("gimme") {
                // dbg!(&function_definition.value_expr);
            }
            type_env.pop_type_aliases();

            if function_definition.generics.is_empty() {
                type_env.insert_identifier_type(
                    function_definition.name.clone(),
                    fn_type_expr,
                    true,
                    false,
                );
            }
            type_env
                .function_definitions
                .push(function_definition.clone());
        });
    println!("{} typeresolve functions", Tag::TypeResolve);
    println!("{} final resolve of all functions", Tag::TypeResolve);

    for global in &mut source_file.global_var_decls {
        resolve_all_aliases_type_expr(&mut global.type_expr, type_env);
        resolve_all_aliases_value_expr(&mut global.initializer, type_env);
        typeresolve_value_expr((&mut global.initializer.0, global.initializer.1), type_env);
        type_env.insert_identifier_type(
            global.name.clone(),
            global.type_expr.0.clone(),
            !global.is_mut,
            false,
        );
    }
    for s in &mut source_file.jsx_compontents {
        typeresolve_jsx_component(s, type_env);
    }

    for s in &mut source_file.duckx_components {
        typeresolve_duckx_component(s, type_env);
    }

    // TODO: typeresolve for tests can be disabled when not in test mode
    for test_case in &mut source_file.test_cases {
        typeresolve_test_case(test_case, type_env);
    }

    type_env.jsx_components = source_file.jsx_compontents.clone();
    type_env.duckx_components = source_file.duckx_components.clone();

    for extensions_def in &mut source_file.extensions_defs {
        typeresolve_extensions_def(extensions_def, type_env);
    }

    source_file.schema_defs.iter_mut().for_each(|schema_def| {
        typeresolve_schema_def(schema_def, type_env);
    });

    source_file
        .struct_definitions
        .iter_mut()
        .for_each(|struct_definition| {
            if struct_definition.generics.is_empty() {
                typeresolve_struct_def(struct_definition, vec![], type_env, true);
            }
        });

    source_file
        .function_definitions
        .iter_mut()
        .for_each(|function_defintion| {
            if function_defintion.generics.is_empty() {
                typeresolve_function_definition(function_defintion, type_env);
                process_keyof_in_value_expr(&mut function_defintion.value_expr, type_env);
            }

            if function_defintion.name.starts_with("gimme") {
                // dbg!(&function_defintion);
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
    function_definition
        .generics
        .iter()
        .for_each(|(generic, _)| {
            type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
        });

    let (return_type, _span) = &mut function_definition.return_type;
    if let TypeExpr::And(_) = &return_type {
        *return_type = translate_intersection_to_duck(return_type);
    }

    type_env.push_identifier_types();

    for p in function_definition.params.iter() {
        type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false, false);
    }

    type_env.insert_identifier_type(
        "self".to_string(),
        if is_mut {
            TypeExpr::RefMut(self_type.into())
        } else {
            TypeExpr::Ref(self_type.into())
        },
        true,
        false,
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
    function_definition
        .generics
        .iter()
        .for_each(|(generic, _)| {
            type_env.insert_type_alias(generic.name.clone(), TypeExpr::Any);
        });

    let (return_type, _span) = &mut function_definition.return_type;

    if let TypeExpr::And(_) = &return_type {
        *return_type = translate_intersection_to_duck(return_type);
    }

    type_env.push_identifier_types();

    for p in function_definition.params.iter_mut() {
        type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false, false);
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
        ValueExpr::InlineGo(_, ty) => {
            if ty.is_none() {
                *ty = Some(req.clone());
            }
        }
        ValueExpr::Int(_, ty) => {
            if matches!(req.0, TypeExpr::Int) || matches!(req.0, TypeExpr::UInt) {
                *ty = Some(req.clone());
            }
        }
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
                    expr.return_type = Some(ret_type.as_ref().clone());
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
        ValueExpr::Negate(inner) => {
            infer_against(inner, req, type_env);
        }
        _ => {}
    }
}

fn typeresolve_value_expr(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let span = &value_expr.1;
    let value_expr = value_expr.0;
    let owned = value_expr.clone();
    match value_expr {
        ValueExpr::Negate(v) => {
            typeresolve_value_expr((&mut v.0, v.1), type_env);

            let ty = TypeExpr::from_value_expr(v, type_env);

            match ty {
                TypeExpr::Int | TypeExpr::Float => {}
                TypeExpr::UInt => {
                    let msg = "Cannot negate unsigned ints since they have no negative";
                    failure_with_occurence(msg, v.1, [(msg, v.1)]);
                }
                _ => {
                    let msg = "Can only negate numbers (Int, UInt, Float)";
                    failure_with_occurence(msg, v.1, [(msg, v.1)]);
                }
            }
        }
        ValueExpr::RawStruct { .. } => panic!("raw struct should not be here {value_expr:?}"),
        ValueExpr::Async(inner) => {
            if !matches!(inner.0, ValueExpr::FunctionCall { .. }) {
                let msg = "Can only async call a function call".to_string();
                failure_with_occurence(msg.clone(), *span, [(msg.clone(), inner.1)]);
            }

            typeresolve_value_expr((&mut inner.0, inner.1), type_env);

            let inner_type = TypeExpr::from_value_expr(inner.as_ref(), type_env);

            let channel_type = TypeExpr::from_value_expr(&(owned, *span), type_env);
            if let TypeExpr::Struct {
                ref name,
                ref type_params,
            } = channel_type
            {
                type_env.get_struct_def_with_type_params_mut(name, type_params, *span);
            }
            let new_channel_fn_name = mangle(&["std", "sync", "Channel", "new"]);

            let fn_type = type_env
                .function_definitions
                .iter()
                .find(|s| s.name == new_channel_fn_name)
                .expect("new channel fn not found")
                .type_expr();

            let mut new_channel_call = ValueExpr::FunctionCall {
                target: (
                    ValueExpr::Variable(
                        true,
                        new_channel_fn_name.clone(),
                        Some(fn_type.0),
                        Some(true),
                        false,
                    ),
                    *span,
                )
                    .into(),
                params: vec![],
                type_params: vec![(inner_type.clone(), *span)],
            };

            typeresolve_function_call((&mut new_channel_call, *span), type_env);

            let TypeExpr::Struct {
                name: chan_struct_name,
                type_params: chan_struct_type_params,
            } = TypeExpr::from_value_expr(&new_channel_call.clone().into_empty_span(), type_env)
            else {
                panic!("Compiler Bug: Async doesn't return channel")
            };

            let channel_struct_def = type_env
                .get_struct_def_with_type_params_mut(
                    &chan_struct_name,
                    &chan_struct_type_params,
                    *span,
                )
                .clone();

            // this loop ensures that all channel methods are emitted
            for m in &channel_struct_def.methods {
                if !m.generics.is_empty() {
                    continue;
                }

                typeresolve_value_expr(
                    (
                        &mut ValueExpr::FieldAccess {
                            target_obj: new_channel_call.clone().into_empty_span().into(),
                            field_name: m.name.clone(),
                        },
                        *span,
                    ),
                    type_env,
                );
            }
        }
        ValueExpr::Defer(inner) => {
            typeresolve_value_expr((&mut inner.0, inner.1), type_env);
            if !matches!(inner.0, ValueExpr::FunctionCall { .. }) {
                let msg = "Can only defer a function call".to_string();
                failure_with_occurence(msg.clone(), *span, [(msg.clone(), inner.1)]);
            }
        }
        ValueExpr::As(v, t) => {
            if let ValueExpr::InlineGo(_, ty) = &mut v.0 {
                *ty = Some(t.clone());
            } else if let ValueExpr::Int(_, ty) = &mut v.0 {
                *ty = Some(t.clone());
            }

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

            let current = &mut target_type;
            let ref_type = 0;

            let fail = || {
                let msg = "Can only use for on std::col::Iterator".to_string();
                failure_with_occurence(msg.clone(), target.1, [(msg.clone(), target.1)])
            };
            if let TypeExpr::Struct { name, type_params } = current.clone() {
                let unmangled = unmangle(name.as_str());

                if unmangled.as_slice() != &["std", "col", "Iter"] || type_params.len() != 1 {
                    fail();
                }

                let new_iter_call = mangle(&["std", "col", "Iter", "from"]);

                let mut new_iter_call_expr = ValueExpr::FunctionCall {
                    target: ValueExpr::Variable(
                        true,
                        new_iter_call,
                        Some(TypeExpr::Fun(
                            vec![(
                                None,
                                TypeExpr::Fun(
                                    vec![],
                                    TypeExpr::Or(vec![
                                        type_params[0].clone(),
                                        TypeExpr::Tag("no_next_elem".to_string()).into_empty_span(),
                                    ])
                                    .into_empty_span()
                                    .into(),
                                    true,
                                )
                                .into_empty_span(),
                            )],
                            (current.clone(), target.1).into(),
                            true,
                        )),
                        Some(true),
                        false,
                    )
                    .into_empty_span()
                    .into(),
                    params: vec![
                        ValueExpr::Lambda(
                            LambdaFunctionExpr {
                                is_mut: true,
                                params: vec![],
                                return_type: Some(
                                    TypeExpr::Or(vec![
                                        type_params[0].clone(),
                                        TypeExpr::Tag("no_next_elem".to_string()).into_empty_span(),
                                    ])
                                    .into_empty_span()
                                    .into(),
                                ),
                                value_expr: ValueExpr::Return(Some(
                                    ValueExpr::Tag("no_next_elem".to_string())
                                        .into_empty_span()
                                        .into(),
                                ))
                                .into_empty_span()
                                .into(),
                            }
                            .into(),
                        )
                        .into_empty_span(),
                    ],
                    type_params: vec![type_params[0].clone()],
                };

                typeresolve_value_expr((&mut new_iter_call_expr, target.1), type_env);

                let mut iter_def = type_env
                    .get_struct_def_with_type_params_mut(&name, &type_params, target.1)
                    .clone();

                // this loop ensures that all channel methods are emitted
                for m in iter_def.methods.iter_mut() {
                    if !m.generics.is_empty() {
                        continue;
                    }

                    typeresolve_value_expr(
                        (
                            &mut ValueExpr::FieldAccess {
                                target_obj: new_iter_call_expr.clone().into_empty_span().into(),
                                field_name: m.name.clone(),
                            },
                            *span,
                        ),
                        type_env,
                    );
                }

                let content_type = type_params[0].clone();

                if ref_type > 0 {
                    target_type = if ref_type == 1 {
                        TypeExpr::Ref(content_type.clone().into())
                    } else {
                        TypeExpr::RefMut(content_type.clone().into())
                    };
                } else {
                    *current = content_type.0.clone();
                }
            } else {
                fail();
            }

            type_env.push_identifier_types();
            *ty = Some(target_type.clone());
            type_env.insert_identifier_type(ident.clone(), target_type, *is_const, false);
            typeresolve_value_expr((&mut block.0, block.1), type_env);
            type_env.pop_identifier_types();
        }

        ValueExpr::Deref(v) => {
            typeresolve_value_expr((&mut v.0, v.1), type_env);
            let t = TypeExpr::from_value_expr(v, type_env);
            if !t.implements_copy(type_env) {
                let msg = "The value of this type needs to implement Copy since it is dereferenced, but it does not";
                failure_with_occurence(msg, v.1, [(msg, v.1)]);
            }
        }

        ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            if let ValueExpr::Variable(_, _, _, _, needs_copy) = &mut v.0 {
                *needs_copy = false;
            }

            typeresolve_value_expr((&mut v.0, v.1), type_env);

            let t = TypeExpr::from_value_expr(v, type_env);
            if t.is_duck() {
                let msg = "Cannot take references to ducks";
                failure_with_occurence(msg, v.1, [(msg, v.1)]);
            }
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
            let (type_expr, is_const, _) = type_env
                .get_identifier_type_and_const(&ident)
                .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {ident}"));
            resolve_all_aliases_type_expr(&mut type_expr.clone().into_empty_span(), type_env);
            *value_expr = ValueExpr::Variable(true, ident, Some(type_expr), Some(is_const), true);
        }
        ValueExpr::VarDecl(..) => typeresolve_var_decl((value_expr, *span), type_env),
        ValueExpr::FormattedString(contents) => {
            for c in contents {
                match c {
                    ValFmtStringContents::Expr(e) => {
                        typeresolve_value_expr((&mut e.0, e.1), type_env)
                    }
                    ValFmtStringContents::String(s) => {
                        type_env.check_for_tailwind(s);
                    }
                }
            }
        }
        ValueExpr::ArrayAccess(target, idx) => {
            let target = target.as_mut();

            if let ValueExpr::Variable(_, _, _, _, needs_copy) = &mut target.0 {
                *needs_copy = false;
            }

            let idx = idx.as_mut();
            typeresolve_value_expr((&mut target.0, target.1), type_env);
            typeresolve_value_expr((&mut idx.0, idx.1), type_env);
        }
        ValueExpr::Array(exprs) => {
            if exprs.is_empty() {
                return;
            }
            for expr in exprs {
                typeresolve_value_expr((&mut expr.0, expr.1), type_env);
            }
        }
        ValueExpr::InlineGo(_, ty) => {
            if ty.is_none() {
                *ty = Some(TypeExpr::unit_with_span(*span));
            }
        }
        ValueExpr::Lambda(..) => typeresolve_lambda((value_expr, *span), type_env),
        ValueExpr::FunctionCall { .. } => typeresolve_function_call((value_expr, *span), type_env),
        ValueExpr::Variable(..) => typeresolve_variable((value_expr, *span), type_env),
        ValueExpr::If { .. } => typeresolve_if_expr((value_expr, *span), type_env),
        ValueExpr::While { .. } => typeresolve_while((value_expr, *span), type_env),
        ValueExpr::Tuple(..) => typeresolve_tuple((value_expr, *span), type_env),
        ValueExpr::Block(..) => typeresolve_block((value_expr, *span), type_env),
        ValueExpr::Duck(..) => typeresolve_duck_value_expr((value_expr, *span), type_env),
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            if let ValueExpr::Variable(_, _, _, _, needs_copy) = &mut target_obj.0 {
                *needs_copy = false;
            }
            let target_obj = target_obj.as_mut();
            typeresolve_value_expr((&mut target_obj.0, target_obj.1), type_env);

            let target_type = TypeExpr::from_value_expr(target_obj, type_env);

            if let TypeExpr::Struct { name, type_params } = &target_type {
                let mut def = type_env
                    .get_struct_def_with_type_params_mut(name, type_params, *span)
                    .clone();

                if type_env.total_structs_resolved.insert(def.name.clone()) {
                    for m in &mut def.methods {
                        if !m.generics.is_empty() {
                            continue;
                        }

                        if !type_env.mark_resolved(&def.name, &m.name) {
                            continue;
                        }

                        type_env.push_identifier_types();

                        for p in &mut m.params {
                            type_env.insert_identifier_type(
                                p.0.clone(),
                                p.1.0.clone(),
                                false,
                                false,
                            );
                        }

                        type_env.insert_identifier_type(
                            "self".to_string(),
                            if def.mut_methods.contains(&m.name) {
                                TypeExpr::RefMut(target_type.clone().into_empty_span().into())
                            } else {
                                TypeExpr::Ref(target_type.clone().into_empty_span().into())
                            },
                            true,
                            false,
                        );

                        typeresolve_value_expr((&mut m.value_expr.0, m.value_expr.1), type_env);
                        // if m.name == "for_each" {
                        //     dbg!(&m.value_expr.0);
                        // }

                        type_env.pop_identifier_types();
                    }

                    let new_struct_name = def.name.clone();

                    type_env
                        .struct_definitions
                        .retain(|f| f.name.as_str() != new_struct_name.as_str());
                    type_env
                        .generic_structs_generated
                        .retain(|f| f.name.as_str() != new_struct_name.as_str());
                    if type_params.is_empty() {
                        type_env.struct_definitions.push(def);
                    } else {
                        type_env.generic_structs_generated.push(def);
                    }
                }
            }
        }
        ValueExpr::Struct { .. } => typeresolve_struct((value_expr, *span), type_env),
        ValueExpr::Return(Some(value_expr)) => {
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
        }
        ValueExpr::VarAssign(..) => typeresolve_var_assign((value_expr, *span), type_env),
        ValueExpr::Match { .. } => typeresolve_match((value_expr, *span), type_env),
        ValueExpr::BoolNegate(value_expr) => {
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
        }
        ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Add(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            typeresolve_value_expr((&mut lhs.0, lhs.1), type_env);
            typeresolve_value_expr((&mut rhs.0, rhs.1), type_env);
        }
        ValueExpr::String(str, _) => {
            type_env.check_for_tailwind(str);
        }
        ValueExpr::Int(_, t) => {
            if let Some(t) = t.as_ref().as_ref()
                && !matches!(t.0, TypeExpr::Int | TypeExpr::UInt)
            {
                let msg = "Int literal can only coerce to number type";
                failure_with_occurence(msg, t.1, [(msg, t.1), (msg, *span)]);
            }

            if t.is_none() {
                *t = Some((TypeExpr::Int, *span));
            }
        }
        ValueExpr::Bool(..)
        | ValueExpr::Tag(..)
        | ValueExpr::Char(..)
        | ValueExpr::Float(..)
        | ValueExpr::Break
        | ValueExpr::Return(None)
        | ValueExpr::Continue => {}
    }
}

fn typeresolve_match(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Match {
        value_expr,
        arms,
        else_arm,
        span: _,
    } = value_expr.0
    else {
        unreachable!("only pass match exprs to this function")
    };

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
                false,
            );
        }
        typeresolve_value_expr((&mut arm.value_expr.0, arm.value_expr.1), type_env);
        type_env.pop_identifier_types();
    }
}

fn typeresolve_function_call(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let span = value_expr.1;
    let ValueExpr::FunctionCall {
        target,
        params,
        type_params,
    } = value_expr.0
    else {
        unreachable!("only pass functioncalls to this function")
    };

    if let ValueExpr::Variable(_, _, _, _, needs_copy) = &mut target.0 {
        *needs_copy = false;
    }

    let header: FunHeader;
    if type_params.is_empty() {
        typeresolve_value_expr((&mut target.0, target.1), type_env);

        let target_type = TypeExpr::from_value_expr(target, type_env);

        let TypeExpr::Fun(def_params, def_ret, _) = target_type else {
            failure_with_occurence(
                "Can only call functions",
                span,
                [("Can only call functions", span)],
            )
        };

        header = FunHeader {
            params: def_params.into_iter().map(|(_, x)| x).collect(),
            return_type: def_ret.as_ref().clone(),
        };
    } else {
        match &mut target.0 {
            ValueExpr::Variable(_, name, ty, _, _needs_copy) => {
                let fn_def = type_env
                    .function_definitions
                    .iter()
                    .find(|x| name.as_str() == x.name.as_str())
                    .cloned()
                    .unwrap_or_else(|| panic!("could not find {name} {type_params:?}"));

                if type_params.len() != fn_def.generics.len() {
                    let msg = "Wrong number of type parameters";
                    failure_with_occurence(msg, span, [(msg, span)])
                }

                let generic_arguments = fn_def
                    .generics
                    .iter()
                    .map(|(x, _)| x)
                    .zip(type_params.iter())
                    .fold(IndexMap::new(), |mut acc, (def, arg)| {
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
                cloned_fn_def.generics.clear();

                let prevent_generic_generation = type_env
                    .prevent_generic_generation
                    .insert(new_fn_name.clone());

                if prevent_generic_generation {
                    replace_generics_in_function_definition(
                        &mut cloned_fn_def,
                        &generic_arguments,
                        type_env,
                    );
                    for p in &mut cloned_fn_def.params.iter_mut() {
                        process_keyof_in_type_expr(&mut p.1.0, type_env);
                    }
                    process_keyof_in_type_expr(&mut cloned_fn_def.return_type.0, type_env);
                    process_keyof_in_value_expr(&mut cloned_fn_def.value_expr, type_env);
                    type_env.function_headers.insert(
                        new_fn_name.clone(),
                        FunHeader {
                            params: cloned_fn_def
                                .params
                                .iter()
                                .cloned()
                                .map(|(_, x)| x)
                                .collect(),
                            return_type: cloned_fn_def.return_type.clone(),
                        },
                    );
                    typeresolve_function_definition(&mut cloned_fn_def, type_env);
                    type_env.generic_fns_generated.push(cloned_fn_def.clone());
                }
                header = type_env.get_method_header(&new_fn_name);
                *ty = Some(TypeExpr::Fun(
                    cloned_fn_def
                        .params
                        .iter()
                        .cloned()
                        .map(|x| (Some(x.0), x.1))
                        .collect(),
                    cloned_fn_def.return_type.clone().into(),
                    true,
                ));
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                typeresolve_value_expr((&mut target_obj.0, target_obj.1), type_env);

                let self_type = TypeExpr::from_value_expr_dereferenced(target_obj, type_env);
                let target_type = TypeExpr::from_value_expr_dereferenced(target_obj, type_env);

                let TypeExpr::Struct {
                    name: struct_name,
                    type_params: struct_type_params,
                } = target_type.clone()
                else {
                    let msg = "Can only generic method call a struct";
                    failure_with_occurence(msg, span, [(msg, span)]);
                };

                let mut mut_struct_def = type_env
                    .get_struct_def_with_type_params_mut(&struct_name, &struct_type_params, span)
                    .clone();

                for m in &mut mut_struct_def.methods {
                    if !m.generics.is_empty() {
                        continue;
                    }

                    if !type_env.mark_resolved(&mut_struct_def.name, &m.name) {
                        continue;
                    }

                    type_env.push_identifier_types();

                    for p in &mut m.params {
                        type_env.insert_identifier_type(p.0.clone(), p.1.0.clone(), false, false);
                    }

                    type_env.insert_identifier_type(
                        "self".to_string(),
                        if mut_struct_def.mut_methods.contains(&m.name) {
                            TypeExpr::RefMut(target_type.clone().into_empty_span().into())
                        } else {
                            TypeExpr::Ref(target_type.clone().into_empty_span().into())
                        },
                        true,
                        false,
                    );

                    typeresolve_value_expr((&mut m.value_expr.0, m.value_expr.1), type_env);
                    // if m.name == "for_each" {
                    //     dbg!(&m.value_expr.0);
                    // }

                    type_env.pop_identifier_types();
                }

                {
                    let new_struct_name = mut_struct_def.name.clone();

                    type_env
                        .struct_definitions
                        .retain(|f| f.name.as_str() != new_struct_name.as_str());
                    type_env
                        .generic_structs_generated
                        .retain(|f| f.name.as_str() != new_struct_name.as_str());
                    if struct_type_params.is_empty() {
                        type_env.struct_definitions.push(mut_struct_def);
                    } else {
                        type_env.generic_structs_generated.push(mut_struct_def);
                    }
                }

                let mut_struct_def = type_env.get_struct_def_with_type_params_mut(
                    &struct_name,
                    &struct_type_params,
                    span,
                );

                let replaced_struct_name = mut_struct_def.name.clone();

                let method = mut_struct_def
                    .methods
                    .iter()
                    .find(|m| m.name.as_str() == field_name.as_str())
                    .cloned()
                    .unwrap();

                if type_params.len() != method.generics.len() {
                    let msg = "Wrong number of type parameters";
                    failure_with_occurence(msg, span, [(msg, span)])
                }

                let is_mut = mut_struct_def.mut_methods.contains(&method.name);

                let generic_arguments = method
                    .generics
                    .iter()
                    // .chain(mut_struct_def.generics.clone().iter())
                    .map(|(x, _)| x)
                    .zip(type_params.iter()) //.chain(struct_type_params.iter()))
                    .fold(IndexMap::new(), |mut acc, (def, arg)| {
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
                cloned_fn_def.generics.clear();

                if type_env
                    .prevent_generic_generation
                    .insert(global_generic_generation_id.clone())
                {
                    replace_generics_in_function_definition(
                        &mut cloned_fn_def,
                        &generic_arguments,
                        type_env,
                    );
                    for p in &mut cloned_fn_def.params.iter_mut() {
                        resolve_all_aliases_type_expr(&mut p.1, type_env);
                        process_keyof_in_type_expr(&mut p.1.0, type_env);
                    }
                    {
                        let p = &mut cloned_fn_def.return_type;
                        resolve_all_aliases_type_expr(p, type_env);
                        process_keyof_in_type_expr(&mut p.0, type_env);
                    }
                    process_keyof_in_value_expr(&mut cloned_fn_def.value_expr, type_env);
                    type_env.function_headers.insert(
                        global_generic_generation_id.clone(),
                        FunHeader {
                            params: cloned_fn_def
                                .params
                                .iter()
                                .cloned()
                                .map(|(_, x)| x)
                                .collect(),
                            return_type: cloned_fn_def.return_type.clone(),
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
                failure_with_occurence(msg, span, [(msg, span)]);
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

fn typeresolve_var_decl(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let _span = value_expr.1;
    let ValueExpr::VarDecl(declaration) = value_expr.0 else {
        unreachable!("only pass var declarations to this function")
    };

    let declaration = &mut declaration.0;

    // Resolve the type expression on the declaration
    if let Some(type_expr) = &mut declaration.type_expr {
        if let TypeExpr::And(_) = &type_expr.0 {
            type_expr.0 = translate_intersection_to_duck(&type_expr.0);
        }

        resolve_all_aliases_type_expr(type_expr, type_env);

        if let Some(initializer) = declaration.initializer.as_mut() {
            infer_against(initializer, type_expr, type_env);

            typeresolve_value_expr((&mut initializer.0, initializer.1), type_env);
        }
    } else if let Some(initializer) = declaration.initializer.as_mut() {
        typeresolve_value_expr((&mut initializer.0, initializer.1), type_env);

        let type_expr = TypeExpr::from_value_expr(initializer, type_env);
        declaration.type_expr = Some((type_expr.clone(), initializer.1));
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
        false,
    );
}

fn typeresolve_var_assign(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let _span = value_expr.1;
    let ValueExpr::VarAssign(assignment) = value_expr.0 else {
        unreachable!("only pass var assignments to this function")
    };

    typeresolve_value_expr(
        (&mut assignment.0.target.0, assignment.0.target.1),
        type_env,
    );
    let target_type = TypeExpr::from_value_expr(&assignment.0.target, type_env);

    if let ValueExpr::Variable(_, _, _, _, needs_copy) = &mut assignment.0.target.0 {
        *needs_copy = false;
    }

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

fn typeresolve_struct(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let span = value_expr.1;
    let ValueExpr::Struct {
        name,
        fields,
        type_params,
    } = value_expr.0
    else {
        unreachable!("only pass structs to this function")
    };

    let og_def = type_env.get_struct_def(name);

    if type_params.len() != og_def.generics.len() {
        let msg = "Wrong number of type parameters A";
        failure_with_occurence(msg, span, [(msg, span)])
    }

    let def = type_env
        .get_struct_def_with_type_params_mut(name.as_str(), type_params.as_slice(), span)
        .clone();

    fields
        .iter_mut()
        .zip(def.fields.iter())
        .for_each(|((_field_name, value_expr), def_field)| {
            infer_against(value_expr, &def_field.type_expr, type_env);
            typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
        });
}

fn typeresolve_block(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Block(value_exprs) = value_expr.0 else {
        unreachable!("only pass structs to this function")
    };

    type_env.push_identifier_types();
    value_exprs
        .iter_mut()
        .for_each(|value_expr| typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env));
    type_env.pop_identifier_types();
}

fn typeresolve_duck_value_expr(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Duck(items) = value_expr.0 else {
        unreachable!("only pass structs to this function")
    };

    items.iter_mut().for_each(|(_, value_expr)| {
        typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env)
    });
}

fn typeresolve_tuple(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Tuple(value_exprs) = value_expr.0 else {
        unreachable!("only pass structs to this function")
    };

    value_exprs
        .iter_mut()
        .for_each(|value_expr| typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env));
}

fn typeresolve_while(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::While { condition, body } = value_expr.0 else {
        unreachable!("only pass structs to this function")
    };

    typeresolve_value_expr((&mut condition.0, condition.1), type_env);
    type_env.push_identifier_types();
    typeresolve_value_expr((&mut body.0, body.1), type_env);
    type_env.pop_identifier_types();
}

fn typeresolve_if_expr(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::If {
        condition,
        then,
        r#else,
    } = value_expr.0
    else {
        unreachable!("only pass structs to this function")
    };

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

fn typeresolve_variable(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Variable(_, identifier, type_expr_opt, const_opt, needs_copy) = value_expr.0
    else {
        unreachable!("only pass structs to this function")
    };
    // if let Some(type_expr) = type_expr_opt {
    //     resolve_all_aliases_type_expr(type_expr, type_env);
    //     return;
    // }
    let (type_expr, is_const, _) = type_env
        .get_identifier_type_and_const(identifier)
        .unwrap_or_else(|| panic!("Couldn't resolve type of identifier {identifier}"));

    //resolve_all_aliases_type_expr(&mut type_expr, type_env, generics_to_ignore);

    if *needs_copy && !type_expr.implements_copy(type_env) {
        failure_with_occurence(
            "This type is not trivially copyable",
            value_expr.1,
            [(
                "A type is trivially copyable if it's either a primitive, an immutable reference or a composition of primitive types",
                value_expr.1,
            )],
        )
    }

    *type_expr_opt = Some(type_expr);
    *const_opt = Some(is_const);
}

fn typeresolve_lambda(value_expr: SpannedMutRef<ValueExpr>, type_env: &mut TypeEnv) {
    let ValueExpr::Lambda(lambda_body) = value_expr.0 else {
        unreachable!("only pass structs to this function")
    };

    let LambdaFunctionExpr {
        is_mut,
        params,
        return_type,
        value_expr,
    } = &mut **lambda_body;

    let captured = type_env.identifier_types[1..]
        .iter()
        .flat_map(|v| v.iter())
        .fold(HashMap::new(), |mut acc, (name, (ty, is_const, _))| {
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
        type_env.insert_identifier_type(name, ty, !capture_as_mut, false);
    }

    for (name, ty) in params {
        let ty = ty.as_mut().unwrap();
        type_env.insert_identifier_type(name.to_owned(), ty.0.clone(), false, false);
        resolve_all_aliases_type_expr(ty, type_env);
    }

    if let Some(return_type) = return_type {
        resolve_all_aliases_type_expr(return_type, type_env);
    }

    if let Some(return_type) = return_type.as_ref() {
        infer_against(value_expr, return_type, type_env);
    }

    if return_type.is_none() {
        *return_type = Some((TypeExpr::Tuple(vec![]), value_expr.1));
    }

    typeresolve_value_expr((&mut value_expr.0, value_expr.1), type_env);
    type_env.pop_identifier_types();
}
