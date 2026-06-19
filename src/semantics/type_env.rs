use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::mpsc::Sender;

use chumsky::container::Container;
use indexmap::IndexMap;

use crate::parse::generics_parser::Generic;
use crate::parse::source_file_parser::SourceFile;
use crate::parse::type_parser::Duck;
use crate::parse::value_parser::{IntoBlock, IntoReturn, ValueExpr, empty_range};
use crate::parse::{SS, failure_with_occurence};
use crate::parse::{
    Spanned,
    duckx_component_parser::DuckxComponent,
    function_parser::FunctionDefintion,
    jsx_component_parser::{JsxComponent, JsxComponentDependencies},
    schema_def_parser::SchemaDefinition,
    struct_parser::{NamedDuckDefinition, StructDefinition},
    type_parser::{TypeDefinition, TypeExpr},
};
use crate::semantics::ident_mangler::MANGLE_SEP;
use crate::semantics::type_resolve::{
    NeedsSearchResult, build_struct_generic_id, build_tuples_and_ducks_type_expr_trav_fn,
    build_tuples_and_ducks_value_expr_trav_fn, replace_generics_in_named_duck_def,
    replace_generics_in_struct_definition, resolve_all_aliases_type_expr, trav_type_expr,
    trav_value_expr, typeresolve_struct_def,
};
use crate::semantics::typechecker::check_type_compatability;

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
            function_definitions: vec![FunctionDefintion {
                name: "parse_json".to_string(),
                return_type: TypeExpr::Or(vec![
                    TypeExpr::TemplParam("T".to_string()).into_empty_span(),
                    TypeExpr::Tag("err".to_string()).into_empty_span(),
                ])
                .into_empty_span(),
                params: vec![(
                    "json_str".to_string(),
                    TypeExpr::String(None).into_empty_span(),
                )],
                value_expr: ValueExpr::InlineGo(
                    String::new(),
                    Some(TypeExpr::Never.into_empty_span()),
                )
                .into_empty_span()
                .into_block()
                .into_return(),
                generics: vec![(
                    Generic {
                        name: "T".to_string(),
                        constraint: None,
                    },
                    empty_range(),
                )],
                span: empty_range(),
                comments: vec![],
            }],
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
                        "Unknown Struct",
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
                "Unkown Struct",
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

    pub fn find_ducks_and_tuples(&mut self, src_file: &SourceFile) -> Vec<NeedsSearchResult> {
        let mut result = Vec::new();

        let cloned_resolve = self.resolved_methods.clone();

        for duckx_comp in &mut self.duckx_components.clone() {
            self.find_ducks_and_tuples_type_expr(&mut duckx_comp.props_type, &mut result);
            self.find_tuples_and_ducks_value_expr(&mut duckx_comp.value_expr, &mut result);
        }

        for jsx_comp in &mut self.jsx_components.clone() {
            self.find_ducks_and_tuples_type_expr(&mut jsx_comp.props_type, &mut result);
        }

        for fun_def in self
            .function_definitions
            .clone()
            .iter_mut()
            .chain(
                src_file
                    .extensions_defs
                    .clone()
                    .iter_mut()
                    .flat_map(|x| x.function_definitions.iter_mut().map(|x| &mut x.0)),
            )
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
