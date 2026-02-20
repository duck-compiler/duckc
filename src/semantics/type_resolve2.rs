use std::collections::{HashMap, HashSet};

use colored::Colorize;

const INITIAL_MAP_CAP: usize = 4096;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    emit::types::AsDereferenced,
    parse::{
        Field, SS, Spanned,
        duckx_component_parser::DuckxComponent,
        extensions_def_parser::ExtensionsDef,
        failure_with_occurence,
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        generics_parser::Generic,
        jsx_component_parser::{Edit, JsxComponent, JsxSourceUnit, do_edits},
        source_file_parser::SourceFile,
        struct_parser::{DerivableInterface, StructDefinition},
        type_parser::{Duck, TypeExpr, merge_or},
        value_parser::{
            MatchArm, TypeParam, ValFmtStringContents, ValHtmlStringContents, ValueExpr,
            empty_range, type_expr_into_empty_range,
        },
    },
    semantics::{
        ident_mangler::{mangle, unmangle},
        type_resolve::{
            NeedsSearchResult, TravControlFlow, TypeEnv, build_struct_generic_id,
            build_struct_generic_id2, infer_against, infer_against2, is_base_const_var,
            is_const_var, replace_generics_in_struct_definition, replace_generics_in_type_expr,
            replace_generics_in_type_expr2, replace_generics_in_value_expr2, trav_type_expr2,
            unset_const_func_call_assign, unset_copy_var_assign,
        },
    },
};

#[derive(Debug, Clone)]
pub struct FunHeader {
    pub generics: Vec<Generic>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

impl FunHeader {
    pub fn to_type(&self) -> TypeExpr {
        TypeExpr::Fun(
            self.params.iter().map(|x| (None, x.clone())).collect(),
            self.return_type.clone().into(),
            false,
        )
    }
}

#[derive(Debug, Clone)]
pub struct MethodHeader {
    pub is_mut: bool,
    pub generics: Vec<Spanned<Generic>>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

impl MethodHeader {
    pub fn to_type(&self) -> TypeExpr {
        FunHeader {
            generics: self.generics.iter().map(|(x, _)| x.clone()).collect(),
            params: self.params.clone(),
            return_type: self.return_type.clone(),
        }
        .to_type()
    }
}

#[derive(Debug, Clone)]
pub struct StructHeader {
    pub fields: HashMap<String, Spanned<TypeExpr>>,
    pub methods: HashMap<String, MethodHeader>,
    pub generics: Vec<Spanned<Generic>>,
    pub derived: HashSet<DerivableInterface>,
}

impl StructHeader {
    pub fn is_mut_method(&self, name: &str) -> bool {
        self.methods.get(name).filter(|s| s.is_mut).is_some()
    }
}

#[derive(Debug, Clone)]
pub struct InterfaceDefinition {
    pub fields: HashMap<String, Spanned<TypeExpr>>,
    pub generics: Vec<Spanned<Generic>>,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub typ: Spanned<TypeExpr>,
    pub is_const: bool,
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub generics: Vec<Spanned<Generic>>,
    pub expr: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct DuckxComponentHeader {
    pub props: Spanned<TypeExpr>,
}

fn typeresolve_jsx_component(c: &mut JsxComponent, globals: &GlobalsEnv) {
    reduce_type_expr(&mut c.props_type, globals, None);
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

                if globals.jsx_component_headers.contains_key(ident) {
                    globals
                        .jsx_component_dependencies
                        .entry_sync(c.name.clone())
                        .or_default()
                        .get_mut()
                        .client_components
                        .insert_sync(ident.to_string())
                        .unwrap();
                }
            }
        }
    }
    do_edits(&mut c.javascript_source.0, &mut edits);
}

#[derive(Debug, Clone, Default)]
pub struct JsxComponentDependencies {
    pub client_components: scc::HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct JsxComponentHeader {
    pub props_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct GlobalsEnv {
    pub type_aliases: HashMap<String, TypeAlias>,

    pub jsx_component_dependencies: scc::HashMap<String, JsxComponentDependencies>,

    pub extension_functions: scc::HashMap<String, (Spanned<TypeExpr>, TypeExpr)>, // key = extension function name, (actual_fn_type, access_fn_type)

    pub jsx_component_headers: HashMap<String, JsxComponentHeader>,
    pub duckx_component_headers: HashMap<String, DuckxComponentHeader>,

    pub structs: HashMap<String, StructDefinition>,
    pub struct_headers: HashMap<String, StructHeader>,

    pub functions: HashMap<String, FunctionDefintion>,
    pub function_headers: HashMap<String, FunHeader>,

    pub global_variables: HashMap<String, VariableInfo>,

    pub generics_output: GenericsOutput,

    pub used_objects: scc::HashSet<String>,
    pub total_resolved: scc::HashSet<String>,
    pub resolved_methods: scc::HashMap<String, HashSet<String>>,
    pub all_go_imports: HashSet<String>,
}

impl Default for GlobalsEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalsEnv {
    pub fn new() -> GlobalsEnv {
        Self {
            used_objects: scc::HashSet::with_capacity(INITIAL_MAP_CAP),
            type_aliases: HashMap::with_capacity(INITIAL_MAP_CAP),
            structs: HashMap::with_capacity(256),
            struct_headers: HashMap::with_capacity(256),
            functions: HashMap::with_capacity(INITIAL_MAP_CAP),
            function_headers: HashMap::with_capacity(INITIAL_MAP_CAP),
            global_variables: HashMap::with_capacity(INITIAL_MAP_CAP),
            extension_functions: scc::HashMap::with_capacity(INITIAL_MAP_CAP),
            total_resolved: scc::HashSet::with_capacity(INITIAL_MAP_CAP),
            generics_output: GenericsOutput::default(),
            all_go_imports: HashSet::with_capacity(128),
            jsx_component_dependencies: scc::HashMap::with_capacity(INITIAL_MAP_CAP),
            jsx_component_headers: HashMap::with_capacity(INITIAL_MAP_CAP),
            duckx_component_headers: HashMap::with_capacity(INITIAL_MAP_CAP),
            resolved_methods: scc::HashMap::with_capacity(INITIAL_MAP_CAP),
        }
    }

    pub fn get_fun_header(&self, name: &str, params: &[Spanned<TypeExpr>]) -> Option<FunHeader> {
        if params.is_empty() {
            self.function_headers.get(name).cloned()
        } else {
            if let Some(header) = self
                .generics_output
                .get_generic_function_header(name, params)
            {
                Some(header)
            } else {
                let def = self.functions.get(name);
                def.map(|s| {
                    self.generics_output
                        .function_create_generic_instance(s, self, params);
                    self.generics_output
                        .get_generic_function_header(name, params)
                })
                .flatten()
            }
        }
    }

    pub fn get_method_header(
        &self,
        struct_name: &str,
        struct_params: &[Spanned<TypeExpr>],
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<MethodHeader> {
        self.get_struct_header(struct_name, struct_params)
            .map(|s| {
                if params.is_empty() {
                    s.methods.get(name).cloned()
                } else {
                    self.generics_output
                        .get_generic_method_header(struct_name, struct_params, name, params)
                        .or_else(|| {
                            self.generics_output.struct_create_generic_instance(
                                self.structs.get(struct_name).unwrap(),
                                self,
                                struct_params,
                            );
                            let s = self.generics_output.generic_structs.read_sync(
                                &build_struct_generic_id2(struct_name, struct_params),
                                |_, v| v.clone(),
                            );

                            s.map(|s| {
                                self.generics_output.method_create_generic_instance(
                                    &s,
                                    struct_name,
                                    struct_params,
                                    name,
                                    params,
                                    self,
                                );
                                self.generics_output.get_generic_method_header(
                                    struct_name,
                                    struct_params,
                                    name,
                                    params,
                                )
                            })
                            .flatten()
                        })
                }
            })
            .flatten()
    }

    pub fn get_struct_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<StructHeader> {
        if params.is_empty() {
            self.struct_headers.get(name).cloned()
        } else {
            if let Some(header) = self.generics_output.get_generic_struct_header(name, params) {
                Some(header)
            } else {
                let def = self.structs.get(name);
                def.map(|s| {
                    self.generics_output
                        .struct_create_generic_instance(s, self, params);
                    self.generics_output.get_generic_struct_header(name, params)
                })
                .flatten()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericsOutput {
    pub generic_block: scc::HashSet<String>,
    pub generic_functions: scc::HashMap<String, FunctionDefintion>,
    pub generic_function_headers: scc::HashMap<String, FunHeader>,
    pub generic_interfaces: scc::HashMap<String, Spanned<TypeExpr>>,
    pub generic_structs: scc::HashMap<String, StructDefinition>,
    pub generic_struct_headers: scc::HashMap<String, StructHeader>,
    pub generic_methods: scc::HashMap<String, scc::HashMap<String, FunctionDefintion>>,
    pub generic_method_headers: scc::HashMap<String, scc::HashMap<String, MethodHeader>>,
    pub resolve_block: scc::HashSet<String>,
}

impl GenericsOutput {
    pub fn get_generic_function_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<FunHeader> {
        self.generic_function_headers
            .read_sync(&build_struct_generic_id2(name, params), |_, v| v.clone())
    }

    pub fn get_generic_method_header(
        &self,
        struct_name: &str,
        struct_params: &[Spanned<TypeExpr>],
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<MethodHeader> {
        self.generic_method_headers
            .read_sync(
                &build_struct_generic_id2(struct_name, struct_params),
                |_, v| v.read_sync(&build_struct_generic_id2(name, params), |_, v| v.clone()),
            )
            .flatten()
    }

    pub fn get_generic_struct_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<StructHeader> {
        self.generic_struct_headers
            .read_sync(&build_struct_generic_id2(name, params), |_, v| v.clone())
    }

    pub fn resolve_all_methods(
        &self,
        struct_name: &str,
        type_params: &[Spanned<TypeExpr>],
        globals: &GlobalsEnv,
    ) {
        let id = &build_struct_generic_id2(struct_name, type_params);
        if self.resolve_block.insert_sync(id.to_string()).is_err() {
            return;
        }
        self.generic_structs.update_sync(id, |_, struct_def| {
            let mut_methods = struct_def.mut_methods.clone();
            let _ = globals.total_resolved.insert_sync(struct_def.name.clone());
            for method in &mut struct_def.methods {
                if !method.generics.is_empty() {
                    continue;
                }

                globals
                    .resolved_methods
                    .entry_sync(struct_def.name.clone())
                    .or_default()
                    .get_mut()
                    .insert(method.name.clone());

                let mut locals = LocalScoped::default();
                let is_mut = mut_methods.contains(&method.name);
                let self_type = TypeExpr::Struct {
                    name: struct_name.to_string(),
                    type_params: type_params.to_vec(),
                }
                .into_empty_span();

                for param in method.params.iter() {
                    locals.set_info_for_ident(
                        param.0.clone(),
                        VariableInfo {
                            typ: param.1.clone(),
                            is_const: true,
                        },
                    );
                }

                locals.set_info_for_ident(
                    "self".to_string(),
                    VariableInfo {
                        typ: if is_mut {
                            TypeExpr::RefMut(self_type.into()).into_empty_span()
                        } else {
                            TypeExpr::Ref(self_type.into()).into_empty_span()
                        },
                        is_const: true,
                    },
                );

                typeresolve_value_expr(&mut method.value_expr, globals, &mut locals);
                // if method.name.contains("give_me_a_list") {
                //     dbg!(&struct_def.name, &method.name, &method.value_expr);
                // }
            }
        });
    }

    pub fn method_create_generic_instance(
        &self,
        s: &StructDefinition,
        s_name: &str,
        s_params: &[Spanned<TypeParam>],
        method_name: &str,
        params: &[Spanned<TypeParam>],
        globals: &GlobalsEnv,
    ) {
        let id = build_struct_generic_id2(&method_name, params);

        if !self
            .generic_block
            .insert_sync(format!("{}_{id}", s.name))
            .is_ok()
        {
            return;
        }
        let base_def = match s
            .methods
            .iter()
            .find(|m| m.name == method_name && m.generics.len() == params.len())
        {
            Some(s) => s,
            _ => return,
        };

        let mut result = base_def.clone();
        result.generics.clear();
        result.name = id;

        let generics = params.iter().zip(base_def.generics.iter()).fold(
            IndexMap::with_capacity(base_def.generics.len()),
            |mut acc, e| {
                acc.insert(e.1.0.name.clone(), e.0.0.clone());
                acc
            },
        );
        let name = result.name.clone();

        let self_type = TypeExpr::Struct {
            name: s_name.to_string(),
            type_params: s_params.to_vec(),
        }
        .into_empty_span();

        for (_, ty) in result.params.iter_mut() {
            replace_generics_in_type_expr2(ty, &generics, globals);
            reduce_type_expr(ty, globals, Some(&self_type.0));
        }
        replace_generics_in_type_expr2(&mut result.return_type, &generics, globals);
        reduce_type_expr(&mut result.return_type, globals, Some(&self_type.0));

        let mut locals = LocalScoped::default();
        let is_mut = s.mut_methods.contains(method_name);

        locals.set_info_for_ident(
            "self".to_string(),
            VariableInfo {
                typ: if is_mut {
                    TypeExpr::RefMut(self_type.into()).into_empty_span()
                } else {
                    TypeExpr::Ref(self_type.into()).into_empty_span()
                },
                is_const: true,
            },
        );

        resolve_all_types_function_definition(
            &mut result,
            globals,
            move |header| {
                self.generic_method_headers
                    .entry_sync(s.name.clone())
                    .or_default()
                    .insert_sync(
                        name,
                        MethodHeader {
                            generics: vec![],
                            is_mut,
                            params: header.params,
                            return_type: header.return_type,
                        },
                    )
                    .unwrap();
            },
            &generics,
            &mut locals,
        );

        self.generic_methods
            .entry_sync(s.name.clone())
            .or_default()
            .insert_sync(result.name.clone(), result)
            .unwrap();
    }

    pub fn function_create_generic_instance(
        &self,
        base_def: &FunctionDefintion,
        globals: &GlobalsEnv,
        params: &[Spanned<TypeParam>],
    ) {
        let id = build_struct_generic_id2(&base_def.name, params);
        if !self.generic_block.insert_sync(id.clone()).is_ok() {
            return;
        }
        let mut result = base_def.clone();
        result.generics.clear();
        result.name = id;

        let generics = params.iter().zip(base_def.generics.iter()).fold(
            IndexMap::with_capacity(base_def.generics.len()),
            |mut acc, e| {
                acc.insert(e.1.0.name.clone(), e.0.0.clone());
                acc
            },
        );
        let name = result.name.clone();
        resolve_all_types_function_definition(
            &mut result,
            globals,
            move |header| {
                self.generic_function_headers
                    .insert_sync(name, header)
                    .unwrap();
            },
            &generics,
            &mut LocalScoped::default(),
        );

        self.generic_functions
            .insert_sync(result.name.clone(), result.clone())
            .unwrap();
    }

    pub fn struct_create_generic_instance(
        &self,
        base_def: &StructDefinition,
        globals: &GlobalsEnv,
        params: &[Spanned<TypeParam>],
    ) {
        let id = build_struct_generic_id2(&base_def.name, params);
        if !self.generic_block.insert_sync(id.clone()).is_ok() {
            return;
        }
        let mut result = base_def.clone();
        result.generics.clear();
        result.name = id;

        let generics = params.iter().zip(base_def.generics.iter()).fold(
            IndexMap::with_capacity(base_def.generics.len()),
            |mut acc, e| {
                acc.insert(e.1.0.name.clone(), e.0.0.clone());
                acc
            },
        );

        replace_generics_in_struct_definition2(&mut result, &generics, globals);

        let name = result.name.clone();
        // TODO: fix for cascading (don't resolve immediately)
        resolve_all_types_struct_definition(
            &mut result,
            globals,
            &generics,
            Some(&TypeExpr::Struct {
                name: base_def.name.clone(),
                type_params: params.to_vec(),
            }),
            move |header| {
                self.generic_struct_headers
                    .insert_sync(name, header)
                    .unwrap();
            },
        );

        self.generic_structs
            .insert_sync(result.name.clone(), result)
            .unwrap();
    }
}

pub fn replace_generics_in_struct_definition2(
    def: &mut StructDefinition,
    generics: &IndexMap<String, TypeExpr>,
    type_env: &GlobalsEnv,
) {
    for f in def.fields.iter_mut() {
        replace_generics_in_type_expr2(&mut f.type_expr, generics, type_env);
    }

    for m in def.methods.iter_mut() {
        for t in [&mut m.return_type]
            .into_iter()
            .chain(m.params.iter_mut().map(|x| &mut x.1))
        {
            replace_generics_in_type_expr2(t, generics, type_env);
        }
        replace_generics_in_value_expr2(&mut m.value_expr.expr.0, generics, type_env);
    }
}

impl Default for GenericsOutput {
    fn default() -> Self {
        Self {
            resolve_block: scc::HashSet::with_capacity(256),
            generic_block: scc::HashSet::with_capacity(256),
            generic_methods: scc::HashMap::with_capacity(4096),
            generic_method_headers: scc::HashMap::with_capacity(4096),
            generic_functions: scc::HashMap::with_capacity(4096),
            generic_function_headers: scc::HashMap::with_capacity(4096),
            generic_interfaces: scc::HashMap::with_capacity(256),
            generic_structs: scc::HashMap::with_capacity(4096),
            generic_struct_headers: scc::HashMap::with_capacity(4096),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalScoped {
    pub idents: Vec<HashMap<String, VariableInfo>>,
}

impl Default for LocalScoped {
    fn default() -> Self {
        LocalScoped {
            idents: {
                let mut v = Vec::with_capacity(4);
                v.push(HashMap::with_capacity(16));
                v
            },
        }
    }
}

impl LocalScoped {
    pub fn set_info_for_ident(&mut self, name: String, v: VariableInfo) {
        self.idents
            .last_mut()
            .expect("Compiler Bug: no scope")
            .insert(name, v);
    }
    pub fn get_info_for_ident<'a>(&'a self, name: &str) -> Option<&'a VariableInfo> {
        self.idents.iter().rev().find_map(|scope| scope.get(name))
    }
    pub fn push_scope(&mut self) {
        self.idents.push(HashMap::new());
    }
    pub fn pop_scope(&mut self) {
        self.idents.pop().expect("Compiler Bug: no scope to pop");
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "'de: 'static"))]
pub struct ValueExprWithType {
    pub expr: Spanned<ValueExpr>,
    pub typ: Spanned<TypeExpr>,
}

pub trait NeverTypeExt {
    fn replace_if_not_never(&mut self, t: &Spanned<TypeExpr>);
    fn replace_if_other_never(&mut self, t: &Spanned<TypeExpr>);
}

impl NeverTypeExt for Spanned<TypeExpr> {
    fn replace_if_not_never(&mut self, t: &Spanned<TypeExpr>) {
        if !self.0.is_never() {
            self.0 = t.0.clone();
            self.1 = t.1;
        }
    }

    fn replace_if_other_never(&mut self, t: &Spanned<TypeExpr>) {
        if t.0.is_never() {
            self.0 = t.0.clone();
            self.1 = t.1;
        }
    }
}

impl ValueExprWithType {
    pub fn n(expr: Spanned<ValueExpr>) -> Self {
        let s = expr.1;
        Self {
            expr,
            typ: (TypeExpr::Uninit, s),
        }
    }
    pub fn new(expr: Spanned<ValueExpr>, typ: Spanned<TypeExpr>) -> Self {
        Self { expr, typ }
    }
}

// replaces OR, AND and type aliases
pub fn reduce_type_expr(
    t: &mut Spanned<TypeExpr>,
    globals: &GlobalsEnv,
    self_type: Option<&TypeExpr>,
) {
    trav_type_expr2(
        |t, _| match &mut t.0 {
            TypeExpr::TypeName(..) => {
                let mut current = t.clone();

                while let TypeExpr::TypeName(_, ident, params) = &current.0 {
                    if ident == "Self"
                        && let Some(ty) = self_type.clone()
                        && params.is_empty()
                    {
                        current = ty.clone().into_empty_span();
                        break;
                    }

                    if let Some(alias) = globals.type_aliases.get(ident) {
                        if params.len() != alias.generics.len() {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, t.1, [(msg, t.1), (msg, alias.expr.1)]);
                        }

                        let mut generic_params = IndexMap::with_capacity(params.len());

                        for (param, alias_param) in params.iter().zip(alias.generics.iter()) {
                            generic_params.insert(alias_param.0.name.clone(), param.0.clone());
                        }

                        let mut rhs = alias.expr.clone();

                        replace_generics_in_type_expr2(&mut rhs, &generic_params, globals);
                        current = rhs;
                        continue;
                    }

                    if let Some(struct_def) = globals.structs.get(ident) {
                        if params.len() != struct_def.generics.len() {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, t.1, [(msg, t.1)]);
                        }

                        current.0 = TypeExpr::Struct {
                            name: ident.clone(),
                            type_params: params.clone(),
                        };
                        break;
                    }

                    let msg = &format!("Could not find type definition for {ident}");
                    failure_with_occurence(msg, t.1, [(msg, t.1)]);
                }
                t.0 = current.0;
                reduce_type_expr(t, globals, self_type);
                return TravControlFlow::Cancel;
            }
            TypeExpr::And(operands) => {
                let mut out_duck = IndexMap::<String, Spanned<TypeExpr>>::new();
                for operand in operands {
                    reduce_type_expr(operand, globals, self_type.clone());
                    if let TypeExpr::Duck(Duck { fields }) = &operand.0 {
                        for field in fields {
                            if let Some(ty) = out_duck.get(&field.name) {
                                if ty.0.type_id2(globals) != field.type_expr.0.type_id2(globals) {
                                    let msg = &format!("Duplicate definition for {}", field.name);
                                    failure_with_occurence(
                                        msg,
                                        t.1,
                                        [(msg, operand.1), (msg, ty.1)],
                                    );
                                }
                            }
                            out_duck.insert(field.name.clone(), field.type_expr.clone());
                        }
                    } else {
                        let msg = "Intersection (&) can only be applied to ducks";
                        failure_with_occurence(msg, operand.1, [(msg, operand.1)]);
                    }
                }

                let len = out_duck.len();
                let fields = out_duck
                    .into_iter()
                    .fold(Vec::with_capacity(len), |mut acc, elem| {
                        acc.push(Field {
                            name: elem.0,
                            type_expr: elem.1,
                        });
                        acc
                    });

                t.0 = TypeExpr::Duck(Duck { fields });

                return TravControlFlow::Cancel;
            }
            TypeExpr::Or(operands) => {
                let mut out_operands = Vec::with_capacity(operands.len() + 2);
                for operand in operands.iter_mut() {
                    reduce_type_expr(operand, globals, self_type.clone());
                    merge_or(operand, &mut out_operands);
                }
                out_operands.sort_by_key(|t| t.0.type_id2(globals));
                out_operands.dedup_by_key(|t| t.0.type_id2(globals));
                if out_operands.len() == 1 {
                    t.0 = out_operands.pop().expect("Compiler Bug: unreachable").0;
                } else {
                    *operands = out_operands;
                }
                return TravControlFlow::Cancel;
            }
            _ => TravControlFlow::Continue,
        },
        t,
        &mut TypeEnv::default(),
    );
}

fn resolve_all_types_struct_definition<F>(
    struct_def: &mut StructDefinition,
    globals: &GlobalsEnv,
    generics: &IndexMap<String, TypeExpr>,
    self_type: Option<&TypeExpr>,
    header_callback: F,
) where
    F: FnOnce(StructHeader),
{
    for field in struct_def.fields.iter_mut() {
        replace_generics_in_type_expr2(&mut field.type_expr, generics, globals);
        reduce_type_expr(&mut field.type_expr, globals, self_type);
    }

    for method in struct_def.methods.iter_mut() {
        if !method.generics.is_empty() {
            continue;
        }
        for param in method.params.iter_mut() {
            replace_generics_in_type_expr2(&mut param.1, generics, globals);
            reduce_type_expr(&mut param.1, globals, self_type);
        }
        replace_generics_in_type_expr2(&mut method.return_type, generics, globals);
        reduce_type_expr(&mut method.return_type, globals, self_type);
        // TODO: Self type in value expr types
        replace_generics_in_value_expr2(&mut method.value_expr.expr.0, generics, globals);
    }

    header_callback(struct_def.to_header());

    let _ = globals.total_resolved.insert_sync(struct_def.name.clone());

    for method in struct_def.methods.iter_mut() {
        if !method.generics.is_empty() {
            continue;
        }
        globals
            .resolved_methods
            .entry_sync(struct_def.name.clone())
            .or_default()
            .get_mut()
            .insert(method.name.clone());

        let mut local_scope = LocalScoped::default();
        for param in method.params.iter_mut() {
            local_scope.set_info_for_ident(
                param.0.clone(),
                VariableInfo {
                    typ: param.1.clone(),
                    is_const: false,
                },
            );
        }

        let is_mut = struct_def.mut_methods.contains(&method.name);
        let self_type = self_type.cloned().unwrap().into_empty_span();

        local_scope.set_info_for_ident(
            "self".to_string(),
            VariableInfo {
                typ: if is_mut {
                    TypeExpr::RefMut(self_type.into()).into_empty_span()
                } else {
                    TypeExpr::Ref(self_type.into()).into_empty_span()
                },
                is_const: true,
            },
        );
        typeresolve_value_expr(&mut method.value_expr, globals, &mut local_scope);
    }
}

fn resolve_all_types_function_definition<F>(
    fun_def: &mut FunctionDefintion,
    globals: &GlobalsEnv,
    header_callback: F,
    generics: &IndexMap<String, TypeExpr>,
    local_scope: &mut LocalScoped,
) where
    F: FnOnce(FunHeader),
{
    local_scope.push_scope();

    for p in fun_def.params.iter_mut() {
        replace_generics_in_type_expr2(&mut p.1, generics, globals);
        local_scope.set_info_for_ident(
            p.0.clone(),
            VariableInfo {
                is_const: false,
                typ: p.1.clone(),
            },
        );
    }

    replace_generics_in_type_expr2(&mut fun_def.return_type, generics, globals);
    header_callback(fun_def.to_header2());
    replace_generics_in_value_expr2(&mut fun_def.value_expr.expr.0, generics, globals);
    typeresolve_value_expr(&mut fun_def.value_expr, globals, local_scope);

    local_scope.pop_scope();
}

fn typeresolve_struct(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let span = value_expr.expr.1;
    let ValueExpr::Struct {
        name,
        fields,
        type_params,
    } = &mut value_expr.expr.0
    else {
        unreachable!("only pass structs to this function")
    };

    let mut as_type = (
        TypeExpr::TypeName(true, name.clone(), type_params.to_vec()),
        value_expr.expr.1,
    );
    reduce_type_expr(&mut as_type, globals, None);
    {
        let TypeExpr::Struct {
            name: a,
            type_params: b,
        } = as_type.0
        else {
            panic!("Compiler Bug: {as_type:?}")
        };
        *name = a;
        *type_params = b;
    }

    let Some(og_def) = globals.get_struct_header(name, &[]) else {
        let msg = &format!("Struct {name} does not exist");
        failure_with_occurence(msg, span, [(msg, span)])
    };

    if fields.len() != og_def.fields.len() {
        let msg = "Amount of fields doesn't match.";

        let mut hints = vec![format!(
            "{} has {} fields. You provided {} fields",
            name,
            og_def.fields.len(),
            fields.len(),
        )];

        let fields_that_are_too_much = fields
            .iter()
            .map(|field| field.0.clone())
            .filter(|field| {
                !og_def
                    .fields
                    .iter()
                    .map(|og_field| og_field.0.clone())
                    .any(|field_name| field_name.as_str() == field.as_str())
            })
            .collect::<Vec<String>>();

        let missing_fields = og_def
            .fields
            .iter()
            .map(|field| field.0.clone())
            .filter(|field| !fields.iter().any(|given_field| given_field.0 == *field))
            .collect::<Vec<String>>();

        if !fields_that_are_too_much.is_empty() {
            hints.push(format!(
                "The field(s) {} do not exist on type {}",
                fields_that_are_too_much.join(", ").yellow(),
                name.yellow(),
            ));
        }

        if !missing_fields.is_empty() {
            hints.push(format!(
                "The field(s) {} are/is missing",
                missing_fields.join(", ").to_string().yellow(),
            ));
        }

        failure_with_occurence(msg, span, vec![(hints.join(". "), span)]);
    }

    for f in fields.iter() {
        if !og_def
            .fields
            .iter()
            .any(|og_field| og_field.0.as_str() == f.0.as_str())
        {
            let msg = format!("Invalid field {}", f.0);
            failure_with_occurence(
                msg,
                span,
                [(
                    format!(
                        "{} doesn't have a field named {}. It has fields {}",
                        name,
                        f.0,
                        og_def
                            .fields
                            .iter()
                            .map(|f| f.0.clone())
                            .collect::<Vec<_>>()
                            .join(", "),
                    ),
                    span,
                )],
            );
        }
    }

    if type_params.is_empty() && !og_def.generics.is_empty() {
        let og_def = og_def.clone();
        let og_gen = og_def.generics.clone();

        let mut infered_generics = og_def.generics.iter().cloned().fold(
            IndexMap::<String, Option<Spanned<TypeExpr>>>::new(),
            |mut acc, e| {
                acc.insert(e.0.name.clone(), None);
                acc
            },
        );

        for (p, t) in fields.iter_mut().map(|f| {
            (
                &mut f.1,
                og_def
                    .fields
                    .iter()
                    .find_map(|f2| {
                        if f2.0.as_str() == f.0.as_str() {
                            Some(f2.1.clone())
                        } else {
                            None
                        }
                    })
                    .unwrap(),
            )
        }) {
            typeresolve_value_expr(p, globals, locals);
            // infer_type_params(t, &p.typ, &mut infered_generics, type_env);
        }

        let mut new_type_params = Vec::new();

        for (name, infered) in infered_generics.into_iter() {
            if let Some(infered) = infered {
                new_type_params.push(infered);
            } else {
                let msg = &format!("Could not infer type for template parameter {name}");
                let span = og_gen
                    .iter()
                    .find_map(|x| {
                        if x.0.name.as_str() == name.as_str() {
                            Some(x.1)
                        } else {
                            None
                        }
                    })
                    .unwrap();

                failure_with_occurence(
                    msg,
                    value_expr.expr.1,
                    [
                        (msg, value_expr.expr.1),
                        (
                            &format!("The type param {name} must be known at compiletime"),
                            span,
                        ),
                    ],
                );
            }
        }

        *type_params = new_type_params;
        typeresolve_struct(value_expr, globals, locals);
        return;
    }

    if type_params.len() != og_def.generics.len() {
        let msg = "Wrong number of type parameters";
        failure_with_occurence(msg, span, [(msg, span)])
    }

    for t in type_params.iter_mut() {
        reduce_type_expr(t, globals, None);
    }

    let def = globals
        .get_struct_header(name.as_str(), type_params.as_slice())
        .unwrap()
        .clone();

    for f in fields.iter_mut() {
        let og_field = &def
            .fields
            .iter()
            .find(|og_field| og_field.0.as_str() == f.0.as_str())
            .unwrap()
            .1;
        infer_against2(&mut f.1, &og_field, globals);
        typeresolve_value_expr(&mut f.1, globals, locals);
        check_type_compatability(og_field, &f.1.typ, globals);
        if f.1.typ.0.is_never() {
            value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
        }
    }
    value_expr.typ.replace_if_not_never(&(
        TypeExpr::Struct {
            name: name.clone(),
            type_params: type_params.clone(),
        },
        value_expr.expr.1,
    ));
}

pub fn check_type_compatability(
    required_type: &Spanned<TypeExpr>,
    given_type: &Spanned<TypeExpr>,
    globals: &GlobalsEnv,
) {
    let given_type = given_type.clone();

    if matches!(given_type.0, TypeExpr::Uninit) {
        panic!("un init type");
    }

    if matches!(given_type.0, TypeExpr::TemplParam(..) | TypeExpr::Never) {
        return;
    }

    let fail_requirement = |explain_required: String, explain_given: String| {
        let (smaller, larger) = if required_type.1.start <= given_type.1.start {
            (required_type.1, given_type.1)
        } else {
            (given_type.1, required_type.1)
        };

        // this is unused at the moment but come in handy later
        let _combined_span = SS {
            start: smaller.start,
            end: larger.end,
            context: required_type.1.context,
        };

        failure_with_occurence(
            "Incompatible Types",
            given_type.1,
            vec![
                (explain_required.to_string(), required_type.1),
                (explain_given.to_string(), given_type.1),
            ],
        )
    };

    let is_empty_tuple = if let TypeExpr::Tuple(t) = &required_type.0 {
        t.is_empty()
    } else {
        false
    };

    if !is_empty_tuple && matches!(given_type.0, TypeExpr::Statement) {
        let msg = "Statement is not an expression";
        failure_with_occurence(
            msg,
            given_type.1,
            [(
                "This needs an expression, you provided a statement",
                required_type.1,
            )],
        );
    }

    match &required_type.0 {
        TypeExpr::Uninit => unreachable!("uninit types should have been replaced by now"),
        TypeExpr::Indexed(..) => unreachable!("indexed types should have been replaced by now"),
        TypeExpr::Byte => {
            if !matches!(given_type.0, TypeExpr::Byte) {
                fail_requirement(
                    "this expects a Byte.".to_string(),
                    format!(
                        "this is not a Byte. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::UInt => {
            if !matches!(given_type.0, TypeExpr::UInt) {
                fail_requirement(
                    "this expects a UInt.".to_string(),
                    format!(
                        "this is not a UInt. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Statement => panic!("Compiler Bug: statement should never be required"),
        TypeExpr::TemplParam(..) | TypeExpr::Never => return,
        TypeExpr::NamedDuck { name, type_params } => {
            todo!("named ducks")
        }
        TypeExpr::Ref(req_t) => {
            if let TypeExpr::Ref(given_t) | TypeExpr::RefMut(given_t) = &given_type.0 {
                let mut req_x = req_t.clone();
                let mut given_x = given_t.clone();

                loop {
                    match req_x.0 {
                        TypeExpr::Ref(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::Ref(inner_given_ty)
                            | TypeExpr::RefMut(inner_given_ty) = given_x.0
                            {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id2(globals) != given_x.0.type_id2(globals) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        TypeExpr::RefMut(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::RefMut(inner_given_ty) = given_x.0 {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id2(globals) != given_x.0.type_id2(globals) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        _ => {
                            if req_x.0.type_id2(globals) == given_x.0.type_id2(globals) {
                                break;
                            } else {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                    }
                }
            } else {
                fail_requirement(
                    format!(
                        "This is an immutable reference to {}",
                        required_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                    format!(
                        "So this needs to be an immutable reference or a mutable reference but it's of type {}",
                        given_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                );
            }
        }
        TypeExpr::RefMut(req_t) => {
            if let TypeExpr::RefMut(given_t) = &given_type.0 {
                let mut req_x = req_t.clone();
                let mut given_x = given_t.clone();

                loop {
                    match req_x.0 {
                        TypeExpr::Ref(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::Ref(inner_given_ty)
                            | TypeExpr::RefMut(inner_given_ty) = given_x.0
                            {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id2(globals) != given_x.0.type_id2(globals) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        TypeExpr::RefMut(inner_ty) => {
                            req_x = inner_ty;
                            if let TypeExpr::RefMut(inner_given_ty) = given_x.0 {
                                given_x = inner_given_ty;
                            } else if req_x.0.type_id2(globals) != given_x.0.type_id2(globals) {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                        _ => {
                            if req_x.0.type_id2(globals) == given_x.0.type_id2(globals) {
                                break;
                            } else {
                                fail_requirement(
                                    "Referenced types need to match exactly".to_string(),
                                    "So this needs to be exactly the same type".to_string(),
                                );
                            }
                        }
                    }
                }
            } else {
                fail_requirement(
                    "This is a mutable reference".to_string(),
                    "So this needs to be a mutable reference as well".to_string(),
                );
            }
        }
        TypeExpr::Html => {
            if let TypeExpr::Html = &given_type.0 {
                return;
            }
            fail_requirement(
                format!(
                    "the required type is {}",
                    format!("{}", required_type.0).bright_yellow(),
                ),
                format!(
                    "because of the fact, that the required type is {}. The value you need to pass must be a tag as well, but it is a {}",
                    format!("{}", required_type.0).bright_yellow(),
                    format!("{}", given_type.0).bright_yellow(),
                ),
            )
        }
        TypeExpr::TypeOf(..) => panic!("typeof should have been replaced"),
        TypeExpr::KeyOf(..) => panic!("keyof should have been replaced"),
        TypeExpr::Any => return,
        TypeExpr::Go(_) => return,
        TypeExpr::Tag(required_identifier) => {
            if let TypeExpr::Tag(given_identifier) = &given_type.0 {
                if given_identifier != required_identifier {
                    fail_requirement(
                        format!(
                            "the required tag is {}",
                            format!(".{required_identifier}").bright_yellow(),
                        ),
                        format!(
                            "but you've provided the tag {}",
                            format!(".{given_identifier}").bright_yellow(),
                        ),
                    )
                }

                // everything's okay
                return;
            }

            // todo: produce snapshot for the given error
            fail_requirement(
                format!(
                    "the required type is {}",
                    format!("{}", required_type.0).bright_yellow(),
                ),
                format!(
                    "because of the fact, that the required type is {}. The value you need to pass must be a tag as well, but it is a {}",
                    format!("{}", required_type.0).bright_yellow(),
                    format!("{}", given_type.0).bright_yellow(),
                ),
            )
        }
        TypeExpr::Struct {
            name: req_name,
            type_params: req_type_params,
        } => {
            if let TypeExpr::Struct {
                name: given_name,
                type_params: given_type_params,
            } = &given_type.0
            {
                // TODO: STRUCT DISPLAY NAME
                if req_name.as_str() != given_name.as_str() {
                    fail_requirement(
                        format!("the required struct is {}", req_name.bright_yellow(),),
                        format!("you have given a {}", given_name.bright_yellow(),),
                    );
                }

                for (idx, (req_param, given_param)) in req_type_params
                    .iter()
                    .zip(given_type_params.iter())
                    .enumerate()
                {
                    if req_param.0.type_id2(globals) != given_param.0.type_id2(globals) {
                        failure_with_occurence(
                            "Type parameters do not match",
                            given_param.1,
                            [
                                (
                                    format!(
                                        "type parameter no. {} is required to be a {}",
                                        idx + 1,
                                        req_param.0.as_clean_user_faced_type_name()
                                    ),
                                    req_param.1,
                                ),
                                (
                                    format!(
                                        "you have given a {}",
                                        given_param.0.as_clean_user_faced_type_name()
                                    ),
                                    given_param.1,
                                ),
                            ],
                        );
                    }
                }
            } else {
                fail_requirement(
                    format!(
                        "the required type {} is a struct",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "because of the fact, that the required type {} is a struct. The value you need to pass must be a as well, but it is a {}",
                        format!("{}", required_type.0).bright_yellow(),
                        given_type.0.as_clean_user_faced_type_name(),
                    ),
                )
            }
        }
        TypeExpr::Duck(duck) => {
            if duck.fields.is_empty() {
                return;
            }

            match &given_type.0 {
                TypeExpr::Duck(given_duck) => {
                    let required_duck = duck;

                    for required_field in required_duck.fields.iter() {
                        let companion_field = given_duck
                            .fields
                            .iter()
                            .find(|field| field.name == required_field.name);

                        if companion_field.is_none() {
                            fail_requirement(
                                format!(
                                    "this type states that it has requires a field {} of type {}",
                                    required_field.name.bright_purple(),
                                    format!("{}", required_field.type_expr.0).bright_yellow(),
                                ),
                                format!(
                                    "the given type doesn't have a field {}",
                                    required_field.name.bright_purple(),
                                ),
                            )
                        }

                        let companion_field = companion_field.unwrap();

                        check_type_compatability(
                            &required_field.type_expr,
                            &companion_field.type_expr,
                            globals,
                        );
                    }
                }
                TypeExpr::NamedDuck {
                    name: _,
                    type_params: _,
                } => {
                    assert_eq!(
                        required_type.0.type_id2(globals),
                        given_type.0.type_id2(globals)
                    );
                    return;
                }
                TypeExpr::Struct {
                    name: struct_name,
                    type_params,
                } => {
                    let struct_def = globals.get_struct_header(struct_name, type_params).unwrap();

                    for required_field in duck.fields.iter() {
                        if let TypeExpr::Fun(_, _, is_mut) = required_field.type_expr.0 {
                            let companion_method = struct_def
                                .methods
                                .iter()
                                .find(|method| method.0.as_str() == required_field.name.as_str());

                            if companion_method.is_none() {
                                fail_requirement(
                                    format!(
                                        "this type states that it requires a field {} of type {}",
                                        required_field.name.bright_purple(),
                                        format!("{}", required_field.type_expr.0).bright_yellow(),
                                    ),
                                    format!(
                                        "the given type doesn't have a field or method with name {}",
                                        required_field.name.bright_purple(),
                                    ),
                                );
                            }

                            if is_mut {
                                if !struct_def.is_mut_method(&required_field.name) {
                                    fail_requirement(
                                        format!(
                                            "this type states that it requires a mutable method named {}",
                                            required_field.name.bright_purple(),
                                        ),
                                        format!(
                                            "the given type doesn't have a mutable method named {}",
                                            required_field.name.bright_purple(),
                                        ),
                                    );
                                }
                                if false // TODO: check if struct allows mutable access
                                    && !{
                                        let mut is_mut_ref = false;

                                        let mut current = given_type.0.clone();
                                        while let TypeExpr::RefMut(next) = current {
                                            is_mut_ref = true;

                                            if let TypeExpr::Ref(..) = next.0 {
                                                is_mut_ref = false;
                                                break;
                                            }

                                            current = next.0;
                                        }

                                        is_mut_ref
                                    }
                                {
                                    fail_requirement(
                                        "this needs mutable access".to_string(),
                                        "this is a const var".to_string(),
                                    );
                                }
                            }

                            let companion_method = companion_method.unwrap();
                            check_type_compatability(
                                &required_field.type_expr,
                                &(companion_method.1.to_type(), required_type.1), // TODO: add correct span
                                globals,
                            );
                            return;
                        }

                        let companion_field = struct_def
                            .fields
                            .iter()
                            .find(|field| field.0.as_str() == required_field.name.as_str());

                        if companion_field.is_none() {
                            fail_requirement(
                                format!(
                                    "this type states that it has requires a field {} of type {}",
                                    required_field.name.bright_purple(),
                                    format!("{}", required_field.type_expr.0).bright_yellow(),
                                ),
                                format!(
                                    "the given type doesn't have a field {}",
                                    required_field.name.bright_purple(),
                                ),
                            )
                        }

                        let companion_field = companion_field.unwrap();

                        check_type_compatability(
                            &required_field.type_expr,
                            &companion_field.1,
                            globals,
                        );
                    }
                }
                _ => fail_requirement(
                    format!(
                        "the required type {} is a duck",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    format!(
                        "this must be a duck as well, but it's {}",
                        format!("{}", given_type.0).bright_yellow(),
                    ),
                ),
            }
        }
        TypeExpr::Tuple(item_types) => {
            if item_types.is_empty() && matches!(given_type.0, TypeExpr::Statement) {
                return;
            }
            if !given_type.0.is_tuple() {
                fail_requirement(
                    format!(
                        "{} is a tuple",
                        format!("{}", required_type.0).bright_yellow(),
                    ),
                    String::new(),
                )
            }

            let required_item_types = item_types;
            let TypeExpr::Tuple(given_item_types) = &given_type.0 else {
                unreachable!()
            };

            if given_item_types.len() < required_item_types.len() {
                fail_requirement(
                    format!(
                        "requires {} item(s)",
                        format!("{}", required_item_types.len()).bright_green(),
                    ),
                    format!(
                        "only has {} item(s)",
                        format!("{}", given_item_types.len()).bright_green(),
                    ),
                )
            }

            for (idx, (req_param, given_param)) in required_item_types
                .iter()
                .zip(given_item_types.iter())
                .enumerate()
            {
                if let TypeExpr::Or(req_variants) = &req_param.0
                    && req_variants.iter().any(|variant| {
                        variant.0.type_id2(globals) == given_param.0.type_id2(globals)
                    })
                {
                    return;
                }

                if req_param.0.type_id2(globals) != given_param.0.type_id2(globals) {
                    failure_with_occurence(
                        "Incompatible Types",
                        given_param.1,
                        [
                            (
                                format!(
                                    "item no. {} is required to be a {}",
                                    idx + 1,
                                    req_param.0.as_clean_user_faced_type_name()
                                ),
                                req_param.1,
                            ),
                            (
                                format!(
                                    "you have given a {}",
                                    given_param.0.as_clean_user_faced_type_name()
                                ),
                                given_param.1,
                            ),
                        ],
                    );
                }
            }
        }
        TypeExpr::String(..) => {
            if !given_type.0.is_string() {
                fail_requirement(
                    format!("this expects a {}", "String".yellow()),
                    format!(
                        "this is not a {}. It's of type {}",
                        "String".yellow(),
                        given_type.0.as_clean_user_faced_type_name().yellow()
                    ),
                );
            }
        }
        TypeExpr::Int => {
            if !matches!(given_type.0, TypeExpr::Int) {
                fail_requirement(
                    "this expects an Int.".to_string(),
                    format!(
                        "this is not an Int. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Bool(..) => {
            if !given_type.0.is_bool() {
                fail_requirement(
                    format!("a {} value is required here", "Bool".bright_yellow(),),
                    format!(
                        "this is not a Bool. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Char => {
            if !given_type.0.is_char() {
                fail_requirement(
                    "this expects a Char.".to_string(),
                    "this is not a Char.".to_string(),
                );
            }
        }
        TypeExpr::Float => {
            if !matches!(given_type.0, TypeExpr::Float) {
                fail_requirement(
                    "this expects a Float.".to_string(),
                    format!(
                        "this is not a Float. it's a {}",
                        format!("{}", given_type.0).bright_yellow()
                    ),
                )
            }
        }
        TypeExpr::Or(contents) => {
            let other_contents = if let TypeExpr::Or(other_contents) = &given_type.0 {
                if other_contents.len() > contents.len() {
                    fail_requirement(
                        "This union is smaller than".to_string(),
                        "this one".to_string(),
                    );
                }
                other_contents
            } else {
                &vec![given_type.clone()]
            };

            for giv in other_contents.iter() {
                let found = contents
                    .iter()
                    .any(|c| c.0.type_id2(globals) == giv.0.type_id2(globals));
                if !found {
                    let msg = format!(
                        "This expression is of type `{}`",
                        giv.0.as_clean_user_faced_type_name().blue(),
                    );
                    let msg = msg.as_str();

                    failure_with_occurence(
                        "Incompatible Types",
                        giv.1,
                        [
                            (msg, giv.1),
                            (
                                format!(
                                    "Must be one of these: {}",
                                    contents
                                        .iter()
                                        .map(|c| c.0.as_clean_user_faced_type_name())
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                        .yellow()
                                )
                                .as_str(),
                                required_type.1,
                            ),
                        ],
                    );
                }
            }
        }
        TypeExpr::Fun(required_params, required_return_type, is_mut_required) => {
            if !given_type.0.is_fun() {
                fail_requirement(
                    "this requires a function".to_string(),
                    "this value isn't even a function.".to_string(),
                )
            }

            let TypeExpr::Fun(given_params, given_return_type, is_mut_given) = &given_type.0 else {
                unreachable!("we've already checked that it's a function")
            };

            if !*is_mut_required && *is_mut_given {
                fail_requirement(
                    "this requires a function that does not modify its environment".to_string(),
                    "thus, this must not be a mut fn".to_string(),
                );
            }

            if given_params.len() != required_params.len() {
                fail_requirement(
                    format!(
                        "this requires a function with {} argument(s)",
                        format!("{}", required_params.len()).bright_green(),
                    ),
                    format!(
                        "this is a function, but it takes {} arguments(s)",
                        format_args!("{}", given_params.len())
                    ),
                )
            }

            for (idx, ((_, req_param), (_, given_param))) in
                required_params.iter().zip(given_params.iter()).enumerate()
            {
                if req_param.0.type_id2(globals) != given_param.0.type_id2(globals) {
                    failure_with_occurence(
                        "Parameter types do not match",
                        given_param.1,
                        [
                            (
                                format!(
                                    "parameter no. {} is required to be a {}",
                                    idx + 1,
                                    req_param.0.as_clean_user_faced_type_name()
                                ),
                                req_param.1,
                            ),
                            (
                                format!(
                                    "you have given a {}",
                                    given_param.0.as_clean_user_faced_type_name()
                                ),
                                given_param.1,
                            ),
                        ],
                    );
                }
            }
            if required_return_type.0.type_id2(globals) != given_return_type.0.type_id2(globals) {
                failure_with_occurence(
                    "Return types do not match",
                    given_return_type.1,
                    [
                        (
                            format!(
                                "Return type needs to be a {}",
                                required_return_type.0.as_clean_user_faced_type_name()
                            ),
                            required_return_type.1,
                        ),
                        (
                            format!(
                                "You have given a {}",
                                given_return_type.0.as_clean_user_faced_type_name(),
                            ),
                            given_return_type.1,
                        ),
                    ],
                );
            }
        }

        TypeExpr::Array(content_type) => {
            if !given_type.0.is_array() {
                fail_requirement(
                    format!("this requires an array of {}", &content_type.0),
                    "this is not an array".to_string(),
                );
            }

            let TypeExpr::Array(given_content_type) = given_type.clone().0 else {
                unreachable!("we've checked that given_type is an array")
            };

            if content_type.0.type_id2(globals) != given_content_type.0.type_id2(globals) {
                failure_with_occurence(
                    "Array content types do not match",
                    given_content_type.1,
                    [
                        (
                            format!(
                                "This requires an array of {}",
                                content_type.0.as_clean_user_faced_type_name().yellow()
                            ),
                            content_type.1,
                        ),
                        (
                            format!(
                                "You have given an array of {}",
                                given_content_type
                                    .0
                                    .as_clean_user_faced_type_name()
                                    .yellow()
                            ),
                            given_content_type.1,
                        ),
                    ],
                )
            }
        }
        TypeExpr::RawTypeName(..) | TypeExpr::TypeName(..) | TypeExpr::And(..) => {
            panic!("{required_type:?} should not be here")
        }
    }
}

pub fn resolve_all_types_source_file(source_file: &mut SourceFile) -> GlobalsEnv {
    const NUM_THREADS: usize = 8;
    let globals_env = std::thread::scope(|s| {
        let mut globals_env = GlobalsEnv::new();

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for f in source_file
            .function_definitions
            .chunks((source_file.function_definitions.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(f.len());
                for f in f {
                    res.insert(f.name.clone(), f.clone());
                }
                res
            }));
        }

        let (fns, fn_headers) = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                (
                    HashMap::with_capacity(source_file.function_definitions.len()),
                    HashMap::with_capacity(source_file.function_definitions.len()),
                ),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.1.insert(k.clone(), v.to_header2());
                        acc.0.insert(k, v);
                    }
                    acc
                },
            );

        globals_env.functions = fns;
        globals_env.function_headers = fn_headers;

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for struct_defs in source_file
            .struct_definitions
            .chunks((source_file.struct_definitions.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(struct_defs.len());
                for s in struct_defs {
                    res.insert(s.name.clone(), s.clone());
                }
                res
            }));
        }

        let (structs, struct_headers) = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                (
                    HashMap::with_capacity(source_file.struct_definitions.len()),
                    HashMap::with_capacity(source_file.struct_definitions.len()),
                ),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.1.insert(k.clone(), v.to_header());
                        acc.0.insert(k, v);
                    }
                    acc
                },
            );

        globals_env.structs = structs;
        globals_env.struct_headers = struct_headers;

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for global_var_decl in source_file
            .global_var_decls
            .chunks((source_file.global_var_decls.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(global_var_decl.len());
                for s in global_var_decl {
                    res.insert(
                        s.name.clone(),
                        VariableInfo {
                            typ: s.type_expr.clone(),
                            is_const: !s.is_mut,
                        },
                    );
                }
                res
            }));
        }

        globals_env.global_variables = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.global_var_decls.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for type_def in source_file
            .type_definitions
            .chunks((source_file.type_definitions.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(type_def.len());
                for s in type_def {
                    res.insert(
                        s.name.clone(),
                        TypeAlias {
                            generics: s.generics.clone(),
                            expr: s.type_expression.clone(),
                        },
                    );
                }
                res
            }));
        }

        globals_env.type_aliases = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.type_definitions.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for jsx_components in source_file
            .jsx_components
            .chunks((source_file.jsx_components.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(jsx_components.len());
                for jsx_component in jsx_components {
                    res.insert(
                        jsx_component.name.clone(),
                        JsxComponentHeader {
                            props_type: jsx_component.props_type.clone(),
                        },
                    );
                }
                res
            }));
        }

        globals_env.jsx_component_headers = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.jsx_components.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for duckx_components in source_file
            .duckx_components
            .chunks((source_file.duckx_components.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(duckx_components.len());
                for duckx_component in duckx_components {
                    res.insert(
                        duckx_component.name.clone(),
                        DuckxComponentHeader {
                            props: duckx_component.props_type.clone(),
                        },
                    );
                }
                res
            }));
        }

        globals_env.duckx_component_headers = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.jsx_components.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

        globals_env
            .duckx_component_headers
            .clone()
            .into_iter()
            .for_each(|(name, duckx_component)| {
                globals_env.function_headers.insert(
                    name,
                    FunHeader {
                        generics: Vec::default(),
                        params: vec![duckx_component.props],
                        return_type: TypeExpr::Html.into_empty_span(),
                    },
                );
            });

        globals_env
    });

    {
        let globals_env = &globals_env;
        std::thread::scope(|s| {
            let mut handles = Vec::with_capacity(NUM_THREADS);
            let chunk_size = (source_file.function_definitions.len() / NUM_THREADS).max(1);
            for fun_def in source_file.function_definitions.chunks_mut(chunk_size) {
                handles.push(s.spawn(move || {
                    for f in fun_def {
                        if f.generics.is_empty() {
                            resolve_all_types_function_definition(
                                f,
                                globals_env,
                                |_| {},
                                &IndexMap::default(),
                                &mut LocalScoped::default(),
                            );
                        }
                    }
                }));
            }

            for h in handles.drain(..) {
                h.join().unwrap();
            }

            let chunk_size = (source_file.struct_definitions.len() / NUM_THREADS).max(1);
            for struct_def in source_file.struct_definitions.chunks_mut(chunk_size) {
                handles.push(s.spawn(move || {
                    for f in struct_def {
                        if f.generics.is_empty() {
                            resolve_all_types_struct_definition(
                                f,
                                globals_env,
                                &IndexMap::default(),
                                Some(&TypeExpr::Struct {
                                    name: f.name.clone(),
                                    type_params: Vec::new(),
                                }),
                                |_| {},
                            );
                        }
                    }
                }));
            }

            for h in handles.drain(..) {
                h.join().unwrap();
            }

            let chunk_size = (source_file.extensions_defs.len() / NUM_THREADS).max(1);
            for ext_def in source_file.extensions_defs.chunks_mut(chunk_size) {
                handles.push(s.spawn(move || {
                    for f in ext_def {
                        typeresolve_extensions_def(f, globals_env);
                    }
                }));
            }

            for h in handles.drain(..) {
                h.join().unwrap();
            }

            let chunk_size = (source_file.jsx_components.len() / NUM_THREADS).max(1);
            for jsx_component in source_file.jsx_components.chunks_mut(chunk_size) {
                handles.push(s.spawn(move || {
                    for f in jsx_component {
                        typeresolve_jsx_component(f, globals_env);
                    }
                }));
            }

            for h in handles.drain(..) {
                h.join().unwrap();
            }

            let chunk_size = (source_file.duckx_components.len() / NUM_THREADS).max(1);
            for duckx_component in source_file.duckx_components.chunks_mut(chunk_size) {
                handles.push(s.spawn(move || {
                    for f in duckx_component {
                        typeresolve_duckx_component(f, globals_env);
                    }
                }));
            }

            for h in handles.drain(..) {
                h.join().unwrap();
            }
        });
    }
    globals_env
}

fn typeresolve_duckx_component(c: &mut DuckxComponent, globals: &GlobalsEnv) {
    reduce_type_expr(&mut c.props_type, globals, None);
    let mut scope = LocalScoped::default();
    scope.set_info_for_ident(
        "props".to_string(),
        VariableInfo {
            typ: c.props_type.clone(),
            is_const: false,
        },
    );
    typeresolve_value_expr(&mut c.value_expr, globals, &mut scope);
}

fn typeresolve_extensions_def(extensions_def: &mut ExtensionsDef, globals: &GlobalsEnv) {
    let mut locals = LocalScoped::default();
    locals.push_scope();
    locals.set_info_for_ident(
        "self".to_string(),
        VariableInfo {
            typ: extensions_def.target_type_expr.clone(),
            is_const: false,
        },
    );

    let type_expr = extensions_def.target_type_expr.clone();
    for extension_method in &mut extensions_def.function_definitions {
        let extension_function_name = type_expr
            .0
            .build_extension_access_function_name2(&extension_method.0.name.clone());

        if globals
            .extension_functions
            .contains_sync(&extension_function_name)
        {
            continue;
        }

        for (_, p) in &mut extension_method.0.params {
            reduce_type_expr(p, globals, None);
            // TODO: keyof
        }

        reduce_type_expr(&mut extension_method.0.return_type, globals, None);
        // TODO: keyof

        let underlying_fn_type = extension_method.0.type_expr();

        let access_fn_type = TypeExpr::Fun(
            vec![(Some("self".to_string()), type_expr.clone())],
            Box::new(extension_method.0.type_expr()),
            // todo: mutable extension fns?
            false,
        );

        globals
            .extension_functions
            .insert_sync(
                extension_function_name,
                (underlying_fn_type.clone(), access_fn_type.clone()),
            )
            .unwrap();

        resolve_all_types_function_definition(
            &mut extension_method.0,
            globals,
            |_| {},
            &IndexMap::default(),
            &mut locals,
        );
    }
    locals.pop_scope();
}

fn typeresolve_function_call(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let span = value_expr.expr.1;
    let ValueExpr::FunctionCall {
        target,
        params,
        type_params,
    } = &mut value_expr.expr.0
    else {
        unreachable!("only pass function calls to this function")
    };
    unset_const_func_call_assign(target);

    if type_params.is_empty() {
        typeresolve_value_expr(target, globals, locals);
        let target_type = &target.typ;
        let TypeExpr::Fun(..) = &target_type.0 else {
            failure_with_occurence(
                "Can only call functions",
                span,
                [("Can only call functions", span)],
            )
        };
    } else {
        for t in type_params.iter_mut() {
            reduce_type_expr(t, globals, None);
        }

        match &mut target.expr.0 {
            ValueExpr::Variable(_, name, ty, _, _needs_copy) => {
                let x = globals.get_fun_header(name, type_params);
                if let Some(x) = x {
                    let fun_type = (x.to_type(), target.expr.1);
                    *ty = Some(fun_type.0.clone());
                    target.typ = fun_type;
                } else {
                    let msg = &format!("There is no function {} ({}))", name, type_params.len());
                    failure_with_occurence(msg, target.expr.1, [(msg, target.expr.1)]);
                }
            }
            ValueExpr::FieldAccess {
                target_obj,
                field_name,
            } => {
                typeresolve_value_expr(target_obj, globals, locals);

                if let TypeExpr::Struct {
                    name,
                    type_params: struct_type_params,
                } = &target_obj.typ.dereferenced().0
                {
                    let def = globals.get_method_header(
                        name,
                        struct_type_params,
                        field_name,
                        type_params,
                    );
                    if let Some(def) = def {
                        target.typ = (def.to_type(), target.expr.1);
                    } else {
                        let msg =
                            &format!("Method {} does not exist on struct {}", field_name, name);
                        failure_with_occurence(msg, span, [(msg, span)]);
                    }
                } else {
                    let msg = "Can only generic method call on structs";
                    failure_with_occurence(msg, span, [(msg, span)]);
                }
            }
            _ => {}
        }
    }

    let TypeExpr::Fun(header_params, ret, _) = &target.typ.0 else {
        unreachable!()
    };

    if params.len() != header_params.len() {
        failure_with_occurence(
            "Wrong number of parameters",
            value_expr.expr.1,
            [
                (
                    format!("This function takes {} parameters", header_params.len()),
                    target.expr.1,
                ),
                (
                    format!("You provided {} parameters", params.len()),
                    value_expr.expr.1,
                ),
            ],
        );
    }

    params
        .iter_mut()
        .zip(header_params.iter())
        .for_each(|(param_expr, (_, param_def))| {
            infer_against2(param_expr, param_def, globals);
            typeresolve_value_expr(param_expr, globals, locals);
            if matches!(param_def.0, TypeExpr::Any) {
                return;
            }
            check_type_compatability(param_def, &param_expr.typ, globals);
        });

    value_expr.typ = ret.as_ref().clone();
}

fn typeresolve_block(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::Block(value_exprs) = &mut value_expr.expr.0 else {
        unreachable!("only pass structs to this function")
    };

    locals.push_scope();

    value_exprs.iter_mut().for_each(|v| {
        typeresolve_value_expr(v, globals, locals);
        value_expr.typ.replace_if_other_never(&v.typ);
    });

    locals.pop_scope();

    value_expr.typ.replace_if_not_never(
        value_exprs
            .last()
            .map(|x| &x.typ)
            .unwrap_or(&TypeExpr::unit_with_span(value_expr.expr.1)),
    );
}

fn typeresolve_tuple(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::Tuple(value_exprs) = &mut value_expr.expr.0 else {
        unreachable!("only pass structs to this function")
    };

    let mut field_types = Vec::new();
    value_exprs.iter_mut().for_each(|v| {
        typeresolve_value_expr(v, globals, locals);
        value_expr.typ.replace_if_other_never(&v.typ);
        field_types.push(v.typ.clone());
    });
    value_expr
        .typ
        .replace_if_not_never(&(TypeExpr::Tuple(field_types), value_expr.expr.1));
}

fn typeresolve_while(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::While { condition, body } = &mut value_expr.expr.0 else {
        unreachable!("only pass structs to this function")
    };

    typeresolve_value_expr(condition, globals, locals);
    locals.push_scope();
    typeresolve_value_expr(body, globals, locals);
    locals.pop_scope();
    value_expr.typ = (TypeExpr::Statement, value_expr.expr.1);
}

fn typeresolve_if_expr(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::If {
        condition,
        then,
        r#else,
    } = &mut value_expr.expr.0
    else {
        unreachable!("only pass structs to this function")
    };

    typeresolve_value_expr(condition, globals, locals);
    locals.push_scope();
    typeresolve_value_expr(then, globals, locals);
    locals.pop_scope();
    if let Some(r#else) = r#else {
        locals.push_scope();
        typeresolve_value_expr(r#else, globals, locals);
        locals.pop_scope();
    }

    let condition_type_expr = &condition.typ;

    if condition_type_expr.0.is_never() {
        value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
    }

    if !condition.typ.0.is_bool() {
        let msg = "If condition needs to be bool";
        failure_with_occurence(msg, condition.expr.1, [(msg, condition.expr.1)]);
    }

    let then_type_expr = &then.typ;
    if let Some(r#else) = r#else {
        let else_type_expr = &r#else.typ;

        if !matches!(else_type_expr.0, TypeExpr::Statement) {
            if then_type_expr.0.is_never() && else_type_expr.0.is_never() {
                value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
                return;
            }

            let mut both = (
                TypeExpr::Or(vec![then_type_expr.clone(), else_type_expr.clone()]),
                value_expr.expr.1,
            );

            reduce_type_expr(&mut both, globals, None);

            value_expr.typ = both;
            return;
        }
    }

    value_expr
        .typ
        .replace_if_not_never(&(TypeExpr::Statement, value_expr.expr.1));
}

fn typeresolve_lambda(
    value_expr_param: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::Lambda(lambda_body) = &mut value_expr_param.expr.0 else {
        unreachable!("only pass structs to this function")
    };

    let LambdaFunctionExpr {
        is_mut,
        params,
        return_type,
        value_expr,
    } = lambda_body.as_mut();

    let captured = locals.idents.iter().flat_map(|v| v.iter()).fold(
        HashMap::new(),
        |mut acc, (name, VariableInfo { typ: ty, is_const })| {
            if !params.iter().any(|(param_name, _)| param_name == name)
            // don't capture shadowed variables
            {
                acc.insert(name.clone(), (ty.clone(), *is_mut && !*is_const));
            }
            acc
        },
    );

    locals.push_scope();

    for (name, (ty, capture_as_mut)) in captured {
        locals.set_info_for_ident(
            name,
            VariableInfo {
                typ: ty,
                is_const: !capture_as_mut,
            },
        );
    }

    for (name, ty) in params.iter_mut() {
        let ty = ty.as_mut().unwrap();
        reduce_type_expr(ty, globals, None);
        locals.set_info_for_ident(
            name.to_owned(),
            VariableInfo {
                typ: ty.clone(),
                is_const: false,
            },
        );
    }

    if let Some(return_type) = return_type.as_mut() {
        reduce_type_expr(return_type, globals, None);
    }

    if return_type.is_none() {
        *return_type = Some((TypeExpr::Tuple(vec![]), value_expr.expr.1));
    }

    typeresolve_value_expr(value_expr, globals, locals);
    locals.pop_scope();

    // check_returns(return_type.as_ref().unwrap(), value_expr, type_env);

    value_expr_param.typ.replace_if_not_never(&(
        TypeExpr::Fun(
            params
                .iter()
                .map(|x| {
                    (
                        None,
                        x.1.as_ref()
                            .cloned()
                            .expect("Compiler Bug: lambda parameter has no type"),
                    )
                })
                .collect(),
            return_type
                .as_ref()
                .cloned()
                .expect("Compiler Bug: lambda has no return type")
                .into(),
            *is_mut,
        ),
        value_expr_param.expr.1,
    ));
}

fn typeresolve_variable(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &LocalScoped,
) {
    let ValueExpr::Variable(_, identifier, type_expr_opt, const_opt, needs_copy) =
        &mut value_expr.expr.0
    else {
        unreachable!("only pass structs to this function")
    };
    let VariableInfo {
        typ: (type_expr, _),
        is_const,
    } = locals
        .get_info_for_ident(identifier)
        .cloned()
        .or_else(|| {
            globals
                .get_fun_header(identifier, &[])
                .map(|fun_header| VariableInfo {
                    typ: (fun_header.to_type(), value_expr.expr.1),
                    is_const: true,
                })
        })
        .unwrap_or_else(|| {
            failure_with_occurence(
                "Unknown identifier",
                value_expr.expr.1,
                [(
                    format!(
                        "The identifier {} is not found in the current scope",
                        identifier.yellow(),
                    ),
                    value_expr.expr.1,
                )],
            )
        });

    if *needs_copy && !type_expr.implements_copy2(globals) {
        failure_with_occurence(
            "This type is not trivially copyable",
            value_expr.expr.1,
            [(
                &format!(
                    "A type is trivially copyable if it's either a primitive, an immutable reference or a composition of primitive types {}",
                    type_expr
                ),
                value_expr.expr.1,
            )],
        )
    }

    *type_expr_opt = Some(type_expr.clone());
    *const_opt = Some(is_const);
    value_expr
        .typ
        .replace_if_not_never(&(type_expr.clone(), value_expr.expr.1));
}

pub fn typeresolve_duck_value_expr(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::Duck(items) = &mut value_expr.expr.0 else {
        unreachable!("only pass structs to this function")
    };

    let mut field_types = Vec::new();
    items.iter_mut().for_each(|(n, v)| {
        typeresolve_value_expr(v, globals, locals);
        value_expr.typ.replace_if_other_never(&v.typ);
        field_types.push(Field {
            name: n.clone(),
            type_expr: v.typ.clone(),
        });
    });
    value_expr.typ.replace_if_not_never(&(
        TypeExpr::Duck(Duck {
            fields: field_types,
        }),
        value_expr.expr.1,
    ));
}

fn typeresolve_var_decl(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let span = value_expr.expr.1;
    let ValueExpr::VarDecl(declaration) = &mut value_expr.expr.0 else {
        unreachable!("only pass var declarations to this function")
    };

    let declaration = &mut declaration.0;

    // Resolve the type expression on the declaration
    if let Some(type_expr) = &mut declaration.type_expr {
        reduce_type_expr(type_expr, globals, None);

        if let Some(initializer) = declaration.initializer.as_mut() {
            infer_against2(initializer, type_expr, globals);

            typeresolve_value_expr(initializer, globals, locals);
            check_type_compatability(type_expr, &initializer.typ, globals);
        }
    } else if let Some(initializer) = declaration.initializer.as_mut() {
        typeresolve_value_expr(initializer, globals, locals);

        let type_expr = &initializer.typ;
        declaration.type_expr = Some((type_expr.0.clone(), initializer.expr.1));
    }

    locals.set_info_for_ident(
        declaration.name.clone(),
        VariableInfo {
            typ: declaration.type_expr.clone().unwrap(),
            is_const: declaration.is_const,
        },
    );

    value_expr.typ = (TypeExpr::Statement, span);
}

fn typeresolve_var_assign(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let _span = value_expr.expr.1;
    let ValueExpr::VarAssign(assignment) = &mut value_expr.expr.0 else {
        unreachable!("only pass var assignments to this function")
    };

    typeresolve_value_expr(&mut assignment.0.target, globals, locals);

    unset_copy_var_assign(&mut assignment.0.target);
    let target_type = &assignment.0.target.typ;

    // if let ValueExpr::Variable(_, name, ..) = &assignment.0.target.expr.0 {
    //     let span = assignment.0.target.expr.1;
    //     if let Some(info) = locals.get_info_for_ident(name) {

    // if let Some(name) = is_base_const_var(&assignment.0.target, locals) {
    //     let span = assignment.0.target.expr.1;
    //     let msg = &format!("Can't assign to const variable {name}");
    //     failure_with_occurence(msg, span, [(msg, span)])
    // }

    //     } else {
    //         let msg = &format!("Variable {name} does not exist");
    //         failure_with_occurence(msg, span, [(msg, span)]);
    //     }
    // }

    infer_against2(&mut assignment.0.value_expr, &target_type, globals);

    typeresolve_value_expr(&mut assignment.0.value_expr, globals, locals);

    check_type_compatability(&target_type, &assignment.0.value_expr.typ, globals);

    value_expr.typ = (TypeExpr::Statement, value_expr.expr.1);
}

fn typeresolve_match(
    value_expr_param: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let ValueExpr::Match {
        value_expr,
        arms,
        else_arm,
        span,
    } = &mut value_expr_param.expr.0
    else {
        unreachable!("only pass match exprs to this function")
    };

    for arm in arms.iter_mut() {
        reduce_type_expr(&mut arm.type_case, globals, None);
    }

    let mut replacements = Vec::new();

    for (i, arm) in arms.clone().into_iter().enumerate() {
        let span = arm.type_case.1;
        let o = arm.type_case.clone();
        if let TypeExpr::Or(v) = arm.type_case.0 {
            let mut s = Vec::new();
            for v in v {
                s.push(MatchArm {
                    type_case: v,
                    base: Some(o.clone()),
                    condition: arm.condition.clone(),
                    identifier_binding: arm.identifier_binding.clone(),
                    span,
                    value_expr: arm.value_expr.clone(),
                })
            }
            replacements.push((i, s));
        }
    }

    for (i, replacements) in replacements.into_iter().rev() {
        arms.remove(i);
        for x in replacements.into_iter().rev() {
            arms.insert(i, x);
        }
    }

    typeresolve_value_expr(value_expr, globals, locals);

    let (match_var_type, mv_count, mv_is_mut) = value_expr.typ.derefenced_with_count_and_mut();
    let match_var_type = match_var_type.clone();

    arms.iter_mut().for_each(|arm| {
        locals.push_scope();
        if let Some(identifier) = &arm.identifier_binding {
            let tmp_t = if let Some(base_type) = arm.base.as_ref().cloned() {
                base_type
            } else {
                arm.type_case.clone()
            };
            locals.set_info_for_ident(
                identifier.clone(),
                VariableInfo {
                    typ: (
                        if mv_count >= 1 {
                            if mv_is_mut {
                                TypeExpr::RefMut(tmp_t.into())
                            } else {
                                TypeExpr::Ref(tmp_t.into())
                            }
                        } else {
                            tmp_t.0
                        },
                        arm.span, // TODO(@Apfelfrosch), replace this with identifier span
                    ),
                    is_const: false,
                },
            );
            if let Some(condition) = &mut arm.condition {
                typeresolve_value_expr(condition, globals, locals);
            }
        }
        typeresolve_value_expr(&mut arm.value_expr, globals, locals);
        locals.pop_scope();
    });

    let mut all = if let TypeExpr::Or(contents) = match_var_type.0 {
        contents
    } else {
        vec![match_var_type]
    };

    for c in arms.iter() {
        if c.condition.is_none() {
            let type_id = c.type_case.0.type_id2(globals);
            all.retain(|f| f.0.type_id2(globals) != type_id);
        }
    }

    let else_type = if all.is_empty() {
        (TypeExpr::Any, value_expr.expr.1)
    } else {
        let mut tmp = (TypeExpr::Or(all.clone()), value_expr.expr.1);
        reduce_type_expr(&mut tmp, globals, None);
        tmp
    };

    if let Some(arm) = else_arm {
        if !arm.type_case.0.is_never() {
            arm.type_case = else_type.clone();
        }
        locals.push_scope();
        if let Some(identifier) = &arm.identifier_binding {
            locals.set_info_for_ident(
                identifier.clone(),
                VariableInfo {
                    typ: (
                        if mv_count >= 1 {
                            if mv_is_mut {
                                TypeExpr::RefMut(else_type.into())
                            } else {
                                TypeExpr::Ref(else_type.into())
                            }
                        } else {
                            else_type.0
                        },
                        arm.span,
                    ),
                    is_const: false,
                },
            );
            if let Some(condition) = &mut arm.condition {
                typeresolve_value_expr(condition, globals, locals);
            }
        }
        typeresolve_value_expr(&mut arm.value_expr, globals, locals);
        locals.pop_scope();
    } else if !all.is_empty() {
        for c in arms.iter() {
            if c.type_case.0.type_id2(globals) == all[0].0.type_id2(globals)
                && c.condition.is_some()
            {
                failure_with_occurence(
                    "Unexhaustive Match",
                    *span,
                    vec![
                        (
                            format!(
                                "possible type {} not covered",
                                format!("{}", all[0].0).bright_yellow()
                            ),
                            *span,
                        ),
                        (
                            format!(
                                "This only covers {} partially and you're not providing an else",
                                format!("{}", all[0].0).bright_yellow()
                            ),
                            c.span,
                        ),
                    ],
                );
            }
        }

        failure_with_occurence(
            "Unexhaustive Match",
            *span,
            vec![(
                format!(
                    "possible type {} not covered",
                    format!("{}", all[0].0).bright_yellow()
                ),
                *span,
            )],
        );
    }

    let mut x = || {
        let v_expr_type = value_expr.typ.clone();
        if v_expr_type.0.is_never() {
            return (TypeExpr::Never, *span);
        }

        let mut arms = arms.clone();
        if let Some(arm) = else_arm {
            arms.push(arm.as_ref().clone());
        }

        let mut arm_types = Vec::new();
        for arm in &arms {
            let arm_type = arm.value_expr.typ.clone();

            let mut cloned_arm_type = arm_type.clone().0.into_empty_span();
            reduce_type_expr(&mut cloned_arm_type, globals, None);
            type_expr_into_empty_range(&mut cloned_arm_type);

            if !arm_types.iter().any(|t: &Spanned<TypeExpr>| {
                let mut cl1 = t.clone();
                type_expr_into_empty_range(&mut cl1);
                cl1.0.unconst() == cloned_arm_type.0.unconst()
            }) {
                arm_types.push((arm_type.0.unconst(), arm.value_expr.expr.1));
            }
        }

        // arm_types.retain(|f| !f.0.is_unit());

        if else_arm.is_none() {
            let possible_types: Vec<Spanned<TypeExpr>> =
                match value_expr.typ.dereferenced().0.clone() {
                    TypeExpr::Or(types) => types,
                    other => vec![(other, value_expr.expr.1)],
                };

            let mut covered_types = Vec::new();
            for arm in &arms {
                let case_type = &arm.type_case.0;

                let mut cloned_arm_type = case_type.clone().into_empty_span();
                type_expr_into_empty_range(&mut cloned_arm_type);
                if !covered_types.iter().any(|t: &Spanned<TypeExpr>| {
                    let mut cl1 = t.clone();
                    type_expr_into_empty_range(&mut cl1);
                    cl1.0 == cloned_arm_type.0
                }) {
                    covered_types.push((case_type.clone(), arm.type_case.1));
                }
            }

            possible_types.iter().for_each(|possible_type| {
                let mut b = possible_type.clone();
                type_expr_into_empty_range(&mut b);
                let is_covered = &covered_types.iter().any(|x| {
                    let mut a = x.clone();
                    type_expr_into_empty_range(&mut a);
                    a.0.unconst() == b.0.unconst()
                });
                if !is_covered {
                    let missing_type = possible_type;
                    failure_with_occurence(
                        "Unexhaustive Match",
                        *span,
                        vec![(
                            format!(
                                "possible type {} not covered",
                                format!("{}", missing_type.0).bright_yellow()
                            ),
                            *span,
                        )],
                    );
                }
            });
        }

        let was_empty_before = arm_types.is_empty();
        arm_types.retain(|t| !t.0.is_never());

        if arm_types.is_empty() && !was_empty_before {
            return (TypeExpr::Never, *span);
        }

        if arm_types.is_empty() {
            (TypeExpr::Tuple(vec![]), *span)
        } else {
            let mut a = (TypeExpr::Or(arm_types), *span);
            reduce_type_expr(&mut a, globals, None);
            a
        }
    };
    let x = x();
    value_expr_param.typ.replace_if_not_never(&x);
}

pub fn typeresolve_value_expr(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let span = value_expr.expr.1;
    match &mut value_expr.expr.0 {
        ValueExpr::Async(inner) => {
            typeresolve_value_expr(inner, globals, locals);
            let ValueExpr::FunctionCall {
                target,
                params,
                type_params: _,
            } = &inner.expr.0
            else {
                let msg = "Can only async call a function call".to_string();
                failure_with_occurence(msg.clone(), inner.expr.1, [(msg.clone(), inner.expr.1)]);
            };

            value_expr.typ.replace_if_not_never(&(
                if [target.as_ref()]
                    .into_iter()
                    .chain(params.iter())
                    .any(|v| v.typ.0.is_never())
                {
                    TypeExpr::Never
                } else {
                    TypeExpr::Struct {
                        name: mangle(&["std", "sync", "Channel"]),
                        type_params: vec![inner.typ.clone()],
                    }
                },
                span,
            ));

            let channel_type = inner.typ.clone();
            if let TypeExpr::Struct {
                ref name,
                ref type_params,
            } = channel_type.0
            {
                globals.get_struct_header(name, type_params);
            }
            let new_channel_fn_name = mangle(&["std", "sync", "Channel", "new"]);

            let fn_type = globals
                .function_headers
                .get(&new_channel_fn_name)
                .expect("new channel fn not found")
                .to_type();

            let mut new_channel_call = ValueExprWithType::n((
                ValueExpr::FunctionCall {
                    target: ValueExprWithType::n((
                        ValueExpr::Variable(
                            true,
                            new_channel_fn_name.clone(),
                            Some(fn_type),
                            Some(true),
                            false,
                        ),
                        span,
                    ))
                    .into(),
                    params: vec![],
                    type_params: vec![inner.typ.clone()],
                },
                span,
            ));

            typeresolve_function_call(&mut new_channel_call, globals, locals);

            let TypeExpr::Struct {
                name: chan_struct_name,
                type_params: chan_struct_type_params,
            } = &new_channel_call.typ.0
            else {
                panic!("Compiler Bug: Async doesn't return channel")
            };

            let channel_struct_def = globals
                .get_struct_header(&chan_struct_name, &chan_struct_type_params)
                .clone();

            // this loop ensures that all channel methods are emitted
            // TODO: STRUCT RESOLVE for cascading generics
            // for m in &channel_struct_def.methods {
            //     if !m.generics.is_empty() {
            //         continue;
            //     }

            //     typeresolve_value_expr(
            //         &mut ValueExprWithType::n((
            //             ValueExpr::FieldAccess {
            //                 target_obj: new_channel_call.clone().into(),
            //                 field_name: m.name.clone(),
            //             },
            //             span,
            //         )),
            //         globals,
            //         locals,
            //     );
            // }
        }
        ValueExpr::For {
            ident: (ident, is_const, ty),
            target,
            block,
        } => {
            typeresolve_value_expr(target, globals, locals);
            let mut target_type = target.typ.clone();

            let current = &mut target_type;
            let ref_type = 0;

            let fail = || {
                let msg = "Can only use for on std::col::Iterator".to_string();
                failure_with_occurence(msg.clone(), target.expr.1, [(msg.clone(), target.expr.1)])
            };
            if let TypeExpr::Struct { name, type_params } = current.clone().0 {
                let unmangled = unmangle(name.as_str());

                if unmangled.as_slice() != ["std", "col", "Iter"] || type_params.len() != 1 {
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
                            (current.clone().0, target.expr.1).into(),
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
                                    .into_empty_span(),
                                ),
                                value_expr: ValueExpr::Return(Some(
                                    ValueExpr::Tag("no_next_elem".to_string())
                                        .into_empty_span()
                                        .into(),
                                ))
                                .into_empty_span(),
                            }
                            .into(),
                        )
                        .into_empty_span(),
                    ],
                    type_params: vec![type_params[0].clone()],
                }
                .into_empty_span();

                typeresolve_value_expr(&mut new_iter_call_expr, globals, locals);

                let mut iter_def = globals.get_struct_header(&name, &type_params).clone();

                // this loop ensures that all channel methods are emitted
                // TODO CASCADING GENERICS
                // for m in iter_def.methods.iter_mut() {
                //     if !m.generics.is_empty() {
                //         continue;
                //     }

                //     typeresolve_value_expr(
                //         &mut ValueExprWithType::n((
                //             ValueExpr::FieldAccess {
                //                 target_obj: new_iter_call_expr.clone().into(),
                //                 field_name: m.name.clone(),
                //             },
                //             span,
                //         )),
                //         globals,
                //         locals,
                //     );
                // }

                let content_type = type_params[0].clone();

                if ref_type > 0 {
                    target_type.0 = if ref_type == 1 {
                        TypeExpr::Ref(content_type.clone().into())
                    } else {
                        TypeExpr::RefMut(content_type.clone().into())
                    };
                } else {
                    *current = content_type.clone();
                }
            } else {
                fail();
            }

            locals.push_scope();
            *ty = Some(target_type.clone().0);
            locals.set_info_for_ident(
                ident.clone(),
                VariableInfo {
                    typ: target_type,
                    is_const: *is_const,
                },
            );
            typeresolve_value_expr(block, globals, locals);
            locals.pop_scope();
            value_expr.typ = (TypeExpr::Statement, span);
        }
        ValueExpr::RawVariable(_, path) => {
            let ident = mangle(path);
            let VariableInfo { typ, is_const } =
                locals.get_info_for_ident(&ident).unwrap_or_else(|| {
                    failure_with_occurence(
                        "Unknown identifier",
                        span,
                        [(
                            format!(
                                "The identifier {} is not found in the current scope",
                                ident.red(),
                            ),
                            span,
                        )],
                    );
                });
            value_expr.expr.0 =
                ValueExpr::Variable(true, ident, Some(typ.0.clone()), Some(*is_const), true);
            value_expr.typ = (typ.0.clone(), span);
        }
        ValueExpr::Duck(..) => typeresolve_duck_value_expr(value_expr, globals, locals),
        ValueExpr::As(v, t) => {
            reduce_type_expr(t, globals, None);
            if let ValueExpr::InlineGo(_, ty) = &mut v.expr.0 {
                *ty = Some(t.clone());
            } else if let ValueExpr::Int(_, ty) = &mut v.expr.0 {
                *ty = Some(t.clone());
            } else if let ValueExpr::Float(float_value) = &mut v.expr.0 {
                match &t.0 {
                    TypeExpr::Int | TypeExpr::UInt => {
                        v.expr.0 = ValueExpr::Int(*float_value as u64, Some(t.clone()));
                    }
                    _ => {}
                }
            } else if let ValueExpr::Array(_, ty) = &mut v.expr.0
                && let TypeExpr::Array(ct) = &t.0
            {
                *ty = Some(ct.as_ref().clone());
            }

            typeresolve_value_expr(v, globals, locals);
            let v_type = &v.typ;

            value_expr.typ = if v_type.0.is_never() {
                (TypeExpr::Never, v_type.1)
            } else {
                check_type_compatability(t, v_type, globals);
                t.clone()
            };
        }
        ValueExpr::FieldAccess {
            target_obj,
            field_name,
        } => {
            typeresolve_value_expr(target_obj, globals, locals);
            let target_type = &target_obj.typ;

            if let TypeExpr::Struct { name, type_params } = &target_type.0 {
                globals
                    .generics_output
                    .resolve_all_methods(name, type_params, globals);
            }
            let target_obj_type_expr = &target_obj.typ;
            if !(target_obj_type_expr
                .dereferenced()
                .0
                .has_field_by_name2(field_name, globals)
                || target_obj_type_expr
                    .dereferenced()
                    .0
                    .has_method_by_name2(field_name, globals))
            // TODO: Extensions
            {
                // dbg!(&field_name, &target_obj_type_expr);
                failure_with_occurence(
                    "Invalid Field Access",
                    {
                        let mut span = span;
                        span.end += 2;
                        span
                    },
                    vec![(
                        format!(
                            "this is of type {} and it has no field '{}'",
                            target_obj_type_expr
                                .0
                                .as_clean_user_faced_type_name()
                                .bright_yellow(),
                            field_name.bright_blue()
                        ),
                        target_obj.expr.1,
                    )],
                )
            }

            let target_obj_type_expr = target_obj_type_expr;
            value_expr.typ.replace_if_not_never(&(
                target_obj_type_expr
                    .dereferenced()
                    .0
                    .typeof_field2(field_name.to_string(), globals)
                    .unwrap_or_else(|| panic!("Invalid field access {}", target_obj_type_expr.0)),
                value_expr.expr.1,
            ));
        }
        ValueExpr::Array(exprs, given_type) => {
            if let Some(given_type) = given_type.as_mut() {
                reduce_type_expr(given_type, globals, None);
            }

            let mut found_types = Vec::new();
            for expr in exprs {
                if let Some(given_type) = given_type.as_ref() {
                    infer_against2(expr, given_type, globals);
                }

                typeresolve_value_expr(expr, globals, locals);
                let ty = &expr.typ;

                if let Some(given_type) = given_type.as_ref() {
                    check_type_compatability(given_type, &ty, globals);
                }
                found_types.push(ty.clone());
            }

            if given_type.is_none() {
                if found_types.is_empty() {
                    let msg = "empty array must be wrapped in as expression";
                    failure_with_occurence(msg, span, [(msg, span)]);
                }
                let mut as_or = (TypeExpr::Or(found_types), span);
                reduce_type_expr(&mut as_or, globals, None);
                *given_type = Some(as_or);
            }
            value_expr.typ = (
                TypeExpr::Array(given_type.as_ref().cloned().unwrap().into()),
                span,
            );
        }
        ValueExpr::VarAssign(..) => typeresolve_var_assign(value_expr, globals, locals),
        ValueExpr::VarDecl(..) => typeresolve_var_decl(value_expr, globals, locals),
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    typeresolve_value_expr(e, globals, locals);
                    value_expr.typ.replace_if_other_never(&e.typ);
                }

                if let ValHtmlStringContents::String(s) = c {
                    // TODO: add tailwind
                    // type_env.check_for_tailwind(s);
                }
            }
            value_expr.typ.replace_if_not_never(&(TypeExpr::Html, span));
        }
        ValueExpr::Variable(..) => typeresolve_variable(value_expr, globals, locals),
        ValueExpr::Match { .. } => typeresolve_match(value_expr, globals, locals),
        ValueExpr::InlineGo(_, ty) => {
            if ty.is_none() {
                *ty = Some(TypeExpr::unit_with_span(span));
            }
            value_expr.typ = ty
                .as_ref()
                .cloned()
                .expect("Compiler Bug: Inline Go does not have type");
        }
        ValueExpr::Deref(v) => {
            typeresolve_value_expr(v, globals, locals);
            let t = &v.typ;
            if !t.0.implements_copy2(globals) {
                let msg = "The value of this type needs to implement Copy since it is dereferenced, but it does not";
                failure_with_occurence(msg, v.expr.1, [(msg, v.expr.1)]);
            }

            if let TypeExpr::Ref(v) | TypeExpr::RefMut(v) = &t.0 {
                value_expr.typ = v.as_ref().clone();
            } else {
                failure_with_occurence(
                    "Can only dereference a reference",
                    span,
                    [("This is not a reference".to_string(), v.expr.1)],
                );
            }
        }
        ValueExpr::FormattedString(contents) => {
            let mut t = (TypeExpr::String(None), span);
            for c in contents {
                match c {
                    ValFmtStringContents::Expr(e) => {
                        typeresolve_value_expr(e, globals, locals);
                        t.replace_if_other_never(&e.typ);
                        if !t.0.is_never() && !t.0.is_string() {
                            let hints = [
                                (
                                    "interpolated values inside a f-string must evaluate to a string".to_string(),
                                    e.expr.1,
                                ),
                                (
                                    format!(
                                        "this is of type {}{}",
                                        t.0.as_clean_user_faced_type_name().yellow(),
                                        if t.0.implements_to_string2(globals) {
                                            format!(
                                                ", which implements {}. Add the method-call after the value",
                                                "to_string".yellow(),
                                            )
                                        } else {
                                            String::new()
                                        }
                                    ),
                                    e.expr.1,
                                ),
                            ];

                            failure_with_occurence("Incompatible Types", e.expr.1, hints);
                        }
                    }
                    ValFmtStringContents::String(s) => {
                        // type_env.check_for_tailwind(s); // TODO: TAILWIND CHECK
                    }
                }
            }
            value_expr.typ = t;
        }
        ValueExpr::ArrayAccess(target, idx) => {
            typeresolve_value_expr(target, globals, locals);
            typeresolve_value_expr(idx, globals, locals);

            if target.typ.0.is_never() || idx.typ.0.is_never() {
                value_expr.typ = (TypeExpr::Never, span);
            } else {
                if !target.typ.0.is_array() && !target.typ.0.ref_is_array() {
                    let msg = &format!(
                        "Needs to be array, but is {}",
                        target.typ.0.as_clean_user_faced_type_name()
                    );
                    failure_with_occurence(msg, target.expr.1, [(msg, target.expr.1)]);
                }
                if !idx.typ.0.is_int() {
                    let msg = &format!(
                        "Array index needs to be int, but is {}",
                        idx.typ.0.as_clean_user_faced_type_name()
                    );
                    failure_with_occurence(msg, idx.expr.1, [(msg, idx.expr.1)]);
                }

                let target_type = target.typ.dereferenced();
                let TypeExpr::Array(array_content_type) = &target_type.0 else {
                    unreachable!("{:?}\n{:?}", &target.typ.0, target_type.0)
                };
                value_expr.typ = array_content_type.as_ref().clone();
            }
        }
        ValueExpr::RefMut(v) => {
            unset_copy_var_assign(v);
            typeresolve_value_expr(v, globals, locals);
            let v_type = &v.typ;
            value_expr.typ.replace_if_not_never(&(
                if v_type.0.is_never() {
                    TypeExpr::Never
                } else {
                    TypeExpr::RefMut(v_type.clone().into())
                },
                span,
            ));
        }
        ValueExpr::FunctionCall { .. } => typeresolve_function_call(value_expr, globals, locals),
        ValueExpr::Ref(v) => {
            unset_copy_var_assign(v);
            typeresolve_value_expr(v, globals, locals);
            let v_type = &v.typ;
            value_expr.typ.replace_if_not_never(&(
                if v_type.0.is_never() {
                    TypeExpr::Never
                } else {
                    TypeExpr::Ref(v_type.clone().into())
                },
                span,
            ));
        }
        ValueExpr::Struct { .. } => typeresolve_struct(value_expr, globals, locals),
        ValueExpr::Lambda(..) => typeresolve_lambda(value_expr, globals, locals),
        ValueExpr::If { .. } => typeresolve_if_expr(value_expr, globals, locals),
        ValueExpr::While { .. } => typeresolve_while(value_expr, globals, locals),
        ValueExpr::Tuple(..) => typeresolve_tuple(value_expr, globals, locals),
        ValueExpr::Block(..) => typeresolve_block(value_expr, globals, locals),
        ValueExpr::RawStruct { .. } => panic!("raw struct should not be here {value_expr:?}"),
        ValueExpr::BitAnd { lhs, rhs }
        | ValueExpr::BitOr { lhs, rhs }
        | ValueExpr::BitXor { lhs, rhs }
        | ValueExpr::ShiftLeft {
            target: lhs,
            amount: rhs,
        }
        | ValueExpr::ShiftRight {
            target: lhs,
            amount: rhs,
        } => {
            typeresolve_value_expr(lhs, globals, locals);
            typeresolve_value_expr(rhs, globals, locals);

            let lhs_type = &lhs.typ;
            let rhs_type = &rhs.typ;

            for t in [&lhs_type, &rhs_type] {
                match &t.0 {
                    TypeExpr::Int | TypeExpr::UInt => {}
                    _ => {
                        let msg = "Can only use bit operations on Int or UInt";
                        failure_with_occurence(msg, span, [(msg, lhs_type.1), (msg, rhs_type.1)]);
                    }
                }
            }

            value_expr.typ.replace_if_not_never(lhs_type);
        }
        ValueExpr::BitNot(inner) => {
            typeresolve_value_expr(inner, globals, locals);
            let t = &inner.typ;
            match &t.0 {
                TypeExpr::Int | TypeExpr::UInt => {}
                _ => {
                    let msg = "Can only use bit operations on Int or UInt";
                    failure_with_occurence(msg, span, [(msg, t.1)]);
                }
            }
            value_expr.typ.replace_if_not_never(t);
        }
        ValueExpr::Negate(v) => {
            typeresolve_value_expr(v, globals, locals);

            let ty = &v.typ;

            match ty.0 {
                TypeExpr::Int | TypeExpr::Float => {}
                TypeExpr::UInt => {
                    let msg = "Cannot negate unsigned ints since they have no negative";
                    failure_with_occurence(msg, v.expr.1, [(msg, v.expr.1)]);
                }
                _ => {
                    let msg = "Can only negate numbers that can have negative values (Int, Float)";
                    failure_with_occurence(msg, v.expr.1, [(msg, v.expr.1)]);
                }
            }
            value_expr.typ.replace_if_not_never(ty);
        }
        ValueExpr::Defer(inner) => {
            typeresolve_value_expr(inner, globals, locals);
            if !matches!(inner.expr.0, ValueExpr::FunctionCall { .. }) {
                let msg = "Can only defer a function call".to_string();
                failure_with_occurence(msg.clone(), span, [(msg.clone(), inner.expr.1)]);
            }
            value_expr.typ = (TypeExpr::Statement, span);
        }
        ValueExpr::Return(Some(v)) => {
            typeresolve_value_expr(v, globals, locals);
            value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
        }
        ValueExpr::BoolNegate(v) => {
            typeresolve_value_expr(v, globals, locals);
            // TODO(@Apfelfrosch): Check for bools
            value_expr.typ = (TypeExpr::Bool(None), value_expr.expr.1);
        }
        ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            typeresolve_value_expr(lhs, globals, locals);
            typeresolve_value_expr(rhs, globals, locals);
            // TODO(@Apfelfrosch): Check for bools
            value_expr.typ = (TypeExpr::Bool(None), value_expr.expr.1);
        }
        ValueExpr::Add(lhs, rhs)
        | ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Mul(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs) => {
            typeresolve_value_expr(lhs, globals, locals);
            typeresolve_value_expr(rhs, globals, locals);

            // TOOD(@Apfelfrosch): assert that lhs = rhs and that only ints, uints or floats are used

            value_expr.typ = (lhs.typ.0.clone(), value_expr.typ.1);
        }
        ValueExpr::String(..) => value_expr.typ = (TypeExpr::String(None), value_expr.expr.1),
        ValueExpr::Int(_, t) => {
            if let Some(t) = t.as_ref().as_ref()
                && !matches!(t.0, TypeExpr::Int | TypeExpr::UInt | TypeExpr::Byte)
            {
                let msg = "Int literal can only coerce to number type";
                failure_with_occurence(msg, t.1, [(msg, t.1), (msg, span)]);
            }

            if t.is_none() {
                *t = Some((TypeExpr::Int, span));
            }
            value_expr.typ = t.as_ref().cloned().unwrap();
        }
        ValueExpr::Bool(..) => {
            value_expr.typ = (TypeExpr::Bool(None), value_expr.expr.1);
        }
        ValueExpr::Tag(s) => {
            value_expr.typ = (TypeExpr::Tag(s.clone()), value_expr.expr.1);
        }
        ValueExpr::Char(..) => {
            value_expr.typ = (TypeExpr::Char, value_expr.expr.1);
        }
        ValueExpr::Float(..) => {
            value_expr.typ = (TypeExpr::Float, value_expr.expr.1);
        }
        ValueExpr::Break | ValueExpr::Return(None) | ValueExpr::Continue => {
            value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
        }
    }
}
