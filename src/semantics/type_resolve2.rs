use std::collections::{HashMap, HashSet};

const INITIAL_MAP_CAP: usize = 4096;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    parse::{
        Field, Spanned, failure_with_occurence,
        function_parser::{FunctionDefintion, LambdaFunctionExpr},
        generics_parser::Generic,
        source_file_parser::SourceFile,
        struct_parser::{DerivableInterface, StructDefinition},
        type_parser::{Duck, TypeExpr, merge_or},
        value_parser::{TypeParam, ValueExpr},
    },
    semantics::{
        type_resolve::{
            TravControlFlow, TypeEnv, build_struct_generic_id, infer_against, is_const_var, replace_generics_in_type_expr, trav_type_expr2, unset_const_func_call_assign, unset_copy_var_assign
        },
        typechecker::{check_type_compatability, check_type_compatability_full},
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
pub struct GlobalsEnv<'a> {
    pub type_aliases: HashMap<String, TypeAlias>,
    pub interfaces: HashMap<String, InterfaceDefinition>,
    pub structs: HashMap<String, StructDefinition>,
    pub functions: HashMap<String, FunctionDefintion>,
    pub struct_headers: HashMap<String, StructHeader>,
    pub function_headers: HashMap<String, FunHeader>,
    pub global_variables: HashMap<String, VariableInfo>,
    pub generics_output: &'a GenericsOutput,
    pub used_objects: scc::HashSet<String>,
}

impl<'a> GlobalsEnv<'a> {
    pub fn new(generics: &'a GenericsOutput) -> GlobalsEnv<'a> {
        Self {
            used_objects: scc::HashSet::with_capacity(INITIAL_MAP_CAP),
            type_aliases: HashMap::with_capacity(INITIAL_MAP_CAP),
            interfaces: HashMap::with_capacity(INITIAL_MAP_CAP),
            structs: HashMap::with_capacity(256),
            struct_headers: HashMap::with_capacity(256),
            functions: HashMap::with_capacity(INITIAL_MAP_CAP),
            function_headers: HashMap::with_capacity(INITIAL_MAP_CAP),
            global_variables: HashMap::with_capacity(INITIAL_MAP_CAP),
            generics_output: generics,
        }
    }

    pub fn get_fun_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
        globals: &GlobalsEnv,
    ) -> Option<FunHeader> {
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
                        .function_create_generic_instance(s, globals, params);
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
        globals: &GlobalsEnv,
    ) -> Option<MethodHeader> {
        self.get_struct_header(struct_name, struct_params, globals)
            .map(|s| {
                if params.is_empty() {
                    s.methods.get(name).cloned()
                } else {
                    self.generics_output
                        .get_generic_method_header(struct_name, struct_params, name, params)
                        .or_else(|| {
                            self.generics_output.struct_create_generic_instance(
                                self.structs.get(struct_name).unwrap(),
                                globals,
                                struct_params,
                            );
                            let s = self.generics_output.generic_structs.read_sync(
                                &build_struct_generic_id(
                                    struct_name,
                                    struct_params,
                                    &mut TypeEnv::default(),
                                ),
                                |_, v| v.clone(),
                            );

                            s.map(|s| {
                                self.generics_output
                                    .method_create_generic_instance(&s, name, params, globals);
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
        globals: &GlobalsEnv,
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
                        .struct_create_generic_instance(s, globals, params);
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
}

impl GenericsOutput {
    pub fn get_generic_function_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<FunHeader> {
        self.generic_function_headers.read_sync(
            &build_struct_generic_id(name, params, &mut TypeEnv::default()),
            |_, v| v.clone(),
        )
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
                &build_struct_generic_id(
                    &build_struct_generic_id(struct_name, struct_params, &mut TypeEnv::default()),
                    params,
                    &mut TypeEnv::default(),
                ),
                |_, v| {
                    v.read_sync(
                        &build_struct_generic_id(name, params, &mut TypeEnv::default()),
                        |_, v| v.clone(),
                    )
                },
            )
            .flatten()
    }

    pub fn get_generic_struct_header(
        &self,
        name: &str,
        params: &[Spanned<TypeExpr>],
    ) -> Option<StructHeader> {
        self.generic_struct_headers.read_sync(
            &build_struct_generic_id(name, params, &mut TypeEnv::default()),
            |_, v| v.clone(),
        )
    }

    pub fn method_create_generic_instance(
        &self,
        s: &StructDefinition,
        method_name: &str,
        params: &[Spanned<TypeParam>],
        globals: &GlobalsEnv,
    ) {
        let id = build_struct_generic_id(&method_name, params, &mut TypeEnv::default());

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
        let is_mut = s.mut_methods.contains(&base_def.name);

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
        let id = build_struct_generic_id(&base_def.name, params, &mut TypeEnv::default());
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
        let id = build_struct_generic_id(&base_def.name, params, &mut TypeEnv::default());
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

impl Default for GenericsOutput {
    fn default() -> Self {
        Self {
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
fn reduce_type_expr(t: &mut Spanned<TypeExpr>, globals: &GlobalsEnv, self_type: Option<&TypeExpr>) {
    trav_type_expr2(
        |t, _| match &mut t.0 {
            TypeExpr::TypeName(..) => {
                let mut current = t.0.clone();

                while let TypeExpr::TypeName(_, ident, params) = &current {
                    if ident == "Self"
                        && let Some(ty) = self_type.clone()
                        && params.is_empty()
                    {
                        current = ty.clone();
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

                        let mut rhs = alias.expr.0.clone();

                        replace_generics_in_type_expr(
                            &mut rhs,
                            &generic_params,
                            &mut TypeEnv::default(),
                        );
                        current = rhs;
                        continue;
                    }

                    if let Some(struct_def) = globals.structs.get(ident) {
                        if params.len() != struct_def.generics.len() {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, t.1, [(msg, t.1)]);
                        }

                        current = TypeExpr::Struct {
                            name: ident.clone(),
                            type_params: params.clone(),
                        };
                        continue;
                    }

                    if let Some(interface_def) = globals.interfaces.get(ident) {
                        if params.len() != interface_def.generics.len() {
                            let msg = "Wrong number of type parameters";
                            failure_with_occurence(msg, t.1, [(msg, t.1)]);
                        }

                        current = TypeExpr::NamedDuck {
                            name: ident.clone(),
                            type_params: params.clone(),
                        };
                        continue;
                    }
                }
                t.0 = current;
                return TravControlFlow::Cancel;
            }
            TypeExpr::And(operands) => {
                let mut out_duck = IndexMap::<String, Spanned<TypeExpr>>::new();
                for operand in operands {
                    reduce_type_expr(operand, globals, self_type.clone());
                    if let TypeExpr::Duck(Duck { fields }) = &operand.0 {
                        for field in fields {
                            if let Some(ty) = out_duck.get(&field.name) {
                                if ty.0.type_id(&mut TypeEnv::default())
                                    != field.type_expr.0.type_id(&mut TypeEnv::default())
                                {
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
                out_operands.sort_by_key(|t| t.0.type_id(&mut TypeEnv::default()));
                out_operands.dedup_by_key(|t| t.0.type_id(&mut TypeEnv::default()));
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
        replace_generics_in_type_expr(&mut field.type_expr.0, generics, &mut TypeEnv::default());
        reduce_type_expr(&mut field.type_expr, globals, self_type.clone());
    }

    for method in struct_def.methods.iter_mut() {
        for param in method.params.iter_mut() {
            replace_generics_in_type_expr(&mut param.1.0, generics, &mut TypeEnv::default());
            reduce_type_expr(&mut param.1, globals, self_type.clone());
        }
        replace_generics_in_type_expr(&mut method.return_type.0, generics, &mut TypeEnv::default());
    }

    header_callback(struct_def.to_header());

    for method in struct_def.methods.iter_mut() {
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
        typeresolve_value_expr(&mut method.value_expr, globals, &mut local_scope);
    }
}

fn resolve_all_types_function_definition<F>(
    fun_def: &mut FunctionDefintion,
    globals: &GlobalsEnv,
    header_callback: F,
    generics: &IndexMap<String, TypeExpr>,
) where
    F: FnOnce(FunHeader),
{
    let mut local_scope = LocalScoped::default();

    for p in fun_def.params.iter_mut() {
        replace_generics_in_type_expr(&mut p.1.0, generics, &mut TypeEnv::default());
        reduce_type_expr(&mut p.1, globals, None);
        local_scope.set_info_for_ident(
            p.0.clone(),
            VariableInfo {
                is_const: false,
                typ: p.1.clone(),
            },
        );
    }

    replace_generics_in_type_expr(
        &mut fun_def.return_type.0,
        generics,
        &mut TypeEnv::default(),
    );
    reduce_type_expr(&mut fun_def.return_type, globals, None);

    header_callback(fun_def.to_header2());

    typeresolve_value_expr(&mut fun_def.value_expr, globals, &mut local_scope);
}

fn resolve_all_types_source_file(source_file: &mut SourceFile) {
    const NUM_THREADS: usize = 8;
    let generics_output = GenericsOutput::default();
    let globals_env = std::thread::scope(|s| {
        let mut globals_env = GlobalsEnv::new(&generics_output);

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

        globals_env
    });

    let chunk_count = (source_file.function_definitions.len() / NUM_THREADS).max(1);
    let globals_env = &globals_env;
    std::thread::scope(move |s| {
        for fun_def in source_file.function_definitions.chunks_mut(chunk_count) {
            s.spawn(move || {
                for f in fun_def {
                    resolve_all_types_function_definition(
                        f,
                        globals_env,
                        |_| {},
                        &IndexMap::default(),
                    );
                }
            });
        }
    });
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
                let x = globals.get_fun_header(name, type_params, globals);
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
                } = &target_obj.typ.0
                {
                    let def = globals.get_method_header(
                        name,
                        struct_type_params,
                        field_name,
                        type_params,
                        globals,
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
            infer_against(param_expr, param_def, &mut TypeEnv::default());
            typeresolve_value_expr(param_expr, globals, locals);
            if matches!(param_def.0, TypeExpr::Any) {
                return;
            }
            check_type_compatability_full(
                param_def,
                &param_expr.typ,
                &mut TypeEnv::default(),
                is_const_var(&param_expr.expr.0),
            );
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

fn typeresolve_struct(value_expr: &mut ValueExprWithType, globals: &GlobalsEnv, locals: &mut LocalScoped) {
    let span = value_expr.expr.1;
    let ValueExpr::Struct {
        name,
        fields,
        type_params,
    } = &mut value_expr.expr.0
    else {
        unreachable!("only pass structs to this function")
    };

    let og_def = globals.get_struct_header(name, type_params);

    if fields.len() != og_def.fields.len() {
        let msg = "Amount of fields doesn't match.";

        let mut hints = vec![format!(
            "{} has {} fields. You provided {} fields",
            og_def.name,
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
                    .map(|og_field| &og_field.name)
                    .any(|field_name| *field_name == *field)
            })
            .collect::<Vec<String>>();

        let missing_fields = og_def
            .fields
            .iter()
            .map(|field| field.name.clone())
            .filter(|field| !fields.iter().any(|given_field| given_field.0 == *field))
            .collect::<Vec<String>>();

        if !fields_that_are_too_much.is_empty() {
            hints.push(format!(
                "The field(s) {} do not exist on type {}",
                fields_that_are_too_much.join(", ").yellow(),
                og_def.name.yellow(),
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
            .any(|og_field| og_field.name.as_str() == f.0)
        {
            let msg = format!("Invalid field {}", f.0);
            failure_with_occurence(
                msg,
                span,
                [(
                    format!(
                        "{} doesn't have a field named {}. It has fields {}",
                        og_def.name,
                        f.0,
                        og_def
                            .fields
                            .iter()
                            .map(|f| f.name.clone())
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
                        if f2.name.as_str() == f.0.as_str() {
                            Some(&f2.type_expr)
                        } else {
                            None
                        }
                    })
                    .unwrap(),
            )
        }) {
            typeresolve_value_expr(p, type_env);
            infer_type_params(t, &p.typ, &mut infered_generics, type_env);
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
        typeresolve_struct(value_expr, type_env);
        return;
    }

    if type_params.len() != og_def.generics.len() {
        let msg = "Wrong number of type parameters A";
        failure_with_occurence(msg, span, [(msg, span)])
    }

    for t in type_params.iter_mut() {
        resolve_all_aliases_type_expr(t, type_env);
    }

    let def = type_env
        .get_struct_def_with_type_params_mut(name.as_str(), type_params.as_slice(), span)
        .clone();

    for f in fields.iter_mut() {
        let og_field = def
            .fields
            .iter()
            .find(|og_field| og_field.name.as_str() == f.0.as_str())
            .cloned()
            .unwrap()
            .type_expr;
        infer_against(&mut f.1, &og_field, type_env);
        typeresolve_value_expr(&mut f.1, type_env);
        check_type_compatability(&og_field, &f.1.typ, type_env);
        if f.1.typ.0.is_never() {
            value_expr.typ = (TypeExpr::Never, value_expr.expr.1);
        }
    }
    value_expr.typ = (
        TypeExpr::Struct {
            name: name.clone(),
            type_params: type_params.clone(),
        },
        value_expr.expr.1,
    );
}

fn typeresolve_value_expr(
    value_expr: &mut ValueExprWithType,
    globals: &GlobalsEnv,
    locals: &mut LocalScoped,
) {
    let span = value_expr.expr.1;
    match &mut value_expr.expr.0 {
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
        _ => todo!(),
    }
}
