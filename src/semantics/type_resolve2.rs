use std::collections::HashMap;

const INITIAL_MAP_CAP: usize = 4096;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    parse::{
        Spanned, failure_with_occurence, function_parser::FunctionDefintion,
        generics_parser::Generic, source_file_parser::SourceFile, struct_parser::StructDefinition,
        type_parser::TypeExpr, value_parser::ValueExpr,
    },
    semantics::typechecker::check_type_compatability,
};

#[derive(Debug, Clone)]
pub struct FunHeader {
    pub generics: Vec<Generic>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct MethodHeader {
    pub is_mut: bool,
    pub generics: Vec<Spanned<Generic>>,
    pub params: Vec<Spanned<TypeExpr>>,
    pub return_type: Spanned<TypeExpr>,
}

#[derive(Debug, Clone)]
pub struct StructHeader {
    pub fields: HashMap<String, Spanned<TypeExpr>>,
    pub methods: HashMap<String, MethodHeader>,
    pub generics: Vec<Spanned<Generic>>,
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
    pub structs: HashMap<String, StructHeader>,
    pub functions: HashMap<String, FunHeader>,
    pub global_variables: HashMap<String, VariableInfo>,
    pub generics_output: &'a GenericsOutput,
}

impl<'a> GlobalsEnv<'a> {
    pub fn resolve_type_expr(&self, t: &mut Spanned<TypeExpr>) {
        let mut res = t.clone();

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

            if let Some(s_def) = self.interfaces.get(name)
                && !matches!(res.0, TypeExpr::NamedDuck { .. })
            {
                res.0 = TypeExpr::NamedDuck {
                    name: name.clone(),
                    type_params: generics.to_vec(),
                };
                continue;
            }

            if let Some(s_def) = self.structs.get(name)
                && !matches!(res.0, TypeExpr::Struct { .. })
            {
                res.0 = TypeExpr::Struct {
                    name: name.clone(),
                    type_params: generics.to_vec(),
                };
                continue;
            }

            if let Some(simple_resolved) = self.type_aliases.get(name)
                && generics.is_empty()
                && &res.0 != &simple_resolved.expr.0
            {
                res = simple_resolved.expr.clone();
                continue;
            }

            if let Some(def) = self
                .type_aliases
                .iter()
                .find(|(k, d)| k.as_str() == name.as_str())
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
                            check_type_compatability(&constraint, &exp);
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

        t.0 = res.0;
    }

    pub fn new(generics: &'a GenericsOutput) -> GlobalsEnv<'a> {
        Self {
            type_aliases: HashMap::with_capacity(INITIAL_MAP_CAP),
            interfaces: HashMap::with_capacity(INITIAL_MAP_CAP),
            structs: HashMap::with_capacity(256),
            functions: HashMap::with_capacity(INITIAL_MAP_CAP),
            global_variables: HashMap::with_capacity(INITIAL_MAP_CAP),
            generics_output: generics,
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
        self.idents
            .last()
            .expect("Compiler Bug: no scope")
            .get(name)
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

// pub fn resolve_all_aliases_type_expr(expr: &mut Spanned<TypeExpr>, globals: &GlobalsEnv) {
//     let span = expr.1;
//     trav_type_expr(
//         |node, env| match &mut node.0 {
//             TypeExpr::TypeOf(identifier) => {
//                 let type_expr = env.get_identifier_type_in_typeof(identifier);
//                 *node = resolve_type_expr(
//                     &(type_expr.expect("couldn't find identifier type"), span),
//                     env,
//                 );
//             }
//             TypeExpr::KeyOf(type_expr) => {
//                 let type_expr = type_expr.as_mut();
//                 *type_expr = resolve_type_expr(type_expr, env);
//             }
//             TypeExpr::TypeName(..) => {
//                 *node = resolve_type_expr(node, env);
//             }
//             TypeExpr::RawTypeName(_, v, _) => {
//                 assert_eq!(1, v.len(), "should be mangled");
//                 *node = resolve_type_expr(node, env);
//             }
//             _ => {}
//         },
//         expr,
//         env,
//     );

//     resolve_all_indexed_type_expr(expr, env);
//     resolve_all_intersection_type_expr(expr, env);
//     merge_all_or_type_expr(expr, env);
// }

// pub fn resolve_types_value_expr(
//     v: &mut ValueExprWithType,
//     global_headers: &GlobalsEnv,
//     scoped: &mut LocalScoped,
// ) {
//     match &mut v.expr.0 {
//         ValueExpr::VarDecl(decl) => {
//             let decl = &mut decl.as_mut().0;

//             if let Some(type_expr) = &mut decl.type_expr {
//                 resolve_all_aliases_type_expr(type_expr, type_env);
//                 process_keyof_in_type_expr(&mut type_expr.0, type_env);

//                 if let Some(initializer) = declaration.initializer.as_mut() {
//                     infer_against(initializer, type_expr, type_env);

//                     typeresolve_value_expr(initializer, type_env);

//                     check_type_compatability(type_expr, &initializer.typ, type_env);
//                 }
//             } else if let Some(initializer) = declaration.initializer.as_mut() {
//                 typeresolve_value_expr(initializer, type_env);

//                 let type_expr = &initializer.typ;
//                 declaration.type_expr = Some((type_expr.0.clone(), initializer.expr.1));
//             }

//             scoped.set_info_for_ident(
//                 decl.name.clone(),
//                 VariableInfo {
//                     typ: decl.type_expr.as_ref().cloned().unwrap(),
//                     is_const: decl.is_const,
//                 },
//             );
//             v.typ = (TypeExpr::Statement, v.expr.1);
//         }
//         _ => {}
//     }
// }

fn resolve_all_types_source_file(source_file: &SourceFile) {
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
                    res.insert(f.name.clone(), f.to_header2());
                }
                res
            }));
        }

        globals_env.functions = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.function_definitions.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

        let mut handles = Vec::with_capacity(NUM_THREADS);

        for struct_defs in source_file
            .struct_definitions
            .chunks((source_file.struct_definitions.len() / NUM_THREADS).max(1))
        {
            handles.push(s.spawn(move || {
                let mut res = HashMap::with_capacity(struct_defs.len());
                for s in struct_defs {
                    res.insert(s.name.clone(), s.to_header());
                }
                res
            }));
        }

        globals_env.structs = handles
            .into_iter()
            .map(|j| j.join().expect("Compiler Bug: Handles "))
            .fold(
                HashMap::with_capacity(source_file.struct_definitions.len()),
                |mut acc, elem| {
                    for (k, v) in elem.into_iter() {
                        acc.insert(k, v);
                    }
                    acc
                },
            );

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
}
