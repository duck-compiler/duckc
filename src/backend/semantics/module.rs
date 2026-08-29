use std::collections::HashMap;

use crate::{ast::{AstRoot, NodeId}, backend::semantics::{symbol::{ScopeId, SymbolId}, r#type::{TypeId, TypeParamId}}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Debug, Clone)]
pub struct FreeFunctionMember<'src> {
    pub name: &'src str,
    pub receiver_type: Option<TypeId>,
    pub value_type: TypeId,
}

pub struct ModuleTables<'src> {
    pub ast: AstRoot<'src>,
    pub resolutions: Vec<Option<SymbolId>>,
    pub definitions: Vec<Option<SymbolId>>,
    pub node_types: Vec<Option<TypeId>>,
    pub self_symbols: HashMap<NodeId, SymbolId>,
    pub type_arguments: HashMap<NodeId, Vec<TypeId>>,
    pub free_function_members: HashMap<NodeId, FreeFunctionMember<'src>>,
    pub method_type_params: HashMap<NodeId, Vec<TypeParamId>>,
    pub root_scope: ScopeId
}

impl<'src> ModuleTables<'src> {
    pub fn new(ast: AstRoot<'src>, n: usize, root_scope: ScopeId) -> ModuleTables<'src> {
        ModuleTables {
            ast,
            resolutions: vec![None; n],
            definitions: vec![None; n],
            node_types: vec![None; n],
            self_symbols: HashMap::new(),
            type_arguments: HashMap::new(),
            free_function_members: HashMap::new(),
            method_type_params: HashMap::new(),
            root_scope,
        }
    }
}
