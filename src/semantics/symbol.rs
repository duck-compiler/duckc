use std::collections::HashMap;

use crate::{ast::NodeId, semantics::{module::ModuleId, r#type::TypeId}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Param,
    Variable,
    Struct,
}

#[derive(Debug, Clone)]
pub enum Origin {
    Duck {
        module: ModuleId,
        declaration: NodeId,
    },
    Builtin,
}

#[derive(Debug, Clone)]
pub struct SymbolData<'src> {
    pub name: &'src str,
    pub kind: SymbolKind,
    pub type_: Option<TypeId>,
    pub origin: Origin,
}

pub struct Scope {
    pub parent: Option<ScopeId>,
    pub names: HashMap<String, SymbolId>,
}
