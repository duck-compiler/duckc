use std::collections::HashMap;

use crate::{ast::NodeId, backend::semantics::{module::ModuleId, r#type::TypeId}};

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
    Module,
}

#[derive(Debug, Clone)]
pub enum Origin<'src> {
    Duck {
        module: ModuleId,
        declaration: NodeId,
    },
    GoPackage {
        path: &'src str,
    },
    GoType {
        package: &'src str,
    },
}

#[derive(Debug, Clone)]
pub struct SymbolData<'src> {
    pub name: &'src str,
    pub kind: SymbolKind,
    pub type_: Option<TypeId>,
    pub origin: Origin<'src>,
}

pub struct Scope {
    pub parent: Option<ScopeId>,
    pub names: HashMap<String, SymbolId>,
}
