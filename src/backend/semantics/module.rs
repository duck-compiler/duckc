use crate::{ast::AstRoot, backend::semantics::{symbol::{ScopeId, SymbolId}, r#type::TypeId}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

pub struct ModuleTables<'src> {
    pub ast: AstRoot<'src>,
    pub resolutions: Vec<Option<SymbolId>>,
    pub definitions: Vec<Option<SymbolId>>,
    pub node_types: Vec<Option<TypeId>>,
    pub root_scope: ScopeId
}

impl<'src> ModuleTables<'src> {
    pub fn new(ast: AstRoot<'src>, n: usize, root_scope: ScopeId) -> ModuleTables<'src> {
        ModuleTables {
            ast,
            resolutions: vec![None; n],
            definitions: vec![None; n],
            node_types: vec![None; n],
            root_scope,
        }
    }
}
