use std::collections::HashMap;

use crate::backend::semantics::{module::ModuleTables, symbol::{Scope, ScopeId, SymbolData, SymbolId}, r#type::{Type, TypeId}};

pub struct SemanticsContext<'src> {
    pub modules: Vec<ModuleTables<'src>>,
    pub symbols: Vec<SymbolData<'src>>,
    pub scopes: Vec<Scope>,
    pub types: Vec<Type>,
    type_dedup:  HashMap<Type, TypeId>
}

impl<'src> SemanticsContext<'src> {
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            symbols: Vec::new(),
            scopes: Vec::new(),
            types: Vec::new(),
            type_dedup: HashMap::new(),
        }
    }

    pub fn intern(&mut self, type_: Type) -> TypeId {
        if let Some(id) = self.type_dedup.get(&type_) {
            return *id;
        }

        let id = TypeId(self.types.len() as u32);
        self.types.push(type_.clone());
        self.type_dedup.insert(type_, id);

        id
    }

    pub fn add_symbol(&mut self, data: SymbolData<'src>) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(data);

        id
    }

    pub fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.scopes.len() as u32);
        self.scopes.push(Scope {
            parent,
            names: HashMap::new(),
        });

        id
    }

    pub fn define(&mut self, scope: ScopeId, name: &str, sym: SymbolId) {
        self.scopes[scope.0 as usize].names.insert(name.to_string(), sym);
    }

    pub fn lookup(&self, mut scope: ScopeId, name: &str) -> Option<SymbolId> {
        loop {
            let s = &self.scopes[scope.0 as usize];
            if let Some(sym) = s.names.get(name) {
                return Some(*sym)
            }

            scope = s.parent?;
        }
    }
}
