use std::collections::HashMap;

use crate::{ast::{AstRoot, NodeId}, type_resolve::types::Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

pub struct IdGen {
    next_id: u32,
}

impl IdGen {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

   pub fn fresh(&mut self) -> NodeId {
       let id = NodeId(self.next_id);
       self.next_id += 1;

       id
   }

   pub fn count(&self) -> usize {
       self.next_id as usize
   }
}

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

pub struct TypeEnv<'src> {
    pub modules: Vec<ModuleTables<'src>>,
    pub symbols: Vec<SymbolData<'src>>,
    pub scopes: Vec<Scope>,
    pub types: Vec<Type>,
    type_dedup:  HashMap<Type, TypeId>
}

impl<'src> TypeEnv<'src> {
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
