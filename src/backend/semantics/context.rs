use std::collections::HashMap;
use std::rc::Rc;

use bumpalo::Bump;

use crate::ast::struct_definition::{MethodKind, Visibility};
use crate::ast::{AstRoot, assign_generate_node_ids};
use crate::backend::semantics::{diagnostic::Diagnostic, go_resolve::GoResolver, module::{ModuleId, ModuleTables}, symbol::{Scope, ScopeId, SymbolData, SymbolId}, r#type::{Type, TypeId, TypeParamData, TypeParamId}};

#[derive(Debug, Clone)]
pub struct MethodSignature {
    pub kind: MethodKind,
    pub visibility: Visibility,
    pub value_type: TypeId,
    pub type_params: Rc<[TypeParamId]>,
}

impl MethodSignature {
    pub fn is_free_function(&self) -> bool {
        matches!(self.kind, MethodKind::Static) || !self.type_params.is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FreeFunctionMethodKey<'src> {
    pub struct_symbol: SymbolId,
    pub method_name: &'src str,
}

pub struct SemanticsContext<'src> {
    arena: &'src Bump,
    pub modules: Vec<ModuleTables<'src>>,
    pub symbols: Vec<SymbolData<'src>>,
    pub scopes: Vec<Scope>,
    pub types: Vec<Type>,
    type_dedup:  HashMap<Type, TypeId>,
    pub diagnostics: Vec<Diagnostic<'src>>,
    pub go_resolver: GoResolver,
    pub go_struct_symbols: HashMap<String, SymbolId>,
    pub go_package_names: HashMap<String, &'src str>,
    pub struct_fields: HashMap<SymbolId, Vec<(&'src str, TypeId, Visibility)>>,
    pub struct_methods: HashMap<SymbolId, HashMap<&'src str, MethodSignature>>,
    pub type_param_data: Vec<TypeParamData<'src>>,
    pub symbol_type_params: HashMap<SymbolId, Vec<TypeParamId>>,
    pub mangled_free_function_names: HashMap<FreeFunctionMethodKey<'src>, &'src str>,
}

impl<'src> SemanticsContext<'src> {
    pub fn new(arena: &'src Bump) -> Self {
        Self {
            arena,
            modules: Vec::new(),
            symbols: Vec::new(),
            scopes: Vec::new(),
            types: Vec::new(),
            type_dedup: HashMap::new(),
            diagnostics: Vec::new(),
            go_resolver: GoResolver::new(),
            go_struct_symbols: HashMap::new(),
            go_package_names: HashMap::new(),
            struct_fields: HashMap::new(),
            struct_methods: HashMap::new(),
            type_param_data: Vec::new(),
            symbol_type_params: HashMap::new(),
            mangled_free_function_names: HashMap::new(),
        }
    }

    pub fn alloc_str(&self, s: &str) -> &'src str {
        self.arena.alloc_str(s)
    }

    pub fn free_function_method_name(&self, struct_name: &str, method_name: &str) -> &'src str {
        self.alloc_str(&format!("{struct_name}_{method_name}"))
    }

    pub fn add_type_param(&mut self, name: &'src str) -> TypeParamId {
        let id = TypeParamId(self.type_param_data.len() as u32);
        self.type_param_data.push(TypeParamData { name });

        id
    }

    pub fn type_param_name(&self, type_param: TypeParamId) -> &'src str {
        self.type_param_data[type_param.0 as usize].name
    }

    pub fn type_params_of(&self, symbol: SymbolId) -> &[TypeParamId] {
        self.symbol_type_params.get(&symbol).map(Vec::as_slice).unwrap_or_default()
    }

    pub fn add_module(&mut self, mut ast: AstRoot<'src>) -> ModuleId {
        let node_count = assign_generate_node_ids(&mut ast);
        let root_scope = self.new_scope(None);

        let id = ModuleId(self.modules.len() as u32);
        self.modules.push(ModuleTables::new(ast, node_count, root_scope));

        id
    }

    pub fn report(&mut self, diagnostic: Diagnostic<'src>) {
        self.diagnostics.push(diagnostic);
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
