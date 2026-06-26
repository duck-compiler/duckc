use crate::type_resolve::type_env::{SymbolId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Int,
    Float,
    Bool,
    String,
    Array(TypeId),
    Struct(SymbolId),
    Fn {
        params: Vec<TypeId>,
        return_type: TypeId,
    }
}
