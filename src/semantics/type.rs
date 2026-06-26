use crate::semantics::symbol::SymbolId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

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
