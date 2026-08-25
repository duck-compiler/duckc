use crate::backend::semantics::symbol::SymbolId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Int,
    Int8,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint32,
    Uint64,
    Float,
    Float32,
    Bool,
    String,
    Array(TypeId),
    Pointer(TypeId),
    Struct(SymbolId),
    Fn {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    Never,
    TypeError,
}
