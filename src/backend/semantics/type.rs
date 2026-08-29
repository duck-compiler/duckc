use crate::backend::semantics::symbol::SymbolId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeParamId(pub u32);

#[derive(Debug, Clone)]
pub struct TypeParamData<'src> {
    pub name: &'src str,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float,
    Float32,
    Bool,
    String,
    Array(TypeId),
    Pointer(TypeId),
    Tuple(Vec<TypeId>),
    Struct(SymbolId, Vec<TypeId>),
    TypeParam(TypeParamId),
    Fn {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    Never,
    TypeError,
}
