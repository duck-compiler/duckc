use duckc_macros::ast_derive;

use crate::ast::{Identifier, Span};

#[ast_derive]
pub struct TypeParam<'src> {
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub span: Span<'src>,
}

#[ast_derive]
pub enum TypeExpression<'src> {
    Ident {
        #[serde(borrow)]
        name: Identifier<'src>,
        type_args: Vec<TypeExpression<'src>>,
    },
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
    Array {
        inner: Box<TypeExpression<'src>>,
    },
    Pointer {
        inner: Box<TypeExpression<'src>>,
    },
    Tuple(Vec<TypeExpression<'src>>),
}

#[ast_derive]
pub struct TypeAnnotation<'src> {
    #[serde(borrow)]
    pub annotation: Option<TypeExpression<'src>>,
    pub span: Span<'src>,
}
