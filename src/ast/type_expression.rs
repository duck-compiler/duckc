use duckc_macros::ast_derive;

use crate::ast::{Identifier, Span};

#[ast_derive]
pub enum TypeExpression<'src> {
    #[serde(borrow)]
    Ident(Identifier<'src>),
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
}

#[ast_derive]
pub struct TypeAnnotation<'src> {
    #[serde(borrow)]
    pub annotation: Option<TypeExpression<'src>>,
    pub span: Span<'src>,
}
