use duckc_macros::ast_derive;

use crate::ast::{Identifier, Span};

#[ast_derive]
pub enum TypeExpression<'src> {
    #[serde(borrow)]
    Ident(Identifier<'src>),
    Int,
    Float,
    Bool,
    String,
    Array {
        inner: Box<TypeExpression<'src>>,
    },
}

#[ast_derive]
pub struct TypeAnnotation<'src> {
    #[serde(borrow)]
    pub annotation: Option<TypeExpression<'src>>,
    pub span: Span<'src>,
}
