use duckc_macros::ast_derive;
use serde::{Deserialize, Serialize};

use crate::ast::{Identifier, Span};

#[ast_derive]
pub enum TyExpr<'src> {
    #[serde(borrow)]
    Ident(Identifier<'src>),
    Int,
    Float,
    Bool,
    Array {
        inner: Box<TyExpr<'src>>,
    },
}

#[ast_derive]
pub struct TypeExpression<'src> {
    #[serde(borrow)]
    type_expr: TyExpr<'src>,
    span: Span<'src>,
}
