use crate::ast::{Identifier, MemoryTarget, Span};

use duckc_macros::ast_derive;
use serde::{Deserialize, Serialize};

#[ast_derive]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[ast_derive]
pub enum UnaryOperator {
    /// !
    Bang,
    /// -
    Neg,
}

#[ast_derive]
pub enum Expr<'src> {
    StringLiteral(&'src str),
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    Binary {
        left: Box<Expression<'src>>,
        op: BinaryOperator,
        right: Box<Expression<'src>>,
    },
    Unary {
        op: UnaryOperator,
        expr: Box<Expression<'src>>,
    },
    MemoryTarget(MemoryTarget<'src>),
}

#[ast_derive]
pub struct Expression<'src> {
    #[serde(borrow)]
    pub variant: Expr<'src>,
    pub span: Span<'src>,
}
