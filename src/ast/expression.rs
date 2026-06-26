use crate::ast::{Block, Identifier, MemoryTarget, NodeId, Span};
use duckc_macros::ast_derive;

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
    FunctionCall {
        #[serde(borrow)]
        name: Identifier<'src>,
        args: ExpressionList<'src>,
    },
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
    If {
        expr: Box<Expression<'src>>,
        body: Block<'src>,
    },
    While {
        expr: Box<Expression<'src>>,
        body: Block<'src>,
    },
    GoImmediateSource {
        #[serde(borrow)]
        source: &'src str,
    },

}

#[ast_derive]
pub struct Expression<'src> {
    pub id: NodeId,
    #[serde(borrow)]
    pub variant: Box<Expr<'src>>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct ExpressionList<'src> {
    #[serde(borrow)]
    pub list: Vec<Expression<'src>>,
    pub span: Span<'src>,
}
