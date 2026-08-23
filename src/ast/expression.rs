use crate::ast::{Block, Identifier, MemoryTarget, NodeId, Span, TypeExpression, memory_target::MemTar};
use duckc_macros::ast_derive;

#[derive(Clone, Copy)]
#[ast_derive]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy)]
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
    ArrayExpression {
        values_exprs: Vec<Box<Expression<'src>>>,
    },
    FunctionCall {
        #[serde(borrow)]
        target: Box<Expression<'src>>,
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
        else_branch: Option<Block<'src>>,
    },
    While {
        expr: Box<Expression<'src>>,
        body: Block<'src>,
    },
    StructInit {
        #[serde(borrow)]
        type_name: Identifier<'src>,
        fields: Vec<FieldInit<'src>>,
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

#[ast_derive]
pub struct FieldInit<'src> {
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub value: Expression<'src>,
    pub span: Span<'src>,
}
