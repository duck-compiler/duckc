use crate::ast::{Identifier, Span};
use crate::ast_derives;

ast_derives! {
    pub enum Operator {
        Add,
        Sub,
        Mul,
        Div,
    }
}

ast_derives! {
    pub enum UnaryOperator {
        /// !
        Bang,
        /// -
        Neg,
    }
}

ast_derives! {
    pub enum Expr<'src> {
        Identifier(Identifier<'src>),
        StringLiteral(&'src str),
        IntLiteral(u64),
        FloatLiteral(f64),
        BoolLiteral(bool),
        Binary {
            left: Box<Expression<'src>>,
            op: Operator,
            right: Box<Expression<'src>>,
        },
        Unary {
            op: Operator,
            expr: Box<Expression<'src>>,
        },
    }
}

ast_derives! {
    pub struct Expression<'src> {
        pub variant: Expr<'src>,
        pub span: Span<'src>,
    }
}
