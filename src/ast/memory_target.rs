use crate::ast::Span;
use crate::ast::{Expression, Identifier, TypeExpression};
use duckc_macros::ast_derive;

#[ast_derive]
pub enum MemTar<'src> {
    #[serde(borrow)]
    Name(Identifier<'src>),
    Dereference(Box<Expression<'src>>),
    ArrayAccess {
        target: Box<Expression<'src>>,
        index_expression: Box<Expression<'src>>,
    },
    FieldAccess {
        target: Box<Expression<'src>>,
        field_name: Identifier<'src>,
        type_args: Vec<TypeExpression<'src>>,
    },
    TupleIndex {
        target: Box<Expression<'src>>,
        index: usize,
    },
}

#[ast_derive]
pub struct MemoryTarget<'src> {
    #[serde(borrow)]
    pub variant: MemTar<'src>,
    pub span: Span<'src>,
}
