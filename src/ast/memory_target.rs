use crate::ast::Span;

use crate::ast::{Expression, Identifier};
use serde::{Deserialize, Serialize};

use duckc_macros::ast_derive;

#[ast_derive]
pub enum MemTar<'src> {
    #[serde(borrow)]
    Name(Identifier<'src>),
    Dereference(Box<Expression<'src>>),
    ArrayAccess {
        target: Box<MemoryTarget<'src>>,
        index_expression: Box<Expression<'src>>,
    },
    FieldAccess {
        target: Box<MemoryTarget<'src>>,
        field_name: Identifier<'src>,
    },
}

#[ast_derive]
pub struct MemoryTarget<'src> {
    #[serde(borrow)]
    pub variant: MemTar<'src>,
    pub span: Span<'src>,
}
