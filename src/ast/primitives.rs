use crate::ast::{Span, Statement};
use duckc_macros::ast_derive;
use serde::{Deserialize, Serialize};

#[ast_derive]
pub struct Body<'src> {
    #[serde(borrow)]
    pub statements: Vec<Statement<'src>>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct Parameter<'src> {
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub type_: Identifier<'src>,
}

#[ast_derive]
pub struct Identifier<'src> {
    pub ident: &'src str,
    pub span: Span<'src>,
}
