use duckc_macros::ast_derive;
use crate::ast::{Identifier, Span};

#[ast_derive]
pub struct UseStatement<'src> {
    #[serde(borrow)]
    pub path: Identifier<'src>,
    pub alias: Option<Identifier<'src>>,
    pub span: Span<'src>,
}
