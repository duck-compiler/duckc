use crate::ast::{
    Span,
    primitives::{Identifier, Parameter},
    statement::Block,
};
use duckc_macros::ast_derive;
use serde::{Deserialize, Serialize};

#[ast_derive]
pub struct FunctionDeclaration<'src> {
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub params: Vec<Parameter<'src>>,
    pub body: Block<'src>,
    pub span: Span<'src>,
}
