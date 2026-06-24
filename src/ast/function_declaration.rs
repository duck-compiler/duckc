use crate::ast::{
    Span,
    primitives::{Identifier, Parameter},
    statement::Block,
};
use crate::ast_derives;
use serde::{Deserialize, Serialize};

ast_derives! {
    pub struct FunctionDeclaration<'src> {
        #[serde(borrow)]
        pub name: Identifier<'src>,
        pub params: Vec<Parameter<'src>>,
        pub body: Block<'src>,
        pub span: Span<'src>,
    }
}
