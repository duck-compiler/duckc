use crate::ast::{Span, Statement};
use crate::ast_derives;
use serde::{Deserialize, Serialize};

ast_derives! {
    pub struct Body<'src> {
        #[serde(borrow)]
        pub contents: Vec<Statement<'src>>,
        pub span: Span<'src>,
    }
}

ast_derives! {
    pub struct Parameter<'src> {
        #[serde(borrow)]
        pub name: Identifier<'src>,
        pub type_: Identifier<'src>,
    }
}

ast_derives! {
    pub struct Identifier<'src> {
        pub ident: &'src str,
        pub span: Span<'src>,
    }
}
