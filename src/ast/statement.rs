use serde::{Deserialize, Serialize};

use crate::{
    ast::{Expression, Identifier, Span},
    ast_derives,
};

ast_derives! {
    pub enum Stmt<'src> {
        VariableDeclaration {
            #[serde(borrow)]
            name: Identifier<'src>,
            init_expression: Option<Expression>,
        },
        VariableAssignment {},
        FunctionCall,
        If,
        While,
    }
}

ast_derives! {
    pub struct Block<'src> {
        #[serde(borrow)]
        pub contents: Vec<Statement<'src>>,
        pub span: Span<'src>,
    }
}

ast_derives! {
    pub struct Statement<'src> {
        #[serde(borrow)]
        pub variant: Stmt<'src>,
        pub span: Span<'src>,
    }
}
