use duckc_macros::ast_derive;
use serde::{Deserialize, Serialize};

use crate::ast::{Expression, Identifier, MemoryTarget, Span, TypeExpression};

#[ast_derive]
pub enum Stmt<'src> {
    VariableDeclaration {
        #[serde(borrow)]
        name: Identifier<'src>,
        type_expresssion: Option<TypeExpression<'src>>,
        init_expression: Option<Expression<'src>>,
    },
    VariableAssignment {
        #[serde(borrow)]
        target: MemoryTarget<'src>,
        assign_expression: Option<Expression<'src>>,
    },
    FunctionCall,
    If {
        expr: Expression<'src>,
        contents: Block<'src>,
        // TODO: ELSE
    },
    While {
        expr: Expression<'src>,
        contents: Block<'src>,
    },
}

#[ast_derive]
pub struct Block<'src> {
    #[serde(borrow)]
    pub contents: Vec<Statement<'src>>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct Statement<'src> {
    #[serde(borrow)]
    pub variant: Stmt<'src>,
    pub span: Span<'src>,
}
