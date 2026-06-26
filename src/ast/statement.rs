use duckc_macros::ast_derive;

use crate::ast::type_expression::TypeAnnotation;
use crate::ast::use_statement::UseStatement;
use crate::ast::{Block, NodeId, ParameterList};
use crate::ast::{Expression, Identifier, MemoryTarget, Span};

#[ast_derive]
pub enum Stmt<'src> {
    Use(UseStatement<'src>),
    FunctionDefinition {
        #[serde(borrow)]
        name: Identifier<'src>,
        params: ParameterList<'src>,
        body: Block<'src>,
        return_type: TypeAnnotation<'src>,
    },
    VariableDeclaration {
        #[serde(borrow)]
        name: Identifier<'src>,
        type_: TypeAnnotation<'src>,
        init_expression: Option<Expression<'src>>,
    },
    VariableAssignment {
        #[serde(borrow)]
        target: MemoryTarget<'src>,
        assign_expression: Expression<'src>,
    },
    Expression {
        #[serde(borrow)]
        expr: Expression<'src>,
    }
}

#[ast_derive]
pub struct Statement<'src> {
    pub id: NodeId,
    #[serde(borrow)]
    pub variant: Stmt<'src>,
    pub span: Span<'src>,
}
