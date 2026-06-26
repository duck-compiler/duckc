//! Contains definitions for use in both frontend and backend

pub mod builder;
pub mod expression;
pub mod memory_target;
pub mod span;
pub mod statement;
pub mod type_expression;
pub mod use_statement;

mod node_id;
pub use node_id::NodeId;
pub use node_id::assign_generate_node_ids;

use duckc_macros::ast_derive;
pub use expression::Expression;
pub use expression::Expr;
pub use memory_target::MemoryTarget;
pub use span::Span;
pub use statement::Statement;
pub use statement::Stmt;
pub use type_expression::TypeExpression;

use crate::ast::type_expression::TypeAnnotation;

#[ast_derive]
pub struct AstRoot<'src> {
    #[serde(borrow)]
    pub statements: Vec<Statement<'src>>,
}

#[ast_derive]
pub struct Identifier<'src> {
    pub id: NodeId,
    pub ident: &'src str,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct Block<'src> {
    #[serde(borrow)]
    pub statements: Vec<Statement<'src>>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct Parameter<'src> {
    #[serde(borrow)]
    pub name: Identifier<'src>,
    pub type_: TypeAnnotation<'src>,
    pub span: Span<'src>,
}

#[ast_derive]
pub struct ParameterList<'src> {
    #[serde(borrow)]
    pub list: Vec<Parameter<'src>>,
    pub span: Span<'src>,
}
