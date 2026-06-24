//! Contains definitions for use in both frontend and backend

pub mod expression;
pub mod function_declaration;
pub mod primitives;
pub mod span;
pub mod statement;
pub mod use_statement;

#[macro_export]
macro_rules! ast_derives {
    ($item:item) => {
        #[derive(Debug, Deserialize, Serialize, PartialEq)]
        $item
    };
}

pub use expression::Expression;
pub use function_declaration::FunctionDeclaration;
pub use primitives::Identifier;
pub use span::Span;
pub use statement::Statement;
pub use use_statement::UseStatement;
