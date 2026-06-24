//! Contains definitions for use in both frontend and backend

pub mod expression;
pub mod function_declaration;
pub mod memory_target;
pub mod primitives;
pub mod span;
pub mod statement;
pub mod type_expression;
pub mod use_statement;

pub use expression::Expression;
pub use function_declaration::FunctionDeclaration;
pub use memory_target::MemoryTarget;
pub use primitives::Identifier;
use serde_json::ser::CharEscape::Quote;
pub use span::Span;
pub use statement::Statement;
pub use type_expression::TypeExpression;
pub use use_statement::UseStatement;
