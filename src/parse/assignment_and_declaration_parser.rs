use crate::parse::{type_parser::TypeExpression, value_parser::ValueExpr};

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub type_expr: TypeExpression,
    pub initializer: Option<ValueExpr>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: String,
    pub value_expr: ValueExpr,
}
