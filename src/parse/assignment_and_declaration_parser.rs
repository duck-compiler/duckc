use crate::parse::{type_parser::TypeExpr, value_parser::ValueExpr};

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub type_expr: TypeExpr,
    pub initializer: Option<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value_expr: ValueExpr,
}
