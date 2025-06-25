use crate::parse::{Spanned, type_parser::TypeExpr, value_parser::ValueExpr};

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub type_expr: Spanned<TypeExpr>,
    pub initializer: Option<Spanned<ValueExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value_expr: Spanned<ValueExpr>,
}
