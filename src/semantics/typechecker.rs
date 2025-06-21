use std::collections::HashMap;

use crate::parse::type_parser::{TypeExpression, Duck};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    pub types: Vec<HashMap<String, TypeExpression>>,
}

impl Into<TypeExpression> for crate::parse::value_parser::ValueExpr {
    fn into(self) -> TypeExpression {
        return match self {
            Self::Int(..) => TypeExpression::Int,
            Self::Bool(..) => TypeExpression::Bool,
            Self::Char(..) => TypeExpression::Char,
            Self::Float(..) => TypeExpression::Float,
            Self::VarAssign(..) => TypeExpression::Tuple(vec![]),
            Self::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| value_expr.into())
                    .collect::<Vec<TypeExpression>>();

                TypeExpression::Tuple(types)
            },
            Self::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| (field_name, value_expr.into()))
                    .collect::<Vec<(String, TypeExpression)>>();

                TypeExpression::Duck(Duck { fields: types })
            },
        }
    }
}
