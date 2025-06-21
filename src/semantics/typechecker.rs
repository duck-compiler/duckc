use std::{collections::HashMap, process};

use crate::parse::{type_parser::{Duck, TypeExpression}, value_parser::ValueExpr};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    pub types: Vec<HashMap<String, TypeExpression>>,
}

impl From<TypeExpression> for String {
    fn from(type_expr: TypeExpression) -> String {
        return match type_expr {
            TypeExpression::Any => "Any".to_string(),
            TypeExpression::Bool => "bool".to_string(),
            TypeExpression::Int => "int".to_string(),
            TypeExpression::Float => "float32".to_string(),
            TypeExpression::Char => "char".to_string(),
            TypeExpression::String => "string".to_string(),
            TypeExpression::Go(identifier) => identifier.clone(),
            TypeExpression::TypeName(name) => name.clone(),
            TypeExpression::Struct(r#struct) => format!("Struct{}", r#struct.fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", (*type_expr).into())).collect::<Vec<_>>().join("_")),
            TypeExpression::Duck(duck) => {
                let fields = duck.fields.clone().sort_by_key(|(field_name, _)| field_name);
                format!("Duck{}", r#duck.fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", type_expr.into())).collect::<Vec<_>>().join("_"))
            },
            TypeExpression::Tuple(fields) => {
                format!("Tuple{}", fields.iter().map(|type_expr| format!("{}", type_expr.into())).collect::<Vec<_>>().join("_"))
            },
        };
    }
}

impl From<ValueExpr> for TypeExpression {
    fn from(value: ValueExpr) -> Self {
        return match value {
            ValueExpr::Int(..) => TypeExpression::Int,
            ValueExpr::Bool(..) => TypeExpression::Bool,
            ValueExpr::Char(..) => TypeExpression::Char,
            ValueExpr::Float(..) => TypeExpression::Float,
            ValueExpr::VarAssign(..) => TypeExpression::Tuple(vec![]),
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| value_expr.into())
                    .collect::<Vec<TypeExpression>>();

                TypeExpression::Tuple(types)
            },
            ValueExpr::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| (field_name, value_expr.into()))
                    .collect::<Vec<(String, TypeExpression)>>();

                TypeExpression::Duck(Duck { fields: types })
            },
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpression = (*left).into();
                let right_type_expr: TypeExpression = (*right).into();

                require_type_is_number(&left_type_expr);
                check_type_compatability(&left_type_expr, &right_type_expr);

                TypeExpression::Any
            },
            _ => todo!(),
        };
    }
}

fn type_is_number(type_expr: &TypeExpression) -> bool {
    return *type_expr == TypeExpression::Int || *type_expr == TypeExpression::Float;
}

fn require_type_is_number(type_expr: &TypeExpression) {
    if !type_is_number(type_expr) {
        println!("Required type to be of class number but got {}.", String::from(type_expr.clone()));
        process::exit(2);
    }
}

fn check_type_compatability(one: &TypeExpression, two: &TypeExpression) {
    if type_is_number(&one) {
        if !type_is_number(&two) {
            println!("Types {} and {} are not compatible.", String::from(one.clone()), String::from(two.clone()));
            process::exit(2);
        }

        return;
    }

    if *one != *two {
        println!("Types {} and {} are not compatible.", String::from(one.clone()), String::from(two.clone()));
        process::exit(2);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_typechecking() {}
}
