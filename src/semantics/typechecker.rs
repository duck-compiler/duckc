use std::{collections::HashMap, process};

use crate::parse::{type_parser::{Duck, TypeExpression}, value_parser::ValueExpr};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    pub types: Vec<HashMap<String, TypeExpression>>,
}

impl TypeExpression {
    fn to_ref_string(&self) -> String {
        return match self {
            TypeExpression::Any => "Any".to_string(),
            TypeExpression::Bool => "bool".to_string(),
            TypeExpression::Int => "int".to_string(),
            TypeExpression::Float => "float32".to_string(),
            TypeExpression::Char => "char".to_string(),
            TypeExpression::String => "string".to_string(),
            TypeExpression::Go(identifier) => identifier.clone(),
            TypeExpression::TypeName(name) => name.clone(),
            TypeExpression::Struct(r#struct) => format!("Struct{}", r#struct.fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", (*type_expr).to_ref_string())).collect::<Vec<_>>().join("_")),
            TypeExpression::Duck(duck) => {
                let fields = duck.fields.clone().sort_by_key(|(field_name, _)| field_name.clone());
                format!("Duck{}", r#duck.fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", type_expr.to_ref_string())).collect::<Vec<_>>().join("_"))
            },
            TypeExpression::Tuple(fields) => {
                format!("Tuple{}", fields.iter().map(|type_expr| format!("{}", type_expr.to_ref_string())).collect::<Vec<_>>().join("_"))
            },
            TypeExpression::Or(variants) => todo!("implement variants")
        };
    }

    fn from_value_expr(value_expr: &ValueExpr) -> TypeExpression {
        return match value_expr {
            ValueExpr::Int(..) => TypeExpression::Int,
            ValueExpr::Bool(..) => TypeExpression::Bool,
            ValueExpr::Char(..) => TypeExpression::Char,
            ValueExpr::Float(..) => TypeExpression::Float,
            ValueExpr::VarAssign(..) => TypeExpression::Tuple(vec![]),
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| TypeExpression::from_value_expr(value_expr))
                    .collect::<Vec<TypeExpression>>();

                TypeExpression::Tuple(types)
            },
            ValueExpr::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| (field_name.to_string(), TypeExpression::from_value_expr(value_expr)))
                    .collect::<Vec<(String, TypeExpression)>>();

                TypeExpression::Duck(Duck { fields: types })
            },
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpression = TypeExpression::from_value_expr(left);
                let right_type_expr: TypeExpression = TypeExpression::from_value_expr(right);

                require_type_is_number(&left_type_expr);
                check_type_compatability(&left_type_expr, &right_type_expr);

                TypeExpression::Any
            },
            ValueExpr::FunctionCall { target, params } => {
                todo!("Return Type of Function");
            },
            ValueExpr::Block(value_exprs) => TypeExpression::from(
                value_exprs
                    .last()
                    .map(|value_expr| TypeExpression::from_value_expr(value_expr))
                    .expect("Block Expressions must be at least one expression long."),
            ),
            ValueExpr::Variable(identifier) => todo!("resolve type for identifier"),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(&TypeExpression::from_value_expr(bool_expr), &TypeExpression::Bool);
                TypeExpression::Bool
            },
            ValueExpr::If { condition, then, r#else } => {
                let condition_type_expr = TypeExpression::from_value_expr(condition);
                check_type_compatability(&condition_type_expr, &TypeExpression::Bool);

                let then_type_expr = TypeExpression::from_value_expr(then);
                let else_type_expr = TypeExpression::from_value_expr(r#else);

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                todo!("combine then and else, then return combined type");
            },
            ValueExpr::FieldAccess { target_obj, field_name } => {
                let target_obj_type_expr = TypeExpression::from_value_expr(target_obj);
                todo!()
            }
        };
    }
}

fn type_has_field(type_expr: &TypeExpression, identifier: &TypeExpression) {
}

fn type_is_object_like(type_expr: &TypeExpression) -> bool {
    match type_expr {
        TypeExpression::Struct(..) => true,
        TypeExpression::Tuple(..) => true,
        _ => false,
    }
}

fn type_is_number(type_expr: &TypeExpression) -> bool {
    return *type_expr == TypeExpression::Int || *type_expr == TypeExpression::Float;
}

fn require_type_is_number(type_expr: &TypeExpression) {
    if !type_is_number(type_expr) {
        println!("Required type to be of class number but got {}.", type_expr.to_ref_string());
        process::exit(2);
    }
}

fn check_type_compatability(one: &TypeExpression, two: &TypeExpression) {
    if type_is_number(&one) {
        if !type_is_number(&two) {
            println!("Types {} and {} are not compatible.", one.to_ref_string(), two.clone().to_ref_string());
            process::exit(2);
        }

        return;
    }

    if *one != *two {
        println!("Types {} and {} are not compatible.", one.to_ref_string(), two.to_ref_string());
        process::exit(2);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_typechecking() {}
}
