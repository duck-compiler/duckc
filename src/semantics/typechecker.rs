use std::{collections::HashMap, process};

use crate::parse::{type_parser::{Duck, Struct, TypeExpr}, value_parser::ValueExpr};

#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    pub types: Vec<HashMap<String, TypeExpr>>,
}

impl TypeExpr {
    fn to_ref_string(&self) -> String {
        return match self {
            TypeExpr::Any => "Any".to_string(),
            TypeExpr::Bool => "bool".to_string(),
            TypeExpr::Int => "int".to_string(),
            TypeExpr::Float => "float32".to_string(),
            TypeExpr::Char => "char".to_string(),
            TypeExpr::String => "string".to_string(),
            TypeExpr::Go(identifier) => identifier.clone(),
            TypeExpr::TypeName(name) => name.clone(),
            TypeExpr::Struct(r#struct) => format!("Struct{}", r#struct.fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", (*type_expr).to_ref_string())).collect::<Vec<_>>().join("_")),
            TypeExpr::Duck(duck) => {
                let mut fields = duck.fields.clone();
                fields.sort_by_key(|(field_name, _)| field_name.clone());

                format!("Duck{}", fields.iter().map(|(field_name, type_expr)| format!("{field_name}_{}", type_expr.to_ref_string())).collect::<Vec<_>>().join("_"))
            },
            TypeExpr::Tuple(fields) => {
                format!("Tuple{}", fields.iter().map(|type_expr| format!("{}", type_expr.to_ref_string())).collect::<Vec<_>>().join("_"))
            },
            TypeExpr::Or(variants) => todo!("implement variants")
        };
    }

    fn from_value_expr(value_expr: &ValueExpr) -> TypeExpr {
        return match value_expr {
            ValueExpr::Int(..) => TypeExpr::Int,
            ValueExpr::Bool(..) => TypeExpr::Bool,
            ValueExpr::Char(..) => TypeExpr::Char,
            ValueExpr::Float(..) => TypeExpr::Float,
            ValueExpr::String(..) => TypeExpr::String,
            ValueExpr::Break => TypeExpr::Tuple(vec![]),
            ValueExpr::Continue => TypeExpr::Tuple(vec![]),
            ValueExpr::Return(Some(value_expr)) => TypeExpr::from_value_expr(value_expr),
            ValueExpr::Return(None) => TypeExpr::Any, // TODO return never !
            ValueExpr::VarAssign(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::VarDecl(..) => TypeExpr::Tuple(vec![]),
            ValueExpr::Struct(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| (field_name.to_string(), TypeExpr::from_value_expr(value_expr)))
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Struct(Struct { fields: types })
            },
            ValueExpr::Tuple(fields) => {
                let types = fields
                    .into_iter()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr))
                    .collect::<Vec<TypeExpr>>();

                TypeExpr::Tuple(types)
            },
            ValueExpr::Duck(fields) => {
                let types = fields
                    .into_iter()
                    .map(|(field_name, value_expr)| (field_name.to_string(), TypeExpr::from_value_expr(value_expr)))
                    .collect::<Vec<(String, TypeExpr)>>();

                TypeExpr::Duck(Duck { fields: types })
            },
            ValueExpr::Add(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                require_type_is_number(&left_type_expr);
                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            },
            ValueExpr::Equals(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            },
            ValueExpr::Mul(left, right) => {
                let left_type_expr: TypeExpr = TypeExpr::from_value_expr(left);
                let right_type_expr: TypeExpr = TypeExpr::from_value_expr(right);

                require_type_is_number(&left_type_expr);
                check_type_compatability(&left_type_expr, &right_type_expr);

                left_type_expr
            },
            ValueExpr::FunctionCall { target, params } => {
                todo!("Return Type of Function");
            },
            ValueExpr::Block(value_exprs) => TypeExpr::from(
                value_exprs
                    .last()
                    .map(|value_expr| TypeExpr::from_value_expr(value_expr))
                    .expect("Block Expressions must be at least one expression long."),
            ),
            ValueExpr::Variable(identifier) => todo!("resolve type for identifier"),
            ValueExpr::BoolNegate(bool_expr) => {
                check_type_compatability(&TypeExpr::from_value_expr(bool_expr), &TypeExpr::Bool);
                TypeExpr::Bool
            },
            ValueExpr::If { condition, then, r#else } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool);

                let then_type_expr = TypeExpr::from_value_expr(then);
                let else_type_expr = TypeExpr::from_value_expr(r#else);

                // let x: TypeExpression = combine_types(vec![else_type_expr, then]);

                todo!("combine then and else, then return combined type");
            },
            ValueExpr::FieldAccess { target_obj, field_name } => {
                let target_obj_type_expr = TypeExpr::from_value_expr(target_obj);
                todo!()
            },
            ValueExpr::While { condition, body } => {
                let condition_type_expr = TypeExpr::from_value_expr(condition);
                check_type_compatability(&condition_type_expr, &TypeExpr::Bool);

                let body_type_expr = TypeExpr::from_value_expr(body);

                return TypeExpr::Tuple(vec![])
            },
        };
    }
}

fn type_has_field(type_expr: &TypeExpr, identifier: &TypeExpr) {
}
    fn is_object_like(&self) -> bool {
        match self {
            Self::Tuple(..)
            | Self::Duck(..)
            | Self::Struct(..) => true,
            _ => false,
        }
    }

    fn has_field(&self, field: (String, TypeExpr)) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct.fields.iter().any(|struct_field| *struct_field == field),
            Self::Duck(duck) => duck.fields.iter().any(|struct_field| *struct_field == field),
            _ => false,
        }
    }

    fn has_field_by_name(&self, name: String) -> bool {
        match self {
            Self::Tuple(..) => todo!("Waiting for field access to have numbers available."),
            Self::Struct(r#struct) => r#struct.fields.iter().any(|struct_field| *struct_field.0 == name),
            Self::Duck(duck) => duck.fields.iter().any(|struct_field| *struct_field.0 == name),
            _ => false,
        }
    }

    fn get_field_type(&self, field_name)

    fn is_number(&self) -> bool {
        return *self == TypeExpr::Int || *self == TypeExpr::Float;
    }

}

fn require(condition: bool, fail_message: String) {
    if !condition {
        println!("TypeError: {}", fail_message);
        process::exit(2);
    }
}

fn require_type_is_number(type_expr: &TypeExpr) {
    if !type_is_number(type_expr) {
        println!("Required type to be of class number but got {}.", type_expr.to_ref_string());
        process::exit(2);
    }
}

fn check_type_compatability(one: &TypeExpr, two: &TypeExpr) {
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
