use crate::ast::expression::{BinaryOperator, UnaryOperator};
use crate::backend::gost::go_tree::{GoExpression, GoStatement, GoType, GostRoot};

pub fn emit_gost<'src>(root: GostRoot<'src>) -> String {
    let mut output = String::from("package main\n\n");

    output.push_str(
        &root.body
            .iter()
            .map(|statement| emit_gost_statement(statement))
            .collect::<Vec<_>>()
            .join("\n")
    );

    output
}

fn emit_type<'src>(go_type: &GoType<'src>) -> String {
    match go_type {
        GoType::String => "string".to_string(),
        GoType::Bool => "bool".to_string(),
        GoType::Int => "int".to_string(),
        GoType::Int8 => "int8".to_string(),
        GoType::Int32 => "int32".to_string(),
        GoType::Int64 => "int64".to_string(),
        GoType::Uint => "uint".to_string(),
        GoType::Uint8 => "uint8".to_string(),
        GoType::Uint32 => "uint32".to_string(),
        GoType::Uint64 => "uint64".to_string(),
        GoType::Float32 => "float32".to_string(),
        GoType::Float64 => "float64".to_string(),
        GoType::Array(go_type_box) => format!("[]{}", emit_type(go_type_box)),
        GoType::Pointer(go_type) => format!("*{}", emit_type(go_type)),
        GoType::Struct { fields } => format!(
            "struct {{\n{}\n}}",
            fields
                .into_iter()
                .map(|field| format!(
                    "{} {}{}",
                    field.name,
                    emit_type(&field.type_),
                    field.tag
                        .map(|tag| format!(" {}", tag))
                        .unwrap_or_default()
                ))
                .collect::<Vec<_>>()
                .join("\n")
        ),
        GoType::TypeName(name) => name.to_string(),
        GoType::Func { params, return_type } => format!(
            "func({}){}",
            params.iter().map(emit_type).collect::<Vec<_>>().join(", "),
            return_type.as_ref().map(|rt| format!(" {}", emit_type(rt))).unwrap_or_default(),
        ),
    }
}

fn maybe_emit_type<'src>(maybe_go_type: &Option<GoType<'src>>) -> String {
    if maybe_go_type.is_none() {
        return "".to_string();
    }

    let go_type = maybe_go_type.as_ref().expect("should not be none");

    emit_type(go_type)
}


fn emit_params<'src>(params: &Vec<(&'src str, GoType<'src>)>) -> String {
    params
        .iter()
        .map(|(param_name, param_type)| format!("{param_name} {}", emit_type(param_type)))
        .collect::<Vec<_>>().join(", ")
}

fn emit_arguments<'src>(args: &Vec<GoExpression<'src>>) -> String {
    args
        .iter()
        .map(|arg| emit_expr(arg))
        .collect::<Vec<_>>().join(", ")
}

fn emit_block<'src>(body: &Vec<GoStatement<'src>>) -> String {
    format!("{{\n{}\n}}", body
        .iter()
        .map(|statement| emit_gost_statement(statement))
        .collect::<Vec<_>>().join("\n"))
}

fn emit_gost_statement<'src>(statement: &GoStatement<'src>) -> String {
    match statement {
        GoStatement::Expr { expr } => emit_expr(expr),
        GoStatement::GoImport { alias, path } => {
            format!(
                "import {}\"{path}\"",
                alias
                    .map(|a| format!("{a} "))
                    .unwrap_or_default()
            )
        }
        GoStatement::FuncDef { name, params, return_type, body } => {
            format!(
                "func {name}({}) {} {}",
                emit_params(params),
                if return_type.is_some() { maybe_emit_type(return_type) } else { "".to_string() },
                emit_block(body)
            )
        },
        GoStatement::VarDecl { name, type_, init_expression } => {
            format!(
                "var {name} {}{}",
                maybe_emit_type(type_),
                if init_expression.is_some() {
                    format!(" = {}", emit_expr(init_expression.as_ref().expect("should never be none")))
                } else {
                    "".to_string()
                }
            )
        },
        GoStatement::Assign { target, expr } => {
            format!("{} = {}", emit_expr(target), emit_expr(expr))
        },
        GoStatement::TypeDecl { name, type_ } => {
            format!("type {name} {}", emit_type(type_))
        },
        GoStatement::If { condition, body, else_body } => {
            format!(
                "if {} {}{}",
                emit_expr(condition),
                emit_block(body),
                else_body.as_ref().map(|else_body| format!(" else {}", emit_block(else_body))).unwrap_or_default(),
            )
        },
        GoStatement::While { condition, body } => {
            format!("for {} {}", emit_expr(condition), emit_block(body))
        },
        GoStatement::Return { value } => {
            format!("return{}", value.as_ref().map(|value| format!(" {}", emit_expr(value))).unwrap_or_default())
        },
        GoStatement::Break => "break".to_string(),
        GoStatement::Continue => "continue".to_string(),
    }
}

fn emit_binary_operator(op: &BinaryOperator) -> &'static str {
    match op {
        BinaryOperator::Add => "+",
        BinaryOperator::Sub => "-",
        BinaryOperator::Mul => "*",
        BinaryOperator::Div => "/",
        BinaryOperator::Eq => "==",
        BinaryOperator::NotEq => "!=",
        BinaryOperator::Less => "<",
        BinaryOperator::Greater => ">",
        BinaryOperator::LessEq => "<=",
        BinaryOperator::GreaterEq => ">=",
        BinaryOperator::And => "&&",
        BinaryOperator::Or => "||",
    }
}

fn emit_unary_operator(op: &UnaryOperator) -> &'static str {
    match op {
        UnaryOperator::Bang => "!",
        UnaryOperator::Neg => "-",
    }
}

fn emit_expr<'src>(expr: &GoExpression) -> String {
    match expr {
        GoExpression::String(str) => format!("\"{str}\""),
        GoExpression::FuncCall { callee, args } => {
            format!("{}({})", emit_expr(callee), emit_arguments(args))
        },
        GoExpression::Selector { base, field } => {
            format!("{}.{field}", emit_expr(base))
        },
        GoExpression::Array { elem_type, values } => {
            format!("[]{}{{{}}}", emit_type(elem_type), emit_arguments(values))
        },
        GoExpression::ArrayIndex { base, index } => {
            format!("{}[{}]", emit_expr(base), emit_expr(index))
        },
        GoExpression::StructInit { type_name, fields } => {
            format!(
                "{type_name}{{{}}}",
                fields
                    .iter()
                    .map(|(name, value)| format!("{name}: {}", emit_expr(value)))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        },
        GoExpression::Bool(b) => format!("{b}"),
        GoExpression::BinaryOp { left, op, right } => {
            format!("({} {} {})", emit_expr(left), emit_binary_operator(op), emit_expr(right))
        },
        GoExpression::UnaryOp { op, expr } => {
            format!("({}{})", emit_unary_operator(op), emit_expr(expr))
        },
        GoExpression::AddressOf(expr) => format!("(&{})", emit_expr(expr)),
        GoExpression::Dereference(expr) => format!("(*{})", emit_expr(expr)),
        GoExpression::Int(i) => format!("{i}"),
        GoExpression::Float64(f) => format!("{f:?}"),
        GoExpression::Immediate(source) => source.to_string(),
    }
}
