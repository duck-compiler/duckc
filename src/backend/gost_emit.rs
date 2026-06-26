use crate::backend::gost::{GoExpression, GoStatement, GoType, GostRoot};

pub fn emit_gost<'src>(root: GostRoot<'src>) -> String {
    let mut output = String::new();

    for statement in &root.body {
        output.push_str(&emit_gost_statement(statement));
    }

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
            format!("{target} = {}", emit_expr(expr))
        },
    }
}

fn emit_expr<'src>(expr: &GoExpression) -> String {
    match expr {
        GoExpression::String(str) => format!("\"{str}\""),
        GoExpression::FuncCall { name, args } => {
            format!("{}({})", name, emit_arguments(args))
        },
        GoExpression::Int(i) => format!("{i}"),
        GoExpression::Int8(i) => format!("int8({i})"),
        GoExpression::Int32(i) => format!("int32({i})"),
        GoExpression::Int64(i) => format!("int64({i})"),
        GoExpression::Uint(i) => format!("uint({i})"),
        GoExpression::Uint8(i) => format!("uint8({i})"),
        GoExpression::Uint32(i) => format!("uint32({i})"),
        GoExpression::Uint64(i) => format!("uint64({i})"),
        GoExpression::Float32(f) => format!("float32({f})"),
        GoExpression::Float64(f) => format!("float64({f})"),
        GoExpression::Immediate(source) => source.to_string(),
    }
}
