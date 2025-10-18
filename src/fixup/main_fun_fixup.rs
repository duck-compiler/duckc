use crate::{parse::{function_parser::FunctionDefintion, value_parser::ValueExpr}, semantics::type_resolve::TypeEnv};

#[allow(dead_code)]
fn fixup_main_fun(fun_def: &mut FunctionDefintion) {
    if fun_def.name != "main" {
        return;
    }
    // TODO: resolve returns in main function to use the exit() fn of go, or wrap the code of the main function into a second function which then get's called inside a new main which just returns exit(other_main())
}

fn resolve_extension_calls_in_value_expr(value_expr: &mut ValueExpr, type_env: &mut TypeEnv) {
    match value_expr {
        ValueExpr::FunctionCall { target, params, type_params } => {
            resolve_extension_calls_in_value_expr(&mut target.as_mut().0, type_env);
            for param in params {
                resolve_extension_calls_in_value_expr(&mut param.0, type_env);
            }
        },
        ValueExpr::Int(_) => todo!(),
        ValueExpr::String(_, _) => todo!(),
        ValueExpr::Bool(_) => todo!(),
        ValueExpr::Float(_) => todo!(),
        ValueExpr::Char(_) => todo!(),
        ValueExpr::RawVariable(_, items) => todo!(),
        ValueExpr::Variable(_, _, type_expr, _) => todo!(),
        ValueExpr::If { condition, then, r#else } => todo!(),
        ValueExpr::While { condition, body } => todo!(),
        ValueExpr::Tuple(items) => todo!(),
        ValueExpr::Block(items) => todo!(),
        ValueExpr::Break => todo!(),
        ValueExpr::Continue => todo!(),
        ValueExpr::Duck(items) => todo!(),
        ValueExpr::HtmlString(items) => todo!(),
        ValueExpr::Tag(_) => todo!(),
        ValueExpr::Struct { name, fields, type_params } => todo!(),
        ValueExpr::FieldAccess { target_obj, field_name } => todo!(),
        ValueExpr::Array(_, items) => todo!(),
        ValueExpr::Return(_) => todo!(),
        ValueExpr::VarAssign(_) => todo!(),
        ValueExpr::VarDecl(_) => todo!(),
        ValueExpr::Add(_, _) => todo!(),
        ValueExpr::Sub(_, _) => todo!(),
        ValueExpr::Mul(_, _) => todo!(),
        ValueExpr::Div(_, _) => todo!(),
        ValueExpr::Mod(_, _) => todo!(),
        ValueExpr::BoolNegate(_) => todo!(),
        ValueExpr::Equals(_, _) => todo!(),
        ValueExpr::NotEquals(_, _) => todo!(),
        ValueExpr::LessThan(_, _) => todo!(),
        ValueExpr::LessThanOrEquals(_, _) => todo!(),
        ValueExpr::GreaterThan(_, _) => todo!(),
        ValueExpr::GreaterThanOrEquals(_, _) => todo!(),
        ValueExpr::And(_, _) => todo!(),
        ValueExpr::Or(_, _) => todo!(),
        ValueExpr::InlineGo(_) => todo!(),
        ValueExpr::Lambda(lambda_function_expr) => todo!(),
        ValueExpr::ArrayAccess(_, _) => todo!(),
        ValueExpr::Match { value_expr, arms, else_arm, span } => todo!(),
        ValueExpr::FormattedString(items) => todo!(),
        ValueExpr::Ref(_) => todo!(),
        ValueExpr::RefMut(_) => todo!(),
        ValueExpr::Deref(_) => todo!(),
    }
}
