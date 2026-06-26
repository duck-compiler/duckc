use crate::{ast::{Block, Expression, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, statement::Stmt, type_expression::TypeAnnotation}, backend::gost::{GoExpression, GoStatement, GoType}};

pub fn translate_statement<'src>(statement: Statement<'src>) -> GoStatement<'src> {
    match statement.variant {
        Stmt::FunctionDefinition { name, params, return_type, body } => {
            GoStatement::FuncDef {
                name: name.ident,
                params: translate_params(params),
                return_type: translate_type(return_type),
                body: translate_block(body)
            }
        }
        Stmt::Expression { expr } => {
            GoStatement::Expr { expr: translate_expression(expr) }
        }
        Stmt::VariableDeclaration { name, type_, init_expression } => {
            GoStatement::VarDecl {
                name: name.ident,
                type_: translate_type(type_),
                init_expression: if init_expression.is_some() {
                    Some(translate_expression(init_expression.expect("should never be none")))
                } else {
                    None
                },
            }
        }
        case => {
            unimplemented!("translate_statement: {:?}", case)
        }
    }
}

fn translate_type<'src>(type_annotation: TypeAnnotation<'src>) -> Option<GoType<'src>> {
    let annotation = type_annotation.annotation;
    if annotation.is_none() {
        return None;
    }

    let annotation = annotation.expect("annotation should not be none");
    match annotation {
        TypeExpression::String => Some(GoType::String),
        case => unimplemented!("translate_type: {:?}", case)
    }
}

fn translate_params<'src>(params: ParameterList<'src>) -> Vec<(&'src str, GoType<'src>)> {
    params
        .list
        .into_iter()
        .map(|param| {
            (param.name.ident, translate_type(param.type_).unwrap())
        })
        .collect()
}

fn translate_block<'src>(body: Block<'src>) -> Vec<GoStatement<'src>> {
    body
        .statements
        .into_iter()
        .map(translate_statement)
        .collect()
}

fn translate_expression<'src>(expr: Expression<'src>) -> GoExpression<'src> {
    match *expr.variant {
        Expr::StringLiteral(str) => {
            GoExpression::String(str)
        },
        Expr::FunctionCall { name, args } => {
            GoExpression::FuncCall { name: name.ident, args: translate_expression_list(args) }
        },
        Expr::GoImmediateSource { source } => {
            GoExpression::Immediate(source)
        }
        case => unimplemented!("translate_expression: {:?}", case)
    }
}

fn translate_expression_list<'src>(expr_list: ExpressionList<'src>) -> Vec<GoExpression<'src>> {
    expr_list
        .list
        .into_iter()
        .map(translate_expression)
        .collect()
}
