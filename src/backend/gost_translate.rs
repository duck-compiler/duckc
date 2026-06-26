use crate::{ast::{Block, Expression, NodeId, ParameterList, Statement, TypeExpression, expression::{Expr, ExpressionList}, statement::Stmt, type_expression::TypeAnnotation}, backend::gost::{GoExpression, GoStatement, GoType}, semantics::{context::SemanticsContext, module::ModuleId, symbol::Origin}};

pub struct Translator<'a, 'src> {
    pub context: &'a SemanticsContext<'src>,
    pub module: ModuleId
}

impl<'a, 'src> Translator<'a, 'src> {
    fn resolved_name(&self, node: NodeId, fallback: &'src str) -> &'src str {
        let resolutions = &self.context.modules[self.module.0 as usize].resolutions;
        match resolutions[node.0 as usize] {
            Some(symbol) => match &self.context.symbols[symbol.0 as usize].origin {
                Origin::Duck { .. } => self.context.symbols[symbol.0 as usize].name,
                Origin::Builtin => self.context.symbols[symbol.0 as usize].name
            },
            None => fallback,
        }
    }

    pub fn translate_statement(&self, statement: &Statement<'src>) -> GoStatement<'src> {
        match &statement.variant {
            Stmt::FunctionDefinition { name, params, return_type, body } => {
                GoStatement::FuncDef {
                    name: name.ident,
                    params: self.translate_params(params),
                    return_type: self.translate_type_annotation(return_type),
                    body: self.translate_block(body)
                }
            }
            Stmt::Expression { expr } => {
                GoStatement::Expr { expr: self.translate_expression(expr) }
            }
            Stmt::VariableDeclaration { name, type_, init_expression } => {
                GoStatement::VarDecl {
                    name: name.ident,
                    type_: self.translate_type_annotation(type_),
                    init_expression: if init_expression.is_some() {
                        Some(self.translate_expression(init_expression.as_ref().expect("should never be none")))
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

    fn translate_type_annotation(&self, type_annotation: &TypeAnnotation<'src>) -> Option<GoType<'src>> {
        type_annotation.annotation.as_ref().map(|type_expr| self.translate_type_expression(type_expr))
    }

    fn translate_type_expression(&self, expr: &TypeExpression<'src>) -> GoType<'src> {
        match expr {
            TypeExpression::String => GoType::String,
            case => unimplemented!("translate_type_expression: {:?}", case)
        }
    }

    fn translate_params(&self, params: &ParameterList<'src>) -> Vec<(&'src str, GoType<'src>)> {
        params
            .list
            .iter()
            .map(|param| {
                (param.name.ident, self.translate_type_annotation(&param.type_).unwrap())
            })
            .collect()
    }

    fn translate_block(&self, body: &Block<'src>) -> Vec<GoStatement<'src>> {
        body
            .statements
            .iter()
            .map(|stmt| self.translate_statement(&stmt))
            .collect()
    }

    fn translate_expression(&self, expr: &Expression<'src>) -> GoExpression<'src> {
        match &*expr.variant {
            Expr::StringLiteral(str) => {
                GoExpression::String(str)
            },
            Expr::FunctionCall { name, args } => {
                GoExpression::FuncCall {
                    name: self.resolved_name(name.id, name.ident),
                    args: self.translate_expression_list(args)
                }
            },
            Expr::GoImmediateSource { source } => {
                GoExpression::Immediate(source)
            }
            case => unimplemented!("translate_expression: {:?}", case)
        }
    }

    fn translate_expression_list(&self, expr_list: &ExpressionList<'src>) -> Vec<GoExpression<'src>> {
        expr_list
            .list
            .iter()
            .map(|expr| self.translate_expression(expr))
            .collect()
    }
}
