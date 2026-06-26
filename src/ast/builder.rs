//! the builder is used for constructing AST nodes with empty spans, used when doing middle-/backend tests
//! these builders should never be used inside of parsers for constructing AST nodes

use crate::ast::{
    AstRoot, Block, Expression, Identifier, NodeId, Parameter, ParameterList, Span, Statement, TypeExpression, expression::{Expr, ExpressionList}, statement::Stmt, type_expression::TypeAnnotation
};

fn empty_span<'src>() -> Span<'src> {
    Span {
        start: 0,
        end: 0,
        file_path: "",
    }
}

pub fn ident<'src>(name: &'src str) -> Identifier<'src> {
    Identifier {
        id: NodeId::DUMMY,
        ident: name,
        span: empty_span(),
    }
}

pub fn type_<'src>(type_expr: TypeExpression) -> TypeAnnotation {
    TypeAnnotation {
        annotation: Some(type_expr),
        span: empty_span(),
    }
}

pub fn no_type<'src>() -> TypeAnnotation<'src> {
    TypeAnnotation {
        annotation: None,
        span: empty_span(),
    }
}

pub fn stmt<'src>(variant: Stmt<'src>) -> Statement<'src> {
    Statement {
        id: NodeId::DUMMY,
        variant,
        span: empty_span(),
    }
}

pub fn expr_stmt<'src>(expr: Expression<'src>) -> Statement<'src> {
    stmt(Stmt::Expression {
        expr,
    })
}

pub fn expr<'src>(variant: Expr<'src>) -> Expression<'src> {
    Expression {
        id: NodeId::DUMMY,
        variant: Box::new(variant),
        span: empty_span(),
    }
}

fn block<'src>(stmts: Vec<Statement<'src>>) -> Block<'src> {
    Block {
        statements: stmts,
        span: empty_span(),
    }
}

pub fn param<'src>(name: &'src str, t: TypeExpression<'src>) -> Parameter<'src> {
    Parameter {
        name: ident(name),
        type_: type_(t),
        span: empty_span(),
    }
}

pub fn go_imm<'src>(src: &'src str) -> Statement<'src> {
    expr_stmt(expr(Expr::GoImmediateSource {
        source: src,
    }))
}

pub fn string<'src>(str: &'src str) -> Expression<'src> {
    Expression {
        id: NodeId::DUMMY,
        variant: Box::new(Expr::StringLiteral(str)),
        span: empty_span(),
    }
}

pub fn fn_call<'src>(
    name: &'src str,
    args: Vec<Expression<'src>>
) -> Expression<'src> {
    expr(Expr::FunctionCall {
        name: ident(name),
        args: ExpressionList {
            list: args,
            span: empty_span(),
        },
    })
}


pub fn fn_def<'src>(
    name: &'src str,
    params: Vec<(&'src str, TypeExpression<'src>)>,
    return_type: TypeAnnotation<'src>,
    body: Vec<Statement<'src>>,
) -> Statement<'src> {
    stmt(Stmt::FunctionDefinition {
        name: ident(name),
        params: ParameterList {
            list: params
                .into_iter()
                .map(|(param_name, param_type)| param(param_name, param_type))
                .collect(),
            span: empty_span(),
        },
        body: block(body),
        return_type: return_type
    })
}

pub fn program<'src>(statements: Vec<Statement<'src>>) -> AstRoot<'src> {
    AstRoot {
        statements: statements
    }
}
