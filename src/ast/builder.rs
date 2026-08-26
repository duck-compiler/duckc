//! the builder is used for constructing AST nodes with empty spans, used when doing middle-/backend tests
//! these builders should never be used inside of parsers for constructing AST nodes

use crate::ast::{
    AstRoot, Block, Expression, Identifier, NodeId, Parameter, ParameterList, Span, Statement, TypeExpression, expression::{BinaryOperator, Expr, ExpressionList, FieldInit, UnaryOperator}, memory_target::{self, MemTar}, statement::Stmt, struct_definition::{ImplBlock, Method, MethodKind, StructField, Visibility}, type_expression::TypeAnnotation, use_statement::UseStatement
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

pub fn var_decl<'src>(
    name: &'src str,
    type_annotation: TypeAnnotation<'src>,
    init_expression: Option<Expression<'src>>,
) -> Statement<'src> {
    stmt(Stmt::VariableDeclaration {
        name: ident(name),
        type_: type_annotation,
        init_expression: init_expression,
    })
}

pub fn assign<'src>(
    memory_target: MemTar<'src>,
    value: Expression<'src>,
) -> Statement<'src> {
    stmt(Stmt::VariableAssignment {
        target: super::MemoryTarget { variant: memory_target, span: empty_span() },
        assign_expression: value
    })
}

pub fn return_stmt<'src>(value: Option<Expression<'src>>) -> Statement<'src> {
    stmt(Stmt::Return { value })
}

pub fn break_stmt<'src>() -> Statement<'src> {
    stmt(Stmt::Break)
}

pub fn continue_stmt<'src>() -> Statement<'src> {
    stmt(Stmt::Continue)
}

pub fn string<'src>(str: &'src str) -> Expression<'src> {
    Expression {
        id: NodeId::DUMMY,
        variant: Box::new(Expr::StringLiteral(str)),
        span: empty_span(),
    }
}

pub fn int<'src>(i: u64) -> Expression<'src> {
    Expression {
        id: NodeId::DUMMY,
        variant: Box::new(Expr::IntLiteral(i)),
        span: empty_span(),
    }
}

pub fn float<'src>(f: f64) -> Expression<'src> {
    expr(Expr::FloatLiteral(f))
}

pub fn bool_lit<'src>(b: bool) -> Expression<'src> {
    expr(Expr::BoolLiteral(b))
}

pub fn binary<'src>(left: Expression<'src>, op: BinaryOperator, right: Expression<'src>) -> Expression<'src> {
    expr(Expr::Binary {
        left: Box::new(left),
        op,
        right: Box::new(right),
    })
}

pub fn unary<'src>(op: UnaryOperator, value: Expression<'src>) -> Expression<'src> {
    expr(Expr::Unary {
        op,
        expr: Box::new(value),
    })
}

pub fn if_expr<'src>(condition: Expression<'src>, body: Vec<Statement<'src>>) -> Expression<'src> {
    expr(Expr::If {
        expr: Box::new(condition),
        body: block(body),
        else_branch: None,
    })
}

pub fn if_else_expr<'src>(
    condition: Expression<'src>,
    then_body: Vec<Statement<'src>>,
    else_body: Vec<Statement<'src>>,
) -> Expression<'src> {
    expr(Expr::If {
        expr: Box::new(condition),
        body: block(then_body),
        else_branch: Some(block(else_body)),
    })
}

pub fn while_expr<'src>(condition: Expression<'src>, body: Vec<Statement<'src>>) -> Expression<'src> {
    expr(Expr::While {
        expr: Box::new(condition),
        body: block(body),
    })
}

pub fn array<'src>(values: Vec<Expression<'src>>) -> Expression<'src> {
    expr(Expr::ArrayExpression {
        values_exprs: values.into_iter().map(Box::new).collect(),
    })
}

pub fn array_index<'src>(name: &'src str, index: Expression<'src>) -> Expression<'src> {
    expr(Expr::MemoryTarget(super::MemoryTarget {
        variant: MemTar::ArrayAccess {
            target: Box::new(super::MemoryTarget {
                variant: MemTar::Name(ident(name)),
                span: empty_span(),
            }),
            index_expression: Box::new(index),
        },
        span: empty_span(),
    }))
}

pub fn field_access<'src>(target: memory_target::MemTar<'src>, field: &'src str) -> Expression<'src> {
    expr(Expr::MemoryTarget(super::MemoryTarget {
        variant: MemTar::FieldAccess {
            target: Box::new(super::MemoryTarget {
                variant: target,
                span: empty_span(),
            }),
            field_name: ident(field),
        },
        span: empty_span(),
    }))
}

pub fn name_target<'src>(name: &'src str) -> memory_target::MemTar<'src> {
    MemTar::Name(ident(name))
}

pub fn dereference<'src>(value: Expression<'src>) -> Expression<'src> {
    expr(Expr::MemoryTarget(super::MemoryTarget {
        variant: deref_target(value),
        span: empty_span(),
    }))
}

pub fn deref_target<'src>(value: Expression<'src>) -> memory_target::MemTar<'src> {
    MemTar::Dereference(Box::new(value))
}

pub fn reference<'src>(value: Expression<'src>) -> Expression<'src> {
    expr(Expr::Reference {
        expr: Box::new(value),
    })
}

pub fn pointer_type<'src>(inner: TypeExpression<'src>) -> TypeExpression<'src> {
    TypeExpression::Pointer { inner: Box::new(inner) }
}

pub fn field_target<'src>(target: memory_target::MemTar<'src>, field: &'src str) -> memory_target::MemTar<'src> {
    MemTar::FieldAccess {
        target: Box::new(super::MemoryTarget {
            variant: target,
            span: empty_span(),
        }),
        field_name: ident(field),
    }
}

pub fn mem_name<'src>(name: &'src str) -> Expression<'src> {
    expr(Expr::MemoryTarget(super::MemoryTarget {
        variant: MemTar::Name(ident(name)),
        span: empty_span(),
    }))
}

pub fn call<'src>(
    target: Expression<'src>,
    args: Vec<Expression<'src>>
) -> Expression<'src> {
    expr(Expr::FunctionCall {
        target: Box::new(target),
        args: ExpressionList {
            list: args,
            span: empty_span(),
        },
    })
}

pub fn fn_call<'src>(
    name: &'src str,
    args: Vec<Expression<'src>>
) -> Expression<'src> {
    call(mem_name(name), args)
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

pub fn struct_field<'src>(
    visibility: Visibility,
    name: &'src str,
    field_type: TypeExpression<'src>,
) -> StructField<'src> {
    StructField {
        visibility,
        name: ident(name),
        type_: type_(field_type),
        span: empty_span(),
    }
}

pub fn pub_field<'src>(name: &'src str, field_type: TypeExpression<'src>) -> StructField<'src> {
    struct_field(Visibility::Public, name, field_type)
}

pub fn priv_field<'src>(name: &'src str, field_type: TypeExpression<'src>) -> StructField<'src> {
    struct_field(Visibility::Private, name, field_type)
}

fn struct_fields<'src>(
    visibility: Visibility,
    fields: Vec<(&'src str, TypeExpression<'src>)>,
) -> Vec<StructField<'src>> {
    fields
        .into_iter()
        .map(|(field_name, field_type)| struct_field(visibility, field_name, field_type))
        .collect()
}

pub fn struct_def<'src>(
    name: &'src str,
    fields: Vec<(&'src str, TypeExpression<'src>)>,
) -> Statement<'src> {
    struct_def_with_impl(name, struct_fields(Visibility::Private, fields), vec![])
}

pub fn pub_struct_def<'src>(
    name: &'src str,
    fields: Vec<(&'src str, TypeExpression<'src>)>,
) -> Statement<'src> {
    struct_def_with_impl(name, struct_fields(Visibility::Public, fields), vec![])
}

pub fn struct_def_with_impl<'src>(
    name: &'src str,
    fields: Vec<StructField<'src>>,
    methods: Vec<Method<'src>>,
) -> Statement<'src> {
    stmt(Stmt::StructDefinition {
        name: ident(name),
        fields,
        impl_block: match methods.is_empty() {
            true => None,
            false => Some(ImplBlock { methods, span: empty_span() }),
        },
    })
}

pub fn method<'src>(
    visibility: Visibility,
    kind: MethodKind,
    name: &'src str,
    params: Vec<(&'src str, TypeExpression<'src>)>,
    return_type: TypeAnnotation<'src>,
    body: Vec<Statement<'src>>,
) -> Method<'src> {
    Method {
        visibility,
        kind,
        name: ident(name),
        params: ParameterList {
            list: params
                .into_iter()
                .map(|(param_name, param_type)| param(param_name, param_type))
                .collect(),
            span: empty_span(),
        },
        return_type,
        body: block(body),
        span: empty_span(),
    }
}

pub fn pub_method<'src>(
    name: &'src str,
    params: Vec<(&'src str, TypeExpression<'src>)>,
    return_type: TypeAnnotation<'src>,
    body: Vec<Statement<'src>>,
) -> Method<'src> {
    method(Visibility::Public, MethodKind::Instance, name, params, return_type, body)
}

pub fn priv_method<'src>(
    name: &'src str,
    params: Vec<(&'src str, TypeExpression<'src>)>,
    return_type: TypeAnnotation<'src>,
    body: Vec<Statement<'src>>,
) -> Method<'src> {
    method(Visibility::Private, MethodKind::Instance, name, params, return_type, body)
}

pub fn pub_static_method<'src>(
    name: &'src str,
    params: Vec<(&'src str, TypeExpression<'src>)>,
    return_type: TypeAnnotation<'src>,
    body: Vec<Statement<'src>>,
) -> Method<'src> {
    method(Visibility::Public, MethodKind::Static, name, params, return_type, body)
}

pub fn struct_init<'src>(
    type_name: &'src str,
    fields: Vec<(&'src str, Expression<'src>)>,
) -> Expression<'src> {
    expr(Expr::StructInit {
        type_name: ident(type_name),
        fields: fields
            .into_iter()
            .map(|(field_name, value)| FieldInit {
                name: ident(field_name),
                value,
                span: empty_span(),
            })
            .collect(),
    })
}

pub fn use_stmt<'src>(path: &'src str, alias: Option<&'src str>) -> Statement<'src> {
    stmt(Stmt::Use(UseStatement {
        path: ident(path),
        alias: alias.map(ident),
        span: empty_span(),
    }))
}

pub fn field_call<'src>(
    module: &'src str,
    field: &'src str,
    args: Vec<Expression<'src>>
) -> Expression<'src> {
    expr(Expr::FunctionCall {
        target: Box::new(expr(Expr::MemoryTarget(super::MemoryTarget {
            variant: MemTar::FieldAccess {
                target: Box::new(super::MemoryTarget {
                    variant: MemTar::Name(ident(module)),
                    span: empty_span(),
                }),
                field_name: ident(field),
            },
            span: empty_span(),
        }))),
        args: ExpressionList {
            list: args,
            span: empty_span(),
        },
    })
}

pub fn program<'src>(statements: Vec<Statement<'src>>) -> AstRoot<'src> {
    AstRoot {
        statements: statements
    }
}
