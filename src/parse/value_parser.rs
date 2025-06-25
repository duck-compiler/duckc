use crate::parse::{
    Spanned,
    assignment_and_declaration_parser::{Assignment, Declaration},
    function_parser::{LambdaFunctionExpr, Param},
    source_file_parser::SourceFile,
    type_parser::type_expression_parser,
};

use super::{lexer::Token, type_parser::TypeExpr};
use chumsky::{input::BorrowInput, prelude::*};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    FunctionCall {
        target: Box<Spanned<ValueExpr>>,
        params: Vec<Spanned<ValueExpr>>,
    },
    Int(i64),
    String(String),
    Bool(bool),
    Float(f64),
    Char(char),
    Variable(String, Option<TypeExpr>),
    If {
        condition: Box<Spanned<ValueExpr>>,
        then: Box<Spanned<ValueExpr>>,
        r#else: Option<Box<Spanned<ValueExpr>>>,
    },
    While {
        condition: Box<Spanned<ValueExpr>>,
        body: Box<Spanned<ValueExpr>>,
    },
    Tuple(Vec<Spanned<ValueExpr>>),
    Block(Vec<Spanned<ValueExpr>>),
    Break,
    Continue,
    Duck(Vec<(String, Spanned<ValueExpr>)>),
    Struct(Vec<(String, Spanned<ValueExpr>)>),
    FieldAccess {
        target_obj: Box<Spanned<ValueExpr>>,
        field_name: String,
    },
    Return(Option<Box<Spanned<ValueExpr>>>),
    VarAssign(Box<Spanned<Assignment>>),
    VarDecl(Box<Spanned<Declaration>>),
    Add(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Mul(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    BoolNegate(Box<Spanned<ValueExpr>>),
    Equals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    InlineGo(String),
    Lambda(Box<LambdaFunctionExpr>),
}

pub trait IntoEmptySpan {
    fn into_empty_span(self) -> Spanned<ValueExpr>;
}

impl IntoEmptySpan for ValueExpr {
    fn into_empty_span(self) -> Spanned<ValueExpr> {
        (self, (0..1).into())
    }
}

pub trait IntoBlock {
    fn into_block(self) -> Spanned<ValueExpr>;
}

impl IntoBlock for Spanned<ValueExpr> {
    fn into_block(self) -> Spanned<ValueExpr> {
        let cl = self.1;
        (ValueExpr::Block(vec![self]), cl)
    }
}

pub trait Combi {
    fn into_empty_span_and_block(self) -> Spanned<ValueExpr>;
}

impl<T> Combi for T
where
    T: IntoEmptySpan,
{
    fn into_empty_span_and_block(self) -> Spanned<ValueExpr> {
        self.into_empty_span().into_block()
    }
}

impl ValueExpr {
    pub fn needs_semicolon(&self) -> bool {
        match self {
            ValueExpr::If {
                condition: _,
                then: _,
                r#else: _,
            } => false,
            ValueExpr::While {
                condition: _,
                body: _,
            } => false,
            ValueExpr::Block(_) => false,
            ValueExpr::InlineGo(_) => false,
            _ => true,
        }
    }
}

pub fn value_expr_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
    M: Fn(SimpleSpan, &'src [Spanned<Token>]) -> I + Clone + 'src,
{
    recursive(
        |value_expr_parser: Recursive<
            dyn Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token>>>,
        >| {
            let lambda_parser = {
                let param_parser =
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .then_ignore(just(Token::ControlChar(':')))
                        .then(type_expression_parser())
                        .map(|(identifier, type_expr)| (identifier, type_expr) as Param)
                        .boxed();

                let params_parser = param_parser
                    .separated_by(just(Token::ControlChar(',')))
                    .allow_trailing()
                    .collect::<Vec<Param>>()
                    .or_not()
                    .boxed();

                let return_type_parser = just(Token::ControlChar('-'))
                    .ignore_then(just(Token::ControlChar('>')))
                    .ignore_then(type_expression_parser())
                    .boxed();

                just(Token::ControlChar('('))
                    .ignore_then(params_parser)
                    .then_ignore(just(Token::ControlChar(')')))
                    .then(return_type_parser.or_not())
                    .then_ignore(just(Token::ControlChar('=')))
                    .then_ignore(just(Token::ControlChar('>')))
                    .then(value_expr_parser.clone())
                    .map(|((params, return_type), value_expr)| {
                        ValueExpr::Lambda(
                            LambdaFunctionExpr {
                                params: params.unwrap_or_default(),
                                return_type,
                                value_expr,
                            }
                            .into(),
                        )
                    })
                    .boxed()
            };

            let params = value_expr_parser
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .boxed();

            let tuple = lambda_parser
                .clone()
                .or((just(Token::ControlChar('('))
                    .ignore_then(just(Token::ControlChar(')')))
                    .to(ValueExpr::Tuple(vec![])))
                .or(value_expr_parser
                    .clone()
                    .separated_by(just(Token::ControlChar(',')))
                    .at_least(1)
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .map(ValueExpr::Tuple)))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let initializer = just(Token::ControlChar('='))
                .ignore_then(value_expr_parser.clone())
                .or_not()
                .boxed();

            let declaration = just(Token::Let)
                .ignore_then(
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .map_with(|x, e| (x, e.span())),
                )
                .then_ignore(just(Token::ControlChar(':')))
                .then(type_expression_parser())
                .then(initializer)
                .map(|(((ident, ident_span), type_expr), initializer)| {
                    ValueExpr::VarDecl(
                        (
                            Declaration {
                                name: ident,
                                type_expr,
                                initializer: initializer.clone(),
                            },
                            initializer.map(|x| x.1).unwrap_or(ident_span),
                        )
                            .into(),
                    )
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let struct_expression = just(Token::ControlChar('.'))
                .ignore_then(
                    select_ref! { Token::Ident(ident) => ident.to_owned() }
                        .then_ignore(just(Token::ControlChar(':')))
                        .then(value_expr_parser.clone())
                        .separated_by(just(Token::ControlChar(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                        .map(ValueExpr::Struct),
                )
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(value_expr_parser.clone())
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                .map(|mut x| {
                    x.sort_by_key(|(name, _)| name.clone());
                    ValueExpr::Duck(x)
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let block_expression = value_expr_parser
                .clone()
                .then(just(Token::ControlChar(';')).or_not())
                .repeated()
                .collect::<Vec<_>>()
                .map_err(|error| {
                    dbg!(error);
                    todo!()
                })
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                .map_with(|mut exprs, e| {
                    if exprs.len() >= 2 {
                        for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                            if expr.0.needs_semicolon() && has_semi.is_none() {
                                panic!("needs_semi")
                            }
                        }
                    }

                    if exprs.is_empty() || exprs.last().unwrap().1.is_some() {
                        exprs.push((empty_tuple().into_empty_span(), None));
                    }

                    (
                        ValueExpr::Block(exprs.into_iter().map(|(expr, _)| expr).collect()),
                        e.span(),
                    )
                })
                .boxed();

            let if_condition = value_expr_parser
                .clone()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .boxed();
            let if_body = block_expression.clone();
            let if_with_condition_and_body = just(Token::If)
                .ignore_then(if_condition.clone())
                .then(if_body.clone())
                .boxed();

            let while_condition = if_condition.clone();
            let while_body = block_expression.clone();
            let while_with_condition_and_body = just(Token::While)
                .ignore_then(while_condition.clone())
                .then(while_body.clone())
                .boxed();

            let field_access = any_ref()
                .filter(|t| !matches!(t, Token::ControlChar('.')))
                .map_with(|x, e| (x, e.span()))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .then(
                    (just(Token::ControlChar('.')).ignore_then(
                        select_ref! { Token::Ident(field_name) => field_name.to_owned() }
                            .or(select_ref! { Token::IntLiteral(i) => i.to_string() }),
                    ))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
                )
                .map({
                    let e = value_expr_parser.clone();
                    move |(base_expr, field_accesses)| {
                        let base_expr = base_expr
                            .clone()
                            .into_iter()
                            .map(|x| (x.0.to_owned(), x.1) as Spanned<Token>)
                            .collect::<Vec<_>>()
                            .leak() as &[Spanned<Token>];

                        let base = e
                            .parse(make_input(
                                (base_expr.first().unwrap().1.start
                                    ..base_expr.last().unwrap().1.end)
                                    .into(),
                                base_expr,
                            ))
                            .unwrap();

                        field_accesses.into_iter().fold(base, |acc, x| {
                            (
                                ValueExpr::FieldAccess {
                                    target_obj: acc.clone().into(),
                                    field_name: x,
                                },
                                acc.1,
                            )
                        })
                    }
                })
                .map_with(|x, e| (x.0, e.span()))
                .boxed();

            let int = select_ref! { Token::IntLiteral(i) => *i }
                .map(ValueExpr::Int)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let bool_val = select_ref! { Token::BoolLiteral(b) => *b }
                .map(ValueExpr::Bool)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let string_val = select_ref! { Token::StringLiteral(s) => s.to_owned() }
                .map(ValueExpr::String)
                .map_with(|x, e| (x, e.span()));
            let var_expr = select_ref! { Token::Ident(ident) => ident.to_owned() }
                .map(|ident| ValueExpr::Variable(ident, None))
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let if_expr = if_with_condition_and_body
                .clone()
                .then(
                    just(Token::Else)
                        .ignore_then(if_with_condition_and_body.clone())
                        .repeated()
                        .collect::<Vec<(Spanned<ValueExpr>, Spanned<ValueExpr>)>>(),
                )
                .then(just(Token::Else).ignore_then(if_body.clone()).or_not())
                .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    r#else: else_ifs.into_iter().rfold(
                        r#else.map(Box::new),
                        |acc, (cond, then)| {
                            Some(Box::new(
                                ValueExpr::If {
                                    condition: Box::new(cond),
                                    then: Box::new(then),
                                    r#else: Some(acc.unwrap()),
                                }
                                .into_empty_span(),
                            ))
                        },
                    ),
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let char_expr = select_ref! { Token::CharLiteral(c) => *c }
                .map(ValueExpr::Char)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let float_expr = select_ref! { Token::FloatLiteral(num) => *num }
                .map(ValueExpr::Float)
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let atom = just(Token::ControlChar('!'))
                .repeated()
                .collect::<Vec<_>>()
                .then(
                    field_access.clone().or(value_expr_parser
                        .clone()
                        .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                        .or(choice((
                            field_access.clone(),
                            int,
                            bool_val,
                            string_val,
                            var_expr,
                            if_expr,
                            char_expr,
                            float_expr,
                            tuple,
                            duck_expression,
                            struct_expression,
                            block_expression,
                            just(Token::Break)
                                .to(ValueExpr::Break)
                                .map_with(|x, e| (x, e.span())),
                            just(Token::Continue)
                                .to(ValueExpr::Continue)
                                .map_with(|x, e| (x, e.span())),
                            while_with_condition_and_body
                                .clone()
                                .map(|(cond, body)| ValueExpr::While {
                                    condition: Box::new(cond),
                                    body: Box::new(body),
                                })
                                .map_with(|x, e| (x, e.span())),
                        )))),
                )
                .then(params.clone().or_not())
                .map(|((neg, target), params)| {
                    let res = if let Some(params) = params {
                        ValueExpr::FunctionCall {
                            target: target.into(),
                            params,
                        }
                    } else {
                        target.0
                    };

                    neg.into_iter().fold(res, |acc, _| {
                        ValueExpr::BoolNegate(acc.into_empty_span().into())
                    })
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let assignment = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar('=')))
                .then(value_expr_parser.clone())
                .map(|(identifier, value_expr)| {
                    ValueExpr::VarAssign(
                        (
                            Assignment {
                                name: identifier,
                                value_expr,
                            },
                            (0..1).into(),
                        )
                            .into(),
                    )
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            //

            let prod = atom
                .clone()
                .then(
                    just(Token::ControlChar('*'))
                        .ignore_then(atom.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, x| {
                        (ValueExpr::Mul(acc.into(), x.clone().into()), x.1)
                    })
                })
                .map_with(|x, e| (x.0, e.span()))
                .boxed();

            let add = prod
                .clone()
                .then(
                    just(Token::ControlChar('+'))
                        .ignore_then(prod.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, x| {
                        (ValueExpr::Add(acc.into(), x.clone().into()), x.1)
                    })
                })
                .map_with(|x, e| (x.0, e.span()))
                .boxed();

            let equals = add
                .clone()
                .then_ignore(just(Token::Equals))
                .then(add.clone())
                .map(|(x, y)| ValueExpr::Equals(x.into(), y.into()))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }
                .map(ValueExpr::InlineGo)
                .map_with(|x, e| (x, e.span()))
                .boxed();

            choice((
                inline_go,
                assignment,
                equals,
                add,
                declaration,
                just(Token::Return)
                    .ignore_then(value_expr_parser.clone().or_not())
                    .map_with(|x: Option<Spanned<ValueExpr>>, e| {
                        (ValueExpr::Return(x.map(Box::new)), e.span())
                    }),
                atom,
            ))
            .labelled("expression")
            .boxed()
        },
    )
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

#[allow(dead_code)]
fn empty_duck() -> ValueExpr {
    ValueExpr::Duck(Vec::new())
}

pub fn empty_range() -> SimpleSpan {
    (0..1).into()
}

pub fn source_file_into_empty_range(v: &mut SourceFile) {
    for x in &mut v.function_definitions {
        all_into_empty_range(&mut x.value_expr);
    }
    for x in &mut v.sub_modules {
        source_file_into_empty_range(&mut x.1);
    }
}

pub fn all_into_empty_range(v: &mut Spanned<ValueExpr>) {
    v.1 = empty_range();
    match &mut v.0 {
        ValueExpr::FunctionCall { target, params } => {
            all_into_empty_range(target);
            for p in params {
                all_into_empty_range(p);
            }
        }
        ValueExpr::Add(v1, v2) => {
            all_into_empty_range(v1);
            all_into_empty_range(v2);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            all_into_empty_range(condition);
            all_into_empty_range(then);
            if let Some(r#else) = r#else {
                all_into_empty_range(r#else);
            }
        }
        ValueExpr::Mul(v1, v2) => {
            all_into_empty_range(v1);
            all_into_empty_range(v2);
        }
        ValueExpr::Duck(fields) => {
            for field in fields {
                all_into_empty_range(&mut field.1);
            }
        }
        ValueExpr::Block(exprs) => {
            for expr in exprs {
                all_into_empty_range(expr);
            }
        }
        ValueExpr::Tuple(fields) => {
            for field in fields {
                all_into_empty_range(field);
            }
        }
        ValueExpr::Equals(v1, v2) => {
            all_into_empty_range(v1);
            all_into_empty_range(v2);
        }
        ValueExpr::Lambda(b) => {
            all_into_empty_range(&mut b.value_expr);
        }
        ValueExpr::Return(Some(v)) => all_into_empty_range(v),
        ValueExpr::Struct(fields) => {
            for field in fields {
                all_into_empty_range(&mut field.1);
            }
        }
        ValueExpr::While { condition, body } => {
            all_into_empty_range(condition);
            all_into_empty_range(body);
        }
        ValueExpr::VarDecl(b) => {
            b.1 = empty_range();
            if let Some(init) = &mut b.0.initializer {
                all_into_empty_range(init);
            }
        }
        ValueExpr::BoolNegate(b) => all_into_empty_range(b),
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            all_into_empty_range(target_obj);
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::parse::{
        Spanned,
        assignment_and_declaration_parser::Declaration,
        function_parser::LambdaFunctionExpr,
        lexer::lexer,
        make_input,
        type_parser::{Duck, Field, TypeExpr},
        value_parser::{
            Combi, IntoEmptySpan, all_into_empty_range, empty_duck, empty_tuple, value_expr_parser,
        },
    };

    use super::ValueExpr;

    fn var(x: impl Into<String>) -> Box<Spanned<ValueExpr>> {
        ValueExpr::Variable(x.into(), None).into_empty_span().into()
    }

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
            (
                ".{ x: 5 }",
                ValueExpr::Struct(vec![("x".to_string(), ValueExpr::Int(5).into_empty_span())]),
            ),
            (
                ".{ x: 5, y: .{ x: 5 } }",
                ValueExpr::Struct(vec![
                    ("x".to_string(), ValueExpr::Int(5).into_empty_span()),
                    (
                        "y".to_string(),
                        ValueExpr::Struct(vec![(
                            "x".to_string(),
                            ValueExpr::Int(5).into_empty_span(),
                        )])
                        .into_empty_span(),
                    ),
                ]),
            ),
            ("{}", empty_duck()),
            (
                "to_upper()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper(1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper(1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper ()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper (1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper (1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper (   )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper ( 1 )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                },
            ),
            (
                "to_upper ( to_lower(1,2,add(5, 10),4), true  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![
                        ValueExpr::FunctionCall {
                            target: var("to_lower"),
                            params: vec![
                                ValueExpr::Int(1).into_empty_span(),
                                ValueExpr::Int(2).into_empty_span(),
                                ValueExpr::FunctionCall {
                                    target: var("add"),
                                    params: vec![
                                        ValueExpr::Int(5).into_empty_span(),
                                        ValueExpr::Int(10).into_empty_span(),
                                    ],
                                }
                                .into_empty_span(),
                                ValueExpr::Int(4).into_empty_span(),
                            ],
                        }
                        .into_empty_span(),
                        ValueExpr::Bool(true).into_empty_span(),
                    ],
                },
            ),
            (
                "print(\"hallo\", \"moin\")",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::String("hallo".into()).into_empty_span(),
                        ValueExpr::String("moin".into()).into_empty_span(),
                    ],
                },
            ),
            ("x", ValueExpr::Variable("x".into(), None)),
            (
                "print(x, true, lol())",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::Variable("x".into(), None).into_empty_span(),
                        ValueExpr::Bool(true).into_empty_span(),
                        ValueExpr::FunctionCall {
                            target: var("lol"),
                            params: vec![],
                        }
                        .into_empty_span(),
                    ],
                },
            ),
            (
                "if (true) { 1 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Int(1).into_empty_span_and_block().into(),
                    r#else: Some(ValueExpr::Int(2).into_empty_span_and_block().into()),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Int(1).into_empty_span_and_block().into(),
                    r#else: Some(
                        ValueExpr::If {
                            condition: ValueExpr::Bool(false).into_empty_span().into(),
                            then: ValueExpr::Int(3).into_empty_span_and_block().into(),
                            r#else: Some(
                                ValueExpr::If {
                                    condition: ValueExpr::Int(200).into_empty_span().into(),
                                    then: ValueExpr::Int(4).into_empty_span_and_block().into(),
                                    r#else: Some(
                                        ValueExpr::Int(2).into_empty_span_and_block().into(),
                                    ),
                                }
                                .into_empty_span()
                                .into(),
                            ),
                        }
                        .into_empty_span()
                        .into(),
                    ),
                },
            ),
            (
                "(1,true,2,\"hallo\")",
                ValueExpr::Tuple(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Bool(true).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::String("hallo".into()).into_empty_span(),
                ]),
            ),
            ("{}", empty_duck()),
            (
                "{1}",
                ValueExpr::Block(vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            (
                "{1;  2   ;3;x()}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::Int(3).into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span(),
                ]),
            ),
            (
                "{1;  2   ;3;x({})}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::Int(3).into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![empty_duck().into_empty_span()],
                    }
                    .into_empty_span(),
                ]),
            ),
            (
                "{x();y();}",
                ValueExpr::Block(vec![
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into_empty_span(),
                    empty_tuple().into_empty_span(),
                ]),
            ),
            (
                "x({ 1; 2; y({ z(); }) }, lol)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            ValueExpr::Int(1).into_empty_span(),
                            ValueExpr::Int(2).into_empty_span(),
                            ValueExpr::FunctionCall {
                                target: var("y"),
                                params: vec![
                                    ValueExpr::Block(vec![
                                        ValueExpr::FunctionCall {
                                            target: var("z"),
                                            params: vec![],
                                        }
                                        .into_empty_span(),
                                        empty_tuple().into_empty_span(),
                                    ])
                                    .into_empty_span(),
                                ],
                            }
                            .into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::Variable("lol".into(), None).into_empty_span(),
                    ],
                },
            ),
            (
                "while (true) {}",
                ValueExpr::While {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    body: empty_tuple().into_empty_span_and_block().into(),
                },
            ),
            (
                "while (my_func()) {}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    body: empty_tuple().into_empty_span_and_block().into(),
                },
            ),
            (
                "while (my_func()) {1;break;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Break.into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                },
            ),
            (
                "while (my_func()) {1;continue;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Continue.into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                },
            ),
            ("()", empty_tuple()),
            (
                "(1.1, 'x')",
                ValueExpr::Tuple(vec![
                    ValueExpr::Float(1.1).into_empty_span(),
                    ValueExpr::Char('x').into_empty_span(),
                ]),
            ),
            (
                "{x: 1, y: { z: true }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1).into_empty_span()),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![(
                            "z".into(),
                            ValueExpr::Bool(true).into_empty_span(),
                        )])
                        .into_empty_span(),
                    ),
                ]),
            ),
            (
                "{x: 1, y: { z: true, w: { print();2;true } }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1).into_empty_span()),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![
                            (
                                "w".into(),
                                ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("print"),
                                        params: vec![],
                                    }
                                    .into_empty_span(),
                                    ValueExpr::Int(2).into_empty_span(),
                                    ValueExpr::Bool(true).into_empty_span(),
                                ])
                                .into_empty_span(),
                            ),
                            ("z".into(), ValueExpr::Bool(true).into_empty_span()),
                        ])
                        .into_empty_span(),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Duck(vec![]).into_empty_span_and_block().into(),
                    r#else: Some(
                        ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1).into_empty_span())])
                            .into_empty_span_and_block()
                            .into(),
                    ),
                },
            ),
            (
                "x.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Variable("x".into(), None)
                        .into_empty_span()
                        .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "{x: 123}.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![(
                        "x".into(),
                        ValueExpr::Int(123).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "x().y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "(x)()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                },
            ),
            (
                "x()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                },
            ),
            (
                "(1)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(1).into_empty_span().into(),
                    params: vec![],
                },
            ),
            (
                "(123)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(123).into_empty_span().into(),
                    params: vec![],
                },
            ),
            (
                "(returns_lambda())()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FunctionCall {
                        target: var("returns_lambda"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    params: vec![],
                },
            ),
            (
                "x.y.z.w",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "w".into(),
                },
            ),
            ("((1))", ValueExpr::Int(1)),
            (
                "x({();();},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple().into_empty_span(),
                            empty_tuple().into_empty_span(),
                            empty_tuple().into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                    ],
                },
            ),
            (
                "x({();{();1;};},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple().into_empty_span(),
                            ValueExpr::Block(vec![
                                empty_tuple().into_empty_span(),
                                ValueExpr::Int(1).into_empty_span(),
                                empty_tuple().into_empty_span(),
                            ])
                            .into_empty_span(),
                            empty_tuple().into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                    ],
                },
            ),
            (
                "return 123",
                ValueExpr::Return(Some(Box::new(ValueExpr::Int(123).into_empty_span().into()))),
            ),
            (
                "let x: String",
                ValueExpr::VarDecl(
                    (
                        Declaration {
                            name: "x".into(),
                            initializer: None,
                            type_expr: TypeExpr::String,
                        },
                        (0..1).into(),
                    )
                        .into(),
                ),
            ),
            (
                "x() * y()",
                ValueExpr::Mul(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "3 * 5",
                ValueExpr::Mul(
                    ValueExpr::Int(3).into_empty_span().into(),
                    ValueExpr::Int(5).into_empty_span().into(),
                ),
            ),
            (
                "3 + 5",
                ValueExpr::Add(
                    ValueExpr::Int(3).into_empty_span().into(),
                    ValueExpr::Int(5).into_empty_span().into(),
                ),
            ),
            (
                "3 * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(
                        ValueExpr::Int(3).into_empty_span().into(),
                        ValueExpr::Int(5).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(6).into_empty_span().into(),
                ),
            ),
            (
                "x() * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                        }
                        .into_empty_span()
                        .into(),
                        ValueExpr::Int(5).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(6).into_empty_span().into(),
                ),
            ),
            (
                "!true",
                ValueExpr::BoolNegate(ValueExpr::Bool(true).into_empty_span().into()),
            ),
            (
                "!{1;2;true}",
                ValueExpr::BoolNegate(
                    ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Int(2).into_empty_span(),
                        ValueExpr::Bool(true).into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                        }
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FieldAccess {
                            target_obj: ValueExpr::FieldAccess {
                                target_obj: ValueExpr::Variable("x".into(), None)
                                    .into_empty_span()
                                    .into(),
                                field_name: "y".into(),
                            }
                            .into_empty_span()
                            .into(),
                            field_name: "z".into(),
                        }
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: ValueExpr::Variable("x".into(), None)
                                .into_empty_span()
                                .into(),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "x() == y()",
                ValueExpr::Equals(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "1 == 2",
                ValueExpr::Equals(
                    ValueExpr::Int(1).into_empty_span().into(),
                    ValueExpr::Int(2).into_empty_span().into(),
                ),
            ),
            (
                "!(1 == 2)",
                ValueExpr::BoolNegate(
                    ValueExpr::Equals(
                        ValueExpr::Int(1).into_empty_span().into(),
                        ValueExpr::Int(2).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!1 == !2",
                ValueExpr::Equals(
                    ValueExpr::BoolNegate(ValueExpr::Int(1).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                    ValueExpr::BoolNegate(ValueExpr::Int(2).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                ),
            ),
            ("go {}", ValueExpr::InlineGo(String::new())),
            (
                "go { go func() {} }",
                ValueExpr::InlineGo(String::from(" go func() {} ")),
            ),
            (
                "() => {}",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: None,
                        value_expr: ValueExpr::Duck(vec![]).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "() => 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: None,
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "() -> Int => 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: Some(TypeExpr::Int),
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "(x: String) -> Int => 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![("x".into(), TypeExpr::String)],
                        return_type: Some(TypeExpr::Int),
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "{x: 1}.x",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![(
                        "x".into(),
                        ValueExpr::Int(1).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                    field_name: "x".into(),
                },
            ),
            (
                "(1,(3,4),\"s\").0",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Tuple(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Tuple(vec![
                            ValueExpr::Int(3).into_empty_span(),
                            ValueExpr::Int(4).into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::String("s".into()).into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                    field_name: "0".into(),
                },
            ),
        ];

        for (i, (src, expected_tokens)) in test_cases.into_iter().enumerate() {
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result =
                value_expr_parser(make_input).parse(make_input((0..src.len()).into(), &lex_result));

            assert_eq!(parse_result.has_errors(), false, "{i}: {}", src);
            assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

            let mut output = parse_result.into_result().expect(&src);
            all_into_empty_range(&mut output);

            assert_eq!(output.0, expected_tokens, "{i}: {}", src);
        }
    }

    #[test]
    pub fn test_declaration_parser() {
        let inputs_and_expected_outputs = vec![
            (
                "let x: String",
                Declaration {
                    name: "x".to_string(),
                    type_expr: TypeExpr::String,
                    initializer: None,
                },
            ),
            (
                "let y: { x: Int } = {}",
                Declaration {
                    name: "y".to_string(),
                    type_expr: TypeExpr::Duck(Duck {
                        fields: vec![Field::new("x".to_string(), TypeExpr::Int)],
                    }),
                    initializer: Some(ValueExpr::Duck(vec![]).into_empty_span()),
                },
            ),
            (
                "let z: {}",
                Declaration {
                    name: "z".to_string(),
                    type_expr: TypeExpr::Duck(Duck { fields: vec![] }),
                    initializer: None,
                },
            ),
        ];

        for (input, expected_output) in inputs_and_expected_outputs {
            let lexer_parse_result = lexer().parse(input);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            let declaration_parse_result =
                value_expr_parser(make_input).parse(make_input((1..10).into(), tokens.as_slice()));
            assert_eq!(declaration_parse_result.has_errors(), false);
            assert_eq!(declaration_parse_result.has_output(), true);

            let Some((ValueExpr::VarDecl(mut declaration), _)) =
                declaration_parse_result.into_output()
            else {
                unreachable!()
            };

            declaration.0.initializer.as_mut().map(|x| {
                all_into_empty_range(x);
                x
            });

            assert_eq!(declaration.0, expected_output.into());
        }

        let valid_declarations = vec![
            "let x: String",
            "let x: { x: String, y: String }",
            "let y: { x: String, y: String }",
            "let z: { h: String, x: { y: String }}",
            "let x: { h: String, x: { y: String }} = 0",
            "let x: { h: String, x: { y: String }} = true",
            "let x: { h: String, x: { y: String }} = false",
            "let x: { h: Int, x: { y: Int }} = { h: 4, x: { y: 8 } }",
            "let x: Int = false",
            "let x: String = \"Hallo, Welt!\"",
            "let x: go sync.WaitGroup",
        ];

        for valid_declaration in valid_declarations {
            println!("lexing {valid_declaration}");
            let lexer_parse_result = lexer().parse(valid_declaration);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("declaration parsing {valid_declaration}");
            let typedef_parse_result =
                value_expr_parser(make_input).parse(make_input((1..10).into(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    pub fn test_assignment_parser() {
        let valid_assignments = vec![
            "y = 1",
            "{y = 1}",
            "while(true){y = 1}",
            "while(true){y = y + 1}",
            "{let y: Int = 0; while(true){y = 1;println(y)}}",
            "x = 580",
            "y = 80",
            "y = true",
            "y = false",
            "y = \"Hallo\"",
        ];

        for valid_assignment in valid_assignments {
            let lexer_parse_result = lexer().parse(valid_assignment);
            assert_eq!(lexer_parse_result.has_errors(), false, "{valid_assignment}");
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_assignment}");
            let typedef_parse_result =
                value_expr_parser(make_input).parse(make_input((1..10).into(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }
}
