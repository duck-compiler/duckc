use crate::parse::{
    Spanned,
    assignment_and_declaration_parser::{Assignment, Declaration},
    function_parser::{LambdaFunctionExpr, Param},
    make_input,
    type_parser::type_expression_parser,
};

use super::{lexer::Token, type_parser::TypeExpr};
use chumsky::{input::BorrowInput, prelude::*};

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    FunctionCall {
        target: Box<ValueExpr>,
        params: Vec<ValueExpr>,
    },
    Int(i64),
    String(String),
    Bool(bool),
    Float(f64),
    Char(char),
    Variable(String, Option<TypeExpr>),
    If {
        condition: Box<ValueExpr>,
        then: Box<ValueExpr>,
        r#else: Option<Box<ValueExpr>>,
    },
    While {
        condition: Box<ValueExpr>,
        body: Box<ValueExpr>,
    },
    Tuple(Vec<ValueExpr>),
    Block(Vec<ValueExpr>),
    Break,
    Continue,
    Duck(Vec<(String, ValueExpr)>),
    Struct(Vec<(String, ValueExpr)>),
    FieldAccess {
        target_obj: Box<ValueExpr>,
        field_name: String,
    },
    Return(Option<Box<ValueExpr>>),
    VarAssign(Box<Assignment>),
    VarDecl(Box<Declaration>),
    Add(Box<ValueExpr>, Box<ValueExpr>),
    Mul(Box<ValueExpr>, Box<ValueExpr>),
    BoolNegate(Box<ValueExpr>),
    Equals(Box<ValueExpr>, Box<ValueExpr>),
    InlineGo(String),
    Lambda(Box<LambdaFunctionExpr>),
}

impl ValueExpr {
    pub fn into_block(self) -> ValueExpr {
        ValueExpr::Block(vec![self])
    }

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
) -> impl Parser<'src, I, ValueExpr, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
    M: Fn(SimpleSpan, &'src [Spanned<Token>]) -> I + Clone + 'src,
{
    recursive(
        |value_expr_parser: Recursive<
            dyn Parser<'src, I, ValueExpr, extra::Err<Rich<'src, Token>>>,
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
                    .map(|x| ValueExpr::Tuple(x))))
                .boxed();

            let initializer = just(Token::ControlChar('='))
                .ignore_then(value_expr_parser.clone())
                .or_not()
                .boxed();

            let declaration = just(Token::Let)
                .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
                .then_ignore(just(Token::ControlChar(':')))
                .then(type_expression_parser())
                .then(initializer)
                .map(|((identifier, type_expr), initializer)| {
                    ValueExpr::VarDecl(
                        Declaration {
                            name: identifier,
                            type_expr,
                            initializer,
                        }
                        .into(),
                    )
                })
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
                .map(|mut exprs| {
                    if exprs.len() >= 2 {
                        for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                            if expr.needs_semicolon() && has_semi.is_none() {
                                panic!("needs_semi")
                            }
                        }
                    }

                    if exprs.is_empty() || exprs.last().unwrap().1.is_some() {
                        exprs.push((empty_tuple(), None));
                    }

                    ValueExpr::Block(exprs.into_iter().map(|(expr, _)| expr).collect())
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
                            .map(|x| {
                                (x.to_owned(), SimpleSpan::from(0_usize..10_usize))
                                    as Spanned<Token>
                            })
                            .collect::<Vec<_>>()
                            .leak() as &[Spanned<Token>];

                        let base = e.parse(make_input((1..10).into(), base_expr)).unwrap();

                        field_accesses
                            .into_iter()
                            .fold(base, |acc, x| ValueExpr::FieldAccess {
                                target_obj: acc.into(),
                                field_name: x,
                            })
                    }
                })
                .boxed();

            let int = select_ref! { Token::IntLiteral(i) => *i }
                .map(ValueExpr::Int)
                .boxed();
            let bool_val = select_ref! { Token::BoolLiteral(b) => *b }
                .map(ValueExpr::Bool)
                .boxed();
            let string_val =
                select_ref! { Token::StringLiteral(s) => s.to_owned() }.map(ValueExpr::String);
            let var_expr = select_ref! { Token::Ident(ident) => ident.to_owned() }
                .map(|ident| ValueExpr::Variable(ident, None))
                .boxed();
            let if_expr = if_with_condition_and_body
                .clone()
                .then(
                    just(Token::Else)
                        .ignore_then(if_with_condition_and_body.clone())
                        .repeated()
                        .collect::<Vec<(ValueExpr, ValueExpr)>>(),
                )
                .then(just(Token::Else).ignore_then(if_body.clone()).or_not())
                .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    r#else: else_ifs.into_iter().rfold(
                        r#else.map(Box::new),
                        |acc, (cond, then)| {
                            Some(Box::new(ValueExpr::If {
                                condition: Box::new(cond),
                                then: Box::new(then),
                                r#else: Some(acc.unwrap()),
                            }))
                        },
                    ),
                })
                .boxed();
            let char_expr = select_ref! { Token::CharLiteral(c) => *c }
                .map(ValueExpr::Char)
                .boxed();
            let float_expr = select_ref! { Token::FloatLiteral(num) => *num }
                .map(ValueExpr::Float)
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
                            just(Token::Break).to(ValueExpr::Break),
                            just(Token::Continue).to(ValueExpr::Continue),
                            while_with_condition_and_body.clone().map(|(cond, body)| {
                                ValueExpr::While {
                                    condition: Box::new(cond),
                                    body: Box::new(body),
                                }
                            }),
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
                        target
                    };

                    neg.into_iter()
                        .fold(res, |acc, _| ValueExpr::BoolNegate(acc.into()))
                })
                .boxed();

            let assignment = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar('=')))
                .then(value_expr_parser.clone())
                .map(|(identifier, value_expr)| {
                    ValueExpr::VarAssign(
                        Assignment {
                            name: identifier,
                            value_expr,
                        }
                        .into(),
                    )
                })
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
                    additional
                        .into_iter()
                        .fold(init, |acc, x| ValueExpr::Mul(acc.into(), x.into()))
                })
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
                    additional
                        .into_iter()
                        .fold(init, |acc, x| ValueExpr::Add(acc.into(), x.into()))
                })
                .boxed();

            let equals = add
                .clone()
                .then_ignore(just(Token::Equals))
                .then(add.clone())
                .map(|(x, y)| ValueExpr::Equals(x.into(), y.into()))
                .boxed();

            let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }
                .map(ValueExpr::InlineGo)
                .boxed();

            choice((
                inline_go,
                assignment,
                equals,
                add,
                declaration,
                just(Token::Return)
                    .ignore_then(value_expr_parser.clone().or_not())
                    .map(|x: Option<ValueExpr>| ValueExpr::Return(x.map(Box::new))),
                atom,
            ))
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

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::parse::{
        assignment_and_declaration_parser::Declaration,
        function_parser::LambdaFunctionExpr,
        lexer::lexer,
        make_input,
        type_parser::{Duck, TypeExpr},
        value_parser::{empty_duck, empty_tuple, value_expr_parser},
    };

    use super::ValueExpr;

    fn var(x: impl Into<String>) -> Box<ValueExpr> {
        ValueExpr::Variable(x.into(), None).into()
    }

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
            (
                ".{ x: 5 }",
                ValueExpr::Struct(vec![("x".to_string(), ValueExpr::Int(5))]),
            ),
            (
                ".{ x: 5, y: .{ x: 5 } }",
                ValueExpr::Struct(vec![
                    ("x".to_string(), ValueExpr::Int(5)),
                    (
                        "y".to_string(),
                        ValueExpr::Struct(vec![("x".to_string(), ValueExpr::Int(5))]),
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
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper(1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
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
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper (1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
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
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1)],
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
                                ValueExpr::Int(1),
                                ValueExpr::Int(2),
                                ValueExpr::FunctionCall {
                                    target: var("add"),
                                    params: vec![ValueExpr::Int(5), ValueExpr::Int(10)],
                                },
                                ValueExpr::Int(4),
                            ],
                        },
                        ValueExpr::Bool(true),
                    ],
                },
            ),
            (
                "print(\"hallo\", \"moin\")",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::String("hallo".into()),
                        ValueExpr::String("moin".into()),
                    ],
                },
            ),
            ("x", ValueExpr::Variable("x".into(), None)),
            (
                "print(x, true, lol())",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::Variable("x".into(), None),
                        ValueExpr::Bool(true),
                        ValueExpr::FunctionCall {
                            target: var("lol"),
                            params: vec![],
                        },
                    ],
                },
            ),
            (
                "if (true) { 1 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into_block().into(),
                    r#else: Some(ValueExpr::Int(2).into_block().into()),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into_block().into(),
                    r#else: Some(
                        ValueExpr::If {
                            condition: ValueExpr::Bool(false).into(),
                            then: ValueExpr::Int(3).into_block().into(),
                            r#else: Some(
                                ValueExpr::If {
                                    condition: ValueExpr::Int(200).into(),
                                    then: ValueExpr::Int(4).into_block().into(),
                                    r#else: Some(ValueExpr::Int(2).into_block().into()),
                                }
                                .into(),
                            ),
                        }
                        .into(),
                    ),
                },
            ),
            (
                "(1,true,2,\"hallo\")",
                ValueExpr::Tuple(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Bool(true),
                    ValueExpr::Int(2),
                    ValueExpr::String("hallo".into()),
                ]),
            ),
            ("{}", empty_duck()),
            ("{1}", ValueExpr::Int(1).into_block()),
            (
                "{1;  2   ;3;x()}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Int(2),
                    ValueExpr::Int(3),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    },
                ]),
            ),
            (
                "{1;  2   ;3;x({})}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Int(2),
                    ValueExpr::Int(3),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![empty_duck()],
                    },
                ]),
            ),
            (
                "{x();y();}",
                ValueExpr::Block(vec![
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                    },
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    },
                    empty_tuple(),
                ]),
            ),
            (
                "x({ 1; 2; y({ z(); }) }, lol)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            ValueExpr::Int(1),
                            ValueExpr::Int(2),
                            ValueExpr::FunctionCall {
                                target: var("y"),
                                params: vec![ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("z"),
                                        params: vec![],
                                    },
                                    empty_tuple(),
                                ])],
                            },
                        ]),
                        ValueExpr::Variable("lol".into(), None),
                    ],
                },
            ),
            (
                "while (true) {}",
                ValueExpr::While {
                    condition: ValueExpr::Bool(true).into(),
                    body: empty_tuple().into_block().into(),
                },
            ),
            (
                "while (my_func()) {}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into(),
                    body: empty_tuple().into_block().into(),
                },
            ),
            (
                "while (my_func()) {1;break;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                    }
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Break,
                        empty_tuple(),
                    ])
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
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Continue,
                        empty_tuple(),
                    ])
                    .into(),
                },
            ),
            ("()", empty_tuple()),
            (
                "(1.1, 'x')",
                ValueExpr::Tuple(vec![ValueExpr::Float(1.1), ValueExpr::Char('x')]),
            ),
            (
                "{x: 1, y: { z: true }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1)),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![("z".into(), ValueExpr::Bool(true))]),
                    ),
                ]),
            ),
            (
                "{x: 1, y: { z: true, w: { print();2;true } }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1)),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![
                            (
                                "w".into(),
                                ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("print"),
                                        params: vec![],
                                    },
                                    ValueExpr::Int(2),
                                    ValueExpr::Bool(true),
                                ]),
                            ),
                            ("z".into(), ValueExpr::Bool(true)),
                        ]),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Duck(vec![]).into_block().into(),
                    r#else: Some(
                        ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1))])
                            .into_block()
                            .into(),
                    ),
                },
            ),
            (
                "x.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Variable("x".into(), None).into(),
                    field_name: "y".into(),
                },
            ),
            (
                "{x: 123}.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(123))]).into(),
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
                    target: ValueExpr::Int(1).into(),
                    params: vec![],
                },
            ),
            (
                "(123)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(123).into(),
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
                        .into(),
                        field_name: "z".into(),
                    }
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
                        ValueExpr::Block(vec![empty_tuple(), empty_tuple(), empty_tuple()]),
                        ValueExpr::Int(1),
                    ],
                },
            ),
            (
                "x({();{();1;};},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple(),
                            ValueExpr::Block(vec![empty_tuple(), ValueExpr::Int(1), empty_tuple()]),
                            empty_tuple(),
                        ]),
                        ValueExpr::Int(1),
                    ],
                },
            ),
            (
                "return 123",
                ValueExpr::Return(Some(Box::new(ValueExpr::Int(123).into()))),
            ),
            (
                "let x: String",
                ValueExpr::VarDecl(
                    Declaration {
                        name: "x".into(),
                        initializer: None,
                        type_expr: TypeExpr::String,
                    }
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
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into(),
                ),
            ),
            (
                "3 * 5",
                ValueExpr::Mul(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()),
            ),
            (
                "3 + 5",
                ValueExpr::Add(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()),
            ),
            (
                "3 * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(ValueExpr::Int(3).into(), ValueExpr::Int(5).into()).into(),
                    ValueExpr::Int(6).into(),
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
                        .into(),
                        ValueExpr::Int(5).into(),
                    )
                    .into(),
                    ValueExpr::Int(6).into(),
                ),
            ),
            ("!true", ValueExpr::BoolNegate(ValueExpr::Bool(true).into())),
            (
                "!{1;2;true}",
                ValueExpr::BoolNegate(
                    ValueExpr::Block(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Int(2),
                        ValueExpr::Bool(true),
                    ])
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
                        .into(),
                    )
                    .into(),
                ),
            ),
            (
                "!!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FieldAccess {
                            target_obj: ValueExpr::FieldAccess {
                                target_obj: ValueExpr::Variable("x".into(), None).into(),
                                field_name: "y".into(),
                            }
                            .into(),
                            field_name: "z".into(),
                        }
                        .into(),
                    )
                    .into(),
                ),
            ),
            (
                "!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: ValueExpr::Variable("x".into(), None).into(),
                            field_name: "y".into(),
                        }
                        .into(),
                        field_name: "z".into(),
                    }
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
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                    }
                    .into(),
                ),
            ),
            (
                "1 == 2",
                ValueExpr::Equals(ValueExpr::Int(1).into(), ValueExpr::Int(2).into()),
            ),
            (
                "!(1 == 2)",
                ValueExpr::BoolNegate(
                    ValueExpr::Equals(ValueExpr::Int(1).into(), ValueExpr::Int(2).into()).into(),
                ),
            ),
            (
                "!1 == !2",
                ValueExpr::Equals(
                    ValueExpr::BoolNegate(ValueExpr::Int(1).into()).into(),
                    ValueExpr::BoolNegate(ValueExpr::Int(2).into()).into(),
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
                        value_expr: ValueExpr::Duck(vec![]),
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
                        value_expr: ValueExpr::Int(1),
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
                        value_expr: ValueExpr::Int(1),
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
                        value_expr: ValueExpr::Int(1),
                    }
                    .into(),
                ),
            ),
            (
                "{x: 1}.x",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1))]).into(),
                    field_name: "x".into(),
                },
            ),
            (
                "(1,(3,4),\"s\").0",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Tuple(vec![
                        ValueExpr::Int(1),
                        ValueExpr::Tuple(vec![ValueExpr::Int(3), ValueExpr::Int(4)]),
                        ValueExpr::String("s".into()),
                    ])
                    .into(),
                    field_name: "0".into(),
                },
            ),
        ];

        for (src, expected_tokens) in test_cases {
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result =
                value_expr_parser(make_input).parse(make_input((1..10).into(), &lex_result));

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.into_result().expect(&src);

            assert_eq!(output, expected_tokens, "{}", src);
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
                        fields: vec![("x".to_string(), TypeExpr::Int)],
                    }),
                    initializer: Some(ValueExpr::Duck(vec![])),
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

            let Some(ValueExpr::VarDecl(declaration)) = declaration_parse_result.into_output()
            else {
                unreachable!()
            };

            assert_eq!(declaration, expected_output.into());
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
            "{let y: Int = 0; while(true){y = 1;@println(y)}}",
            "x = 580",
            "y = 80",
            "y = true",
            "y = false",
            "y = \"Hallo\"",
        ];

        for valid_assignment in valid_assignments {
            let lexer_parse_result = lexer().parse(valid_assignment);
            assert_eq!(lexer_parse_result.has_errors(), false);
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
