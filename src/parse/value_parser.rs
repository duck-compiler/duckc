use super::lexer::Token;
use chumsky::prelude::*;

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
    Variable(String),
    If {
        condition: Box<ValueExpr>,
        then: Box<ValueExpr>,
        r#else: Box<ValueExpr>,
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
    FieldAccess {
        target_obj: Box<ValueExpr>,
        field_name: String,
    },
}

impl ValueExpr {
    fn flatten_block(&self) -> ValueExpr {
        match self {
            ValueExpr::Block(x) if x.len() <= 1 => {
                if let Some(x) = x.get(0) {
                    x.clone()
                } else {
                    empty_tuple()
                }
            }
            _ => self.clone(),
        }
    }
}

pub fn value_expr_parser<'src>() -> impl Parser<'src, &'src [Token], ValueExpr> {
    recursive(|e| {
        let params = e
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));

        let tuple = (just(Token::ControlChar('('))
            .ignore_then(just(Token::ControlChar(')')))
            .to(ValueExpr::Tuple(vec![])))
        .or(e
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(|x| ValueExpr::Tuple(dbg!(x))));

        let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(e.clone())
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|x| ValueExpr::Duck(x));

        let block_expression = e
            .clone()
            .then(just(Token::ControlChar(';')).or_not())
            .repeated()
            .collect::<Vec<_>>()
            .map_err(|e| {
                dbg!(e);
                todo!()
            })
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut x| {
                if x.is_empty() || x.last().unwrap().1.is_some() {
                    x.push((empty_tuple(), None));
                }
                ValueExpr::Block(x.into_iter().map(|(t, _)| t).collect()).flatten_block()
            });

        let if_condition = e
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));
        let if_body = block_expression.clone();
        let if_with_condition_and_body = just(Token::If)
            .ignore_then(if_condition.clone())
            .then(if_body.clone());

        let while_condition = if_condition.clone();
        let while_body = block_expression.clone();
        let while_with_condition_and_body = just(Token::While)
            .ignore_then(while_condition.clone())
            .then(while_body.clone());

        let field_access = any()
            .filter(|t| !matches!(t, Token::ControlChar('.')))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(
                (just(Token::ControlChar('.')).ignore_then(
                    select_ref! { Token::Ident(field_name) => field_name.to_owned() },
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
            )
            .map({
                let e = e.clone();
                move |(base_expr, field_accesses)| {
                    let base_expr = base_expr.leak() as &[Token];
                    dbg!(&base_expr);
                    let base = e.parse(base_expr).unwrap();
                    let r =
                        field_accesses
                            .into_iter()
                            .fold(base, |acc, x| ValueExpr::FieldAccess {
                                target_obj: acc.into(),
                                field_name: x,
                            });
                    r
                }
            });

        let int = select_ref! { Token::IntLiteral(i) => *i }.map(ValueExpr::Int);
        let bool_val = select_ref! { Token::BoolLiteral(b) => *b }.map(ValueExpr::Bool);
        let string_val =
            select_ref! { Token::StringLiteral(s) => s.to_owned() }.map(ValueExpr::String);
        let var_expr =
            select_ref! { Token::Ident(ident) => ident.to_owned() }.map(ValueExpr::Variable);
        let if_expr = if_with_condition_and_body
            .clone()
            .then(
                just(Token::Else)
                    .ignore_then(if_with_condition_and_body.clone())
                    .repeated()
                    .collect::<Vec<(ValueExpr, ValueExpr)>>(),
            )
            .then_ignore(just(Token::Else))
            .then(if_body.clone())
            .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                condition: Box::new(condition),
                then: Box::new(then),
                r#else: else_ifs
                    .into_iter()
                    .rfold(Box::new(r#else), |acc, (cond, then)| {
                        Box::new(ValueExpr::If {
                            condition: Box::new(cond),
                            then: Box::new(then),
                            r#else: acc,
                        })
                    }),
            });
        let char_expr = select_ref! { Token::CharLiteral(c) => *c }.map(ValueExpr::Char);
        let float_expr = select_ref! { Token::FloatLiteral(num) => *num }.map(ValueExpr::Float);

        let atom = e
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .or(choice((
                field_access,
                int,
                bool_val,
                string_val,
                var_expr,
                if_expr,
                char_expr,
                float_expr,
                tuple,
                duck_expression,
                block_expression,
                just(Token::Break).to(ValueExpr::Break),
                just(Token::Continue).to(ValueExpr::Continue),
                while_with_condition_and_body
                    .clone()
                    .map(|(cond, body)| ValueExpr::While {
                        condition: Box::new(cond),
                        body: Box::new(body),
                    }),
            )));

        choice((
            atom.clone()
                .then(params.clone())
                .map(|(target, params)| ValueExpr::FunctionCall {
                    target: target.into(),
                    params,
                }),
            atom,
        ))
    })
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

fn empty_duck() -> ValueExpr {
    ValueExpr::Duck(Vec::new())
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::parse::{
        lexer::lexer,
        value_parser::{empty_duck, empty_tuple, value_expr_parser},
    };

    use super::ValueExpr;

    fn var(x: impl Into<String>) -> Box<ValueExpr> {
        ValueExpr::Variable(x.into()).into()
    }

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
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
            ("x", ValueExpr::Variable("x".into())),
            (
                "print(x, true, lol())",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::Variable("x".into()),
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
                    then: ValueExpr::Int(1).into(),
                    r#else: ValueExpr::Int(2).into(),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Int(1).into(),
                    r#else: ValueExpr::If {
                        condition: ValueExpr::Bool(false).into(),
                        then: ValueExpr::Int(3).into(),
                        r#else: ValueExpr::If {
                            condition: ValueExpr::Int(200).into(),
                            then: ValueExpr::Int(4).into(),
                            r#else: ValueExpr::Int(2).into(),
                        }
                        .into(),
                    }
                    .into(),
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
            ("{1}", ValueExpr::Int(1)),
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
                        ValueExpr::Variable("lol".into()),
                    ],
                },
            ),
            (
                "while (true) {}",
                ValueExpr::While {
                    condition: ValueExpr::Bool(true).into(),
                    body: empty_tuple().into(),
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
                    body: empty_tuple().into(),
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
                            ("z".into(), ValueExpr::Bool(true)),
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
                        ]),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into(),
                    then: ValueExpr::Duck(vec![]).into(),
                    r#else: ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1))]).into(),
                },
            ),
            (
                "x.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Variable("x".into()).into(),
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
        ];

        for (src, expected_tokens) in test_cases {
            dbg!(src);
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result = value_expr_parser().parse(&lex_result);

            dbg!(&lex_result, &parse_result);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.into_result().expect(&src);

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }
}
