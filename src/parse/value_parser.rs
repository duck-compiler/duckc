use super::lexer::Token;
use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    FunctionCall {
        name: String,
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

        let concated_body = e
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
        let if_body = concated_body.clone();
        let if_with_condition_and_body = just(Token::If)
            .ignore_then(if_condition.clone())
            .then(if_body.clone());

        let while_condition = if_condition.clone();
        let while_body = concated_body.clone();
        let while_with_condition_and_body = just(Token::While)
            .ignore_then(while_condition.clone())
            .then(while_body.clone());

        choice((
            select_ref! { Token::Ident(ident) => ident.to_string() }
                .then(params.clone())
                .map(|(fn_name, params)| ValueExpr::FunctionCall {
                    name: fn_name,
                    params,
                }),
            select_ref! { Token::IntLiteral(i) => *i }.map(ValueExpr::Int),
            select_ref! { Token::BoolLiteral(b) => *b }.map(ValueExpr::Bool),
            select_ref! { Token::StringLiteral(s) => s.to_owned() }.map(ValueExpr::String),
            select_ref! { Token::Ident(ident) => ident.to_owned() }.map(ValueExpr::Variable),
            if_with_condition_and_body
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
                }),
            params.clone().map(ValueExpr::Tuple),
            while_with_condition_and_body
                .clone()
                .map(|(cond, body)| ValueExpr::While {
                    condition: Box::new(cond),
                    body: Box::new(body),
                }),
            concated_body.clone(),
            just(Token::Break).to(ValueExpr::Break),
            just(Token::Continue).to(ValueExpr::Continue),
            select_ref! { Token::CharLiteral(c) => *c }.map(ValueExpr::Char),
            select_ref! { Token::FloatLiteral(num) => *num }.map(ValueExpr::Float),
        ))
    })
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::parse::{
        lexer::lexer,
        value_parser::{empty_tuple, value_expr_parser},
    };

    use super::ValueExpr;

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
            (
                "to_upper()",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper(1)",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper(1,)",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ()",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper (1)",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper (1,)",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper (   )",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: Vec::new(),
                },
            ),
            (
                "to_upper ( 1 )",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![ValueExpr::Int(1)],
                },
            ),
            (
                "to_upper ( to_lower(1,2,add(5, 10),4), true  )",
                ValueExpr::FunctionCall {
                    name: "to_upper".into(),
                    params: vec![
                        ValueExpr::FunctionCall {
                            name: "to_lower".into(),
                            params: vec![
                                ValueExpr::Int(1),
                                ValueExpr::Int(2),
                                ValueExpr::FunctionCall {
                                    name: "add".to_string(),
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
                    name: "print".into(),
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
                    name: "print".into(),
                    params: vec![
                        ValueExpr::Variable("x".into()),
                        ValueExpr::Bool(true),
                        ValueExpr::FunctionCall {
                            name: "lol".into(),
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
            ("{}", empty_tuple()),
            ("{1}", ValueExpr::Int(1)),
            (
                "{1;  2   ;3;x()}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1),
                    ValueExpr::Int(2),
                    ValueExpr::Int(3),
                    ValueExpr::FunctionCall {
                        name: "x".into(),
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
                        name: "x".into(),
                        params: vec![empty_tuple()],
                    },
                ]),
            ),
            (
                "{x();y();}",
                ValueExpr::Block(vec![
                    ValueExpr::FunctionCall {
                        name: "x".into(),
                        params: vec![],
                    },
                    ValueExpr::FunctionCall {
                        name: "y".into(),
                        params: vec![],
                    },
                    empty_tuple(),
                ]),
            ),
            (
                "x({ 1; 2; y({ z(); }) }, lol)",
                ValueExpr::FunctionCall {
                    name: "x".into(),
                    params: vec![
                        ValueExpr::Block(vec![
                            ValueExpr::Int(1),
                            ValueExpr::Int(2),
                            ValueExpr::FunctionCall {
                                name: "y".into(),
                                params: vec![ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        name: "z".into(),
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
                        name: "my_func".into(),
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
                        name: "my_func".into(),
                        params: vec![],
                    }
                    .into(),
                    body: ValueExpr::Block(vec![ValueExpr::Int(1), ValueExpr::Break, empty_tuple()]).into(),
                },
            ),
            (
                "while (my_func()) {1;continue;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        name: "my_func".into(),
                        params: vec![],
                    }
                    .into(),
                    body: ValueExpr::Block(vec![ValueExpr::Int(1), ValueExpr::Continue, empty_tuple()]).into(),
                },
            ),
            ("()", empty_tuple()),
            ("(1.1, 'x')", ValueExpr::Tuple(vec![ValueExpr::Float(1.1), ValueExpr::Char('x')])),
        ];

        for (src, expected_tokens) in test_cases {
            let lex_result = lexer().parse(src).into_result().expect(&src);
            let parse_result = value_expr_parser().parse(&lex_result);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: ValueExpr = parse_result.unwrap();

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }
}
