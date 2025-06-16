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
    Variable(String),
    If {
        condition: Box<ValueExpr>,
        then: Box<ValueExpr>,
        r#else: Box<ValueExpr>,
    },
}

pub fn value_expr_parser<'src>() -> impl Parser<'src, &'src [Token], ValueExpr> {
    recursive(|e| {
        let params = e
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));

        let if_condition = e
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')));
        let if_body = e
            .clone()
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')));
        let if_with_condition_and_body = just(Token::If)
            .ignore_then(if_condition.clone())
            .then(if_body.clone());

        choice((
            select_ref! { Token::Ident(ident) => ident.to_string() }
                .then(params)
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
        ))
    })
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use crate::parse::{lexer::lexer, value_parser::value_expr_parser};

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
        ];

        for (src, expected_tokens) in test_cases {
            let lex_result = lexer().parse(src).unwrap();
            let parse_result = value_expr_parser().parse(&lex_result);

            assert_eq!(parse_result.has_errors(), false);
            assert_eq!(parse_result.has_output(), true);

            let output: ValueExpr = parse_result.unwrap();

            assert_eq!(output, expected_tokens);
        }
    }
}
