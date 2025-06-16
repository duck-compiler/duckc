use super::{lexer::Token, statement_parser::Statement};
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
    Block {
        statements: Vec<Statement>
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

        choice((
            select_ref! { Token::Ident(ident) => ident.to_string() }
                .then(params)
                .map(|(fn_name, params)| ValueExpr::FunctionCall {
                    name: fn_name,
                    params,
                }),
            select_ref! { Token::IntLiteral(i) => *i }.map(|i| ValueExpr::Int(i)),
            select_ref! { Token::BoolLiteral(b) => *b }.map(|b| ValueExpr::Bool(b)),
            select_ref! { Token::StringLiteral(s) => s }.map(|s| ValueExpr::String(s.to_owned())),
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
