use chumsky::prelude::*;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Indicator {
    Symbols(Vec<String>),
    Module(String),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseStatement {
    pub indicator_track: Vec<Indicator>,
}

pub fn use_statement_parser<'src>() -> impl Parser<'src, &'src [Token], UseStatement> {
    let module_indicator_parser =
        select_ref! { Token::Ident(identifier) => identifier.to_string() }.map(Indicator::Module);

    let symbols_indicator_parser = just(Token::ControlChar('{'))
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .separated_by(just(Token::ControlChar(',')))
                .at_least(1)
                .collect::<Vec<String>>(),
        )
        .then_ignore(just(Token::ControlChar('}')))
        .map(Indicator::Symbols);

    let wildcard_indicator_parser = just(Token::ControlChar('*')).to(Indicator::Wildcard);

    just(Token::Use)
        .ignore_then(
            module_indicator_parser
                .or(symbols_indicator_parser)
                .or(wildcard_indicator_parser)
                .separated_by(
                    just(Token::ControlChar(':')).ignore_then(just(Token::ControlChar(':'))),
                )
                .at_least(1)
                .collect::<Vec<Indicator>>(),
        )
        .then_ignore(just(Token::ControlChar(';')))
        .map(|indicators| UseStatement {
            indicator_track: indicators,
        })
}

#[cfg(test)]
mod tests {
    use crate::parse::lexer::lexer;

    use super::*;

    #[test]
    fn test_use_statement_parser() {
        let src_and_expected_ast = vec![
            (
                "use std;",
                UseStatement {
                    indicator_track: vec![Indicator::Module("std".to_string())],
                },
            ),
            (
                "use std::io;",
                UseStatement {
                    indicator_track: vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                    ],
                },
            ),
            (
                "use std::io::{println};",
                UseStatement {
                    indicator_track: vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                        Indicator::Symbols(vec!["println".to_string()]),
                    ],
                },
            ),
            (
                "use std::io::{println, print};",
                UseStatement {
                    indicator_track: vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                        Indicator::Symbols(vec!["println".to_string(), "print".to_string()]),
                    ],
                },
            ),
        ];

        for (src, expected_ast) in src_and_expected_ast {
            println!("lexing {src}");
            let lexer_parse_result = lexer().parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("parsing use statement {src}");
            let use_statement_parse_result = use_statement_parser().parse(tokens.as_slice());
            assert_eq!(use_statement_parse_result.has_errors(), false);
            assert_eq!(use_statement_parse_result.has_output(), true);

            let Some(ast) = use_statement_parse_result.into_output() else {
                unreachable!()
            };

            assert_eq!(ast, expected_ast);
        }

        let invalid_use_statements = vec![
            "use x::;",
            "use y::{};",
            "use std::{}",
            "use ::;",
            "use ::std;",
            "use :std:;",
            "use :std::{};",
        ];

        for invalid_use_statement in invalid_use_statements {
            println!("lexing {invalid_use_statement}");
            let lexer_parse_result = lexer().parse(invalid_use_statement);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_use_statement}");
            let typedef_parse_result = use_statement_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
