use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::SS;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Indicator {
    Symbols(Vec<String>),
    Module(String),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseStatement {
    Regular(bool, Vec<Indicator>),
    Go(String, Option<String>),
}

fn regular_use_parser<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
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
        .ignore_then(just(Token::ScopeRes).or_not().map(|x| x.is_some()))
        .then(
            module_indicator_parser
                .or(symbols_indicator_parser)
                .or(wildcard_indicator_parser)
                .separated_by(just(Token::ScopeRes))
                .at_least(1)
                .collect::<Vec<Indicator>>(),
        )
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(is_global, i)| UseStatement::Regular(is_global, i))
}

fn go_use_parser<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    (just(Token::Use).then(just(Token::Go)))
        .ignore_then(select_ref! { Token::StringLiteral(s) => s.to_owned() })
        .then(
            just(Token::As)
                .ignore_then(select_ref! { Token::Ident(i) => i.to_owned() })
                .or_not(),
        )
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(package_name, alias)| UseStatement::Go(package_name, alias))
}

pub fn use_statement_parser<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    choice((go_use_parser(), regular_use_parser()))
}

#[cfg(test)]
mod tests {
    use crate::parse::{lexer::lexer, make_input, value_parser::empty_range};

    use super::*;

    #[test]
    fn test_use_statement_parser() {
        let src_and_expected_ast = vec![
            (
                "use std;",
                UseStatement::Regular(false, vec![Indicator::Module("std".to_string())]),
            ),
            (
                "use std::io;",
                UseStatement::Regular(
                    false,
                    vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                    ],
                ),
            ),
            (
                "use std::io::{println};",
                UseStatement::Regular(
                    false,
                    vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                        Indicator::Symbols(vec!["println".to_string()]),
                    ],
                ),
            ),
            (
                "use std::io::{println, print};",
                UseStatement::Regular(
                    false,
                    vec![
                        Indicator::Module("std".to_string()),
                        Indicator::Module("io".to_string()),
                        Indicator::Symbols(vec!["println".to_string(), "print".to_string()]),
                    ],
                ),
            ),
            ("use go \"fmt\";", UseStatement::Go("fmt".into(), None)),
            (
                "use go \"fmt\" as FMT;",
                UseStatement::Go("fmt".into(), Some("FMT".into())),
            ),
        ];

        for (src, expected_ast) in src_and_expected_ast {
            println!("lexing {src}");
            let lexer_parse_result = lexer("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("parsing use statement {src}");
            let use_statement_parse_result =
                use_statement_parser().parse(make_input(empty_range(), tokens.as_slice()));
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
            "use :std:;",
            "use :std::{};",
            "use go x;",
            "use go;",
            "use go \"fmt\" as;",
            "use go fmt as x",
            "use go::x;",
            "use go::x;",
            "use go as;",
        ];

        for invalid_use_statement in invalid_use_statements {
            println!("lexing {invalid_use_statement}");
            let lexer_parse_result = lexer("test", "").parse(invalid_use_statement);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_use_statement}");
            let typedef_parse_result =
                use_statement_parser().parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
