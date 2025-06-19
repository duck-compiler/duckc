use chumsky::prelude::*;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
enum Indicator {
    Symbols(Vec<String>),
    Module(String),
    Wildcard,
}

#[derive(Debug, Clone, PartialEq)]
struct UseStatement {
    indicator_track: Vec<Indicator>,
}

pub fn use_statement_parser<'src>() -> impl Parser<'src, &'src [Token], UseStatement> {
    let module_indicator_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .map(Indicator::Module);

    let symbols_indicator_parser = just(Token::ControlChar('{'))
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .separated_by(just(Token::ControlChar(',')))
                .at_least(1)
                .collect::<Vec<String>>()
        ).then_ignore(just(Token::ControlChar('}')))
        .map(Indicator::Symbols);

    let wildcard_indicator_parser = just(Token::ControlChar('*'))
        .to(Indicator::Wildcard);

    just(Token::Use)
        .ignore_then(
            module_indicator_parser.or(symbols_indicator_parser).or(wildcard_indicator_parser)
                .separated_by(just(Token::ControlChar(':')).ignore_then(just(Token::ControlChar(':'))))
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
        let valid_use_statements = vec![
            "use std;",
            "use std::io;",
            "use std::io::{println};",
            "use std::io::{println, print};",
        ];

        for valid_use_statement in valid_use_statements {
            println!("lexing {valid_use_statement}");
            let lexer_parse_result = lexer().parse(valid_use_statement);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("parsing use statement {valid_use_statement}");
            let typedef_parse_result = use_statement_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_use_statements = vec![
            "use x::;",
            "use y::{};",
            "use std::{}",
        ];

        for invalid_use_statement in invalid_use_statements {
            println!("lexing {invalid_use_statement}");
            let lexer_parse_result = lexer().parse(invalid_use_statement);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {invalid_use_statement}");
            let typedef_parse_result = use_statement_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
