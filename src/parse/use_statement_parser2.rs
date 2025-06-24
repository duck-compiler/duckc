use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::use_statement_parser::{Indicator, UseStatement};

use super::lexer::Token;

fn regular_use_parser<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
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
        .map(UseStatement::Regular)
}

fn go_use_parser<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
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

pub fn use_statement_parser2<'src, I>()
-> impl Parser<'src, I, UseStatement, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
{
    choice((go_use_parser(), regular_use_parser()))
}
