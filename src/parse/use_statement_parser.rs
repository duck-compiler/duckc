use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{SS, failure_with_occurence};

use super::lexer::Token;

#[cfg(test)]
#[path = "use_statement_parser_test.rs"]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub enum UseStatement {
    Regular(bool, Vec<String>),
    Go(String, Option<String>),
}

fn regular_use_parser<'src, I>()
-> impl Parser<'src, I, Vec<UseStatement>, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    just(Token::Use)
        .ignore_then(just(Token::ScopeRes).or_not().map(|x| x.is_some()))
        .then(
            choice((
                select_ref! {Token::Ident(i) => i.to_string() }.map(|v| vec![v]),
                select_ref! { Token::Ident(i) => i.to_string() }
                    .separated_by(just(Token::ControlChar(',')))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))),
            ))
            .map_with(|x, e| (x, e.span()))
            .separated_by(just(Token::ScopeRes))
            .at_least(1)
            .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(is_glob, v)| {
            let mut base_path = Vec::new();
            for item in v.iter().take(v.len() - 1) {
                if item.0.len() != 1 {
                    let msg = "Only last part may specify multiple imports";
                    failure_with_occurence(msg, item.1, [(msg, item.1)]);
                }

                base_path.push(item.0[0].clone());
            }

            let mut out = Vec::new();

            for end in v.last().unwrap().0.iter() {
                let mut cloned_path = base_path.clone();
                cloned_path.push(end.clone());
                out.push(UseStatement::Regular(is_glob, cloned_path));
            }

            out
        })
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
-> impl Parser<'src, I, Vec<UseStatement>, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    choice((go_use_parser().map(|u| vec![u]), regular_use_parser()))
}
