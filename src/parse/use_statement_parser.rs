use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{SS, failure_with_occurence};

use super::lexer::Token;

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
            for i in 0..v.len() - 1 {
                if v[i].0.len() != 1 {
                    let msg = "Only last part may specify multiple imports";
                    failure_with_occurence(msg, v[i].1, [(msg, v[i].1)]);
                }

                base_path.push(v[i].0[0].clone());
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

#[cfg(test)]
mod tests {
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

    use super::*;

    #[test]
    fn test_use_statement_parser() {
        let src_and_expected_ast = vec![
            (
                "use std::abc;",
                vec![UseStatement::Regular(
                    false,
                    vec!["std".to_string(), "abc".to_string()],
                )],
            ),
            (
                "use ::std::abc;",
                vec![UseStatement::Regular(
                    true,
                    vec!["std".to_string(), "abc".to_string()],
                )],
            ),
            (
                "use ::std::{abc, xyz};",
                vec![
                    UseStatement::Regular(true, vec!["std".to_string(), "abc".to_string()]),
                    UseStatement::Regular(true, vec!["std".to_string(), "xyz".to_string()]),
                ],
            ),
            (
                "use ::{assert};",
                vec![UseStatement::Regular(true, vec!["assert".to_string()])],
            ),
        ];

        for (src, expected_ast) in src_and_expected_ast {
            println!("lexing {src}");
            let lexer_parse_result = lex_parser("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("parsing use statement {src}");
            let use_statement_parse_result = use_statement_parser()
                .parse(make_input(empty_range(), tokens.as_slice()))
                .into_result()
                .unwrap();

            assert_eq!(use_statement_parse_result, expected_ast);
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
            let lexer_parse_result = lex_parser("test", "").parse(invalid_use_statement);

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
