use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{lexer::Token, Spanned, SS};



#[derive(Debug, Clone)]
pub struct Generic {
    pub name: String,
}

pub fn generics_parser<'src, I>() -> impl Parser<'src, I, Vec<Spanned<Generic>>, extra::Err<Rich<'src, Token, SS>>> + Clone + 'src
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    // '<' <identifier> '>'
    just(Token::ControlChar('<'))
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.clone() }
                .map(|identifier| Generic { name: identifier.clone() })
                .map_with(|generic, ctx| (generic, ctx.span()))
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<Spanned<Generic>>>()
        )
        .then_ignore(just(Token::ControlChar('>')))
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{generics_parser::generics_parser, lexer::lex_parser, make_input, value_parser::empty_range};
    use super::*;

    #[test]
    fn test_simple_generics_parser() {
        let valid_generic_definitions = vec![
            "<TYPENAME>",
            "<TYPENAME1, TYPENAME2>",
            "<TYPENAME1, TYPENAME2, TYPENAME3, TYPENAME4, TYPENAME5, TYPENAME6, TYPENAME7, TYPENAME8, TYPENAME9>",
            "<ABCDEFGHIJKLMNOPQRSTUVWQXYZ>",
            "<XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>",
            "<XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX, XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>",
        ];

        for valid_generic_definition in valid_generic_definitions {
            println!("lexing {valid_generic_definition}");
            let lexer_parse_result = lex_parser("test", "").parse(valid_generic_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_generic_definition}");
            let typedef_parse_result = generics_parser()
                .parse(make_input(empty_range(), &tokens));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_generic_definitions = vec!["<>", "<{}>", "<*()>"];

        for invalid_generic_definition in invalid_generic_definitions {
            println!("lexing {invalid_generic_definition}");
            let lexer_parse_result = lex_parser("test", "").parse(invalid_generic_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_generic_definition}");
            let typedef_parse_result = generics_parser()
                .parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
