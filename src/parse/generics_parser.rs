use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{lexer::Token, Spanned, SS};



#[derive(Debug, Clone, PartialEq)]
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
            "<WITH_TRAILING_COMMA,>",
            "<WITH_SPACE_BETWEEN, AND_TRAILING_COMMA_WITH_EVEN_MORE_SPACE,       >",
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

    #[test]
    fn test_detailed_generics_parser() {
        let test_cases = vec![
            (
                "<TYPENAME>",
                vec![Generic { name: "TYPENAME".to_string() }]
            ),
            (
                "<TYPENAME, TYPENAMETWO>",
                vec![
                    Generic { name: "TYPENAME".to_string() },
                    Generic { name: "TYPENAMETWO".to_string() },
                ]
            ),
            (
                "<TYPENAME, TYPENAMETWO, TYPENAMETHREE>",
                vec![
                    Generic { name: "TYPENAME".to_string() },
                    Generic { name: "TYPENAMETWO".to_string() },
                    Generic { name: "TYPENAMETHREE".to_string() },
                ]
            ),
            (
                "<ABCDEFGHIJKLMNOPQRSTUVWXYZ>",
                vec![
                    Generic { name: "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string() },
                ]
            ),
            (
                "<A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z>",
                vec![
                    Generic { name: "A".to_string() },
                    Generic { name: "B".to_string() },
                    Generic { name: "C".to_string() },
                    Generic { name: "D".to_string() },
                    Generic { name: "E".to_string() },
                    Generic { name: "F".to_string() },
                    Generic { name: "G".to_string() },
                    Generic { name: "H".to_string() },
                    Generic { name: "I".to_string() },
                    Generic { name: "J".to_string() },
                    Generic { name: "K".to_string() },
                    Generic { name: "L".to_string() },
                    Generic { name: "M".to_string() },
                    Generic { name: "N".to_string() },
                    Generic { name: "O".to_string() },
                    Generic { name: "P".to_string() },
                    Generic { name: "Q".to_string() },
                    Generic { name: "R".to_string() },
                    Generic { name: "S".to_string() },
                    Generic { name: "T".to_string() },
                    Generic { name: "U".to_string() },
                    Generic { name: "V".to_string() },
                    Generic { name: "W".to_string() },
                    Generic { name: "X".to_string() },
                    Generic { name: "Y".to_string() },
                    Generic { name: "Z".to_string() },
                ]
            ),
        ];

        for (i, (src, expected_generics)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
            let parse_result = generics_parser()
                .parse(make_input(empty_range(), &lex_result));

            assert_eq!(
                parse_result.has_errors(),
                false,
                "{i}: {} {:?} {:?}",
                src,
                lex_result,
                parse_result
            );

            assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

            let output = parse_result
                .into_result()
                .expect(&src);

            let actual_generics = output.iter()
                .map(|(generic, _)| generic.clone())
                .collect::<Vec<Generic>>();

            assert_eq!(actual_generics, expected_generics, "{i}: {}", src);
        }
    }

}
