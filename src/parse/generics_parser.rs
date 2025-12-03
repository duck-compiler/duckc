use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{
    SS, Spanned, failure_with_occurence,
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Generic {
    pub name: String,
    pub constraint: Option<Spanned<TypeExpr>>,
}

pub fn generics_parser<'src, I>()
-> impl Parser<'src, I, Vec<Spanned<Generic>>, extra::Err<Rich<'src, Token, SS>>> + Clone + 'src
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    // '<' <identifier> '>'
    just(Token::ControlChar('<'))
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.clone() }
                .then(
                    just(Token::ControlChar(':'))
                        .ignore_then(
                            type_expression_parser()
                                .map(|type_expr| {
                                    let span = type_expr.1;
                                    match &type_expr.0 {
                                        TypeExpr::Duck(_) => type_expr,
                                        other if false => {
                                            failure_with_occurence(
                                                "Invalid Syntax".to_string(),
                                                span,
                                                vec![
                                                    (
                                                        format!(
                                                            "Type constraints are defined using ducks. You've passed a {other}"
                                                        ),
                                                        span,
                                                    ),
                                                ],
                                            );
                                        }
                                        _ => type_expr,
                                    }
                                })
                        )
                        .or_not()
                )
                .map(|(identifier, constraint)| Generic {
                    name: identifier.clone(),
                    constraint
                })
                .map_with(|generic, ctx| (generic, ctx.span()))
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<Spanned<Generic>>>(),
        )
        .then_ignore(just(Token::ControlChar('>')))
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::{
        parse::{
            Field,
            generics_parser::generics_parser,
            lexer::lex_parser,
            make_input,
            type_parser::Duck,
            value_parser::{empty_range, type_expr_into_empty_range},
        },
        semantics::type_resolve::sort_fields_type_expr,
    };

    #[test]
    fn test_simple_generics_parser() {
        let valid_generic_definitions = vec![
            "<TYPENAME>",
            "<TYPENAME1, TYPENAME2>",
            "<TYPENAME1, TYPENAME2, TYPENAME3, TYPENAME4, TYPENAME5, TYPENAME6, TYPENAME7, TYPENAME8, TYPENAME9>",
            "<ABCDEFGHIJKLMNOPQRSTUVWQXYZ>",
            "<XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>",
            "<XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX, XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>",
            "<XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX, XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX>",
            "<WITH_TRAILING_COMMA,>",
            "<WITH_SPACE_BETWEEN, AND_TRAILING_COMMA_WITH_EVEN_MORE_SPACE,       >",
            "<WITH_SPACE_BETWEEN,
            AND_A_LINE_BREAK_AND_EVEN_MORE_SPACE,       >",
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
            let typedef_parse_result = generics_parser().parse(make_input(empty_range(), &tokens));
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
            let typedef_parse_result =
                generics_parser().parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }

    #[test]
    fn test_detailed_generics_parser() {
        let test_cases = vec![
            (
                "<TYPENAME>",
                vec![Generic {
                    name: "TYPENAME".to_string(),
                    constraint: None,
                }],
            ),
            (
                "<TYPENAME, TYPENAMETWO>",
                vec![
                    Generic {
                        name: "TYPENAME".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETWO".to_string(),
                        constraint: None,
                    },
                ],
            ),
            (
                "<TYPENAME, TYPENAMETWO, TYPENAMETHREE>",
                vec![
                    Generic {
                        name: "TYPENAME".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETWO".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETHREE".to_string(),
                        constraint: None,
                    },
                ],
            ),
            (
                "<ABCDEFGHIJKLMNOPQRSTUVWXYZ>",
                vec![Generic {
                    name: "ABCDEFGHIJKLMNOPQRSTUVWXYZ".to_string(),
                    constraint: None,
                }],
            ),
            (
                "<A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z>",
                vec![
                    Generic {
                        name: "A".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "B".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "C".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "D".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "E".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "F".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "G".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "H".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "I".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "J".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "K".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "L".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "M".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "N".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "O".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "P".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "Q".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "R".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "S".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "T".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "U".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "V".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "W".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "X".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "Y".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "Z".to_string(),
                        constraint: None,
                    },
                ],
            ),
        ];

        for (i, (src, expected_generics)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
            let parse_result = generics_parser().parse(make_input(empty_range(), &lex_result));

            assert_eq!(
                parse_result.has_errors(),
                false,
                "{i}: {} {:?} {:?}",
                src,
                lex_result,
                parse_result
            );

            assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

            let output = parse_result.into_result().expect(&src);

            let actual_generics = output
                .iter()
                .map(|(generic, _)| generic.clone())
                .collect::<Vec<Generic>>();

            assert_eq!(actual_generics, expected_generics, "{i}: {}", src);
        }
    }

    #[test]
    fn test_constrained_generics() {
        let test_cases = vec![
            (
                "<TYPENAME: { username: String }>",
                vec![Generic {
                    name: "TYPENAME".to_string(),
                    constraint: Some((
                        TypeExpr::Duck(Duck {
                            fields: vec![Field {
                                name: "username".to_string(),
                                type_expr: (TypeExpr::String(None), empty_range()),
                            }],
                        }),
                        empty_range(),
                    )),
                }],
            ),
            (
                "<TYPENAME, TYPENAMETWO: { username: String, b: Int }>",
                vec![
                    Generic {
                        name: "TYPENAME".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETWO".to_string(),
                        constraint: Some((
                            TypeExpr::Duck(Duck {
                                fields: vec![
                                    Field {
                                        name: "username".to_string(),
                                        type_expr: (TypeExpr::String(None), empty_range()),
                                    },
                                    Field {
                                        name: "b".to_string(),
                                        type_expr: (TypeExpr::Int(None), empty_range()),
                                    },
                                ],
                            }),
                            empty_range(),
                        )),
                    },
                ],
            ),
            (
                "<TYPENAME, TYPENAMETWO, TYPENAMETHREE: { username: { x: String }}>",
                vec![
                    Generic {
                        name: "TYPENAME".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETWO".to_string(),
                        constraint: None,
                    },
                    Generic {
                        name: "TYPENAMETHREE".to_string(),
                        constraint: Some((
                            TypeExpr::Duck(Duck {
                                fields: vec![Field {
                                    name: "username".to_string(),
                                    type_expr: (
                                        TypeExpr::Duck(Duck {
                                            fields: vec![Field {
                                                name: "x".to_string(),
                                                type_expr: (TypeExpr::String(None), empty_range()),
                                            }],
                                        }),
                                        empty_range(),
                                    ),
                                }],
                            }),
                            empty_range(),
                        )),
                    },
                ],
            ),
        ];

        for (i, (src, mut expected_generics)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
            let parse_result = generics_parser().parse(make_input(empty_range(), &lex_result));

            assert_eq!(
                parse_result.has_errors(),
                false,
                "{i}: {} {:?} {:?}",
                src,
                lex_result,
                parse_result
            );

            assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

            let output = parse_result.into_result().expect(&src);

            let actual_generics = output
                .iter()
                .map(|generic| {
                    let mut generic = generic.clone();
                    if let Some((type_expr, span)) = generic.0.constraint.as_mut() {
                        *span = empty_range();
                        if let TypeExpr::Duck(duck) = type_expr {
                            duck.fields.iter_mut().for_each(|field| {
                                type_expr_into_empty_range(&mut field.type_expr);
                            });
                        } else {
                            unreachable!("this should be anything else than a duck")
                        }
                    }
                    generic.0
                })
                .collect::<Vec<Generic>>();

            expected_generics.iter_mut().for_each(|generic| {
                if let Some((type_expr, _)) = &mut generic.constraint {
                    sort_fields_type_expr(type_expr);
                }
            });
            assert_eq!(actual_generics, expected_generics, "{i}: {}", src);
        }
    }
}
