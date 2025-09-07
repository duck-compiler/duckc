use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{
    SS, Spanned,
    type_parser::{TypeExpr, type_expression_parser},
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TsxComponent {
    pub name: String,
    pub params: Vec<(String, Spanned<TypeExpr>)>,
    pub typescript_source: Spanned<String>,
}

pub fn tsx_component_parser<'src, I>()
-> impl Parser<'src, I, TsxComponent, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    // component Name {
    //   %javascript source
    // }
    just(Token::Component)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.clone() })
        .then(
            select_ref! { Token::Ident(i) => i.clone() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(type_expression_parser())
                .separated_by(just(Token::ControlChar(',')))
                .collect()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))),
        )
        .then(
            select_ref! { Token::InlineTsx(tsx_source) => tsx_source.clone() }
                .map_with(|x, e| (x, e.span())),
        )
        .map(|((identifier, params), tsx_source)| TsxComponent {
            name: identifier.clone(),
            params,
            typescript_source: tsx_source.clone(),
        })
}

#[cfg(test)]
mod tests {
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

    use super::*;

    #[test]
    fn test_component_parser() {
        let src_and_expected_ast = vec![(
            "component T() tsx {useState()}",
            TsxComponent {
                name: "T".to_string(),
                params: Vec::new(),
                typescript_source: ("useState()".to_string(), empty_range()),
            },
        )];

        for (src, expected_ast) in src_and_expected_ast {
            println!("lexing {src}");
            let lexer_parse_result = lex_parser("test", "").parse(src);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("parsing component statement {src}");
            let component_parse_result =
                tsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(component_parse_result.has_errors(), false);
            assert_eq!(component_parse_result.has_output(), true);

            let Some(ast) = component_parse_result.into_output() else {
                unreachable!()
            };

            let mut ast = ast;
            ast.typescript_source.1 = empty_range();

            assert_eq!(ast, expected_ast);
        }

        let invalid_component_statements = vec![
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

        for invalid_component_statement in invalid_component_statements {
            println!("lexing {invalid_component_statement}");

            let lexer_parse_result = lex_parser("test", "").parse(invalid_component_statement);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("component parser try invalid {invalid_component_statement}");

            let component_parse_result =
                tsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));

            assert_eq!(component_parse_result.has_errors(), true);
            assert_eq!(component_parse_result.has_output(), false);
        }
    }
}
