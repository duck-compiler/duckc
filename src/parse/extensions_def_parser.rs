use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{
    function_parser::{function_definition_parser, FunctionDefintion}, Spanned, SS
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
};

pub type Param = (String, Spanned<TypeExpr>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionsDef {
    pub target_type_expr: Spanned<TypeExpr>,
    pub function_definitions: Vec<Spanned<FunctionDefintion>>,
    pub span: SS,
}

pub fn extensions_def_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, ExtensionsDef, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    just(Token::Extend)
        .ignore_then(type_expression_parser())
        .then(
            function_definition_parser(make_input)
                .map_with(|fn_def, ctx| (fn_def, ctx.span()))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
        )
        .map_with(|(target_type_expr, function_definitions), ctx| ExtensionsDef {
            target_type_expr,
            function_definitions,
            span: ctx.span()
        })
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};
    use super::*;

    #[test]
    fn test_extensions_definition_parsing() {
        let valid_extensions_definitions = vec![
            "extend Int { fn to_str() -> String { return \"h\" } }",
        ];

        for valid_extensions_defintion in valid_extensions_definitions {
            println!("lexing {valid_extensions_defintion}");

            let lexer_parse_result = lex_parser("test", "").parse(valid_extensions_defintion);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("extensions definition parsing {valid_extensions_defintion}");

            let extensions_def_parse_result = extensions_def_parser(make_input)
                .parse(make_input(empty_range(), &tokens));

            assert_eq!(extensions_def_parse_result.has_errors(), false);
            assert_eq!(extensions_def_parse_result.has_output(), true);
        }

        let invalid_extensions_defs = vec![];
        for invalid_extensions_def in invalid_extensions_defs {
            println!("lexing {invalid_extensions_def}");

            let lexer_parse_result = lex_parser("test", "").parse(invalid_extensions_def);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_extensions_def}");

            let extensions_def_parse_result = extensions_def_parser(make_input)
                .parse(make_input(empty_range(), tokens.as_slice()));

            assert_eq!(extensions_def_parse_result.has_errors(), true);
            assert_eq!(extensions_def_parse_result.has_output(), false);
        }
    }

    #[test]
    fn test_detailed_function_definitions() {
        // // let test_cases = vec![
        // // ];

        // for (i, (src, expected_extensions_defs)) in test_cases.into_iter().enumerate() {
        //     let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
        //     let parse_result = extensions_def_parser(make_input)
        //         .parse(make_input(empty_range(), &lex_result));

        //     assert_eq!(
        //         parse_result.has_errors(),
        //         false,
        //         "{i}: {} {:?} {:?}",
        //         src,
        //         lex_result,
        //         parse_result
        //     );

        //     assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

        //     // let mut output = parse_result.into_result().expect(&src);
        //     // output
        //     //     .generics
        //     //     .as_mut()
        //     //     .unwrap()
        //     //     .iter_mut()
        //     //     .for_each(|generic| {
        //     //         *generic = (generic.0.clone(), empty_range());
        //     //     });
        //     // output.span = empty_range();
        //     // output.value_expr = ValueExpr::Block(vec![]).into_empty_span();

        //     // assert_eq!(output, expected_extensions_defs, "{i}: {}", src);
        // }
    }
}
