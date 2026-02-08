use chumsky::{input::BorrowInput, prelude::*};
use serde::{Deserialize, Serialize};

use crate::parse::{
    SS, Spanned,
    function_parser::{FunctionDefintion, function_definition_parser},
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
};

pub type Param = (String, Spanned<TypeExpr>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(bound(deserialize = "'de: 'static"))]
pub struct ExtensionsDef {
    pub target_type_expr: Spanned<TypeExpr>,
    pub function_definitions: Vec<Spanned<FunctionDefintion>>,
    pub doc_comments: Vec<Spanned<String>>,
    pub span: SS,
}

pub fn extensions_def_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, ExtensionsDef, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    doc_comments_parser
        .then_ignore(just(Token::Extend))
        .then(type_expression_parser())
        .then_ignore(just(Token::With))
        .then_ignore(just(Token::Impl))
        .then(
            function_definition_parser(make_input)
                .map_with(|fn_def, ctx| (fn_def, ctx.span()))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))),
        )
        .map_with(
            |((doc_comments, target_type_expr), function_definitions), ctx| ExtensionsDef {
                target_type_expr,
                function_definitions,
                span: ctx.span(),
                doc_comments: doc_comments.unwrap_or_else(Vec::new),
            },
        )
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

    #[test]
    fn test_extensions_definition_parsing() {
        let valid_extensions_definitions =
            vec!["extend Int with impl { fn to_str() -> String { return \"h\" } }"];

        for valid_extensions_defintion in valid_extensions_definitions {
            println!("lexing {valid_extensions_defintion}");

            let lexer_parse_result = lex_parser("test", "").parse(valid_extensions_defintion);

            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("extensions definition parsing {valid_extensions_defintion}");

            let extensions_def_parse_result =
                extensions_def_parser(make_input).parse(make_input(empty_range(), &tokens));

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
}
