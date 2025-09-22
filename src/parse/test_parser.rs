use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::parse::value_parser::value_expr_parser;
use crate::{
    parse::{
        value_parser::ValueExpr, Spanned, SS
    },
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TestCase {
    pub name: String,
    pub body: Spanned<ValueExpr>,
}

pub fn test_parser<'src, I, M>(make_input: M) -> impl Parser<'src, I, Spanned<TestCase>, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    just(Token::Test)
        .ignore_then(select_ref! { Token::ConstString(str) => str.clone() })
        .then(value_expr_parser(make_input))
        .map_with(|(name, mut body), ctx| {
            body = match body {
                (ValueExpr::Duck(x), loc) if x.is_empty() => (ValueExpr::Block(vec![]), loc),
                block @ (ValueExpr::Block(_), _) => block,
                _ => panic!("Function must be block"),
            };

            (TestCase { name, body }, ctx.span())
        })
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{
        lexer::lex_parser,
        make_input,
        value_parser::{empty_range, value_expr_into_empty_range},
    };
    use chumsky::Parser;

    use super::*;

    fn strip_spans(spanned_type_expr: Spanned<TestCase>) -> Spanned<TestCase> {
        let (mut expr, _) = spanned_type_expr;
        expr.body.1 = empty_range();
        value_expr_into_empty_range(&mut expr.body);
        (expr, empty_range())
    }

    fn assert_test_case(input_str: &str, expected_expr: TestCase) {
        println!("lexing and parsing: \"{}\"", input_str);
        let lexer_parse_result = lex_parser("test", "").parse(input_str);
        assert!(
            !lexer_parse_result.has_errors(),
            "lexing errors for \"{}\": {:?}",
            input_str,
            lexer_parse_result
                .errors()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
        );

        assert!(
            lexer_parse_result.has_output(),
            "lexer produced no output for \"{}\"",
            input_str
        );

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!();
        };

        let parse_result = test_parser(make_input)
            .parse(make_input(empty_range(), &tokens));

        assert!(
            !parse_result.has_errors(),
            "parsing errors for \"{}\": {:?}",
            input_str,
            parse_result
                .errors()
                .map(|err| err.to_string())
                .collect::<Vec<_>>(),
        );

        assert!(
            parse_result.has_output(),
            "parser produced no output for \"{}\"",
            input_str
        );

        let parsed = parse_result.into_output().unwrap();
        let parsed = strip_spans(parsed);

        let stripped_parsed = strip_spans(parsed);

        assert_eq!(
            stripped_parsed.0, expected_expr,
            "mismatch for \"{}\"",
            input_str
        );
    }

    #[test]
    fn test_assert_type() {
        assert_test_case(
            "test \"lol\" { return }",
            TestCase {
                name: "lol".to_string(),
                body: (
                    ValueExpr::Block(vec![(ValueExpr::Return(None), empty_range())]),
                    empty_range()
                )
            },
        );
    }
}
