use crate::parse::{
    lexer::lex_parser,
    make_input,
    value_parser::{empty_range, value_expr_into_empty_range},
};
use chumsky::Parser;

use crate::parse::test_parser::*;

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

    let parse_result = test_parser(make_input).parse(make_input(empty_range(), &tokens));

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
            body: ValueExpr::Return(Some(ValueExpr::Tuple(vec![]).into_empty_span().into()))
                .into_empty_span_and_block(),
        },
    );
}
