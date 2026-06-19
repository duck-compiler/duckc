use crate::parse::function_parser::*;
use crate::parse::{
    lexer::lex_parser,
    make_input,
    value_parser::{empty_range, type_expr_into_empty_range, value_expr_into_empty_range},
};

#[test]
fn test_function_parser() {
    let valid_function_definitions = vec![
        "fn x(){}",
        "fn x(x: String){}",
        "fn x(x: { hallo: String, x: { y: {} }}){}",
        "fn x() -> String {}",
        "fn x() -> {x: String} {}",
        "fn x() -> {x: String} { 5; }",
        "fn x() -> {x: String} { 5; }",
        "fn x<TYPE>() -> {x: String} { 5; }",
        "fn x<TYPE, TYPE2>() -> {x: String} { 5; }",
        "fn x<TYPE, TYPE2, TYPE3>() -> {x: String} { 5; }",
    ];

    for valid_function_definition in valid_function_definitions {
        println!("lexing {valid_function_definition}");
        let lexer_parse_result = lex_parser("test", "").parse(valid_function_definition);
        assert_eq!(lexer_parse_result.has_errors(), false);
        assert_eq!(lexer_parse_result.has_output(), true);

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!()
        };

        println!("typedef_parsing {valid_function_definition}");
        let typedef_parse_result =
            function_definition_parser(make_input).parse(make_input(empty_range(), &tokens));
        assert_eq!(typedef_parse_result.has_errors(), false);
        assert_eq!(typedef_parse_result.has_output(), true);
    }

    let invalid_function_definitions = vec![];

    for invalid_function_definition in invalid_function_definitions {
        println!("lexing {invalid_function_definition}");
        let lexer_parse_result = lex_parser("test", "").parse(invalid_function_definition);
        assert_eq!(lexer_parse_result.has_errors(), false);
        assert_eq!(lexer_parse_result.has_output(), true);

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!()
        };

        println!("typedef_parsing {invalid_function_definition}");
        let typedef_parse_result = function_definition_parser(make_input)
            .parse(make_input(empty_range(), tokens.as_slice()));
        assert_eq!(typedef_parse_result.has_errors(), true);
        assert_eq!(typedef_parse_result.has_output(), false);
    }
}

#[test]
fn test_detailed_function_definitions() {
    let test_cases = vec![
        (
            "fn y<TYPENAME>() {}",
            FunctionDefintion {
                name: "y".to_string(),
                params: vec![],
                return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                generics: vec![(
                    Generic {
                        name: "TYPENAME".to_string(),
                        constraint: None,
                    },
                    empty_range(),
                )],
                value_expr: ValueExpr::Return(Some(
                    ValueExpr::Block(vec![]).into_empty_span().into(),
                ))
                .into_empty_span(),
                span: empty_range(),
                comments: Vec::new(),
            },
        ),
        (
            "fn y<TYPENAME, TYPENAME2>() {}",
            FunctionDefintion {
                name: "y".to_string(),
                params: vec![],
                return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                generics: vec![
                    (
                        Generic {
                            name: "TYPENAME".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                    (
                        Generic {
                            name: "TYPENAME2".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                ],
                value_expr: ValueExpr::Return(Some(
                    ValueExpr::Block(vec![]).into_empty_span().into(),
                ))
                .into_empty_span(),
                span: empty_range(),
                comments: Vec::new(),
            },
        ),
        (
            "fn y<TYPENAME, TYPENAME2, TYPENAME3>() {}",
            FunctionDefintion {
                name: "y".to_string(),
                params: vec![],
                return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                generics: vec![
                    (
                        Generic {
                            name: "TYPENAME".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                    (
                        Generic {
                            name: "TYPENAME2".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                    (
                        Generic {
                            name: "TYPENAME3".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                ],
                value_expr: ValueExpr::Return(Some(
                    ValueExpr::Block(vec![]).into_empty_span().into(),
                ))
                .into_empty_span(),
                span: empty_range(),
                comments: Vec::new(),
            },
        ),
    ];

    for (i, (src, expected_fns)) in test_cases.into_iter().enumerate() {
        let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
        let parse_result =
            function_definition_parser(make_input).parse(make_input(empty_range(), &lex_result));

        assert_eq!(
            parse_result.has_errors(),
            false,
            "{i}: {} {:?} {:?}",
            src,
            lex_result,
            parse_result
        );

        assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

        let mut output = parse_result.into_result().expect(&src);

        output.generics.iter_mut().for_each(|generic| {
            *generic = (generic.0.clone(), empty_range());
        });

        type_expr_into_empty_range(&mut output.return_type);
        value_expr_into_empty_range(&mut output.value_expr);
        output.span = empty_range();

        assert_eq!(output, expected_fns, "{i}: {}", src);
    }
}
