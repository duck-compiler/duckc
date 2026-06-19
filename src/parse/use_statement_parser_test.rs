use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

use crate::parse::use_statement_parser::*;

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
