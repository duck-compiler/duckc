use crate::parse::extensions_def_parser::*;
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

        let extensions_def_parse_result =
            extensions_def_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));

        assert_eq!(extensions_def_parse_result.has_errors(), true);
        assert_eq!(extensions_def_parse_result.has_output(), false);
    }
}
