use crate::parse::{
    lexer::lex_parser,
    make_input,
    value_parser::{empty_range, type_expr_into_empty_range},
};

use crate::parse::jsx_component_parser::*;

#[test]
fn test_component_parser() {
    let src_and_expected_ast = vec![(
        "component T() jsx {useState()}",
        JsxComponent {
            name: "T".to_string(),
            props_type: TypeExpr::Duck(Duck { fields: Vec::new() }).into_empty_span(),
            javascript_source: ("useState()".to_string(), empty_range()),
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
            jsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));
        assert_eq!(component_parse_result.has_errors(), false);
        assert_eq!(component_parse_result.has_output(), true);

        let Some(ast) = component_parse_result.into_output() else {
            unreachable!()
        };

        let mut ast = ast;
        ast.javascript_source.1 = empty_range();
        type_expr_into_empty_range(&mut ast.props_type);

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
            jsx_component_parser().parse(make_input(empty_range(), tokens.as_slice()));

        assert_eq!(component_parse_result.has_errors(), true);
        assert_eq!(component_parse_result.has_output(), false);
    }
}
