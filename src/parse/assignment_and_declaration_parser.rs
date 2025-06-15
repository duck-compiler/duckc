use chumsky::select_ref;
use chumsky::{prelude::just, Parser};

use super::type_parser::{type_expression_parser, TypeExpression};
use super::lexer::Token;
use super::value_parser::{value_expr_parser, ValueExpr};

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: String,
    pub type_expr: TypeExpression,
    pub initializer: Option<ValueExpr>,
}

// TODO parse initializer
fn declaration_parser<'src>() -> impl Parser<'src, &'src [Token], Declaration> {
    let initializer = just(Token::ControlChar('='))
        .ignore_then(value_expr_parser())
        .or_not();

    just(Token::Let)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .then(initializer)
        .then_ignore(just(Token::ControlChar(';')))
        .map(|((identifier, type_expr), initializer)| Declaration { name: identifier, type_expr, initializer })
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub name: String,
    pub value_expr: ValueExpr,
}

fn assignment_parser<'src>() -> impl Parser<'src, &'src [Token], Assignment> {
    select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar('=')))
        .then(value_expr_parser())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(identifier, value_expr)| Assignment { name: identifier, value_expr })
}

#[cfg(test)]
pub mod tests {
    use crate::parse::lexer::lexer;

    use super::*;

    #[test]
    pub fn test_declaration_parser() {
        let valid_declarations = vec![
            "let x: String;",
            "let x: { x: String, y: String };",
            "let y: { x: String, y: String };",
            "let z: { h: String, x: { y: String }};",
            "let x: { h: String, x: { y: String }} = 0;",
            "let x: { h: String, x: { y: String }} = true;",
            "let x: { h: String, x: { y: String }} = false;",
            "let x: Int = false;",
            "let x: String = \"Hallo, Welt!\";",
        ];

        for valid_declaration in valid_declarations {
            println!("lexing {valid_declaration}");
            let lexer_parse_result = lexer().parse(valid_declaration);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_declaration}");
            let typedef_parse_result = declaration_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    pub fn test_assignment_parser() {
        let valid_assignments = vec![
            "x = 580;",
            "y = 80;",
            "y = true;",
            "y = false;",
            "y = \"Hallo\";",
        ];

        for valid_assignment in valid_assignments {
            println!("lexing {valid_assignment}");
            let lexer_parse_result = lexer().parse(valid_assignment);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_assignment}");
            let typedef_parse_result = assignment_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }
}
