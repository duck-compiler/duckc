use chumsky::prelude::*;
use chumsky::Parser;

use super::lexer::Token;

#[derive(Debug, Clone)]
pub struct TypeDefinition {
    pub name: String,
    pub type_expression: TypeExpression,
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Any,
    Duck(Duck),
    TypeName(String),
}

#[derive(Debug, Clone)]
pub struct Duck {
    pub fields: Vec<(String, TypeExpression)>,
}

pub fn type_expression_parser<'src>() -> impl Parser<'src, &'src [Token], TypeExpression> {
    recursive(|p| {
        let field = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(p);

        let fields = field
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<(String, TypeExpression)>>();

        let duck = just(Token::Duck).or_not()
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(fields.or_not())
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| match fields {
                Some(fields) => TypeExpression::Duck(Duck { fields }),
                None => TypeExpression::Any,
            });

        let type_name = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .map(|identifier| TypeExpression::TypeName(identifier));

        type_name.or(duck)
    })
}

pub fn type_def_parser<'src>() -> impl Parser<'src, &'src [Token], TypeDefinition> {
    just(Token::Type)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('=')))
        .then(type_expression_parser())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(identifier, type_expression)| TypeDefinition { name: identifier, type_expression })
}

#[cfg(test)]
pub mod tests {
    use chumsky::Parser;
    use crate::parse::lexer::lexer;

    use super::*;

    #[test]
    pub fn test_type_definition_parser() {
        let valid_type_definitions = vec![
            "type X = Hallo;",
            "type Yolo = duck { x: String };",
            "type whatEverY = {};",
        ];

        for valid_type_definition in valid_type_definitions {
            println!("lexing {valid_type_definition}");
            let lexer_parse_result = lexer().parse(valid_type_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_type_definition}");
            let typedef_parse_result = type_def_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    pub fn test_type_expression_parser() {
        let valid_type_expressions = vec![
            "duck {}",
            "{}",
            "duck { x: String }",
            "duck { x: duck { y: String } }",
            "duck { x: String, }",
            "duck { x: String, y : {}, }",
            "{ x: { y: String } }",
            "String",
            "{ x: {}, y: {}, z: {} }",
            "duck { x: duck {}, y: duck {}, z: duck {} }",
            "duck { x: String, y: duck {}, z: {}, w: { a: String, b: {}, c: duck { aa: String } } }",
        ];

        for valid_type_expression in valid_type_expressions {
            println!("lexing {valid_type_expression}");
            let lexer_parse_result = lexer().parse(valid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_type_expression}");
            let typedef_parse_result = type_expression_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_type_expressions = vec![
            "{ x }",
            "{ x: String,, }",
            "{ x:{ }",
            "{ y:} }",
            "{ y: }",
            "x: {}",
            "{ {}: x }",
            "{ x: String",
            "x: String }",
            "{ x: duck { }",
        ];

        for invalid_type_expression in invalid_type_expressions {
            println!("lexing {invalid_type_expression}");
            let lexer_parse_result = lexer().parse(invalid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {invalid_type_expression}");
            let typedef_parse_result = type_expression_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
