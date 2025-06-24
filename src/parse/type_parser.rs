use chumsky::Parser;
use chumsky::prelude::*;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: String,
    pub type_expression: TypeExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_expr: TypeExpr,
}

impl Field {
    pub fn new(name: String, type_expr: TypeExpr) -> Self {
        return Self { name, type_expr };
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Duck {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Any,
    Struct(Struct),
    Go(String),
    Duck(Duck),
    Tuple(Vec<TypeExpr>),
    TypeName(String),
    String,
    Int,
    Bool,
    Char,
    Float,
    Or(Vec<TypeExpr>),
    Fun(Vec<(Option<String>, TypeExpr)>, Option<Box<TypeExpr>>),
}

impl TypeExpr {}

pub fn type_expression_parser<'src>() -> impl Parser<'src, &'src [Token], TypeExpr> + Clone {
    recursive(|p| {
        let field = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(p.clone());

        let duck_fields = field
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<(String, TypeExpr)>>();

        let struct_fields = field
            .separated_by(just(Token::ControlChar(',')))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<(String, TypeExpr)>>();

        let go_type_identifier: impl Parser<'src, &'src [Token], String> =
            select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .separated_by(just(Token::ControlChar('.')))
                .at_least(1)
                .at_most(2)
                .collect::<Vec<String>>()
                .map(|str| str.join("."));

        let go_type = just(Token::Go)
            .ignore_then(go_type_identifier)
            .map(|go_type_identifier| TypeExpr::Go(go_type_identifier));

        let r#struct = just(Token::Struct)
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(struct_fields)
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| {
                TypeExpr::Struct(Struct {
                    fields: fields
                        .iter()
                        .cloned()
                        .map(|(name, type_expr)| Field { name, type_expr })
                        .collect(),
                })
            });

        let duck = just(Token::Duck)
            .or_not()
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(duck_fields.or_not())
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| match fields {
                Some(mut fields) => {
                    fields.sort_by_key(|x| x.0.clone());
                    TypeExpr::Duck(Duck {
                        fields: fields
                            .iter()
                            .cloned()
                            .map(|(name, type_expr)| Field { name, type_expr })
                            .collect(),
                    })
                }
                None => TypeExpr::Any,
            });

        let tuple = p
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(TypeExpr::Tuple);

        let type_name =
            select_ref! { Token::Ident(identifier) => identifier.to_string() }.map(|identifier| {
                match identifier.as_str() {
                    "Int" => TypeExpr::Int,
                    "Float" => TypeExpr::Float,
                    "Bool" => TypeExpr::Bool,
                    "String" => TypeExpr::String,
                    "Char" => TypeExpr::Char,
                    _ => TypeExpr::TypeName(identifier),
                }
            });

        go_type.or(type_name).or(r#struct).or(duck).or(tuple)
    })
}

pub fn type_definition_parser<'src>() -> impl Parser<'src, &'src [Token], TypeDefinition> + Clone {
    just(Token::Type)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('=')))
        .then(type_expression_parser())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(identifier, type_expression)| TypeDefinition {
            name: identifier,
            type_expression,
        })
}

#[cfg(test)]
pub mod tests {
    use crate::parse::lexer::lexer;
    use chumsky::Parser;

    use super::*;

    #[test]
    fn test_type_definition_parser() {
        let valid_type_definitions = vec![
            "type X = Hallo;",
            "type Yolo = duck { x: String };",
            "type whatEverY = {};",
            "type EmptyTup = ();",
            "type Tup = (Int, String);",
            "type Tup = (Int, String,);",
            "type Tup = (Int, String, (Float, {x: String}));",
            "type Tup = (Int, String, (Float, {x: String}),);",
            "type Tup = (Int, String, (Float, {x: String}),);",
            "type Tup = go fmt;",
            "type Tup = go sync.WaitGroup;",
            "type Struct = struct { x: String };",
            "type Struct = struct { x: String, y: String, z: String, };",
            "type Struct = struct { x: String, y: String, z: String, };",
        ];

        for valid_type_definition in valid_type_definitions {
            println!("lexing {valid_type_definition}");
            let lexer_parse_result = lexer().parse(valid_type_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_type_definition}");
            let typedef_parse_result = type_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    fn test_type_expression_parser() {
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
            "go sync.WaitGroup",
        ];

        for valid_type_expression in valid_type_expressions {
            println!("lexing {valid_type_expression}");
            let lexer_parse_result = lexer().parse(valid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

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
            "struct {}",
            "struct {,}",
        ];

        for invalid_type_expression in invalid_type_expressions {
            println!("lexing {invalid_type_expression}");
            let lexer_parse_result = lexer().parse(invalid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_type_expression}");
            let typedef_parse_result = type_expression_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
