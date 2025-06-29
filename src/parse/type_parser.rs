use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::parse::{SS, Spanned, value_parser::empty_range};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: String,
    pub type_expression: Spanned<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_expr: Spanned<TypeExpr>,
}

impl Field {
    pub fn new(name: String, type_expr: Spanned<TypeExpr>) -> Self {
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
    InlineGo,
    Struct(Struct),
    Go(String),
    Duck(Duck),
    Tuple(Vec<Spanned<TypeExpr>>),
    TypeName(bool, String),
    String,
    Int,
    Bool,
    Char,
    Float,
    Or(Vec<Spanned<TypeExpr>>),
    Fun(
        Vec<(Option<String>, Spanned<TypeExpr>)>,
        Option<Box<Spanned<TypeExpr>>>,
    ),
}

impl TypeExpr {
    pub fn into_empty_span(self) -> Spanned<TypeExpr> {
        (self, empty_range())
    }
}

pub fn type_expression_parser<'src, I>()
-> impl Parser<'src, I, Spanned<TypeExpr>, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    recursive(
        |p: Recursive<dyn Parser<'_, _, Spanned<TypeExpr>, extra::Err<Rich<'src, Token, SS>>>>| {
            let field = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(p.clone());

            let duck_fields = field
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<(String, Spanned<TypeExpr>)>>();

            let struct_fields = field
                .separated_by(just(Token::ControlChar(',')))
                .at_least(1)
                .allow_trailing()
                .collect::<Vec<(String, Spanned<TypeExpr>)>>();

            let go_type_identifier: impl Parser<'src, I, String, extra::Err<Rich<'src, Token, SS>>> =
                select_ref! { Token::Ident(identifier) => identifier.to_string() }
                    .separated_by(just(Token::ControlChar('.')))
                    .at_least(1)
                    .at_most(2)
                    .collect::<Vec<String>>()
                    .map(|str| str.join("."));

            let go_type = just(Token::Go)
                .ignore_then(go_type_identifier)
                .map(TypeExpr::Go);

            let r#struct = just(Token::Struct)
                .ignore_then(just(Token::ControlChar('{')))
                .ignore_then(struct_fields)
                .then_ignore(just(Token::ControlChar('}')))
                .map(|fields| {
                    TypeExpr::Struct(Struct {
                        fields: fields
                            .iter()
                            .cloned()
                            .map(|(name, (type_expr, e))| Field {
                                name,
                                type_expr: (type_expr, e),
                            })
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
                                .map(|(name, (type_expr, e))| Field {
                                    name,
                                    type_expr: (type_expr, e),
                                })
                                .collect(),
                        })
                    }
                    _ => TypeExpr::Any,
                });

            let tuple = p
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .map(TypeExpr::Tuple);

            let type_name = just(Token::ScopeRes)
                .or_not()
                .then(
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .separated_by(just(Token::ScopeRes))
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .map(|(is_global, identifier)| match identifier[0].as_str() {
                    "Int" => TypeExpr::Int,
                    "Float" => TypeExpr::Float,
                    "Bool" => TypeExpr::Bool,
                    "String" => TypeExpr::String,
                    "Char" => TypeExpr::Char,
                    _ => TypeExpr::TypeName(is_global.is_some(), identifier.join("_")),
                });

            choice((go_type, type_name, r#struct, duck, tuple)).map_with(|x, e| (x, e.span()))
        },
    )
}

pub fn type_definition_parser<'src, I>()
-> impl Parser<'src, I, TypeDefinition, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
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
    use crate::parse::{lexer::lexer, make_input, value_parser::empty_range};
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
            "type Tup = (::Int, String);",
            "type Tup = (Int, String,);",
            "type Tup = (Int, String, (Float, {x: String}));",
            "type Tup = (Int, String, (Float, {x: String}),);",
            "type Tup = (Int, String, (Float, {x: String}),);",
            "type Tup = go fmt;",
            "type Tup = go sync.WaitGroup;",
            "type Struct = struct { x: String };",
            "type Struct = struct { x: String, y: String, z: String, };",
            "type Struct = struct { x: String, y: String, z: String, };",
            "type X = ::String;",
            "type X = String::ABC::C;",
            "type X = ::String::ABC::C;",
        ];

        for valid_type_definition in valid_type_definitions {
            println!("lexing {valid_type_definition}");
            let lexer_parse_result = lexer("test", "").parse(valid_type_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_type_definition}");
            let typedef_parse_result =
                type_definition_parser().parse(make_input(empty_range(), tokens.as_slice()));
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
            "::X",
            "::X::Y",
            "X::Y::Z",
        ];

        for valid_type_expression in valid_type_expressions {
            println!("lexing {valid_type_expression}");
            let lexer_parse_result = lexer("test", "").parse(valid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_type_expression}");
            let typedef_parse_result =
                type_expression_parser().parse(make_input(empty_range(), tokens.as_slice()));
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
            "::",
            "X:Y:Z:",
            "Y::Y::",
        ];

        for invalid_type_expression in invalid_type_expressions {
            println!("lexing {invalid_type_expression}");
            let lexer_parse_result = lexer("test", "").parse(invalid_type_expression);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_type_expression}");
            let typedef_parse_result =
                type_expression_parser().parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
