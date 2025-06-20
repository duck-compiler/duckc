use chumsky::prelude::*;
use chumsky::Parser;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: String,
    pub type_expression: TypeExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    Any,
    Duck(Duck),
    Tuple(Vec<TypeExpression>),
    TypeName(String),
}

impl TypeExpression {

    /// First return is go code repr, second is id
    pub fn emit(&self) -> (String, String) {
        match self {
            TypeExpression::Tuple(types) => ([
                "struct {\n",
                &types.iter()
                    .enumerate()
                    .map(|(i, x)| format!("field_{i} {}\n", x.emit().0))
                    .collect::<Vec<_>>().join(""),
                "}"
            ].join(""), format!("Tup{}", types.iter().map(|x| format!("_{}", x.emit().1)).collect::<Vec<_>>().join(""))),
            TypeExpression::Duck(Duck { fields }) => {
                let mut fields = fields.clone();
                fields.sort_by_key(|x| x.0.clone());
                if fields.is_empty() {
                    ("interface{}".to_string(), "Any".to_string())
                } else {
                    let name = format!("Duck{}", fields.into_iter().map(|x| format!("_Has{}{}", x.0, x.1.emit().1)).collect::<Vec<_>>().join(""));
                    (name.clone(), name)
                }
            }
            TypeExpression::TypeName(x) => (x.clone(), x.clone()),
            _ => todo!(),
        }
    }

}

#[derive(Debug, Clone, PartialEq)]
pub struct Duck {
    pub fields: Vec<(String, TypeExpression)>,
}

pub fn type_expression_parser<'src>() -> impl Parser<'src, &'src [Token], TypeExpression> + Clone {
    recursive(|p| {
        let field = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(p.clone());

        let fields = field
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<(String, TypeExpression)>>();

        let duck = just(Token::Duck).or_not()
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(fields.or_not())
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| match fields {
                Some(mut fields) => {
                    fields.sort_by_key(|x| x.0.clone());
                    TypeExpression::Duck(Duck { fields })
                },
                None => TypeExpression::Any,
            });

        let tuple = p.clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(TypeExpression::Tuple);

        let type_name = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .map(TypeExpression::TypeName);

        type_name.or(duck).or(tuple)
    })
}

pub fn type_definition_parser<'src>() -> impl Parser<'src, &'src [Token], TypeDefinition> + Clone {
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
        ];

        for valid_type_definition in valid_type_definitions {
            println!("lexing {valid_type_definition}");
            let lexer_parse_result = lexer().parse(valid_type_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_type_definition}");
            let typedef_parse_result = type_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    fn test_emit() {
        let test_cases = vec![
            (
                "()",
                (
                    "struct {\n}",
                    "Tup"
                )
            ),
            (
                "((), {})",
                (
                    "struct {\nfield_0 struct {\n}\nfield_1 interface{}\n}",
                    "Tup_Tup_Any"
                )
            ),
            ("{}",("interface{}", "Any")),
            ("({})", ("struct {\nfield_0 interface{}\n}", "Tup_Any")),
            ("{x: String, y: ({}, Int)}", ("Duck_HasxString_HasyTup_Any_Int", "Duck_HasxString_HasyTup_Any_Int")),
        ];

        for (src, exp) in test_cases {
            let lex = lexer().parse(src).unwrap();
            let parse = type_expression_parser().parse(&lex).unwrap().emit();
            let exp = (exp.0.to_string(), exp.1.to_string());
            assert_eq!(parse, exp, "{src}");
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
