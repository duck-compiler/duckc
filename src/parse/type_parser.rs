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
    TypeNameInternal(String),
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

    pub fn primitives() -> Vec<TypeExpr> {
        return vec![
            TypeExpr::Int,
            TypeExpr::Float,
            TypeExpr::Bool,
            TypeExpr::String,
            TypeExpr::Char,
        ];
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

            let function_fields = duck_fields.clone();

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
                        if fields.is_empty() {
                            return TypeExpr::Any;
                        }

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

            let function = just(Token::ControlChar('('))
                .ignore_then(function_fields)
                .then_ignore(just(Token::ControlChar(')')))
                .then_ignore(just(Token::ControlChar('-')))
                .then_ignore(just(Token::ControlChar('>')))
                .then(p.clone())
                .map(|(fields, return_type)| {
                    TypeExpr::Fun(
                        fields
                            .iter()
                            .map(|field| (Some(field.0.clone()), field.1.clone()))
                            .collect::<Vec<_>>(),
                        Some(Box::new(return_type.clone())),
                    )
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

            let term_type_expr = choice((go_type, type_name, r#struct, duck, function, tuple))
                .map_with(|x, e| (x, e.span()));

            term_type_expr
                .separated_by(just(Token::ControlChar('|')))
                .at_least(1)
                .collect::<Vec<Spanned<TypeExpr>>>()
                .map_with(|elements, e| {
                    if elements.len() == 1 {
                        elements.into_iter().next().unwrap()
                    } else {
                        (TypeExpr::Or(elements), e.span())
                    }
                })
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

    fn strip_spans(spanned_type_expr: Spanned<TypeExpr>) -> Spanned<TypeExpr> {
        let (expr, _span) = spanned_type_expr;
        let stripped_expr = match expr {
            TypeExpr::Struct(s) => TypeExpr::Struct(Struct {
                fields: s
                    .fields
                    .into_iter()
                    .map(|field| Field {
                        name: field.name,
                        type_expr: strip_spans(field.type_expr),
                    })
                    .collect(),
            }),
            TypeExpr::Duck(d) => TypeExpr::Duck(Duck {
                fields: d
                    .fields
                    .into_iter()
                    .map(|field| Field {
                        name: field.name,
                        type_expr: strip_spans(field.type_expr),
                    })
                    .collect(),
            }),
            TypeExpr::Tuple(t) => TypeExpr::Tuple(t.into_iter().map(strip_spans).collect()),
            TypeExpr::Fun(params, return_type) => TypeExpr::Fun(
                params
                    .into_iter()
                    .map(|(name, param_type_expr)| (name, strip_spans(param_type_expr)))
                    .collect(),
                return_type.map(|rt_box| Box::new(strip_spans(*rt_box))),
            ),
            TypeExpr::Or(variants) => TypeExpr::Or(variants.into_iter().map(strip_spans).collect()),
            other => other,
        };
        (stripped_expr, empty_range())
    }

    fn assert_type_expression(input_str: &str, expected_expr: TypeExpr) {
        println!("lexing and parsing: \"{}\"", input_str);
        let lexer_parse_result = lexer("test", "").parse(input_str);
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

        let parse_result = type_expression_parser().parse(make_input(empty_range(), &tokens));

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

        let stripped_parsed = strip_spans(parsed);

        assert_eq!(
            stripped_parsed.0, expected_expr,
            "mismatch for \"{}\"",
            input_str
        );
    }

    #[test]
    fn test_assert_type() {
        assert_type_expression(
            "() -> String",
            TypeExpr::Fun(vec![], Some(Box::new(TypeExpr::String.into_empty_span()))),
        );

        assert_type_expression(
            "(x: Int) -> Bool",
            TypeExpr::Fun(
                vec![("x".to_string().into(), TypeExpr::Int.into_empty_span())],
                Some(Box::new(TypeExpr::Bool.into_empty_span())),
            ),
        );

        assert_type_expression(
            "(a: Float, b: String) -> Char",
            TypeExpr::Fun(
                vec![
                    ("a".to_string().into(), TypeExpr::Float.into_empty_span()),
                    ("b".to_string().into(), TypeExpr::String.into_empty_span()),
                ],
                Some(Box::new(TypeExpr::Char.into_empty_span())),
            ),
        );

        assert_type_expression(
            "(param1: TypeName,) -> ()",
            TypeExpr::Fun(
                vec![(
                    "param1".to_string().into(),
                    TypeExpr::TypeName(false, "TypeName".to_string()).into_empty_span(),
                )],
                Some(Box::new(TypeExpr::Tuple(Vec::new()).into_empty_span())),
            ),
        );

        // TODO
        assert_type_expression(
            "(data: duck { name: String, age: Int }) -> ::MyResult",
            TypeExpr::Fun(
                vec![(
                    "data".to_string().into(),
                    TypeExpr::Duck(Duck {
                        fields: vec![
                            Field::new("age".to_string(), TypeExpr::Int.into_empty_span()),
                            Field::new("name".to_string(), TypeExpr::String.into_empty_span()),
                        ],
                    })
                    .into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::TypeName(true, "MyResult".to_string()).into_empty_span(),
                )),
            ),
        );

        assert_type_expression(
            "() -> (Int, String)",
            TypeExpr::Fun(
                vec![],
                Some(Box::new(
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int.into_empty_span(),
                        TypeExpr::String.into_empty_span(),
                    ])
                    .into_empty_span(),
                )),
            ),
        );

        assert_type_expression(
            "(x: Int) -> go fmt.Stringer",
            TypeExpr::Fun(
                vec![("x".to_string().into(), TypeExpr::Int.into_empty_span())],
                Some(Box::new(
                    TypeExpr::Go("fmt.Stringer".to_string()).into_empty_span(),
                )),
            ),
        );

        assert_type_expression(
            "(input: struct { id: Int }) -> struct { success: Bool }",
            TypeExpr::Fun(
                vec![(
                    "input".to_string().into(),
                    TypeExpr::Struct(Struct {
                        fields: vec![Field::new(
                            "id".to_string(),
                            TypeExpr::Int.into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::Struct(Struct {
                        fields: vec![Field::new(
                            "success".to_string(),
                            TypeExpr::Bool.into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                )),
            ),
        );

        assert_type_expression(
            "() -> (Int, duck { val: Char })",
            TypeExpr::Fun(
                vec![],
                Some(Box::new(
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int.into_empty_span(),
                        TypeExpr::Duck(Duck {
                            fields: vec![Field::new(
                                "val".to_string(),
                                TypeExpr::Char.into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                    ])
                    .into_empty_span(),
                )),
            ),
        );

        assert_type_expression("{}", TypeExpr::Any);
        assert_type_expression("duck {}", TypeExpr::Any);

        assert_type_expression("go fmt", TypeExpr::Go("fmt".to_string()));
        assert_type_expression(
            "go sync.WaitGroup",
            TypeExpr::Go("sync.WaitGroup".to_string()),
        );

        assert_type_expression(
            "struct { x: String }",
            TypeExpr::Struct(Struct {
                fields: vec![Field::new(
                    "x".to_string(),
                    TypeExpr::String.into_empty_span(),
                )],
            }),
        );

        assert_type_expression(
            "struct { a: Int, b: Bool }",
            TypeExpr::Struct(Struct {
                fields: vec![
                    Field::new("a".to_string(), TypeExpr::Int.into_empty_span()),
                    Field::new("b".to_string(), TypeExpr::Bool.into_empty_span()),
                ],
            }),
        );

        assert_type_expression(
            "struct { nested: struct { inner: Float } }",
            TypeExpr::Struct(Struct {
                fields: vec![Field::new(
                    "nested".to_string(),
                    TypeExpr::Struct(Struct {
                        fields: vec![Field::new(
                            "inner".to_string(),
                            TypeExpr::Float.into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                )],
            }),
        );

        assert_type_expression(
            "struct { x: String, }",
            TypeExpr::Struct(Struct {
                fields: vec![Field::new(
                    "x".to_string(),
                    TypeExpr::String.into_empty_span(),
                )],
            }),
        );

        assert_type_expression(
            "duck { name: String }",
            TypeExpr::Duck(Duck {
                fields: vec![Field::new(
                    "name".to_string(),
                    TypeExpr::String.into_empty_span(),
                )],
            }),
        );

        assert_type_expression(
            "duck { age: Int, active: Bool }",
            TypeExpr::Duck(Duck {
                fields: vec![
                    Field::new("active".to_string(), TypeExpr::Bool.into_empty_span()),
                    Field::new("age".to_string(), TypeExpr::Int.into_empty_span()),
                ],
            }),
        );

        assert_type_expression(
            "duck { nested: duck { value: Char } }",
            TypeExpr::Duck(Duck {
                fields: vec![Field::new(
                    "nested".to_string(),
                    TypeExpr::Duck(Duck {
                        fields: vec![Field::new(
                            "value".to_string(),
                            TypeExpr::Char.into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                )],
            }),
        );

        assert_type_expression(
            "{ x: String, y : {}, }",
            TypeExpr::Duck(Duck {
                fields: vec![
                    Field::new("x".to_string(), TypeExpr::String.into_empty_span()),
                    Field::new("y".to_string(), TypeExpr::Any.into_empty_span()),
                ],
            }),
        );

        assert_type_expression("()", TypeExpr::Tuple(vec![]));
        assert_type_expression(
            "(Int)",
            TypeExpr::Tuple(vec![TypeExpr::Int.into_empty_span()]),
        );
        assert_type_expression(
            "(Int, String)",
            TypeExpr::Tuple(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String.into_empty_span(),
            ]),
        );
        assert_type_expression(
            "(Int, String,)",
            TypeExpr::Tuple(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String.into_empty_span(),
            ]),
        );
        assert_type_expression(
            "(Int, (Float, Bool))",
            TypeExpr::Tuple(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::Tuple(vec![
                    TypeExpr::Float.into_empty_span(),
                    TypeExpr::Bool.into_empty_span(),
                ])
                .into_empty_span(),
            ]),
        );
        assert_type_expression(
            "(Int, String, (Float, {x: String}),)",
            TypeExpr::Tuple(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String.into_empty_span(),
                TypeExpr::Tuple(vec![
                    TypeExpr::Float.into_empty_span(),
                    TypeExpr::Duck(Duck {
                        fields: vec![Field::new(
                            "x".to_string(),
                            TypeExpr::String.into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                ])
                .into_empty_span(),
            ]),
        );

        assert_type_expression("MyType", TypeExpr::TypeName(false, "MyType".to_string()));
        assert_type_expression(
            "::GlobalType",
            TypeExpr::TypeName(true, "GlobalType".to_string()),
        );
        assert_type_expression(
            "Module::MyType",
            TypeExpr::TypeName(false, "Module_MyType".to_string()),
        );
        assert_type_expression(
            "::Module::MyType",
            TypeExpr::TypeName(true, "Module_MyType".to_string()),
        );
        assert_type_expression(
            "::Module::SubModule::MyType",
            TypeExpr::TypeName(true, "Module_SubModule_MyType".to_string()),
        );

        assert_type_expression("String", TypeExpr::String);
        assert_type_expression("Int", TypeExpr::Int);
        assert_type_expression("Bool", TypeExpr::Bool);
        assert_type_expression("Char", TypeExpr::Char);
        assert_type_expression("Float", TypeExpr::Float);

        assert_type_expression(
            "Int | String",
            TypeExpr::Or(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String.into_empty_span(),
            ]),
        );
        assert_type_expression(
            "Bool | Char | Float",
            TypeExpr::Or(vec![
                TypeExpr::Bool.into_empty_span(),
                TypeExpr::Char.into_empty_span(),
                TypeExpr::Float.into_empty_span(),
            ]),
        );

        assert_type_expression(
            "Int | (String | Bool)",
            TypeExpr::Or(vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::Tuple(vec![
                    TypeExpr::Or(vec![
                        TypeExpr::String.into_empty_span(),
                        TypeExpr::Bool.into_empty_span(),
                    ])
                    .into_empty_span(),
                ])
                .into_empty_span(),
            ]),
        );

        assert_type_expression(
            "duck { x: Int } | struct { y: String }",
            TypeExpr::Or(vec![
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new("x".to_string(), TypeExpr::Int.into_empty_span())],
                })
                .into_empty_span(),
                TypeExpr::Struct(Struct {
                    fields: vec![Field::new(
                        "y".to_string(),
                        TypeExpr::String.into_empty_span(),
                    )],
                })
                .into_empty_span(),
            ]),
        );
    }

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
            "type X = () -> String;",
            "type X = () -> String;",
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
