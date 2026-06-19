use crate::parse::{
    lexer::lex_parser,
    make_input,
    value_parser::{empty_range, type_expr_into_empty_range},
};
use chumsky::Parser;

use crate::parse::type_parser::*;

fn strip_spans(spanned_type_expr: Spanned<TypeExpr>) -> Spanned<TypeExpr> {
    let (expr, _span) = spanned_type_expr;
    let stripped_expr = match expr {
        TypeExpr::Struct { name, type_params } => TypeExpr::Struct {
            name,
            type_params: type_params.into_iter().map(|x| strip_spans(x)).collect(),
        },
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
        TypeExpr::Fun(params, return_type, is_mut) => TypeExpr::Fun(
            params
                .into_iter()
                .map(|(name, param_type_expr)| (name, strip_spans(param_type_expr)))
                .collect(),
            Box::new(strip_spans(*return_type)),
            is_mut,
        ),
        TypeExpr::Or(variants) => TypeExpr::Or(variants.into_iter().map(strip_spans).collect()),
        TypeExpr::And(variants) => TypeExpr::And(variants.into_iter().map(strip_spans).collect()),
        TypeExpr::TypeName(is_global, type_name, generics) => TypeExpr::TypeName(
            is_global,
            type_name,
            generics
                .into_iter()
                .map(|generic| strip_spans(generic))
                .collect(),
        ),
        TypeExpr::Array(type_expr) => {
            TypeExpr::Array(Box::new(strip_spans(type_expr.as_ref().clone())))
        }
        other => other,
    };
    (stripped_expr, empty_range())
}

fn assert_type_expression(input_str: &str, expected_expr: TypeExpr) {
    println!("lexing and parsing: \"{}\"", input_str);
    let lexer_parse_result = lex_parser("test", "").parse(input_str);
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

    let mut parsed = parse_result.into_output().unwrap();
    type_expr_into_empty_range(&mut parsed);

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
        "fn() -> String",
        TypeExpr::Fun(
            vec![],
            Box::new(TypeExpr::String(None).into_empty_span()),
            false,
        ),
    );

    // assert_type_expression("true", TypeExpr::Bool(Some(true)));

    // assert_type_expression("false", TypeExpr::Bool(Some(false)));

    // assert_type_expression(
    //     "true | false",
    //     TypeExpr::Or(vec![
    //         TypeExpr::Bool(Some(true)).into_empty_span(),
    //         TypeExpr::Bool(Some(false)).into_empty_span(),
    //     ]),
    // );

    // assert_type_expression("\"str\"", TypeExpr::String(Some("str".to_string())));

    // assert_type_expression(
    //     "\"other_str\"",
    //     TypeExpr::String(Some("other_str".to_string())),
    // );

    // assert_type_expression(
    //     "\"str\" | \"other_str\"",
    //     TypeExpr::Or(vec![
    //         TypeExpr::String(Some("str".to_string())).into_empty_span(),
    //         TypeExpr::String(Some("other_str".to_string())).into_empty_span(),
    //     ]),
    // );

    assert_type_expression(
        "fn(x: Int) -> Bool",
        TypeExpr::Fun(
            vec![("x".to_string().into(), TypeExpr::Int.into_empty_span())],
            Box::new(TypeExpr::Bool(None).into_empty_span()),
            false,
        ),
    );

    assert_type_expression(
        "fn(a: Float, b: String) -> Char",
        TypeExpr::Fun(
            vec![
                ("a".to_string().into(), TypeExpr::Float.into_empty_span()),
                (
                    "b".to_string().into(),
                    TypeExpr::String(None).into_empty_span(),
                ),
            ],
            Box::new(TypeExpr::Char.into_empty_span()),
            false,
        ),
    );

    assert_type_expression(
        "(String | Bool) | Int",
        TypeExpr::Or(vec![
            TypeExpr::String(None).into_empty_span(),
            TypeExpr::Bool(None).into_empty_span(),
            TypeExpr::Int.into_empty_span(),
        ]),
    );

    assert_type_expression(
        "Generic<String>",
        TypeExpr::RawTypeName(
            false,
            vec!["Generic".to_string()],
            vec![TypeExpr::String(None).into_empty_span()],
        ),
    );

    assert_type_expression(
        "Option<String>",
        TypeExpr::RawTypeName(
            false,
            vec!["Option".to_string()],
            vec![TypeExpr::String(None).into_empty_span()],
        ),
    );

    assert_type_expression(
        "Result<Int, String>",
        TypeExpr::RawTypeName(
            false,
            vec!["Result".to_string()],
            vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String(None).into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Map<String, Int>",
        TypeExpr::RawTypeName(
            false,
            vec!["Map".to_string()],
            vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Int.into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "::std::Vec<::std::fs::File>",
        TypeExpr::RawTypeName(
            true,
            vec!["std".to_string(), "Vec".to_string()],
            vec![
                TypeExpr::RawTypeName(
                    true,
                    vec!["std".to_string(), "fs".to_string(), "File".to_string()],
                    vec![],
                )
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Option<Option<Int>>",
        TypeExpr::RawTypeName(
            false,
            vec!["Option".to_string()],
            vec![
                TypeExpr::RawTypeName(
                    false,
                    vec!["Option".to_string()],
                    vec![TypeExpr::Int.into_empty_span()],
                )
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Result<Result<Int, String>, Error>",
        TypeExpr::RawTypeName(
            false,
            vec!["Result".to_string()],
            vec![
                TypeExpr::RawTypeName(
                    false,
                    vec!["Result".to_string()],
                    vec![
                        TypeExpr::Int.into_empty_span(),
                        TypeExpr::String(None).into_empty_span(),
                    ],
                )
                .into_empty_span(),
                TypeExpr::RawTypeName(false, vec!["Error".to_string()], vec![]).into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Map<String, Vec<Int>>",
        TypeExpr::RawTypeName(
            false,
            vec!["Map".to_string()],
            vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::RawTypeName(
                    false,
                    vec!["Vec".to_string()],
                    vec![TypeExpr::Int.into_empty_span()],
                )
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Option<String>[]",
        TypeExpr::Array(
            TypeExpr::RawTypeName(
                false,
                vec!["Option".to_string()],
                vec![TypeExpr::String(None).into_empty_span()],
            )
            .into_empty_span()
            .into(),
        ),
    );

    assert_type_expression(
        "(Int, Result<String, Error>)",
        TypeExpr::Tuple(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::RawTypeName(
                false,
                vec!["Result".to_string()],
                vec![
                    TypeExpr::String(None).into_empty_span(),
                    TypeExpr::RawTypeName(false, vec!["Error".to_string()], vec![])
                        .into_empty_span(),
                ],
            )
            .into_empty_span(),
        ]),
    );

    assert_type_expression(
        "Option<Int> | Option<String>",
        TypeExpr::Or(vec![
            TypeExpr::RawTypeName(
                false,
                vec!["Option".to_string()],
                vec![TypeExpr::Int.into_empty_span()],
            )
            .into_empty_span(),
            TypeExpr::RawTypeName(
                false,
                vec!["Option".to_string()],
                vec![TypeExpr::String(None).into_empty_span()],
            )
            .into_empty_span(),
        ]),
    );

    assert_type_expression(
        "fn(p: Promise<Int>) -> Future<String>",
        TypeExpr::Fun(
            vec![(
                "p".to_string().into(),
                TypeExpr::RawTypeName(
                    false,
                    vec!["Promise".to_string()],
                    vec![TypeExpr::Int.into_empty_span()],
                )
                .into_empty_span(),
            )],
            Box::new(
                TypeExpr::RawTypeName(
                    false,
                    vec!["Future".to_string()],
                    vec![TypeExpr::String(None).into_empty_span()],
                )
                .into_empty_span(),
            ),
            false,
        ),
    );

    assert_type_expression(
        "Box<(Int, String)>",
        TypeExpr::RawTypeName(
            false,
            vec!["Box".to_string()],
            vec![
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::String(None).into_empty_span(),
                ])
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Container<{ id: Int, data: String }>",
        TypeExpr::RawTypeName(
            false,
            vec!["Container".to_string()],
            vec![
                TypeExpr::Duck(Duck {
                    fields: vec![
                        Field::new("data".to_string(), TypeExpr::String(None).into_empty_span()),
                        Field::new("id".to_string(), TypeExpr::Int.into_empty_span()),
                    ],
                })
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Holder<{}>",
        TypeExpr::RawTypeName(
            false,
            vec!["Holder".to_string()],
            vec![TypeExpr::Any.into_empty_span()],
        ),
    );

    // TODO: discuss unnamed paramters in fn typeexpr
    //      fn (String) -> Int
    // without the x:
    // There's an example of this right below this issue.
    // It shows that you have to declare the parameter name

    assert_type_expression(
        "Executor<fn(x: String) -> Int>",
        TypeExpr::RawTypeName(
            false,
            vec!["Executor".to_string()],
            vec![
                TypeExpr::Fun(
                    vec![(
                        "x".to_string().into(),
                        TypeExpr::String(None).into_empty_span(),
                    )],
                    Box::new(TypeExpr::Int.into_empty_span()),
                    false,
                )
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Collection<Int[]>",
        TypeExpr::RawTypeName(
            false,
            vec!["Collection".to_string()],
            vec![TypeExpr::Array(TypeExpr::Int.into_empty_span().into()).into_empty_span()],
        ),
    );

    assert_type_expression(
        "Result<Map<String, User[]>, (Int, String)>",
        TypeExpr::RawTypeName(
            false,
            vec!["Result".to_string()],
            vec![
                TypeExpr::RawTypeName(
                    false,
                    vec!["Map".to_string()],
                    vec![
                        TypeExpr::String(None).into_empty_span(),
                        TypeExpr::Array(
                            TypeExpr::RawTypeName(false, vec!["User".to_string()], vec![])
                                .into_empty_span()
                                .into(),
                        )
                        .into_empty_span(),
                    ],
                )
                .into_empty_span(),
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::String(None).into_empty_span(),
                ])
                .into_empty_span(),
            ],
        ),
    );

    // TODO: allow no return type in fn's type expr
    // case example right below issue.
    assert_type_expression(
        "fn(cb: fn(x: Result<Int, E>) -> ()) -> Subscription<T>",
        TypeExpr::Fun(
            vec![(
                "cb".to_string().into(),
                TypeExpr::Fun(
                    vec![(
                        "x".to_string().into(),
                        TypeExpr::RawTypeName(
                            false,
                            vec!["Result".to_string()],
                            vec![
                                TypeExpr::Int.into_empty_span(),
                                TypeExpr::RawTypeName(false, vec!["E".to_string()], vec![])
                                    .into_empty_span(),
                            ],
                        )
                        .into_empty_span(),
                    )],
                    Box::new(TypeExpr::Tuple(vec![]).into_empty_span()),
                    false,
                )
                .into_empty_span(),
            )],
            Box::new(
                TypeExpr::RawTypeName(
                    false,
                    vec!["Subscription".to_string()],
                    vec![
                        TypeExpr::RawTypeName(false, vec!["T".to_string()], vec![])
                            .into_empty_span(),
                    ],
                )
                .into_empty_span(),
            ),
            false,
        ),
    );

    assert_type_expression(
        "Cache<String, duck { data: Result<T, E> }>",
        TypeExpr::RawTypeName(
            false,
            vec!["Cache".to_string()],
            vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "data".to_string(),
                        TypeExpr::RawTypeName(
                            false,
                            vec!["Result".to_string()],
                            vec![
                                TypeExpr::RawTypeName(false, vec!["T".to_string()], vec![])
                                    .into_empty_span(),
                                TypeExpr::RawTypeName(false, vec!["E".to_string()], vec![])
                                    .into_empty_span(),
                            ],
                        )
                        .into_empty_span(),
                    )],
                })
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Result<Int, String,>",
        TypeExpr::RawTypeName(
            false,
            vec!["Result".to_string()],
            vec![
                TypeExpr::Int.into_empty_span(),
                TypeExpr::String(None).into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "Box<Int | String | Bool>",
        TypeExpr::RawTypeName(
            false,
            vec!["Box".to_string()],
            vec![
                TypeExpr::Or(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::String(None).into_empty_span(),
                    TypeExpr::Bool(None).into_empty_span(),
                ])
                .into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "  Map < String,  Int >  ",
        TypeExpr::RawTypeName(
            false,
            vec!["Map".to_string()],
            vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Int.into_empty_span(),
            ],
        ),
    );

    assert_type_expression(
        "fn(param1: TypeName,) -> ()",
        TypeExpr::Fun(
            vec![(
                "param1".to_string().into(),
                TypeExpr::RawTypeName(false, vec!["TypeName".to_string()], vec![])
                    .into_empty_span(),
            )],
            Box::new(TypeExpr::Tuple(Vec::new()).into_empty_span()),
            false,
        ),
    );

    // TODO(@Apfelfrosch): Here's an TODO, which had no title.
    assert_type_expression(
        "fn(data: duck { name: String, age: Int }) -> ::MyResult",
        TypeExpr::Fun(
            vec![(
                "data".to_string().into(),
                TypeExpr::Duck(Duck {
                    fields: vec![
                        Field::new("age".to_string(), TypeExpr::Int.into_empty_span()),
                        Field::new("name".to_string(), TypeExpr::String(None).into_empty_span()),
                    ],
                })
                .into_empty_span(),
            )],
            Box::new(
                TypeExpr::RawTypeName(true, vec!["MyResult".to_string()], vec![]).into_empty_span(),
            ),
            false,
        ),
    );

    assert_type_expression(
        "fn() -> (Int, String)",
        TypeExpr::Fun(
            vec![],
            Box::new(
                TypeExpr::Tuple(vec![
                    TypeExpr::Int.into_empty_span(),
                    TypeExpr::String(None).into_empty_span(),
                ])
                .into_empty_span(),
            ),
            false,
        ),
    );

    assert_type_expression(
        "Int[]",
        TypeExpr::Array(TypeExpr::Int.into_empty_span().into()),
    );

    assert_type_expression(
        "Int[][]",
        TypeExpr::Array(
            TypeExpr::Array(TypeExpr::Int.into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );
    assert_type_expression(
        "Int[][][]",
        TypeExpr::Array(
            TypeExpr::Array(
                TypeExpr::Array(TypeExpr::Int.into_empty_span().into())
                    .into_empty_span()
                    .into(),
            )
            .into_empty_span()
            .into(),
        ),
    );

    assert_type_expression(
        "(Int,)[]",
        TypeExpr::Array(
            TypeExpr::Tuple(vec![TypeExpr::Int.into_empty_span()])
                .into_empty_span()
                .into(),
        ),
    );

    assert_type_expression(
        "String[] | Int[][]",
        TypeExpr::Or(vec![
            TypeExpr::Array(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
            TypeExpr::Array(
                TypeExpr::Array(TypeExpr::Int.into_empty_span().into())
                    .into_empty_span()
                    .into(),
            )
            .into_empty_span(),
        ]),
    );

    assert_type_expression(
        "(String[] | Int[])[]",
        TypeExpr::Array(
            TypeExpr::Or(vec![
                TypeExpr::Array(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
                TypeExpr::Array(TypeExpr::Int.into_empty_span().into()).into_empty_span(),
            ])
            .into_empty_span()
            .into(),
        ),
    );

    assert_type_expression(
        "fn(x: Int) -> go \"fmt.Stringer\"",
        TypeExpr::Fun(
            vec![("x".to_string().into(), TypeExpr::Int.into_empty_span())],
            Box::new(TypeExpr::Go("fmt.Stringer".to_string()).into_empty_span()),
            false,
        ),
    );

    assert_type_expression(
        "fn() -> (Int, duck { val: Char })",
        TypeExpr::Fun(
            vec![],
            Box::new(
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
            ),
            false,
        ),
    );

    assert_type_expression("{}", TypeExpr::Any);
    assert_type_expression("duck {}", TypeExpr::Any);

    assert_type_expression("go \"fmt\"", TypeExpr::Go("fmt".to_string()));
    assert_type_expression(
        "go \"sync.WaitGroup\"",
        TypeExpr::Go("sync.WaitGroup".to_string()),
    );

    assert_type_expression(
        "duck { name: String }",
        TypeExpr::Duck(Duck {
            fields: vec![Field::new(
                "name".to_string(),
                TypeExpr::String(None).into_empty_span(),
            )],
        }),
    );

    assert_type_expression(
        "duck { age: Int, active: Bool }",
        TypeExpr::Duck(Duck {
            fields: vec![
                Field::new("active".to_string(), TypeExpr::Bool(None).into_empty_span()),
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
                Field::new("x".to_string(), TypeExpr::String(None).into_empty_span()),
                Field::new("y".to_string(), TypeExpr::Any.into_empty_span()),
            ],
        }),
    );

    assert_type_expression("()", TypeExpr::Tuple(vec![]));
    assert_type_expression(
        "(Int,)",
        TypeExpr::Tuple(vec![TypeExpr::Int.into_empty_span()]),
    );
    assert_type_expression(
        "(Int, String)",
        TypeExpr::Tuple(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::String(None).into_empty_span(),
        ]),
    );
    assert_type_expression(
        "(Int, String,)",
        TypeExpr::Tuple(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::String(None).into_empty_span(),
        ]),
    );
    assert_type_expression(
        "(Int, (Float, Bool))",
        TypeExpr::Tuple(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::Tuple(vec![
                TypeExpr::Float.into_empty_span(),
                TypeExpr::Bool(None).into_empty_span(),
            ])
            .into_empty_span(),
        ]),
    );
    assert_type_expression(
        "(Int, String, (Float, {x: String}),)",
        TypeExpr::Tuple(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::String(None).into_empty_span(),
            TypeExpr::Tuple(vec![
                TypeExpr::Float.into_empty_span(),
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String(None).into_empty_span(),
                    )],
                })
                .into_empty_span(),
            ])
            .into_empty_span(),
        ]),
    );

    assert_type_expression(
        "MyType",
        TypeExpr::RawTypeName(false, vec!["MyType".to_string()], vec![]),
    );
    assert_type_expression(
        "::GlobalType",
        TypeExpr::RawTypeName(true, vec!["GlobalType".to_string()], vec![]),
    );
    assert_type_expression(
        "Module::MyType",
        TypeExpr::RawTypeName(
            false,
            vec!["Module".to_string(), "MyType".to_string()],
            vec![],
        ),
    );
    assert_type_expression(
        "::Module::MyType",
        TypeExpr::RawTypeName(
            true,
            vec!["Module".to_string(), "MyType".to_string()],
            vec![],
        ),
    );
    assert_type_expression(
        "::Module::SubModule::MyType",
        TypeExpr::RawTypeName(
            true,
            vec![
                "Module".to_string(),
                "SubModule".to_string(),
                "MyType".to_string(),
            ],
            vec![],
        ),
    );

    assert_type_expression("String", TypeExpr::String(None));
    assert_type_expression("Int", TypeExpr::Int);
    assert_type_expression("Bool", TypeExpr::Bool(None));
    assert_type_expression("Char", TypeExpr::Char);
    assert_type_expression("Float", TypeExpr::Float);

    assert_type_expression(
        "Int | String",
        TypeExpr::Or(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::String(None).into_empty_span(),
        ]),
    );
    assert_type_expression(
        "Bool | Char | Float",
        TypeExpr::Or(vec![
            TypeExpr::Bool(None).into_empty_span(),
            TypeExpr::Char.into_empty_span(),
            TypeExpr::Float.into_empty_span(),
        ]),
    );

    assert_type_expression(
        "Int | (String | Bool,)",
        TypeExpr::Or(vec![
            TypeExpr::Int.into_empty_span(),
            TypeExpr::Tuple(vec![
                TypeExpr::Or(vec![
                    TypeExpr::String(None).into_empty_span(),
                    TypeExpr::Bool(None).into_empty_span(),
                ])
                .into_empty_span(),
            ])
            .into_empty_span(),
        ]),
    );

    // assert_type_expression(
    //     "{ x: \"hallo\" } | { x: \"bye\" }",
    //     TypeExpr::Or(vec![
    //         TypeExpr::Duck(Duck {
    //             fields: vec![Field::new(
    //                 "x".to_string(),
    //                 TypeExpr::String(Some("hallo".to_string())).into_empty_span(),
    //             )],
    //         })
    //         .into_empty_span(),
    //         TypeExpr::Duck(Duck {
    //             fields: vec![Field::new(
    //                 "x".to_string(),
    //                 TypeExpr::String(Some("bye".to_string())).into_empty_span(),
    //             )],
    //         })
    //         .into_empty_span(),
    //     ]),
    // );

    assert_type_expression(
        "&Int",
        TypeExpr::Ref(TypeExpr::Int.into_empty_span().into()),
    );

    assert_type_expression(
        "&String[]",
        TypeExpr::Ref(
            TypeExpr::Array(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );

    assert_type_expression(
        "&(String | Int)",
        TypeExpr::Ref(
            TypeExpr::Or(vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Int.into_empty_span(),
            ])
            .into_empty_span()
            .into(),
        ),
    );

    assert_type_expression(
        "{x: Int} & {y: String} | {z: Int}",
        TypeExpr::Or(vec![
            TypeExpr::And(vec![
                TypeExpr::Duck(Duck {
                    fields: vec![Field {
                        name: "x".to_string(),
                        type_expr: TypeExpr::Int.into_empty_span(),
                    }],
                })
                .into_empty_span(),
                TypeExpr::Duck(Duck {
                    fields: vec![Field {
                        name: "y".to_string(),
                        type_expr: TypeExpr::String(None).into_empty_span(),
                    }],
                })
                .into_empty_span(),
            ])
            .into_empty_span(),
            TypeExpr::Duck(Duck {
                fields: vec![Field {
                    name: "z".to_string(),
                    type_expr: TypeExpr::Int.into_empty_span(),
                }],
            })
            .into_empty_span(),
        ]),
    );

    assert_type_expression(
        "&String | Int",
        TypeExpr::Or(vec![
            TypeExpr::Ref(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
            TypeExpr::Int.into_empty_span(),
        ]),
    );

    assert_type_expression(
        "&mut Int",
        TypeExpr::RefMut(TypeExpr::Int.into_empty_span().into()),
    );

    assert_type_expression(
        "&mut String[]",
        TypeExpr::RefMut(
            TypeExpr::Array(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );

    assert_type_expression(
        "&mut (String | Int)",
        TypeExpr::RefMut(
            TypeExpr::Or(vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Int.into_empty_span(),
            ])
            .into_empty_span()
            .into(),
        ),
    );

    assert_type_expression(
        "&mut String | Int",
        TypeExpr::Or(vec![
            TypeExpr::RefMut(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
            TypeExpr::Int.into_empty_span(),
        ]),
    );

    assert_type_expression(
        "&&mut String | Int",
        TypeExpr::Or(vec![
            TypeExpr::Ref(
                TypeExpr::RefMut(TypeExpr::String(None).into_empty_span().into())
                    .into_empty_span()
                    .into(),
            )
            .into_empty_span(),
            TypeExpr::Int.into_empty_span(),
        ]),
    );

    assert_type_expression(
        "&String[]",
        TypeExpr::Ref(
            TypeExpr::Array(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );
    assert_type_expression(
        "&mut String[]",
        TypeExpr::RefMut(
            TypeExpr::Array(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );
    assert_type_expression(
        "(&String)[]",
        TypeExpr::Array(
            TypeExpr::Ref(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );
    assert_type_expression(
        "(&mut String)[]",
        TypeExpr::Array(
            TypeExpr::RefMut(TypeExpr::String(None).into_empty_span().into())
                .into_empty_span()
                .into(),
        ),
    );

    assert_type_expression(
        "String[.username]",
        TypeExpr::Indexed(
            TypeExpr::String(None).into_empty_span().into(),
            TypeExpr::Tag("username".to_string())
                .into_empty_span()
                .into(),
        ),
    );

    assert_type_expression(
        "String[Int]",
        TypeExpr::Indexed(
            TypeExpr::String(None).into_empty_span().into(),
            TypeExpr::Int.into_empty_span().into(),
        ),
    );
}

// TODO(@mvmo): type definition testing
// At the moment type definitions are just tested by if they parse or not.
// They should be tested against their structure, so doing asserts on the actual TypeDefinition.
// Just as we do it everywhere else
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
        "type Tup = go \"fmt\";",
        "type Tup = go \"sync.WaitGroup\";",
        "type X = ::String;",
        "type X = String::ABC::C;",
        "type X = ::String::ABC::C;",
        "type X = fn() -> String;",
        "type X = fn() -> String;",
        "type X<TYPENAME> = fn() -> String;",
        "type X<TYPENAME, TYPENAME2> = fn() -> String;",
        "type X<TYPENAME, TYPENAME2, TYPENAME3> = fn() -> String;",
    ];

    for valid_type_definition in valid_type_definitions {
        println!("lexing {valid_type_definition}");
        let lexer_parse_result = lex_parser("test", "").parse(valid_type_definition);
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
        "go \"sync.WaitGroup\"",
        "::X",
        "::X::Y",
        "X::Y::Z",
    ];

    for valid_type_expression in valid_type_expressions {
        println!("lexing {valid_type_expression}");
        let lexer_parse_result = lex_parser("test", "").parse(valid_type_expression);
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
        "struct { name: String }",
        "struct {,}",
        "::",
        "X:Y:Z:",
        "Y::Y::",
    ];

    for invalid_type_expression in invalid_type_expressions {
        println!("lexing {invalid_type_expression}");
        let lexer_parse_result = lex_parser("test", "").parse(invalid_type_expression);
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
