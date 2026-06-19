use chumsky::prelude::*;

use crate::parse::{
    Field, Spanned,
    function_parser::LambdaFunctionExpr,
    lexer::lex_parser,
    make_input,
    type_parser::{Duck, TypeExpr},
    value_parser::{
        Assignment, Declaration, MatchArm, ValHtmlStringContents, ValueExpr, empty_block,
        empty_range, empty_tuple, type_expr_into_empty_range, value_expr_into_empty_range,
        value_expr_parser,
    },
};

fn var(x: impl Into<String>) -> Box<Spanned<ValueExpr>> {
    ValueExpr::RawVariable(false, vec![x.into()])
        .into_empty_span()
        .into()
}

fn gvar(x: impl Into<String>) -> Box<Spanned<ValueExpr>> {
    ValueExpr::RawVariable(true, vec![x.into()])
        .into_empty_span()
        .into()
}

fn v_var(x: &[impl AsRef<str>]) -> Box<Spanned<ValueExpr>> {
    ValueExpr::RawVariable(false, x.iter().map(|x| x.as_ref().to_string()).collect())
        .into_empty_span()
        .into()
}

fn v_gvar(x: &[impl AsRef<str>]) -> Box<Spanned<ValueExpr>> {
    ValueExpr::RawVariable(true, x.iter().map(|x| x.as_ref().to_string()).collect())
        .into_empty_span()
        .into()
}

#[test]
fn test_value_expression_parser() {
    let test_cases = vec![
        // (
        //     r#"
        //     {
        //     for i in a {}
        //     for i in a {}
        //     for i in a {}
        //     for i in a {}
        //     const x = 10;
        //     const x = 10;
        //     for i in a {}
        //     const x = 10;
        //     for i in a {}
        //     for i in a {}
        //     const x = 10;
        //     }

        //     "#,
        //     ValueExpr::Break,
        // ),
        (
            "duckx {5;<h1>{<p></p>}</h1>}",
            ValueExpr::Block(vec![
                ValueExpr::Int(5, None).into_empty_span(),
                ValueExpr::HtmlString(vec![
                    ValHtmlStringContents::String("<h1>".to_string()),
                    ValHtmlStringContents::Expr(
                        ValueExpr::Block(vec![
                            ValueExpr::HtmlString(vec![ValHtmlStringContents::String(
                                "<p></p>".to_string(),
                            )])
                            .into_empty_span(),
                        ])
                        .into_empty_span(),
                    ),
                    ValHtmlStringContents::String("</h1>".to_string()),
                ])
                .into_empty_span(),
            ]),
        ),
        (".tag", ValueExpr::Tag("tag".to_string())),
        ("..", ValueExpr::Tag("DOT".to_string())),
        (
            "a<String>()",
            ValueExpr::FunctionCall {
                target: var("a"),
                params: vec![],
                type_params: vec![TypeExpr::String(None).into_empty_span()],
            },
        ),
        (
            "b->a<String>()",
            ValueExpr::FunctionCall {
                target: var("a"),
                params: vec![*var("b")],
                type_params: vec![TypeExpr::String(None).into_empty_span()],
            },
        ),
        (
            "a->b() == x->c()",
            ValueExpr::Equals(
                ValueExpr::FunctionCall {
                    target: var("b"),
                    params: vec![*var("a")],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                ValueExpr::FunctionCall {
                    target: var("c"),
                    params: vec![*var("x")],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "a->x.x()->c(1)",
            ValueExpr::FunctionCall {
                target: var("c"),
                params: vec![
                    ValueExpr::FunctionCall {
                        target: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "x".into(),
                        }
                        .into_empty_span()
                        .into(),
                        params: vec![*var("a")],
                        type_params: vec![],
                    }
                    .into_empty_span(),
                    ValueExpr::Int(1, None).into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "a->b()",
            ValueExpr::FunctionCall {
                target: var("b"),
                params: vec![*var("a")],
                type_params: vec![],
            },
        ),
        (
            "[1]",
            ValueExpr::Array(vec![ValueExpr::Int(1, None).into_empty_span()], None),
        ),
        (
            "[1,]",
            ValueExpr::Array(vec![ValueExpr::Int(1, None).into_empty_span()], None),
        ),
        ("{}", ValueExpr::Block(vec![])),
        (
            "[1,]",
            ValueExpr::Array(vec![ValueExpr::Int(1, None).into_empty_span()], None),
        ),
        (
            "[1,2]",
            ValueExpr::Array(
                vec![
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Int(2, None).into_empty_span(),
                ],
                None,
            ),
        ),
        (
            "[1 == 2, 2]",
            ValueExpr::Array(
                vec![
                    ValueExpr::Equals(
                        Box::new(ValueExpr::Int(1, None).into_empty_span()),
                        Box::new(ValueExpr::Int(2, None).into_empty_span()),
                    )
                    .into_empty_span(),
                    ValueExpr::Int(2, None).into_empty_span(),
                ],
                None,
            ),
        ),
        (
            "[[1,2,3], []]",
            ValueExpr::Array(
                vec![
                    ValueExpr::Array(
                        vec![
                            ValueExpr::Int(1, None).into_empty_span(),
                            ValueExpr::Int(2, None).into_empty_span(),
                            ValueExpr::Int(3, None).into_empty_span(),
                        ],
                        None,
                    )
                    .into_empty_span(),
                    ValueExpr::Array(vec![], None).into_empty_span(),
                ],
                None,
            ),
        ),
        (
            "a.y[0]",
            ValueExpr::ArrayAccess(
                ValueExpr::FieldAccess {
                    target_obj: var("a"),
                    field_name: "y".into(),
                }
                .into_empty_span()
                .into(),
                ValueExpr::Int(0, None).into_empty_span().into(),
            ),
        ),
        (
            "[1][0]",
            ValueExpr::ArrayAccess(
                ValueExpr::Array(vec![ValueExpr::Int(1, None).into_empty_span()], None)
                    .into_empty_span()
                    .into(),
                ValueExpr::Int(0, None).into_empty_span().into(),
            ),
        ),
        (
            "a.y()",
            ValueExpr::FunctionCall {
                target: ValueExpr::FieldAccess {
                    target_obj: var("a"),
                    field_name: "y".into(),
                }
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "mut fn() {}",
            ValueExpr::Lambda(Box::new(LambdaFunctionExpr {
                is_mut: true,
                params: vec![],
                return_type: None,
                value_expr: ValueExpr::Return(Some(
                    ValueExpr::Block(vec![]).into_empty_span().into(),
                ))
                .into_empty_span(),
            })),
        ),
        (
            "a.y[0] = 5",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::ArrayAccess(
                            ValueExpr::FieldAccess {
                                target_obj: var("a"),
                                field_name: "y".into(),
                            }
                            .into_empty_span()
                            .into(),
                            ValueExpr::Int(0, None).into_empty_span().into(),
                        )
                        .into_empty_span()
                        .into(),
                        value_expr: ValueExpr::Int(5, None).into_empty_span().into(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "a[0]",
            ValueExpr::ArrayAccess(var("a"), ValueExpr::Int(0, None).into_empty_span().into()),
        ),
        (
            "a[0][0]",
            ValueExpr::ArrayAccess(
                ValueExpr::ArrayAccess(var("a"), ValueExpr::Int(0, None).into_empty_span().into())
                    .into_empty_span()
                    .into(),
                ValueExpr::Int(0, None).into_empty_span().into(),
            ),
        ),
        (
            "a()[0]()",
            ValueExpr::FunctionCall {
                target: ValueExpr::ArrayAccess(
                    ValueExpr::FunctionCall {
                        target: var("a"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(0, None).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "a()[0][0]()",
            ValueExpr::FunctionCall {
                target: ValueExpr::ArrayAccess(
                    ValueExpr::ArrayAccess(
                        ValueExpr::FunctionCall {
                            target: var("a"),
                            params: vec![],
                            type_params: vec![],
                        }
                        .into_empty_span()
                        .into(),
                        ValueExpr::Int(0, None).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(0, None).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "[a(), b(), (1,2)]",
            ValueExpr::Array(
                vec![
                    ValueExpr::FunctionCall {
                        target: var("a"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("b"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span(),
                    ValueExpr::Tuple(vec![
                        ValueExpr::Int(1, None).into_empty_span(),
                        ValueExpr::Int(2, None).into_empty_span(),
                    ])
                    .into_empty_span(),
                ],
                None,
            ),
        ),
        ("true", ValueExpr::Bool(true)),
        ("false", ValueExpr::Bool(false)),
        (
            "::MyStruct { x: 5 }",
            ValueExpr::RawStruct {
                is_global: true,
                name: vec!["MyStruct".to_string()],
                fields: vec![("x".to_string(), ValueExpr::Int(5, None).into_empty_span())],
                type_params: vec![],
            },
        ),
        (
            "MyStruct { x: 5 }",
            ValueExpr::RawStruct {
                is_global: false,
                name: vec!["MyStruct".to_string()],
                fields: vec![("x".to_string(), ValueExpr::Int(5, None).into_empty_span())],
                type_params: vec![],
            },
        ),
        (
            "Outer { x: 5, y: Inner { x: 5 } }",
            ValueExpr::RawStruct {
                is_global: false,
                name: vec!["Outer".to_string()],
                fields: vec![
                    ("x".to_string(), ValueExpr::Int(5, None).into_empty_span()),
                    (
                        "y".to_string(),
                        ValueExpr::RawStruct {
                            is_global: false,
                            name: vec!["Inner".to_string()],
                            fields: vec![(
                                "x".to_string(),
                                ValueExpr::Int(5, None).into_empty_span(),
                            )],
                            type_params: vec![],
                        }
                        .into_empty_span(),
                    ),
                ],
                type_params: vec![],
            },
        ),
        ("{}", empty_block()),
        (
            "to_upper()",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: Vec::new(),
                type_params: vec![],
            },
        ),
        (
            "to_upper(1)",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper(1,)",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper ()",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: Vec::new(),
                type_params: vec![],
            },
        ),
        (
            "to_upper (1)",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper (1,)",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper (   )",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: Vec::new(),
                type_params: vec![],
            },
        ),
        (
            "to_upper ( 1 )",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "::to_upper ( 1  ,  )",
            ValueExpr::FunctionCall {
                target: gvar("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "abc::to_upper ( 1  ,  )",
            ValueExpr::FunctionCall {
                target: v_var(&["abc", "to_upper"]),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "abc::xyz::to_upper ( 1  ,  )",
            ValueExpr::FunctionCall {
                target: v_var(&["abc", "xyz", "to_upper"]),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "::abc::xyz::to_upper ( 1  ,  )",
            ValueExpr::FunctionCall {
                target: v_gvar(&["abc", "xyz", "to_upper"]),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper ( 1  ,  )",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![ValueExpr::Int(1, None).into_empty_span()],
                type_params: vec![],
            },
        ),
        (
            "to_upper ( to_lower(1,2,add(5, 10),4), true  )",
            ValueExpr::FunctionCall {
                target: var("to_upper"),
                params: vec![
                    ValueExpr::FunctionCall {
                        target: var("to_lower"),
                        params: vec![
                            ValueExpr::Int(1, None).into_empty_span(),
                            ValueExpr::Int(2, None).into_empty_span(),
                            ValueExpr::FunctionCall {
                                target: var("add"),
                                params: vec![
                                    ValueExpr::Int(5, None).into_empty_span(),
                                    ValueExpr::Int(10, None).into_empty_span(),
                                ],
                                type_params: vec![],
                            }
                            .into_empty_span(),
                            ValueExpr::Int(4, None).into_empty_span(),
                        ],
                        type_params: vec![],
                    }
                    .into_empty_span(),
                    ValueExpr::Bool(true).into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "print(\"hallo\", \"moin\")",
            ValueExpr::FunctionCall {
                target: var("print"),
                params: vec![
                    ValueExpr::String("hallo".into(), true).into_empty_span(),
                    ValueExpr::String("moin".into(), true).into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        ("x", ValueExpr::RawVariable(false, vec!["x".into()])),
        (
            "print(x, true, lol())",
            ValueExpr::FunctionCall {
                target: var("print"),
                params: vec![
                    *var("x"),
                    ValueExpr::Bool(true).into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("lol"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "if (true) { 1 } else { 2 }",
            ValueExpr::If {
                condition: ValueExpr::Bool(true).into_empty_span().into(),
                then: ValueExpr::Int(1, None).into_empty_span_and_block().into(),
                r#else: Some(ValueExpr::Int(2, None).into_empty_span_and_block().into()),
            },
        ),
        (
            "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
            ValueExpr::If {
                condition: ValueExpr::Bool(true).into_empty_span().into(),
                then: ValueExpr::Int(1, None).into_empty_span_and_block().into(),
                r#else: Some(
                    ValueExpr::If {
                        condition: ValueExpr::Bool(false).into_empty_span().into(),
                        then: ValueExpr::Int(3, None).into_empty_span_and_block().into(),
                        r#else: Some(
                            ValueExpr::If {
                                condition: ValueExpr::Int(200, None).into_empty_span().into(),
                                then: ValueExpr::Int(4, None).into_empty_span_and_block().into(),
                                r#else: Some(
                                    ValueExpr::Int(2, None).into_empty_span_and_block().into(),
                                ),
                            }
                            .into_empty_span()
                            .into(),
                        ),
                    }
                    .into_empty_span()
                    .into(),
                ),
            },
        ),
        (
            "(1,true,2,\"hallo\")",
            ValueExpr::Tuple(vec![
                ValueExpr::Int(1, None).into_empty_span(),
                ValueExpr::Bool(true).into_empty_span(),
                ValueExpr::Int(2, None).into_empty_span(),
                ValueExpr::String("hallo".into(), true).into_empty_span(),
            ]),
        ),
        (
            "!std::arch::is_windows()",
            ValueExpr::BoolNegate(
                ValueExpr::FunctionCall {
                    target: v_var(&["std", "arch", "is_windows"]),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!std::arch::is_windows",
            ValueExpr::BoolNegate(v_var(&["std", "arch", "is_windows"])),
        ),
        ("!std::arch", ValueExpr::BoolNegate(v_var(&["std", "arch"]))),
        (
            "x.y = 100",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span(),
                        value_expr: ValueExpr::Int(100, None).into_empty_span(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "std::xs.y.z = 100",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::FieldAccess {
                            field_name: "z".into(),
                            target_obj: Box::new(
                                ValueExpr::FieldAccess {
                                    target_obj: v_var(&["std", "xs"]),
                                    field_name: "y".into(),
                                }
                                .into_empty_span(),
                            ),
                        }
                        .into_empty_span(),
                        value_expr: ValueExpr::Int(100, None).into_empty_span(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "x.y.z = 100",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::FieldAccess {
                            field_name: "z".into(),
                            target_obj: Box::new(
                                ValueExpr::FieldAccess {
                                    target_obj: var("x"),
                                    field_name: "y".into(),
                                }
                                .into_empty_span(),
                            ),
                        }
                        .into_empty_span(),
                        value_expr: ValueExpr::Int(100, None).into_empty_span(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        ("{}", empty_block()),
        (
            "{}.x",
            ValueExpr::FieldAccess {
                target_obj: empty_block().into_empty_span().into(),
                field_name: "x".into(),
            },
        ),
        (
            "!{}.x",
            ValueExpr::BoolNegate(
                ValueExpr::FieldAccess {
                    target_obj: empty_block().into_empty_span().into(),
                    field_name: "x".into(),
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "{1}",
            ValueExpr::Block(vec![ValueExpr::Int(1, None).into_empty_span()]),
        ),
        (
            "{1;  2   ;3;x()}",
            ValueExpr::Block(vec![
                ValueExpr::Int(1, None).into_empty_span(),
                ValueExpr::Int(2, None).into_empty_span(),
                ValueExpr::Int(3, None).into_empty_span(),
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span(),
            ]),
        ),
        (
            "{1;  2   ;3;x({})}",
            ValueExpr::Block(vec![
                ValueExpr::Int(1, None).into_empty_span(),
                ValueExpr::Int(2, None).into_empty_span(),
                ValueExpr::Int(3, None).into_empty_span(),
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![empty_block().into_empty_span()],
                    type_params: vec![],
                }
                .into_empty_span(),
            ]),
        ),
        (
            "{x();y();}",
            ValueExpr::Block(vec![
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span(),
                ValueExpr::FunctionCall {
                    target: var("y"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span(),
                empty_tuple().into_empty_span(),
            ]),
        ),
        (
            "x({ 1; 2; y({ z(); }) }, lol)",
            ValueExpr::FunctionCall {
                target: var("x"),
                params: vec![
                    ValueExpr::Block(vec![
                        ValueExpr::Int(1, None).into_empty_span(),
                        ValueExpr::Int(2, None).into_empty_span(),
                        ValueExpr::FunctionCall {
                            target: var("y"),
                            params: vec![
                                ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("z"),
                                        params: vec![],
                                        type_params: vec![],
                                    }
                                    .into_empty_span(),
                                    empty_tuple().into_empty_span(),
                                ])
                                .into_empty_span(),
                            ],
                            type_params: vec![],
                        }
                        .into_empty_span(),
                    ])
                    .into_empty_span(),
                    *var("lol"),
                ],
                type_params: vec![],
            },
        ),
        (
            "a()()",
            ValueExpr::FunctionCall {
                target: ValueExpr::FunctionCall {
                    target: var("a"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "(fn() {fn() {1}})()()",
            ValueExpr::FunctionCall {
                target: ValueExpr::FunctionCall {
                    target: ValueExpr::Lambda(
                        LambdaFunctionExpr {
                            is_mut: false,
                            params: vec![],
                            return_type: None,
                            value_expr: ValueExpr::Return(Some(
                                ValueExpr::Block(vec![
                                    ValueExpr::Lambda(
                                        LambdaFunctionExpr {
                                            is_mut: false,
                                            params: vec![],
                                            return_type: None,
                                            value_expr: ValueExpr::Return(Some(
                                                ValueExpr::Block(vec![
                                                    ValueExpr::Int(1, None).into_empty_span(),
                                                ])
                                                .into_empty_span()
                                                .into(),
                                            ))
                                            .into_empty_span(),
                                        }
                                        .into(),
                                    )
                                    .into_empty_span()
                                    .into(),
                                ])
                                .into_empty_span()
                                .into(),
                            ))
                            .into_empty_span()
                            .into(),
                        }
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "while (true) {}",
            ValueExpr::While {
                condition: ValueExpr::Bool(true).into_empty_span().into(),
                body: ValueExpr::Block(vec![]).into_empty_span().into(),
            },
        ),
        (
            "while (my_func()) {}",
            ValueExpr::While {
                condition: ValueExpr::FunctionCall {
                    target: var("my_func"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                body: ValueExpr::Block(vec![]).into_empty_span().into(),
            },
        ),
        (
            "while (my_func()) {1;break;}",
            ValueExpr::While {
                condition: ValueExpr::FunctionCall {
                    target: var("my_func"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                body: ValueExpr::Block(vec![
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Break.into_empty_span(),
                    empty_tuple().into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            },
        ),
        (
            "while (my_func()) {1;continue;}",
            ValueExpr::While {
                condition: ValueExpr::FunctionCall {
                    target: var("my_func"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                body: ValueExpr::Block(vec![
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Continue.into_empty_span(),
                    empty_tuple().into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            },
        ),
        (
            "[[], []]",
            ValueExpr::Array(
                vec![
                    ValueExpr::Array(vec![], None).into_empty_span(),
                    ValueExpr::Array(vec![], None).into_empty_span(),
                ],
                None,
            ),
        ),
        (
            "[1]",
            ValueExpr::Array(vec![ValueExpr::Int(1, None).into_empty_span()], None),
        ),
        ("()", empty_tuple()),
        (
            "(1.1, 'x')",
            ValueExpr::Tuple(vec![
                ValueExpr::Float(1.1).into_empty_span(),
                ValueExpr::Char('x').into_empty_span(),
            ]),
        ),
        (
            "{x: 1, y: { z: true }}",
            ValueExpr::Duck(vec![
                ("x".into(), ValueExpr::Int(1, None).into_empty_span()),
                (
                    "y".into(),
                    ValueExpr::Duck(vec![("z".into(), ValueExpr::Bool(true).into_empty_span())])
                        .into_empty_span(),
                ),
            ]),
        ),
        (
            "{x: 1, y: { z: true, w: { print();2;true } }}",
            ValueExpr::Duck(vec![
                ("x".into(), ValueExpr::Int(1, None).into_empty_span()),
                (
                    "y".into(),
                    ValueExpr::Duck(vec![
                        (
                            "w".into(),
                            ValueExpr::Block(vec![
                                ValueExpr::FunctionCall {
                                    target: var("print"),
                                    params: vec![],
                                    type_params: vec![],
                                }
                                .into_empty_span(),
                                ValueExpr::Int(2, None).into_empty_span(),
                                ValueExpr::Bool(true).into_empty_span(),
                            ])
                            .into_empty_span(),
                        ),
                        ("z".into(), ValueExpr::Bool(true).into_empty_span()),
                    ])
                    .into_empty_span(),
                ),
            ]),
        ),
        (
            "if (true) {{}} else {{x: 1}}",
            ValueExpr::If {
                condition: ValueExpr::Bool(true).into_empty_span().into(),
                then: ValueExpr::Block(vec![]).into_empty_span_and_block().into(),
                r#else: Some(
                    ValueExpr::Duck(vec![(
                        "x".into(),
                        ValueExpr::Int(1, None).into_empty_span(),
                    )])
                    .into_empty_span_and_block()
                    .into(),
                ),
            },
        ),
        (
            "x.y",
            ValueExpr::FieldAccess {
                target_obj: var("x"),
                field_name: "y".into(),
            },
        ),
        (
            "{x: 123}.y",
            ValueExpr::FieldAccess {
                target_obj: ValueExpr::Duck(vec![(
                    "x".into(),
                    ValueExpr::Int(123, None).into_empty_span(),
                )])
                .into_empty_span()
                .into(),
                field_name: "y".into(),
            },
        ),
        (
            "x().y",
            ValueExpr::FieldAccess {
                target_obj: ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                field_name: "y".into(),
            },
        ),
        (
            "(x)()",
            ValueExpr::FunctionCall {
                target: var("x"),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "x()",
            ValueExpr::FunctionCall {
                target: var("x"),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "(1)()",
            ValueExpr::FunctionCall {
                target: ValueExpr::Int(1, None).into_empty_span().into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "(123)()",
            ValueExpr::FunctionCall {
                target: ValueExpr::Int(123, None).into_empty_span().into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "(returns_lambda())()",
            ValueExpr::FunctionCall {
                target: ValueExpr::FunctionCall {
                    target: var("returns_lambda"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                params: vec![],
                type_params: vec![],
            },
        ),
        (
            "x.y.z == z.w.a",
            ValueExpr::Equals(
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: var("x"),
                        field_name: "y".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "z".into(),
                }
                .into_empty_span()
                .into(),
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: var("z"),
                        field_name: "w".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "a".into(),
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "x.y.z.w",
            ValueExpr::FieldAccess {
                target_obj: ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: var("x"),
                        field_name: "y".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "z".into(),
                }
                .into_empty_span()
                .into(),
                field_name: "w".into(),
            },
        ),
        ("((1))", ValueExpr::Int(1, None)),
        (
            "x({();();},1)",
            ValueExpr::FunctionCall {
                target: var("x"),
                params: vec![
                    ValueExpr::Block(vec![
                        empty_tuple().into_empty_span(),
                        empty_tuple().into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span(),
                    ValueExpr::Int(1, None).into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "x({();{();1;};},1)",
            ValueExpr::FunctionCall {
                target: var("x"),
                params: vec![
                    ValueExpr::Block(vec![
                        empty_tuple().into_empty_span(),
                        ValueExpr::Block(vec![
                            empty_tuple().into_empty_span(),
                            ValueExpr::Int(1, None).into_empty_span(),
                            empty_tuple().into_empty_span(),
                        ])
                        .into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span(),
                    ValueExpr::Int(1, None).into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "return 123",
            ValueExpr::Return(Some(Box::new(
                ValueExpr::Int(123, None).into_empty_span().into(),
            ))),
        ),
        (
            "let x: String = \"\"",
            ValueExpr::VarDecl(
                (
                    Declaration {
                        name: "x".into(),
                        initializer: Some(
                            ValueExpr::String("".to_string(), true).into_empty_span(),
                        ),
                        type_expr: Some(TypeExpr::String(None).into_empty_span()),
                        is_const: false,
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "const x: String = \"\"",
            ValueExpr::VarDecl(
                (
                    Declaration {
                        name: "x".into(),
                        initializer: Some(
                            ValueExpr::String("".to_string(), true).into_empty_span(),
                        ),
                        type_expr: Some(TypeExpr::String(None).into_empty_span()),
                        is_const: true,
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "x() * y()",
            ValueExpr::Mul(
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                ValueExpr::FunctionCall {
                    target: var("y"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "3 * 5",
            ValueExpr::Mul(
                ValueExpr::Int(3, None).into_empty_span().into(),
                ValueExpr::Int(5, None).into_empty_span().into(),
            ),
        ),
        (
            "3 + 5",
            ValueExpr::Add(
                ValueExpr::Int(3, None).into_empty_span().into(),
                ValueExpr::Int(5, None).into_empty_span().into(),
            ),
        ),
        (
            "3 * 5 * 6",
            ValueExpr::Mul(
                ValueExpr::Mul(
                    ValueExpr::Int(3, None).into_empty_span().into(),
                    ValueExpr::Int(5, None).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
                ValueExpr::Int(6, None).into_empty_span().into(),
            ),
        ),
        (
            "x() * 5 * 6",
            ValueExpr::Mul(
                ValueExpr::Mul(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(5, None).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
                ValueExpr::Int(6, None).into_empty_span().into(),
            ),
        ),
        (
            "!true",
            ValueExpr::BoolNegate(ValueExpr::Bool(true).into_empty_span().into()),
        ),
        (
            "!{1;2;true}",
            ValueExpr::BoolNegate(
                ValueExpr::Block(vec![
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Int(2, None).into_empty_span(),
                    ValueExpr::Bool(true).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!x()",
            ValueExpr::BoolNegate(
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!!x()",
            ValueExpr::BoolNegate(
                ValueExpr::BoolNegate(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!!x.y.z",
            ValueExpr::BoolNegate(
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!x.y.z",
            ValueExpr::BoolNegate(
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: var("x"),
                        field_name: "y".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "z".into(),
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "x() == y()",
            ValueExpr::Equals(
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                ValueExpr::FunctionCall {
                    target: var("y"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "1 == 2",
            ValueExpr::Equals(
                ValueExpr::Int(1, None).into_empty_span().into(),
                ValueExpr::Int(2, None).into_empty_span().into(),
            ),
        ),
        (
            "!(1 == 2)",
            ValueExpr::BoolNegate(
                ValueExpr::Equals(
                    ValueExpr::Int(1, None).into_empty_span().into(),
                    ValueExpr::Int(2, None).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "!1 == !2",
            ValueExpr::Equals(
                ValueExpr::BoolNegate(ValueExpr::Int(1, None).into_empty_span().into())
                    .into_empty_span()
                    .into(),
                ValueExpr::BoolNegate(ValueExpr::Int(2, None).into_empty_span().into())
                    .into_empty_span()
                    .into(),
            ),
        ),
        ("go {}", ValueExpr::InlineGo(String::new(), None)),
        (
            "a(go {} as Int)",
            ValueExpr::FunctionCall {
                target: Box::new(
                    ValueExpr::RawVariable(false, vec!["a".to_string()]).into_empty_span(),
                ),
                params: vec![
                    ValueExpr::As(
                        ValueExpr::InlineGo("".to_string(), None)
                            .into_empty_span()
                            .into(),
                        TypeExpr::Int.into_empty_span(),
                    )
                    .into_empty_span(),
                ],
                type_params: vec![],
            },
        ),
        (
            "go { go func() {} }",
            ValueExpr::InlineGo(String::from(" go func() {} "), None),
        ),
        (
            "fn() {}",
            ValueExpr::Lambda(
                LambdaFunctionExpr {
                    is_mut: false,
                    params: vec![],
                    return_type: None,
                    value_expr: ValueExpr::Return(Some(
                        ValueExpr::Block(vec![]).into_empty_span().into(),
                    ))
                    .into_empty_span(),
                }
                .into(),
            ),
        ),
        (
            "fn() {1}",
            ValueExpr::Lambda(
                LambdaFunctionExpr {
                    is_mut: false,
                    params: vec![],
                    return_type: None,
                    value_expr: ValueExpr::Int(1, None).into_empty_span_and_block_and_return(),
                }
                .into(),
            ),
        ),
        (
            "fn() -> Int {1}",
            ValueExpr::Lambda(
                LambdaFunctionExpr {
                    is_mut: false,
                    params: vec![],
                    return_type: Some(TypeExpr::Int.into_empty_span()),
                    value_expr: ValueExpr::Int(1, None).into_empty_span_and_block_and_return(),
                }
                .into(),
            ),
        ),
        (
            "fn(x: String) -> Int {1}",
            ValueExpr::Lambda(
                LambdaFunctionExpr {
                    is_mut: false,
                    params: vec![("x".into(), Some(TypeExpr::String(None).into_empty_span()))],
                    return_type: Some(TypeExpr::Int.into_empty_span()),
                    value_expr: ValueExpr::Int(1, None).into_empty_span_and_block_and_return(),
                }
                .into(),
            ),
        ),
        (
            "{x: 1}.x",
            ValueExpr::FieldAccess {
                target_obj: ValueExpr::Duck(vec![(
                    "x".into(),
                    ValueExpr::Int(1, None).into_empty_span(),
                )])
                .into_empty_span()
                .into(),
                field_name: "x".into(),
            },
        ),
        (
            "(1,(3,4),\"s\").0",
            ValueExpr::FieldAccess {
                target_obj: ValueExpr::Tuple(vec![
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Tuple(vec![
                        ValueExpr::Int(3, None).into_empty_span(),
                        ValueExpr::Int(4, None).into_empty_span(),
                    ])
                    .into_empty_span(),
                    ValueExpr::String("s".into(), true).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
                field_name: "0".into(),
            },
        ),
        (
            "{go {} 1;2}",
            ValueExpr::Block(vec![
                ValueExpr::InlineGo("".into(), None).into_empty_span(),
                ValueExpr::Int(1, None).into_empty_span(),
                ValueExpr::Int(2, None).into_empty_span(),
            ]),
        ),
        (
            "if (true) {go {} 1;2}",
            ValueExpr::If {
                condition: ValueExpr::Bool(true).into_empty_span().into(),
                then: ValueExpr::Block(vec![
                    ValueExpr::InlineGo("".into(), None).into_empty_span(),
                    ValueExpr::Int(1, None).into_empty_span(),
                    ValueExpr::Int(2, None).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
                r#else: None,
            },
        ),
        (
            "match (5) { Int @i => i }",
            ValueExpr::Match {
                value_expr: Box::new(ValueExpr::Int(5, None).into_empty_span()),
                arms: vec![MatchArm {
                    type_case: TypeExpr::Int.into_empty_span(),
                    base: None,
                    identifier_binding: Some("i".to_string()),
                    value_expr: *var("i"),
                    condition: None,
                    span: empty_range(),
                }],
                else_arm: None,
                span: empty_range(),
            },
        ),
        (
            "match (\"Hallo\") { String @s => s, Int @i => i }",
            ValueExpr::Match {
                value_expr: Box::new(
                    ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                ),
                arms: vec![
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::String(None).into_empty_span(),
                        identifier_binding: Some("s".to_string()),
                        value_expr: *var("s"),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        type_case: TypeExpr::Int.into_empty_span(),
                        base: None,
                        identifier_binding: Some("i".to_string()),
                        value_expr: *var("i"),
                        condition: None,
                        span: empty_range(),
                    },
                ],
                else_arm: None,
                span: empty_range(),
            },
        ),
        (
            "match (\"Hallo\") { String @s if s => s, Int @i => i }",
            ValueExpr::Match {
                value_expr: Box::new(
                    ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                ),
                arms: vec![
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::String(None).into_empty_span(),
                        identifier_binding: Some("s".to_string()),
                        value_expr: *var("s"),
                        condition: Some(*var("s")),
                        span: empty_range(),
                    },
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::Int.into_empty_span(),
                        identifier_binding: Some("i".to_string()),
                        value_expr: *var("i"),
                        condition: None,
                        span: empty_range(),
                    },
                ],
                else_arm: None,
                span: empty_range(),
            },
        ),
        (
            "match (\"Hallo\") { String @s => s, Int @i => i, Other @o => {
            return o
            } }",
            ValueExpr::Match {
                value_expr: Box::new(
                    ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                ),
                arms: vec![
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::String(None).into_empty_span(),
                        identifier_binding: Some("s".to_string()),
                        value_expr: *var("s"),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        type_case: TypeExpr::Int.into_empty_span(),
                        identifier_binding: Some("i".to_string()),
                        base: None,
                        value_expr: *var("i"),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], vec![])
                            .into_empty_span(),
                        identifier_binding: Some("o".to_string()),
                        base: None,
                        value_expr: ValueExpr::Block(vec![
                            ValueExpr::Return(Some(var("o"))).into_empty_span(),
                        ])
                        .into_empty_span(),
                        condition: None,
                        span: empty_range(),
                    },
                ],
                else_arm: None,
                span: empty_range(),
            },
        ),
        (
            "match (\"Hallo\") {
            String @s => s,
            Int @i => i,
            Other @o => {
            return o
            },
            Other @o => {
            match (o) {
            String @s => s
            }
            },
            }",
            ValueExpr::Match {
                value_expr: Box::new(
                    ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                ),
                arms: vec![
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::String(None).into_empty_span(),
                        identifier_binding: Some("s".to_string()),
                        value_expr: *var("s"),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::Int.into_empty_span(),
                        identifier_binding: Some("i".to_string()),
                        value_expr: *var("i"),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], vec![])
                            .into_empty_span(),
                        identifier_binding: Some("o".to_string()),
                        value_expr: ValueExpr::Block(vec![
                            ValueExpr::Return(Some(var("o"))).into_empty_span(),
                        ])
                        .into_empty_span(),
                        condition: None,
                        span: empty_range(),
                    },
                    MatchArm {
                        base: None,
                        type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], vec![])
                            .into_empty_span(),
                        identifier_binding: Some("o".to_string()),
                        value_expr: ValueExpr::Block(vec![
                            ValueExpr::Match {
                                value_expr: var("o"),
                                arms: vec![MatchArm {
                                    base: None,
                                    type_case: TypeExpr::String(None).into_empty_span(),
                                    identifier_binding: Some("s".to_string()),
                                    value_expr: *var("s"),
                                    condition: None,
                                    span: empty_range(),
                                }],
                                else_arm: None,
                                span: empty_range(),
                            }
                            .into_empty_span(),
                        ])
                        .into_empty_span(),
                        condition: None,
                        span: empty_range(),
                    },
                ],
                else_arm: None,
                span: empty_range(),
            },
        ),
        ("&mut a", ValueExpr::RefMut(var("a"))),
        (
            "&mut { x: 10 }",
            ValueExpr::RefMut(
                ValueExpr::Duck(vec![(
                    "x".to_string(),
                    ValueExpr::Int(10, None).into_empty_span(),
                )])
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "&{ x: 10 }",
            ValueExpr::Ref(
                ValueExpr::Duck(vec![(
                    "x".to_string(),
                    ValueExpr::Int(10, None).into_empty_span(),
                )])
                .into_empty_span()
                .into(),
            ),
        ),
        ("&a", ValueExpr::Ref(var("a"))),
        ("*a", ValueExpr::Deref(var("a"))),
        (
            "&*a",
            ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into()),
        ),
        (
            "&&*a",
            ValueExpr::Ref(
                ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                    .into_empty_span()
                    .into(),
            ),
        ),
        (
            "&mut &&*a",
            ValueExpr::RefMut(
                ValueExpr::Ref(
                    ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "*&mut &&*a",
            ValueExpr::Deref(
                ValueExpr::RefMut(
                    ValueExpr::Ref(
                        ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                            .into_empty_span()
                            .into(),
                    )
                    .into_empty_span()
                    .into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "*a.x",
            ValueExpr::Deref(
                ValueExpr::FieldAccess {
                    target_obj: var("a"),
                    field_name: "x".to_string(),
                }
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "*a * *b",
            ValueExpr::Mul(
                ValueExpr::Deref(var("a")).into_empty_span().into(),
                ValueExpr::Deref(var("b")).into_empty_span().into(),
            ),
        ),
        (
            "*a = 10",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::Deref(var("a")).into_empty_span(),
                        value_expr: ValueExpr::Int(10, None).into_empty_span(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
        (
            "*a.x = 10",
            ValueExpr::VarAssign(
                (
                    Assignment {
                        target: ValueExpr::Deref(
                            ValueExpr::FieldAccess {
                                target_obj: var("a"),
                                field_name: "x".to_string(),
                            }
                            .into_empty_span()
                            .into(),
                        )
                        .into_empty_span(),
                        value_expr: ValueExpr::Int(10, None).into_empty_span(),
                    },
                    empty_range(),
                )
                    .into(),
            ),
        ),
    ];

    for (i, (src, expected_tokens)) in test_cases.into_iter().enumerate() {
        let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
        dbg!(&lex_result);
        let parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));

        assert_eq!(
            parse_result.has_errors(),
            false,
            "{i}: {} {:?} {:?}",
            src,
            lex_result,
            parse_result
        );
        assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

        let mut output = parse_result.into_result().expect(&src);
        value_expr_into_empty_range(&mut output);

        assert_eq!(output.0, expected_tokens, "{i}: {}", src);
    }
}

#[test]
pub fn test_declaration_parser() {
    let inputs_and_expected_outputs = vec![
        (
            "let x: String = \"\"",
            Declaration {
                name: "x".to_string(),
                type_expr: Some(TypeExpr::String(None).into_empty_span()),
                initializer: Some(ValueExpr::String("".to_string(), true).into_empty_span()),
                is_const: false,
            },
        ),
        (
            "let y: { x: Int } = {}",
            Declaration {
                name: "y".to_string(),
                type_expr: Some(
                    TypeExpr::Duck(Duck {
                        fields: vec![Field::new("x".to_string(), TypeExpr::Int.into_empty_span())],
                    })
                    .into_empty_span(),
                ),
                initializer: Some(ValueExpr::Block(vec![]).into_empty_span()),
                is_const: false,
            },
        ),
        (
            "let z: {} = {}",
            Declaration {
                name: "z".to_string(),
                type_expr: Some(TypeExpr::Any.into_empty_span()),
                initializer: Some(empty_block().into_empty_span()),
                is_const: false,
            },
        ),
    ];

    for (input, expected_output) in inputs_and_expected_outputs {
        let lexer_parse_result = lex_parser("test", "").parse(input);
        assert_eq!(lexer_parse_result.has_errors(), false);
        assert_eq!(lexer_parse_result.has_output(), true);

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!()
        };

        let declaration_parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
        assert_eq!(declaration_parse_result.has_errors(), false);
        assert_eq!(declaration_parse_result.has_output(), true);

        let Some((ValueExpr::VarDecl(mut declaration), _)) = declaration_parse_result.into_output()
        else {
            unreachable!()
        };

        if let Some(initializer) = declaration.0.initializer.as_mut() {
            value_expr_into_empty_range(initializer);
        }

        if let Some(type_expr) = &mut declaration.0.type_expr {
            type_expr_into_empty_range(type_expr);
        }

        assert_eq!(declaration.0, expected_output.into());
    }

    let valid_declarations = vec![
        "let x: String = \"\"",
        "let x: { x: String, y: String } = { x: \"\", y: \"\" }",
        "let y: { x: String, y: String } = { x: \"\", y: \"\" }",
        "let z: { h: String, x: { y: String }} = {}",
        "let x: { h: String, x: { y: String }} = 0",
        "let x: { h: String, x: { y: String }} = true",
        "let x: { h: String, x: { y: String }} = false",
        "let x: { h: Int, x: { y: Int }} = { h: 4, x: { y: 8 } }",
        "let x: Int = false",
        "let x: String = \"Hallo, Welt!\"",
        "let x: go \"sync.WaitGroup\" = {}",
    ];

    for valid_declaration in valid_declarations {
        println!("lexing {valid_declaration}");
        let lexer_parse_result = lex_parser("test", "").parse(valid_declaration);
        assert_eq!(lexer_parse_result.has_errors(), false);
        assert_eq!(lexer_parse_result.has_output(), true);

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!()
        };

        println!("declaration parsing {valid_declaration}");
        let typedef_parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
        assert_eq!(typedef_parse_result.has_errors(), false);
        assert_eq!(typedef_parse_result.has_output(), true);
    }
}

#[test]
pub fn test_assignment_parser() {
    let valid_assignments = vec![
        "y = 1",
        "{y = 1}",
        "while(true){y = 1}",
        "while(true){y = y + 1}",
        "{let y: Int = 0; while(true){y = 1;println(y)}}",
        "x = 580",
        "y = 80",
        "y = true",
        "y = false",
        "y = \"Hallo\"",
    ];

    for valid_assignment in valid_assignments {
        let lexer_parse_result = lex_parser("test", "").parse(valid_assignment);
        assert_eq!(lexer_parse_result.has_errors(), false, "{valid_assignment}");
        assert_eq!(lexer_parse_result.has_output(), true);

        let Some(tokens) = lexer_parse_result.into_output() else {
            unreachable!()
        };

        println!("typedef_parsing {valid_assignment}");
        let typedef_parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
        assert_eq!(typedef_parse_result.has_errors(), false);
        assert_eq!(typedef_parse_result.has_output(), true);
    }
}

fn int(i: u64) -> Box<Spanned<ValueExpr>> {
    ValueExpr::Int(i, None).into_empty_span().into()
}

fn add(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Add(lhs, rhs)
}

fn sub(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Sub(lhs, rhs)
}

fn mul(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Mul(lhs, rhs)
}

fn div(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Div(lhs, rhs)
}

fn r#mod(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Mod(lhs, rhs)
}

fn eq(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Equals(lhs, rhs)
}

fn ne(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::NotEquals(lhs, rhs)
}

fn lt(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::LessThan(lhs, rhs)
}

fn lte(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::LessThanOrEquals(lhs, rhs)
}

fn gt(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::GreaterThan(lhs, rhs)
}

fn gte(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::GreaterThanOrEquals(lhs, rhs)
}

fn and(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::And(lhs, rhs)
}

fn or(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
    ValueExpr::Or(lhs, rhs)
}

#[test]
fn test_arithmetic_expressions() {
    let test_cases = vec![
        ("1 + 2", add(int(1), int(2))),
        ("10 - 3", sub(int(10), int(3))),
        ("4 * 5", mul(int(4), int(5))),
        ("20 / 4", div(int(20), int(4))),
        ("21 % 5", r#mod(int(21), int(5))),
        (
            "2 + 3 * 4",
            add(int(2), mul(int(3), int(4)).into_empty_span().into()),
        ),
        (
            "10 - 8 / 2",
            sub(int(10), div(int(8), int(2)).into_empty_span().into()),
        ),
        (
            "1 + 10 % 3",
            add(int(1), r#mod(int(10), int(3)).into_empty_span().into()),
        ),
        (
            "10 - 2 + 3",
            add(sub(int(10), int(2)).into_empty_span().into(), int(3)),
        ),
        (
            "100 / 10 / 2",
            div(div(int(100), int(10)).into_empty_span().into(), int(2)),
        ),
        (
            "10 * 2 / 5 * 3",
            mul(
                div(mul(int(10), int(2)).into_empty_span().into(), int(5))
                    .into_empty_span()
                    .into(),
                int(3),
            ),
        ),
        (
            "[] as Int[]",
            ValueExpr::As(
                ValueExpr::Array(vec![], None).into_empty_span().into(),
                TypeExpr::Array(TypeExpr::Int.into_empty_span().into()).into_empty_span(),
            ),
        ),
        (
            "(2 + 3) * 4",
            mul(add(int(2), int(3)).into_empty_span().into(), int(4)),
        ),
        (
            "2 * (3 + 4)",
            mul(int(2), add(int(3), int(4)).into_empty_span().into()),
        ),
        (
            "(100 - 5 * 3) / 5 + 25 % 6",
            add(
                div(
                    sub(int(100), mul(int(5), int(3)).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                    int(5),
                )
                .into_empty_span()
                .into(),
                r#mod(int(25), int(6)).into_empty_span().into(),
            ),
        ),
        (
            "a() + b() * c()",
            add(
                ValueExpr::FunctionCall {
                    target: var("a"),
                    params: vec![],
                    type_params: vec![],
                }
                .into_empty_span()
                .into(),
                mul(
                    ValueExpr::FunctionCall {
                        target: var("b"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("c"),
                        params: vec![],
                        type_params: vec![],
                    }
                    .into_empty_span()
                    .into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "1 + (2 + (3 + 4))",
            add(
                int(1),
                add(int(2), add(int(3), int(4)).into_empty_span().into())
                    .into_empty_span()
                    .into(),
            ),
        ),
        (
            "1 * 2 + 3 * 4",
            add(
                mul(int(1), int(2)).into_empty_span().into(),
                mul(int(3), int(4)).into_empty_span().into(),
            ),
        ),
        (
            "1 as String",
            ValueExpr::As(
                ValueExpr::Int(1, None).into_empty_span().into(),
                TypeExpr::String(None).into_empty_span(),
            ),
        ),
    ];

    for (i, (src, expected_ast)) in test_cases.into_iter().enumerate() {
        let lex_result = lex_parser("test", "")
            .parse(src)
            .into_result()
            .unwrap_or_else(|_| panic!("Lexer failed on case {i}: '{src}'"));

        let parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));

        if parse_result.has_errors() {
            panic!(
                "parse failed for {i}: '{src}'. Errors: {:?}",
                parse_result.into_errors()
            );
        }

        let mut output = parse_result.into_output().unwrap();

        value_expr_into_empty_range(&mut output);
        assert_eq!(output.0, expected_ast, "unexpected ast {i}: '{src}'");
    }
}

#[test]
fn test_comparison_and_logical_expressions() {
    let test_cases = vec![
        ("1 < 2", lt(int(1), int(2))),
        ("1 <= 1", lte(int(1), int(1))),
        ("5 > 3", gt(int(5), int(3))),
        ("5 >= 5", gte(int(5), int(5))),
        ("1 == 2", eq(int(1), int(2))),
        ("1 != 2", ne(int(1), int(2))),
        (
            "true and false",
            and(
                ValueExpr::Bool(true).into_empty_span().into(),
                ValueExpr::Bool(false).into_empty_span().into(),
            ),
        ),
        (
            "true or false",
            or(
                ValueExpr::Bool(true).into_empty_span().into(),
                ValueExpr::Bool(false).into_empty_span().into(),
            ),
        ),
        (
            "1 + 2 < 4",
            lt(add(int(1), int(2)).into_empty_span().into(), int(4)),
        ),
        (
            "3 < 4 == true",
            eq(
                lt(int(3), int(4)).into_empty_span().into(),
                ValueExpr::Bool(true).into_empty_span().into(),
            ),
        ),
        (
            "true == true and false",
            and(
                eq(
                    ValueExpr::Bool(true).into_empty_span().into(),
                    ValueExpr::Bool(true).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
                ValueExpr::Bool(false).into_empty_span().into(),
            ),
        ),
        (
            "false or true and true",
            or(
                ValueExpr::Bool(false).into_empty_span().into(),
                and(
                    ValueExpr::Bool(true).into_empty_span().into(),
                    ValueExpr::Bool(true).into_empty_span().into(),
                )
                .into_empty_span()
                .into(),
            ),
        ),
        (
            "1 < 2 and 3 > 1",
            and(
                lt(int(1), int(2)).into_empty_span().into(),
                gt(int(3), int(1)).into_empty_span().into(),
            ),
        ),
    ];

    for (i, (src, expected_ast)) in test_cases.into_iter().enumerate() {
        let lex_result = lex_parser("test", "")
            .parse(src)
            .into_result()
            .unwrap_or_else(|_| panic!("Lexer failed on case {i}: '{src}'"));

        let parse_result =
            value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));
        if parse_result.has_errors() {
            panic!(
                "parse failed for {i}: '{src}'. Errors: {:?}",
                parse_result.into_errors()
            );
        }

        let mut output = parse_result.into_output().unwrap();

        value_expr_into_empty_range(&mut output);

        assert_eq!(output.0, expected_ast, "ast mismatch on {i}: '{src}'");
    }
}
