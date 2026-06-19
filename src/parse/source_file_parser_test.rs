use chumsky::Parser;
use std::{collections::HashSet, path::PathBuf};

use crate::parse::{
    Field,
    function_parser::FunctionDefintion,
    jsx_component_parser::JsxComponent,
    lexer::lex_parser,
    make_input,
    schema_def_parser::{IfBranch, SchemaDefinition, SchemaField},
    source_file_parser::{SourceFile, source_file_parser},
    struct_parser::StructDefinition,
    type_parser::{Duck, TypeDefinition, TypeExpr},
    use_statement_parser::UseStatement,
    value_parser::{
        IntoBlock, ValueExpr, empty_range, source_file_into_empty_range,
        type_expr_into_empty_range, value_expr_into_empty_range,
    },
};

#[test]
fn do_test() {
    let test_cases = vec![
        (
            "fn abc(){}",
            SourceFile {
                function_definitions: vec![FunctionDefintion {
                    name: "abc".into(),
                    return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                    ..Default::default()
                }],
                ..Default::default()
            },
        ),
        (
            "schema Yoo = { name: String if true }",
            SourceFile {
                schema_defs: vec![SchemaDefinition {
                    name: "Yoo".into(),
                    fields: vec![SchemaField {
                        name: "name".to_string(),
                        type_expr: (TypeExpr::String(None), empty_range()),
                        if_branch: Some((
                            IfBranch {
                                condition: (ValueExpr::Bool(true), empty_range()),
                                value_expr: None,
                            },
                            empty_range(),
                        )),
                        else_branch_value_expr: None,
                        span: empty_range(),
                    }],
                    comments: vec![],
                    span: empty_range(),
                    out_type: None,
                    schema_fn_type: None,
                }],
                ..Default::default()
            },
        ),
        (
            "component MyComp() jsx {console.log('hallo, welt')}",
            SourceFile {
                jsx_components: vec![JsxComponent {
                    name: "MyComp".to_string(),
                    props_type: TypeExpr::Duck(Duck { fields: vec![] }).into_empty_span(),
                    javascript_source: ("console.log('hallo, welt')".to_string(), empty_range()),
                }],
                ..Default::default()
            },
        ),
        (
            "component MyComp(props: {x: String, y: Int}) jsx {console.log('hallo, welt')}",
            SourceFile {
                jsx_components: vec![JsxComponent {
                    name: "MyComp".to_string(),
                    props_type: TypeExpr::Duck(Duck {
                        fields: vec![
                            Field {
                                name: "x".to_string(),
                                type_expr: TypeExpr::String(None).into_empty_span(),
                            },
                            Field {
                                name: "y".to_string(),
                                type_expr: TypeExpr::Int.into_empty_span(),
                            },
                        ],
                    })
                    .into_empty_span(),
                    javascript_source: ("console.log('hallo, welt')".to_string(), empty_range()),
                }],
                ..Default::default()
            },
        ),
        (
            "fn abc(){}fn xyz(){}",
            SourceFile {
                function_definitions: vec![
                    FunctionDefintion {
                        name: "abc".into(),
                        return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                        ..Default::default()
                    },
                    FunctionDefintion {
                        name: "xyz".into(),
                        return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            },
        ),
        (
            "use x;",
            SourceFile {
                use_statements: vec![UseStatement::Regular(false, vec!["x".into()])],
                ..Default::default()
            },
        ),
        (
            "type X = {x: String};",
            SourceFile {
                type_definitions: vec![TypeDefinition {
                    name: "X".into(),
                    type_expression: TypeExpr::Duck(Duck {
                        fields: vec![Field::new(
                            "x".into(),
                            TypeExpr::String(None).into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                    generics: vec![],
                }],
                ..Default::default()
            },
        ),
        (
            "struct X {x: String}",
            SourceFile {
                struct_definitions: vec![StructDefinition {
                    name: "X".into(),
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String(None).into_empty_span(),
                    )],
                    methods: vec![],
                    mut_methods: HashSet::new(),
                    generics: vec![],
                    doc_comments: vec![],
                    derived: Default::default(),
                }],
                ..Default::default()
            },
        ),
        (
            "module abc {}",
            SourceFile {
                sub_modules: vec![(
                    "abc".into(),
                    SourceFile {
                        ..Default::default()
                    },
                )],
                ..Default::default()
            },
        ),
        (
            "module abc {module xyz{}}",
            SourceFile {
                sub_modules: vec![(
                    "abc".into(),
                    SourceFile {
                        sub_modules: vec![(
                            "xyz".into(),
                            SourceFile {
                                ..Default::default()
                            },
                        )],
                        ..Default::default()
                    },
                )],
                ..Default::default()
            },
        ),
        (
            "module abc {use test_mod; module xyz { use lol; } fn abc() {} }",
            SourceFile {
                sub_modules: vec![(
                    "abc".into(),
                    SourceFile {
                        sub_modules: vec![(
                            "xyz".into(),
                            SourceFile {
                                use_statements: vec![UseStatement::Regular(
                                    false,
                                    vec!["lol".into()],
                                )],
                                ..Default::default()
                            },
                        )],
                        use_statements: vec![UseStatement::Regular(false, vec!["test_mod".into()])],
                        function_definitions: vec![FunctionDefintion {
                            name: "abc".into(),
                            return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                            ..Default::default()
                        }],
                        ..Default::default()
                    },
                )],
                ..Default::default()
            },
        ),
        (
            "use x;fn abc() -> String {}type X = {x: String};fn xyz(){}",
            SourceFile {
                function_definitions: vec![
                    FunctionDefintion {
                        name: "abc".into(),
                        return_type: TypeExpr::String(None).into_empty_span(),
                        ..Default::default()
                    },
                    FunctionDefintion {
                        name: "xyz".into(),
                        return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
                        ..Default::default()
                    },
                ],
                use_statements: vec![UseStatement::Regular(false, vec!["x".into()])],
                type_definitions: vec![TypeDefinition {
                    name: "X".into(),
                    type_expression: TypeExpr::Duck(Duck {
                        fields: vec![Field::new(
                            "x".into(),
                            TypeExpr::String(None).into_empty_span(),
                        )],
                    })
                    .into_empty_span(),
                    generics: vec![],
                }],
                ..Default::default()
            },
        ),
    ];

    for (src, exp) in test_cases {
        let lex = lex_parser("test", "").parse(src).into_result().expect(src);
        let mut parse = source_file_parser(PathBuf::from("test_files"), make_input)
            .parse(make_input(empty_range(), &lex))
            .into_result()
            .expect(src);

        source_file_into_empty_range(&mut parse);

        for c in parse.jsx_components.iter_mut() {
            type_expr_into_empty_range(&mut c.props_type);
        }

        assert_eq!(parse, exp, "{src}");
    }
}

#[test]
fn test_mod_structure() {
    let test_cases = vec![
        ("01.duck", SourceFile::default()),
        (
            "02.duck",
            SourceFile {
                sub_modules: vec![
                    ("abc".into(), SourceFile::default()),
                    ("xyz".into(), SourceFile::default()),
                ],
                ..Default::default()
            },
        ),
        (
            "03.duck",
            SourceFile {
                sub_modules: vec![
                    (
                        "abc".into(),
                        SourceFile {
                            sub_modules: vec![("lol".into(), SourceFile::default())],
                            ..SourceFile::default()
                        },
                    ),
                    (
                        "xyz".into(),
                        SourceFile {
                            sub_modules: vec![("foo".into(), SourceFile::default())],
                            ..SourceFile::default()
                        },
                    ),
                ],
                ..Default::default()
            },
        ),
        (
            "04.duck",
            SourceFile {
                sub_modules: vec![("empty".into(), SourceFile::default())],
                ..Default::default()
            },
        ),
        (
            "05.duck",
            SourceFile {
                sub_modules: vec![
                    ("empty".into(), SourceFile::default()),
                    (
                        "single".into(),
                        SourceFile {
                            function_definitions: vec![FunctionDefintion {
                                name: "my_single_fun".into(),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                    ),
                ],
                ..Default::default()
            },
        ),
        (
            "06.duck",
            SourceFile {
                sub_modules: vec![(
                    "multiple".into(),
                    SourceFile {
                        function_definitions: vec![
                            FunctionDefintion {
                                name: "some_abc_func".into(),
                                value_expr: ValueExpr::Return(Some(
                                    ValueExpr::String("Hello from module".into(), true)
                                        .into_empty_span()
                                        .into_block()
                                        .into(),
                                ))
                                .into_empty_span(),
                                ..Default::default()
                            },
                            FunctionDefintion {
                                name: "some_xyz_func".into(),
                                value_expr: ValueExpr::Return(Some(
                                    ValueExpr::Int(1, None)
                                        .into_empty_span()
                                        .into_block()
                                        .into(),
                                ))
                                .into_empty_span(),
                                ..Default::default()
                            },
                        ],
                        ..Default::default()
                    },
                )],
                ..Default::default()
            },
        ),
        (
            "07.duck",
            SourceFile {
                sub_modules: vec![
                    (
                        "multiple".into(),
                        SourceFile {
                            function_definitions: vec![
                                FunctionDefintion {
                                    name: "some_abc_func".into(),
                                    value_expr: ValueExpr::String("Hello from module".into(), true)
                                        .into_empty_span_and_block_and_return(),
                                    ..Default::default()
                                },
                                FunctionDefintion {
                                    name: "some_xyz_func".into(),
                                    value_expr: ValueExpr::Int(1, None)
                                        .into_empty_span_and_block_and_return(),
                                    ..Default::default()
                                },
                            ],
                            ..Default::default()
                        },
                    ),
                    (
                        "nested".into(),
                        SourceFile {
                            function_definitions: vec![FunctionDefintion {
                                name: "hello_from_x".into(),
                                ..Default::default()
                            }],
                            sub_modules: vec![(
                                "level1".into(),
                                SourceFile {
                                    function_definitions: vec![FunctionDefintion {
                                        name: "hello_from_y".into(),
                                        ..Default::default()
                                    }],
                                    sub_modules: vec![(
                                        "level2".into(),
                                        SourceFile {
                                            function_definitions: vec![FunctionDefintion {
                                                name: "hello_from_z".into(),
                                                ..Default::default()
                                            }],
                                            ..Default::default()
                                        },
                                    )],
                                    ..Default::default()
                                },
                            )],
                            ..Default::default()
                        },
                    ),
                    ("empty".into(), SourceFile::default()),
                    ("another_mod".into(), SourceFile::default()),
                ],
                ..Default::default()
            },
        ),
        (
            "08.duck",
            SourceFile {
                sub_modules: vec![
                    (
                        "multiple".into(),
                        SourceFile {
                            function_definitions: vec![
                                FunctionDefintion {
                                    name: "some_abc_func".into(),
                                    value_expr: ValueExpr::String("Hello from module".into(), true)
                                        .into_empty_span_and_block_and_return(),
                                    ..Default::default()
                                },
                                FunctionDefintion {
                                    name: "some_xyz_func".into(),
                                    value_expr: ValueExpr::Int(1, None)
                                        .into_empty_span_and_block_and_return(),
                                    ..Default::default()
                                },
                            ],
                            ..Default::default()
                        },
                    ),
                    (
                        "nested".into(),
                        SourceFile {
                            function_definitions: vec![FunctionDefintion {
                                name: "hello_from_x".into(),
                                ..Default::default()
                            }],
                            sub_modules: vec![(
                                "level1".into(),
                                SourceFile {
                                    function_definitions: vec![FunctionDefintion {
                                        name: "hello_from_y".into(),
                                        ..Default::default()
                                    }],
                                    sub_modules: vec![(
                                        "level2".into(),
                                        SourceFile {
                                            function_definitions: vec![FunctionDefintion {
                                                name: "hello_from_z".into(),
                                                ..Default::default()
                                            }],
                                            ..Default::default()
                                        },
                                    )],
                                    ..Default::default()
                                },
                            )],
                            ..Default::default()
                        },
                    ),
                    (
                        "nested2".into(),
                        SourceFile {
                            function_definitions: vec![FunctionDefintion {
                                name: "hello_from_x".into(),
                                ..Default::default()
                            }],
                            sub_modules: vec![(
                                "level1".into(),
                                SourceFile {
                                    function_definitions: vec![FunctionDefintion {
                                        name: "hello_from_y".into(),
                                        ..Default::default()
                                    }],
                                    sub_modules: vec![(
                                        "level2".into(),
                                        SourceFile {
                                            function_definitions: vec![
                                                FunctionDefintion {
                                                    name: "this_is_a_func".into(),
                                                    ..Default::default()
                                                },
                                                FunctionDefintion {
                                                    name: "this_is_another_func".into(),
                                                    ..Default::default()
                                                },
                                                FunctionDefintion {
                                                    name: "yet_another".into(),
                                                    ..Default::default()
                                                },
                                            ],
                                            ..Default::default()
                                        },
                                    )],
                                    ..Default::default()
                                },
                            )],
                            ..Default::default()
                        },
                    ),
                ],
                ..Default::default()
            },
        ),
    ];

    let dir = PathBuf::from("tests/test_files/modules");

    for (main_file, mut expected) in test_cases {
        let src = std::fs::read_to_string(dir.join(main_file)).unwrap();
        let lex = lex_parser("test", "").parse(&src).unwrap();
        let mut got = source_file_parser(dir.clone(), make_input)
            .parse(make_input(empty_range(), &lex))
            .unwrap();
        source_file_into_empty_range(&mut got);
        fn sort_all(x: &mut SourceFile) {
            x.function_definitions.sort_by_key(|x| x.name.clone());
            for (_, s) in x.sub_modules.iter_mut() {
                sort_all(s);
            }
        }

        sort_all(&mut expected);
        sort_all(&mut got);

        assert_eq!(expected, got, "{main_file}");
    }
}

#[test]
fn test_flatten() {
    let test_cases = vec![
        (
            SourceFile::default().flatten(&vec![], false),
            SourceFile::default(),
        ),
        (
            SourceFile::default(),
            SourceFile {
                sub_modules: vec![
                    ("abc".into(), SourceFile::default()),
                    ("xyz".into(), SourceFile::default()),
                ],
                ..Default::default()
            },
        ),
        // todo(@Apfelfrosch): respect the new way to create structs
        // (
        //     SourceFile {
        //         type_definitions: vec![TypeDefinition {
        //             name: mangle(&["abc", "TestStruct"]),
        //             type_expression: TypeExpr::Struct(StructDefinition {
        //                 name: "TestStruct",
        //                 fields: vec![Field {
        //                     name: "recv".into(),
        //                     type_expr: TypeExpr::TypeName(
        //                         false,
        //                         mangle(&["abc", "TestStruct"]),
        //                         None,
        //                     )
        //                     .into_empty_span(),
        //                 }],
        //                 methods: vec![],
        //                 generics: None,
        //             })
        //             .into_empty_span(),
        //             generics: None,
        //         }],
        //         use_statements: vec![UseStatement::Go("fmt".into(), None)],
        //         function_definitions: vec![
        //             FunctionDefintion {
        //                 name: mangle(&["abc", "lol", "im_a_func"]),
        //                 value_expr: ValueExpr::Block(vec![
        //                     ValueExpr::FunctionCall {
        //                         target: ValueExpr::Variable(
        //                             true,
        //                             mangle(&["abc", "lol", "called"]),
        //                             None,
        //                         )
        //                         .into_empty_span()
        //                         .into(),
        //                         params: vec![],
        //                         type_params: None,
        //                     }
        //                     .into_empty_span(),
        //                 ])
        //                 .into_empty_span(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: mangle(&["abc", "im_calling_a_sub_module"]),
        //                 value_expr: ValueExpr::Block(vec![
        //                     ValueExpr::Variable(true, mangle(&["abc", "lol", "called"]), None)
        //                         .into_empty_span(),
        //                 ])
        //                 .into_empty_span(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: mangle(&["abc", "lol", "called"]),
        //                 value_expr: ValueExpr::Block(vec![
        //                     ValueExpr::FunctionCall {
        //                         target: ValueExpr::Variable(
        //                             true,
        //                             mangle(&["abc", "lol", "called"]),
        //                             None,
        //                         )
        //                         .into_empty_span()
        //                         .into(),
        //                         params: vec![],
        //                         type_params: None,
        //                     }
        //                     .into_empty_span(),
        //                 ])
        //                 .into_empty_span(),
        //                 ..Default::default()
        //             },
        //         ],
        //         ..Default::default()
        //     },
        //     SourceFile {
        //         sub_modules: vec![
        //             (
        //                 "abc".into(),
        //                 SourceFile {
        //                     use_statements: vec![UseStatement::Regular(
        //                         false,
        //                         vec![
        //                             Indicator::Module("lol".into()),
        //                             Indicator::Symbols(vec!["called".into()]),
        //                         ],
        //                     )],
        //                     type_definitions: vec![TypeDefinition {
        //                         name: "TestStruct".into(),
        //                         type_expression: TypeExpr::Struct(StructDefinition {
        //                             fields: vec![Field {
        //
        //                                 name: "recv".into(),
        //                                 type_expr: TypeExpr::RawTypeName(
        //                                     false,
        //                                     vec!["TestStruct".into()],
        //                                     None,
        //                                 )
        //                                 .into_empty_span(),
        //                             }],
        //                         })
        //                         .into_empty_span(),
        //                         generics: None,
        //                     }],
        //                     function_definitions: vec![FunctionDefintion {
        //                         name: "im_calling_a_sub_module".into(),
        //                         value_expr: ValueExpr::Block(vec![
        //                             ValueExpr::RawVariable(false, vec!["called".into()])
        //                                 .into_empty_span(),
        //                         ])
        //                         .into_empty_span(),
        //                         ..Default::default()
        //                     }],
        //                     sub_modules: vec![(
        //                         "lol".into(),
        //                         SourceFile {
        //                             use_statements: vec![UseStatement::Go("fmt".into(), None)],
        //                             function_definitions: vec![
        //                                 FunctionDefintion {
        //                                     name: "im_a_func".into(),
        //                                     value_expr: ValueExpr::Block(vec![
        //                                         ValueExpr::FunctionCall {
        //                                             target: ValueExpr::RawVariable(
        //                                                 false,
        //                                                 vec!["called".into()],
        //                                             )
        //                                             .into_empty_span()
        //                                             .into(),
        //                                             params: vec![],
        //                                             type_params: None,
        //                                         }
        //                                         .into_empty_span(),
        //                                     ])
        //                                     .into_empty_span(),
        //                                     ..Default::default()
        //                                 },
        //                                 FunctionDefintion {
        //                                     name: "called".into(),
        //                                     value_expr: ValueExpr::Block(vec![
        //                                         ValueExpr::FunctionCall {
        //                                             target: ValueExpr::RawVariable(
        //                                                 false,
        //                                                 vec!["called".into()],
        //                                             )
        //                                             .into_empty_span()
        //                                             .into(),
        //                                             params: vec![],
        //                                             type_params: None,
        //                                         }
        //                                         .into_empty_span(),
        //                                     ])
        //                                     .into_empty_span(),
        //                                     ..Default::default()
        //                                 },
        //                             ],
        //                             ..SourceFile::default()
        //                         },
        //                     )],
        //                     ..SourceFile::default()
        //                 },
        //             ),
        //             (
        //                 "xyz".into(),
        //                 SourceFile {
        //                     sub_modules: vec![("foo".into(), SourceFile::default())],
        //                     ..SourceFile::default()
        //                 },
        //             ),
        //         ],
        //         ..Default::default()
        //     },
        // ),
        // (
        //     SourceFile::default(),
        //     SourceFile {
        //         sub_modules: vec![("empty".into(), SourceFile::default())],
        //         ..Default::default()
        //     },
        // ),
        // (
        //     SourceFile {
        //         function_definitions: vec![FunctionDefintion {
        //             name: "single_my_single_fun".into(),
        //             ..Default::default()
        //         }],
        //         ..Default::default()
        //     },
        //     SourceFile {
        //         sub_modules: vec![
        //             ("empty".into(), SourceFile::default()),
        //             (
        //                 "single".into(),
        //                 SourceFile {
        //                     function_definitions: vec![FunctionDefintion {
        //                         name: "my_single_fun".into(),
        //                         ..Default::default()
        //                     }],
        //                     ..Default::default()
        //                 },
        //             ),
        //         ],
        //         ..Default::default()
        //     },
        // ),
        // (
        //     SourceFile {
        //         function_definitions: vec![
        //             FunctionDefintion {
        //                 name: "multiple_some_abc_func".into(),
        //                 value_expr: ValueExpr::String("Hello from module".into())
        //                     .into_empty_span_and_block(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: "multiple_some_xyz_func".into(),
        //                 value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
        //                 ..Default::default()
        //             },
        //         ],
        //         ..Default::default()
        //     },
        //     SourceFile {
        //         sub_modules: vec![(
        //             "multiple".into(),
        //             SourceFile {
        //                 function_definitions: vec![
        //                     FunctionDefintion {
        //                         name: "some_abc_func".into(),
        //                         value_expr: ValueExpr::String("Hello from module".into())
        //                             .into_empty_span_and_block(),
        //                         ..Default::default()
        //                     },
        //                     FunctionDefintion {
        //                         name: "some_xyz_func".into(),
        //                         value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
        //                         ..Default::default()
        //                     },
        //                 ],
        //                 ..Default::default()
        //             },
        //         )],
        //         ..Default::default()
        //     },
        // ),
        // (
        //     SourceFile {
        //         function_definitions: vec![
        //             FunctionDefintion {
        //                 name: "multiple_some_abc_func".into(),
        //                 value_expr: ValueExpr::String("Hello from module".into())
        //                     .into_empty_span_and_block(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: "multiple_some_xyz_func".into(),
        //                 value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: "nested_hello_from_x".into(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: "nested_level1_hello_from_y".into(),
        //                 ..Default::default()
        //             },
        //             FunctionDefintion {
        //                 name: "nested_level1_level2_hello_from_z".into(),
        //                 ..Default::default()
        //             },
        //         ],
        //         ..Default::default()
        //     },
        //     SourceFile {
        //         sub_modules: vec![
        //             (
        //                 "multiple".into(),
        //                 SourceFile {
        //                     function_definitions: vec![
        //                         FunctionDefintion {
        //                             name: "some_abc_func".into(),
        //                             value_expr: ValueExpr::String("Hello from module".into())
        //                                 .into_empty_span_and_block(),
        //                             ..Default::default()
        //                         },
        //                         FunctionDefintion {
        //                             name: "some_xyz_func".into(),
        //                             value_expr: ValueExpr::Int(1).into_empty_span_and_block(),
        //                             ..Default::default()
        //                         },
        //                     ],
        //                     ..Default::default()
        //                 },
        //             ),
        //             (
        //                 "nested".into(),
        //                 SourceFile {
        //                     function_definitions: vec![FunctionDefintion {
        //                         name: "hello_from_x".into(),
        //                         ..Default::default()
        //                     }],
        //                     sub_modules: vec![(
        //                         "level1".into(),
        //                         SourceFile {
        //                             function_definitions: vec![FunctionDefintion {
        //                                 name: "hello_from_y".into(),
        //                                 ..Default::default()
        //                             }],
        //                             sub_modules: vec![(
        //                                 "level2".into(),
        //                                 SourceFile {
        //                                     function_definitions: vec![FunctionDefintion {
        //                                         name: "hello_from_z".into(),
        //                                         ..Default::default()
        //                                     }],
        //                                     ..Default::default()
        //                                 },
        //                             )],
        //                             ..Default::default()
        //                         },
        //                     )],
        //                     ..Default::default()
        //                 },
        //             ),
        //             ("empty".into(), SourceFile::default()),
        //             ("another_mod".into(), SourceFile::default()),
        //         ],
        //         ..Default::default()
        //     },
        // ),
    ];

    for (i, (mut expected, mut original)) in test_cases.into_iter().enumerate() {
        fn sort_all(x: &mut SourceFile) {
            x.function_definitions.sort_by_key(|x| x.name.clone());
            x.type_definitions.sort_by_key(|x| x.name.clone());
            for (_, s) in x.sub_modules.iter_mut() {
                sort_all(s);
            }
        }
        source_file_into_empty_range(&mut original);

        let mut original = original.flatten(&vec![], false);

        sort_all(&mut expected);
        sort_all(&mut original);

        for func in original.function_definitions.iter_mut() {
            func.span = empty_range();
            value_expr_into_empty_range(&mut func.value_expr);
        }

        assert_eq!(expected, original, "{i}");
    }
}
