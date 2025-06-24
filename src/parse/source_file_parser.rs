use std::{fs::File, path::PathBuf};

use chumsky::prelude::*;

use crate::parse::{
    function_parser::{FunctionDefintion, function_definition_parser},
    lexer::{Token, lexer},
    type_parser::{TypeDefinition, type_definition_parser},
    use_statement_parser::{UseStatement, use_statement_parser},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceFile {
    pub function_definitions: Vec<FunctionDefintion>,
    pub type_definitions: Vec<TypeDefinition>,
    pub use_statements: Vec<UseStatement>,
    pub sub_modules: Vec<(String, SourceFile)>,
}

#[derive(Debug, Clone)]
pub enum SourceUnit {
    Func(FunctionDefintion),
    Type(TypeDefinition),
    Use(UseStatement),
    Module(String, SourceFile),
}

impl SourceFile {
    pub fn push_use(&mut self, s: &UseStatement) {
        if !self.use_statements.contains(s) {
            self.use_statements.push(s.to_owned());
        }
    }

    pub fn flatten(&self) -> SourceFile {
        let mut res = SourceFile::default();

        fn flatten0(s: &SourceFile, result: &mut SourceFile, prefix: &str) {
            for func in &s.function_definitions {
                result.function_definitions.push(FunctionDefintion {
                    name: format!("{prefix}{}", func.name),
                    ..func.clone()
                });
            }

            for t in &s.type_definitions {
                result.type_definitions.push(TypeDefinition {
                    name: format!("{prefix}_{}", t.name),
                    ..t.clone()
                });
            }

            for u in &s.use_statements {
                if matches!(u, UseStatement::Go(..)) {
                    result.push_use(u);
                }
            }

            for (name, sub_module) in &s.sub_modules {
                flatten0(sub_module, result, &format!("{prefix}{name}_"));
            }
        }

        flatten0(self, &mut res, "");

        res
    }
}

fn module_descent(name: String, current_dir: PathBuf) -> SourceFile {
    let joined = current_dir.join(&name);
    let mod_dir = File::open(&joined);
    if let Ok(mod_dir) = mod_dir
        && mod_dir.metadata().unwrap().is_dir()
    {
        let combined = std::fs::read_dir(&joined)
            .unwrap()
            .filter_map(|dir_entry| match dir_entry {
                Ok(dir_entry)
                    if dir_entry.metadata().unwrap().is_dir()
                        || dir_entry.file_name().to_string_lossy().ends_with(".duck") =>
                {
                    Some(dir_entry)
                }
                _ => None,
            })
            .map(|dir_entry| {
                (
                    dir_entry.metadata().unwrap().is_file(),
                    module_descent(
                        dir_entry
                            .file_name()
                            .into_string()
                            .unwrap()
                            .split(".duck")
                            .next()
                            .unwrap()
                            .into(),
                        joined.clone(),
                    ),
                )
            })
            .fold(SourceFile::default(), |mut acc, (i, x)| {
                dbg!(&x);
                if i {
                    acc.function_definitions.extend(x.function_definitions);
                    acc.type_definitions.extend(x.type_definitions);
                    acc.sub_modules.extend(x.sub_modules);
                }
                acc.use_statements.extend(x.use_statements);
                acc
            });
        combined
    } else {
        let src_text =
            std::fs::read_to_string(dbg!(format!("{}.duck", joined.to_str().unwrap()))).unwrap();
        let lex = lexer().parse(&src_text).unwrap();
        let parse = source_file_parser(current_dir.clone()).parse(&lex).unwrap();
        parse
    }
}

pub fn source_file_parser<'src>(p: PathBuf) -> impl Parser<'src, &'src [Token], SourceFile> {
    let p = Box::leak(Box::new(p));
    recursive(|e| {
        choice((
            use_statement_parser().map(SourceUnit::Use),
            type_definition_parser().map(SourceUnit::Type),
            function_definition_parser().map(SourceUnit::Func),
            just(Token::Module)
                .ignore_then(select_ref! { Token::Ident(i) => i.to_owned() })
                .then(choice((
                    just(Token::ControlChar(';')).to(None),
                    e.clone()
                        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                        .map(Some),
                )))
                .map(|(name, src)| {
                    println!("X: {} {}", name, p.to_str().unwrap());
                    if let Some(src) = src {
                        SourceUnit::Module(name, src)
                    } else {
                        SourceUnit::Module(name.clone(), module_descent(name.clone(), p.clone()))
                    }
                }),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .map(|xs| {
            let mut f = Vec::new();
            let mut t = Vec::new();
            let mut u = Vec::new();
            let mut s = Vec::new();

            for x in xs {
                use SourceUnit::*;
                match x {
                    Func(def) => f.push(def),
                    Type(def) => t.push(def),
                    Use(def) => u.push(def),
                    Module(name, def) => s.push((name, def)),
                }
            }

            SourceFile {
                function_definitions: f,
                type_definitions: t,
                use_statements: u,
                sub_modules: s,
            }
        })
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use chumsky::Parser;

    use crate::parse::{
        function_parser::FunctionDefintion,
        lexer::lexer,
        source_file_parser::{source_file_parser, SourceFile},
        type_parser::{Duck, Field, TypeDefinition, TypeExpr},
        use_statement_parser::{Indicator, UseStatement},
        value_parser::ValueExpr,
    };

    #[test]
    fn do_test() {
        let test_cases = vec![
            (
                "fun abc(){}",
                SourceFile {
                    function_definitions: vec![FunctionDefintion {
                        name: "abc".into(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
            ),
            (
                "fun abc(){}fun xyz(){}",
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "abc".into(),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "xyz".into(),
                            ..Default::default()
                        },
                    ],
                    ..Default::default()
                },
            ),
            (
                "use x;",
                SourceFile {
                    use_statements: vec![UseStatement::Regular(vec![Indicator::Module(
                        "x".into(),
                    )])],
                    ..Default::default()
                },
            ),
            (
                "type X = {x: String};",
                SourceFile {
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![Field::new("x".into(), TypeExpr::String)],
                        }),
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
                "module abc {use test; module xyz { use lol; } fun abc() {} }",
                SourceFile {
                    sub_modules: vec![(
                        "abc".into(),
                        SourceFile {
                            sub_modules: vec![(
                                "xyz".into(),
                                SourceFile {
                                    use_statements: vec![UseStatement::Regular(vec![
                                        Indicator::Module("lol".into()),
                                    ])],
                                    ..Default::default()
                                },
                            )],
                            use_statements: vec![UseStatement::Regular(vec![Indicator::Module(
                                "test".into(),
                            )])],
                            function_definitions: vec![FunctionDefintion {
                                name: "abc".into(),
                                ..Default::default()
                            }],
                            ..Default::default()
                        },
                    )],
                    ..Default::default()
                },
            ),
            (
                "use x;fun abc() -> String {}type X = {x: String};fun xyz(){}",
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "abc".into(),
                            return_type: Some(TypeExpr::String),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "xyz".into(),
                            ..Default::default()
                        },
                    ],
                    use_statements: vec![UseStatement::Regular(vec![Indicator::Module(
                        "x".into(),
                    )])],
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![Field::new("x".into(), TypeExpr::String)],
                        }),
                    }],
                    ..Default::default()
                },
            ),
        ];

        for (src, exp) in test_cases {
            let lex = lexer().parse(src).into_result().expect(src);
            let parse = source_file_parser(PathBuf::from("test_files"))
                .parse(&lex)
                .into_result()
                .expect(src);
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
                                    value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                        "Hello from module".into(),
                                    )]),
                                    ..Default::default()
                                },
                                FunctionDefintion {
                                    name: "some_xyz_func".into(),
                                    value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
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
                                        value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                            "Hello from module".into(),
                                        )]),
                                        ..Default::default()
                                    },
                                    FunctionDefintion {
                                        name: "some_xyz_func".into(),
                                        value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
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
                                        value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                            "Hello from module".into(),
                                        )]),
                                        ..Default::default()
                                    },
                                    FunctionDefintion {
                                        name: "some_xyz_func".into(),
                                        value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
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

        let dir = PathBuf::from("test_files").join("modules");

        for (main_file, mut expected) in test_cases {
            let src = std::fs::read_to_string(dir.join(main_file)).unwrap();
            let lex = lexer().parse(&src).unwrap();
            let mut got = source_file_parser(dir.clone()).parse(&lex).unwrap();

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
            (SourceFile::default().flatten(), SourceFile::default()),
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
            (
                SourceFile::default(),
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
                SourceFile::default(),
                SourceFile {
                    sub_modules: vec![("empty".into(), SourceFile::default())],
                    ..Default::default()
                },
            ),
            (
                SourceFile {
                    function_definitions: vec![FunctionDefintion {
                        name: "single_my_single_fun".into(),
                        ..Default::default()
                    }],
                    ..Default::default()
                },
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
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "multiple_some_abc_func".into(),
                            value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                "Hello from module".into(),
                            )]),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "multiple_some_xyz_func".into(),
                            value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
                            ..Default::default()
                        },
                    ],
                    ..Default::default()
                },
                SourceFile {
                    sub_modules: vec![(
                        "multiple".into(),
                        SourceFile {
                            function_definitions: vec![
                                FunctionDefintion {
                                    name: "some_abc_func".into(),
                                    value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                        "Hello from module".into(),
                                    )]),
                                    ..Default::default()
                                },
                                FunctionDefintion {
                                    name: "some_xyz_func".into(),
                                    value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
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
                SourceFile {
                    function_definitions: vec![
                        FunctionDefintion {
                            name: "multiple_some_abc_func".into(),
                            value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                "Hello from module".into(),
                            )]),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "multiple_some_xyz_func".into(),
                            value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "nested_hello_from_x".into(),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "nested_level1_hello_from_y".into(),
                            ..Default::default()
                        },
                        FunctionDefintion {
                            name: "nested_level1_level2_hello_from_z".into(),
                            ..Default::default()
                        },
                    ],
                    ..Default::default()
                },
                SourceFile {
                    sub_modules: vec![
                        (
                            "multiple".into(),
                            SourceFile {
                                function_definitions: vec![
                                    FunctionDefintion {
                                        name: "some_abc_func".into(),
                                        value_expr: ValueExpr::Block(vec![ValueExpr::String(
                                            "Hello from module".into(),
                                        )]),
                                        ..Default::default()
                                    },
                                    FunctionDefintion {
                                        name: "some_xyz_func".into(),
                                        value_expr: ValueExpr::Block(vec![ValueExpr::Int(1)]),
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
        ];

        for (i, (mut expected, mut original)) in test_cases.into_iter().enumerate() {
            fn sort_all(x: &mut SourceFile) {
                x.function_definitions.sort_by_key(|x| x.name.clone());
                for (_, s) in x.sub_modules.iter_mut() {
                    sort_all(s);
                }
            }

            sort_all(&mut expected);
            sort_all(&mut original);

            assert_eq!(expected, original.flatten(), "{i}");
        }
    }
}
