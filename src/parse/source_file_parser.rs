use std::{fs::File, io::Read, path::{Path, PathBuf}};

use chumsky::prelude::*;

use crate::parse::{
    function_parser::{function_definition_parser, FunctionDefintion},
    lexer::{lexer, Token},
    type_parser::{type_definition_parser, TypeDefinition},
    use_statement_parser::{use_statement_parser, UseStatement},
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
                    e.clone().delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))).map(Some)
                )))
                .map(|(name, src)| {
                    println!("X: {} {}", name, p.to_str().unwrap());
                    if let Some(src) = src {
                        SourceUnit::Module(name, src)
                    } else {
                        let mut f = File::open(dbg!(p.join(&name)));
                        if let Ok(f) = f && f.metadata().unwrap().is_dir() {
                            let src_text = std::fs::read_to_string(p.join(&name).join("mod.duck")).unwrap();
                            let lex = lexer().parse(&src_text).unwrap();
                            let parse = source_file_parser(p.join(&name)).parse(&lex).unwrap();
                            SourceUnit::Module(name, parse)
                        } else {
                            let mut src_text = std::fs::read_to_string(p.join(format!("{name}.duck"))).unwrap();
                            let lex = lexer().parse(&src_text).unwrap();
                            let parse = source_file_parser(p.clone()).parse(&lex).unwrap();
                            SourceUnit::Module(name, parse)
                        }
                    }
                })
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
        source_file_parser::{SourceFile, source_file_parser},
        type_parser::{Duck, TypeDefinition, TypeExpr},
        use_statement_parser::{Indicator, UseStatement},
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
                    use_statements: vec![UseStatement::Regular(
                        vec![Indicator::Module("x".into())],
                    )],
                    ..Default::default()
                },
            ),
            (
                "type X = {x: String};",
                SourceFile {
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![("x".into(), TypeExpr::String)],
                        }),
                    }],
                    ..Default::default()
                },
            ),
            (
                "module abc {}",
                SourceFile {
                    sub_modules: vec![
                        ("abc".into(), SourceFile {
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
                }
            ),
            ("module xx;",
            SourceFile {
                sub_modules: vec![
                    ("xx".into(), Default::default())
                ],
                ..Default::default()
            }),
            (
                "module abc {module xyz{}}",
                SourceFile {
                    sub_modules: vec![
                        ("abc".into(), SourceFile {
                            sub_modules: vec![
                                ("xyz".into(), SourceFile {
                                    ..Default::default()
                                })
                            ],
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
                }
            ),
            (
                "module abc {use test; module xyz { use lol; } fun abc() {} }",
                SourceFile {
                    sub_modules: vec![
                        ("abc".into(), SourceFile {
                            sub_modules: vec![
                                ("xyz".into(), SourceFile {
                                    use_statements: vec![
                                        UseStatement::Regular(vec![Indicator::Module("lol".into())])
                                    ],
                                    ..Default::default()
                                })
                            ],
                            use_statements: vec![
                                UseStatement::Regular(vec![Indicator::Module("test".into())])
                            ],
                            function_definitions: vec![
                                FunctionDefintion {
                                    name: "abc".into(),
                                    ..Default::default()
                                }
                            ],
                            ..Default::default()
                        })
                    ],
                    ..Default::default()
                }
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
                    use_statements: vec![UseStatement::Regular(
                        vec![Indicator::Module("x".into())],
                    )],
                    type_definitions: vec![TypeDefinition {
                        name: "X".into(),
                        type_expression: TypeExpr::Duck(Duck {
                            fields: vec![("x".into(), TypeExpr::String)],
                        }),
                    }],
                    ..Default::default()
                },
            ),
        ];

        for (src, exp) in test_cases {
            let lex = lexer().parse(src).into_result().expect(src);
            let parse = source_file_parser(PathBuf::from(".")).parse(&lex).into_result().expect(src);
            assert_eq!(parse, exp, "{src}");
        }
    }
}
