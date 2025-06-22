use chumsky::{IterParser, Parser, prelude::choice};

use crate::parse::{
    function_parser::{FunctionDefintion, function_definition_parser},
    lexer::Token,
    type_parser::{TypeDefinition, type_definition_parser},
    use_statement_parser::{UseStatement, use_statement_parser},
};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceFile {
    pub function_definitions: Vec<FunctionDefintion>,
    pub type_definitions: Vec<TypeDefinition>,
    pub use_statements: Vec<UseStatement>,
}

#[derive(Debug, Clone)]
pub enum SourceUnit {
    Func(FunctionDefintion),
    Type(TypeDefinition),
    Use(UseStatement),
}

pub fn source_file_parser<'src>() -> impl Parser<'src, &'src [Token], SourceFile> {
    choice((
        use_statement_parser().map(SourceUnit::Use),
        type_definition_parser().map(SourceUnit::Type),
        function_definition_parser().map(SourceUnit::Func),
    ))
    .repeated()
    .collect::<Vec<_>>()
    .map(|xs| {
        let mut f = Vec::new();
        let mut t = Vec::new();
        let mut u = Vec::new();

        for x in xs {
            use SourceUnit::*;
            match x {
                Func(def) => f.push(def),
                Type(def) => t.push(def),
                Use(def) => u.push(def),
            }
        }

        SourceFile {
            function_definitions: f,
            type_definitions: t,
            use_statements: u,
        }
    })
}

#[cfg(test)]
mod tests {
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
                },
            ),
        ];

        for (src, exp) in test_cases {
            let lex = lexer().parse(src).into_result().expect(src);
            let parse = source_file_parser().parse(&lex).into_result().expect(src);
            assert_eq!(parse, exp, "{src}");
        }
    }
}
