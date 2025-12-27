use std::collections::{HashMap, HashSet};

use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::parse::{
    Field, SS, Spanned, failure_with_occurence,
    function_parser::{FunctionDefintion, function_definition_parser},
    generics_parser::{Generic, generics_parser},
    type_parser::type_expression_parser,
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDuckDefinition {
    pub name: String,
    pub fields: Vec<Field>,
    pub generics: Vec<Spanned<Generic>>,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum DerivableInterface {
    Eq,
    ToString,
    Ord,
    Clone,
    ToJson,
    Hash,
    FromJson,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<FunctionDefintion>,
    pub mut_methods: HashSet<String>,
    pub generics: Vec<Spanned<Generic>>,
    pub doc_comments: Vec<Spanned<String>>,
    pub derived: HashSet<DerivableInterface>,
}

pub fn struct_definition_parser<'src, M, I>(
    make_input: M,
) -> impl Parser<'src, I, (StructDefinition, Vec<FunctionDefintion>), extra::Err<Rich<'src, Token, SS>>>
+ Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let field_parser = select_ref! { Token::Ident(identifier) => identifier.clone() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| Field::new(identifier, type_expr));

    let impl_parser = just(Token::Impl)
        .ignore_then(just(Token::ControlChar('{')))
        .ignore_then(
            (just(Token::Mut)
                .or(just(Token::Static))
                .or_not()
                .then(function_definition_parser(make_input))
                .map_with(|x, e| (x, e.span())))
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ControlChar('}')))
        .or_not()
        .map(|x| x.or_else(|| Some(vec![])).unwrap());

    #[non_exhaustive]
    #[derive(Debug, Clone, PartialEq)]
    enum StructAttribute {
        Auto { impls: Vec<Spanned<String>> },
    }

    let with_parser = (just(Token::Ident("auto".to_string()))
        .ignore_then(
            select_ref! { Token::Ident(i) => i.to_string() }
                .map_with(|x, e| (x, e.span()))
                .separated_by(just(Token::ControlChar(',')))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))),
        )
        .map(|x| StructAttribute::Auto { impls: x }))
    .separated_by(just(Token::ControlChar(',')))
    .at_least(1)
    .collect::<Vec<_>>()
    .delimited_by(just(Token::ControlChar('[')), just(Token::ControlChar(']')));

    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    doc_comments_parser
        .then(with_parser.or_not())
        .then_ignore(just(Token::Struct))
        .then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then(generics_parser().or_not())
        .then_ignore(just(Token::ControlChar('{')))
        .then(
            field_parser
                .map_with(|x, e| (x, e.span()))
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ControlChar('}')))
        .then(impl_parser)
        .map(
            |(((((doc_comments, attributes), identifier), generics), fields), methods)| {
                let mut names_with_spans = HashMap::new();

                for f in &fields {
                    if let Some(decl_span) = names_with_spans.get(&f.0.name) {
                        let msg = format!("{} declared multiple times", f.0.name);

                        let declared_here_again_msg = format!("You declared {} here", f.0.name);
                        let other_msg = format!("But you already declared {} here", f.0.name);

                        failure_with_occurence(
                            msg.clone(),
                            f.1,
                            [(declared_here_again_msg, f.1), (other_msg, *decl_span)],
                        );
                    }
                    names_with_spans.insert(f.0.name.clone(), f.1);
                }

                for m in &methods {
                    if let Some(decl_span) = names_with_spans.get(&m.0.1.name) {
                        let msg = format!("{} declared multiple times", m.0.1.name);

                        let declared_here_again_msg = format!("You declared {} here", m.0.1.name);
                        let other_msg = format!("But you already declared {} here", m.0.1.name);

                        failure_with_occurence(
                            msg.clone(),
                            m.1,
                            [(declared_here_again_msg, m.1), (other_msg, *decl_span)],
                        );
                    }
                    names_with_spans.insert(m.0.1.name.clone(), m.1);
                }

                let (mut_methods_names, methods, static_methods) = methods.into_iter().fold(
                    (HashSet::new(), Vec::new(), Vec::new()),
                    |(mut mut_method_names, mut methods, mut static_methods),
                     ((modifier, elem), _)| {
                        if modifier.as_ref().is_some_and(|v| matches!(v, Token::Mut)) {
                            mut_method_names.insert(elem.name.clone());
                        }

                        if modifier
                            .as_ref()
                            .is_some_and(|v| matches!(v, Token::Static))
                        {
                            static_methods.push(elem);
                        } else {
                            methods.push(elem);
                        }

                        (mut_method_names, methods, static_methods)
                    },
                );

                let fields: Vec<Field> = fields.into_iter().map(|(f, _)| f).collect();
                let mut derived = HashMap::new();

                if let Some(attributes) = attributes {
                    for attribute in attributes {
                        #[allow(irrefutable_let_patterns)]
                        if let StructAttribute::Auto { impls } = attribute {
                            for (i, span) in impls {
                                let a = match i.as_str() {
                                    "Eq" => DerivableInterface::Eq,
                                    "ToString" => DerivableInterface::ToString,
                                    "Ord" => DerivableInterface::Ord,
                                    "Clone" => DerivableInterface::Clone,
                                    "Hash" => DerivableInterface::Hash,
                                    "ToJson" => DerivableInterface::ToJson,
                                    "FromJson" => DerivableInterface::FromJson,
                                    _ => {
                                        let msg = &format!("Invalid with declaration {i}");
                                        failure_with_occurence(msg, span, [(msg, span)]);
                                    }
                                };
                                if let Some(decl_span) = derived.get(&a) {
                                    let msg = format!("{i} already declared");
                                    let other_msg = format!("{i} already declared here");
                                    failure_with_occurence(
                                        msg.clone(),
                                        span,
                                        [(msg, span), (other_msg, *decl_span)],
                                    );
                                }
                                derived.insert(a, span);
                            }
                        }
                    }
                }

                let derived = derived.into_iter().fold(HashSet::new(), |mut acc, (e, _)| {
                    acc.insert(e);
                    acc
                });

                (
                    StructDefinition {
                        name: identifier,
                        fields,
                        methods,
                        mut_methods: mut_methods_names,
                        generics: generics.unwrap_or_default(),
                        doc_comments: doc_comments.unwrap_or_else(Vec::new),
                        derived,
                    },
                    static_methods,
                )
            },
        )
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{
        generics_parser::Generic, lexer::lex_parser, make_input, type_parser::TypeExpr,
        value_parser::empty_range,
    };
    use chumsky::Parser;

    use super::*;

    fn strip_spans(spanned_type_expr: Spanned<TypeExpr>) -> Spanned<TypeExpr> {
        let (expr, _span) = spanned_type_expr;
        let stripped_expr = match expr {
            TypeExpr::Duck(d) => TypeExpr::Duck(crate::parse::type_parser::Duck {
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
            TypeExpr::TypeName(is_global, type_name, generics) => TypeExpr::TypeName(
                is_global,
                type_name,
                generics
                    .into_iter()
                    .map(|generic| strip_spans(generic))
                    .collect::<Vec<_>>(),
            ),
            TypeExpr::RawTypeName(is_global, raw_type_name, generics) => TypeExpr::RawTypeName(
                is_global,
                raw_type_name,
                generics
                    .into_iter()
                    .map(|generic| strip_spans(generic))
                    .collect::<Vec<_>>(),
            ),
            TypeExpr::Array(type_expr) => {
                TypeExpr::Array(Box::new(strip_spans(type_expr.as_ref().clone())))
            }
            other => other,
        };
        (stripped_expr, empty_range())
    }

    fn strip_struct_definition_spans(mut def: StructDefinition) -> StructDefinition {
        for comment in &mut def.doc_comments {
            comment.1 = empty_range();
        }

        for field in &mut def.fields {
            field.type_expr = strip_spans(field.type_expr.clone());
        }

        for generic in &mut def.generics {
            generic.1 = empty_range();
        }

        def
    }

    fn assert_struct_definition(input_str: &str, expected_def: StructDefinition) {
        println!("lexing and parsing struct definition: \"{}\"", input_str);
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

        let tokens = lexer_parse_result.output().unwrap();

        let parse_result =
            struct_definition_parser(make_input).parse(make_input(empty_range(), &tokens));

        assert!(
            !parse_result.has_errors(),
            "parsing errors for \"{}\": {:?}",
            input_str,
            parse_result
                .errors()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
        );

        let parsed_def = parse_result.output().unwrap().clone().0;
        let stripped_parsed = strip_struct_definition_spans(parsed_def);

        assert_eq!(
            stripped_parsed.name, expected_def.name,
            "Struct name mismatch for \"{}\"",
            input_str
        );

        assert_eq!(
            stripped_parsed.methods, expected_def.methods,
            "Struct methods mismatch for \"{}\"",
            input_str
        );

        assert_eq!(
            stripped_parsed.generics, expected_def.generics,
            "Struct generics mismatch for \"{}\"",
            input_str
        );

        assert_eq!(
            stripped_parsed.fields.len(),
            expected_def.fields.len(),
            "Field count mismatch for \"{}\"",
            input_str
        );

        let fields_match = stripped_parsed
            .fields
            .iter()
            .zip(expected_def.fields.iter())
            .all(|(p, e)| p.name == e.name && p.type_expr.0 == e.type_expr.0);

        assert!(
            fields_match,
            "Fields do not match for \"{}\".\nParsed: {:?}\nExpected: {:?}",
            input_str, stripped_parsed.fields, expected_def.fields
        );
    }

    #[test]
    fn test_struct_definition_parser() {
        assert_struct_definition(
            "struct Point { x: Int, y: Int }",
            StructDefinition {
                name: "Point".to_string(),
                fields: vec![
                    Field::new("x".to_string(), TypeExpr::Int.into_empty_span()),
                    Field::new("y".to_string(), TypeExpr::Int.into_empty_span()),
                ],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![],
                doc_comments: vec![],
                derived: Default::default(),
            },
        );

        assert_struct_definition(
            "struct Empty {}",
            StructDefinition {
                name: "Empty".to_string(),
                fields: vec![],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![],
                doc_comments: vec![],
                derived: Default::default(),
            },
        );

        assert_struct_definition(
            "struct User { id: Int, name: String, }",
            StructDefinition {
                name: "User".to_string(),
                fields: vec![
                    Field::new("id".to_string(), TypeExpr::Int.into_empty_span()),
                    Field::new("name".to_string(), TypeExpr::String(None).into_empty_span()),
                ],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![],
                doc_comments: vec![],
                derived: Default::default(),
            },
        );

        assert_struct_definition(
            "struct Option<T> { value: T }",
            StructDefinition {
                name: "Option".to_string(),
                fields: vec![Field::new(
                    "value".to_string(),
                    TypeExpr::RawTypeName(false, vec!["T".to_string()], vec![]).into_empty_span(),
                )],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![(
                    Generic {
                        name: "T".to_string(),
                        constraint: None,
                    },
                    empty_range(),
                )],
                doc_comments: vec![],
                derived: Default::default(),
            },
        );

        assert_struct_definition(
            "[auto(Eq)] struct S {}",
            StructDefinition {
                name: "S".to_string(),
                derived: {
                    let mut s = HashSet::new();
                    s.insert(DerivableInterface::Eq);
                    s
                },
                fields: vec![],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![],
                doc_comments: vec![],
            },
        );

        assert_struct_definition(
            "struct Map<K, V> { entries: Entry<K, V>[] }",
            StructDefinition {
                name: "Map".to_string(),
                derived: Default::default(),
                fields: vec![Field::new(
                    "entries".to_string(),
                    TypeExpr::Array(Box::new(
                        TypeExpr::RawTypeName(
                            false,
                            vec!["Entry".to_string()],
                            vec![
                                TypeExpr::RawTypeName(false, vec!["K".to_string()], vec![])
                                    .into_empty_span(),
                                TypeExpr::RawTypeName(false, vec!["V".to_string()], vec![])
                                    .into_empty_span(),
                            ],
                        )
                        .into_empty_span(),
                    ))
                    .into_empty_span(),
                )],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![
                    (
                        Generic {
                            name: "K".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                    (
                        Generic {
                            name: "V".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                ],
                doc_comments: vec![],
            },
        );

        assert_struct_definition(
            "/// hello\nstruct Map<K, V> { entries: Entry<K, V>[] }",
            StructDefinition {
                name: "Map".to_string(),
                derived: Default::default(),
                fields: vec![Field::new(
                    "entries".to_string(),
                    TypeExpr::Array(Box::new(
                        TypeExpr::RawTypeName(
                            false,
                            vec!["Entry".to_string()],
                            vec![
                                TypeExpr::RawTypeName(false, vec!["K".to_string()], vec![])
                                    .into_empty_span(),
                                TypeExpr::RawTypeName(false, vec!["V".to_string()], vec![])
                                    .into_empty_span(),
                            ],
                        )
                        .into_empty_span(),
                    ))
                    .into_empty_span(),
                )],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: vec![
                    (
                        Generic {
                            name: "K".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                    (
                        Generic {
                            name: "V".to_string(),
                            constraint: None,
                        },
                        empty_range(),
                    ),
                ],
                doc_comments: vec![("hello".to_string(), empty_range())],
            },
        );

        let invalid_structs = vec![
            "type MissingBody = ;",
            "type MissingSemi = { x: Int }",
            "type BadField = { x: Int, y };",
            "type BadComma = { x: Int,, y: Bool };",
            "type X = String;",
        ];

        for invalid in invalid_structs {
            println!("testing invalid struct: \"{}\"", invalid);
            let lexer_parse_result = lex_parser("test", "").parse(invalid);
            let tokens = lexer_parse_result.output().unwrap();
            let parse_result =
                struct_definition_parser(make_input).parse(make_input(empty_range(), &tokens));
            assert!(parse_result.has_errors());
        }
    }
}
