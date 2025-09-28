use std::collections::HashSet;

use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::parse::{
    Field, SS, Spanned,
    function_parser::{FunctionDefintion, function_definition_parser},
    generics_parser::{Generic, generics_parser},
    type_parser::type_expression_parser,
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<FunctionDefintion>,
    pub mut_methods: HashSet<String>,
    pub generics: Option<Vec<Spanned<Generic>>>,
}

pub fn struct_definition_parser<'src, M, I>(
    make_input: M,
) -> impl Parser<'src, I, StructDefinition, extra::Err<Rich<'src, Token, SS>>> + Clone
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
                .or_not()
                .map(|x| x.is_some())
                .then(function_definition_parser(make_input)))
            .repeated()
            .at_least(0)
            .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::ControlChar('}')))
        .or_not()
        .map(|x| x.or_else(|| Some(vec![])).unwrap());

    just(Token::Struct)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then(generics_parser().or_not())
        .then_ignore(just(Token::ControlChar('=')))
        .then_ignore(just(Token::ControlChar('{')))
        .then(
            field_parser
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect(),
        )
        .then_ignore(just(Token::ControlChar('}')))
        .then(impl_parser)
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(((identifier, generics), fields), methods)| {
            let (mut_methods_names, methods) = methods.into_iter().fold(
                (HashSet::new(), Vec::new()),
                |(mut mut_method_names, mut methods), (is_mut, elem)| {
                    if is_mut {
                        mut_method_names.insert(elem.name.clone());
                    }
                    methods.push(elem);
                    (mut_method_names, methods)
                },
            );
            StructDefinition {
                name: identifier,
                fields,
                methods,
                mut_methods: mut_methods_names,
                generics,
            }
        })
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
            TypeExpr::Fun(params, return_type) => TypeExpr::Fun(
                params
                    .into_iter()
                    .map(|(name, param_type_expr)| (name, strip_spans(param_type_expr)))
                    .collect(),
                return_type.map(|rt_box| Box::new(strip_spans(*rt_box))),
            ),
            TypeExpr::Or(variants) => TypeExpr::Or(variants.into_iter().map(strip_spans).collect()),
            TypeExpr::TypeName(is_global, type_name, Some(generics)) => TypeExpr::TypeName(
                is_global,
                type_name,
                Some(
                    generics
                        .into_iter()
                        .map(|generic| strip_spans(generic))
                        .collect::<Vec<_>>(),
                ),
            ),
            TypeExpr::RawTypeName(is_global, raw_type_name, Some(generics)) => {
                TypeExpr::RawTypeName(
                    is_global,
                    raw_type_name,
                    Some(
                        generics
                            .into_iter()
                            .map(|generic| strip_spans(generic))
                            .collect::<Vec<_>>(),
                    ),
                )
            }
            TypeExpr::Array(type_expr) => {
                TypeExpr::Array(Box::new(strip_spans(type_expr.as_ref().clone())))
            }
            other => other,
        };
        (stripped_expr, empty_range())
    }

    fn strip_struct_definition_spans(mut def: StructDefinition) -> StructDefinition {
        for field in &mut def.fields {
            field.type_expr = strip_spans(field.type_expr.clone());
        }
        if let Some(generics) = &mut def.generics {
            for generic in generics {
                generic.1 = empty_range();
            }
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

        let parsed_def = parse_result.output().unwrap().clone();
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
            "struct Point = { x: Int, y: Int };",
            StructDefinition {
                name: "Point".to_string(),
                fields: vec![
                    Field::new("x".to_string(), TypeExpr::Int.into_empty_span()),
                    Field::new("y".to_string(), TypeExpr::Int.into_empty_span()),
                ],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: None,
            },
        );

        assert_struct_definition(
            "struct Empty = {};",
            StructDefinition {
                name: "Empty".to_string(),
                fields: vec![],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: None,
            },
        );

        assert_struct_definition(
            "struct User = { id: Int, name: String, };",
            StructDefinition {
                name: "User".to_string(),
                fields: vec![
                    Field::new("id".to_string(), TypeExpr::Int.into_empty_span()),
                    Field::new("name".to_string(), TypeExpr::String.into_empty_span()),
                ],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: None,
            },
        );

        assert_struct_definition(
            "struct Option<T> = { value: T };",
            StructDefinition {
                name: "Option".to_string(),
                fields: vec![Field::new(
                    "value".to_string(),
                    TypeExpr::RawTypeName(false, vec!["T".to_string()], None).into_empty_span(),
                )],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: Some(vec![(
                    Generic {
                        name: "T".to_string(),
                        constraint: None,
                    },
                    empty_range(),
                )]),
            },
        );

        assert_struct_definition(
            "struct Map<K, V> = { entries: Entry<K, V>[] };",
            StructDefinition {
                name: "Map".to_string(),
                fields: vec![Field::new(
                    "entries".to_string(),
                    TypeExpr::Array(Box::new(
                        TypeExpr::RawTypeName(
                            false,
                            vec!["Entry".to_string()],
                            Some(vec![
                                TypeExpr::RawTypeName(false, vec!["K".to_string()], None)
                                    .into_empty_span(),
                                TypeExpr::RawTypeName(false, vec!["V".to_string()], None)
                                    .into_empty_span(),
                            ]),
                        )
                        .into_empty_span(),
                    ))
                    .into_empty_span(),
                )],
                methods: vec![],
                mut_methods: HashSet::new(),
                generics: Some(vec![
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
                ]),
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
