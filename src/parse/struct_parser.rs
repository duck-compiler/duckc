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

#[cfg(test)]
#[path = "struct_parser_test.rs"]
mod tests;

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
    EmitJs,
}

#[derive(Debug, Clone, PartialEq, Default)]
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
                    if m.0.0.as_ref().is_some_and(|v| matches!(v, Token::Static)) {
                        continue;
                    }
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

                {
                    let mut names_with_spans = HashMap::new();
                    for m in &methods {
                        if !m.0.0.as_ref().is_some_and(|v| matches!(v, Token::Static)) {
                            continue;
                        }
                        if let Some(decl_span) = names_with_spans.get(&m.0.1.name) {
                            let msg = format!("{} declared multiple times", m.0.1.name);

                            let declared_here_again_msg =
                                format!("You declared {} here", m.0.1.name);
                            let other_msg = format!("But you already declared {} here", m.0.1.name);

                            failure_with_occurence(
                                msg.clone(),
                                m.1,
                                [(declared_here_again_msg, m.1), (other_msg, *decl_span)],
                            );
                        }
                        names_with_spans.insert(m.0.1.name.clone(), m.1);
                    }
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
