use std::fmt::{Display, Formatter, Result};

use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::{
    parse::{
        Field, SS, Spanned,
        generics_parser::{Generic, generics_parser},
        value_parser::{TypeParam, empty_range},
    },
    semantics::type_resolve::TypeEnv,
};

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinition {
    pub name: String,
    pub type_expression: Spanned<TypeExpr>,
    pub generics: Vec<Spanned<Generic>>,
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
    Html,
    Any,
    InlineGo,
    Struct {
        name: String,
        type_params: Vec<Spanned<TypeExpr>>,
    },
    Go(String),
    Duck(Duck),
    NamedDuck {
        name: String,
        type_params: Vec<Spanned<TypeExpr>>,
    },
    Tuple(Vec<Spanned<TypeExpr>>),
    RawTypeName(bool, Vec<String>, Vec<Spanned<TypeParam>>),
    TypeName(bool, String, Vec<Spanned<TypeParam>>),
    Tag(String),
    String(Option<String>),
    Int(Option<i64>),
    Bool(Option<bool>),
    Char,
    Float,
    Or(Vec<Spanned<TypeExpr>>),
    And(Vec<Spanned<TypeExpr>>),
    Fun(
        Vec<(Option<String>, Spanned<TypeExpr>)>, // params
        Option<Box<Spanned<TypeExpr>>>,           // return type
        bool,                                     // is mut
    ),
    Array(Box<Spanned<TypeExpr>>),
    TypeOf(String),
    KeyOf(Box<Spanned<TypeExpr>>),
    Ref(Box<Spanned<TypeExpr>>),
    RefMut(Box<Spanned<TypeExpr>>),
}

impl TypeExpr {
    pub fn into_empty_span(self) -> Spanned<TypeExpr> {
        (self, empty_range())
    }

    pub fn as_go_return_type(&self, type_env: &mut TypeEnv) -> String {
        if self.is_unit() {
            String::new()
        } else {
            self.as_go_type_annotation(type_env)
        }
    }

    pub fn primitives() -> Vec<TypeExpr> {
        return vec![
            TypeExpr::Int(None),
            TypeExpr::Float,
            TypeExpr::Bool(None),
            TypeExpr::String(None),
            TypeExpr::Char,
        ];
    }
}

pub fn type_expression_parser_without_array<'src, I>()
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

            let function_param =
                (select_ref! { Token::Ident(identifier) => identifier.to_string() }
                    .then_ignore(just(Token::ControlChar(':'))))
                .or_not()
                .then(p.clone());

            let function_params = function_param
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<(Option<String>, Spanned<TypeExpr>)>>();

            let keyof_expr = just(Token::KeyOf)
                .ignore_then(p.clone().map(Box::new))
                .map(TypeExpr::KeyOf);

            let typeof_expr = just(Token::TypeOf)
                .ignore_then(select_ref! { Token::Ident(identifier) => identifier.clone() })
                .map(TypeExpr::TypeOf);

            let go_type_identifier = just(Token::ControlChar('`'))
                .ignore_then(
                    choice((
                        select_ref! { Token::Ident(ident) => ident.to_string() },
                        just(Token::ControlChar('[')).map(|c| c.to_string()),
                        just(Token::ControlChar(']')).map(|c| c.to_string()),
                        just(Token::ControlChar('.')).map(|c| c.to_string()),
                    ))
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::ControlChar('`')))
                .map(|strs| strs.join(""));

            let go_type = just(Token::Go)
                .ignore_then(go_type_identifier)
                .map(TypeExpr::Go);

            let string_literal = select_ref! { Token::StringLiteral(str) => str.clone() }
                .map(|str| TypeExpr::String(Some(str)));

            let bool_literal =
                select_ref! { Token::BoolLiteral(b) => *b }.map(|b| TypeExpr::Bool(Some(b)));

            let int_literal =
                select_ref! { Token::IntLiteral(int) => *int }.map(|int| TypeExpr::Int(Some(int)));

            let tag_identifier = choice((
                select_ref! { Token::Ident(ident) => ident.to_string() },
                just(Token::ControlChar('.')).map(|_| "DOT".to_string()),
            ))
            .boxed();

            let tag = just(Token::ControlChar('.'))
                .ignore_then(tag_identifier)
                .map(TypeExpr::Tag);

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

            let function = just(Token::Mut)
                .or_not()
                .then(
                    just(Token::Function)
                        .ignore_then(just(Token::ControlChar('(')))
                        .ignore_then(function_params)
                        .then_ignore(just(Token::ControlChar(')')))
                        .then(just(Token::ThinArrow).ignore_then(p.clone()).or_not()),
                )
                .map(|(is_mut, (params, return_type))| {
                    TypeExpr::Fun(params, return_type.map(Box::new), is_mut.is_some())
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
                .then(
                    just(Token::ControlChar('<'))
                        .ignore_then(
                            p.clone()
                                .separated_by(just(Token::ControlChar(',')))
                                .allow_trailing()
                                .at_least(1)
                                .collect::<Vec<Spanned<TypeParam>>>(),
                        )
                        .then_ignore(just(Token::ControlChar('>')))
                        .or_not(),
                )
                .map(
                    |((is_global, identifier), type_params)| match identifier[0].as_str() {
                        "Int" => TypeExpr::Int(None),
                        "Float" => TypeExpr::Float,
                        "Bool" => TypeExpr::Bool(None),
                        "String" => TypeExpr::String(None),
                        "Char" => TypeExpr::Char,
                        "Html" => TypeExpr::Html,
                        _ => TypeExpr::RawTypeName(
                            is_global.is_some(),
                            identifier,
                            type_params.unwrap_or_default(),
                        ),
                    },
                );

            let term_type_expr = p
                .clone()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .or(choice((
                    string_literal,
                    int_literal,
                    bool_literal,
                    tag,
                    typeof_expr,
                    keyof_expr,
                    go_type,
                    type_name,
                    duck,
                    function,
                    tuple,
                ))
                .map_with(|x, e| (x, e.span())));

            #[derive(Debug, Clone)]
            enum RefType {
                Immutable,
                Mutable,
            }

            let ref_parser = choice((
                just(Token::ControlChar('&')).map(|_| RefType::Immutable),
                just(Token::RefMut).map(|_| RefType::Mutable),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(term_type_expr.clone())
            .map_with(|(is_ref, x): (Vec<RefType>, Spanned<TypeExpr>), e| {
                let res = is_ref
                    .into_iter()
                    .rev()
                    .fold(x, |(acc_ty, acc_span), ref_type| match ref_type {
                        RefType::Mutable => (TypeExpr::RefMut((acc_ty, acc_span).into()), acc_span),
                        RefType::Immutable => (TypeExpr::Ref((acc_ty, acc_span).into()), acc_span),
                    });
                (res.0, e.span())
            });

            ref_parser
                .separated_by(just(Token::ControlChar('|')))
                .at_least(1)
                .collect::<Vec<Spanned<TypeExpr>>>()
                .map_with(|elements, e| {
                    if elements.len() == 1 {
                        elements.into_iter().next().unwrap()
                    } else {
                        let mut elems = Vec::new();
                        let expr = (TypeExpr::Or(elements), e.span());
                        merge_or(&expr, &mut elems);
                        (TypeExpr::Or(elems), e.span())
                    }
                })
        },
    )
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

            let function_param =
                (select_ref! { Token::Ident(identifier) => identifier.to_string() }
                    .then_ignore(just(Token::ControlChar(':'))))
                .or_not()
                .then(p.clone());

            let function_params = function_param
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<(Option<String>, Spanned<TypeExpr>)>>();

            let go_type_identifier = just(Token::ControlChar('`'))
                .ignore_then(
                    choice((
                        select_ref! { Token::Ident(ident) => ident.to_string() },
                        just(Token::ControlChar('[')).map(|c| c.to_string()),
                        just(Token::ControlChar(']')).map(|c| c.to_string()),
                        just(Token::ControlChar('.')).map(|c| c.to_string()),
                    ))
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::ControlChar('`')))
                .map(|strs| strs.join(""));

            let go_type = just(Token::Go)
                .ignore_then(go_type_identifier)
                .map(TypeExpr::Go);

            let keyof_expr = just(Token::KeyOf)
                .ignore_then(p.clone().map(Box::new))
                .map(TypeExpr::KeyOf);

            let typeof_expr = just(Token::TypeOf)
                .ignore_then(select_ref! { Token::Ident(identifier) => identifier.clone() })
                .map(TypeExpr::TypeOf);

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

            let string_literal = select_ref! { Token::StringLiteral(str) => str.clone() }
                .map(|str| TypeExpr::String(Some(str.clone())));

            let bool_literal =
                select_ref! { Token::BoolLiteral(b) => *b }.map(|b| TypeExpr::Bool(Some(b)));

            let int_literal =
                select_ref! { Token::IntLiteral(int) => *int }.map(|int| TypeExpr::Int(int.into())); // TODO: unwrap!

            let tag_identifier = choice((
                select_ref! { Token::Ident(ident) => ident.to_string() },
                just(Token::ControlChar('.')).map(|_| "DOT".to_string()),
            ))
            .boxed();

            let tag = just(Token::ControlChar('.'))
                .ignore_then(tag_identifier)
                .map(TypeExpr::Tag);

            let function = just(Token::Mut)
                .or_not()
                .then(
                    just(Token::Function)
                        .ignore_then(just(Token::ControlChar('(')))
                        .ignore_then(function_params)
                        .then_ignore(just(Token::ControlChar(')')))
                        .then(just(Token::ThinArrow).ignore_then(p.clone()).or_not()),
                )
                .map(|(is_mut, (params, return_type))| {
                    TypeExpr::Fun(params, return_type.map(Box::new), is_mut.is_some())
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
                .then(
                    just(Token::ControlChar('<'))
                        .ignore_then(
                            p.clone()
                                .separated_by(just(Token::ControlChar(',')))
                                .allow_trailing()
                                .at_least(1)
                                .collect::<Vec<Spanned<TypeParam>>>(),
                        )
                        .then_ignore(just(Token::ControlChar('>')))
                        .or_not(),
                )
                .map(
                    |((is_global, identifier), type_params)| match identifier[0].as_str() {
                        "Int" => TypeExpr::Int(None),
                        "Float" => TypeExpr::Float,
                        "Bool" => TypeExpr::Bool(None),
                        "String" => TypeExpr::String(None),
                        "Char" => TypeExpr::Char,
                        "Html" => TypeExpr::Html,
                        _ => TypeExpr::RawTypeName(
                            is_global.is_some(),
                            identifier,
                            type_params.unwrap_or_default(),
                        ),
                    },
                );

            let term_type_expr = p
                .clone()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .or(choice((
                    int_literal,
                    bool_literal,
                    string_literal,
                    tag,
                    typeof_expr,
                    keyof_expr,
                    go_type,
                    type_name,
                    duck,
                    function,
                    tuple,
                ))
                .map_with(|x, e| (x, e.span())));

            let array = term_type_expr
                .clone()
                .then(
                    (just(Token::ControlChar('[')).then(just(Token::ControlChar(']'))))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|((x, span), is_array)| {
                    is_array
                        .iter()
                        .fold(x, |acc, _| TypeExpr::Array((acc, span).into()))
                })
                .map_with(|x, e| (x, e.span()));

            #[derive(Debug, Clone)]
            enum RefType {
                Immutable,
                Mutable,
            }

            let ref_parser = choice((
                just(Token::ControlChar('&')).map(|_| RefType::Immutable),
                just(Token::RefMut).map(|_| RefType::Mutable),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(array.clone())
            .map_with(|(is_ref, x): (Vec<RefType>, Spanned<TypeExpr>), e| {
                let res = is_ref
                    .into_iter()
                    .rev()
                    .fold(x, |(acc_ty, acc_span), ref_type| match ref_type {
                        RefType::Mutable => (TypeExpr::RefMut((acc_ty, acc_span).into()), acc_span),
                        RefType::Immutable => (TypeExpr::Ref((acc_ty, acc_span).into()), acc_span),
                    });
                (res.0, e.span())
            });

            let union_expr = ref_parser
                .separated_by(just(Token::ControlChar('|')))
                .at_least(1)
                .collect::<Vec<Spanned<TypeExpr>>>()
                .map_with(|elements, e| {
                    if elements.len() == 1 {
                        elements.into_iter().next().unwrap()
                    } else {
                        let mut elems = Vec::new();
                        let expr = (TypeExpr::Or(elements), e.span());
                        merge_or(&expr, &mut elems);
                        (TypeExpr::Or(elems), e.span())
                    }
                });

            union_expr
                .separated_by(just(Token::ControlChar('&')))
                .at_least(1)
                .collect::<Vec<Spanned<TypeExpr>>>()
                .map_with(|elements, e| {
                    if elements.len() == 1 {
                        elements.into_iter().next().unwrap()
                    } else {
                        let mut elems = Vec::new();
                        let expr = (TypeExpr::And(elements), e.span());
                        merge_and(&expr, &mut elems);
                        (TypeExpr::And(elems), e.span())
                    }
                })
        },
    )
}

pub fn merge_or(t: &Spanned<TypeExpr>, o: &mut Vec<Spanned<TypeExpr>>) {
    if let TypeExpr::Or(elems) = &t.0 {
        for elem in elems {
            merge_or(elem, o);
        }
    } else {
        o.push(t.clone());
    }
}

pub fn merge_and(t: &Spanned<TypeExpr>, o: &mut Vec<Spanned<TypeExpr>>) {
    if let TypeExpr::And(elems) = &t.0 {
        for elem in elems {
            merge_and(elem, o);
        }
    } else {
        o.push(t.clone());
    }
}

pub fn type_definition_parser<'src, I>()
-> impl Parser<'src, I, TypeDefinition, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    just(Token::Type)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then(generics_parser().or_not())
        .then_ignore(just(Token::ControlChar('=')))
        .then(type_expression_parser())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|((identifier, generics), type_expression)| TypeDefinition {
            name: identifier,
            type_expression,
            generics: generics.unwrap_or_default(),
        })
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TypeExpr::Ref(t) | TypeExpr::RefMut(t) => write!(f, "&{}", t.0),
            TypeExpr::Html => write!(f, "html"),
            TypeExpr::Tag(identifier) => write!(f, ".{identifier}"),
            TypeExpr::TypeOf(identifier) => write!(f, "typeof {identifier}"),
            TypeExpr::KeyOf(identifier) => write!(
                f,
                "keyof {}",
                identifier.as_ref().0.as_clean_user_faced_type_name()
            ),
            TypeExpr::Any => write!(f, "any"),
            TypeExpr::InlineGo => write!(f, "inline_go"),
            TypeExpr::Struct {
                name: s,
                type_params,
            } => {
                let s = s.replace("_____", "::");
                write!(
                    f,
                    "{s}{}",
                    if type_params.is_empty() {
                        ""
                    } else {
                        &format!(
                            "<{}>",
                            type_params
                                .iter()
                                .map(|x| x.0.as_clean_user_faced_type_name())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                )
            }
            TypeExpr::Go(s) => write!(f, "go {s}"),
            TypeExpr::NamedDuck { name, type_params } => write!(
                f,
                "Named Duck {name}{}",
                if type_params.is_empty() {
                    String::new()
                } else {
                    format!(
                        "<{}>",
                        type_params
                            .iter()
                            .map(|f| f.0.as_clean_user_faced_type_name())
                            .collect::<Vec<_>>()
                            .join(",")
                    )
                }
            ),
            TypeExpr::Duck(d) => write!(f, "{d}"), // Delegates to Duck's Display impl
            TypeExpr::Tuple(elements) => {
                write!(f, "(")?;
                elements.iter().enumerate().try_for_each(|(i, elem)| {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem.0)
                })?;
                if elements.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            TypeExpr::TypeName(is_global, name, generics) => {
                if *is_global {
                    write!(f, "::")?;
                }
                write!(f, "{name}")?;

                if !generics.is_empty() {
                    write!(f, "<")?;
                    generics.iter().enumerate().try_for_each(|(i, param)| {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param.0)
                    })?;
                    write!(f, ">")?;
                }

                Ok(())
            }
            TypeExpr::RawTypeName(is_global, path, generics) => {
                if *is_global {
                    write!(f, "::")?;
                }
                write!(f, "{}", path.join("::"))?;

                if !generics.is_empty() {
                    write!(f, "<")?;
                    generics.iter().enumerate().try_for_each(|(i, param)| {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param.0)
                    })?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            TypeExpr::String(s) => write!(
                f,
                "String{}",
                if let Some(str) = s {
                    format!(" {str}")
                } else {
                    String::from("")
                }
            ),
            TypeExpr::Int(i) => write!(
                f,
                "Int{}",
                if let Some(int) = i {
                    format!(" {int}")
                } else {
                    String::from("")
                }
            ),
            TypeExpr::Bool(b) => write!(
                f,
                "Bool{}",
                if let Some(boo) = b {
                    format!(" {boo}")
                } else {
                    String::from("")
                }
            ),
            TypeExpr::Char => write!(f, "Char"),
            TypeExpr::Float => write!(f, "Float"),
            TypeExpr::Or(variants) => variants.iter().enumerate().try_for_each(|(i, variant)| {
                if i > 0 {
                    write!(f, " | ")?;
                }
                write!(f, "{}", variant.0)
            }),
            TypeExpr::And(variants) => variants.iter().enumerate().try_for_each(|(i, variant)| {
                if i > 0 {
                    write!(f, " & ")?;
                }
                write!(f, "{}", variant.0)
            }),
            TypeExpr::Fun(params, return_type, _) => {
                write!(f, "fn(")?;
                params
                    .iter()
                    .enumerate()
                    .try_for_each(|(i, (name, type_expr))| {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        if let Some(n) = name {
                            write!(f, "{n}: ")?;
                        }
                        write!(f, "{}", type_expr.0)
                    })?;
                write!(f, ")")?;
                if let Some(rt) = return_type {
                    write!(f, " -> {}", rt.0)?;
                }
                Ok(())
            }
            TypeExpr::Array(inner) => write!(f, "{}[]", inner.0),
        }
    }
}

impl Display for Duck {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        self.fields.iter().enumerate().try_for_each(|(i, field)| {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "\"{}\": {}", field.name, field.type_expr.0)
        })?;
        write!(f, " }}")
    }
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{
        lexer::lex_parser,
        make_input,
        value_parser::{empty_range, type_expr_into_empty_range},
    };
    use chumsky::Parser;

    use super::*;

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
                return_type.map(|rt_box| Box::new(strip_spans(*rt_box))),
                is_mut,
            ),
            TypeExpr::Or(variants) => TypeExpr::Or(variants.into_iter().map(strip_spans).collect()),
            TypeExpr::And(variants) => {
                TypeExpr::And(variants.into_iter().map(strip_spans).collect())
            }
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
                Some(Box::new(TypeExpr::String(None).into_empty_span())),
                false,
            ),
        );

        assert_type_expression("true", TypeExpr::Bool(Some(true)));

        assert_type_expression("false", TypeExpr::Bool(Some(false)));

        assert_type_expression(
            "true | false",
            TypeExpr::Or(vec![
                TypeExpr::Bool(Some(true)).into_empty_span(),
                TypeExpr::Bool(Some(false)).into_empty_span(),
            ]),
        );

        assert_type_expression("1", TypeExpr::Int(Some(1)));

        assert_type_expression("555", TypeExpr::Int(Some(555)));

        assert_type_expression(
            "555 | 1000",
            TypeExpr::Or(vec![
                TypeExpr::Int(Some(555)).into_empty_span(),
                TypeExpr::Int(Some(1000)).into_empty_span(),
            ]),
        );

        assert_type_expression("\"str\"", TypeExpr::String(Some("str".to_string())));

        assert_type_expression(
            "\"other_str\"",
            TypeExpr::String(Some("other_str".to_string())),
        );

        assert_type_expression(
            "\"str\" | \"other_str\"",
            TypeExpr::Or(vec![
                TypeExpr::String(Some("str".to_string())).into_empty_span(),
                TypeExpr::String(Some("other_str".to_string())).into_empty_span(),
            ]),
        );

        assert_type_expression(
            "fn(x: Int) -> Bool",
            TypeExpr::Fun(
                vec![(
                    "x".to_string().into(),
                    TypeExpr::Int(None).into_empty_span(),
                )],
                Some(Box::new(TypeExpr::Bool(None).into_empty_span())),
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
                Some(Box::new(TypeExpr::Char.into_empty_span())),
                false,
            ),
        );

        assert_type_expression(
            "(String | Bool) | Int",
            TypeExpr::Or(vec![
                TypeExpr::String(None).into_empty_span(),
                TypeExpr::Bool(None).into_empty_span(),
                TypeExpr::Int(None).into_empty_span(),
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
                    TypeExpr::Int(None).into_empty_span(),
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
                    TypeExpr::Int(None).into_empty_span(),
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
                        vec![TypeExpr::Int(None).into_empty_span()],
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
                            TypeExpr::Int(None).into_empty_span(),
                            TypeExpr::String(None).into_empty_span(),
                        ],
                    )
                    .into_empty_span(),
                    TypeExpr::RawTypeName(false, vec!["Error".to_string()], vec![])
                        .into_empty_span(),
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
                        vec![TypeExpr::Int(None).into_empty_span()],
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
                TypeExpr::Int(None).into_empty_span(),
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
                    vec![TypeExpr::Int(None).into_empty_span()],
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
                        vec![TypeExpr::Int(None).into_empty_span()],
                    )
                    .into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::RawTypeName(
                        false,
                        vec!["Future".to_string()],
                        vec![TypeExpr::String(None).into_empty_span()],
                    )
                    .into_empty_span(),
                )),
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
                        TypeExpr::Int(None).into_empty_span(),
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
                            Field::new(
                                "data".to_string(),
                                TypeExpr::String(None).into_empty_span(),
                            ),
                            Field::new("id".to_string(), TypeExpr::Int(None).into_empty_span()),
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
                        Some(Box::new(TypeExpr::Int(None).into_empty_span())),
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
                vec![
                    TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into()).into_empty_span(),
                ],
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
                        TypeExpr::Int(None).into_empty_span(),
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
                                    TypeExpr::Int(None).into_empty_span(),
                                    TypeExpr::RawTypeName(false, vec!["E".to_string()], vec![])
                                        .into_empty_span(),
                                ],
                            )
                            .into_empty_span(),
                        )],
                        Some(Box::new(TypeExpr::Tuple(vec![]).into_empty_span())),
                        false,
                    )
                    .into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::RawTypeName(
                        false,
                        vec!["Subscription".to_string()],
                        vec![
                            TypeExpr::RawTypeName(false, vec!["T".to_string()], vec![])
                                .into_empty_span(),
                        ],
                    )
                    .into_empty_span(),
                )),
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
                    TypeExpr::Int(None).into_empty_span(),
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
                        TypeExpr::Int(None).into_empty_span(),
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
                    TypeExpr::Int(None).into_empty_span(),
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
                Some(Box::new(TypeExpr::Tuple(Vec::new()).into_empty_span())),
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
                            Field::new("age".to_string(), TypeExpr::Int(None).into_empty_span()),
                            Field::new(
                                "name".to_string(),
                                TypeExpr::String(None).into_empty_span(),
                            ),
                        ],
                    })
                    .into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::RawTypeName(true, vec!["MyResult".to_string()], vec![])
                        .into_empty_span(),
                )),
                false,
            ),
        );

        assert_type_expression(
            "fn() -> (Int, String)",
            TypeExpr::Fun(
                vec![],
                Some(Box::new(
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int(None).into_empty_span(),
                        TypeExpr::String(None).into_empty_span(),
                    ])
                    .into_empty_span(),
                )),
                false,
            ),
        );

        assert_type_expression(
            "Int[]",
            TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into()),
        );

        assert_type_expression(
            "Int[][]",
            TypeExpr::Array(
                TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into())
                    .into_empty_span()
                    .into(),
            ),
        );
        assert_type_expression(
            "Int[][][]",
            TypeExpr::Array(
                TypeExpr::Array(
                    TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into())
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
                TypeExpr::Tuple(vec![TypeExpr::Int(None).into_empty_span()])
                    .into_empty_span()
                    .into(),
            ),
        );

        assert_type_expression(
            "String[] | Int[][]",
            TypeExpr::Or(vec![
                TypeExpr::Array(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
                TypeExpr::Array(
                    TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into())
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
                    TypeExpr::Array(TypeExpr::String(None).into_empty_span().into())
                        .into_empty_span(),
                    TypeExpr::Array(TypeExpr::Int(None).into_empty_span().into()).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            ),
        );

        assert_type_expression(
            "fn(x: Int) -> go `fmt.Stringer`",
            TypeExpr::Fun(
                vec![(
                    "x".to_string().into(),
                    TypeExpr::Int(None).into_empty_span(),
                )],
                Some(Box::new(
                    TypeExpr::Go("fmt.Stringer".to_string()).into_empty_span(),
                )),
                false,
            ),
        );

        assert_type_expression(
            "fn() -> (Int, duck { val: Char })",
            TypeExpr::Fun(
                vec![],
                Some(Box::new(
                    TypeExpr::Tuple(vec![
                        TypeExpr::Int(None).into_empty_span(),
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
                false,
            ),
        );

        assert_type_expression("{}", TypeExpr::Any);
        assert_type_expression("duck {}", TypeExpr::Any);

        assert_type_expression("go `fmt`", TypeExpr::Go("fmt".to_string()));
        assert_type_expression(
            "go `sync.WaitGroup`",
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
                    Field::new("age".to_string(), TypeExpr::Int(None).into_empty_span()),
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
            TypeExpr::Tuple(vec![TypeExpr::Int(None).into_empty_span()]),
        );
        assert_type_expression(
            "(Int, String)",
            TypeExpr::Tuple(vec![
                TypeExpr::Int(None).into_empty_span(),
                TypeExpr::String(None).into_empty_span(),
            ]),
        );
        assert_type_expression(
            "(Int, String,)",
            TypeExpr::Tuple(vec![
                TypeExpr::Int(None).into_empty_span(),
                TypeExpr::String(None).into_empty_span(),
            ]),
        );
        assert_type_expression(
            "(Int, (Float, Bool))",
            TypeExpr::Tuple(vec![
                TypeExpr::Int(None).into_empty_span(),
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
                TypeExpr::Int(None).into_empty_span(),
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
        assert_type_expression("Int", TypeExpr::Int(None));
        assert_type_expression("Bool", TypeExpr::Bool(None));
        assert_type_expression("Char", TypeExpr::Char);
        assert_type_expression("Float", TypeExpr::Float);

        assert_type_expression(
            "Int | String",
            TypeExpr::Or(vec![
                TypeExpr::Int(None).into_empty_span(),
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
                TypeExpr::Int(None).into_empty_span(),
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

        assert_type_expression(
            "{ x: \"hallo\" } | { x: \"bye\" }",
            TypeExpr::Or(vec![
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String(Some("hallo".to_string())).into_empty_span(),
                    )],
                })
                .into_empty_span(),
                TypeExpr::Duck(Duck {
                    fields: vec![Field::new(
                        "x".to_string(),
                        TypeExpr::String(Some("bye".to_string())).into_empty_span(),
                    )],
                })
                .into_empty_span(),
            ]),
        );

        assert_type_expression(
            "&Int",
            TypeExpr::Ref(TypeExpr::Int(None).into_empty_span().into()),
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
                    TypeExpr::Int(None).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            ),
        );

        assert_type_expression(
            "&String | Int",
            TypeExpr::Or(vec![
                TypeExpr::Ref(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
                TypeExpr::Int(None).into_empty_span(),
            ]),
        );

        assert_type_expression(
            "&mut Int",
            TypeExpr::RefMut(TypeExpr::Int(None).into_empty_span().into()),
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
                    TypeExpr::Int(None).into_empty_span(),
                ])
                .into_empty_span()
                .into(),
            ),
        );

        assert_type_expression(
            "&mut String | Int",
            TypeExpr::Or(vec![
                TypeExpr::RefMut(TypeExpr::String(None).into_empty_span().into()).into_empty_span(),
                TypeExpr::Int(None).into_empty_span(),
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
                TypeExpr::Int(None).into_empty_span(),
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
            "type Tup = go `fmt`;",
            "type Tup = go `sync.WaitGroup`;",
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
            "go `sync.WaitGroup`",
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
}
