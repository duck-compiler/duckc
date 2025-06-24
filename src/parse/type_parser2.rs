use chumsky::input::BorrowInput;
use chumsky::Parser;
use chumsky::prelude::*;

use crate::parse::type_parser::*;

use super::lexer::Token;

pub fn type_expression_parser2<'src, I>() -> impl Parser<'src, I, TypeExpr, extra::Err<Rich<'src, Token>>> + Clone
where I: BorrowInput<'src, Token = Token, Span = SimpleSpan>
{
    recursive(|p| {
        let field = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(p.clone());

        let duck_fields = field
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<(String, TypeExpr)>>();

        let struct_fields = field
            .separated_by(just(Token::ControlChar(',')))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<(String, TypeExpr)>>();

        let go_type_identifier: impl Parser<'src, I, String, extra::Err<Rich<'src, Token>>> =
            select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .separated_by(just(Token::ControlChar('.')))
                .at_least(1)
                .at_most(2)
                .collect::<Vec<String>>()
                .map(|str| str.join("."));

        let go_type = just(Token::Go)
            .ignore_then(go_type_identifier)
            .map(|go_type_identifier| TypeExpr::Go(go_type_identifier));

        let r#struct = just(Token::Struct)
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(struct_fields)
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| TypeExpr::Struct(Struct { fields }));

        let duck = just(Token::Duck)
            .or_not()
            .ignore_then(just(Token::ControlChar('{')))
            .ignore_then(duck_fields.or_not())
            .then_ignore(just(Token::ControlChar('}')))
            .map(|fields| match fields {
                Some(mut fields) => {
                    fields.sort_by_key(|x| x.0.clone());
                    TypeExpr::Duck(Duck { fields })
                }
                None => TypeExpr::Any,
            });

        let tuple = p
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(TypeExpr::Tuple);

        let type_name =
            select_ref! { Token::Ident(identifier) => identifier.to_string() }.map(|identifier| {
                match identifier.as_str() {
                    "Int" => TypeExpr::Int,
                    "Float" => TypeExpr::Float,
                    "Bool" => TypeExpr::Bool,
                    "String" => TypeExpr::String,
                    "Char" => TypeExpr::Char,
                    _ => TypeExpr::TypeName(identifier),
                }
            });

        choice((
            go_type,
            type_name,
            r#struct,
            duck,
            tuple,
        ))
    })
}

pub fn type_definition_parser2<'src, I>() -> impl Parser<'src, I, TypeDefinition, extra::Err<Rich<'src, Token>>> + Clone
where I: BorrowInput<'src, Token = Token, Span = SimpleSpan>
{
    just(Token::Type)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('=')))
        .then(type_expression_parser2())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|(identifier, type_expression)| TypeDefinition {
            name: identifier,
            type_expression,
        })
}
