use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{
    SS, Spanned, failure_with_occurence,
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
};

#[cfg(test)]
#[path = "generics_parser_test.rs"]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct Generic {
    pub name: String,
    pub constraint: Option<Spanned<TypeExpr>>,
}

pub fn generics_parser<'src, I>()
-> impl Parser<'src, I, Vec<Spanned<Generic>>, extra::Err<Rich<'src, Token, SS>>> + Clone + 'src
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
{
    // '<' <identifier> '>'
    just(Token::ControlChar('<'))
        .ignore_then(
            select_ref! { Token::Ident(identifier) => identifier.clone() }
                .then(
                    just(Token::ControlChar(':'))
                        .ignore_then(
                            type_expression_parser()
                                .map(|type_expr| {
                                    let span = type_expr.1;
                                    match &type_expr.0 {
                                        TypeExpr::Duck(_) => type_expr,
                                        other if false => {
                                            failure_with_occurence(
                                                "Invalid Syntax",
                                                span,
                                                vec![
                                                    (
                                                        format!(
                                                            "Type constraints are defined using ducks. You've passed a {other}"
                                                        ),
                                                        span,
                                                    ),
                                                ],
                                            );
                                        }
                                        _ => type_expr,
                                    }
                                })
                        )
                        .or_not()
                )
                .map(|(identifier, constraint)| Generic {
                    name: identifier.clone(),
                    constraint
                })
                .map_with(|generic, ctx| (generic, ctx.span()))
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<Spanned<Generic>>>(),
        )
        .then_ignore(just(Token::ControlChar('>')))
}
