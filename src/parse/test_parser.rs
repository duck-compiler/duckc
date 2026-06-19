use chumsky::Parser;
use chumsky::input::BorrowInput;
use chumsky::prelude::*;

use crate::parse::value_parser::value_expr_parser;
use crate::parse::{SS, Spanned, failure_with_occurence, value_parser::ValueExpr};

use super::lexer::Token;

#[cfg(test)]
#[path = "test_parser_test.rs"]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct TestCase {
    pub name: String,
    pub body: Spanned<ValueExpr>,
}

pub fn test_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<TestCase>, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    just(Token::Test)
        .ignore_then(select_ref! { Token::StringLiteral(str) => str.clone() })
        .then(value_expr_parser(make_input))
        .map_with(|(name, mut body), ctx| {
            body = match body {
                x @ (ValueExpr::Block(_), _) => x,
                _ => {
                    let msg = "Test body needs to be a block";
                    failure_with_occurence(msg, body.1, [(msg, body.1)]);
                }
            };

            (TestCase { name, body }, ctx.span())
        })
}
