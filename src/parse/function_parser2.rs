use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{function_parser::{FunctionDefintion, Param}, lexer::Token, type_parser2::type_expression_parser2, val2::value_expr_parser2, value_parser::ValueExpr, Spanned};

pub fn function_definition_parser2<'src, I>()
-> impl Parser<'src, I, FunctionDefintion, extra::Err<Rich<'src, Token>>> + Clone
where I: BorrowInput<'src, Token = Token, Span = SimpleSpan>
{
    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser2())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>()
        .or_not();

    let return_type_parser = just(Token::ControlChar('-'))
        .ignore_then(just(Token::ControlChar('>')))
        .ignore_then(type_expression_parser2());

    just(Token::Function)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('(')))
        .then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then(value_expr_parser2())
        .map(|(((identifier, params), return_type), mut value_expr)| {
            value_expr = match value_expr {
                ValueExpr::Duck(x) if x.is_empty() => {
                    ValueExpr::Block(vec![ValueExpr::Tuple(vec![])])
                }
                x @ ValueExpr::Block(_) => x,
                _ => panic!("Function must be block"),
            };
            FunctionDefintion {
                name: identifier,
                return_type,
                params,
                value_expr,
            }
        })
}
