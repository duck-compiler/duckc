use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{
    SS, Spanned,
    function_parser::{FunctionDefintion, function_definition_parser},
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
};

#[cfg(test)]
#[path = "extensions_def_parser_test.rs"]
mod tests;

pub type Param = (String, Spanned<TypeExpr>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionsDef {
    pub target_type_expr: Spanned<TypeExpr>,
    pub function_definitions: Vec<Spanned<FunctionDefintion>>,
    pub doc_comments: Vec<Spanned<String>>,
    pub span: SS,
}

pub fn extensions_def_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, ExtensionsDef, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    doc_comments_parser
        .then_ignore(just(Token::Extend))
        .then(type_expression_parser())
        .then_ignore(just(Token::With))
        .then_ignore(just(Token::Impl))
        .then(
            function_definition_parser(make_input)
                .map_with(|fn_def, ctx| (fn_def, ctx.span()))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))),
        )
        .map_with(
            |((doc_comments, target_type_expr), function_definitions), ctx| ExtensionsDef {
                target_type_expr,
                function_definitions,
                span: ctx.span(),
                doc_comments: doc_comments.unwrap_or_else(Vec::new),
            },
        )
}
