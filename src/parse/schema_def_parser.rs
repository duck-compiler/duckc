use chumsky::{input::BorrowInput, prelude::*};

use crate::parse::{SS, Spanned};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
    value_parser::{ValueExpr, value_expr_parser},
};

#[cfg(test)]
#[path = "schema_def_parser_test.rs"]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct IfBranch {
    pub condition: Spanned<ValueExpr>,
    pub value_expr: Option<Spanned<ValueExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SchemaField {
    pub name: String,
    pub type_expr: Spanned<TypeExpr>,
    pub if_branch: Option<Spanned<IfBranch>>,
    pub else_branch_value_expr: Option<Spanned<ValueExpr>>,
    pub span: SS,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct SchemaDefinition {
    pub name: String,
    pub fields: Vec<SchemaField>,
    pub comments: Vec<Spanned<String>>,
    pub span: SS,
    pub out_type: Option<Spanned<TypeExpr>>,
    pub schema_fn_type: Option<Spanned<TypeExpr>>,
}

pub fn schema_definition_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, SchemaDefinition, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    let if_branch_parser = just(Token::If)
        .ignore_then(value_expr_parser(make_input.clone()))
        .then(value_expr_parser(make_input.clone()).or_not())
        .map_with(|(condition, maybe_value_expr), ctx| {
            (
                IfBranch {
                    condition,
                    value_expr: maybe_value_expr,
                },
                ctx.span(),
            )
        });

    let else_branch_parser = just(Token::Else)
        .ignore_then(value_expr_parser(make_input))
        .map(|value_expr| value_expr);

    let field_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .then(if_branch_parser.or_not())
        .then(else_branch_parser.or_not())
        .map_with(
            |(((identifier, type_expr), if_branch), else_branch), ctx| SchemaField {
                name: identifier,
                type_expr,
                if_branch,
                else_branch_value_expr: else_branch,
                span: ctx.span(),
            },
        );

    let fields_parser = field_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .at_least(1)
        .collect::<Vec<SchemaField>>();

    doc_comments_parser
        .then_ignore(just(Token::Schema))
        .then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('=')))
        .then_ignore(just(Token::ControlChar('{')))
        .then(fields_parser)
        .then_ignore(just(Token::ControlChar('}')))
        .map_with(|((doc_comments, identifier), mut fields), ctx| {
            // todo: do a check if all fields if's value_exprs have a block for the value expr
            // value_expr = match value_expr {
            //     (ValueExpr::Duck(x), loc) if x.is_empty() => (ValueExpr::Block(vec![]), loc),
            //     x @ (ValueExpr::Block(_), _) => x,
            //     _ => panic!("Function must be block"),

            fields.sort_by_key(|x| x.name.clone());
            SchemaDefinition {
                name: identifier,
                fields,
                span: ctx.span(),
                comments: doc_comments.unwrap_or_else(Vec::new),
                out_type: None,
                schema_fn_type: None,
            }
        })
}
