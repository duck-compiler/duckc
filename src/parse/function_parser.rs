use chumsky::{input::BorrowInput, prelude::*};

use crate::{
    parse::{
        SS, Spanned, failure_with_occurence,
        generics_parser::{Generic, generics_parser},
        value_parser::empty_range,
    },
    semantics::type_env::FunHeader,
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
    value_parser::{ValueExpr, block_expr_parser, value_expr_parser},
};

#[cfg(test)]
#[path = "function_parser_test.rs"]
mod tests;

pub type Param = (String, Spanned<TypeExpr>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefintion {
    pub name: String,
    pub return_type: Spanned<TypeExpr>,
    pub params: Vec<Param>,
    pub value_expr: Spanned<ValueExpr>,
    pub generics: Vec<Spanned<Generic>>,
    pub span: SS,
    pub comments: Vec<Spanned<String>>,
}

impl FunctionDefintion {
    pub fn to_header(&self) -> FunHeader {
        let return_type = self.return_type.clone();

        FunHeader {
            params: self.params.iter().map(|x| x.1.clone()).collect(),
            return_type,
        }
    }

    pub fn type_expr(&self) -> Spanned<TypeExpr> {
        // todo: retrieve correct span for function defintions typeexpr
        let return_type = self.return_type.clone();

        return (
            TypeExpr::Fun(
                self.params
                    .iter()
                    .map(|(name, type_expr)| (Some(name.to_owned()), type_expr.to_owned()))
                    .collect::<Vec<_>>(),
                return_type.into(),
                false,
            ),
            self.value_expr.1,
        );
    }
}

impl Default for FunctionDefintion {
    fn default() -> Self {
        FunctionDefintion {
            name: Default::default(),
            return_type: TypeExpr::Tuple(vec![]).into_empty_span(),
            params: Default::default(),
            value_expr: ValueExpr::Return(Some(ValueExpr::Block(vec![]).into_empty_span().into()))
                .into_empty_span(),
            generics: vec![],
            span: empty_range(),
            comments: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaFunctionExpr {
    pub is_mut: bool,
    pub params: Vec<(String, Option<Spanned<TypeExpr>>)>,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub value_expr: Spanned<ValueExpr>,
}

pub fn function_definition_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, FunctionDefintion, extra::Err<Rich<'src, Token, SS>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let doc_comments_parser = select_ref! { Token::DocComment(comment) => comment.to_string() }
        .map_with(|comment, ctx| (comment, ctx.span()))
        .repeated()
        .collect()
        .or_not();

    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>();

    let return_type_parser = just(Token::ThinArrow).ignore_then(type_expression_parser());

    doc_comments_parser
        .then_ignore(just(Token::Function))
        .then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then(generics_parser().or_not())
        .then_ignore(just(Token::ControlChar('(')))
        .then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then(block_expr_parser(
            make_input.clone(),
            value_expr_parser(make_input.clone()),
        ))
        .map_with(
            |(((((doc_comments, identifier), generics), params), return_type), mut value_expr),
             ctx| {
                value_expr = match value_expr {
                    x @ (ValueExpr::Block(_), _) => x,
                    _ => {
                        let msg = "Function must be a block expression";
                        failure_with_occurence(msg, value_expr.1, [(msg, value_expr.1)]);
                    }
                };

                FunctionDefintion {
                    name: identifier,
                    return_type: return_type.unwrap_or((TypeExpr::Tuple(vec![]), ctx.span())),
                    params,
                    value_expr: (
                        ValueExpr::Return(Some(Box::new(value_expr.clone()))),
                        value_expr.1,
                    ),
                    generics: generics.unwrap_or_default(),
                    span: ctx.span(),
                    comments: doc_comments.unwrap_or_else(Vec::new),
                }
            },
        )
}
