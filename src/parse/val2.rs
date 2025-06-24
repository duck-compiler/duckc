use crate::parse::{
    assignment_and_declaration_parser::{Assignment, Declaration},
    function_parser::{LambdaFunctionExpr, Param},
    lexer::Token,
    type_parser2::type_expression_parser2,
    value_parser::ValueExpr,
};
use chumsky::{input::BorrowInput, prelude::*};

pub fn value_expr_parser2<'src, I>()
-> impl Parser<'src, I, ValueExpr, extra::Err<Rich<'src, Token>>> + Clone
where
    I: BorrowInput<'src, Token = Token, Span = SimpleSpan>,
{
    recursive(|value_expr_parser| {
        let lambda_parser = {
            let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(type_expression_parser2())
                .map(|(identifier, type_expr)| (identifier, type_expr) as Param).boxed();

            let params_parser = param_parser
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<Param>>()
                .or_not().boxed();

            let return_type_parser = just(Token::ControlChar('-'))
                .ignore_then(just(Token::ControlChar('>')))
                .ignore_then(type_expression_parser2()).boxed();

            just(Token::ControlChar('('))
                .ignore_then(params_parser)
                .then_ignore(just(Token::ControlChar(')')))
                .then(return_type_parser.or_not())
                .then_ignore(just(Token::ControlChar('=')))
                .then_ignore(just(Token::ControlChar('>')))
                .then(value_expr_parser.clone())
                .map(|((params, return_type), value_expr)| {
                    ValueExpr::Lambda(
                        LambdaFunctionExpr {
                            params: params.unwrap_or_default(),
                            return_type,
                            value_expr,
                        }
                        .into(),
                    )
                }).boxed()
        };

        let params = value_expr_parser
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))).boxed();

        let tuple = lambda_parser.clone().or((just(Token::ControlChar('('))
            .ignore_then(just(Token::ControlChar(')')))
            .to(ValueExpr::Tuple(vec![])))
        .or(value_expr_parser
            .clone()
            .separated_by(just(Token::ControlChar(',')))
            .at_least(1)
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
            .map(|x| ValueExpr::Tuple(x)))).boxed();

        let initializer = just(Token::ControlChar('='))
            .ignore_then(value_expr_parser.clone())
            .or_not().boxed();

        let declaration = just(Token::Let)
            .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
            .then_ignore(just(Token::ControlChar(':')))
            .then(type_expression_parser2())
            .then(initializer)
            .map(|((identifier, type_expr), initializer)| {
                ValueExpr::VarDecl(
                    Declaration {
                        name: identifier,
                        type_expr,
                        initializer,
                    }
                    .into(),
                )
            }).boxed();

        let struct_expression = just(Token::ControlChar('.')).ignore_then(
            select_ref! { Token::Ident(ident) => ident.to_owned() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(value_expr_parser.clone())
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                .map(ValueExpr::Struct),
        ).boxed();

        let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
            .then_ignore(just(Token::ControlChar(':')))
            .then(value_expr_parser.clone())
            .separated_by(just(Token::ControlChar(',')))
            .allow_trailing()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut x| {
                x.sort_by_key(|(name, _)| name.clone());
                ValueExpr::Duck(x)
            }).boxed();

        let block_expression = value_expr_parser
            .clone()
            .then(just(Token::ControlChar(';')).or_not())
            .repeated()
            .collect::<Vec<_>>()
            .map_err(|error| {
                dbg!(error);
                todo!()
            })
            .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
            .map(|mut exprs| {
                if exprs.len() >= 2 {
                    for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                        if expr.needs_semicolon() && has_semi.is_none() {
                            panic!("needs_semi")
                        }
                    }
                }

                if exprs.is_empty() || exprs.last().unwrap().1.is_some() {
                    exprs.push((empty_tuple(), None));
                }

                ValueExpr::Block(exprs.into_iter().map(|(expr, _)| expr).collect())
            }).boxed();

        let if_condition = value_expr_parser
            .clone()
            .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))).boxed();
        let if_body = block_expression.clone();
        let if_with_condition_and_body = just(Token::If)
            .ignore_then(if_condition.clone())
            .then(if_body.clone()).boxed();

        let while_condition = if_condition.clone();
        let while_body = block_expression.clone();
        let while_with_condition_and_body = just(Token::While)
            .ignore_then(while_condition.clone())
            .then(while_body.clone()).boxed();

        let field_access = any_ref()
            .filter(|t| !matches!(t, Token::ControlChar('.')))
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(
                (just(Token::ControlChar('.')).ignore_then(
                    select_ref! { Token::Ident(field_name) => field_name.to_owned() }
                        .or(select_ref! { Token::IntLiteral(i) => i.to_string() }),
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
            )
            .map({
                let e = value_expr_parser.clone();
                move |(base_expr, field_accesses)| {
                    let base_expr = todo!();
                    let base = e.parse(base_expr).unwrap();
                    field_accesses
                        .into_iter()
                        .fold(base, |acc, x| ValueExpr::FieldAccess {
                            target_obj: acc.into(),
                            field_name: x,
                        })
                }
            }).boxed();

        let int = select_ref! { Token::IntLiteral(i) => *i }.map(ValueExpr::Int).boxed();
        let bool_val = select_ref! { Token::BoolLiteral(b) => *b }.map(ValueExpr::Bool).boxed();
        let string_val =
            select_ref! { Token::StringLiteral(s) => s.to_owned() }.map(ValueExpr::String);
        let var_expr = select_ref! { Token::Ident(ident) => ident.to_owned() }
            .map(|ident| ValueExpr::Variable(ident, None)).boxed();
        let if_expr = if_with_condition_and_body
            .clone()
            .then(
                just(Token::Else)
                    .ignore_then(if_with_condition_and_body.clone())
                    .repeated()
                    .collect::<Vec<(ValueExpr, ValueExpr)>>(),
            )
            .then_ignore(just(Token::Else))
            .then(if_body.clone())
            .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                condition: Box::new(condition),
                then: Box::new(then),
                r#else: else_ifs
                    .into_iter()
                    .rfold(Box::new(r#else), |acc, (cond, then)| {
                        Box::new(ValueExpr::If {
                            condition: Box::new(cond),
                            then: Box::new(then),
                            r#else: acc,
                        })
                    }),
            }).boxed();
        let char_expr = select_ref! { Token::CharLiteral(c) => *c }.map(ValueExpr::Char).boxed();
        let float_expr = select_ref! { Token::FloatLiteral(num) => *num }.map(ValueExpr::Float).boxed();

        let atom = just(Token::ControlChar('!'))
            .repeated()
            .collect::<Vec<_>>()
            .then(
                field_access.clone().or(value_expr_parser
                    .clone()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .or(choice((
                        field_access.clone(),
                        int,
                        bool_val,
                        string_val,
                        var_expr,
                        if_expr,
                        char_expr,
                        float_expr,
                        tuple,
                        duck_expression,
                        struct_expression,
                        block_expression,
                        just(Token::Break).to(ValueExpr::Break),
                        just(Token::Continue).to(ValueExpr::Continue),
                        while_with_condition_and_body.clone().map(|(cond, body)| {
                            ValueExpr::While {
                                condition: Box::new(cond),
                                body: Box::new(body),
                            }
                        }),
                    )))),
            )
            .then(params.clone().or_not())
            .map(|((neg, target), params)| {
                let res = if let Some(params) = params {
                    ValueExpr::FunctionCall {
                        target: target.into(),
                        params,
                    }
                } else {
                    target
                };

                neg.into_iter()
                    .fold(res, |acc, _| ValueExpr::BoolNegate(acc.into()))
            }).boxed();

        let assignment = select_ref! { Token::Ident(identifier) => identifier.to_string() }
            .then_ignore(just(Token::ControlChar('=')))
            .then(value_expr_parser.clone())
            .map(|(identifier, value_expr)| {
                ValueExpr::VarAssign(
                    Assignment {
                        name: identifier,
                        value_expr,
                    }
                    .into(),
                )
            }).boxed();

        //

        let prod = atom
            .clone()
            .then(
                just(Token::ControlChar('*'))
                    .ignore_then(atom.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(init, additional)| {
                additional
                    .into_iter()
                    .fold(init, |acc, x| ValueExpr::Mul(acc.into(), x.into()))
            }).boxed();

        let add = prod
            .clone()
            .then(
                just(Token::ControlChar('+'))
                    .ignore_then(prod.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(init, additional)| {
                additional
                    .into_iter()
                    .fold(init, |acc, x| ValueExpr::Add(acc.into(), x.into()))
            }).boxed();

        let equals = add
            .clone()
            .then_ignore(just(Token::Equals))
            .then(add.clone())
            .map(|(x, y)| ValueExpr::Equals(x.into(), y.into())).boxed();

        let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }.map(ValueExpr::InlineGo).boxed();

        choice((
            inline_go,
            assignment,
            equals,
            add,
            declaration,
            just(Token::Return)
                .ignore_then(value_expr_parser.clone().or_not())
                .map(|x: Option<ValueExpr>| ValueExpr::Return(x.map(Box::new))),
            atom,
        )).boxed()
    })
}

fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(vec![])
}
