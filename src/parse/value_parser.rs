use crate::parse::{
    Context, SS, Spanned,
    function_parser::{LambdaFunctionExpr, Param},
    lexer::{FmtStringContents, HtmlStringContents},
    source_file_parser::SourceFile,
    type_parser::{type_expression_parser, type_expression_parser_without_array},
};

use super::{lexer::Token, type_parser::TypeExpr};
use chumsky::{input::BorrowInput, prelude::*};

pub type TypeParam = TypeExpr;

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub type_case: Spanned<TypeExpr>,
    pub identifier_binding: Option<String>,
    pub condition: Option<Spanned<ValueExpr>>,
    pub value_expr: Spanned<ValueExpr>,
    pub span: SS,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValFmtStringContents {
    String(String),
    Expr(Spanned<ValueExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValHtmlStringContents {
    String(String),
    Expr(Spanned<ValueExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DuckxContents {
    HtmlString(Vec<ValHtmlStringContents>),
    Expr(Spanned<ValueExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub type_expr: Option<Spanned<TypeExpr>>,
    pub initializer: Spanned<ValueExpr>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Spanned<ValueExpr>,
    pub value_expr: Spanned<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    FunctionCall {
        target: Box<Spanned<ValueExpr>>,
        params: Vec<Spanned<ValueExpr>>,
        type_params: Option<Vec<Spanned<TypeParam>>>,
    },
    Int(i64),
    String(String, bool),
    Bool(bool),
    Float(f64),
    Char(char),
    RawVariable(bool, Vec<String>),
    Variable(bool, String, Option<TypeExpr>),
    If {
        condition: Box<Spanned<ValueExpr>>,
        then: Box<Spanned<ValueExpr>>,
        r#else: Option<Box<Spanned<ValueExpr>>>,
    },
    While {
        condition: Box<Spanned<ValueExpr>>,
        body: Box<Spanned<ValueExpr>>,
    },
    Tuple(Vec<Spanned<ValueExpr>>),
    Block(Vec<Spanned<ValueExpr>>),
    Break,
    Continue,
    Duck(Vec<(String, Spanned<ValueExpr>)>),
    HtmlString(Vec<ValHtmlStringContents>),
    Tag(String),
    Struct {
        name: String,
        fields: Vec<(String, Spanned<ValueExpr>)>,
        type_params: Option<Vec<Spanned<TypeParam>>>,
    },
    FieldAccess {
        target_obj: Box<Spanned<ValueExpr>>,
        field_name: String,
    },
    Array(Option<Spanned<TypeExpr>>, Vec<Spanned<ValueExpr>>),
    Return(Option<Box<Spanned<ValueExpr>>>),
    VarAssign(Box<Spanned<Assignment>>),
    VarDecl(Box<Spanned<Declaration>>),
    Add(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Sub(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Mul(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Div(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Mod(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    BoolNegate(Box<Spanned<ValueExpr>>),
    Equals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    NotEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    LessThan(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    LessThanOrEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    GreaterThan(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    GreaterThanOrEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    And(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Or(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    InlineGo(String),
    Lambda(Box<LambdaFunctionExpr>),
    ArrayAccess(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Match {
        value_expr: Box<Spanned<ValueExpr>>,
        arms: Vec<MatchArm>,
        else_arm: Option<Box<MatchArm>>,
        span: SS,
    },
    FormattedString(Vec<ValFmtStringContents>),
    Ref(Box<Spanned<ValueExpr>>),
    RefMut(Box<Spanned<ValueExpr>>),
    Deref(Box<Spanned<ValueExpr>>),
}

pub trait IntoBlock {
    fn into_block(self) -> Spanned<ValueExpr>;
}

impl IntoBlock for Spanned<ValueExpr> {
    fn into_block(self) -> Spanned<ValueExpr> {
        let cl = self.1;
        (ValueExpr::Block(vec![self]), cl)
    }
}

impl ValueExpr {
    pub fn into_empty_span(self) -> Spanned<ValueExpr> {
        (self, empty_range())
    }

    pub fn into_empty_span_and_block(self) -> Spanned<ValueExpr> {
        self.into_empty_span().into_block()
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            ValueExpr::Deref(..) | ValueExpr::Ref(..) | ValueExpr::RefMut(..) => true,
            ValueExpr::HtmlString(..) => true,
            ValueExpr::If {
                condition: _,
                then: _,
                r#else: _,
            } => false,
            ValueExpr::While {
                condition: _,
                body: _,
            } => false,
            ValueExpr::Block(_) => false,
            ValueExpr::InlineGo(_) => false,
            ValueExpr::Match { .. } => false,
            ValueExpr::Add(..)
            | ValueExpr::Mul(..)
            | ValueExpr::Tag(..)
            | ValueExpr::Duck(..)
            | ValueExpr::Int(..)
            | ValueExpr::String(..)
            | ValueExpr::Float(..)
            | ValueExpr::Bool(..)
            | ValueExpr::Char(..)
            | ValueExpr::FieldAccess { .. }
            | ValueExpr::Array(..)
            | ValueExpr::ArrayAccess(..)
            | ValueExpr::Variable(..)
            | ValueExpr::RawVariable(..)
            | ValueExpr::Tuple(..)
            | ValueExpr::Break
            | ValueExpr::Continue
            | ValueExpr::Return(..)
            | ValueExpr::Struct { .. }
            | ValueExpr::VarDecl(..)
            | ValueExpr::VarAssign(..)
            | ValueExpr::BoolNegate(..)
            | ValueExpr::Equals(..)
            | ValueExpr::NotEquals(..)
            | ValueExpr::LessThan(..)
            | ValueExpr::LessThanOrEquals(..)
            | ValueExpr::GreaterThan(..)
            | ValueExpr::GreaterThanOrEquals(..)
            | ValueExpr::And(..)
            | ValueExpr::Or(..)
            | ValueExpr::Lambda(..)
            | ValueExpr::FormattedString(..)
            | ValueExpr::Sub(..)
            | ValueExpr::Div(..)
            | ValueExpr::Mod(..)
            | ValueExpr::FunctionCall { .. } => true,
        }
    }
}

pub fn value_expr_parser<'src, I, M>(
    make_input: M,
) -> impl Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token, SS>>> + Clone + 'src
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    let make_input = Box::leak(Box::new(make_input));
    recursive(
        |value_expr_parser: Recursive<
            dyn Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token, SS>>> + 'src,
        >| {
            let scope_res_ident = just(Token::ScopeRes)
                .or_not()
                .then(
                    select_ref! { Token::Ident(ident) => ident.to_string() }
                        .separated_by(just(Token::ScopeRes))
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .map(|(is_global, path)| ValueExpr::RawVariable(is_global.is_some(), path))
                .map_with(|x, e| (x, e.span()));

            let lambda_parser = {
                let param_parser =
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .then_ignore(just(Token::ControlChar(':')))
                        .then(type_expression_parser())
                        .map(|(identifier, type_expr)| (identifier, type_expr) as Param)
                        .boxed();

                let params_parser = param_parser
                    .separated_by(just(Token::ControlChar(',')))
                    .allow_trailing()
                    .collect::<Vec<Param>>()
                    .or_not()
                    .boxed();

                let return_type_parser = just(Token::ThinArrow)
                    .ignore_then(type_expression_parser())
                    .boxed();

                just(Token::Function)
                    .ignore_then(just(Token::ControlChar('(')))
                    .ignore_then(params_parser)
                    .then_ignore(just(Token::ControlChar(')')))
                    .then(return_type_parser.or_not())
                    .then(value_expr_parser.clone())
                    .map(|((params, return_type), mut value_expr)| {
                        value_expr = match value_expr {
                            (ValueExpr::Duck(x), loc) if x.is_empty() => {
                                (ValueExpr::Block(vec![]), loc)
                            }
                            _ => value_expr,
                        };
                        ValueExpr::Lambda(
                            LambdaFunctionExpr {
                                params: params.unwrap_or_default(),
                                return_type,
                                value_expr,
                            }
                            .into(),
                        )
                    })
                    .boxed()
            };

            let params = value_expr_parser
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .boxed();

            let match_arm_condition = just(Token::If)
                .ignore_then(value_expr_parser.clone())
                .or_not();

            let match_arm_identifier_binding = just(Token::ControlChar('@'))
                .ignore_then(select_ref! { Token::Ident(ident) => ident.to_string() })
                .then(match_arm_condition)
                .or_not();

            let match_arm = type_expression_parser()
                .then(match_arm_identifier_binding.clone())
                .then_ignore(just(Token::ThickArrow))
                .then(value_expr_parser.clone())
                .map_with(|((type_expr, identifier), value_expr), ctx| MatchArm {
                    type_case: type_expr,
                    identifier_binding: identifier.clone().map(|x| x.0),
                    condition: identifier.map(|x| x.1).unwrap_or_else(|| None),
                    value_expr,
                    span: ctx.span(),
                });

            let else_arm = just(Token::Else)
                .then(match_arm_identifier_binding)
                .then_ignore(just(Token::ThickArrow))
                .then(value_expr_parser.clone())
                .then_ignore(just(Token::ControlChar(',')).or_not())
                // todo: add span of else
                .map_with(|((_, identifier), value_expr), ctx| MatchArm {
                    // todo: check if typeexpr::any is correct for the else arm in pattern matching
                    type_case: (TypeExpr::Any, value_expr.1),
                    identifier_binding: identifier.clone().map(|x| x.0),
                    condition: identifier.map(|x| x.1).unwrap_or_else(|| None),
                    value_expr,
                    span: ctx.span(),
                });

            let r#match = just(Token::Match)
                .ignore_then(value_expr_parser.clone())
                .then(
                    match_arm
                        .separated_by(just(Token::ControlChar(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .then(else_arm.map(Box::new).or_not())
                        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))),
                )
                .map_with(|(value_expr, (arms, else_arm)), ctx| ValueExpr::Match {
                    value_expr: Box::new(value_expr),
                    arms,
                    else_arm,
                    span: ctx.span(),
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let tuple = lambda_parser
                .clone()
                .or((just(Token::ControlChar('('))
                    .ignore_then(just(Token::ControlChar(')')))
                    .to(ValueExpr::Tuple(vec![])))
                .or(value_expr_parser
                    .clone()
                    .separated_by(just(Token::ControlChar(',')))
                    .at_least(1)
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .map(ValueExpr::Tuple)))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let initializer = just(Token::ControlChar('='))
                .ignore_then(value_expr_parser.clone())
                .boxed();

            let declare_type = just(Token::ControlChar(':'))
                .ignore_then(type_expression_parser())
                .or_not()
                .boxed();

            let declaration = just(Token::Let)
                .or(just(Token::Const))
                .then(
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .map_with(|x, e| (x, e.span())),
                )
                .then(declare_type)
                .then(initializer)
                .map(|(((let_or_const, (ident, _)), type_expr), initializer)| {
                    ValueExpr::VarDecl(
                        (
                            Declaration {
                                name: ident,
                                type_expr,
                                initializer: initializer.clone(),
                                is_const: matches!(let_or_const, Token::Const),
                            },
                            initializer.1,
                        )
                            .into(),
                    )
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let struct_type_params_parser = just(Token::ControlChar('<'))
                .ignore_then(
                    type_expression_parser()
                        .separated_by(just(Token::ControlChar(',')))
                        .allow_trailing()
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::ControlChar('>')));

            let struct_expression = select_ref! { Token::Ident(identifier) => identifier.clone() }
                .then(struct_type_params_parser.or_not())
                .then(
                    select_ref! { Token::Ident(ident) => ident.to_owned() }
                        .then_ignore(just(Token::ControlChar(':')))
                        .then(value_expr_parser.clone())
                        .separated_by(just(Token::ControlChar(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}'))),
                )
                .map(|((identifier, generics), values)| ValueExpr::Struct {
                    name: identifier,
                    fields: values,
                    type_params: generics,
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

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
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let block_expression = value_expr_parser
                .clone()
                .then(just(Token::ControlChar(';')).or_not())
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                .map_with(|mut exprs, e| {
                    if exprs.len() >= 2 {
                        for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                            if expr.0.needs_semicolon() && has_semi.is_none() {
                                panic!("needs_semi")
                            }
                        }
                    }

                    if !exprs.is_empty() && exprs.last().unwrap().1.is_some() {
                        exprs.push((empty_tuple().into_empty_span(), None));
                    }

                    (
                        ValueExpr::Block(exprs.into_iter().map(|(expr, _)| expr).collect()),
                        e.span(),
                    )
                })
                .boxed();

            let if_condition = value_expr_parser
                .clone()
                .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                .boxed();
            let if_body = block_expression.clone();
            let if_with_condition_and_body = just(Token::If)
                .ignore_then(if_condition.clone())
                .then(if_body.clone())
                .boxed();

            let while_condition = if_condition.clone();
            let while_body = block_expression.clone();
            let while_with_condition_and_body = just(Token::While)
                .ignore_then(while_condition.clone())
                .then(while_body.clone())
                .boxed();

            let int = select_ref! { Token::ConstInt(i) => *i }
                .map(ValueExpr::Int)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let bool_val = select_ref! { Token::ConstBool(b) => *b }
                .map(ValueExpr::Bool)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let string_val = select_ref! { Token::ConstString(s) => s.to_owned() }
                .map(|s| ValueExpr::String(s, true))
                .map_with(|x, e| (x, e.span()));
            let if_expr = if_with_condition_and_body
                .clone()
                .then(
                    just(Token::Else)
                        .ignore_then(if_with_condition_and_body.clone())
                        .repeated()
                        .collect::<Vec<(Spanned<ValueExpr>, Spanned<ValueExpr>)>>(),
                )
                .then(just(Token::Else).ignore_then(if_body.clone()).or_not())
                .map(|(((condition, then), else_ifs), r#else)| ValueExpr::If {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    r#else: else_ifs.into_iter().rfold(
                        r#else.map(Box::new),
                        |acc, (cond, then)| {
                            Some(Box::new(
                                ValueExpr::If {
                                    condition: Box::new(cond),
                                    then: Box::new(then),
                                    r#else: Some(acc.unwrap()),
                                }
                                .into_empty_span(),
                            ))
                        },
                    ),
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let char_expr = select_ref! { Token::CharLiteral(c) => *c }
                .map(ValueExpr::Char)
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let float_expr = select_ref! { Token::ConstInt(num) => *num }
                .then_ignore(just(Token::ControlChar('.')))
                .then(select_ref! { Token::ConstInt(num) => *num })
                .map(|(pre, frac)| {
                    ValueExpr::Float(format!("{pre}.{frac}").parse::<f64>().unwrap())
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let tag_identifier = choice((
                select_ref! { Token::Ident(ident) => ident.to_string() },
                just(Token::ControlChar('.')).map(|_| "DOT".to_string()),
            ))
            .boxed();

            let tag_expr = just(Token::ControlChar('.'))
                .ignore_then(tag_identifier)
                .map(|identifier| ValueExpr::Tag(identifier.clone()))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            #[derive(Debug, PartialEq, Clone)]
            enum AtomPostParseUnit {
                FuncCall(Vec<Spanned<ValueExpr>>, Option<Vec<Spanned<TypeParam>>>),
                ArrayAccess(Spanned<ValueExpr>),
                FieldAccess(String),
            }

            let fmt_string =
                select_ref! { Token::FormatStringLiteral(elements) => elements.clone() }
                    .map({
                        let value_expr_parser = value_expr_parser.clone();
                        let make_input = make_input.clone();
                        move |contents| {
                            let contents = contents.leak();
                            let mut res = Vec::new();

                            for c in contents {
                                match c {
                                    FmtStringContents::String(s) => {
                                        res.push(ValFmtStringContents::String(s.to_owned()))
                                    }
                                    FmtStringContents::Tokens(s) => {
                                        let expr = value_expr_parser
                                            .parse(make_input(empty_range(), s.as_slice()))
                                            .into_result()
                                            .expect("invalid code");
                                        res.push(ValFmtStringContents::Expr(expr));
                                    }
                                }
                            }
                            ValueExpr::FormattedString(res)
                        }
                    })
                    .map_with(|x, e| (x, e.span()));

            let html_string = select_ref! {
                Token::HtmlString(s) => s.clone()
            }
            .map({
                let value_expr_parser = value_expr_parser.clone();
                let make_input = make_input.clone();
                move |contents| {
                    let mut out_contents = Vec::new();
                    for c in contents {
                        match c {
                            HtmlStringContents::String(s) => {
                                out_contents.push(ValHtmlStringContents::String(s))
                            }
                            HtmlStringContents::Tokens(t) => {
                                // t.insert(0, (Token::ControlChar('{'), empty_range()));
                                // t.push((Token::ControlChar('}'), empty_range()));
                                let cl = t.clone();
                                let expr = value_expr_parser
                                    .parse(make_input(empty_range(), t.leak()))
                                    .into_result()
                                    .unwrap_or_else(|_| panic!("invalid code {cl:?}"));
                                out_contents.push(ValHtmlStringContents::Expr(expr));
                            }
                        }
                    }
                    ValueExpr::HtmlString(out_contents)
                }
            })
            .map_with(|x, e| (x, e.span()));

            let duckx = select_ref! {
                Token::InlineDuckx(contents) => contents.clone()
            }
            .map({
                let value_expr_parser = value_expr_parser.clone();
                let make_input = make_input.clone();
                move |x| {
                    // let mut res = Vec::new();
                    // x.insert(0, (Token::ControlChar('{'), empty_range()));
                    // x.push((Token::ControlChar('}'), empty_range()));

                    let cl = x.clone();

                    value_expr_parser
                        .parse(make_input(empty_range(), x.leak()))
                        .into_result()
                        .unwrap_or_else(|_| panic!("invavlid code {cl:?}"))
                }
            });
            // .map_with(|x, e| (x, e.span()));

            let array = value_expr_parser
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .at_least(1)
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('[')), just(Token::ControlChar(']')))
                .map(|exprs| ValueExpr::Array(None, exprs))
                .map_with(|x, e| (x, e.span()));

            let array_with_type = (just(Token::ControlChar('.'))
                .ignore_then(type_expression_parser_without_array())
                .or_not())
            .then(
                (value_expr_parser
                    .clone()
                    .separated_by(just(Token::ControlChar(',')))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::ControlChar('[')), just(Token::ControlChar(']'))))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
            )
            .map(|(declared_content_type, exprs)| {
                if declared_content_type.is_none() && exprs.last().unwrap().is_empty() {
                    panic!("error: empty array must provide type");
                }

                if !exprs.is_empty() && exprs[..exprs.len() - 1].iter().any(|e| !e.is_empty()) {
                    panic!("only last braces my include values");
                }

                let mut content_type = declared_content_type.clone();

                if declared_content_type.is_some() {
                    for _ in 0..exprs.len() - 1 {
                        content_type =
                            Some(TypeExpr::Array(Box::new(content_type.unwrap())).into_empty_span())
                    }
                }

                ValueExpr::Array(content_type, exprs.last().unwrap().clone())
            })
            .map_with(|x, e| (x, e.span()));

            #[derive(Debug, Clone)]
            enum AtomPreParseUnit {
                Ref,
                RefMut,
                BoolNegate,
                Deref,
            }

            let atom = choice((
                just(Token::ControlChar('&')).map(|_| AtomPreParseUnit::Ref),
                just(Token::RefMut).map(|_| AtomPreParseUnit::RefMut),
                just(Token::ControlChar('!')).map(|_| AtomPreParseUnit::BoolNegate),
                just(Token::ControlChar('*')).map(|_| AtomPreParseUnit::Deref),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(
                value_expr_parser
                    .clone()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .or(choice((
                        choice((array.clone(), array_with_type.clone())),
                        float_expr,
                        int,
                        fmt_string,
                        html_string,
                        duckx,
                        tag_expr,
                        bool_val,
                        string_val,
                        struct_expression,
                        scope_res_ident.clone(),
                        r#match,
                        if_expr,
                        char_expr,
                        tuple,
                        duck_expression,
                        block_expression,
                        just(Token::Break)
                            .to(ValueExpr::Break)
                            .map_with(|x, e| (x, e.span())),
                        just(Token::Continue)
                            .to(ValueExpr::Continue)
                            .map_with(|x, e| (x, e.span())),
                        while_with_condition_and_body
                            .clone()
                            .map(|(cond, body)| ValueExpr::While {
                                condition: Box::new(cond),
                                body: Box::new(body),
                            })
                            .map_with(|x, e| (x, e.span())),
                        just(Token::Return)
                            .ignore_then(value_expr_parser.clone().or_not())
                            .map_with(|x: Option<Spanned<ValueExpr>>, e| {
                                (ValueExpr::Return(x.map(Box::new)), e.span())
                            }),
                    ))),
            )
            .then(
                choice((
                    value_expr_parser
                        .clone()
                        .repeated()
                        .at_least(1)
                        .at_most(1)
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::ControlChar('[')), just(Token::ControlChar(']')))
                        .map(|x| AtomPostParseUnit::ArrayAccess(x[0].clone())),
                    just(Token::ControlChar('<'))
                        .ignore_then(
                            type_expression_parser()
                                .clone()
                                .separated_by(just(Token::ControlChar(',')))
                                .allow_trailing()
                                .at_least(1)
                                .collect::<Vec<Spanned<TypeExpr>>>(),
                        )
                        .then_ignore(just(Token::ControlChar('>')))
                        .or_not()
                        .then(params.clone())
                        .map(|(type_params, params)| {
                            AtomPostParseUnit::FuncCall(params, type_params)
                        }),
                    just(Token::ControlChar('.'))
                        .ignore_then(
                            select_ref! { Token::Ident(s) => s.to_string() }
                                .or(select_ref! { Token::ConstInt(i) => i.to_string() }),
                        )
                        .map(AtomPostParseUnit::FieldAccess),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|((pre, target), params)| {
                let mut target = params.into_iter().fold(target, |acc, x| match x {
                    AtomPostParseUnit::ArrayAccess(idx_expr) => {
                        ValueExpr::ArrayAccess(acc.into(), idx_expr.into()).into_empty_span()
                    }
                    AtomPostParseUnit::FuncCall(params, type_params) => ValueExpr::FunctionCall {
                        target: acc.into(),
                        params,
                        type_params,
                    }
                    .into_empty_span(),
                    AtomPostParseUnit::FieldAccess(field_name) => ValueExpr::FieldAccess {
                        target_obj: acc.into(),
                        field_name,
                    }
                    .into_empty_span(),
                });

                let res = pre
                    .into_iter()
                    .rev()
                    .fold(target, |(acc_expr, acc_span), pre_unit| {
                        (
                            match pre_unit {
                                AtomPreParseUnit::Ref => {
                                    ValueExpr::Ref((acc_expr, acc_span).into())
                                }
                                AtomPreParseUnit::RefMut => {
                                    ValueExpr::RefMut((acc_expr, acc_span).into())
                                }
                                AtomPreParseUnit::Deref => {
                                    ValueExpr::Deref((acc_expr, acc_span).into())
                                }
                                AtomPreParseUnit::BoolNegate => {
                                    ValueExpr::BoolNegate((acc_expr, acc_span).into())
                                }
                            },
                            acc_span,
                        )
                    });

                res
            })
            .map_with(|x, e| (x.0, e.span()))
            .boxed();

            let pen = atom
                .clone()
                .separated_by(just(Token::ThinArrow))
                .at_least(1)
                .collect::<Vec<_>>()
                .map(|exprs| {
                    let first = exprs.first().unwrap().to_owned();
                    exprs[1..]
                        .iter()
                        .map(|expr| expr.to_owned())
                        .fold(first, |acc, elem| {
                            let (
                                ValueExpr::FunctionCall {
                                    target,
                                    mut params,
                                    type_params,
                                },
                                s,
                            ) = elem
                            else {
                                panic!("can only pen functions")
                            };

                            params.insert(0, acc);
                            (
                                ValueExpr::FunctionCall {
                                    target,
                                    params,
                                    type_params,
                                },
                                s,
                            )
                        })
                })
                .map_with(|(x, _), e| (x, e.span()));

            let assignment = just(Token::ControlChar('*'))
                .repeated()
                .collect::<Vec<_>>()
                .then(
                    scope_res_ident
                        .clone()
                        .rewind()
                        .ignore_then(pen.clone().or(atom.clone())),
                )
                .then_ignore(just(Token::ControlChar('=')))
                .then(value_expr_parser.clone())
                .map_with(|((deref, target), value_expr), e| {
                    ValueExpr::VarAssign(
                        (
                            Assignment {
                                target: deref.into_iter().fold(
                                    target,
                                    |(acc_expr, acc_span), _| {
                                        (ValueExpr::Deref((acc_expr, acc_span).into()), acc_span)
                                    },
                                ),
                                value_expr: value_expr.clone(),
                            },
                            e.span(),
                        )
                            .into(),
                    )
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let term = pen.clone().or(atom.clone());

            let prod = term
                .clone()
                .then(
                    choice((
                        just(Token::ControlChar('*')),
                        just(Token::ControlChar('/')),
                        just(Token::ControlChar('%')),
                    ))
                    .then(term)
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (op, x)| {
                        let span = acc.1.union(x.1);
                        let new_expr = match op {
                            Token::ControlChar('*') => ValueExpr::Mul(Box::new(acc), Box::new(x)),
                            Token::ControlChar('/') => ValueExpr::Div(Box::new(acc), Box::new(x)),
                            Token::ControlChar('%') => ValueExpr::Mod(Box::new(acc), Box::new(x)),
                            _ => unreachable!(),
                        };
                        (new_expr, span)
                    })
                })
                .boxed();

            let add = prod
                .clone()
                .then(
                    choice((just(Token::ControlChar('+')), just(Token::ControlChar('-'))))
                        .then(prod.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (op, x)| {
                        let span = acc.1.union(x.1);
                        let new_expr = match op {
                            Token::ControlChar('+') => ValueExpr::Add(Box::new(acc), Box::new(x)),
                            Token::ControlChar('-') => ValueExpr::Sub(Box::new(acc), Box::new(x)),
                            _ => unreachable!(),
                        };
                        (new_expr, span)
                    })
                })
                .boxed();

            let relation = add
                .clone()
                .then(
                    choice((
                        just(Token::ControlChar('<')),
                        just(Token::LessThanOrEquals),
                        just(Token::ControlChar('>')),
                        just(Token::GreaterThanOrEquals),
                    ))
                    .then(add.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (op, x)| {
                        let span = acc.1.union(x.1);
                        let new_expr = match op {
                            Token::ControlChar('<') => {
                                ValueExpr::LessThan(Box::new(acc), Box::new(x))
                            }
                            Token::LessThanOrEquals => {
                                ValueExpr::LessThanOrEquals(Box::new(acc), Box::new(x))
                            }
                            Token::ControlChar('>') => {
                                ValueExpr::GreaterThan(Box::new(acc), Box::new(x))
                            }
                            Token::GreaterThanOrEquals => {
                                ValueExpr::GreaterThanOrEquals(Box::new(acc), Box::new(x))
                            }
                            _ => unreachable!(),
                        };
                        (new_expr, span)
                    })
                })
                .boxed();

            let equality = relation
                .clone()
                .then(
                    choice((just(Token::Equals), just(Token::NotEquals)))
                        .then(relation.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (op, x)| {
                        let span = acc.1.union(x.1);
                        let new_expr = match op {
                            Token::Equals => ValueExpr::Equals(Box::new(acc), Box::new(x)),
                            Token::NotEquals => ValueExpr::NotEquals(Box::new(acc), Box::new(x)),
                            _ => unreachable!(),
                        };
                        (new_expr, span)
                    })
                })
                .boxed();

            let and = equality
                .clone()
                .then(
                    just(Token::And)
                        .then(equality.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (_, x)| {
                        let span = acc.1.union(x.1);
                        (ValueExpr::And(Box::new(acc), Box::new(x)), span)
                    })
                })
                .boxed();

            let or = and
                .clone()
                .then(
                    just(Token::Or)
                        .then(and.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .map(|(init, additional)| {
                    additional.into_iter().fold(init, |acc, (_, x)| {
                        let span = acc.1.union(x.1);
                        (ValueExpr::Or(Box::new(acc), Box::new(x)), span)
                    })
                })
                .boxed();

            let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }
                .map(ValueExpr::InlineGo)
                .map_with(|x, e| (x, e.span()))
                .boxed();

            choice((inline_go, assignment, or, declaration, pen, atom))
                .labelled("expression")
                .boxed()
        },
    )
}

#[allow(dead_code)]
fn empty_tuple() -> ValueExpr {
    ValueExpr::Tuple(Vec::new())
}

#[allow(dead_code)]
fn empty_duck() -> ValueExpr {
    ValueExpr::Duck(Vec::new())
}

// track caller so that we find out where this method is called in case of error
//#[track_caller]
pub fn empty_range() -> SS {
    SS {
        start: 0,
        end: 1,
        context: Context {
            file_name: "TODO: Empty Range",
            file_contents: "TODO: PLEASE DONT".to_string().leak(),
        },
    }
}

pub fn source_file_into_empty_range(v: &mut SourceFile) {
    for x in &mut v.function_definitions {
        x.span = empty_range();
        value_expr_into_empty_range(&mut x.value_expr);
        x.return_type.as_mut().map(type_expr_into_empty_range);
        if let Some(params) = &mut x.params {
            for (_, p) in params {
                type_expr_into_empty_range(p);
            }
        }
    }
    for x in &mut v.type_definitions {
        type_expr_into_empty_range(&mut x.type_expression);
    }
    for x in &mut v.sub_modules {
        source_file_into_empty_range(&mut x.1);
    }

    for x in &mut v.tsx_components {
        x.typescript_source.1 = empty_range()
    }
}

pub fn type_expr_into_empty_range(t: &mut Spanned<TypeExpr>) {
    t.1 = empty_range();
    match &mut t.0 {
        TypeExpr::Ref(t) | TypeExpr::RefMut(t) => type_expr_into_empty_range(&mut *t),
        TypeExpr::Duck(d) => {
            for f in &mut d.fields {
                type_expr_into_empty_range(&mut f.type_expr);
            }
        }
        TypeExpr::Array(t) => {
            t.1 = empty_range();
            type_expr_into_empty_range(t);
        }
        TypeExpr::Tuple(fields) => {
            for f in fields {
                type_expr_into_empty_range(f);
            }
        }
        TypeExpr::Or(types) => {
            for t in types {
                type_expr_into_empty_range(t);
            }
        }
        TypeExpr::Fun(params, return_type) => {
            if let Some(x) = return_type.as_mut() {
                type_expr_into_empty_range(&mut *x)
            }
            for (_, p) in params {
                type_expr_into_empty_range(p);
            }
        }
        TypeExpr::TypeName(_, _, Some(params)) => {
            for p in params {
                type_expr_into_empty_range(p);
            }
        }
        TypeExpr::RawTypeName(_, _, Some(params)) => {
            for p in params {
                type_expr_into_empty_range(p);
            }
        }
        _ => {}
    }
}

pub fn value_expr_into_empty_range(v: &mut Spanned<ValueExpr>) {
    v.1 = empty_range();
    match &mut v.0 {
        ValueExpr::Deref(v) | ValueExpr::Ref(v) | ValueExpr::RefMut(v) => {
            value_expr_into_empty_range(&mut *v)
        }
        ValueExpr::ArrayAccess(target, idx_expr) => {
            value_expr_into_empty_range(target);
            value_expr_into_empty_range(idx_expr);
        }
        ValueExpr::Array(t, elems) => {
            if let Some(t) = t {
                type_expr_into_empty_range(t);
            }
            for elem in elems {
                value_expr_into_empty_range(elem);
            }
        }
        ValueExpr::FunctionCall {
            target,
            params,
            type_params,
        } => {
            value_expr_into_empty_range(target);
            for p in params {
                value_expr_into_empty_range(p);
            }

            if let Some(type_params) = type_params {
                type_params.iter_mut().for_each(|type_param| {
                    type_expr_into_empty_range(type_param);
                })
            }
        }
        ValueExpr::Sub(lhs, rhs)
        | ValueExpr::Mod(lhs, rhs)
        | ValueExpr::Div(lhs, rhs)
        | ValueExpr::Add(lhs, rhs) => {
            value_expr_into_empty_range(lhs);
            value_expr_into_empty_range(rhs);
        }
        ValueExpr::If {
            condition,
            then,
            r#else,
        } => {
            value_expr_into_empty_range(condition);
            value_expr_into_empty_range(then);
            if let Some(r#else) = r#else {
                value_expr_into_empty_range(r#else);
            }
        }
        ValueExpr::Mul(v1, v2) => {
            value_expr_into_empty_range(v1);
            value_expr_into_empty_range(v2);
        }
        ValueExpr::Duck(fields) => {
            for field in fields {
                value_expr_into_empty_range(&mut field.1);
            }
        }
        ValueExpr::Block(exprs) => {
            for expr in exprs {
                value_expr_into_empty_range(expr);
            }
        }
        ValueExpr::Tuple(fields) => {
            for field in fields {
                value_expr_into_empty_range(field);
            }
        }
        ValueExpr::Equals(lhs, rhs)
        | ValueExpr::NotEquals(lhs, rhs)
        | ValueExpr::LessThan(lhs, rhs)
        | ValueExpr::LessThanOrEquals(lhs, rhs)
        | ValueExpr::GreaterThan(lhs, rhs)
        | ValueExpr::GreaterThanOrEquals(lhs, rhs)
        | ValueExpr::And(lhs, rhs)
        | ValueExpr::Or(lhs, rhs) => {
            value_expr_into_empty_range(lhs);
            value_expr_into_empty_range(rhs);
        }
        ValueExpr::Lambda(b) => {
            value_expr_into_empty_range(&mut b.value_expr);
            b.return_type.as_mut().map(type_expr_into_empty_range);
            for (_, p) in &mut b.params {
                type_expr_into_empty_range(p);
            }
        }
        ValueExpr::Return(Some(v)) => value_expr_into_empty_range(v),
        ValueExpr::Struct { fields, .. } => {
            for field in fields {
                value_expr_into_empty_range(&mut field.1);
            }
        }
        ValueExpr::While { condition, body } => {
            value_expr_into_empty_range(condition);
            value_expr_into_empty_range(body);
        }
        ValueExpr::VarDecl(b) => {
            b.1 = empty_range();
            value_expr_into_empty_range(&mut b.0.initializer);
            if let Some(type_expr) = &mut b.0.type_expr {
                type_expr_into_empty_range(type_expr);
            }
        }
        ValueExpr::VarAssign(a) => {
            a.1 = empty_range();
            value_expr_into_empty_range(&mut a.0.target);
            value_expr_into_empty_range(&mut a.0.value_expr);
        }
        ValueExpr::BoolNegate(b) => value_expr_into_empty_range(b),
        ValueExpr::FieldAccess {
            target_obj,
            field_name: _,
        } => {
            value_expr_into_empty_range(target_obj);
        }
        ValueExpr::Match {
            value_expr,
            arms,
            else_arm,
            span,
        } => {
            *span = empty_range();

            value_expr_into_empty_range(value_expr);
            arms.iter_mut().for_each(|arm| {
                type_expr_into_empty_range(&mut arm.type_case);
                value_expr_into_empty_range(&mut arm.value_expr);
                if let Some(condition) = &mut arm.condition {
                    value_expr_into_empty_range(condition);
                }
                arm.span = empty_range();
            });

            if let Some(arm) = else_arm {
                arm.span = empty_range();
                type_expr_into_empty_range(&mut arm.type_case);
                value_expr_into_empty_range(&mut arm.value_expr);
            }
        }
        ValueExpr::HtmlString(contents) => {
            for c in contents {
                if let ValHtmlStringContents::Expr(e) = c {
                    value_expr_into_empty_range(e);
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use crate::parse::{
        Field, Spanned,
        function_parser::LambdaFunctionExpr,
        lexer::lex_parser,
        make_input,
        type_parser::{Duck, TypeExpr},
        value_parser::{
            Assignment, Declaration, MatchArm, ValHtmlStringContents, empty_duck, empty_range,
            empty_tuple, type_expr_into_empty_range, value_expr_into_empty_range,
            value_expr_parser,
        },
    };

    use super::ValueExpr;

    fn var(x: impl Into<String>) -> Box<Spanned<ValueExpr>> {
        ValueExpr::RawVariable(false, vec![x.into()])
            .into_empty_span()
            .into()
    }

    fn gvar(x: impl Into<String>) -> Box<Spanned<ValueExpr>> {
        ValueExpr::RawVariable(true, vec![x.into()])
            .into_empty_span()
            .into()
    }

    fn v_var(x: &[impl AsRef<str>]) -> Box<Spanned<ValueExpr>> {
        ValueExpr::RawVariable(false, x.iter().map(|x| x.as_ref().to_string()).collect())
            .into_empty_span()
            .into()
    }

    fn v_gvar(x: &[impl AsRef<str>]) -> Box<Spanned<ValueExpr>> {
        ValueExpr::RawVariable(true, x.iter().map(|x| x.as_ref().to_string()).collect())
            .into_empty_span()
            .into()
    }

    #[test]
    fn test_value_expression_parser() {
        let test_cases = vec![
            (
                "duckx {5;<h1>{<p></p>}</h1>}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(5).into_empty_span(),
                    ValueExpr::HtmlString(vec![
                        ValHtmlStringContents::String("<h1>".to_string()),
                        ValHtmlStringContents::Expr(
                            ValueExpr::Block(vec![
                                ValueExpr::HtmlString(vec![ValHtmlStringContents::String(
                                    "<p></p>".to_string(),
                                )])
                                .into_empty_span(),
                            ])
                            .into_empty_span(),
                        ),
                        ValHtmlStringContents::String("</h1>".to_string()),
                    ])
                    .into_empty_span(),
                ]),
            ),
            (".tag", ValueExpr::Tag("tag".to_string())),
            ("..", ValueExpr::Tag("DOT".to_string())),
            (
                "a<String>()",
                ValueExpr::FunctionCall {
                    target: var("a"),
                    params: vec![],
                    type_params: Some(vec![TypeExpr::String.into_empty_span()]),
                },
            ),
            (
                "b->a<String>()",
                ValueExpr::FunctionCall {
                    target: var("a"),
                    params: vec![*var("b")],
                    type_params: Some(vec![TypeExpr::String.into_empty_span()]),
                },
            ),
            (
                "a->b() == x->c()",
                ValueExpr::Equals(
                    ValueExpr::FunctionCall {
                        target: var("b"),
                        params: vec![*var("a")],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("c"),
                        params: vec![*var("x")],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "a->x.x()->c(1)",
                ValueExpr::FunctionCall {
                    target: var("c"),
                    params: vec![
                        ValueExpr::FunctionCall {
                            target: ValueExpr::FieldAccess {
                                target_obj: var("x"),
                                field_name: "x".into(),
                            }
                            .into_empty_span()
                            .into(),
                            params: vec![*var("a")],
                            type_params: None,
                        }
                        .into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            (
                "a->b()",
                ValueExpr::FunctionCall {
                    target: var("b"),
                    params: vec![*var("a")],
                    type_params: None,
                },
            ),
            (
                ".Int[]",
                ValueExpr::Array(Some(TypeExpr::Int.into_empty_span()), vec![]),
            ),
            (
                "[1]",
                ValueExpr::Array(None, vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            (
                "[1,]",
                ValueExpr::Array(None, vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            (
                "[1,]",
                ValueExpr::Array(None, vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            (
                "[1,2]",
                ValueExpr::Array(
                    None,
                    vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Int(2).into_empty_span(),
                    ],
                ),
            ),
            (
                "[1 == 2, 2]",
                ValueExpr::Array(
                    None,
                    vec![
                        ValueExpr::Equals(
                            Box::new(ValueExpr::Int(1).into_empty_span()),
                            Box::new(ValueExpr::Int(2).into_empty_span()),
                        )
                        .into_empty_span(),
                        ValueExpr::Int(2).into_empty_span(),
                    ],
                ),
            ),
            (
                "[[1,2,3], .Int[]]",
                ValueExpr::Array(
                    None,
                    vec![
                        ValueExpr::Array(
                            None,
                            vec![
                                ValueExpr::Int(1).into_empty_span(),
                                ValueExpr::Int(2).into_empty_span(),
                                ValueExpr::Int(3).into_empty_span(),
                            ],
                        )
                        .into_empty_span(),
                        ValueExpr::Array(Some(TypeExpr::Int.into_empty_span()), vec![])
                            .into_empty_span(),
                    ],
                ),
            ),
            (
                "a.y[0]",
                ValueExpr::ArrayAccess(
                    ValueExpr::FieldAccess {
                        target_obj: var("a"),
                        field_name: "y".into(),
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(0).into_empty_span().into(),
                ),
            ),
            (
                "[1][0]",
                ValueExpr::ArrayAccess(
                    ValueExpr::Array(None, vec![ValueExpr::Int(1).into_empty_span()])
                        .into_empty_span()
                        .into(),
                    ValueExpr::Int(0).into_empty_span().into(),
                ),
            ),
            (
                "a.y()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FieldAccess {
                        target_obj: var("a"),
                        field_name: "y".into(),
                    }
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "a.y[0] = 5",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::ArrayAccess(
                                ValueExpr::FieldAccess {
                                    target_obj: var("a"),
                                    field_name: "y".into(),
                                }
                                .into_empty_span()
                                .into(),
                                ValueExpr::Int(0).into_empty_span().into(),
                            )
                            .into_empty_span()
                            .into(),
                            value_expr: ValueExpr::Int(5).into_empty_span().into(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "a[0]",
                ValueExpr::ArrayAccess(var("a"), ValueExpr::Int(0).into_empty_span().into()),
            ),
            (
                "a[0][0]",
                ValueExpr::ArrayAccess(
                    ValueExpr::ArrayAccess(var("a"), ValueExpr::Int(0).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                    ValueExpr::Int(0).into_empty_span().into(),
                ),
            ),
            (
                "a()[0]()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::ArrayAccess(
                        ValueExpr::FunctionCall {
                            target: var("a"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span()
                        .into(),
                        ValueExpr::Int(0).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "a()[0][0]()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::ArrayAccess(
                        ValueExpr::ArrayAccess(
                            ValueExpr::FunctionCall {
                                target: var("a"),
                                params: vec![],
                                type_params: None,
                            }
                            .into_empty_span()
                            .into(),
                            ValueExpr::Int(0).into_empty_span().into(),
                        )
                        .into_empty_span()
                        .into(),
                        ValueExpr::Int(0).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "[a(), b(), (1,2)]",
                ValueExpr::Array(
                    None,
                    vec![
                        ValueExpr::FunctionCall {
                            target: var("a"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span(),
                        ValueExpr::FunctionCall {
                            target: var("b"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span(),
                        ValueExpr::Tuple(vec![
                            ValueExpr::Int(1).into_empty_span(),
                            ValueExpr::Int(2).into_empty_span(),
                        ])
                        .into_empty_span(),
                    ],
                ),
            ),
            ("true", ValueExpr::Bool(true)),
            ("false", ValueExpr::Bool(false)),
            (
                "MyStruct { x: 5 }",
                ValueExpr::Struct {
                    name: "MyStruct".to_string(),
                    fields: vec![("x".to_string(), ValueExpr::Int(5).into_empty_span())],
                    type_params: None,
                },
            ),
            (
                "Outer { x: 5, y: Inner { x: 5 } }",
                ValueExpr::Struct {
                    name: "Outer".to_string(),
                    fields: vec![
                        ("x".to_string(), ValueExpr::Int(5).into_empty_span()),
                        (
                            "y".to_string(),
                            ValueExpr::Struct {
                                name: "Inner".to_string(),
                                fields: vec![(
                                    "x".to_string(),
                                    ValueExpr::Int(5).into_empty_span(),
                                )],
                                type_params: None,
                            }
                            .into_empty_span(),
                        ),
                    ],
                    type_params: None,
                },
            ),
            ("{}", empty_duck()),
            (
                "to_upper()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                    type_params: None,
                },
            ),
            (
                "to_upper(1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper(1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper ()",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                    type_params: None,
                },
            ),
            (
                "to_upper (1)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper (1,)",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper (   )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: Vec::new(),
                    type_params: None,
                },
            ),
            (
                "to_upper ( 1 )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "::to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: gvar("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "abc::to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: v_var(&["abc", "to_upper"]),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "abc::xyz::to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: v_var(&["abc", "xyz", "to_upper"]),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "::abc::xyz::to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: v_gvar(&["abc", "xyz", "to_upper"]),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper ( 1  ,  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![ValueExpr::Int(1).into_empty_span()],
                    type_params: None,
                },
            ),
            (
                "to_upper ( to_lower(1,2,add(5, 10),4), true  )",
                ValueExpr::FunctionCall {
                    target: var("to_upper"),
                    params: vec![
                        ValueExpr::FunctionCall {
                            target: var("to_lower"),
                            params: vec![
                                ValueExpr::Int(1).into_empty_span(),
                                ValueExpr::Int(2).into_empty_span(),
                                ValueExpr::FunctionCall {
                                    target: var("add"),
                                    params: vec![
                                        ValueExpr::Int(5).into_empty_span(),
                                        ValueExpr::Int(10).into_empty_span(),
                                    ],
                                    type_params: None,
                                }
                                .into_empty_span(),
                                ValueExpr::Int(4).into_empty_span(),
                            ],
                            type_params: None,
                        }
                        .into_empty_span(),
                        ValueExpr::Bool(true).into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            (
                "print(\"hallo\", \"moin\")",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        ValueExpr::String("hallo".into(), true).into_empty_span(),
                        ValueExpr::String("moin".into(), true).into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            ("x", ValueExpr::RawVariable(false, vec!["x".into()])),
            (
                "print(x, true, lol())",
                ValueExpr::FunctionCall {
                    target: var("print"),
                    params: vec![
                        *var("x"),
                        ValueExpr::Bool(true).into_empty_span(),
                        ValueExpr::FunctionCall {
                            target: var("lol"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            (
                "if (true) { 1 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Int(1).into_empty_span_and_block().into(),
                    r#else: Some(ValueExpr::Int(2).into_empty_span_and_block().into()),
                },
            ),
            (
                "if (true) { 1 } else if (false) { 3 } else if (200) { 4 } else { 2 }",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Int(1).into_empty_span_and_block().into(),
                    r#else: Some(
                        ValueExpr::If {
                            condition: ValueExpr::Bool(false).into_empty_span().into(),
                            then: ValueExpr::Int(3).into_empty_span_and_block().into(),
                            r#else: Some(
                                ValueExpr::If {
                                    condition: ValueExpr::Int(200).into_empty_span().into(),
                                    then: ValueExpr::Int(4).into_empty_span_and_block().into(),
                                    r#else: Some(
                                        ValueExpr::Int(2).into_empty_span_and_block().into(),
                                    ),
                                }
                                .into_empty_span()
                                .into(),
                            ),
                        }
                        .into_empty_span()
                        .into(),
                    ),
                },
            ),
            (
                "(1,true,2,\"hallo\")",
                ValueExpr::Tuple(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Bool(true).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::String("hallo".into(), true).into_empty_span(),
                ]),
            ),
            (
                "!std::arch::is_windows()",
                ValueExpr::BoolNegate(
                    ValueExpr::FunctionCall {
                        target: v_var(&["std", "arch", "is_windows"]),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!std::arch::is_windows",
                ValueExpr::BoolNegate(v_var(&["std", "arch", "is_windows"])),
            ),
            ("!std::arch", ValueExpr::BoolNegate(v_var(&["std", "arch"]))),
            (
                "x.y = 100",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::FieldAccess {
                                target_obj: var("x"),
                                field_name: "y".into(),
                            }
                            .into_empty_span(),
                            value_expr: ValueExpr::Int(100).into_empty_span(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "std::xs.y.z = 100",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::FieldAccess {
                                field_name: "z".into(),
                                target_obj: Box::new(
                                    ValueExpr::FieldAccess {
                                        target_obj: v_var(&["std", "xs"]),
                                        field_name: "y".into(),
                                    }
                                    .into_empty_span(),
                                ),
                            }
                            .into_empty_span(),
                            value_expr: ValueExpr::Int(100).into_empty_span(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "x.y.z = 100",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::FieldAccess {
                                field_name: "z".into(),
                                target_obj: Box::new(
                                    ValueExpr::FieldAccess {
                                        target_obj: var("x"),
                                        field_name: "y".into(),
                                    }
                                    .into_empty_span(),
                                ),
                            }
                            .into_empty_span(),
                            value_expr: ValueExpr::Int(100).into_empty_span(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            ("{}", empty_duck()),
            (
                "{}.x",
                ValueExpr::FieldAccess {
                    target_obj: empty_duck().into_empty_span().into(),
                    field_name: "x".into(),
                },
            ),
            (
                "!{}.x",
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: empty_duck().into_empty_span().into(),
                        field_name: "x".into(),
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "{1}",
                ValueExpr::Block(vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            (
                "{1;  2   ;3;x()}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::Int(3).into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span(),
                ]),
            ),
            (
                "{1;  2   ;3;x({})}",
                ValueExpr::Block(vec![
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                    ValueExpr::Int(3).into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![empty_duck().into_empty_span()],
                        type_params: None,
                    }
                    .into_empty_span(),
                ]),
            ),
            (
                "{x();y();}",
                ValueExpr::Block(vec![
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span(),
                    empty_tuple().into_empty_span(),
                ]),
            ),
            (
                "x({ 1; 2; y({ z(); }) }, lol)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            ValueExpr::Int(1).into_empty_span(),
                            ValueExpr::Int(2).into_empty_span(),
                            ValueExpr::FunctionCall {
                                target: var("y"),
                                params: vec![
                                    ValueExpr::Block(vec![
                                        ValueExpr::FunctionCall {
                                            target: var("z"),
                                            params: vec![],
                                            type_params: None,
                                        }
                                        .into_empty_span(),
                                        empty_tuple().into_empty_span(),
                                    ])
                                    .into_empty_span(),
                                ],
                                type_params: None,
                            }
                            .into_empty_span(),
                        ])
                        .into_empty_span(),
                        *var("lol"),
                    ],
                    type_params: None,
                },
            ),
            (
                "a()()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FunctionCall {
                        target: var("a"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "(fn() fn() {1})()()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FunctionCall {
                        target: ValueExpr::Lambda(
                            LambdaFunctionExpr {
                                params: vec![],
                                return_type: None,
                                value_expr: ValueExpr::Lambda(
                                    LambdaFunctionExpr {
                                        params: vec![],
                                        return_type: None,
                                        value_expr: ValueExpr::Block(vec![
                                            ValueExpr::Int(1).into_empty_span(),
                                        ])
                                        .into_empty_span(),
                                    }
                                    .into(),
                                )
                                .into_empty_span()
                                .into(),
                            }
                            .into(),
                        )
                        .into_empty_span()
                        .into(),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "while (true) {}",
                ValueExpr::While {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    body: ValueExpr::Block(vec![]).into_empty_span().into(),
                },
            ),
            (
                "while (my_func()) {}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    body: ValueExpr::Block(vec![]).into_empty_span().into(),
                },
            ),
            (
                "while (my_func()) {1;break;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Break.into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                },
            ),
            (
                "while (my_func()) {1;continue;}",
                ValueExpr::While {
                    condition: ValueExpr::FunctionCall {
                        target: var("my_func"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    body: ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Continue.into_empty_span(),
                        empty_tuple().into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                },
            ),
            (
                ".Int[][.Int[]]",
                ValueExpr::Array(
                    Some(TypeExpr::Array(TypeExpr::Int.into_empty_span().into()).into_empty_span()),
                    vec![
                        ValueExpr::Array(Some(TypeExpr::Int.into_empty_span()), vec![])
                            .into_empty_span(),
                    ],
                ),
            ),
            (
                "[1]",
                ValueExpr::Array(None, vec![ValueExpr::Int(1).into_empty_span()]),
            ),
            ("()", empty_tuple()),
            (
                "(1.1, 'x')",
                ValueExpr::Tuple(vec![
                    ValueExpr::Float(1.1).into_empty_span(),
                    ValueExpr::Char('x').into_empty_span(),
                ]),
            ),
            (
                "{x: 1, y: { z: true }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1).into_empty_span()),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![(
                            "z".into(),
                            ValueExpr::Bool(true).into_empty_span(),
                        )])
                        .into_empty_span(),
                    ),
                ]),
            ),
            (
                "{x: 1, y: { z: true, w: { print();2;true } }}",
                ValueExpr::Duck(vec![
                    ("x".into(), ValueExpr::Int(1).into_empty_span()),
                    (
                        "y".into(),
                        ValueExpr::Duck(vec![
                            (
                                "w".into(),
                                ValueExpr::Block(vec![
                                    ValueExpr::FunctionCall {
                                        target: var("print"),
                                        params: vec![],
                                        type_params: None,
                                    }
                                    .into_empty_span(),
                                    ValueExpr::Int(2).into_empty_span(),
                                    ValueExpr::Bool(true).into_empty_span(),
                                ])
                                .into_empty_span(),
                            ),
                            ("z".into(), ValueExpr::Bool(true).into_empty_span()),
                        ])
                        .into_empty_span(),
                    ),
                ]),
            ),
            (
                "if (true) {{}} else {{x: 1}}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Duck(vec![]).into_empty_span_and_block().into(),
                    r#else: Some(
                        ValueExpr::Duck(vec![("x".into(), ValueExpr::Int(1).into_empty_span())])
                            .into_empty_span_and_block()
                            .into(),
                    ),
                },
            ),
            (
                "x.y",
                ValueExpr::FieldAccess {
                    target_obj: var("x"),
                    field_name: "y".into(),
                },
            ),
            (
                "{x: 123}.y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![(
                        "x".into(),
                        ValueExpr::Int(123).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "x().y",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "y".into(),
                },
            ),
            (
                "(x)()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "x()",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "(1)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(1).into_empty_span().into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "(123)()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::Int(123).into_empty_span().into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "(returns_lambda())()",
                ValueExpr::FunctionCall {
                    target: ValueExpr::FunctionCall {
                        target: var("returns_lambda"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    params: vec![],
                    type_params: None,
                },
            ),
            (
                "x.y.z == z.w.a",
                ValueExpr::Equals(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("z"),
                            field_name: "w".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "a".into(),
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "x.y.z.w",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                    field_name: "w".into(),
                },
            ),
            ("((1))", ValueExpr::Int(1)),
            (
                "x({();();},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple().into_empty_span(),
                            empty_tuple().into_empty_span(),
                            empty_tuple().into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            (
                "x({();{();1;};},1)",
                ValueExpr::FunctionCall {
                    target: var("x"),
                    params: vec![
                        ValueExpr::Block(vec![
                            empty_tuple().into_empty_span(),
                            ValueExpr::Block(vec![
                                empty_tuple().into_empty_span(),
                                ValueExpr::Int(1).into_empty_span(),
                                empty_tuple().into_empty_span(),
                            ])
                            .into_empty_span(),
                            empty_tuple().into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                    ],
                    type_params: None,
                },
            ),
            (
                "return 123",
                ValueExpr::Return(Some(Box::new(ValueExpr::Int(123).into_empty_span().into()))),
            ),
            (
                "let x: String = \"\"",
                ValueExpr::VarDecl(
                    (
                        Declaration {
                            name: "x".into(),
                            initializer: ValueExpr::String("".to_string(), true).into_empty_span(),
                            type_expr: Some(TypeExpr::String.into_empty_span()),
                            is_const: false,
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "const x: String = \"\"",
                ValueExpr::VarDecl(
                    (
                        Declaration {
                            name: "x".into(),
                            initializer: ValueExpr::String("".to_string(), true).into_empty_span(),
                            type_expr: Some(TypeExpr::String.into_empty_span()),
                            is_const: true,
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "x() * y()",
                ValueExpr::Mul(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "3 * 5",
                ValueExpr::Mul(
                    ValueExpr::Int(3).into_empty_span().into(),
                    ValueExpr::Int(5).into_empty_span().into(),
                ),
            ),
            (
                "3 + 5",
                ValueExpr::Add(
                    ValueExpr::Int(3).into_empty_span().into(),
                    ValueExpr::Int(5).into_empty_span().into(),
                ),
            ),
            (
                "3 * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(
                        ValueExpr::Int(3).into_empty_span().into(),
                        ValueExpr::Int(5).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(6).into_empty_span().into(),
                ),
            ),
            (
                "x() * 5 * 6",
                ValueExpr::Mul(
                    ValueExpr::Mul(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span()
                        .into(),
                        ValueExpr::Int(5).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Int(6).into_empty_span().into(),
                ),
            ),
            (
                "!true",
                ValueExpr::BoolNegate(ValueExpr::Bool(true).into_empty_span().into()),
            ),
            (
                "!{1;2;true}",
                ValueExpr::BoolNegate(
                    ValueExpr::Block(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Int(2).into_empty_span(),
                        ValueExpr::Bool(true).into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!!x()",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FunctionCall {
                            target: var("x"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::BoolNegate(
                        ValueExpr::FieldAccess {
                            target_obj: ValueExpr::FieldAccess {
                                target_obj: var("x"),
                                field_name: "y".into(),
                            }
                            .into_empty_span()
                            .into(),
                            field_name: "z".into(),
                        }
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!x.y.z",
                ValueExpr::BoolNegate(
                    ValueExpr::FieldAccess {
                        target_obj: ValueExpr::FieldAccess {
                            target_obj: var("x"),
                            field_name: "y".into(),
                        }
                        .into_empty_span()
                        .into(),
                        field_name: "z".into(),
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "x() == y()",
                ValueExpr::Equals(
                    ValueExpr::FunctionCall {
                        target: var("x"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    ValueExpr::FunctionCall {
                        target: var("y"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "1 == 2",
                ValueExpr::Equals(
                    ValueExpr::Int(1).into_empty_span().into(),
                    ValueExpr::Int(2).into_empty_span().into(),
                ),
            ),
            (
                "!(1 == 2)",
                ValueExpr::BoolNegate(
                    ValueExpr::Equals(
                        ValueExpr::Int(1).into_empty_span().into(),
                        ValueExpr::Int(2).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "!1 == !2",
                ValueExpr::Equals(
                    ValueExpr::BoolNegate(ValueExpr::Int(1).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                    ValueExpr::BoolNegate(ValueExpr::Int(2).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                ),
            ),
            ("go {}", ValueExpr::InlineGo(String::new())),
            (
                "go { go func() {} }",
                ValueExpr::InlineGo(String::from(" go func() {} ")),
            ),
            (
                "fn() {}",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: None,
                        value_expr: ValueExpr::Block(vec![]).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "fn() 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: None,
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "fn() -> Int 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![],
                        return_type: Some(TypeExpr::Int.into_empty_span()),
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "fn(x: String) -> Int 1",
                ValueExpr::Lambda(
                    LambdaFunctionExpr {
                        params: vec![("x".into(), TypeExpr::String.into_empty_span())],
                        return_type: Some(TypeExpr::Int.into_empty_span()),
                        value_expr: ValueExpr::Int(1).into_empty_span(),
                    }
                    .into(),
                ),
            ),
            (
                "{x: 1}.x",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Duck(vec![(
                        "x".into(),
                        ValueExpr::Int(1).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                    field_name: "x".into(),
                },
            ),
            (
                "(1,(3,4),\"s\").0",
                ValueExpr::FieldAccess {
                    target_obj: ValueExpr::Tuple(vec![
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Tuple(vec![
                            ValueExpr::Int(3).into_empty_span(),
                            ValueExpr::Int(4).into_empty_span(),
                        ])
                        .into_empty_span(),
                        ValueExpr::String("s".into(), true).into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                    field_name: "0".into(),
                },
            ),
            (
                "{go {} 1;2}",
                ValueExpr::Block(vec![
                    ValueExpr::InlineGo("".into()).into_empty_span(),
                    ValueExpr::Int(1).into_empty_span(),
                    ValueExpr::Int(2).into_empty_span(),
                ]),
            ),
            (
                "if (true) {go {} 1;2}",
                ValueExpr::If {
                    condition: ValueExpr::Bool(true).into_empty_span().into(),
                    then: ValueExpr::Block(vec![
                        ValueExpr::InlineGo("".into()).into_empty_span(),
                        ValueExpr::Int(1).into_empty_span(),
                        ValueExpr::Int(2).into_empty_span(),
                    ])
                    .into_empty_span()
                    .into(),
                    r#else: None,
                },
            ),
            (
                "match (5) { Int @i => i }",
                ValueExpr::Match {
                    value_expr: Box::new(ValueExpr::Int(5).into_empty_span()),
                    arms: vec![MatchArm {
                        type_case: TypeExpr::Int.into_empty_span(),
                        identifier_binding: Some("i".to_string()),
                        value_expr: *var("i"),
                        condition: None,
                        span: empty_range(),
                    }],
                    else_arm: None,
                    span: empty_range(),
                },
            ),
            (
                "match (\"Hallo\") { String @s => s, Int @i => i }",
                ValueExpr::Match {
                    value_expr: Box::new(
                        ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                    ),
                    arms: vec![
                        MatchArm {
                            type_case: TypeExpr::String.into_empty_span(),
                            identifier_binding: Some("s".to_string()),
                            value_expr: *var("s"),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::Int.into_empty_span(),
                            identifier_binding: Some("i".to_string()),
                            value_expr: *var("i"),
                            condition: None,
                            span: empty_range(),
                        },
                    ],
                    else_arm: None,
                    span: empty_range(),
                },
            ),
            (
                "match (\"Hallo\") { String @s if s => s, Int @i => i }",
                ValueExpr::Match {
                    value_expr: Box::new(
                        ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                    ),
                    arms: vec![
                        MatchArm {
                            type_case: TypeExpr::String.into_empty_span(),
                            identifier_binding: Some("s".to_string()),
                            value_expr: *var("s"),
                            condition: Some(*var("s")),
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::Int.into_empty_span(),
                            identifier_binding: Some("i".to_string()),
                            value_expr: *var("i"),
                            condition: None,
                            span: empty_range(),
                        },
                    ],
                    else_arm: None,
                    span: empty_range(),
                },
            ),
            (
                "match (\"Hallo\") { String @s => s, Int @i => i, Other @o => {
                return o
                } }",
                ValueExpr::Match {
                    value_expr: Box::new(
                        ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                    ),
                    arms: vec![
                        MatchArm {
                            type_case: TypeExpr::String.into_empty_span(),
                            identifier_binding: Some("s".to_string()),
                            value_expr: *var("s"),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::Int.into_empty_span(),
                            identifier_binding: Some("i".to_string()),
                            value_expr: *var("i"),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], None)
                                .into_empty_span(),
                            identifier_binding: Some("o".to_string()),
                            value_expr: ValueExpr::Block(vec![
                                ValueExpr::Return(Some(var("o"))).into_empty_span(),
                            ])
                            .into_empty_span(),
                            condition: None,
                            span: empty_range(),
                        },
                    ],
                    else_arm: None,
                    span: empty_range(),
                },
            ),
            (
                "match (\"Hallo\") {
                String @s => s,
                Int @i => i,
                Other @o => {
                return o
                },
                Other @o => {
                match (o) {
                String @s => s
                }
                },
                }",
                ValueExpr::Match {
                    value_expr: Box::new(
                        ValueExpr::String("Hallo".to_string(), true).into_empty_span(),
                    ),
                    arms: vec![
                        MatchArm {
                            type_case: TypeExpr::String.into_empty_span(),
                            identifier_binding: Some("s".to_string()),
                            value_expr: *var("s"),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::Int.into_empty_span(),
                            identifier_binding: Some("i".to_string()),
                            value_expr: *var("i"),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], None)
                                .into_empty_span(),
                            identifier_binding: Some("o".to_string()),
                            value_expr: ValueExpr::Block(vec![
                                ValueExpr::Return(Some(var("o"))).into_empty_span(),
                            ])
                            .into_empty_span(),
                            condition: None,
                            span: empty_range(),
                        },
                        MatchArm {
                            type_case: TypeExpr::RawTypeName(false, vec!["Other".into()], None)
                                .into_empty_span(),
                            identifier_binding: Some("o".to_string()),
                            value_expr: ValueExpr::Block(vec![
                                ValueExpr::Match {
                                    value_expr: var("o"),
                                    arms: vec![MatchArm {
                                        type_case: TypeExpr::String.into_empty_span(),
                                        identifier_binding: Some("s".to_string()),
                                        value_expr: *var("s"),
                                        condition: None,
                                        span: empty_range(),
                                    }],
                                    else_arm: None,
                                    span: empty_range(),
                                }
                                .into_empty_span(),
                            ])
                            .into_empty_span(),
                            condition: None,
                            span: empty_range(),
                        },
                    ],
                    else_arm: None,
                    span: empty_range(),
                },
            ),
            ("&mut a", ValueExpr::RefMut(var("a"))),
            (
                "&mut { x: 10 }",
                ValueExpr::RefMut(
                    ValueExpr::Duck(vec![(
                        "x".to_string(),
                        ValueExpr::Int(10).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "&{ x: 10 }",
                ValueExpr::Ref(
                    ValueExpr::Duck(vec![(
                        "x".to_string(),
                        ValueExpr::Int(10).into_empty_span(),
                    )])
                    .into_empty_span()
                    .into(),
                ),
            ),
            ("&a", ValueExpr::Ref(var("a"))),
            ("*a", ValueExpr::Deref(var("a"))),
            (
                "&*a",
                ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into()),
            ),
            (
                "&&*a",
                ValueExpr::Ref(
                    ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                ),
            ),
            (
                "&mut &&*a",
                ValueExpr::RefMut(
                    ValueExpr::Ref(
                        ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                            .into_empty_span()
                            .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "*&mut &&*a",
                ValueExpr::Deref(
                    ValueExpr::RefMut(
                        ValueExpr::Ref(
                            ValueExpr::Ref(ValueExpr::Deref(var("a")).into_empty_span().into())
                                .into_empty_span()
                                .into(),
                        )
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "*a.x",
                ValueExpr::Deref(
                    ValueExpr::FieldAccess {
                        target_obj: var("a"),
                        field_name: "x".to_string(),
                    }
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "*a * *b",
                ValueExpr::Mul(
                    ValueExpr::Deref(var("a")).into_empty_span().into(),
                    ValueExpr::Deref(var("b")).into_empty_span().into(),
                ),
            ),
            (
                "*a = 10",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::Deref(var("a")).into_empty_span(),
                            value_expr: ValueExpr::Int(10).into_empty_span(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
            (
                "*a.x = 10",
                ValueExpr::VarAssign(
                    (
                        Assignment {
                            target: ValueExpr::Deref(
                                ValueExpr::FieldAccess {
                                    target_obj: var("a"),
                                    field_name: "x".to_string(),
                                }
                                .into_empty_span()
                                .into(),
                            )
                            .into_empty_span(),
                            value_expr: ValueExpr::Int(10).into_empty_span(),
                        },
                        empty_range(),
                    )
                        .into(),
                ),
            ),
        ];

        for (i, (src, expected_tokens)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
            dbg!(&lex_result);
            let parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));

            assert_eq!(
                parse_result.has_errors(),
                false,
                "{i}: {} {:?} {:?}",
                src,
                lex_result,
                parse_result
            );
            assert_eq!(parse_result.has_output(), true, "{i}: {}", src);

            let mut output = parse_result.into_result().expect(&src);
            value_expr_into_empty_range(&mut output);

            assert_eq!(output.0, expected_tokens, "{i}: {}", src);
        }
    }

    #[test]
    pub fn test_declaration_parser() {
        let inputs_and_expected_outputs = vec![
            (
                "let x: String = \"\"",
                Declaration {
                    name: "x".to_string(),
                    type_expr: Some(TypeExpr::String.into_empty_span()),
                    initializer: ValueExpr::String("".to_string(), true).into_empty_span(),
                    is_const: false,
                },
            ),
            (
                "let y: { x: Int } = {}",
                Declaration {
                    name: "y".to_string(),
                    type_expr: Some(
                        TypeExpr::Duck(Duck {
                            fields: vec![Field::new(
                                "x".to_string(),
                                TypeExpr::Int.into_empty_span(),
                            )],
                        })
                        .into_empty_span(),
                    ),
                    initializer: ValueExpr::Duck(vec![]).into_empty_span(),
                    is_const: false,
                },
            ),
            (
                "let z: {} = {}",
                Declaration {
                    name: "z".to_string(),
                    type_expr: Some(TypeExpr::Any.into_empty_span()),
                    initializer: empty_duck().into_empty_span(),
                    is_const: false,
                },
            ),
        ];

        for (input, expected_output) in inputs_and_expected_outputs {
            let lexer_parse_result = lex_parser("test", "").parse(input);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            let declaration_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(declaration_parse_result.has_errors(), false);
            assert_eq!(declaration_parse_result.has_output(), true);

            let Some((ValueExpr::VarDecl(mut declaration), _)) =
                declaration_parse_result.into_output()
            else {
                unreachable!()
            };

            value_expr_into_empty_range(&mut declaration.0.initializer);

            if let Some(type_expr) = &mut declaration.0.type_expr {
                type_expr_into_empty_range(type_expr);
            }

            assert_eq!(declaration.0, expected_output.into());
        }

        let valid_declarations = vec![
            "let x: String = \"\"",
            "let x: { x: String, y: String } = { x: \"\", y: \"\" }",
            "let y: { x: String, y: String } = { x: \"\", y: \"\" }",
            "let z: { h: String, x: { y: String }} = {}",
            "let x: { h: String, x: { y: String }} = 0",
            "let x: { h: String, x: { y: String }} = true",
            "let x: { h: String, x: { y: String }} = false",
            "let x: { h: Int, x: { y: Int }} = { h: 4, x: { y: 8 } }",
            "let x: Int = false",
            "let x: String = \"Hallo, Welt!\"",
            "let x: go `sync.WaitGroup` = {}",
        ];

        for valid_declaration in valid_declarations {
            println!("lexing {valid_declaration}");
            let lexer_parse_result = lex_parser("test", "").parse(valid_declaration);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("declaration parsing {valid_declaration}");
            let typedef_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    #[test]
    pub fn test_assignment_parser() {
        let valid_assignments = vec![
            "y = 1",
            "{y = 1}",
            "while(true){y = 1}",
            "while(true){y = y + 1}",
            "{let y: Int = 0; while(true){y = 1;println(y)}}",
            "x = 580",
            "y = 80",
            "y = true",
            "y = false",
            "y = \"Hallo\"",
        ];

        for valid_assignment in valid_assignments {
            let lexer_parse_result = lex_parser("test", "").parse(valid_assignment);
            assert_eq!(lexer_parse_result.has_errors(), false, "{valid_assignment}");
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_assignment}");
            let typedef_parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }
    }

    fn int(i: i64) -> Box<Spanned<ValueExpr>> {
        ValueExpr::Int(i).into_empty_span().into()
    }

    fn add(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Add(lhs, rhs)
    }

    fn sub(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Sub(lhs, rhs)
    }

    fn mul(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Mul(lhs, rhs)
    }

    fn div(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Div(lhs, rhs)
    }

    fn r#mod(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Mod(lhs, rhs)
    }

    fn eq(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Equals(lhs, rhs)
    }

    fn ne(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::NotEquals(lhs, rhs)
    }

    fn lt(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::LessThan(lhs, rhs)
    }

    fn lte(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::LessThanOrEquals(lhs, rhs)
    }

    fn gt(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::GreaterThan(lhs, rhs)
    }

    fn gte(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::GreaterThanOrEquals(lhs, rhs)
    }

    fn and(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::And(lhs, rhs)
    }

    fn or(lhs: Box<Spanned<ValueExpr>>, rhs: Box<Spanned<ValueExpr>>) -> ValueExpr {
        ValueExpr::Or(lhs, rhs)
    }

    #[test]
    fn test_arithmetic_expressions() {
        let test_cases = vec![
            ("1 + 2", add(int(1), int(2))),
            ("10 - 3", sub(int(10), int(3))),
            ("4 * 5", mul(int(4), int(5))),
            ("20 / 4", div(int(20), int(4))),
            ("21 % 5", r#mod(int(21), int(5))),
            (
                "2 + 3 * 4",
                add(int(2), mul(int(3), int(4)).into_empty_span().into()),
            ),
            (
                "10 - 8 / 2",
                sub(int(10), div(int(8), int(2)).into_empty_span().into()),
            ),
            (
                "1 + 10 % 3",
                add(int(1), r#mod(int(10), int(3)).into_empty_span().into()),
            ),
            (
                "10 - 2 + 3",
                add(sub(int(10), int(2)).into_empty_span().into(), int(3)),
            ),
            (
                "100 / 10 / 2",
                div(div(int(100), int(10)).into_empty_span().into(), int(2)),
            ),
            (
                "10 * 2 / 5 * 3",
                mul(
                    div(mul(int(10), int(2)).into_empty_span().into(), int(5))
                        .into_empty_span()
                        .into(),
                    int(3),
                ),
            ),
            (
                "(2 + 3) * 4",
                mul(add(int(2), int(3)).into_empty_span().into(), int(4)),
            ),
            (
                "2 * (3 + 4)",
                mul(int(2), add(int(3), int(4)).into_empty_span().into()),
            ),
            (
                "(100 - 5 * 3) / 5 + 25 % 6",
                add(
                    div(
                        sub(int(100), mul(int(5), int(3)).into_empty_span().into())
                            .into_empty_span()
                            .into(),
                        int(5),
                    )
                    .into_empty_span()
                    .into(),
                    r#mod(int(25), int(6)).into_empty_span().into(),
                ),
            ),
            (
                "a() + b() * c()",
                add(
                    ValueExpr::FunctionCall {
                        target: var("a"),
                        params: vec![],
                        type_params: None,
                    }
                    .into_empty_span()
                    .into(),
                    mul(
                        ValueExpr::FunctionCall {
                            target: var("b"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span()
                        .into(),
                        ValueExpr::FunctionCall {
                            target: var("c"),
                            params: vec![],
                            type_params: None,
                        }
                        .into_empty_span()
                        .into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "1 + (2 + (3 + 4))",
                add(
                    int(1),
                    add(int(2), add(int(3), int(4)).into_empty_span().into())
                        .into_empty_span()
                        .into(),
                ),
            ),
            (
                "1 * 2 + 3 * 4",
                add(
                    mul(int(1), int(2)).into_empty_span().into(),
                    mul(int(3), int(4)).into_empty_span().into(),
                ),
            ),
        ];

        for (i, (src, expected_ast)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "")
                .parse(src)
                .into_result()
                .unwrap_or_else(|_| panic!("Lexer failed on case {i}: '{src}'"));

            let parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));

            if parse_result.has_errors() {
                panic!(
                    "parse failed for {i}: '{src}'. Errors: {:?}",
                    parse_result.into_errors()
                );
            }

            let mut output = parse_result.into_output().unwrap();

            value_expr_into_empty_range(&mut output);
            assert_eq!(output.0, expected_ast, "unexpected ast {i}: '{src}'");
        }
    }

    #[test]
    fn test_comparison_and_logical_expressions() {
        let test_cases = vec![
            ("1 < 2", lt(int(1), int(2))),
            ("1 <= 1", lte(int(1), int(1))),
            ("5 > 3", gt(int(5), int(3))),
            ("5 >= 5", gte(int(5), int(5))),
            ("1 == 2", eq(int(1), int(2))),
            ("1 != 2", ne(int(1), int(2))),
            (
                "true and false",
                and(
                    ValueExpr::Bool(true).into_empty_span().into(),
                    ValueExpr::Bool(false).into_empty_span().into(),
                ),
            ),
            (
                "true or false",
                or(
                    ValueExpr::Bool(true).into_empty_span().into(),
                    ValueExpr::Bool(false).into_empty_span().into(),
                ),
            ),
            (
                "1 + 2 < 4",
                lt(add(int(1), int(2)).into_empty_span().into(), int(4)),
            ),
            (
                "3 < 4 == true",
                eq(
                    lt(int(3), int(4)).into_empty_span().into(),
                    ValueExpr::Bool(true).into_empty_span().into(),
                ),
            ),
            (
                "true == true and false",
                and(
                    eq(
                        ValueExpr::Bool(true).into_empty_span().into(),
                        ValueExpr::Bool(true).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                    ValueExpr::Bool(false).into_empty_span().into(),
                ),
            ),
            (
                "false or true and true",
                or(
                    ValueExpr::Bool(false).into_empty_span().into(),
                    and(
                        ValueExpr::Bool(true).into_empty_span().into(),
                        ValueExpr::Bool(true).into_empty_span().into(),
                    )
                    .into_empty_span()
                    .into(),
                ),
            ),
            (
                "1 < 2 and 3 > 1",
                and(
                    lt(int(1), int(2)).into_empty_span().into(),
                    gt(int(3), int(1)).into_empty_span().into(),
                ),
            ),
        ];

        for (i, (src, expected_ast)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "")
                .parse(src)
                .into_result()
                .unwrap_or_else(|_| panic!("Lexer failed on case {i}: '{src}'"));

            let parse_result =
                value_expr_parser(make_input).parse(make_input(empty_range(), &lex_result));
            if parse_result.has_errors() {
                panic!(
                    "parse failed for {i}: '{src}'. Errors: {:?}",
                    parse_result.into_errors()
                );
            }

            let mut output = parse_result.into_output().unwrap();

            value_expr_into_empty_range(&mut output);

            assert_eq!(output.0, expected_ast, "ast mismatch on {i}: '{src}'");
        }
    }
}
