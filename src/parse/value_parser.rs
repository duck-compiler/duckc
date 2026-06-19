use crate::{
    Tag,
    parse::{
        Context, SS, Spanned, failure_with_occurence,
        function_parser::LambdaFunctionExpr,
        lexer::{FmtStringContents, HtmlStringContents},
        source_file_parser::SourceFile,
        type_parser::type_expression_parser,
    },
    parse_failure,
    semantics::type_env::TypeEnv,
};

use super::{lexer::Token, type_parser::TypeExpr};
use chumsky::{input::BorrowInput, prelude::*, span::Span};

#[cfg(test)]
#[path = "value_parser_test.rs"]
mod tests;

pub type TypeParam = TypeExpr;

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub type_case: Spanned<TypeExpr>,
    pub base: Option<Spanned<TypeExpr>>,
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
    pub initializer: Option<Spanned<ValueExpr>>,
    pub is_const: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub target: Spanned<ValueExpr>,
    pub value_expr: Spanned<ValueExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    Defer(Box<Spanned<ValueExpr>>),
    Async(Box<Spanned<ValueExpr>>),
    For {
        ident: (String, bool, Option<TypeExpr>),
        target: Box<Spanned<ValueExpr>>,
        block: Box<Spanned<ValueExpr>>,
    },
    FunctionCall {
        target: Box<Spanned<ValueExpr>>,
        params: Vec<Spanned<ValueExpr>>,
        type_params: Vec<Spanned<TypeParam>>,
    },
    Int(u64, Option<Spanned<TypeExpr>>),
    String(String, bool),
    Bool(bool),
    Float(f64),
    Char(char),
    RawVariable(bool, Vec<String>),
    Variable(
        bool,             // is_global
        String,           // name
        Option<TypeExpr>, // type
        Option<bool>,     // constness
        bool,             // needs_copy (for emit)
    ),
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
    As(Box<Spanned<ValueExpr>>, Spanned<TypeExpr>),
    RawStruct {
        is_global: bool,
        name: Vec<String>,
        fields: Vec<(String, Spanned<ValueExpr>)>,
        type_params: Vec<Spanned<TypeParam>>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Spanned<ValueExpr>)>,
        type_params: Vec<Spanned<TypeParam>>,
    },
    FieldAccess {
        target_obj: Box<Spanned<ValueExpr>>,
        field_name: String,
    },
    Array(Vec<Spanned<ValueExpr>>, Option<Spanned<TypeExpr>>),
    Return(Option<Box<Spanned<ValueExpr>>>),
    VarAssign(Box<Spanned<Assignment>>),
    VarDecl(Box<Spanned<Declaration>>),
    Add(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Sub(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Mul(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Div(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Mod(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    BoolNegate(Box<Spanned<ValueExpr>>),
    Negate(Box<Spanned<ValueExpr>>),
    Equals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    NotEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    LessThan(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    LessThanOrEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    GreaterThan(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    GreaterThanOrEquals(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    And(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    Or(Box<Spanned<ValueExpr>>, Box<Spanned<ValueExpr>>),
    InlineGo(String, Option<Spanned<TypeExpr>>),
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
    ShiftLeft {
        target: Box<Spanned<ValueExpr>>,
        amount: Box<Spanned<ValueExpr>>,
    },
    ShiftRight {
        target: Box<Spanned<ValueExpr>>,
        amount: Box<Spanned<ValueExpr>>,
    },
    BitAnd {
        lhs: Box<Spanned<ValueExpr>>,
        rhs: Box<Spanned<ValueExpr>>,
    },
    BitOr {
        lhs: Box<Spanned<ValueExpr>>,
        rhs: Box<Spanned<ValueExpr>>,
    },
    BitXor {
        lhs: Box<Spanned<ValueExpr>>,
        rhs: Box<Spanned<ValueExpr>>,
    },
    BitNot(Box<Spanned<ValueExpr>>),
}

pub trait IntoBlock {
    fn into_block(self) -> Spanned<ValueExpr>;
}

pub trait IntoReturn {
    fn into_return(self) -> Spanned<ValueExpr>;
}

impl IntoReturn for Spanned<ValueExpr> {
    fn into_return(self) -> Spanned<ValueExpr> {
        let cl = self.1;
        (ValueExpr::Return(Some(self.into())), cl)
    }
}

impl IntoBlock for Spanned<ValueExpr> {
    fn into_block(self) -> Spanned<ValueExpr> {
        let cl = self.1;
        (ValueExpr::Block(vec![self]), cl)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NeverReturnAnalysisResult {
    Global, // entire function / Program
    Local,  // loop scope
    Partial,
    None,
}

pub fn value_expr_returns_never(
    v: &Spanned<ValueExpr>,
    is_in_loop: bool,
) -> NeverReturnAnalysisResult {
    let span = v.1;
    use NeverReturnAnalysisResult::*;
    match &v.0 {
        ValueExpr::Return(..) => Global,
        ValueExpr::Continue | ValueExpr::Break => {
            if !is_in_loop {
                let msg = "Can only use this in a loop";
                failure_with_occurence(msg, span, [(msg, span)]);
            }
            Local
        }

        ValueExpr::While { condition, body } => {
            let condition_res = value_expr_returns_never(condition, is_in_loop);
            if condition_res != None {
                return condition_res;
            }

            if value_expr_returns_never(body, true) != None {
                Partial
            } else {
                None
            }
        }

        ValueExpr::For {
            ident: _,
            target,
            block,
        } => {
            let target_res = value_expr_returns_never(target, is_in_loop);
            if target_res != None {
                return target_res;
            }

            let block_res = value_expr_returns_never(block, true);
            return block_res;
        }
        _ => None,
    }
}

impl ValueExpr {
    pub fn into_empty_span(self) -> Spanned<ValueExpr> {
        (self, empty_range())
    }

    pub fn into_empty_span_and_block(self) -> Spanned<ValueExpr> {
        self.into_empty_span().into_block()
    }

    pub fn into_empty_span_and_block_and_return(self) -> Spanned<ValueExpr> {
        self.into_empty_span().into_block().into_return()
    }

    pub fn needs_semicolon(&self) -> bool {
        match self {
            ValueExpr::Negate(..) => true,
            ValueExpr::RawStruct { .. } => true,
            ValueExpr::Async(..) => true,
            ValueExpr::Defer(..) => true,
            ValueExpr::As(..) => true,
            ValueExpr::For { .. } => false,
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
            ValueExpr::InlineGo(..) => false,
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
            | ValueExpr::BitAnd { .. }
            | ValueExpr::BitOr { .. }
            | ValueExpr::BitXor { .. }
            | ValueExpr::ShiftLeft { .. }
            | ValueExpr::ShiftRight { .. }
            | ValueExpr::BitNot(..)
            | ValueExpr::FunctionCall { .. } => true,
        }
    }
}

pub fn block_expr_parser<'src, I, M>(
    _make_input: M,
    value_expr_parser: impl Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token, SS>>>
    + Clone
    + 'src,
) -> impl Parser<'src, I, Spanned<ValueExpr>, extra::Err<Rich<'src, Token, SS>>> + Clone + 'src
where
    I: BorrowInput<'src, Token = Token, Span = SS>,
    M: Fn(SS, &'src [Spanned<Token>]) -> I + Clone + 'static,
{
    value_expr_parser
        .clone()
        .then(just(Token::ControlChar(';')).or_not())
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
        .map_with(|mut exprs, e| {
            if exprs.len() >= 2 {
                for (expr, has_semi) in &exprs[..exprs.len() - 1] {
                    if expr.0.needs_semicolon() && has_semi.is_none() {
                        failure_with_occurence(
                            "This expression needs a semicolon",
                            expr.1,
                            [(
                                "This expression needs a semicolon at the end".to_string(),
                                expr.1,
                            )],
                        );
                    }
                }
            }

            if !exprs.is_empty() && exprs.last().unwrap().1.is_some() {
                exprs.push(((empty_tuple(), exprs.last().unwrap().0.1), None));
            }

            (
                ValueExpr::Block(exprs.into_iter().map(|(expr, _)| expr).collect()),
                e.span(),
            )
        })
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
            let block_expression = block_expr_parser(make_input.clone(), value_expr_parser.clone());
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
                let params_parser =
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .then(
                            (just(Token::ControlChar(':')))
                                .ignore_then(type_expression_parser())
                                .or_not(),
                        )
                        .separated_by(just(Token::ControlChar(',')))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .boxed();

                let return_type_parser = just(Token::ThinArrow)
                    .ignore_then(type_expression_parser())
                    .boxed();

                just(Token::Mut)
                    .or_not()
                    .then(
                        just(Token::Function)
                            .ignore_then(just(Token::ControlChar('(')))
                            .ignore_then(params_parser)
                            .then_ignore(just(Token::ControlChar(')')))
                            .then(return_type_parser.or_not())
                            .then(block_expression.clone()),
                    )
                    .map(|(is_mut, ((params, return_type), value_expr))| {
                        ValueExpr::Lambda(
                            LambdaFunctionExpr {
                                is_mut: is_mut.is_some(),
                                params,
                                return_type,
                                value_expr: (
                                    ValueExpr::Return(Some(value_expr.clone().into())),
                                    value_expr.1,
                                ),
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
                    base: None,
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
                    base: None,
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

            let declaration = choice((just(Token::Let), just(Token::Const)))
                .then(
                    select_ref! { Token::Ident(identifier) => identifier.to_string() }
                        .map_with(|x, e| (x, e.span())),
                )
                .then(declare_type)
                .then(initializer.or_not())
                .map_with(
                    |(((let_or_const, (ident, _)), type_expr), initializer), e| {
                        (
                            ValueExpr::VarDecl(
                                (
                                    Declaration {
                                        name: ident,
                                        type_expr,
                                        initializer: initializer.clone(),
                                        is_const: matches!(let_or_const, Token::Const),
                                    },
                                    initializer.as_ref().map(|obj| obj.1).unwrap_or(e.span()),
                                )
                                    .into(),
                            ),
                            e.span(),
                        )
                    },
                )
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

            let struct_expression = just(Token::ScopeRes)
                .or_not()
                .map(|x| x.is_some())
                .then(
                    select_ref! { Token::Ident(identifier) => identifier.clone() }
                        .separated_by(just(Token::ScopeRes))
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
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
                .map(
                    |(((is_global, identifier), generics), values)| ValueExpr::RawStruct {
                        is_global,
                        name: identifier,
                        fields: values,
                        type_params: generics.unwrap_or_default(),
                    },
                )
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let for_parser = just(Token::For)
                .ignore_then(
                    just(Token::Mut)
                        .or_not()
                        .then(select_ref! { Token::Ident(ident) => ident.to_owned() })
                        .then_ignore(just(Token::In))
                        .then(value_expr_parser.clone())
                        .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')'))),
                )
                .then(block_expression.clone())
                .map(|(((is_mut, ident), target), block)| ValueExpr::For {
                    ident: (ident, is_mut.is_none(), None),
                    target: Box::new(target),
                    block: Box::new(block),
                })
                .map_with(|x, e| (x, e.span()));

            let duck_expression = select_ref! { Token::Ident(ident) => ident.to_owned() }
                .then_ignore(just(Token::ControlChar(':')))
                .then(value_expr_parser.clone())
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('{')), just(Token::ControlChar('}')))
                .filter(|x| !x.is_empty())
                .map(|mut x| {
                    x.sort_by_key(|(name, _)| name.clone());
                    ValueExpr::Duck(x)
                })
                .map_with(|x, e| (x, e.span()))
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

            let int = select_ref! { Token::IntLiteral(i) => *i }
                .map(|x| ValueExpr::Int(x, None))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let bool_val = select_ref! { Token::BoolLiteral(b) => *b }
                .map(ValueExpr::Bool)
                .map_with(|x, e| (x, e.span()))
                .boxed();
            let string_val = select_ref! { Token::StringLiteral(s) => s.to_owned() }
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
                            let span = then.1;
                            Some(Box::new((
                                ValueExpr::If {
                                    condition: Box::new(cond),
                                    then: Box::new(then),
                                    r#else: acc,
                                },
                                span,
                            )))
                        },
                    ),
                })
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let char_expr = select_ref! { Token::CharLiteral(c) => *c }
                .map(ValueExpr::Char)
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let float_expr = select_ref! { Token::IntLiteral(num) => *num }
                .then_ignore(just(Token::ControlChar('.')))
                .then(select_ref! { Token::IntLiteral(num) => *num })
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
                                        let span = SS {
                                            start: s[0].1.start,
                                            end: s.last().unwrap().1.end,
                                            context: s[0].1.context,
                                        };
                                        if !s.is_empty() {
                                            let (expr, expr_errors) = value_expr_parser
                                                .parse(make_input(span, s.as_slice()))
                                                .into_output_errors();

                                            expr_errors.into_iter().for_each(|e| {
                                                parse_failure(
                                                    span.context.file_name,
                                                    &Rich::<&str, SS>::custom(
                                                        SS {
                                                            start: e.span().start,
                                                            end: e.span().end,
                                                            context: Context {
                                                                file_name: span.context.file_name,
                                                                file_contents: span
                                                                    .context
                                                                    .file_contents,
                                                            },
                                                        },
                                                        format!(
                                                            "{}{} {}",
                                                            Tag::Parser,
                                                            Tag::Err,
                                                            e.reason()
                                                        ),
                                                    ),
                                                    span.context.file_contents,
                                                );
                                            });

                                            res.push(ValFmtStringContents::Expr(expr.unwrap()));
                                        }
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
                    let cl = x.clone();

                    value_expr_parser
                        .parse(make_input(empty_range(), x.leak()))
                        .into_result()
                        .unwrap_or_else(|e| panic!("invavlid code {cl:?} {e:?}"))
                }
            });
            // .map_with(|x, e| (x, e.span()));

            let array = value_expr_parser
                .clone()
                .separated_by(just(Token::ControlChar(',')))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::ControlChar('[')), just(Token::ControlChar(']')))
                .map(|v| ValueExpr::Array(v, None))
                .map_with(|x, e| (x, e.span()));

            #[derive(Debug, Clone)]
            enum AtomPreParseUnit {
                Ref,
                RefMut,
                BoolNegate,
                Deref,
                Negate,
                BitNegate,
            }

            let inline_go = select_ref! { Token::InlineGo(x) => x.to_owned() }
                .map(|s| ValueExpr::InlineGo(s, None))
                .map_with(|x, e| (x, e.span()))
                .boxed();

            let atom = choice((
                just(Token::ControlChar('&')).map(|_| AtomPreParseUnit::Ref),
                just(Token::RefMut).map(|_| AtomPreParseUnit::RefMut),
                just(Token::ControlChar('!')).map(|_| AtomPreParseUnit::BoolNegate),
                just(Token::ControlChar('*')).map(|_| AtomPreParseUnit::Deref),
                just(Token::ControlChar('-')).map(|_| AtomPreParseUnit::Negate),
                just(Token::ControlChar('~')).map(|_| AtomPreParseUnit::BitNegate),
            ))
            .repeated()
            .collect::<Vec<_>>()
            .then(
                value_expr_parser
                    .clone()
                    .delimited_by(just(Token::ControlChar('(')), just(Token::ControlChar(')')))
                    .or(choice((
                        array.clone(),
                        inline_go,
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
                        block_expression.clone(),
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
                                (
                                    ValueExpr::Return(Some(Box::new(
                                        x.unwrap_or((ValueExpr::Tuple(vec![]), e.span())),
                                    ))),
                                    e.span(),
                                )
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
                                .or(select_ref! { Token::IntLiteral(i) => i.to_string() }),
                        )
                        .map(AtomPostParseUnit::FieldAccess),
                ))
                .map_with(|x, e| (x, e.span()))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|((pre, target), params)| {
                let target = params.into_iter().fold(target, |acc, (x, span)| {
                    let span = SS {
                        start: acc.1.start,
                        end: span.end,
                        context: span.context,
                    };

                    match x {
                        AtomPostParseUnit::ArrayAccess(idx_expr) => {
                            (ValueExpr::ArrayAccess(acc.into(), idx_expr.into()), span)
                        }
                        AtomPostParseUnit::FuncCall(params, type_params) => (
                            ValueExpr::FunctionCall {
                                target: acc.clone().into(),
                                params,
                                type_params: type_params.unwrap_or_default(),
                            },
                            span,
                        ),
                        AtomPostParseUnit::FieldAccess(field_name) => (
                            ValueExpr::FieldAccess {
                                target_obj: acc.into(),
                                field_name,
                            },
                            span,
                        ),
                    }
                });

                pre.into_iter()
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
                                AtomPreParseUnit::Negate => {
                                    ValueExpr::Negate((acc_expr, acc_span).into())
                                }
                                AtomPreParseUnit::BitNegate => {
                                    ValueExpr::BitNot((acc_expr, acc_span).into())
                                }
                            },
                            acc_span,
                        )
                    })
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
                                    ..
                                },
                                s,
                            ) = elem
                            else {
                                panic!("can only pen functions")
                            };

                            params.insert(0, acc.clone());
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

            let assignment = atom
                .clone()
                .then(choice((
                    just(Token::ControlChar('=')),
                    just(Token::PlusEquals),
                    just(Token::SubEquals),
                    just(Token::MulEquals),
                    just(Token::DivEquals),
                    just(Token::ModEquals),
                    just(Token::ShiftLeftEquals),
                    just(Token::ShiftRightEquals),
                )))
                .then(value_expr_parser.clone())
                .map_with(|((target, op), value_expr), e| {
                    ValueExpr::VarAssign(
                        (
                            Assignment {
                                target: target.clone(),
                                value_expr: match op {
                                    Token::ControlChar('=') => value_expr.clone(),
                                    Token::PlusEquals => (
                                        ValueExpr::Add(
                                            target.clone().into(),
                                            value_expr.clone().into(),
                                        ),
                                        value_expr.1,
                                    ),
                                    Token::SubEquals => (
                                        ValueExpr::Sub(
                                            target.clone().into(),
                                            value_expr.clone().into(),
                                        ),
                                        value_expr.1,
                                    ),
                                    Token::MulEquals => (
                                        ValueExpr::Mul(
                                            target.clone().into(),
                                            value_expr.clone().into(),
                                        ),
                                        value_expr.1,
                                    ),
                                    Token::DivEquals => (
                                        ValueExpr::Div(
                                            target.clone().into(),
                                            value_expr.clone().into(),
                                        ),
                                        value_expr.1,
                                    ),
                                    Token::ModEquals => (
                                        ValueExpr::Mod(
                                            target.clone().into(),
                                            value_expr.clone().into(),
                                        ),
                                        value_expr.1,
                                    ),
                                    Token::ShiftLeftEquals => (
                                        ValueExpr::ShiftLeft {
                                            target: target.clone().into(),
                                            amount: value_expr.clone().into(),
                                        },
                                        value_expr.1,
                                    ),
                                    Token::ShiftRightEquals => (
                                        ValueExpr::ShiftRight {
                                            target: target.clone().into(),
                                            amount: value_expr.clone().into(),
                                        },
                                        value_expr.1,
                                    ),
                                    _ => panic!("Compiler Bug: invalid assign op {op:?}"),
                                },
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
                        just(Token::ControlChar('&')),
                        just(Token::ControlChar('|')),
                        just(Token::ControlChar('^')),
                        just(Token::ControlChar('>')).ignore_then(just(Token::ControlChar('>'))),
                        just(Token::ControlChar('<')).ignore_then(just(Token::ControlChar('<'))),
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
                            Token::ControlChar('&') => ValueExpr::BitAnd {
                                lhs: Box::new(acc),
                                rhs: Box::new(x),
                            },
                            Token::ControlChar('|') => ValueExpr::BitOr {
                                lhs: Box::new(acc),
                                rhs: Box::new(x),
                            },
                            Token::ControlChar('^') => ValueExpr::BitXor {
                                lhs: Box::new(acc),
                                rhs: Box::new(x),
                            },
                            Token::ControlChar('<') => ValueExpr::ShiftLeft {
                                target: Box::new(acc),
                                amount: Box::new(x),
                            },
                            Token::ControlChar('>') => ValueExpr::ShiftRight {
                                target: Box::new(acc),
                                amount: Box::new(x),
                            },
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

            let defer = just(Token::Defer)
                .ignore_then(value_expr_parser.clone())
                .map(|b| ValueExpr::Defer(Box::new(b)))
                .map_with(|x, e| (x, e.span()));

            let async_call = just(Token::Async)
                .ignore_then(value_expr_parser.clone())
                .map(|b| ValueExpr::Async(Box::new(b)))
                .map_with(|x, e| (x, e.span()));

            let casted = choice((assignment, or, declaration, pen, atom))
                .then(
                    just(Token::As)
                        .ignore_then(type_expression_parser())
                        .or_not(),
                )
                .map(|(v, t)| {
                    t.map(|t| ValueExpr::As(Box::new(v.clone()), t))
                        .unwrap_or(v.0)
                })
                .map_with(|x, e| (x, e.span()));

            choice((casted, defer, async_call, for_parser))
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
fn empty_block() -> ValueExpr {
    ValueExpr::Block(Vec::new())
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
        type_expr_into_empty_range(&mut x.return_type);
        for (_, p) in &mut x.params {
            type_expr_into_empty_range(p);
        }
    }
    for x in &mut v.type_definitions {
        type_expr_into_empty_range(&mut x.type_expression);
    }
    for x in &mut v.sub_modules {
        source_file_into_empty_range(&mut x.1);
    }

    for schema_def in &mut v.schema_defs {
        schema_def.span = empty_range();
        for field in &mut schema_def.fields {
            field.span = empty_range();
            type_expr_into_empty_range(&mut field.type_expr);
            if let Some(if_branch) = &mut field.if_branch {
                if_branch.1 = empty_range();
                value_expr_into_empty_range(&mut if_branch.0.condition);
                if let Some(value_expr) = &mut if_branch.0.value_expr {
                    value_expr_into_empty_range(value_expr);
                }
            }

            if let Some(else_branch_value_expr) = &mut field.else_branch_value_expr {
                value_expr_into_empty_range(else_branch_value_expr);
            }
        }
    }

    for x in &mut v.jsx_components {
        x.javascript_source.1 = empty_range()
    }
}

pub fn type_expr_into_empty_range(t: &mut Spanned<TypeExpr>) {
    use crate::semantics::type_resolve::trav_type_expr;
    trav_type_expr(
        |t, _| {
            t.1 = empty_range();
        },
        t,
        &mut TypeEnv::default(),
    );
}

pub fn value_expr_into_empty_range(v: &mut Spanned<ValueExpr>) {
    use crate::semantics::type_resolve::trav_value_expr;
    trav_value_expr(
        |t, _| {
            t.1 = empty_range();
        },
        |v, _| {
            v.1 = empty_range();
            match &mut v.0 {
                ValueExpr::VarAssign(a) => a.1 = empty_range(),
                ValueExpr::VarDecl(a) => a.1 = empty_range(),
                ValueExpr::Match {
                    value_expr: _,
                    arms,
                    else_arm,
                    span,
                } => {
                    *span = empty_range();
                    for arm in arms {
                        arm.span = empty_range();
                    }
                    if let Some(else_arm) = else_arm.as_mut() {
                        else_arm.span = empty_range();
                    }
                }
                _ => {}
            }
        },
        v,
        &mut TypeEnv::default(),
    );
}
