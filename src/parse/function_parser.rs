use chumsky::{input::BorrowInput, prelude::*};

use crate::{
    parse::{
        SS, Spanned,
        generics_parser::{Generic, generics_parser},
    },
    semantics::type_resolve::FunHeader,
};

use super::{
    lexer::Token,
    type_parser::{TypeExpr, type_expression_parser},
    value_parser::{ValueExpr, value_expr_parser},
};

pub type Param = (String, Spanned<TypeExpr>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefintion {
    pub name: String,
    pub return_type: Option<Spanned<TypeExpr>>,
    pub params: Option<Vec<Param>>,
    pub value_expr: Spanned<ValueExpr>,
    pub generics: Option<Vec<Spanned<Generic>>>,
}

impl FunctionDefintion {
    pub fn to_header(&self) -> FunHeader {
        FunHeader {
            params: self
                .params
                .iter()
                .flat_map(|x| x.iter())
                .map(|x| x.1.clone())
                .collect(),
            return_type: self.return_type.clone(),
        }
    }

    pub fn type_expr(&self) -> Spanned<TypeExpr> {
        // todo: retrieve correct span for function defintions typeexpr
        return (
            TypeExpr::Fun(
                self.params
                    .clone()
                    .or_else(|| Some(Vec::new()))
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|(name, type_expr)| (Some(name.to_owned()), type_expr.to_owned()))
                    .collect::<Vec<_>>(),
                self.return_type.clone().map(Box::new),
            ),
            self.value_expr.1,
        );
    }
}

impl Default for FunctionDefintion {
    fn default() -> Self {
        FunctionDefintion {
            name: Default::default(),
            return_type: None,
            params: Some(Default::default()),
            value_expr: ValueExpr::Block(vec![]).into_empty_span(),
            generics: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaFunctionExpr {
    pub params: Vec<Param>,
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
    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>()
        .or_not();

    let return_type_parser = just(Token::ThinArrow).ignore_then(type_expression_parser());

    just(Token::Sus)
        .or_not()
        .then_ignore(just(Token::Function))
        .then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then(generics_parser().or_not())
        .then_ignore(just(Token::ControlChar('(')))
        .then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then(value_expr_parser(make_input))
        .map(
            |(((((has_sus, identifier), generics), params), return_type), mut value_expr)| {
                let is_sus = has_sus.is_some();

                if is_sus && return_type.is_some() {
                    panic!("sus function is not allowed to return something");
                }

                value_expr = match value_expr {
                    (ValueExpr::Duck(x), loc) if x.is_empty() => (ValueExpr::Block(vec![]), loc),
                    x @ (ValueExpr::Block(_), _) => x,
                    _ => panic!("Function must be block"),
                };

                if is_sus {
                    value_expr = (
                        ValueExpr::FunctionCall {
                            target: ValueExpr::RawVariable(
                                true,
                                vec!["std".into(), "task".into(), "spawn".into()],
                            )
                            .into_empty_span()
                            .into(),
                            params: vec![
                                ValueExpr::Lambda(
                                    LambdaFunctionExpr {
                                        params: vec![],
                                        return_type: None,
                                        value_expr: value_expr.clone(),
                                    }
                                    .into(),
                                )
                                .into_empty_span(),
                            ],
                            type_params: None,
                        },
                        value_expr.1,
                    );
                }

                FunctionDefintion {
                    name: identifier,
                    return_type,
                    params,
                    value_expr,
                    generics,
                }
            },
        )
}

#[cfg(test)]
pub mod tests {
    use crate::parse::{lexer::lex_parser, make_input, value_parser::empty_range};

    use super::*;

    #[test]
    fn test_function_parser() {
        let valid_function_definitions = vec![
            "fn x(){}",
            "fn x(x: String){}",
            "fn x(x: { hallo: String, x: { y: {} }}){}",
            "fn x() -> String {}",
            "fn x() -> {x: String} {}",
            "fn x() -> {x: String} { 5; }",
            "fn x() -> {x: String} { 5; }",
            "fn x<TYPE>() -> {x: String} { 5; }",
            "fn x<TYPE, TYPE2>() -> {x: String} { 5; }",
            "fn x<TYPE, TYPE2, TYPE3>() -> {x: String} { 5; }",
        ];

        for valid_function_definition in valid_function_definitions {
            println!("lexing {valid_function_definition}");
            let lexer_parse_result = lex_parser("test", "").parse(valid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_function_definition}");
            let typedef_parse_result =
                function_definition_parser(make_input).parse(make_input(empty_range(), &tokens));
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_function_definitions = vec![];

        for invalid_function_definition in invalid_function_definitions {
            println!("lexing {invalid_function_definition}");
            let lexer_parse_result = lex_parser("test", "").parse(invalid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_function_definition}");
            let typedef_parse_result = function_definition_parser(make_input)
                .parse(make_input(empty_range(), tokens.as_slice()));
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }

    #[test]
    fn test_detailed_function_definitions() {
        let test_cases = vec![
            (
                "fn y<TYPENAME>() {}",
                FunctionDefintion {
                    name: "y".to_string(),
                    params: Some(vec![]),
                    return_type: None,
                    generics: Some(vec![(
                        Generic {
                            name: "TYPENAME".to_string(),
                        },
                        empty_range(),
                    )]),
                    value_expr: ValueExpr::Block(vec![]).into_empty_span(),
                },
            ),
            (
                "fn y<TYPENAME, TYPENAME2>() {}",
                FunctionDefintion {
                    name: "y".to_string(),
                    params: Some(vec![]),
                    return_type: None,
                    generics: Some(vec![
                        (
                            Generic {
                                name: "TYPENAME".to_string(),
                            },
                            empty_range(),
                        ),
                        (
                            Generic {
                                name: "TYPENAME2".to_string(),
                            },
                            empty_range(),
                        ),
                    ]),
                    value_expr: ValueExpr::Block(vec![]).into_empty_span(),
                },
            ),
            (
                "fn y<TYPENAME, TYPENAME2, TYPENAME3>() {}",
                FunctionDefintion {
                    name: "y".to_string(),
                    params: Some(vec![]),
                    return_type: None,
                    generics: Some(vec![
                        (
                            Generic {
                                name: "TYPENAME".to_string(),
                            },
                            empty_range(),
                        ),
                        (
                            Generic {
                                name: "TYPENAME2".to_string(),
                            },
                            empty_range(),
                        ),
                        (
                            Generic {
                                name: "TYPENAME3".to_string(),
                            },
                            empty_range(),
                        ),
                    ]),
                    value_expr: ValueExpr::Block(vec![]).into_empty_span(),
                },
            ),
        ];

        for (i, (src, expected_fns)) in test_cases.into_iter().enumerate() {
            let lex_result = lex_parser("test", "").parse(src).into_result().expect(&src);
            let parse_result = function_definition_parser(make_input)
                .parse(make_input(empty_range(), &lex_result));

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

            output
                .generics
                .as_mut()
                .unwrap()
                .iter_mut()
                .for_each(|generic| {
                    *generic = (generic.0.clone(), empty_range());
                });

            output.value_expr = ValueExpr::Block(vec![]).into_empty_span();

            assert_eq!(output, expected_fns, "{i}: {}", src);
        }
    }
}
