use chumsky::prelude::*;

use crate::parse::value_parser::{EmitEnvironment, emit};

use super::{
    lexer::Token,
    type_parser::{TypeExpression, type_expression_parser},
    value_parser::{ValueExpr, value_expr_parser},
};

pub type Param = (String, TypeExpression);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefintion {
    pub name: String,
    pub return_type: Option<TypeExpression>,
    pub params: Option<Vec<Param>>,
    pub value_expr: ValueExpr,
}

impl Default for FunctionDefintion {
    fn default() -> Self {
        FunctionDefintion {
            name: Default::default(),
            return_type: Default::default(),
            params: Some(Default::default()),
            value_expr: ValueExpr::Block(vec![ValueExpr::Tuple(vec![])]),
        }
    }
}

impl FunctionDefintion {
    pub fn emit(&self, env: EmitEnvironment) -> String {
        let (mut emitted_body, res_name) = emit(self.value_expr.clone(), env.clone());
        if let Some(res_name) = res_name {
            emitted_body.push(format!("_ = {res_name}\n"));
        }
        let body = emitted_body.join("");
        [
            format!(
                "func {}({}) {} {}\n",
                self.name,
                self.params
                    .as_ref()
                    .map(|x| x
                        .iter()
                        .map(|x| format!("{} {}", x.0, x.1.emit().0))
                        .collect::<Vec<_>>()
                        .join(", "))
                    .unwrap_or_default(),
                self.return_type
                    .as_ref()
                    .map(|x| x.emit().0)
                    .unwrap_or_default(),
                "{"
            ),
            body,
            "}\n".to_string(),
        ]
        .join("")
    }
}

// TODO body
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct LambdaFunctionExpr {
    pub params: Vec<Param>,
    pub return_type: Option<TypeExpression>,
    pub value_expr: ValueExpr,
}

pub fn lambda_function_expr_parser<'src>() -> impl Parser<'src, &'src [Token], LambdaFunctionExpr> {
    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>()
        .or_not();

    let return_type_parser = just(Token::ControlChar('-'))
        .ignore_then(just(Token::ControlChar('>')))
        .ignore_then(type_expression_parser());

    just(Token::ControlChar('('))
        .ignore_then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then_ignore(just(Token::ControlChar('=')))
        .then_ignore(just(Token::ControlChar('>')))
        .then(value_expr_parser())
        .map(|((params, return_type), value_expr)| LambdaFunctionExpr {
            params: params.unwrap_or_default(),
            return_type,
            value_expr,
        })
}

pub fn function_definition_parser<'src>() -> impl Parser<'src, &'src [Token], FunctionDefintion> {
    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser
        .separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>()
        .or_not();

    let return_type_parser = just(Token::ControlChar('-'))
        .ignore_then(just(Token::ControlChar('>')))
        .ignore_then(type_expression_parser());

    just(Token::Function)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('(')))
        .then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then(value_expr_parser())
        .map(|(((identifier, params), return_type), mut value_expr)| {
            value_expr = match value_expr {
                ValueExpr::Duck(x) if x.is_empty() => {
                    ValueExpr::Block(vec![ValueExpr::Tuple(vec![])])
                }
                x @ ValueExpr::InlineGo(_) => ValueExpr::Block(vec![x]),
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

#[cfg(test)]
pub mod tests {
    use crate::parse::lexer::lexer;

    use super::*;

    #[test]
    fn test_function_parser() {
        let valid_function_definitions = vec![
            "fun x(){}",
            "fun x(x: String){}",
            "fun x(x: { hallo: String, x: { y: {} }}){}",
            "fun x() -> String {}",
            "fun x() -> {x: String} {}",
            "fun x() -> {x: String} { 5; }",
            "fun x() -> {x: String} { 5; }",
        ];

        for valid_function_definition in valid_function_definitions {
            println!("lexing {valid_function_definition}");
            let lexer_parse_result = lexer().parse(valid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {valid_function_definition}");
            let typedef_parse_result = function_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_function_definitions = vec![];

        for invalid_function_definition in invalid_function_definitions {
            println!("lexing {invalid_function_definition}");
            let lexer_parse_result = lexer().parse(invalid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_function_definition}");
            let typedef_parse_result = function_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }

    #[test]
    fn test_lambda_function_defintion() {
        let valid_lambda_function_definitions =
            vec!["() => {}", "() -> String => {}", "() => { 5; }"];

        for valid_lambda_function_definition in valid_lambda_function_definitions {
            println!("lexing {valid_lambda_function_definition}");
            let lexer_parse_result = lexer().parse(valid_lambda_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("lambda parsing {valid_lambda_function_definition}");
            let lambda_parse_result = lambda_function_expr_parser().parse(tokens.as_slice());
            assert_eq!(lambda_parse_result.has_errors(), false);
            assert_eq!(lambda_parse_result.has_output(), true);
        }

        let invalid_function_definitions = vec![];

        for invalid_function_definition in invalid_function_definitions {
            println!("lexing {invalid_function_definition}");
            let lexer_parse_result = lexer().parse(invalid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else {
                unreachable!()
            };

            println!("typedef_parsing {invalid_function_definition}");
            let typedef_parse_result = function_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
