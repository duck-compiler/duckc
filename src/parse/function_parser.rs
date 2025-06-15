use chumsky::prelude::*;

use super::{lexer::Token, type_parser::{type_expression_parser, TypeExpression}};

type Param = (String, TypeExpression);

#[derive(Debug, Clone)]
struct FunctionDefintion {
    name: String,
    return_type: Option<TypeExpression>,
    params: Option<Vec<Param>>,
}

fn function_definition_parser<'src>() -> impl Parser<'src, &'src [Token], FunctionDefintion> {
    let param_parser = select_ref! { Token::Ident(identifier) => identifier.to_string() }
        .then_ignore(just(Token::ControlChar(':')))
        .then(type_expression_parser())
        .map(|(identifier, type_expr)| (identifier, type_expr) as Param);

    let params_parser = param_parser.separated_by(just(Token::ControlChar(',')))
        .allow_trailing()
        .collect::<Vec<Param>>()
        .or_not();

    let return_type_parser = just(Token::ControlChar('-')).ignore_then(just(Token::ControlChar('>')))
        .ignore_then(type_expression_parser());

    just(Token::Function)
        .ignore_then(select_ref! { Token::Ident(identifier) => identifier.to_string() })
        .then_ignore(just(Token::ControlChar('(')))
        .then(params_parser)
        .then_ignore(just(Token::ControlChar(')')))
        .then(return_type_parser.or_not())
        .then_ignore(just(Token::ControlChar('{')))
        .then_ignore(just(Token::ControlChar('}')))
        .map(|((identifier, params), type_expr)| FunctionDefintion { name: identifier, return_type: type_expr, params: params })
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
        ];

        for valid_function_definition in valid_function_definitions {
            println!("lexing {valid_function_definition}");
            let lexer_parse_result = lexer().parse(valid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_function_definition}");
            let typedef_parse_result = function_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_function_definitions = vec![
        ];

        for invalid_function_definition in invalid_function_definitions {
            println!("lexing {invalid_function_definition}");
            let lexer_parse_result = lexer().parse(invalid_function_definition);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {invalid_function_definition}");
            let typedef_parse_result = function_definition_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
