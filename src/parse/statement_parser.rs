use chumsky::prelude::*;

use super::{function_parser::{function_definition_parser, FunctionDefintion}, lexer::Token, type_parser::{type_definition_parser, TypeDefinition}, value_parser::{value_expr_parser, ValueExpr}};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Return(ValueExpr),
    ValueExpr(ValueExpr),
    TypeDefinition(TypeDefinition),
    FunctionDefinition(FunctionDefintion),
}

pub fn statement_parser<'src>() -> impl Parser<'src, &'src [Token], Statement> {
    let return_statement = just(Token::Return)
        .ignore_then(value_expr_parser())
        .then_ignore(just(Token::ControlChar(';')))
        .map(|return_value_expr| Statement::Return(return_value_expr));

    let type_definition = type_definition_parser().map(|type_definition| Statement::TypeDefinition(type_definition));
    let function_definition = function_definition_parser().map(|function_definition| Statement::FunctionDefinition(function_definition));

    choice((
        return_statement,
        type_definition,
        function_definition,
        value_expr_parser().map(|value_expr| Statement::ValueExpr(value_expr))
    ))
}

#[cfg(test)]
pub mod tests {
    use crate::parse::lexer::lexer;

    use super::*;

    #[test]
    pub fn test_statement_parser() {
        let valid_statements = vec![
            "return 5;",
            "type X = {};",
            "fun y() -> String {}",
        ];

        for valid_statement in valid_statements {
            println!("lexing {valid_statement}");
            let lexer_parse_result = lexer().parse(valid_statement);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {valid_statement}");
            let typedef_parse_result = statement_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), false);
            assert_eq!(typedef_parse_result.has_output(), true);
        }

        let invalid_statements = vec![
            "return 5", // missing semicolon
        ];

        for invalid_statement in invalid_statements {
            println!("lexing {invalid_statement}");
            let lexer_parse_result = lexer().parse(invalid_statement);
            assert_eq!(lexer_parse_result.has_errors(), false);
            assert_eq!(lexer_parse_result.has_output(), true);

            let Some(tokens) = lexer_parse_result.into_output() else { unreachable!()};

            println!("typedef_parsing {invalid_statement}");
            let typedef_parse_result = statement_parser().parse(tokens.as_slice());
            assert_eq!(typedef_parse_result.has_errors(), true);
            assert_eq!(typedef_parse_result.has_output(), false);
        }
    }
}
