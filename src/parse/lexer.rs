use chumsky::{prelude::*, text::whitespace};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Type,
    Duck,
    Function,
    Ident(String),
    ControlChar(char),
}

pub type Spanned<T> = (T, SimpleSpan);

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>> {
    let ty = just("type").then_ignore(whitespace().at_least(1)).to(Token::Type);
    let duck = just("duck").to(Token::Duck);
    let function_keyword = just("fun").to(Token::Function);
    let ident = text::ident().map(|str: &str| Token::Ident(str.to_string()));
    let ctrl = one_of("=:{};,&()->").map(Token::ControlChar);

    let token = ty.or(duck).or(function_keyword).or(ident).or(ctrl);

    token.padded()
        .repeated()
        .collect::<Vec<Token>>()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex() {
        let test_cases = vec![
            ("type Y = duck {};", vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ]),
            ("typeY=duck{};", vec![
                Token::Ident("typeY".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ]),
            ("type Y = duck {} & duck {};", vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar('&'),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ]),
            ("type Y = duck { x: String, y: String };", vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar(','),
                Token::Ident("y".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ]),
            ("()", vec![Token::ControlChar('('), Token::ControlChar(')')]),
            ("->", vec![Token::ControlChar('-'), Token::ControlChar('>')]),
            ("fun", vec![Token::Function])
        ];

        for (src, expected_tokens) in test_cases {
            let parse_result = lexer().parse(src);

            assert_eq!(parse_result.has_errors(), false);
            assert_eq!(parse_result.has_output(), true);

            let output: Vec<Token> = parse_result.output()
                .unwrap()
                .iter()
                .map(|token| token.clone())
                .collect();

            assert_eq!(output, expected_tokens);
        }
    }
}
