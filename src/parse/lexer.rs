use chumsky::{prelude::*, text::whitespace};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Type,
    Duck,
    Ident(String),
    ControlChar(char),
    StringLiteral(String),
    IntLiteral(i64),
}

pub type Spanned<T> = (T, SimpleSpan);

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>> {
    let ty = just("type").then_ignore(whitespace().at_least(1)).to(Token::Type);
    let duck = just("duck").to(Token::Duck);
    let ident = text::ident().map(|str: &str| Token::Ident(str.to_string()));
    let ctrl = one_of("=:{};,&").map(Token::ControlChar);
    let string = string_lexer();
    let int = int_lexer();

    let token = ty.or(duck).or(ident).or(ctrl).or(string).or(int);

    token.padded()
        .repeated()
        .collect::<Vec<Token>>()
}

fn string_lexer<'a>() -> impl Parser<'a, &'a str, Token> {
    just('"')
        .ignore_then(
            choice((
                just('\\').ignore_then(choice((just('"'), just('t').to('\t'), just('n').to('\n')))),
                any().filter(|c| *c != '"'),
            ))
            .repeated()
            .collect::<String>(),
        )
        .then_ignore(just('"'))
        .map(Token::StringLiteral)
}

fn int_lexer<'a>() -> impl Parser<'a, &'a str, Token> {
    text::int(10).map(|x: &str| Token::IntLiteral(x.parse().unwrap()))
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
            ("\"\"", vec![Token::StringLiteral(String::from(""))]),
            ("\"XX\"", vec![Token::StringLiteral(String::from("XX"))]),
            ("\"X\\\"X\"", vec![Token::StringLiteral(String::from("X\"X"))]),
            ("\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\"", vec![
                Token::StringLiteral(String::from("Hallo ich bin ein String\n\n\nNeue Zeile"))
            ]),
            ("1", vec![Token::IntLiteral(1)]),
            ("2003", vec![Token::IntLiteral(2003)]),
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
