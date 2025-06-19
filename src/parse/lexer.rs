use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Type,
    Duck,
    Function,
    Return,
    Ident(String),
    ControlChar(char),
    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    CharLiteral(char),
    FloatLiteral(f64),
    Equals,
    If,
    Else,
    Let,
    While,
    Break,
    Continue,
}

pub type Spanned<T> = (T, SimpleSpan);

pub fn lexer<'a>() -> impl Parser<'a, &'a str, Vec<Token>> {
    let keyword_or_ident = just("@")
        .or_not()
        .then(text::ident())
        .map(|(x, str)| match str {
            "type" => Token::Type,
            "duck" => Token::Duck,
            "fun" => Token::Function,
            "return" => Token::Return,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "break" => Token::Break,
            "continue" => Token::Continue,
            _ => Token::Ident(format!("{}{str}", x.unwrap_or(""))),
        });
    let ctrl = one_of("!=:{};,&()->.+-*/%").map(Token::ControlChar);
    let string = string_lexer();
    let r#bool = choice((
        just("true").to(Token::BoolLiteral(true)),
        just("false").to(Token::BoolLiteral(false)),
    ));
    let r#char = char_lexer();
    let num = num_literal();
    let equals = just("==").to(Token::Equals);

    let token = r#bool
        .or(equals)
        .or(keyword_or_ident)
        .or(ctrl)
        .or(string)
        .or(num)
        .or(r#char);

    token.padded().repeated().collect::<Vec<Token>>()
}

fn num_literal<'src>() -> impl Parser<'src, &'src str, Token> {
    let pre = text::int(10).map(|s: &str| s.parse::<i64>().unwrap());
    let frac = just('.').ignore_then(text::digits(10)).to_slice();
    pre.then(frac.or_not()).map(|(pre, frac)| {
        if let Some(frac) = frac {
            let num = format!("{}{}", pre, frac).parse().unwrap();
            Token::FloatLiteral(num)
        } else {
            Token::IntLiteral(pre)
        }
    })
}

fn char_lexer<'src>() -> impl Parser<'src, &'src str, Token> {
    just("'")
        .ignore_then(
            choice((
                just('\\').ignore_then(choice((just('\''), just('t').to('\t'), just('n').to('\n')))),
                any().filter(|c| *c != '\''),
            ))
        )
        .then_ignore(just("'"))
        .map(Token::CharLiteral)
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
            ("fun", vec![Token::Function]),
            ("\"\"", vec![Token::StringLiteral(String::from(""))]),
            ("\"XX\"", vec![Token::StringLiteral(String::from("XX"))]),
            ("\"X\\\"X\"", vec![Token::StringLiteral(String::from("X\"X"))]),
            ("\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\"", vec![
                Token::StringLiteral(String::from("Hallo ich bin ein String\n\n\nNeue Zeile"))
            ]),
            ("1", vec![Token::IntLiteral(1)]),
            ("2003", vec![Token::IntLiteral(2003)]),
            ("true", vec![Token::BoolLiteral(true)]),
            ("false", vec![Token::BoolLiteral(false)]),
            ("if (true) {}", vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}')],
            ),
            ("if (true) {} else {}", vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}')],
            ),
            ("if (true) {} else if {} else if {} else {}", vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}')],
            ),
            ("'c'", vec![
                Token::CharLiteral('c')
            ]),
            ("'\\n'", vec![
                Token::CharLiteral('\n')
            ]),
            ("1.1", vec![Token::FloatLiteral(1.1)]),
            ("let x: {};", vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ]),
        ];

        for (src, expected_tokens) in test_cases {
            let parse_result = lexer().parse(src);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: Vec<Token> = parse_result.output()
                .expect(&src)
                .iter()
                .map(|token| token.clone())
                .collect();

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }
}
