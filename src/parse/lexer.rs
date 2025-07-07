use std::fmt::Display;

use chumsky::{prelude::*, text::whitespace};

use crate::parse::{Context, SS, Spanned};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Token {
    Use,
    Type,
    Go,
    Struct,
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
    As,
    InlineGo(String),
    Module,
    ScopeRes,
    ThinArrow,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            Token::ScopeRes => "::",
            Token::ThinArrow => "->",
            Token::Use => "use",
            Token::Type => "type",
            Token::Go => "go",
            Token::Struct => "struct",
            Token::Duck => "duck",
            Token::Function => "fn",
            Token::Return => "return",
            Token::Ident(_) => "identifier",
            Token::ControlChar(c) => &format!("{c}"),
            Token::StringLiteral(s) => &format!("string {s}"),
            Token::IntLiteral(_) => "int",
            Token::BoolLiteral(_) => "bool",
            Token::CharLiteral(_) => "char",
            Token::FloatLiteral(_) => "float",
            Token::Equals => "equals",
            Token::If => "if",
            Token::Else => "else",
            Token::Let => "let",
            Token::While => "while",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::As => "as",
            Token::InlineGo(_) => "inline go",
            Token::Module => "module",
        };
        write!(f, "{t}")
    }
}

pub fn lexer<'a>(
    file_name: &'static str,
    file_contents: &'static str,
) -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> {
    let keyword_or_ident = text::ident().map(|str| match str {
        "module" => Token::Module,
        "use" => Token::Use,
        "type" => Token::Type,
        "duck" => Token::Duck,
        "go" => Token::Go,
        "struct" => Token::Struct,
        "fn" => Token::Function,
        "return" => Token::Return,
        "let" => Token::Let,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "as" => Token::As,
        _ => Token::Ident(str.to_string()),
    });

    let ctrl = one_of("!=:{};,&()->.+-*/%|[]").map(Token::ControlChar);

    let string = string_lexer();
    let r#bool = choice((
        just("true").to(Token::BoolLiteral(true)),
        just("false").to(Token::BoolLiteral(false)),
    ));
    let r#char = char_lexer();
    let num = num_literal();

    let equals = just("==").to(Token::Equals);
    let scope_res = just("::").to(Token::ScopeRes);
    let thin_arrow = just("->").to(Token::ThinArrow);

    let token = inline_go_parser()
        .or(scope_res)
        .or(thin_arrow)
        .or(r#bool)
        .or(equals)
        .or(keyword_or_ident)
        .or(ctrl)
        .or(string)
        .or(num)
        .or(r#char);

    token
        .map_with(move |t, e| {
            (
                t,
                SS {
                    start: e.span().start,
                    end: e.span().end,
                    context: Context {
                        file_name,
                        file_contents,
                    },
                },
            )
        })
        .padded()
        .repeated()
        .collect::<Vec<Spanned<Token>>>()
}

fn go_text_parser<'src>() -> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char>>> {
    recursive(|e| {
        just("{")
            .ignore_then(
                ((just("{").rewind().ignore_then(e.clone()))
                    .or(any().filter(|c| *c != '{' && *c != '}').map(String::from)))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then_ignore(just("}"))
            .map(|x| {
                let x = x.join("");
                format!("{}{x}{}", "{", "}")
            })
    })
}

fn inline_go_parser<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
    just("go")
        .ignore_then(whitespace().at_least(1))
        .ignore_then(just("{").rewind())
        .ignore_then(go_text_parser())
        .map(|x| Token::InlineGo(x[1..x.len() - 1].to_owned()))
}

fn num_literal<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
    let pre = text::int(10).try_map(|s: &str, span| {
        s.parse::<i64>()
            .map_err(|_| Rich::custom(span, "Invalid integer"))
    });
    let frac = just('.').ignore_then(text::digits(10)).to_slice();
    pre.then(frac.or_not()).map(|(pre, frac)| {
        if let Some(frac) = frac {
            let num = format!("{pre}{frac}").parse().unwrap();
            Token::FloatLiteral(num)
        } else {
            Token::IntLiteral(pre)
        }
    })
}

fn char_lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> {
    just("'")
        .ignore_then(none_of("\\\n\t'").or(choice((
            just("\\\\").to('\\'),
            just("\\n").to('\n'),
            just("\\t").to('\t'),
            just("\\'").to('\''),
        ))))
        .then_ignore(just("'"))
        .map(Token::CharLiteral)
}

fn string_lexer<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> {
    just('"')
        .ignore_then(
            none_of("\\\n\t\"")
                .or(choice((
                    just("\\\\").to('\\'),
                    just("\\n").to('\n'),
                    just("\\t").to('\t'),
                    just("\\\"").to('"'),
                )))
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
            (
                "type Y = duck {};",
                vec![
                    Token::Type,
                    Token::Ident("Y".to_string()),
                    Token::ControlChar('='),
                    Token::Duck,
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::ControlChar(';'),
                ],
            ),
            (
                "typeY=duck{};",
                vec![
                    Token::Ident("typeY".to_string()),
                    Token::ControlChar('='),
                    Token::Duck,
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::ControlChar(';'),
                ],
            ),
            (
                "type Y = duck {} & duck {};",
                vec![
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
                ],
            ),
            (
                "type Y = duck { x: String, y: String };",
                vec![
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
                ],
            ),
            ("()", vec![Token::ControlChar('('), Token::ControlChar(')')]),
            ("->", vec![Token::ThinArrow]),
            ("fn", vec![Token::Function]),
            ("\"\"", vec![Token::StringLiteral(String::from(""))]),
            ("\"XX\"", vec![Token::StringLiteral(String::from("XX"))]),
            (
                "\"X\\\"X\"",
                vec![Token::StringLiteral(String::from("X\"X"))],
            ),
            (
                "\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\"",
                vec![Token::StringLiteral(String::from(
                    "Hallo ich bin ein String\n\n\nNeue Zeile",
                ))],
            ),
            ("1", vec![Token::IntLiteral(1)]),
            ("2003", vec![Token::IntLiteral(2003)]),
            ("true", vec![Token::BoolLiteral(true)]),
            ("false", vec![Token::BoolLiteral(false)]),
            ("go { {} }", vec![Token::InlineGo(String::from(" {} "))]),
            ("go { xx }", vec![Token::InlineGo(String::from(" xx "))]),
            ("go {}", vec![Token::InlineGo(String::from(""))]),
            ("go {{}{}{}}", vec![Token::InlineGo(String::from("{}{}{}"))]),
            (
                "if (true) {}",
                vec![
                    Token::If,
                    Token::ControlChar('('),
                    Token::BoolLiteral(true),
                    Token::ControlChar(')'),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if (true) {} else {}",
                vec![
                    Token::If,
                    Token::ControlChar('('),
                    Token::BoolLiteral(true),
                    Token::ControlChar(')'),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::Else,
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if (true) {} else if {} else if {} else {}",
                vec![
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
                    Token::ControlChar('}'),
                ],
            ),
            ("'c'", vec![Token::CharLiteral('c')]),
            ("'\\n'", vec![Token::CharLiteral('\n')]),
            ("1.1", vec![Token::FloatLiteral(1.1)]),
            (
                "let x: {};",
                vec![
                    Token::Let,
                    Token::Ident("x".to_string()),
                    Token::ControlChar(':'),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::ControlChar(';'),
                ],
            ),
        ];

        for (src, expected_tokens) in test_cases {
            let parse_result = lexer("test", src).parse(src);

            assert_eq!(parse_result.has_errors(), false, "{}", src);
            assert_eq!(parse_result.has_output(), true, "{}", src);

            let output: Vec<Token> = parse_result
                .output()
                .expect(&src)
                .iter()
                .map(|token| token.0.clone())
                .collect();

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }
}
