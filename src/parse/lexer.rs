use std::fmt::Display;

use chumsky::{prelude::*, text::whitespace};

use crate::parse::{Context, SS, Spanned, value_parser::empty_range};

#[derive(Debug, PartialEq, Clone)]
pub enum FmtStringContents {
    Char(char),
    Tokens(Vec<Spanned<Token>>),
}

#[derive(Debug, Clone, PartialEq)]
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
    FormatStringLiteral(Vec<FmtStringContents>),
    IntLiteral(i64),
    BoolLiteral(bool),
    CharLiteral(char),
    FloatLiteral(f64),
    Equals,
    Match,
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
            Token::FormatStringLiteral(s) => &format!("f-string {s:?}"),
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
            Token::Match => "match",
        };
        write!(f, "{t}")
    }
}

pub fn lex_fstring_tokens<'a>(
    lexer: impl Parser<'a, &'a str, Spanned<Token>, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|e| {
        just("{")
            .ignore_then(
                choice((
                    just("{").rewind().ignore_then(e.clone()),
                    any()
                        .filter(|c| *c != '{' && *c != '}')
                        .rewind()
                        .ignore_then(lexer.clone())
                        .map(|x| vec![x]),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then_ignore(just("}"))
            .map(|x| {
                let mut v = Vec::new();
                v.push((Token::ControlChar('{'), empty_range()));
                v.extend(x.into_iter().flatten());
                v.push((Token::ControlChar('}'), empty_range()));
                v
            })
    })
}

pub fn lex_single<'a>(
    file_name: &'static str,
    file_contents: &'static str,
) -> impl Parser<'a, &'a str, Spanned<Token>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|lexer| {
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
            "match" => Token::Match,
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

        let fmt_string = just("f")
            .ignore_then(just('"'))
            .ignore_then(
                choice((
                    just("{")
                        .rewind()
                        .ignore_then(lex_fstring_tokens(lexer.clone()))
                        .map(|e| FmtStringContents::Tokens(e[1..e.len() - 1].to_vec())),
                    none_of("\\\n\t\"")
                        .or(choice((
                            just("\\\\").to('\\'),
                            just("\\{").to('{'),
                            just("\\n").to('\n'),
                            just("\\t").to('\t'),
                            just("\\\"").to('"'),
                        )))
                        .map(FmtStringContents::Char),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then_ignore(just('"'))
            .map(Token::FormatStringLiteral);

        let token = inline_go_parser()
            .or(fmt_string)
            .or(thin_arrow)
            .or(scope_res)
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
    })
}

pub fn lexer<'a>(
    file_name: &'static str,
    file_contents: &'static str,
) -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> {
    lex_single(file_name, file_contents)
        .repeated()
        .collect::<Vec<_>>()
}

fn go_text_parser<'src>()
-> impl Parser<'src, &'src str, String, extra::Err<Rich<'src, char>>> + Clone {
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

fn inline_go_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone {
    just("go")
        .ignore_then(whitespace().at_least(1))
        .ignore_then(just("{").rewind())
        .ignore_then(go_text_parser())
        .map(|x| Token::InlineGo(x[1..x.len() - 1].to_owned()))
}

fn num_literal<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone
{
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

fn char_lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone {
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

fn string_lexer<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> + Clone {
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

pub fn token_empty_range(token_span: &mut Spanned<Token>) {
    token_span.1 = empty_range();
    if let Token::FormatStringLiteral(contents) = &mut token_span.0 {
        for content in contents {
            match content {
                FmtStringContents::Tokens(tokens) => {
                    for token in tokens {
                        token_empty_range(token);
                    }
                }
                FmtStringContents::Char(_) => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::value_parser::empty_range;

    use super::*;

    #[test]
    fn test_lex() {
        let test_cases = vec![
            (
                "f\"{{{1}}}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![
                        (Token::ControlChar('{'), empty_range()),
                        (Token::ControlChar('{'), empty_range()),
                        (Token::IntLiteral(1), empty_range()),
                        (Token::ControlChar('}'), empty_range()),
                        (Token::ControlChar('}'), empty_range()),
                    ],
                )])],
            ),
            (
                "f\"{1}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![(Token::IntLiteral(1), empty_range())],
                )])],
            ),
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
            (
                "f\"FMT {var}\"",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::Char('F'),
                    FmtStringContents::Char('M'),
                    FmtStringContents::Char('T'),
                    FmtStringContents::Char(' '),
                    FmtStringContents::Tokens(vec![(Token::Ident("var".into()), empty_range())]),
                ])],
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
                .map(|token| {
                    let mut cloned = token.clone();
                    token_empty_range(&mut cloned);
                    cloned.0
                })
                .collect();

            assert_eq!(output, expected_tokens, "{}", src);
        }
    }
}
