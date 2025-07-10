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
    Comment(String),
    DocComment(String),
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
            Token::DocComment(comment) => &format!("/// {comment}"),
            Token::Comment(comment) => &format!("// {comment}"),
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

        let doc_comment = just("///")
                .ignore_then(any().and_is(just('\n').not()).repeated().collect::<Vec<_>>())
                .padded()
                .map(|comment| Token::DocComment(comment.iter().collect::<String>().trim().to_string()));

        let comment = just("//")
                .ignore_then(any().and_is(just('\n').not()).repeated().collect::<Vec<_>>())
                .padded()
                .map(|comment| Token::Comment(comment.iter().collect::<String>().trim().to_string()));

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
            .or(doc_comment)
            .or(comment)
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

pub fn lex_parser<'src>(
    file_name: &'static str,
    file_contents: &'static str,
) -> impl Parser<'src, &'src str, Vec<Spanned<Token>>, extra::Err<Rich<'src, char>>> + Clone {
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
    pre.map(Token::IntLiteral)
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
    use crate::{lex, parse::value_parser::empty_range};

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
                "type /// hallo ich bin ein dokkommentar",
                vec![
                    Token::Type,
                    Token::DocComment("hallo ich bin ein dokkommentar".to_string())
                ]
            ),
            (
                "/// hallo ich bin ein dokkommentar",
                vec![
                    Token::DocComment("hallo ich bin ein dokkommentar".to_string())
                ]
            ),
            (
                "// hallo ich bin ein kommentar",
                vec![
                    Token::Comment("hallo ich bin ein kommentar".to_string())
                ]
            ),
            (
                "//",
                vec![
                    Token::Comment("".to_string())
                ]
            ),
            (
                "///",
                vec![
                    Token::DocComment("".to_string())
                ]
            ),
            (
                "//    ",
                vec![
                    Token::Comment("".to_string())
                ]
            ),
            (
                "///    ",
                vec![
                    Token::DocComment("".to_string())
                ]
            ),
            (
                "//  leading and trailing whitespace  ",
                vec![
                    Token::Comment("leading and trailing whitespace".to_string())
                ]
            ),
            (
                "///  leading and trailing whitespace  ",
                vec![
                    Token::DocComment("leading and trailing whitespace".to_string())
                ]
            ),
            (
                "//// this is a doc comment",
                vec![
                    Token::DocComment("/ this is a doc comment".to_string())
                ]
            ),
            (
                "// an /// inner doc comment",
                vec![
                    Token::Comment("an /// inner doc comment".to_string())
                ]
            ),
            (
                "/// a // regular inner comment",
                vec![
                    Token::DocComment("a // regular inner comment".to_string())
                ]
            ),
            (
                "// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![
                    Token::Comment("a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string())
                ]
            ),
            (
                "/// a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![
                    Token::DocComment("a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string())
                ]
            ),
            (
                "// Hallo World 🌍",
                vec![
                    Token::Comment("Hallo World 🌍".to_string())
                ]
            ),
            (
                "/// Hallo World 🌍",
                vec![
                    Token::DocComment("Hallo World 🌍".to_string())
                ]
            ),
            (
                "// first line\n// second line",
                vec![
                    Token::Comment("first line".to_string()),
                    Token::Comment("second line".to_string())
                ]
            ),
            (
                "/// doc line 1\n/// doc line 2\n// regular line 3",
                vec![
                    Token::DocComment("doc line 1".to_string()),
                    Token::DocComment("doc line 2".to_string()),
                    Token::Comment("regular line 3".to_string())
                ]
            ),
            (
                "// a comment with a // nested one",
                vec![
                    Token::Comment("a comment with a // nested one".to_string())
                ]
            ),
            (
                "/// a doc comment with a // nested regular one",
                vec![
                    Token::DocComment("a doc comment with a // nested regular one".to_string())
                ]
            ),
            (
                "// a comment with a /// nested doc one",
                vec![
                    Token::Comment("a comment with a /// nested doc one".to_string())
                ]
            ),
            (
                "//\tcomment with a tab before content",
                vec![
                    Token::Comment("comment with a tab before content".to_string())
                ]
            ),
            (
                "///\t doc comment with a tab before content",
                vec![
                    Token::DocComment("doc comment with a tab before content".to_string())
                ]
            ),
            (
                "// comment with mixed languages: Привет мир, こんにちは世界",
                vec![
                    Token::Comment("comment with mixed languages: Привет мир, こんにちは世界".to_string())
                ]
            ),
            (
                "//\n///",
                vec![
                    Token::Comment("".to_string()),
                    Token::DocComment("".to_string())
                ]
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
            (
                "1.1",
                vec![
                    Token::IntLiteral(1),
                    Token::ControlChar('.'),
                    Token::IntLiteral(1),
                ],
            ),
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
            let parse_result = lex_parser("test", src).parse(src);

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

    #[test]
    fn test_lex_comment_filtered() {
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
                "type /// hallo ich bin ein dokkommentar",
                vec![
                    Token::Type,
                ]
            ),
            (
                "/// hallo ich bin ein dokkommentar",
                vec![
                ]
            ),
            (
                "// hallo ich bin ein kommentar",
                vec![
                ]
            ),
            (
                "//",
                vec![
                ]
            ),
            (
                "///",
                vec![
                ]
            ),
            (
                "//    ",
                vec![
                ]
            ),
            (
                "///    ",
                vec![
                ]
            ),
            (
                "//  leading and trailing whitespace  ",
                vec![
                ]
            ),
            (
                "///  leading and trailing whitespace  ",
                vec![
                ]
            ),
            (
                "//// this is a doc comment",
                vec![
                ]
            ),
            (
                "// an /// inner doc comment",
                vec![
                ]
            ),
            (
                "/// a // regular inner comment",
                vec![
                ]
            ),
            (
                "// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![
                ]
            ),
            (
                "/// a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![
                ]
            ),
            (
                "// Hallo World 🌍",
                vec![
                ]
            ),
            (
                "/// Hallo World 🌍",
                vec![
                ]
            ),
            (
                "// first line\n// second line",
                vec![
                ]
            ),
            (
                "/// doc line 1\n/// doc line 2\n// regular line 3",
                vec![
                ]
            ),
            (
                "// a comment with a // nested one",
                vec![

                ]
            ),
            (
                "/// a doc comment with a // nested regular one",
                vec![

                ]
            ),
            (
                "// a comment with a /// nested doc one",
                vec![

                ]
            ),
            (
                "//\tcomment with a tab before content",
                vec![

                ]
            ),
            (
                "///\t doc comment with a tab before content",
                vec![

                ]
            ),
            (
                "// comment with mixed languages: Привет мир, こんにちは世界",
                vec![

                ]
            ),
            (
                "//\n///",
                vec![

                ]
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
            (
                "1.1",
                vec![
                    Token::IntLiteral(1),
                    Token::ControlChar('.'),
                    Token::IntLiteral(1),
                ],
            ),
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
            let parse_result = lex("test", src);

            let output: Vec<Token> = parse_result
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
