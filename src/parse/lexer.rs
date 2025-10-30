use std::fmt::Display;

use chumsky::{prelude::*, text::whitespace};

use crate::parse::{Context, SS, Spanned, value_parser::empty_range};

#[derive(Debug, PartialEq, Clone)]
pub enum RawFmtStringContents {
    Char(char),
    Tokens(Vec<Spanned<Token>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FmtStringContents {
    String(String),
    Tokens(Vec<Spanned<Token>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HtmlStringContents {
    String(String),
    Tokens(Vec<Spanned<Token>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RawHtmlStringContents {
    Char(char),
    Tokens(Vec<Spanned<Token>>),
    Sub(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Mut,
    Use,
    Type,
    Go,
    Struct,
    Impl,
    With,
    Extend,
    Duck,
    Function,
    Test,
    RefMut,
    Return,
    Ident(String),
    ControlChar(char),
    StringLiteral(String),
    FormatStringLiteral(Vec<FmtStringContents>),
    IntLiteral(i64),
    BoolLiteral(bool),
    CharLiteral(char),
    HtmlString(Vec<HtmlStringContents>),
    Equals,
    NotEquals,
    LessThanOrEquals,
    GreaterThanOrEquals,
    And,
    Or,
    Match,
    If,
    TypeOf,
    KeyOf,
    Else,
    Let,
    Const,
    While,
    Break,
    Continue,
    As,
    InlineGo(String),
    InlineTsx(String),
    InlineDuckx(Vec<Spanned<Token>>),
    Module,
    ScopeRes,
    ThinArrow,
    ThickArrow,
    Comment(String),
    DocComment(String),
    Sus,
    Component,
    Template,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            Token::Mut => "mut",
            Token::Const => "const",
            Token::RefMut => "&mut",
            Token::FormatStringLiteral(s) => &format!("f-string {s:?}"),
            Token::Impl => "impl",
            Token::With => "with",
            Token::Extend => "extend",
            Token::ScopeRes => "::",
            Token::ThinArrow => "->",
            Token::ThickArrow => "=>",
            Token::Use => "use",
            Token::Type => "type",
            Token::Test => "test",
            Token::Go => "go",
            Token::TypeOf => "typeof",
            Token::KeyOf => "keyof",
            Token::Struct => "struct",
            Token::Duck => "duck",
            Token::Component => "component",
            Token::Template => "template",
            Token::Function => "fn",
            Token::Return => "return",
            Token::Ident(_) => "identifier",
            Token::ControlChar(c) => &format!("{c}"),
            Token::StringLiteral(s) => &format!("string {s}"),
            Token::IntLiteral(_) => "int",
            Token::BoolLiteral(_) => "bool",
            Token::CharLiteral(_) => "char",
            Token::Equals => "==",
            Token::NotEquals => "!=",
            Token::LessThanOrEquals => "<=",
            Token::GreaterThanOrEquals => ">=",
            Token::And => "and",
            Token::Or => "or",
            Token::If => "if",
            Token::Else => "else",
            Token::Let => "let",
            Token::While => "while",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::As => "as",
            Token::InlineGo(_) => "inline go",
            Token::InlineTsx(_) => "inline tsx",
            Token::InlineDuckx(_) => "inline duckx",
            Token::Module => "module",
            Token::Match => "match",
            Token::HtmlString(..) => "html string",
            Token::DocComment(comment) => &format!("/// {comment}"),
            Token::Comment(comment) => &format!("// {comment}"),
            Token::Sus => "sus",
        };
        write!(f, "{t}")
    }
}

pub fn tokens_in_curly_braces<'a>(
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

#[derive(Debug, Clone)]
pub struct HtmlAttribute {
    pub name: String,
}

pub fn closing_tag<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> + Clone {
    just("</")
        .then(
            any()
                .filter(|c: &char| *c != '>')
                .repeated()
                .collect::<String>(),
        )
        .then(just(">"))
        .map(|((pre, main), close)| format!("{pre}{main}{close}"))
}

pub fn opening_self_closing<'a>()
-> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> + Clone {
    just("<")
        .and_is(just("</").not())
        .ignore_then(
            any()
                .and_is(just(">").not())
                .and_is(just("/>").not())
                .repeated()
                .collect::<String>(),
        )
        .then_ignore(just("/>"))
        .rewind()
        .ignore_then(
            just("<").and_is(just("</").not()).ignore_then(
                any()
                    .and_is(just(" ").not())
                    .and_is(just("/>").not())
                    .repeated()
                    .collect::<String>(),
            ),
        )
        .map(|x| {
            let complete = format!("<{x}");
            complete
        })
}

pub fn opening_tag<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> + Clone {
    just("<")
        .and_is(just("</").not())
        .and_is(opening_self_closing().not())
        .then(
            any()
                .filter(|c: &char| *c != '>')
                .repeated()
                .collect::<String>(),
        )
        .then(just(">"))
        .rewind()
        .then(
            just("<").and_is(just("</").not()).then(
                any()
                    .filter(|c: &char| *c != ' ' && *c != '>')
                    .repeated()
                    .collect::<String>(),
            ),
        )
        .map(|(((_pre, _main), _close), (_, x))| {
            let complete = format!("<{x}");
            complete
        })
}

pub fn special_tag<'a>() -> impl Parser<'a, &'a str, String, extra::Err<Rich<'a, char>>> + Clone {
    just("<!DOCTYPE HTML>")
        .map(|x| x.to_string())
        .or(just("<!doctype html>").map(|x| x.to_string()))
}

pub fn duckx_parse_html_string<'a>(
    duckx_lexer: impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|e| {
        opening_tag()
            .then(
                choice((
                    just("{")
                        .rewind()
                        .ignore_then(duckx_lexer.clone())
                        .map(RawHtmlStringContents::Tokens),
                    special_tag().map(|x| {
                        RawHtmlStringContents::Sub(Token::HtmlString(vec![
                            HtmlStringContents::String(x),
                        ]))
                    }),
                    opening_self_closing().map(|in_html| {
                        RawHtmlStringContents::Sub(Token::HtmlString(vec![
                            HtmlStringContents::String(in_html),
                        ]))
                    }),
                    opening_tag()
                        .rewind()
                        .ignore_then(e.clone())
                        .map(RawHtmlStringContents::Sub),
                    any()
                        .and_is(closing_tag().not())
                        // .filter(|c: &char| *c != '{' && *c != '<')
                        .map(RawHtmlStringContents::Char),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then(closing_tag())
            .map(
                |((opening_tag, template_contents), closing_tag): (
                    (String, Vec<RawHtmlStringContents>),
                    String,
                )| {
                    let mut new_out = Vec::new();
                    new_out.push(HtmlStringContents::String(opening_tag));
                    for c in template_contents {
                        match c {
                            RawHtmlStringContents::Tokens(t) => {
                                new_out.push(HtmlStringContents::Tokens(t));
                            }
                            RawHtmlStringContents::Char(c) => {
                                new_out.push(HtmlStringContents::String(c.to_string()));
                            }
                            RawHtmlStringContents::Sub(Token::HtmlString(sub)) => {
                                new_out.extend(sub);
                            }
                            _ => panic!("invalid"),
                        }
                    }
                    new_out.push(HtmlStringContents::String(closing_tag));

                    let mut s_buf = String::new();
                    let mut final_out = Vec::new();

                    for c in new_out {
                        match c {
                            HtmlStringContents::String(s) => {
                                s_buf.push_str(&s);
                            }
                            HtmlStringContents::Tokens(tok) => {
                                if !s_buf.is_empty() {
                                    final_out.push(HtmlStringContents::String(s_buf));
                                    s_buf = String::new();
                                }
                                final_out.push(HtmlStringContents::Tokens(tok));
                            }
                        }
                    }

                    if !s_buf.is_empty() {
                        final_out.push(HtmlStringContents::String(s_buf));
                    }

                    Token::HtmlString(final_out)
                },
            )
    })
}

pub fn duckx_contents_in_curly_braces<'a>(
    file_name: &'static str,
    file_contents: &'static str,
    lexer: impl Parser<'a, &'a str, Spanned<Token>, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|duckx_lexer| {
        just("{")
            .then(
                choice((
                    just("{").rewind().ignore_then(duckx_lexer.clone()),
                    special_tag().map(|x| {
                        vec![(
                            Token::HtmlString(vec![HtmlStringContents::String(x.to_string())]),
                            empty_range(),
                        )]
                    }),
                    opening_self_closing()
                        .then(
                            choice((
                                just("{")
                                    .rewind()
                                    .ignore_then(duckx_lexer.clone())
                                    .map(RawHtmlStringContents::Tokens),
                                any()
                                    .and_is(just("/>").not())
                                    .map(RawHtmlStringContents::Char),
                            ))
                            .repeated()
                            .collect::<Vec<_>>(),
                        )
                        .map(|(x1, x2)| {
                            let mut new_out = Vec::new();
                            new_out.push(HtmlStringContents::String(x1));
                            for c in x2 {
                                match c {
                                    RawHtmlStringContents::Tokens(t) => {
                                        new_out.push(HtmlStringContents::Tokens(t));
                                    }
                                    RawHtmlStringContents::Char(c) => {
                                        new_out.push(HtmlStringContents::String(c.to_string()));
                                    }
                                    RawHtmlStringContents::Sub(Token::HtmlString(sub)) => {
                                        new_out.extend(sub);
                                    }
                                    _ => panic!("invalid"),
                                }
                            }
                            new_out.push(HtmlStringContents::String("/>".to_string()));

                            let mut s_buf = String::new();
                            let mut final_out = Vec::new();

                            for c in new_out {
                                match c {
                                    HtmlStringContents::String(s) => {
                                        s_buf.push_str(&s);
                                    }
                                    HtmlStringContents::Tokens(tok) => {
                                        if !s_buf.is_empty() {
                                            final_out.push(HtmlStringContents::String(s_buf));
                                            s_buf = String::new();
                                        }
                                        final_out.push(HtmlStringContents::Tokens(tok));
                                    }
                                }
                            }

                            if !s_buf.is_empty() {
                                final_out.push(HtmlStringContents::String(s_buf));
                            }

                            vec![(Token::HtmlString(final_out), empty_range())]
                            // vec![(
                            //     Token::HtmlString(vec![HtmlStringContents::String(dbg!(format!(
                            //         "{x1}{x2}/>"
                            //     )))]),
                            //     empty_range(),
                            // )]
                        })
                        .then_ignore(just("/>")),
                    opening_tag()
                        .rewind()
                        .ignore_then(duckx_parse_html_string(duckx_lexer.clone()))
                        .map_with(move |x, e| {
                            vec![(
                                x,
                                SS {
                                    start: e.span().start,
                                    end: e.span().end,
                                    context: Context {
                                        file_name,
                                        file_contents,
                                    },
                                },
                            )]
                        }),
                    any()
                        .and_is(choice((just("{"), just("}"))).not())
                        // .filter(|c| *c != '{' && *c != '}')
                        .rewind()
                        .ignore_then(lexer.clone())
                        .map(|x| vec![x]),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then(just("}"))
            .map(|((_, x), _)| {
                let mut v = x.into_iter().flatten().collect::<Vec<_>>();
                v.insert(0, (Token::ControlChar('{'), empty_range()));
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
            "mut" => Token::Mut,
            "module" => Token::Module,
            "use" => Token::Use,
            "typeof" => Token::TypeOf,
            "keyof" => Token::KeyOf,
            "impl" => Token::Impl,
            "extend" => Token::Extend,
            "with" => Token::With,
            "test" => Token::Test,
            "type" => Token::Type,
            "duck" => Token::Duck,
            "go" => Token::Go,
            "struct" => Token::Struct,
            "fn" => Token::Function,
            "return" => Token::Return,
            "component" => Token::Component,
            "let" => Token::Let,
            "const" => Token::Const,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "as" => Token::As,
            "match" => Token::Match,
            "sus" => Token::Sus,
            "and" => Token::And,
            "or" => Token::Or,
            "template" => Token::Template,
            _ => Token::Ident(str.to_string()),
        });

        let ctrl = one_of("`!=:{};,&()-<>.+-*/%|[]@").map(Token::ControlChar);

        let string = string_lexer();
        let r#bool = choice((
            just("true").to(Token::BoolLiteral(true)),
            just("false").to(Token::BoolLiteral(false)),
        ));
        let r#char = char_lexer();
        let num = num_literal();

        let equals = just("==").to(Token::Equals);
        let not_equals = just("!=").to(Token::NotEquals);
        let less_than_or_equals = just("<=").to(Token::LessThanOrEquals);
        let greater_than_or_equals = just(">=").to(Token::GreaterThanOrEquals);
        let and = just("and").to(Token::And);
        let or = just("or").to(Token::Or);

        let scope_res = just("::").to(Token::ScopeRes);
        let thin_arrow = just("->").to(Token::ThinArrow);
        let thick_arrow = just("=>").to(Token::ThickArrow);

        let doc_comment = just("///")
            .ignore_then(
                any()
                    .and_is(just('\n').not())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .padded()
            .map(|comment| {
                Token::DocComment(comment.iter().collect::<String>().trim().to_string())
            });

        let comment = just("//")
            .ignore_then(
                any()
                    .and_is(just('\n').not())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .padded()
            .map(|comment| Token::Comment(comment.iter().collect::<String>().trim().to_string()));

        let fmt_string = just("f")
            .ignore_then(just('"'))
            .ignore_then(
                choice((
                    just("{")
                        .rewind()
                        .ignore_then(tokens_in_curly_braces(lexer.clone()))
                        .map(|e| RawFmtStringContents::Tokens(e[1..e.len() - 1].to_vec())),
                    none_of("\\\"")
                        .or(choice((
                            just("\\\\").to('\\'),
                            just("\\{").to('{'),
                            just("\\n").to('\n'),
                            just("\\t").to('\t'),
                            just("\\\"").to('"'),
                        )))
                        .map(RawFmtStringContents::Char),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then_ignore(just('"'))
            .map(|x| {
                let mut s = String::new();
                let mut xx = Vec::new();

                for e in x {
                    match e {
                        RawFmtStringContents::Tokens(t) => {
                            if !s.is_empty() {
                                xx.push(FmtStringContents::String(s.clone()));
                                s.clear();
                            }
                            xx.push(FmtStringContents::Tokens(t));
                        }
                        RawFmtStringContents::Char(c) => s.push(c),
                    }
                }

                if !s.is_empty() {
                    xx.push(FmtStringContents::String(s.replace("\n", "\\n")));
                }

                Token::FormatStringLiteral(xx)
            });

        let token = inline_go_parser()
            .or(inline_tsx_parser())
            .or(just("duckx")
                .ignore_then(whitespace().at_least(1))
                .ignore_then(just("{").rewind())
                // todo: [TSX] create tsx text parser
                .ignore_then(duckx_contents_in_curly_braces(
                    file_name,
                    file_contents,
                    lexer.clone(),
                ))
                .map(Token::InlineDuckx))
            .or(just("&mut")
                .then_ignore(whitespace().at_least(1))
                .map(|_| Token::RefMut))
            .or(doc_comment)
            .or(comment)
            .or(fmt_string)
            .or(thin_arrow)
            .or(thick_arrow)
            .or(scope_res)
            .or(r#bool)
            .or(equals)
            .or(not_equals)
            .or(less_than_or_equals)
            .or(greater_than_or_equals)
            .or(and)
            .or(or)
            .or(keyword_or_ident)
            .or(num)
            .or(ctrl)
            .or(string)
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
    just('-')
        .or_not()
        .then(text::int(10))
        .to_slice()
        .try_map(|s: &str, span| {
            s.parse::<i64>()
                .map_err(|_| Rich::custom(span, "Invalid integer"))
        })
        .map(Token::IntLiteral)
}

fn inline_tsx_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone {
    just("tsx")
        .ignore_then(whitespace().at_least(1))
        .ignore_then(just("{").rewind())
        // todo: [TSX] create tsx text parser
        .ignore_then(go_text_parser())
        .map(|x| Token::InlineTsx(x[1..x.len() - 1].to_owned()))
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
                FmtStringContents::String(_) => {}
            }
        }
    } else if let Token::InlineDuckx(contents) = &mut token_span.0 {
        for content in contents {
            token_empty_range(content);
        }
    } else if let Token::HtmlString(contents) = &mut token_span.0 {
        for content in contents {
            if let HtmlStringContents::Tokens(contents) = content {
                for content in contents {
                    token_empty_range(content);
                }
            }
        }
    }
}

impl Token {
    pub fn into_empty_span(&self) -> Spanned<Token> {
        (self.clone(), empty_range())
    }
}

#[cfg(test)]
mod tests {
    use crate::{lex, parse::value_parser::empty_range};

    use super::*;

    fn all_empty(v: Vec<Token>) -> Vec<Spanned<Token>> {
        v.iter().map(|x| x.into_empty_span()).collect()
    }

    fn ctrl(c: char) -> Token {
        Token::ControlChar(c)
    }

    fn left_brace() -> Token {
        ctrl('{')
    }

    fn right_brace() -> Token {
        ctrl('}')
    }

    #[test]
    fn test_lex() {
        let test_cases = vec![
            (
                "duckx {<>{{}}</>}",
                vec![Token::InlineDuckx(vec![
                    Token::ControlChar('{').into_empty_span(),
                    Token::HtmlString(vec![
                        HtmlStringContents::String("<>".to_string()),
                        HtmlStringContents::Tokens(vec![
                            Token::ControlChar('{').into_empty_span(),
                            Token::ControlChar('{').into_empty_span(),
                            Token::ControlChar('}').into_empty_span(),
                            Token::ControlChar('}').into_empty_span(),
                        ]),
                        HtmlStringContents::String("</>".to_string()),
                    ])
                    .into_empty_span(),
                    Token::ControlChar('}').into_empty_span(),
                ])],
            ),
            (
                "duckx {<Counter initial={10} />}",
                vec![Token::InlineDuckx(
                    vec![
                        Token::ControlChar('{'),
                        Token::HtmlString(vec![
                            HtmlStringContents::String("<Counter initial=".to_string()),
                            HtmlStringContents::Tokens(all_empty(vec![
                                Token::ControlChar('{'),
                                Token::IntLiteral(10),
                                Token::ControlChar('}'),
                            ])),
                            HtmlStringContents::String(" />".to_string()),
                        ]),
                        Token::ControlChar('}'),
                    ]
                    .into_iter()
                    .map(|x| x.into_empty_span())
                    .collect(),
                )],
            ),
            (
                "duckx {let hello = <> <!doctype html>{<Counter initial={100}/>} </>;}",
                vec![Token::InlineDuckx(all_empty(vec![
                    left_brace(),
                    Token::Let,
                    Token::Ident("hello".to_string()),
                    ctrl('='),
                    Token::HtmlString(vec![
                        HtmlStringContents::String("<> <!doctype html>".to_string()),
                        HtmlStringContents::Tokens(all_empty(vec![
                            left_brace(),
                            Token::HtmlString(vec![
                                HtmlStringContents::String("<Counter initial=".to_string()),
                                HtmlStringContents::Tokens(all_empty(vec![
                                    left_brace(),
                                    Token::IntLiteral(100),
                                    right_brace(),
                                ])),
                                HtmlStringContents::String("/>".to_string()),
                            ]),
                            right_brace(),
                        ])),
                        HtmlStringContents::String(" </>".to_string()),
                    ]),
                    ctrl(';'),
                    right_brace(),
                ]))],
            ),
            (
                "duckx {let hello = <> {ti <span id={props.id} hello={123}></span> tle} <h1> hallo moin  123</h1> abc </>;}",
                vec![Token::InlineDuckx(all_empty(vec![
                    left_brace(),
                    Token::Let,
                    Token::Ident("hello".to_string()),
                    ctrl('='),
                    Token::HtmlString(vec![
                        HtmlStringContents::String("<> ".to_string()),
                        HtmlStringContents::Tokens(all_empty(vec![
                            left_brace(),
                            Token::Ident("ti".to_string()),
                            Token::HtmlString(vec![
                                HtmlStringContents::String("<span id=".to_string()),
                                HtmlStringContents::Tokens(all_empty(vec![
                                    left_brace(),
                                    Token::Ident("props".to_string()),
                                    ctrl('.'),
                                    Token::Ident("id".to_string()),
                                    right_brace(),
                                ])),
                                HtmlStringContents::String(" hello=".to_string()),
                                HtmlStringContents::Tokens(all_empty(vec![
                                    left_brace(),
                                    Token::IntLiteral(123),
                                    right_brace(),
                                ])),
                                HtmlStringContents::String("></span>".to_string()),
                            ]),
                            Token::Ident("tle".to_string()),
                            right_brace(),
                        ])),
                        HtmlStringContents::String(
                            " <h1> hallo moin  123</h1> abc </>".to_string(),
                        ),
                    ]),
                    ctrl(';'),
                    right_brace(),
                ]))],
            ),
            (
                "duckx {let hello = {1};}",
                vec![Token::InlineDuckx(all_empty(vec![
                    left_brace(),
                    Token::Let,
                    Token::Ident("hello".to_string()),
                    ctrl('='),
                    left_brace(),
                    Token::IntLiteral(1),
                    right_brace(),
                    ctrl(';'),
                    right_brace(),
                ]))],
            ),
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
                "f\"{{{-1}}}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![
                        (Token::ControlChar('{'), empty_range()),
                        (Token::ControlChar('{'), empty_range()),
                        (Token::IntLiteral(-1), empty_range()),
                        (Token::ControlChar('}'), empty_range()),
                        (Token::ControlChar('}'), empty_range()),
                    ],
                )])],
            ),
            (
                "type /// hallo ich bin ein dokkommentar",
                vec![
                    Token::Type,
                    Token::DocComment("hallo ich bin ein dokkommentar".to_string()),
                ],
            ),
            ("component", vec![Token::Component]),
            (
                "tsx {console.log(\"Hallo, Welt\")}",
                vec![Token::InlineTsx("console.log(\"Hallo, Welt\")".to_string())],
            ),
            (
                "tsx {{console.log(\"Hallo, Welt\")}}",
                vec![Token::InlineTsx(
                    "{console.log(\"Hallo, Welt\")}".to_string(),
                )],
            ),
            (
                "tsx {<MyComponent name=\"lol\"/>\n<h1>hallo</h1>}",
                vec![Token::InlineTsx(
                    "<MyComponent name=\"lol\"/>\n<h1>hallo</h1>".to_string(),
                )],
            ),
            (
                "/// hallo ich bin ein dokkommentar",
                vec![Token::DocComment(
                    "hallo ich bin ein dokkommentar".to_string(),
                )],
            ),
            (
                "// hallo ich bin ein kommentar",
                vec![Token::Comment("hallo ich bin ein kommentar".to_string())],
            ),
            ("//", vec![Token::Comment("".to_string())]),
            ("///", vec![Token::DocComment("".to_string())]),
            ("//    ", vec![Token::Comment("".to_string())]),
            ("///    ", vec![Token::DocComment("".to_string())]),
            (
                "//  leading and trailing whitespace  ",
                vec![Token::Comment(
                    "leading and trailing whitespace".to_string(),
                )],
            ),
            (
                "///  leading and trailing whitespace  ",
                vec![Token::DocComment(
                    "leading and trailing whitespace".to_string(),
                )],
            ),
            (
                "//// this is a doc comment",
                vec![Token::DocComment("/ this is a doc comment".to_string())],
            ),
            (
                "// an /// inner doc comment",
                vec![Token::Comment("an /// inner doc comment".to_string())],
            ),
            (
                "/// a // regular inner comment",
                vec![Token::DocComment("a // regular inner comment".to_string())],
            ),
            (
                "// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![Token::Comment(
                    "a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string(),
                )],
            ),
            (
                "/// a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![Token::DocComment(
                    "a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string(),
                )],
            ),
            (
                "// Hallo World 🌍",
                vec![Token::Comment("Hallo World 🌍".to_string())],
            ),
            (
                "/// Hallo World 🌍",
                vec![Token::DocComment("Hallo World 🌍".to_string())],
            ),
            (
                "// first line\n// second line",
                vec![
                    Token::Comment("first line".to_string()),
                    Token::Comment("second line".to_string()),
                ],
            ),
            (
                "/// doc line 1\n/// doc line 2\n// regular line 3",
                vec![
                    Token::DocComment("doc line 1".to_string()),
                    Token::DocComment("doc line 2".to_string()),
                    Token::Comment("regular line 3".to_string()),
                ],
            ),
            (
                "// a comment with a // nested one",
                vec![Token::Comment("a comment with a // nested one".to_string())],
            ),
            (
                "/// a doc comment with a // nested regular one",
                vec![Token::DocComment(
                    "a doc comment with a // nested regular one".to_string(),
                )],
            ),
            (
                "// a comment with a /// nested doc one",
                vec![Token::Comment(
                    "a comment with a /// nested doc one".to_string(),
                )],
            ),
            (
                "//\tcomment with a tab before content",
                vec![Token::Comment(
                    "comment with a tab before content".to_string(),
                )],
            ),
            (
                "///\t doc comment with a tab before content",
                vec![Token::DocComment(
                    "doc comment with a tab before content".to_string(),
                )],
            ),
            (
                "// comment with mixed languages: Привет мир, こんにちは世界",
                vec![Token::Comment(
                    "comment with mixed languages: Привет мир, こんにちは世界".to_string(),
                )],
            ),
            (
                "//\n///",
                vec![
                    Token::Comment("".to_string()),
                    Token::DocComment("".to_string()),
                ],
            ),
            (
                "f\"{1}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![(Token::IntLiteral(1), empty_range())],
                )])],
            ),
            (
                "f\"{-1}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![(Token::IntLiteral(-1), empty_range())],
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
                "typeof {};",
                vec![
                    Token::TypeOf,
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
            ("impl", vec![Token::Impl]),
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
            ("-1", vec![Token::IntLiteral(-1)]),
            ("2003", vec![Token::IntLiteral(2003)]),
            ("-2003", vec![Token::IntLiteral(-2003)]),
            ("true", vec![Token::BoolLiteral(true)]),
            ("false", vec![Token::BoolLiteral(false)]),
            ("go { {} }", vec![Token::InlineGo(String::from(" {} "))]),
            ("go { xx }", vec![Token::InlineGo(String::from(" xx "))]),
            ("go {}", vec![Token::InlineGo(String::from(""))]),
            ("go {{}{}{}}", vec![Token::InlineGo(String::from("{}{}{}"))]),
            (
                "if true {}",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if true {} else {}",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::Else,
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if true {} else if {} else if {} else {}",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
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
                    FmtStringContents::String("FMT ".into()),
                    FmtStringContents::Tokens(vec![(Token::Ident("var".into()), empty_range())]),
                ])],
            ),
            (
                // Adjacent strings and f-strings to test greedy tokenizing.
                "\"a\"f\"b\"'c'",
                vec![
                    Token::StringLiteral("a".to_string()),
                    Token::FormatStringLiteral(vec![FmtStringContents::String("b".into())]),
                    Token::CharLiteral('c'),
                ],
            ),
            (
                "123testing",
                vec![Token::IntLiteral(123), Token::Ident("testing".to_string())],
            ),
            ("ifelse", vec![Token::Ident("ifelse".to_string())]),
            (
                "let π = 3;",
                vec![
                    Token::Let,
                    Token::Ident("π".to_string()),
                    Token::ControlChar('='),
                    Token::IntLiteral(3),
                    Token::ControlChar(';'),
                ],
            ),
            (
                // todo: discuss if π should be an valid identifier
                "let π = -3;",
                vec![
                    Token::Let,
                    Token::Ident("π".to_string()),
                    Token::ControlChar('='),
                    Token::IntLiteral(-3),
                    Token::ControlChar(';'),
                ],
            ),
            (
                // todo: divide token
                "5 / 2",
                vec![
                    Token::IntLiteral(5),
                    Token::ControlChar('/'),
                    Token::IntLiteral(2),
                ],
            ),
            (
                // todo: divide token
                "-5 / -2",
                vec![
                    Token::IntLiteral(-5),
                    Token::ControlChar('/'),
                    Token::IntLiteral(-2),
                ],
            ),
            (
                "fn(x){if(true)1 else 0;}",
                vec![
                    Token::Function,
                    Token::ControlChar('('),
                    Token::Ident("x".to_string()),
                    Token::ControlChar(')'),
                    Token::ControlChar('{'),
                    Token::If,
                    Token::ControlChar('('),
                    Token::BoolLiteral(true),
                    Token::ControlChar(')'),
                    Token::IntLiteral(1),
                    Token::Else,
                    Token::IntLiteral(0),
                    Token::ControlChar(';'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "fn(x){if(true)-1 else -0;}",
                vec![
                    Token::Function,
                    Token::ControlChar('('),
                    Token::Ident("x".to_string()),
                    Token::ControlChar(')'),
                    Token::ControlChar('{'),
                    Token::If,
                    Token::ControlChar('('),
                    Token::BoolLiteral(true),
                    Token::ControlChar(')'),
                    Token::IntLiteral(-1),
                    Token::Else,
                    Token::IntLiteral(-0),
                    Token::ControlChar(';'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "1.0// a float-like thing",
                vec![
                    Token::IntLiteral(1),
                    Token::ControlChar('.'),
                    Token::IntLiteral(0),
                    Token::Comment("a float-like thing".to_string()),
                ],
            ),
            (
                "-1.0// a float-like thing",
                vec![
                    Token::IntLiteral(-1),
                    Token::ControlChar('.'),
                    Token::IntLiteral(0),
                    Token::Comment("a float-like thing".to_string()),
                ],
            ),
            (
                "f\" outer {\"inner\"} outer \"",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::String(" outer ".into()),
                    FmtStringContents::Tokens(vec![(
                        Token::StringLiteral("inner".to_string()),
                        empty_range(),
                    )]),
                    FmtStringContents::String(" outer ".into()),
                ])],
            ),
            ("f\"\"", vec![Token::FormatStringLiteral(vec![])]),
            (
                "f\"{}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![],
                )])],
            ),
            (
                "f\"result is {calc(1, 2)}\"",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::String("result is ".into()),
                    FmtStringContents::Tokens(vec![
                        (Token::Ident("calc".to_string()), empty_range()),
                        (Token::ControlChar('('), empty_range()),
                        (Token::IntLiteral(1), empty_range()),
                        (Token::ControlChar(','), empty_range()),
                        (Token::IntLiteral(2), empty_range()),
                        (Token::ControlChar(')'), empty_range()),
                    ]),
                ])],
            ),
            (
                "f\"outer {f\"inner {y}\"} end\"",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::String("outer ".into()),
                    FmtStringContents::Tokens(vec![(
                        Token::FormatStringLiteral(vec![
                            FmtStringContents::String("inner ".into()),
                            FmtStringContents::Tokens(vec![(
                                Token::Ident("y".to_string()),
                                empty_range(),
                            )]),
                        ]),
                        empty_range(),
                    )]),
                    FmtStringContents::String(" end".into()),
                ])],
            ),
            (
                "f\"{1+1}\"",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![
                        (Token::IntLiteral(1), empty_range()),
                        (Token::ControlChar('+'), empty_range()),
                        (Token::IntLiteral(1), empty_range()),
                    ],
                )])],
            ),
            (
                "f\"{1+1}a\"",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::Tokens(vec![
                        (Token::IntLiteral(1), empty_range()),
                        (Token::ControlChar('+'), empty_range()),
                        (Token::IntLiteral(1), empty_range()),
                    ]),
                    FmtStringContents::String("a".into()),
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
                "f\"{{{1}}}\" // check",
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
            ("type /// hallo ich bin ein dokkommentar", vec![Token::Type]),
            ("/// hallo ich bin ein dokkommentar", vec![]),
            ("// hallo ich bin ein kommentar", vec![]),
            ("//", vec![]),
            ("///", vec![]),
            ("//    ", vec![]),
            ("///    ", vec![]),
            ("//  leading and trailing whitespace  ", vec![]),
            ("///  leading and trailing whitespace  ", vec![]),
            ("//// this is a doc comment", vec![]),
            ("// an /// inner doc comment", vec![]),
            ("/// a // regular inner comment", vec![]),
            ("// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?", vec![]),
            (
                "/// a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
                vec![],
            ),
            ("// Hallo World 🌍", vec![]),
            ("/// Hallo World 🌍", vec![]),
            ("// first line\n// second line", vec![]),
            ("/// doc line 1\n/// doc line 2\n// regular line 3", vec![]),
            ("// a comment with a // nested one", vec![]),
            ("/// a doc comment with a // nested regular one", vec![]),
            ("// a comment with a /// nested doc one", vec![]),
            ("//\tcomment with a tab before content", vec![]),
            ("///\t doc comment with a tab before content", vec![]),
            (
                "// comment with mixed languages: Привет мир, こんにちは世界",
                vec![],
            ),
            ("//\n///", vec![]),
            (
                "f\"{1}\" // check",
                vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                    vec![(Token::IntLiteral(1), empty_range())],
                )])],
            ),
            (
                "type Y = duck {}; // check",
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
                "typeY=duck{}; // check",
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
                "type Y = duck {} & duck {}; // check",
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
                "type Y = duck { x: String, y: String }; // check",
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
            (
                "() // check",
                vec![Token::ControlChar('('), Token::ControlChar(')')],
            ),
            ("-> // check", vec![Token::ThinArrow]),
            ("fn // check", vec![Token::Function]),
            (
                "\"\" // check",
                vec![Token::StringLiteral(String::from(""))],
            ),
            (
                "\"XX\" // check",
                vec![Token::StringLiteral(String::from("XX"))],
            ),
            (
                "\"X\\\"X\" // check",
                vec![Token::StringLiteral(String::from("X\"X"))],
            ),
            (
                "\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\" // check",
                vec![Token::StringLiteral(String::from(
                    "Hallo ich bin ein String\n\n\nNeue Zeile",
                ))],
            ),
            ("1 // check", vec![Token::IntLiteral(1)]),
            ("2003 // check", vec![Token::IntLiteral(2003)]),
            ("true // check", vec![Token::BoolLiteral(true)]),
            ("false // check", vec![Token::BoolLiteral(false)]),
            (
                "go { {} } // check",
                vec![Token::InlineGo(String::from(" {} "))],
            ),
            (
                "go { xx } // check",
                vec![Token::InlineGo(String::from(" xx "))],
            ),
            ("go {} // check", vec![Token::InlineGo(String::from(""))]),
            (
                "go {{}{}{}} // check",
                vec![Token::InlineGo(String::from("{}{}{}"))],
            ),
            (
                "if true {} // check",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if true {} else {} // check",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                    Token::Else,
                    Token::ControlChar('{'),
                    Token::ControlChar('}'),
                ],
            ),
            (
                "if true {} else if {} else if {} else {} // check",
                vec![
                    Token::If,
                    Token::BoolLiteral(true),
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
            ("'c' // check", vec![Token::CharLiteral('c')]),
            ("'\\n'//w", vec![Token::CharLiteral('\n')]),
            (
                "1.1 // check",
                vec![
                    Token::IntLiteral(1),
                    Token::ControlChar('.'),
                    Token::IntLiteral(1),
                ],
            ),
            (
                "let x: {}; // with comment",
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
                "f\"FMT {var}\" // check",
                vec![Token::FormatStringLiteral(vec![
                    FmtStringContents::String("FMT ".into()),
                    FmtStringContents::Tokens(vec![(Token::Ident("var".into()), empty_range())]),
                ])],
            ),
            ("sus", vec![Token::Sus]),
            ("sus fn", vec![Token::Sus, Token::Function]),
            ("test", vec![Token::Test]),
            ("keyof", vec![Token::KeyOf]),
            ("typeof", vec![Token::TypeOf]),
            ("extend", vec![Token::Extend]),
            ("with", vec![Token::With]),
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
