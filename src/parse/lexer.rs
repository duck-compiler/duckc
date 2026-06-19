use std::fmt::Display;

use chumsky::{prelude::*, text::whitespace};

use crate::parse::{Context, SS, Spanned, value_parser::empty_range};

#[cfg(test)]
#[path = "lexer_test.rs"]
mod tests;

#[derive(Debug, PartialEq, Clone)]
pub enum RawFmtStringContents {
    Char(&'static str),
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
    Static,
    Mut,
    Use,
    Type,
    Go,
    Struct,
    Schema,
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
    IntLiteral(u64),
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
    InlineJsx(String),
    InlineDuckx(Vec<Spanned<Token>>),
    Module,
    ScopeRes,
    ThinArrow,
    ThickArrow,
    Comment(String),
    DocComment(String),
    Async,
    Component,
    Template,
    For,
    In,
    Defer,
    PlusEquals,
    SubEquals,
    MulEquals,
    DivEquals,
    ModEquals,
    ShiftRightEquals,
    ShiftLeftEquals,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let t = match self {
            Token::Static => "static",
            Token::Defer => "defer",
            Token::For => "for",
            Token::In => "in",
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
            Token::Schema => "schema",
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
            Token::InlineJsx(_) => "inline jsx",
            Token::InlineDuckx(_) => "inline duckx",
            Token::Module => "module",
            Token::Match => "match",
            Token::HtmlString(..) => "html string",
            Token::DocComment(comment) => &format!("/// {comment}"),
            Token::Comment(comment) => &format!("// {comment}"),
            Token::Async => "async",
            Token::PlusEquals => "+=",
            Token::SubEquals => "-=",
            Token::MulEquals => "*=",
            Token::DivEquals => "/=",
            Token::ModEquals => "%=",
            Token::ShiftLeftEquals => "<<=",
            Token::ShiftRightEquals => ">>=",
        };
        write!(f, "{t}")
    }
}

pub fn tokens_in_curly_braces<'a>(
    lexer: impl Parser<'a, &'a str, Spanned<Token>, extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|e| {
        lexer
            .clone()
            .filter(|x| matches!(x.0, Token::ControlChar('{')))
            .then(
                choice((
                    just("{").rewind().ignore_then(e.clone()),
                    any().filter(|c: &char| c.is_whitespace()).to(Vec::new()),
                    any()
                        .filter(|c| *c != '{' && *c != '}')
                        .rewind()
                        .ignore_then(lexer.clone())
                        .map(|x| vec![x]),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .then(just("}").map_with(|_, e| e.span()))
            .map(|((a, x), b)| {
                let mut v = Vec::new();
                let b = (
                    Token::ControlChar('}'),
                    SS {
                        start: b.start,
                        end: b.end,
                        context: a.1.context,
                    },
                );
                v.push(a);
                v.extend(x.into_iter().flatten());
                v.push(b);
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
    opening_tag().filter(|s: &String| s.to_ascii_lowercase().starts_with("<!doctype"))
}

pub fn duckx_parse_html_string<'a>(
    duckx_lexer: impl Parser<'a, &'a str, Vec<Spanned<Token>>, extra::Err<Rich<'a, char>>> + Clone + 'a,
    _context: Context,
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
        let context = Context {
            file_name,
            file_contents,
        };
        lexer
            .clone()
            .filter(|c: &Spanned<Token>| matches!(c.0, Token::ControlChar('{')))
            .then(
                choice((
                    just("{").rewind().ignore_then(duckx_lexer.clone()),
                    special_tag().map_with(move |x, e| {
                        vec![(
                            Token::HtmlString(vec![HtmlStringContents::String(x.to_string())]),
                            SS {
                                start: e.span().start,
                                end: e.span().end,
                                context,
                            },
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
                        .map_with(move |(x1, x2), e| {
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

                            vec![(
                                Token::HtmlString(final_out),
                                SS {
                                    start: e.span().start,
                                    end: e.span().end,
                                    context,
                                },
                            )]
                        })
                        .then_ignore(just("/>")),
                    opening_tag()
                        .rewind()
                        .ignore_then(duckx_parse_html_string(duckx_lexer.clone(), context))
                        .map_with(move |x, e| {
                            vec![(
                                x,
                                SS {
                                    start: e.span().start,
                                    end: e.span().end,
                                    context,
                                },
                            )]
                        }),
                    any().filter(|c: &char| c.is_whitespace()).to(Vec::new()),
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
            .then(just("}").map_with(|_, e| e.span()))
            .map(move |((a, x), b_span)| {
                let mut v = x.into_iter().flatten().collect::<Vec<_>>();
                v.insert(0, a);
                let b = (
                    Token::ControlChar('}'),
                    SS {
                        start: b_span.start,
                        end: b_span.end,
                        context,
                    },
                );
                v.push(b);
                v
            })
    })
}

pub fn oct_digit<'a>() -> impl Parser<'a, &'a str, char, extra::Err<Rich<'a, char>>> + Clone {
    one_of("01234567")
}
pub fn lex_single<'a>(
    file_name: &'static str,
    file_contents: &'static str,
) -> impl Parser<'a, &'a str, Spanned<Token>, extra::Err<Rich<'a, char>>> + Clone {
    recursive(|lexer| {
        let keyword_or_ident = text::ident().map(|str| match str {
            "static" => Token::Static,
            "defer" => Token::Defer,
            "for" => Token::For,
            "in" => Token::In,
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
            "schema" => Token::Schema,
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
            "async" => Token::Async,
            "and" => Token::And,
            "or" => Token::Or,
            "template" => Token::Template,
            _ => Token::Ident(str.to_string()),
        });

        let ctrl = one_of("`!=:{};,&()-<>.+-*/%|[]@~^").map(Token::ControlChar);

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

        let scope_res = just("::").to(Token::ScopeRes);
        let thin_arrow = just("->").to(Token::ThinArrow);
        let thick_arrow = just("=>").to(Token::ThickArrow);

        let assign_equals = choice((
            just("+=").to(Token::PlusEquals),
            just("-=").to(Token::SubEquals),
            just("*=").to(Token::MulEquals),
            just("/=").to(Token::DivEquals),
            just("%=").to(Token::ModEquals),
            just(">>=").to(Token::ShiftRightEquals),
            just("<<=").to(Token::ShiftLeftEquals),
        ));

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
                    just("\\{").to(RawFmtStringContents::Char("{")),
                    just("{")
                        .rewind()
                        .ignore_then(tokens_in_curly_braces(lexer.clone()))
                        .map(|e| RawFmtStringContents::Tokens(e.to_vec())),
                    none_of("\\\"")
                        .map(|c: char| c.to_string().leak() as &'static str)
                        .or(choice((
                            just("\\0").to("\0"),
                            just("\\o")
                                .ignore_then(oct_digit())
                                .then(oct_digit())
                                .then(oct_digit())
                                .map(|((a, b), c)| format!("\\o{a}{b}{c}").leak() as &'static str),
                            just("\\\\").to("\\"),
                            just("\\{").to("{"),
                            just("\\n").to("\n"),
                            just("\\t").to("\t"),
                            just("\\\"").to("\""),
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
                        RawFmtStringContents::Char(c) => s.push_str(c),
                    }
                }

                if !s.is_empty() {
                    xx.push(FmtStringContents::String(s));
                }

                Token::FormatStringLiteral(xx)
            });

        let token = inline_go_parser()
            .or(inline_jsx_parser())
            .or(just("duckx")
                .ignore_then(whitespace().at_least(1))
                .ignore_then(just("{").rewind())
                .ignore_then(duckx_contents_in_curly_braces(
                    file_name,
                    file_contents,
                    lexer.clone(),
                ))
                .map(Token::InlineDuckx))
            .or(just("&mut")
                .then_ignore(whitespace().at_least(1))
                .to(Token::RefMut))
            .or(doc_comment)
            .or(assign_equals)
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
    text::int(10)
        .to_slice()
        .try_map(|s: &str, span| {
            s.parse::<u64>()
                .map_err(|_| Rich::custom(span, "Invalid integer"))
        })
        .map(Token::IntLiteral)
}

fn inline_jsx_parser<'src>()
-> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone {
    just("jsx")
        .ignore_then(whitespace().at_least(1))
        .ignore_then(just("{").rewind())
        .ignore_then(go_text_parser())
        .map(|x| Token::InlineJsx(x[1..x.len() - 1].to_owned()))
}

fn char_lexer<'src>() -> impl Parser<'src, &'src str, Token, extra::Err<Rich<'src, char>>> + Clone {
    just("'")
        .ignore_then(none_of("\\\n\t'").or(choice((
            just("\\0").to('\0'),
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
            choice((
                just("\\0").to('\0'),
                just("\\\\").to('\\'),
                just("\\n").to('\n'),
                just("\\t").to('\t'),
                just("\\\"").to('"'),
                any().and_is(just("\"").not()),
            ))
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
