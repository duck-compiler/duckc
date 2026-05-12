use std::fmt;

use super::parser::{Span, WithSpan};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Fn,
    Let,
    Const,
    Type,
    Struct,
    Use,
    Impl,
    Extend,
    With,
    Mut,
    Static,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Match,
    As,
    And,
    Or,
    Duck,
    Go,
    Schema,
    Module,
    Typeof,
    Keyof,
    Test,
    Component,
    Template,
    Async,
    Defer,

    // Literals
    Int(u64),
    Bool(bool),
    Char(char),
    String(String),

    FmtString(Vec<FmtStringPart>),
    Ident(String),

    // Single-character punctuation and operators
    Bang,     // !
    Percent,  // %
    Amp,      // &
    LParen,   // (
    RParen,   // )
    Star,     // *
    Plus,     // +
    Comma,    // ,
    Minus,    // -
    Dot,      // .
    Slash,    // /
    Colon,    // :
    Semi,     // ;
    Lt,       // <
    Eq,       // =
    Gt,       // >
    At,       // @
    LBracket, // [
    RBracket, // ]
    Caret,    // ^
    LBrace,   // {
    Pipe,     // |
    RBrace,   // }
    Tilde,    // ~

    // Multi-character operators
    EqEq,       // ==
    BangEq,     // !=
    LtEq,       // <=
    GtEq,       // >=
    ScopeRes,   // ::
    ThinArrow,  // ->
    ThickArrow, // =>

    RefMut, // &mut
    PlusEq, // +=
    SubEq,  // -=
    MulEq,  // *=
    DivEq,  // /=
    ModEq,  // %=
    ShrEq,  // >>=
    ShlEq,  // <<=

    InlineGo(String),
    InlineJsx(String),

    InlineDuckx(Vec<WithSpan<Token>>),
    HtmlString(Vec<HtmlPart>),

    Comment(String),
    DocComment(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FmtStringPart {
    Literal(String),
    /// Surrounding `{` and `}` are included.
    Tokens(Vec<WithSpan<Token>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HtmlPart {
    Literal(String),
    /// Surrounding `{` and `}` are included.
    Tokens(Vec<WithSpan<Token>>),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Const => write!(f, "const"),
            Token::Type => write!(f, "type"),
            Token::Struct => write!(f, "struct"),
            Token::Use => write!(f, "use"),
            Token::Impl => write!(f, "impl"),
            Token::Extend => write!(f, "extend"),
            Token::With => write!(f, "with"),
            Token::Mut => write!(f, "mut"),
            Token::Static => write!(f, "static"),
            Token::Return => write!(f, "return"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::Match => write!(f, "match"),
            Token::As => write!(f, "as"),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Duck => write!(f, "duck"),
            Token::Go => write!(f, "go"),
            Token::Schema => write!(f, "schema"),
            Token::Module => write!(f, "module"),
            Token::Typeof => write!(f, "typeof"),
            Token::Keyof => write!(f, "keyof"),
            Token::Test => write!(f, "test"),
            Token::Component => write!(f, "component"),
            Token::Template => write!(f, "template"),
            Token::Async => write!(f, "async"),
            Token::Defer => write!(f, "defer"),
            Token::Int(_) => write!(f, "int"),
            Token::Bool(b) => write!(f, "{b}"),
            Token::Char(c) => write!(f, "'{c}'"),
            Token::String(s) => write!(f, "\"{s}\""),
            Token::FmtString(_) => write!(f, "f-string"),
            Token::Ident(s) => write!(f, "{s}"),
            Token::Bang => write!(f, "!"),
            Token::Percent => write!(f, "%"),
            Token::Amp => write!(f, "&"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Star => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::Comma => write!(f, ","),
            Token::Minus => write!(f, "-"),
            Token::Dot => write!(f, "."),
            Token::Slash => write!(f, "/"),
            Token::Colon => write!(f, ":"),
            Token::Semi => write!(f, ";"),
            Token::Lt => write!(f, "<"),
            Token::Eq => write!(f, "="),
            Token::Gt => write!(f, ">"),
            Token::At => write!(f, "@"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Caret => write!(f, "^"),
            Token::LBrace => write!(f, "{{"),
            Token::Pipe => write!(f, "|"),
            Token::RBrace => write!(f, "}}"),
            Token::Tilde => write!(f, "~"),
            Token::EqEq => write!(f, "=="),
            Token::BangEq => write!(f, "!="),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::ScopeRes => write!(f, "::"),
            Token::ThinArrow => write!(f, "->"),
            Token::ThickArrow => write!(f, "=>"),
            Token::RefMut => write!(f, "&mut"),
            Token::PlusEq => write!(f, "+="),
            Token::SubEq => write!(f, "-="),
            Token::MulEq => write!(f, "*="),
            Token::DivEq => write!(f, "/="),
            Token::ModEq => write!(f, "%="),
            Token::ShrEq => write!(f, ">>="),
            Token::ShlEq => write!(f, "<<="),
            Token::InlineGo(_) => write!(f, "go {{...}}"),
            Token::InlineJsx(_) => write!(f, "jsx {{...}}"),
            Token::InlineDuckx(_) => write!(f, "duckx {{...}}"),
            Token::HtmlString(_) => write!(f, "<html>"),
            Token::Comment(s) => write!(f, "// {s}"),
            Token::DocComment(s) => write!(f, "/// {s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

impl LexError {
    fn new(msg: impl Into<String>, span: Span) -> Self {
        Self {
            msg: msg.into(),
            span,
        }
    }
}

struct Tokenizer<'src> {
    src: &'src str,
    pos: usize,
    file_id: u16,
    errors: Vec<LexError>,
}

impl<'src> Tokenizer<'src> {
    fn new(src: &'src str, file_id: u16) -> Self {
        Self {
            src,
            pos: 0,
            file_id,
            errors: Vec::new(),
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn remaining(&self) -> &'src str {
        &self.src[self.pos..]
    }

    fn peek(&self) -> Option<char> {
        self.remaining().chars().next()
    }

    fn starts_with(&self, s: &str) -> bool {
        self.remaining().starts_with(s)
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.remaining().chars().next()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn eat_while(&mut self, mut pred: impl FnMut(char) -> bool) -> &'src str {
        let start = self.pos;
        loop {
            match self.remaining().chars().next() {
                Some(c) if pred(c) => self.pos += c.len_utf8(),
                _ => break,
            }
        }
        &self.src[start..self.pos]
    }

    fn skip_whitespace(&mut self) {
        self.eat_while(char::is_whitespace);
    }

    fn span_from(&self, start: usize) -> Span {
        Span::new(start as u32, self.pos as u32, self.file_id)
    }

    fn spanned(&self, kind: Token, start: usize) -> WithSpan<Token> {
        WithSpan::new(kind, self.span_from(start))
    }

    fn push_error(&mut self, msg: impl Into<String>, start: usize) {
        let sp = self.span_from(start);
        self.errors.push(LexError::new(msg, sp));
    }

    fn next_token(&mut self) -> Option<WithSpan<Token>> {
        self.skip_whitespace();
        if self.at_end() {
            return None;
        }
        let start = self.pos;

        if self.starts_with("///") {
            return Some(self.lex_doc_comment(start));
        }
        if self.starts_with("//") {
            return Some(self.lex_comment(start));
        }

        if self.starts_with(">>=") {
            self.pos += 3;
            return Some(self.spanned(Token::ShrEq, start));
        }
        if self.starts_with("<<=") {
            self.pos += 3;
            return Some(self.spanned(Token::ShlEq, start));
        }

        macro_rules! two {
            ($s:literal, $t:expr) => {
                if self.starts_with($s) {
                    self.pos += 2;
                    return Some(self.spanned($t, start));
                }
            };
        }

        two!("==", Token::EqEq);
        two!("!=", Token::BangEq);
        two!("<=", Token::LtEq);
        two!(">=", Token::GtEq);
        two!("::", Token::ScopeRes);
        two!("->", Token::ThinArrow);
        two!("=>", Token::ThickArrow);
        two!("+=", Token::PlusEq);
        two!("-=", Token::SubEq);
        two!("*=", Token::MulEq);
        two!("/=", Token::DivEq);
        two!("%=", Token::ModEq);

        // &mut is only a token when followed by whitespace
        if self.starts_with("&mut") {
            if self.src[self.pos + 4..].starts_with(char::is_whitespace) {
                self.pos += 4;
                return Some(self.spanned(Token::RefMut, start));
            }
        }

        // f" is a format string, not the identifier 'f'
        if self.starts_with("f\"") {
            self.pos += 1;
            return self.lex_fmt_string(start);
        }

        let c = self.peek().unwrap();

        if c == '_' || c.is_alphabetic() {
            return Some(self.lex_word(start));
        }
        if c == '"' {
            return self.lex_string(start);
        }
        if c == '\'' {
            return self.lex_char(start);
        }
        if c.is_ascii_digit() {
            return Some(self.lex_int(start));
        }

        self.advance();
        let tok = match c {
            '!' => Token::Bang,
            '%' => Token::Percent,
            '&' => Token::Amp,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '*' => Token::Star,
            '+' => Token::Plus,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '.' => Token::Dot,
            '/' => Token::Slash,
            ':' => Token::Colon,
            ';' => Token::Semi,
            '<' => Token::Lt,
            '=' => Token::Eq,
            '>' => Token::Gt,
            '@' => Token::At,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '^' => Token::Caret,
            '{' => Token::LBrace,
            '|' => Token::Pipe,
            '}' => Token::RBrace,
            '~' => Token::Tilde,
            other => {
                self.errors.push(LexError::new(
                    format!("unexpected character '{other}'"),
                    self.span_from(start),
                ));
                return None;
            }
        };
        Some(self.spanned(tok, start))
    }

    fn lex_word(&mut self, start: usize) -> WithSpan<Token> {
        let word = self.eat_while(|c| c == '_' || c.is_alphanumeric());

        // go/jsx/duckx are only block-prefixes when the next non-whitespace char is '{'
        let followed_by_brace = || self.remaining().trim_start().starts_with('{');

        match word {
            "go" if followed_by_brace() => {
                self.skip_whitespace();
                if let Some(tok) = self.lex_raw_block_as(start, Token::InlineGo) {
                    return tok;
                }
                self.spanned(Token::Ident("go".into()), start)
            }
            "jsx" if followed_by_brace() => {
                self.skip_whitespace();
                if let Some(tok) = self.lex_raw_block_as(start, Token::InlineJsx) {
                    return tok;
                }
                self.spanned(Token::Ident("jsx".into()), start)
            }
            "duckx" if followed_by_brace() => {
                self.skip_whitespace();
                if let Some(toks) = self.lex_duckx_block() {
                    return self.spanned(Token::InlineDuckx(toks), start);
                }
                self.spanned(Token::Ident("duckx".into()), start)
            }
            "fn" => self.spanned(Token::Fn, start),
            "let" => self.spanned(Token::Let, start),
            "const" => self.spanned(Token::Const, start),
            "type" => self.spanned(Token::Type, start),
            "struct" => self.spanned(Token::Struct, start),
            "use" => self.spanned(Token::Use, start),
            "impl" => self.spanned(Token::Impl, start),
            "extend" => self.spanned(Token::Extend, start),
            "with" => self.spanned(Token::With, start),
            "mut" => self.spanned(Token::Mut, start),
            "static" => self.spanned(Token::Static, start),
            "return" => self.spanned(Token::Return, start),
            "if" => self.spanned(Token::If, start),
            "else" => self.spanned(Token::Else, start),
            "while" => self.spanned(Token::While, start),
            "for" => self.spanned(Token::For, start),
            "in" => self.spanned(Token::In, start),
            "break" => self.spanned(Token::Break, start),
            "continue" => self.spanned(Token::Continue, start),
            "match" => self.spanned(Token::Match, start),
            "as" => self.spanned(Token::As, start),
            "and" => self.spanned(Token::And, start),
            "or" => self.spanned(Token::Or, start),
            "duck" => self.spanned(Token::Duck, start),
            "schema" => self.spanned(Token::Schema, start),
            "module" => self.spanned(Token::Module, start),
            "typeof" => self.spanned(Token::Typeof, start),
            "keyof" => self.spanned(Token::Keyof, start),
            "test" => self.spanned(Token::Test, start),
            "component" => self.spanned(Token::Component, start),
            "template" => self.spanned(Token::Template, start),
            "async" => self.spanned(Token::Async, start),
            "defer" => self.spanned(Token::Defer, start),
            "true" => self.spanned(Token::Bool(true), start),
            "false" => self.spanned(Token::Bool(false), start),
            other => self.spanned(Token::Ident(other.to_owned()), start),
        }
    }

    fn lex_string(&mut self, start: usize) -> Option<WithSpan<Token>> {
        self.advance(); // opening "
        let mut buf = String::new();
        loop {
            match self.advance() {
                None => {
                    self.push_error("unterminated string literal", start);
                    return None;
                }
                Some('"') => break,
                Some('\\') => match self.advance() {
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('\\') => buf.push('\\'),
                    Some('"') => buf.push('"'),
                    Some('0') => buf.push('\0'),
                    Some('o') => {
                        let a = self.advance().unwrap_or('0');
                        let b = self.advance().unwrap_or('0');
                        let c = self.advance().unwrap_or('0');
                        let v = u8::from_str_radix(&format!("{a}{b}{c}"), 8).unwrap_or(0);
                        buf.push(v as char);
                    }
                    Some(o) => buf.push(o),
                    None => {
                        self.push_error("unterminated escape", start);
                        return None;
                    }
                },
                Some(c) => buf.push(c),
            }
        }
        Some(self.spanned(Token::String(buf), start))
    }

    fn lex_fmt_string(&mut self, start: usize) -> Option<WithSpan<Token>> {
        self.advance(); // opening "
        let mut parts: Vec<FmtStringPart> = Vec::new();
        let mut lit = String::new();

        loop {
            match self.peek() {
                None => {
                    self.push_error("unterminated format string", start);
                    return None;
                }
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('{') => {
                    if lit.ends_with('\\') {
                        lit.pop();
                        lit.push('{');
                        self.advance();
                        continue;
                    }
                    if !lit.is_empty() {
                        parts.push(FmtStringPart::Literal(std::mem::take(&mut lit)));
                    }
                    let toks = self.lex_tokens_in_braces()?;
                    parts.push(FmtStringPart::Tokens(toks));
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => lit.push('\n'),
                        Some('t') => lit.push('\t'),
                        Some('\\') => lit.push('\\'),
                        Some('"') => lit.push('"'),
                        Some('0') => lit.push('\0'),
                        Some('{') => lit.push('{'),
                        Some(o) => lit.push(o),
                        None => {}
                    }
                }
                Some(_) => lit.push(self.advance().unwrap()),
            }
        }
        if !lit.is_empty() {
            parts.push(FmtStringPart::Literal(lit));
        }
        Some(self.spanned(Token::FmtString(parts), start))
    }

    /// Lex `{ tokens... }` including the delimiter tokens themselves.
    /// Used by format string interpolations and duckx content.
    fn lex_tokens_in_braces(&mut self) -> Option<Vec<WithSpan<Token>>> {
        let brace_start = self.pos;
        self.advance(); // '{'
        let mut out = vec![WithSpan::new(
            Token::LBrace,
            Span::new(brace_start as u32, self.pos as u32, self.file_id),
        )];

        loop {
            self.skip_whitespace();
            match self.peek() {
                None => {
                    self.push_error("unterminated brace block", brace_start);
                    return None;
                }
                Some('{') => out.extend(self.lex_tokens_in_braces()?),
                Some('}') => {
                    let cs = self.pos;
                    self.advance();
                    out.push(WithSpan::new(
                        Token::RBrace,
                        Span::new(cs as u32, self.pos as u32, self.file_id),
                    ));
                    break;
                }
                _ => {
                    if let Some(tok) = self.next_token() {
                        out.push(tok);
                    }
                }
            }
        }
        Some(out)
    }

    fn lex_char(&mut self, start: usize) -> Option<WithSpan<Token>> {
        self.advance(); // opening '
        let c = match self.advance() {
            None => {
                self.push_error("unterminated char literal", start);
                return None;
            }
            Some('\\') => match self.advance() {
                Some('n') => '\n',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('\'') => '\'',
                Some('0') => '\0',
                Some(o) => o,
                None => {
                    self.push_error("unterminated char escape", start);
                    return None;
                }
            },
            Some(c) => c,
        };
        if self.advance() != Some('\'') {
            self.push_error("char literal must contain exactly one character", start);
        }
        Some(self.spanned(Token::Char(c), start))
    }

    fn lex_int(&mut self, start: usize) -> WithSpan<Token> {
        let s = self.eat_while(|c| c.is_ascii_digit());
        let v = s.parse::<u64>().unwrap_or_else(|_| {
            self.push_error(format!("integer '{s}' overflows u64"), start);
            0
        });
        self.spanned(Token::Int(v), start)
    }

    fn lex_doc_comment(&mut self, start: usize) -> WithSpan<Token> {
        self.pos += 3; // ///
        let text = self.eat_while(|c| c != '\n').trim().to_owned();
        self.spanned(Token::DocComment(text), start)
    }

    fn lex_comment(&mut self, start: usize) -> WithSpan<Token> {
        self.pos += 2; // //
        let text = self.eat_while(|c| c != '\n').trim().to_owned();
        self.spanned(Token::Comment(text), start)
    }

    /// Consume `{ ... }` with balanced braces, return the interior as raw text.
    /// Used for `go` and `jsx` blocks where content is passed through uninterpreted.
    fn lex_raw_block_as(
        &mut self,
        start: usize,
        wrap: impl Fn(String) -> Token,
    ) -> Option<WithSpan<Token>> {
        self.advance(); // '{'
        let mut buf = String::new();
        let mut depth: usize = 1;
        loop {
            match self.advance() {
                None => {
                    self.push_error("unterminated block", start);
                    return None;
                }
                Some('{') => {
                    depth += 1;
                    buf.push('{');
                }
                Some('}') => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    buf.push('}');
                }
                Some(c) => buf.push(c),
            }
        }
        Some(self.spanned(wrap(buf), start))
    }

    fn lex_duckx_block(&mut self) -> Option<Vec<WithSpan<Token>>> {
        let open = self.pos;
        self.advance(); // '{'
        let mut out = vec![WithSpan::new(
            Token::LBrace,
            Span::new(open as u32, self.pos as u32, self.file_id),
        )];

        loop {
            self.skip_whitespace();
            match self.peek() {
                None => {
                    self.push_error("unterminated duckx block", open);
                    return None;
                }
                Some('}') => {
                    let cs = self.pos;
                    self.advance();
                    out.push(WithSpan::new(
                        Token::RBrace,
                        Span::new(cs as u32, self.pos as u32, self.file_id),
                    ));
                    break;
                }
                Some('{') => {
                    if let Some(nested) = self.lex_duckx_block() {
                        out.extend(nested);
                    }
                }
                Some('<') if !self.starts_with("</") => {
                    let hs = self.pos;
                    if let Some(html) = self.lex_html_element() {
                        out.push(WithSpan::new(html, self.span_from(hs)));
                    } else {
                        let cs = self.pos;
                        self.advance();
                        out.push(self.spanned(Token::Lt, cs));
                    }
                }
                _ => {
                    if let Some(tok) = self.next_token() {
                        out.push(tok);
                    }
                }
            }
        }
        Some(out)
    }

    fn lex_html_element(&mut self) -> Option<Token> {
        let elem_start = self.pos;
        self.advance(); // '<'

        if self.peek() == Some('!') {
            let raw = self.eat_while(|c| c != '>');
            self.advance(); // '>'
            return Some(Token::HtmlString(vec![HtmlPart::Literal(format!(
                "<!{raw}>"
            ))]));
        }

        let tag_name = self
            .eat_while(|c| !c.is_whitespace() && c != '>' && c != '/')
            .to_owned();
        if tag_name.is_empty() {
            return None;
        }

        let mut attrs = String::new();
        loop {
            match self.peek() {
                None => {
                    self.push_error(
                        format!("unterminated opening tag '<{tag_name}'"),
                        elem_start,
                    );
                    return None;
                }
                Some('/') if self.starts_with("/>") => break,
                Some('>') => break,
                Some(_) => attrs.push(self.advance().unwrap()),
            }
        }

        if self.starts_with("/>") {
            self.pos += 2;
            return Some(Token::HtmlString(vec![HtmlPart::Literal(format!(
                "<{tag_name}{attrs}/>"
            ))]));
        }

        self.advance(); // '>'
        let opening_lit = format!("<{tag_name}{attrs}>");
        let mut parts: Vec<HtmlPart> = vec![HtmlPart::Literal(opening_lit)];
        let mut lit_buf = String::new();
        let closing = format!("</{tag_name}");

        loop {
            if self.at_end() {
                self.push_error(format!("unterminated element '<{tag_name}'"), elem_start);
                return None;
            }

            if self.starts_with(&closing) {
                if !lit_buf.is_empty() {
                    parts.push(HtmlPart::Literal(std::mem::take(&mut lit_buf)));
                }
                let raw = self.eat_while(|c| c != '>');
                self.advance(); // '>'
                parts.push(HtmlPart::Literal(format!("{raw}>")));
                break;
            }

            if self.peek() == Some('<') && !self.starts_with("</") {
                if !lit_buf.is_empty() {
                    parts.push(HtmlPart::Literal(std::mem::take(&mut lit_buf)));
                }
                if let Some(nested) = self.lex_html_element() {
                    match nested {
                        Token::HtmlString(sub) => parts.extend(sub),
                        other => parts.push(HtmlPart::Literal(format!("{other}"))),
                    }
                } else {
                    lit_buf.push(self.advance().unwrap_or('<'));
                }
                continue;
            }

            if self.peek() == Some('{') {
                if !lit_buf.is_empty() {
                    parts.push(HtmlPart::Literal(std::mem::take(&mut lit_buf)));
                }
                if let Some(toks) = self.lex_duckx_block() {
                    parts.push(HtmlPart::Tokens(toks));
                } else {
                    return None;
                }
                continue;
            }

            lit_buf.push(self.advance().unwrap());
        }

        if !lit_buf.is_empty() {
            parts.push(HtmlPart::Literal(lit_buf));
        }
        Some(Token::HtmlString(merge_html_parts(parts)))
    }
}

fn merge_html_parts(parts: Vec<HtmlPart>) -> Vec<HtmlPart> {
    let mut out: Vec<HtmlPart> = Vec::with_capacity(parts.len());
    for p in parts {
        match p {
            HtmlPart::Literal(s) => {
                if let Some(HtmlPart::Literal(prev)) = out.last_mut() {
                    prev.push_str(&s);
                } else {
                    out.push(HtmlPart::Literal(s));
                }
            }
            other => out.push(other),
        }
    }
    out
}

/// Tokenize `src` assigned to `file_id`.
/// Comments are included; filter them out before feeding tokens to the parser.
pub fn tokenize(src: &str, file_id: u16) -> (Vec<WithSpan<Token>>, Vec<LexError>) {
    let mut tz = Tokenizer::new(src, file_id);
    let mut tokens = Vec::new();
    while !tz.at_end() {
        if let Some(tok) = tz.next_token() {
            tokens.push(tok);
        }
    }
    (tokens, tz.errors)
}

/// Tokenize and drop `Comment` tokens. `DocComment` tokens are kept.
pub fn tokenize_no_comments(src: &str, file_id: u16) -> (Vec<WithSpan<Token>>, Vec<LexError>) {
    let (toks, errs) = tokenize(src, file_id);
    let out = toks
        .into_iter()
        .filter(|t| !matches!(t.value, Token::Comment(_)))
        .collect();
    (out, errs)
}
