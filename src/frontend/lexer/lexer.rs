use std::collections::HashMap;

use duckc_macros::ast_derive;

use crate::{
    ast::Span, frontend::lexer::{StrPart, StringPart, Tok, Token},
};

#[ast_derive]
pub struct LexState<'src> {
    pub file_path: &'src str,
    pub file_text: &'src str,
    pub pos: usize,
    pub emitted_eof: bool,
    pub delim_stack: HashMap<char, Vec<usize>>,
    pub non_fail_diagnostics: Vec<LexDiagnostic>,
}

#[ast_derive]
pub enum LexDiag {
    InvalidCharacter(char),
    UnopenedDelimiter(char),
    UnclosedString,
    NewlineInString,
    InvalidEscapeSequence,
    InvalidRawIdentifier,
    OutOfRangeLiteral,
    EOF,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Radix {
    Binary,
    Decimal,
    Hex,
}

impl Radix {
    fn is_in_radix(self, c: char) -> bool {
        match self {
            Radix::Binary => c == '0' || c == '1',
            Radix::Decimal => c.is_ascii_digit(),
            Radix::Hex => c.is_ascii_hexdigit(),
        }
    }
}

#[ast_derive]
pub struct LexDiagnostic {
    pub variant: LexDiag,
    pub pos: usize,
    pub len: usize,
}

fn is_octal_digit(c: char) -> bool {
    let n = c as u32;
    n >= ('0' as u32) && (n <= '7' as u32)
}

fn is_valid_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_valid_identifier_part(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn matching(c: char) -> Option<char> {
    Some(match c {
        ')' => '(',
        '>' => '<',
        '}' => '{',
        ']' => '[',
        '"' => '"',
        _ => return None,
    })
}

impl<'src> LexState<'src> {
    pub fn init(file_path: &'src str, file_text: &'src str) -> Self {
        Self {
            file_path,
            file_text,
            pos: 0,
            emitted_eof: false,
            delim_stack: Default::default(),
            non_fail_diagnostics: Default::default(),
        }
    }

    fn test_peek(&self, offset: usize, char: char) -> bool {
        return self.file_text[self.pos..]
            .chars()
            .skip(offset)
            .next()
            .is_some_and(|c| c == char);
    }

    fn emit_diagnostic<T>(&mut self, d: LexDiag, len: usize) -> Result<T, LexDiagnostic> {
        let r = Err(LexDiagnostic {
            variant: d,
            pos: self.pos,
            len,
        });
        self.pos += len;
        r
    }

    fn lex_ident(&self, s: &'src str) -> Option<&'src str> {
        let mut chars = s.chars();
        let Some(next_char) = chars.next() else { return None; };
        
        let mut bytes = 0;
        
        if is_valid_identifier_start(next_char) {
            bytes += next_char.len_utf8();
            while let Some(c) = chars.next().filter(|e| is_valid_identifier_start(*e)) {
                bytes += c.len_utf8();
            }
            
            Some(&s[..bytes])
        } else {
            None
        }
    }

    fn lex_int(&self, s: &'src str, radix: Radix) -> Option<(String, usize)> {
        let mut res = String::new();
        let mut chars = s.chars();
        let Some(next_char) = chars.next() else { return None; };
        
        let mut bytes = 0;
        
        if radix.is_in_radix(next_char) {
            bytes += next_char.len_utf8();
            res.push(next_char);
            loop {
                let next = chars.next();
                if let Some(next) = next {
                    if next == '_' {
                        bytes += next.len_utf8();
                        continue;
                    }
                    if radix.is_in_radix(next) {
                        bytes += next.len_utf8();
                        res.push(next);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            
            Some((res, bytes))
        } else {
            None
        }
    }

    pub fn advance_and_return(
        &mut self,
        variant: Tok<'src>,
        len: usize,
    ) -> Result<Token<'src>, LexDiagnostic> {
        let res = Token::new(variant, self.file_path, self.pos, len);
        self.pos += len;
        Ok(res)
    }

    fn get_delims(&mut self, c: char) -> &mut Vec<usize> {
        self.delim_stack.entry(c).or_default()
    }

    fn push_delim(&mut self, c: char) {
        let pos = self.pos;
        self.get_delims(c).push(pos);
    }

    fn pop_delim(&mut self, c: char) -> bool {
        return matching(c).is_some_and(|c| self.get_delims(c).pop().is_some());
    }

    pub fn lex_all(&mut self) -> (Box<[Token<'src>]>, Box<[LexDiagnostic]>) {
        let mut v = Vec::new();
        let mut diagnostics = Vec::new();

        loop {
            let next = self.lex_single();
            match next {
                Ok(tok) => v.push(tok),
                Err(e) => {
                    if matches!(e.variant, LexDiag::EOF) {
                        break;
                    }
                    diagnostics.push(e);
                }
            }
        }

        (v.into_boxed_slice(), diagnostics.into_boxed_slice())
    }

    pub fn lex_single(&mut self) -> Result<Token<'src>, LexDiagnostic> {
        let s = &self.file_text[self.pos..];

        if s.is_empty() {
            if !self.emitted_eof {
                self.emitted_eof = true;
                return Ok(Token::new(Tok::EOF, self.file_path, self.pos, 1));
            } else {
                return self.emit_diagnostic(LexDiag::EOF, 0);
            }
        }
        
        let mut chars = s.chars();

        let next_char = chars
            .next()
            .expect("unreachable because we already checked for empty");

        if s.starts_with("r#") {
            if let Some(lexed) = self.lex_ident(&s[2..]) {
                return self.advance_and_return(Tok::Identifier(lexed), lexed.len() + 2);
            } else {
                return self.emit_diagnostic(LexDiag::InvalidRawIdentifier, 2);
            }
        } else if s.starts_with("0x") {
            if let Some(lexed) = self.lex_int(&s[2..], Radix::Hex) {
                if let Ok(parsed) = u64::from_str_radix(&lexed.0, 16) {
                    return self.advance_and_return(Tok::IntLiteral(parsed), lexed.1 + 2);
                } else {
                    return self.emit_diagnostic(LexDiag::InvalidRawIdentifier, 2);
                }
            } else {
                return self.emit_diagnostic(LexDiag::InvalidRawIdentifier, 2);
            }
        } else if s.starts_with("0b") {
            if let Some(lexed) = self.lex_int(&s[2..], Radix::Binary) {
                if let Ok(parsed) = u64::from_str_radix(&lexed.0, 2) {
                    return self.advance_and_return(Tok::IntLiteral(parsed), lexed.1 + 2);
                } else {
                    return self.emit_diagnostic(LexDiag::InvalidRawIdentifier, 2);
                }
            } else {
                return self.emit_diagnostic(LexDiag::InvalidRawIdentifier, 2);
            }
        }

        if next_char.is_ascii_whitespace() {
            self.pos += 1;
            return self.lex_single();
        }

        match next_char {
            ':' => return self.advance_and_return(Tok::Colon, 1),
            
            ',' => return self.advance_and_return(Tok::Comma, 1),
            
            '(' => {
                self.push_delim(next_char);
                return self.advance_and_return(Tok::LeftParen, 1);
            }

            '[' => {
                self.push_delim(next_char);
                return self.advance_and_return(Tok::LeftSquare, 1);
            }

            '<' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::LessEquals, 2);
                } else if self.test_peek(1, '<') {
                    if self.test_peek(2, '=') {
                        return self.advance_and_return(Tok::ShiftLeftAssign, 3);
                    } else {
                        return self.advance_and_return(Tok::ShiftLeft, 2);
                    }
                } else {
                    self.push_delim(next_char);
                    return self.advance_and_return(Tok::LeftAngle, 1);
                }
            }

            '{' => {
                self.push_delim(next_char);
                return self.advance_and_return(Tok::LeftBrace, 1);
            }

            ')' => {
                if !self.pop_delim(next_char) {
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char), 1);
                }
                return self.advance_and_return(Tok::RightParen, 1);
            }

            ']' => {
                if !self.pop_delim(next_char) {
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char), 1);
                }
                return self.advance_and_return(Tok::RightSquare, 1);
            }

            '>' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::GreaterEquals, 2);
                } else if self.test_peek(1, '>') {
                    if self.test_peek(2, '=') {
                        return self.advance_and_return(Tok::ShiftRightAssign, 3);
                    } else {
                        return self.advance_and_return(Tok::ShiftRight, 2);
                    }
                } else {
                    if !self.pop_delim(next_char) {
                        return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char), 1);
                    }
                    return self.advance_and_return(Tok::RightAngle, 1);
                }
            }

            '}' => {
                if !self.pop_delim(next_char) {
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char), 1);
                }

                return self.advance_and_return(Tok::RightBrace, 1);
            }

            '=' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::DoubleEquals, 2);
                } else {
                    return self.advance_and_return(Tok::SingleEquals, 1);
                }
            }

            '!' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::NotEquals, 2);
                } else {
                    return self.advance_and_return(Tok::Bang, 1);
                }
            }

            '&' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::AmpersandAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Ampersand, 1);
                }
            }

            '|' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::BarAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Bar, 1);
                }
            }

            '+' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::PlusAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Plus, 1);
                }
            }

            '-' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::MinusAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Minus, 1);
                }
            }

            '*' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::MulAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Star, 1);
                }
            }

            '/' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::DivAssign, 2);
                } else if self.test_peek(1, '/') {
                    let end_idx = match s.find('\n') {
                        Some(idx) => idx + 1,
                        _ => s.len(),
                    };

                    let comment_text = trim_suffix(&s[2..end_idx], |c| c == '\r' || c == '\n');
                    let res = Ok(Token::new(
                        Tok::Comment(comment_text),
                        self.file_path,
                        self.pos,
                        comment_text.len() + 2,
                    ));
                    self.pos += end_idx;
                    return res;
                } else {
                    return self.advance_and_return(Tok::Slash, 1);
                }
            }
            
            '%' => {
                if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::PercentAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Percent, 1);
                }
            }

            ';' => {
                return self.advance_and_return(Tok::Semicolon, 1);
            }

            '~' => {
                return self.advance_and_return(Tok::Tilde, 1);
            }

            '"' => {
                let mut bytes = 1_usize;
                let mut current_start = 1;
                let mut current_bytes = 0;
                let mut parts = Vec::new();
                self.push_delim('"');

                loop {
                    let c = chars.next();
                    if let Some(c) = c {
                        if c == '\n' {
                            let pos = self.pos + bytes;
                            self.pos += bytes + 1;
                            return Err(LexDiagnostic {
                                variant: LexDiag::NewlineInString,
                                pos: pos,
                                len: 1,
                            });
                        }

                        if c == '\\' {
                            let part = &s[current_start..(current_start + current_bytes)];

                            if !part.is_empty() {
                                parts.push(StringPart {
                                    variant: StrPart::Text(part),
                                    span: Span {
                                        file_path: self.file_path,
                                        start: self.pos + current_start,
                                        end: self.pos + current_start + current_bytes,
                                    },
                                });
                            }

                            current_start += current_bytes;
                            current_start += 1;
                            bytes += 1;
                            current_bytes = 0;

                            let next = chars.next();
                            let mut is_invalid = true;
                            if let Some(next) = next {
                                match next {
                                    't' => {
                                        parts.push(StringPart {
                                            variant: StrPart::Tab,
                                            span: Span {
                                                file_path: self.file_path,
                                                start: self.pos + bytes,
                                                end: self.pos + bytes + 2,
                                            },
                                        });
                                        current_start += 1;
                                        bytes += 1;
                                        is_invalid = false;
                                    }
                                    'n' => {
                                        parts.push(StringPart {
                                            variant: StrPart::Newline,
                                            span: Span {
                                                file_path: self.file_path,
                                                start: self.pos + bytes,
                                                end: self.pos + bytes + 2,
                                            },
                                        });
                                        current_start += 1;
                                        bytes += 1;
                                        is_invalid = false;
                                    }
                                    next if is_octal_digit(next) => {
                                        current_start += 1;
                                        bytes += 1;

                                        let next2 = chars.next();
                                        let next3 = chars.next();

                                        if next2.is_some() {
                                            current_start += 1;
                                            bytes += 1;
                                        }

                                        if next3.is_some() {
                                            current_start += 1;
                                            bytes += 1;
                                        }

                                        if let Some(next2) = next2
                                            && let Some(next3) = next3
                                            && is_octal_digit(next2)
                                            && is_octal_digit(next3)
                                        {
                                            is_invalid = false;

                                            parts.push(StringPart {
                                                variant: StrPart::Octal(&s[(bytes - 3)..(bytes)]),
                                                span: Span {
                                                    file_path: self.file_path,
                                                    start: self.pos + bytes - 4,
                                                    end: self.pos + bytes,
                                                },
                                            })
                                        }
                                    }
                                    _ => {
                                        current_start += 1;
                                        bytes += 1;
                                    }
                                }
                            }

                            if is_invalid {
                                self.non_fail_diagnostics.push(LexDiagnostic {
                                    variant: LexDiag::InvalidEscapeSequence,
                                    pos: self.pos,
                                    len: 2,
                                });
                            }
                        } else {
                            bytes += c.len_utf8();
                            if c == '"' {
                                self.pop_delim('"');
                                let part = &s[current_start..(current_start + current_bytes)];

                                if !part.is_empty() {
                                    parts.push(StringPart {
                                        variant: StrPart::Text(part),
                                        span: Span {
                                            file_path: self.file_path,
                                            start: self.pos + current_start,
                                            end: self.pos + current_start + current_bytes,
                                        },
                                    });
                                }
                            } else {
                                current_bytes += c.len_utf8();
                            }
                        }

                        if c == '"' {
                            break;
                        }
                    } else {
                        return self.emit_diagnostic(LexDiag::UnclosedString, bytes);
                    }
                }

                let pos = self.pos;
                self.pos += bytes;
                return Ok(Token::new(
                    Tok::StringLiteral(parts.into_boxed_slice()),
                    self.file_path,
                    pos,
                    bytes,
                ));
            }

            _ => {}
        }

        if let Some(lexed) = self.lex_int(s, Radix::Decimal) {
            if let Ok(parsed) = u64::from_str_radix(&lexed.0, 10) {
                let rest = &s[lexed.1..];
                if rest.chars().next() == Some('.') {
                    if let Some(other_part) = self.lex_int(&rest[1..], Radix::Decimal) {
                        let total_len = lexed.1 + 1 + other_part.1;
                        let float_lit = &s[..total_len];
                        if let Ok(float) = float_lit.parse::<f64>() {
                            return self.advance_and_return(Tok::FloatLiteral(float), lexed.1 + 1 + other_part.1)
                        } else {
                            return self.emit_diagnostic(LexDiag::OutOfRangeLiteral, total_len);
                        }
                    }
                } else {
                    return self.advance_and_return(Tok::IntLiteral(parsed), lexed.1);
                }
            } else {
                return self.emit_diagnostic(LexDiag::OutOfRangeLiteral, lexed.1);
            }
        } else if is_valid_identifier_start(next_char) {
            if let Some(ident) = self.lex_ident(s) {
                return self.advance_and_return({
                    use Tok::*;
                    match ident {
                        "true" => BoolLiteral(true),
                        "false" => BoolLiteral(false),
                        
                        "let" => Let,
                        "const" => Const,
                        "fun" => Fun,
                        "as" => As,
                        "return" => Return,
                        "if" => If,
                        "while" => While,
                        "continue" => Continue,
                        "break" => Break,

                        "Int" => Int,
                        "Bool" => Bool,
                        "Float" => Float,
                        "String" => String,

                        _ => Identifier(ident),
                    }
                }, ident.len());
            }
        }

        self.emit_diagnostic(LexDiag::InvalidCharacter(next_char), next_char.len_utf8())
    }
}

fn trim_suffix(s: &str, mut f: impl FnMut(char) -> bool) -> &str {
    let mut end_index = s.len();
    let mut chars = s.chars();

    while let Some(c) = chars.next_back() {
        if !f(c) {
            break;
        }

        end_index -= c.len_utf8();
    }

    return &s[..end_index];
}
