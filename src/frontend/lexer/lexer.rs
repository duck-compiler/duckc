use duckc_macros::ast_derive;

use crate::{
    ast::{Identifier, NodeId, Span},
    frontend::lexer::{StrPart, StringPart, Tok, Token},
};

const MAX_INTERPOLATION_DEPTH: usize = 128;

#[ast_derive]
pub struct LexState<'src> {
    pub file_path: &'src str,
    pub file_text: &'src str,
    pub pos: usize,
    pub emitted_eof: bool,
    pub str_interpolation_depth: usize,
    pub non_fail_diagnostics: Vec<LexDiagnostic>,
}

#[ast_derive]
pub enum LexDiag {
    InvalidCharacter(char),
    UnclosedString,
    InterpolationUnclosed,
    InterpolationTooDeep,
    NewlineInString,
    InvalidEscapeSequence,
    IntLiteralOutOfRange,
    EOF,
}


#[ast_derive]
pub struct LexDiagnostic {
    pub variant: LexDiag,
    pub pos: usize,
    pub len: usize,
}


impl<'src> LexState<'src> {
    pub fn init(file_path: &'src str, file_text: &'src str) -> Self {
        Self {
            file_path,
            file_text,
            pos: 0,
            emitted_eof: false,
            str_interpolation_depth: 0,
            non_fail_diagnostics: Default::default(),
        }
    }

    pub fn lex_single(&mut self) -> Result<Token<'src>, LexDiagnostic> {
        self.skip_whitespace();

        let text = &self.file_text[self.pos..];
        if text.is_empty() {
            if !self.emitted_eof {
                self.emitted_eof = true;
                return Ok(Token::new(Tok::EOF, self.file_path, self.pos, 1));
            } else {
                return self.emit_diagnostic(LexDiag::EOF, 0);
            }
        }

        let next_char = text
            .chars()
            .next()
            .expect("unreachable because we already checked for empty");

        match next_char {
            '(' => {
                return self.advance_and_return(Tok::LeftParen, 1);
            }

            '[' => {
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
                    return self.advance_and_return(Tok::Less, 1);
                }
            }

            '{' => {
                return self.advance_and_return(Tok::LeftBrace, 1);
            }

            ')' => {
                return self.advance_and_return(Tok::RightParen, 1);
            }

            ']' => {
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
                    return self.advance_and_return(Tok::Greater, 1);
                }
            }

            '}' => {
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
                if self.test_peek(1, '&') {
                    return self.advance_and_return(Tok::And, 2);
                } else if self.test_peek(1, '=') {
                    return self.advance_and_return(Tok::AmpersandAssign, 2);
                } else {
                    return self.advance_and_return(Tok::Ampersand, 1);
                }
            }

            '|' => {
                if self.test_peek(1, '|') {
                    return self.advance_and_return(Tok::Or, 2);
                } else if self.test_peek(1, '=') {
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
                } else if self.test_peek(1, '>') {
                    return self.advance_and_return(Tok::Arrow, 2);
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
                    let end_idx = match text.find('\n') {
                        Some(idx) => idx + 1,
                        _ => text.len(),
                    };

                    let comment_text = trim_suffix(&text[2..end_idx], |c| c == '\r' || c == '\n');
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

            ',' => {
                return self.advance_and_return(Tok::Comma, 1);
            }

            '.' => {
                return self.advance_and_return(Tok::Dot, 1);
            }

            ':' => {
                return self.advance_and_return(Tok::Colon, 1);
            }

            '~' => {
                return self.advance_and_return(Tok::Tilde, 1);
            }

            '"' => {
                let start = self.pos;
                self.pos += 1;
                return self.lex_string(start, false);
            }

            char if char.is_ascii_alphabetic() || char == '_' => {
                let end = word_end(text);
                let text = &text[..end];

                if text == "f" && self.test_peek(1, '"') {
                    let start = self.pos;
                    self.pos += 2;
                    return self.lex_string(start, true);
                }

                if let Some(token) = match_keyword(text) {
                    return self.advance_and_return(token, end);
                }

                let identifier = Identifier {
                    id: NodeId::DUMMY,
                    ident: text,
                    span: Span {
                        file_path: self.file_path,
                        start: self.pos,
                        end: self.pos + end,
                    },
                };

                return self.advance_and_return(Tok::Identifier(identifier), end);
            }

            c if c.is_ascii_digit() => {
                let integer_end = digit_end(text);
                let float_digit_len = text[integer_end..]
                    .strip_prefix('.')
                    .map(digit_end)
                    .filter(|len| *len > 0);

                if let Some(float_digit_len) = float_digit_len {
                    let end = integer_end + 1 + float_digit_len;
                    let value = text[..end]
                        .parse::<f64>()
                        .expect("unreachable i hope ':)");

                    return self.advance_and_return(Tok::FloatLiteral(value), end);
                }

                let Ok(value) = text[..integer_end].parse::<u64>() else {
                    return self.emit_diagnostic(LexDiag::IntLiteralOutOfRange, integer_end);
                };

                return self.advance_and_return(Tok::IntLiteral(value), integer_end);
            }

            _ => {}
        }

        self.emit_diagnostic(LexDiag::InvalidCharacter(next_char), next_char.len_utf8())
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

    fn advance_and_return(
        &mut self,
        variant: Tok<'src>,
        len: usize,
    ) -> Result<Token<'src>, LexDiagnostic> {
        let res = Token::new(variant, self.file_path, self.pos, len);
        self.pos += len;
        Ok(res)
    }

    fn report_invalid_escape(&mut self, backslash: usize) {
        self.non_fail_diagnostics.push(LexDiagnostic {
            variant: LexDiag::InvalidEscapeSequence,
            pos: backslash,
            len: self.pos - backslash,
        });
    }

    fn lex_escape(&mut self, parts: &mut Vec<StringPart<'src>>) {
        let backslash = self.pos;
        self.pos += 1;

        let mut chars = self.file_text[self.pos..].chars();
        let Some(escaped) = chars.next() else {
            return self.report_invalid_escape(backslash);
        };

        if escaped == 'n' || escaped == 't' {
            parts.push(StringPart {
                variant: if escaped == 'n' {
                    StrPart::Newline
                } else {
                    StrPart::Tab
                },
                span: Span {
                    file_path: self.file_path,
                    start: backslash,
                    end: backslash + 2,
                },
            });
            self.pos += 1;
            return;
        }

        let is_octal_escape = is_octal_digit(escaped)
            && chars.next().is_some_and(is_octal_digit)
            && chars.next().is_some_and(is_octal_digit);

        if !is_octal_escape {
            self.pos += escaped.len_utf8();
            return self.report_invalid_escape(backslash);
        }

        let digits_pos = self.pos;
        self.pos += 3;

        parts.push(StringPart {
            variant: StrPart::Octal(&self.file_text[digits_pos..self.pos]),
            span: Span {
                file_path: self.file_path,
                start: backslash,
                end: self.pos,
            },
        });
    }

    fn lex_str_interpolation(&mut self) -> Result<Vec<Token<'src>>, LexDiagnostic> {
        let mut tokens = Vec::new();
        let mut depth: usize = 1;

        loop {
            self.skip_whitespace();

            if self.pos == self.file_text.len() {
                return self.emit_diagnostic(LexDiag::InterpolationUnclosed, 0);
            }

            let token = self.lex_single()?;

            match token.variant {
                Tok::LeftBrace => depth += 1,
                Tok::RightBrace => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(tokens);
                    }
                }
                _ => {}
            }

            tokens.push(token);
        }
    }

    fn lex_string(
        &mut self,
        start: usize,
        interpolateable: bool,
    ) -> Result<Token<'src>, LexDiagnostic> {
        let mut str_parts = Vec::new();
        let mut text_start = self.pos;

        loop {
            let Some(next_char) = self.file_text[self.pos..].chars().next() else {
                return Err(LexDiagnostic {
                    variant: LexDiag::UnclosedString,
                    pos: start,
                    len: self.pos - start,
                });
            };

            if next_char == '\n' {
                return self.emit_diagnostic(LexDiag::NewlineInString, 1);
            }

            if next_char == '"' {
                self.push_text_parts(&mut str_parts, text_start);
                self.pos += 1;
                break;
            }

            if next_char == '\\' {
                self.push_text_parts(&mut str_parts, text_start);
                self.lex_escape(&mut str_parts);
                text_start = self.pos;
                continue;
            }

            if interpolateable && next_char == '{' {
                if self.str_interpolation_depth == MAX_INTERPOLATION_DEPTH {
                    return self.emit_diagnostic(LexDiag::InterpolationTooDeep, 1);
                }

                self.push_text_parts(&mut str_parts, text_start);

                let brace_pos = self.pos;
                self.pos += 1;

                self.str_interpolation_depth += 1;
                let tokens = self.lex_str_interpolation();
                self.str_interpolation_depth -= 1;

                str_parts.push(StringPart {
                    variant: StrPart::Interpolation(tokens?),
                    span: Span {
                        file_path: self.file_path,
                        start: brace_pos,
                        end: self.pos,
                    },
                });

                text_start = self.pos;
                continue;
            }

            self.pos += next_char.len_utf8();
        }

        Ok(Token::new(
            Tok::StringLiteral(str_parts),
            self.file_path,
            start,
            self.pos - start,
        ))
    }

    fn push_text_parts(
        &self,
        parts: &mut Vec<StringPart<'src>>,
        text_start: usize
    ) {
        if text_start == self.pos {
            return;
        }

        parts.push(StringPart {
            variant: StrPart::Text(&self.file_text[text_start..self.pos]),
            span: Span {
                file_path: self.file_path,
                start: text_start,
                end: self.pos,
            },
        });
    }

    fn skip_whitespace(&mut self) {
        let rest = &self.file_text[self.pos..];
        let trimmed = rest.trim_start_matches(|c: char| c.is_ascii_whitespace());
        self.pos += rest.len() - trimmed.len();
    }

}

fn match_keyword(text: &str) -> Option<Tok<'_>> {
    Some(match text {
        "let" => Tok::Let,
        "const" => Tok::Const,
        "fn" => Tok::Func,
        "as" => Tok::As,
        "return" => Tok::Return,
        "if" => Tok::If,
        "else" => Tok::Else,
        "while" => Tok::While,
        "continue" => Tok::Continue,
        "break" => Tok::Break,
        "struct" => Tok::Struct,
        "impl" => Tok::Impl,
        "pub" => Tok::Pub,
        "use" => Tok::Use,
        "static" => Tok::Static,
        "int" => Tok::Int,
        "int8" => Tok::Int8,
        "int16" => Tok::Int16,
        "int32" => Tok::Int32,
        "int64" => Tok::Int64,
        "uint" => Tok::Uint,
        "uint8" => Tok::Uint8,
        "uint16" => Tok::Uint16,
        "uint32" => Tok::Uint32,
        "uint64" => Tok::Uint64,
        "bool" => Tok::Bool,
        "float" => Tok::Float,
        "float32" => Tok::Float32,
        "string" => Tok::String,
        "true" => Tok::BoolLiteral(true),
        "false" => Tok::BoolLiteral(false),
        _ => return None,
    })
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


fn word_end(input: &str) -> usize {
    input.find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
        .unwrap_or(input.len())
}

fn digit_end(input: &str) -> usize {
    input.find(|c: char| !c.is_ascii_digit())
        .unwrap_or(input.len())
}

fn is_octal_digit(c: char) -> bool {
    let n = c as u32;
    n >= ('0' as u32) && (n <= '7' as u32)
}
