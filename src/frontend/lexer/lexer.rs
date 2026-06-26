use std::collections::HashMap;

use duckc_macros::ast_derive;

use crate::frontend::lexer::{Tok, Token};
use serde::{Deserialize, Serialize};

#[ast_derive]
pub struct LexState<'src> {
    pub file_path: &'src str,
    pub file_text: &'src str,
    pub pos: usize,
    pub emitted_eof: bool,
    pub delim_stack: HashMap<char, Vec<usize>>,
}

#[ast_derive]
pub enum LexDiag {
    InvalidCharacter(char),
    UnopenedDelimiter(char),
    EOF,
}

#[ast_derive]
pub struct LexDiagnostic {
    pub variant: LexDiag,
}

fn matching(c: char) -> Option<char> {
    Some(match c {
        ')' => '(',
        '>' => '<',
        '}' => '{',
        ']' => '[',
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
        }
    }

    fn test_peek(&self, offset: usize, char: char) -> bool {
        return self.file_text[self.pos..]
            .chars()
            .skip(offset)
            .next()
            .is_some_and(|c| c == char);
    }

    fn emit_diagnostic<T>(&self, d: LexDiag) -> Result<T, LexDiagnostic> {
        Err(LexDiagnostic { variant: d })
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
        return matching(c).is_some_and(|c| self.get_delims(c).pop().is_some())
    }

    pub fn lex_single(&mut self) -> Result<Token<'src>, LexDiagnostic> {
        let s = &self.file_text[self.pos..];

        if s.is_empty() {
            if !self.emitted_eof {
                self.emitted_eof = true;
                return Ok(Token::new(Tok::EOF, self.file_path, self.pos, 1));
            } else {
                return self.emit_diagnostic(LexDiag::EOF);
            }
        }

        let mut chars = s.chars();

        let next_char = chars
            .next()
            .expect("unreachable because we already checked for empty");

        if next_char.is_ascii_whitespace() {
            self.pos += 1;
            return self.lex_single();
        }

        match next_char {
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
                    self.pos += 1;
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char));
                }
                return self.advance_and_return(Tok::RightParen, 1);
            }

            ']' => {
                if !self.pop_delim(next_char) {
                    self.pos += 1;
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char));
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
                        self.pos += 1;
                        return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char));
                    }
                    return self.advance_and_return(Tok::RightAngle, 1);
                }
            }

            '}' => {
                if !self.pop_delim(next_char) {
                    self.pos += 1;
                    return self.emit_diagnostic(LexDiag::UnopenedDelimiter(next_char));
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

                    let comment_text = s[2..end_idx].trim_suffix(|c| c == '\r' || c == '\n');
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

            ';' => {
                return self.advance_and_return(Tok::Semicolon, 1);
            }

            '~' => {
                return self.advance_and_return(Tok::Tilde, 1);
            }

            _ => {}
        }

        self.pos += next_char.len_utf8();
        self.emit_diagnostic(LexDiag::InvalidCharacter(next_char))
    }
}
