use std::fmt::{Display, Formatter, Result as FmtResult};

use crate::ast::Span;
use crate::frontend::lexer::{LexDiag, LexDiagnostic};

#[derive(Debug)]
pub struct ParseError<'src> {
    pub message: Box<str>,
    pub location: Span<'src>,
}

impl<'src> ParseError<'src> {
    pub fn new(message: impl Into<String>, location: Span<'src>) -> ParseError<'src> {
        ParseError {
            message: message.into().into_boxed_str(),
            location,
        }
    }

    pub fn from_lex_diagnostic(
        diagnostic: LexDiagnostic,
        file_path: &'src str,
    ) -> ParseError<'src> {
        let message = match diagnostic.variant {
            LexDiag::InvalidCharacter(character) => format!("invalid character `{character}`"),
            LexDiag::UnclosedString => "unterminated string literal".to_string(),
            LexDiag::InterpolationUnclosed => "unterminated string interpolation".to_string(),
            LexDiag::InterpolationTooDeep => "string interpolation is nested too deeply".to_string(),
            LexDiag::NewlineInString => "string literal may not contain a newline".to_string(),
            LexDiag::InvalidEscapeSequence => "invalid escape sequence".to_string(),
            LexDiag::IntLiteralOutOfRange => "integer literal is out of range".to_string(),
            LexDiag::EOF => "unexpected end of file".to_string(),
        };

        ParseError::new(
            message,
            Span {
                file_path,
                start: diagnostic.pos,
                end: diagnostic.pos + diagnostic.len,
            },
        )
    }
}

impl<'src> Display for ParseError<'src> {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> FmtResult {
        write!(
            formatter,
            "{}:{}..{}: {}",
            self.location.file_path, self.location.start, self.location.end, self.message
        )
    }
}
