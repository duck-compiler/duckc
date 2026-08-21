use duckc_macros::ast_derive;

use crate::{
    ast::Span, frontend::lexer::{StrPart, StringPart, Tok, Token},
};

pub enum ParsedVariant<'src> {
    A(&'src ())
}

pub struct Parsed<'src> {
    pub variant: ParsedVariant<'src>,
    pub span: Span<'src>,
}

pub struct ParseState<'src, 'toks> {
    toks: &'toks [Token<'src>]
}
