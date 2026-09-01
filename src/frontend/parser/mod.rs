mod error;
mod expression;
mod statement;
mod token_stream;
mod type_expression;

pub use error::ParseError;

use crate::ast::{AstRoot, Identifier, NodeId, NodeIdGenerator, Span, Statement};
use crate::frontend::lexer::Tok;
use crate::frontend::parser::token_stream::TokenStream;

const MAX_RECURSION_DEPTH: u32 = 64;

pub fn parse_module<'src>(
    file_path: &'src str,
    source: &'src str,
) -> Result<AstRoot<'src>, ParseError<'src>> {
    let mut parser = Parser {
        tokens: TokenStream::lex(file_path, source)?,
        node_ids: NodeIdGenerator::new(),
        allow_struct_init: true,
        depth: 0,
    };

    let mut statements = Vec::new();
    while !parser.tokens.at_end() {
        statements.push(parser.parse_stmt()?);
    }

    Ok(AstRoot {
        statements
    })
}

struct Parser<'src> {
    tokens: TokenStream<'src>,
    node_ids: NodeIdGenerator,
    allow_struct_init: bool,
    depth: u32,
}

impl<'src> Parser<'src> {
    fn fresh_id(&mut self) -> NodeId {
        self.node_ids.fresh()
    }

    fn span_from(&self, start: Span<'src>) -> Span<'src> {
        Span {
            file_path: start.file_path,
            start: start.start,
            end: self.tokens.last_end(),
        }
    }

    fn parse_ident(&mut self, ident_desc: &str) -> Result<Identifier<'src>, ParseError<'src>> {
        let Tok::Identifier(identifier) = self.tokens.current() else {
            return Err(self.tokens.unexpected(ident_desc));
        };

        let (ident, span) = (identifier.ident, identifier.span);
        self.tokens.advance();

        Ok(Identifier {
            id: self.fresh_id(),
            ident,
            span,
        })
    }

    fn recurse_parse<T>(
        &mut self,
        parse: impl FnOnce(&mut Self) -> Result<T, ParseError<'src>>,
    ) -> Result<T, ParseError<'src>> {
        if self.depth == MAX_RECURSION_DEPTH {
            return Err(ParseError::new(
                "input is nested to deep",
                self.tokens.current_span(),
            ));
        }

        self.depth += 1;
        let parsed = parse(self);
        self.depth -= 1;

        parsed
    }

    fn parse_with_struct_init<T>(
        &mut self,
        allowed: bool,
        parse: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let previous = self.allow_struct_init;

        self.allow_struct_init = allowed;
        let parsed = parse(self);

        self.allow_struct_init = previous;

        parsed
    }
}
