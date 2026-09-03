use crate::ast::Span;
use crate::frontend::lexer::{LexState, Tok, Token};
use crate::frontend::parser::error::ParseError;

enum ClosingAngle {
    WholeToken,
    TokenWithRemainder(AngleRemainder),
}

#[derive(Clone, Copy)]
enum AngleRemainder {
    Greater,
    GreaterEquals,
    SingleEquals,
}

impl AngleRemainder {
    fn len(self) -> usize {
        match self {
            AngleRemainder::GreaterEquals => 2,
            AngleRemainder::Greater | AngleRemainder::SingleEquals => 1,
        }
    }
}

pub struct TokenStream<'src> {
    source: &'src str,
    tokens: Vec<Token<'src>>,
    index: usize,
    // tokens that remained because there's for example something like this:
    //      Array<Array<>>
    // where the tokens would look like the following
    //      ident("Array"), lt, ident("array"), rhs
    // but actually the parser needs to consume only on gt at the end so one gt will remain
    remainder: Option<AngleRemainder>,
    last_end: usize, // end of the last token
}

#[derive(Clone, Copy)]
pub struct Mark {
    index: usize,
    remainder: Option<AngleRemainder>,
    last_end: usize,
}

impl<'src> TokenStream<'src> {
    pub fn lex(file_path: &'src str, source: &'src str) -> Result<TokenStream<'src>, ParseError<'src>> {
        let (tokens, warnings) = LexState::tokenize(file_path, source)
            .map_err(|diagnostic| ParseError::from_lex_diagnostic(diagnostic, file_path))?;

        for warning in warnings {
            eprintln!("warning: {}", ParseError::from_lex_diagnostic(warning, file_path));
        }

        let tokens = tokens
            .into_iter()
            .filter(|token| !matches!(token.variant, Tok::Comment(_)))
            .collect();

        Ok(TokenStream {
            source,
            tokens,
            index: 0,
            remainder: None,
            last_end: 0,
        })
    }

    pub fn current(&self) -> &Tok<'src> {
        match self.remainder {
            Some(AngleRemainder::Greater) => &Tok::Greater,
            Some(AngleRemainder::GreaterEquals) => &Tok::GreaterEquals,
            Some(AngleRemainder::SingleEquals) => &Tok::SingleEquals,
            None => &self.tokens[self.index].variant,
        }
    }

    pub fn current_span(&self) -> Span<'src> {
        let span = self.tokens[self.index].span;

        match self.remainder {
            Some(remainder) => Span { start: span.end - remainder.len(), ..span },
            None => span,
        }
    }

    pub fn at_end(&self) -> bool {
        matches!(self.current(), Tok::EOF)
    }

    pub fn last_end(&self) -> usize {
        self.last_end
    }

    pub fn source_slice(&self, span: Span<'src>) -> &'src str {
        &self.source[span.start..span.end]
    }

    pub fn advance(&mut self) -> Span<'src> {
        let span = self.current_span();

        self.last_end = self.tokens[self.index].span.end;
        self.remainder = None;

        if self.index + 1 < self.tokens.len() {
            self.index += 1;
        }

        span
    }

    pub fn take_if(&mut self, expected: Tok<'src>) -> bool {
        if *self.current() != expected {
            return false;
        }

        self.advance();
        true
    }

    pub fn expect(&mut self, expected: Tok<'src>, description: &str) -> Result<Span<'src>, ParseError<'src>> {
        if *self.current() != expected {
            return Err(self.unexpected(description));
        }

        Ok(self.advance())
    }

    pub fn take_closing_angle(&mut self) -> bool {
        match closing_angle_kind(self.current()) {
            None => false,
            Some(ClosingAngle::WholeToken) => {
                self.advance();
                true
            }
            Some(ClosingAngle::TokenWithRemainder(remainder)) => {
                self.last_end = self.tokens[self.index].span.end - remainder.len();
                self.remainder = Some(remainder);
                true
            }
        }
    }

    pub fn mark(&self) -> Mark {
        Mark {
            index: self.index,
            remainder: self.remainder,
            last_end: self.last_end,
        }
    }

    pub fn reset(&mut self, mark: Mark) {
        self.index = mark.index;
        self.remainder = mark.remainder;
        self.last_end = mark.last_end;
    }

    pub fn unexpected(&self, expected: &str) -> ParseError<'src> {
        ParseError::new(
            format!("expected {expected}, found {}", describe(self.current())),
            self.current_span(),
        )
    }
}

fn closing_angle_kind(variant: &Tok<'_>) -> Option<ClosingAngle> {
    Some(match variant {
        Tok::Greater => ClosingAngle::WholeToken,
        Tok::GreaterEquals => ClosingAngle::TokenWithRemainder(AngleRemainder::SingleEquals),
        Tok::ShiftRight => ClosingAngle::TokenWithRemainder(AngleRemainder::Greater),
        Tok::ShiftRightAssign => ClosingAngle::TokenWithRemainder(AngleRemainder::GreaterEquals),
        _ => return None,
    })
}

fn describe(variant: &Tok<'_>) -> String {
    let text = match variant {
        Tok::Let => "`let`",
        Tok::Const => "`const`",
        Tok::Func => "`fn`",
        Tok::As => "`as`",
        Tok::Return => "`return`",
        Tok::If => "`if`",
        Tok::Else => "`else`",
        Tok::While => "`while`",
        Tok::Continue => "`continue`",
        Tok::Break => "`break`",
        Tok::Struct => "`struct`",
        Tok::Impl => "`impl`",
        Tok::Pub => "`pub`",
        Tok::Use => "`use`",
        Tok::Static => "`static`",
        Tok::Int => "`int`",
        Tok::Int8 => "`int8`",
        Tok::Int16 => "`int16`",
        Tok::Int32 => "`int32`",
        Tok::Int64 => "`int64`",
        Tok::Uint => "`uint`",
        Tok::Uint8 => "`uint8`",
        Tok::Uint16 => "`uint16`",
        Tok::Uint32 => "`uint32`",
        Tok::Uint64 => "`uint64`",
        Tok::Bool => "`bool`",
        Tok::Float => "`float`",
        Tok::Float32 => "`float32`",
        Tok::String => "`string`",
        Tok::LeftParen => "`(`",
        Tok::LeftSquare => "`[`",
        Tok::LeftBrace => "`{`",
        Tok::RightParen => "`)`",
        Tok::RightSquare => "`]`",
        Tok::RightBrace => "`}`",
        Tok::Less => "`<`",
        Tok::Greater => "`>`",
        Tok::LessEquals => "`<=`",
        Tok::GreaterEquals => "`>=`",
        Tok::DoubleEquals => "`==`",
        Tok::NotEquals => "`!=`",
        Tok::SingleEquals => "`=`",
        Tok::ShiftLeft => "`<<`",
        Tok::ShiftRight => "`>>`",
        Tok::Plus => "`+`",
        Tok::Minus => "`-`",
        Tok::Star => "`*`",
        Tok::Slash => "`/`",
        Tok::Percent => "`%`",
        Tok::PlusAssign => "`+=`",
        Tok::MinusAssign => "`-=`",
        Tok::MulAssign => "`*=`",
        Tok::DivAssign => "`/=`",
        Tok::PercentAssign => "`%=`",
        Tok::ShiftLeftAssign => "`<<=`",
        Tok::ShiftRightAssign => "`>>=",
        Tok::AmpersandAssign => "`&=`",
        Tok::BarAssign => "`|=`",
        Tok::Semicolon => "`;`",
        Tok::Comma => "`,`",
        Tok::Dot => "`.`",
        Tok::Colon => "`:`",
        Tok::Arrow => "`->`",
        Tok::Ampersand => "`&`",
        Tok::Bang => "`!`",
        Tok::Bar => "`|`",
        Tok::Tilde => "`~`",
        Tok::And => "`&&`",
        Tok::Or => "`||`",
        Tok::IntLiteral(_) => "an integer literal",
        Tok::FloatLiteral(_) => "a float literal",
        Tok::BoolLiteral(_) => "a boolean literal",
        Tok::StringLiteral(_) => "a string literal",
        Tok::Comment(_) => "a comment",
        Tok::EOF => "end of file",
        Tok::Identifier(identifier) => return format!("`{}`", identifier.ident),
    };

    text.to_string()
}
