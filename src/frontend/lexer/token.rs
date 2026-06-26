use duckc_macros::ast_derive;

use crate::ast::{Identifier, Span};
use serde::{Deserialize, Serialize};

#[ast_derive]
pub enum StrPart<'src> {
    Text(&'src str),
    Newline,
    Tab,
    Octal(u64),
}

#[ast_derive]
pub struct StringPart<'src> {
    #[serde(borrow)]
    pub variant: StrPart<'src>,
    pub span: Span<'src>,
}

#[ast_derive]
pub enum Tok<'src> {
    // Keywords
    Let,
    Const,
    Func,
    As,
    Return,
    If,
    While,
    Continue,
    Break,

    // Type keywords
    Int,
    Bool,
    Float,
    String,

    // Control
    LeftParen,
    LeftSquare,
    LeftAngle,
    LeftBrace,

    RightParen,
    RightSquare,
    RightAngle,
    RightBrace,

    LessEquals,
    GreaterEquals,
    DoubleEquals,
    NotEquals,

    SingleEquals,

    ShiftLeft,
    ShiftRight,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    PercentAssign,

    ShiftLeftAssign,
    ShiftRightAssign,
    AmpersandAssign,
    BarAssign,

    Semicolon,
    Ampersand,
    Bang,
    Bar,
    Tilde,

    // Stateful
    #[serde(borrow)]
    Identifier(Identifier<'src>),
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(Vec<StringPart<'src>>),
    Comment(&'src str),

    EOF,
}

#[ast_derive]
pub struct Token<'src> {
    #[serde(borrow)]
    pub variant: Tok<'src>,
    pub span: Span<'src>,
}

impl<'src> Token<'src> {
    pub fn new(variant: Tok<'src>, file_path: &'src str, start: usize, len: usize) -> Token<'src> {
        Self {
            variant,
            span: Span {
                file_path,
                start,
                end: start + len,
            },
        }
    }
}
