use duckc_macros::ast_derive;

use crate::ast::{Identifier, Span};

#[ast_derive]
pub enum StrPart<'src> {
    Text(&'src str),
    Newline,
    Tab,
    Octal(&'src str),
    Interpolation(Vec<Token<'src>>),
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
    Else,
    While,
    Continue,
    Break,
    Struct,
    Impl,
    Pub,
    Use,
    Static,

    // Type keywords
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Bool,
    Float,
    Float32,
    String,

    // Control
    LeftParen,
    LeftSquare,
    LeftBrace,

    RightParen,
    RightSquare,
    RightBrace,

    Less,
    Greater,
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
    Comma,
    Dot,
    Colon,
    Arrow,
    Ampersand,
    Bang,
    Bar,
    Tilde,
    And,
    Or,

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
