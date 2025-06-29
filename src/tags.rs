use std::fmt::Display;

use colored::{Color, Colorize};

#[derive(Debug, Clone)]
pub enum Tag {
    Err,
    Dargo,
    Note,
    GitHub,
    Twitter,
    Check,
    Lexer,
    Parser,
    Compiler,
    Git,
    Build,
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Err => write!(f, "{}", " error ".on_red().bright_white()),
            Self::Dargo => write!(f, "{}", " dargo ".on_bright_yellow().black()),
            Self::Note => write!(f, "{}", " NOTE ".on_bright_black().bright_white()),
            Self::GitHub => write!(f, "{}", " GitHub ".on_black().bright_white()),
            Self::Twitter => write!(f, "{}", " Twitter ".on_blue().bright_white()),
            Self::Check => write!(f, "{}", " ✓ ".on_green().bright_white()),
            Self::Lexer => write!(f, "{}", " lexer ".on_bright_white().black()),
            Self::Parser => write!(f, "{}", " parser ".on_white().black()),
            Self::Compiler => write!(f, "{}", " compiler ".on_black().bright_white()),
            Self::Git => write!(f, "{}", " git ".on_color(Color::TrueColor { r: 243, g: 20, b: 20 }).bright_white()),
            Self::Build => write!(f, "{}", " build ".on_color(Color::TrueColor { r: 243, g: 20, b: 20 }).bright_white()),
        };
    }
}
