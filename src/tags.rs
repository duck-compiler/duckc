use std::fmt::Display;

use colored::Colorize;

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
        };
    }
}
