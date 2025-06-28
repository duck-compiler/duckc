use std::fmt::{write, Display};

use colored::{ColoredString, Colorize};

#[derive(Debug, Clone)]
pub enum Tag {
    Err,
    Note,
    GitHub,
    Twitter,
}

impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return match self {
            Self::Err =>     write!(f, "{}", " Error ".on_red().bright_white()),
            Self::Note =>    write!(f, "{}", " NOTE ".on_bright_black().white()),
            Self::GitHub =>  write!(f, "{}", " GitHub ".on_black().bright_white()),
            Self::Twitter => write!(f, "{}", " Twitter ".on_blue().bright_white()),
        }
    }
}
