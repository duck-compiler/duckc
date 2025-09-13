use std::fmt::Display;

use colored::{Color, Colorize};

#[derive(Debug, Clone)]
pub enum Tag {
    Err,
    Dargo,
    Clean,
    Note,
    GitHub,
    Twitter,
    Check,
    Lexer,
    Parser,
    Compiler,
    Git,
    Build,
    Run,
    Dependency,
    Setup,
    IO,
    Go,
    TypeResolve,
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
            Self::Git => write!(
                f,
                "{}",
                " git "
                    .on_color(Color::TrueColor {
                        r: 243,
                        g: 20,
                        b: 20
                    })
                    .bright_white()
            ),
            Self::Build => write!(
                f,
                "{}",
                " build "
                    .on_color(Color::TrueColor {
                        r: 43,
                        g: 20,
                        b: 20
                    })
                    .bright_white()
            ),
            Self::Run => write!(
                f,
                "{}",
                " run "
                    .on_color(Color::TrueColor {
                        r: 43,
                        g: 80,
                        b: 20
                    })
                    .bright_white()
            ),
            Self::Dependency => write!(
                f,
                "{}",
                " dependency "
                    .on_color(Color::TrueColor {
                        r: 23,
                        g: 10,
                        b: 120
                    })
                    .bright_white()
            ),
            Self::Setup => write!(
                f,
                "{}",
                " setup "
                    .on_color(Color::TrueColor {
                        r: 23,
                        g: 120,
                        b: 20
                    })
                    .bright_white()
            ),
            Self::IO => write!(
                f,
                "{}",
                " IO "
                    .on_color(Color::TrueColor {
                        r: 23,
                        g: 120,
                        b: 20
                    })
                    .bright_white()
            ),
            Self::Clean => write!(f, "{}", " clean ".on_bright_magenta().bright_white()),
            Self::Go => write!(f, "{}", " go ".on_bright_blue().bright_white()),
            Self::TypeResolve => write!(f, "{}", " type resolve ".on_bright_blue().bright_white()),
        };
    }
}
