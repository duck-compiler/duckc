use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, sources};
use chumsky::{
    error::Rich,
    input::{BorrowInput, Input},
    span::SimpleSpan,
};

use crate::parse::lexer::Token;

pub mod function_parser;
pub mod generics_parser;
pub mod lexer;
pub mod source_file_parser;
pub mod type_parser;
pub mod use_statement_parser;
pub mod value_parser;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Context {
    pub file_name: &'static str,
    pub file_contents: &'static str,
}

pub type SS = SimpleSpan<usize, Context>;
pub type Spanned<T> = (T, SS);

pub fn make_input<'src>(
    eoi: SS,
    toks: &'src [Spanned<Token>],
) -> impl BorrowInput<'src, Token = Token, Span = SS> {
    toks.map(eoi, |(t, s)| (t, s))
}

pub fn failure(
    file_name: &'static str,
    msg: String,
    label: (String, SS),
    extra_labels: impl IntoIterator<Item = (String, SS)>,
    src: &str,
) -> ! {
    Report::build(ReportKind::Error, (file_name, label.1.into_range()))
        .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
        .with_message(&msg)
        .with_label(
            Label::new((file_name, label.1.into_range()))
                .with_message(label.0)
                .with_color(Color::Red),
        )
        .with_labels(extra_labels.into_iter().map(|label2| {
            Label::new((file_name, label2.1.into_range()))
                .with_message(label2.0)
                .with_color(Color::Yellow)
        }))
        .finish()
        .print(sources([(file_name, src)]))
        .unwrap();
    panic!("{}", msg)
}

pub fn parse_failure(file_name: &str, err: &Rich<impl fmt::Display, SS>, src: &str) -> ! {
    failure(
        file_name.to_string().leak(),
        err.reason().to_string(),
        (
            err.found()
                .map(|c| c.to_string())
                .unwrap_or_else(|| "end of input".to_string()),
            *err.span(),
        ),
        err.contexts()
            .map(|(l, s)| (format!("while parsing this {l}"), *s)),
        src,
    )
}
