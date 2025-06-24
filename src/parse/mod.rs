use std::fmt;

use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{error::Rich, input::{BorrowInput, Input}, span::SimpleSpan};

use crate::parse::lexer::Token;

pub mod assignment_and_declaration_parser;
pub mod function_parser;
pub mod lexer;
pub mod source_file_parser;
pub mod type_parser;
pub mod use_statement_parser;
pub mod value_parser;

pub type Spanned<T> = (T, SimpleSpan);

pub fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token>],
) -> impl BorrowInput<'src, Token = Token, Span = SimpleSpan> {
    toks.map(eoi, |(t, s)| (t, s))
}

pub fn make_no_span_input<'src>(
    toks: &'src [Spanned<Token>]
) -> impl BorrowInput<'src, Token = Token, Span = SimpleSpan> {
    make_input((0..10).into(), toks)
}

pub fn failure(
    file_name: &'static str,
    msg: String,
    label: (String, SimpleSpan),
    extra_labels: impl IntoIterator<Item = (String, SimpleSpan)>,
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
    std::process::exit(1)
}

pub fn parse_failure(file_name: &str, err: &Rich<impl fmt::Display>, src: &str) -> ! {
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
