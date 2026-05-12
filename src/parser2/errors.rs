use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};

use crate::parser2::parser::Span;

/// Render a batch of errors with source context.
///
/// Each entry is `(kind, msg, span)` where `kind` is the phase label shown in the
/// header ("parse", "resolve", "type", ...).
///
/// Pass `color = false` for plain-text output (playground / logs).
/// Pass `color = true` for terminal output.
pub fn render_errors<'a>(
    src: &str,
    file: &str,
    errors: impl IntoIterator<Item = (&'a str, &'a str, Span)>,
    color: bool,
) -> String {
    // ariadne requires the source-id to be 'static, so we use an owned String.
    let file_id: String = file.to_owned();
    let src_owned: String = src.to_owned();
    let mut out = String::new();
    for (kind, msg, span) in errors {
        if !out.is_empty() {
            out.push('\n');
        }
        if span.is_dummy() {
            out.push_str(&format!("[{kind}] {msg}"));
            continue;
        }
        let start = span.start as usize;
        // Ariadne requires end > start; clamp to at least start+1.
        let end = (span.end as usize).max(start + 1);
        let mut buf: Vec<u8> = Vec::new();
        let result = Report::build(ReportKind::Error, (file_id.clone(), start..end))
            .with_config(
                Config::new()
                    .with_index_type(IndexType::Byte)
                    .with_color(color),
            )
            .with_message(format!("[{kind}] {msg}"))
            .with_label(
                Label::new((file_id.clone(), start..end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .write(
                ariadne::sources([(file_id.clone(), src_owned.as_str())]),
                &mut buf,
            );
        if result.is_ok() {
            out.push_str(std::str::from_utf8(&buf).unwrap_or("(encoding error)"));
        } else {
            out.push_str(&format!("[{kind}] {msg}  (at {start}..{end})"));
        }
    }
    out
}
