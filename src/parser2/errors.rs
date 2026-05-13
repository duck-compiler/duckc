use std::collections::HashMap;

use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};

use crate::parser2::parser::Span;

/// Render a batch of errors with source context.
///
/// `primary_src`/`primary_file` are for the user's source (file_id=1).
/// `extra_sources` maps file_id to (display name, content) for std/module files.
///
/// Pass `color = false` for plain-text output (playground / logs).
/// Pass `color = true` for terminal output.
pub fn render_errors<'a>(
    primary_src: &str,
    primary_file: &str,
    extra_sources: Option<&HashMap<u16, (String, String)>>,
    errors: impl IntoIterator<Item = (&'a str, &'a str, Span)>,
    color: bool,
) -> String {
    let mut out = String::new();
    for (kind, msg, span) in errors {
        if !out.is_empty() {
            out.push('\n');
        }
        if span.is_dummy() {
            out.push_str(&format!("[{kind}] {msg}"));
            continue;
        }

        // determine which source file this span belongs to
        let (src_name, src_content): (String, &str) = if span.file_id == 1 {
            (primary_file.to_owned(), primary_src)
        } else if let Some(src_ref) = extra_sources.and_then(|m| m.get(&span.file_id)) {
            (src_ref.0.clone(), src_ref.1.as_str())
        } else {
            (primary_file.to_owned(), primary_src)
        };

        let start = span.start as usize;
        let end = (span.end as usize).max(start + 1);
        let mut buf: Vec<u8> = Vec::new();

        // collect all sources so ariadne can resolve any file_id in labels
        let mut all_sources: Vec<(String, &str)> = vec![(src_name.clone(), src_content)];
        if let Some(m) = extra_sources {
            for (fid, (name, content)) in m {
                if *fid != span.file_id {
                    all_sources.push((name.clone(), content.as_str()));
                }
            }
        }
        if span.file_id != 1 {
            all_sources.push((primary_file.to_owned(), primary_src));
        }

        let result = Report::build(ReportKind::Error, (src_name.clone(), start..end))
            .with_config(
                Config::new()
                    .with_index_type(IndexType::Byte)
                    .with_color(color),
            )
            .with_message(format!("[{kind}] {msg}"))
            .with_label(
                Label::new((src_name.clone(), start..end))
                    .with_message(msg)
                    .with_color(Color::Red),
            )
            .finish()
            .write(ariadne::sources(all_sources), &mut buf);

        if result.is_ok() {
            out.push_str(std::str::from_utf8(&buf).unwrap_or("(encoding error)"));
        } else {
            out.push_str(&format!("[{kind}] {msg}  (at {start}..{end})"));
        }
    }
    out
}
