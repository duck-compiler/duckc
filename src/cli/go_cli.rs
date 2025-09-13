use std::{ffi::OsString, path::Path, process::Command};

use crate::tags::Tag;

#[derive(Debug)]
pub enum GoCliErrKind {
    SpawnProcess,
    WaitProcess,
    CompileFailed,
    FmtFailed,
}

pub fn format(go_source_file: &Path) -> Result<(), (String, GoCliErrKind)> {
    let cmd_result = Command::new("go")
        .args([OsString::from("fmt"), go_source_file.as_os_str().to_owned()])
        .spawn()
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't spawn go process\n -> {err}",
                    Tag::Go,
                    Tag::Err,
                ),
                GoCliErrKind::SpawnProcess,
            )
        })?
        .wait()
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't wait for go compile process\n -> {err}",
                    Tag::Go,
                    Tag::Err,
                ),
                GoCliErrKind::WaitProcess,
            )
        })?;

    if !cmd_result.success() {
        return Err((
            format!(
                "{}{} couldn't format the generated go code",
                Tag::Go,
                Tag::Err,
            ),
            GoCliErrKind::FmtFailed,
        ));
    }

    Ok(())
}

pub fn build(
    compile_output_target: &Path,
    go_output_file: &Path,
) -> Result<(), (String, GoCliErrKind)> {
    let cmd_result = Command::new("go")
        .args([
            OsString::from("build"),
            OsString::from("-o"),
            compile_output_target.as_os_str().to_owned(),
            go_output_file.as_os_str().to_owned(),
        ])
        .spawn()
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't spawn go process\n -> {err}",
                    Tag::Go,
                    Tag::Err,
                ),
                GoCliErrKind::SpawnProcess,
            )
        })?
        .wait()
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't wait for go compile process\n -> {err}",
                    Tag::Go,
                    Tag::Err,
                ),
                GoCliErrKind::WaitProcess,
            )
        })?;

    if !cmd_result.success() {
        return Err((
            format!(
                "{}{} couldn't compile the generated go code",
                Tag::Go,
                Tag::Err,
            ),
            GoCliErrKind::CompileFailed,
        ));
    }

    Ok(())
}
