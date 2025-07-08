use std::{ffi::OsString, path::Path, process::Command};

use crate::tags::Tag;

#[derive(Debug)]
pub enum GoCliErrKind {
    SpawnProcess,
    WaitProcess,
}

pub fn build(
    compile_output_target: &Path,
    go_output_file: &Path,
) -> Result<(), (String, GoCliErrKind)> {
    Command::new("go")
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
    Ok(())
}
