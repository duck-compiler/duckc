use std::{
    env,
    ffi::OsString,
    path::{Path, PathBuf},
    process::Command,
};

use crate::tags::Tag;

#[derive(Debug)]
pub enum GoCliErrKind {
    SpawnProcess,
    WaitProcess,
    CompileFailed,
    FmtFailed,
}

fn resolve_go_bin() -> OsString {
    let home_dir = env::var_os("HOME").or_else(|| env::var_os("USERPROFILE"));

    if let Some(home) = home_dir {
        let mut duck_go = PathBuf::from(home);
        duck_go.push(".duck");
        duck_go.push("go-compiler");
        duck_go.push("bin");
        duck_go.push("go");

        #[cfg(target_os = "windows")]
        let duck_go = duck_go.with_extension("exe");

        if duck_go.exists() {
            return duck_go.into_os_string();
        }
    }

    OsString::from("go")
}

pub fn format(go_source_file: &Path) -> Result<(), (String, GoCliErrKind)> {
    let go_bin = resolve_go_bin();

    let cmd_result = Command::new(go_bin)
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
    let go_bin = resolve_go_bin();

    let cmd_result = Command::new(go_bin)
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
