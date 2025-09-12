use colored::Colorize;
use lazy_static::lazy_static;
use std::{ffi::OsString, fs, path::PathBuf, process::Command};
use std::io::ErrorKind as IOErrKind;

use crate::{
    cli::go_cli::{self, GoCliErrKind}, dargo::build::{build, BuildErrKind}, emit::ir::join_ir, lex, parse_src_file, tags::Tag, typecheck, write_in_duck_dotdir, DARGO_DOT_DIR
};

#[derive(Debug)]
pub enum RunErrKind {
    BuildErr(BuildErrKind),
    IOErr(IOErrKind),
    Unknown()
}

lazy_static! {
    static ref COMPILE_TAG: String = " compile ".on_bright_black().bright_white().to_string();
}

pub fn run(
) -> Result<(), (String, RunErrKind)> {
    let build_result = build(&crate::dargo::cli::BuildArgs { })
        .map_err(|err| (
            format!(
                "{}{} couldn't build the code\n{}",
                Tag::Build,
                Tag::Err,
                err.0
            ),
            RunErrKind::BuildErr(err.1)
        ))?;

    Command::new(build_result.binary_path)
        .spawn()
        .map_err(|err| (format!("{}{}{} couldn't spawn duck process", Tag::Run, Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?
        .wait()
        .map_err(|err| (format!("{}{}{} couldn't wait for duck process", Tag::Run, Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?;

    Ok(())
}
