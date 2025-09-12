use colored::Colorize;
use lazy_static::lazy_static;
use std::{ffi::OsString, fs, path::PathBuf, process::Command};
use std::io::ErrorKind as IOErrKind;

use crate::dargo::cli::RunArgs;
use crate::dargo::compile::{compile, CompileErrKind};
use crate::{
    cli::go_cli::{self, GoCliErrKind}, dargo::build::{build, BuildErrKind}, emit::ir::join_ir, lex, parse_src_file, tags::Tag, typecheck, write_in_duck_dotdir, DARGO_DOT_DIR
};

#[derive(Debug)]
pub enum RunErrKind {
    BuildErr(BuildErrKind),
    CompileErr(CompileErrKind),
    IOErr(IOErrKind),
    Unknown()
}

lazy_static! {
    static ref COMPILE_TAG: String = " compile ".on_bright_black().bright_white().to_string();
}

pub fn run(
    run_args: &RunArgs
) -> Result<(), (String, RunErrKind)> {
    if run_args.file.is_some() {
        let run_args_file = run_args.file.clone().unwrap();
        let compile_result = compile(run_args_file.clone(), None)
            .map_err(|err| (
                format!(
                    "{}{} couldn't compile the code\n{}",
                    Tag::Build,
                    Tag::Err,
                    err.0
                ),
                RunErrKind::CompileErr(err.1)
            ))?;

        let full_path_name = compile_result.binary_path.canonicalize()
            .map_err(|err| (format!("{}{} couldn't canonicalize path name of just compiled duck binary", Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?;

        Command::new(full_path_name.clone())
            .output()
            .map_err(|err| (format!("{}{} couldn't spawn duck process", Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?;

        println!("{}{}{} Successfully run executable output of {} which is located at {}", Tag::Dargo, Tag::Run, Tag::Check, run_args_file.to_string_lossy(), full_path_name.to_string_lossy());

        return Ok(());
    }

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

    let full_path_name = build_result.binary_path.canonicalize()
        .map_err(|err| (format!("{}{} couldn't canonicalize path name of just compiled duck binary", Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?;

    Command::new(full_path_name.clone())
        .spawn()
        .map_err(|err| (format!("{}{}{} couldn't spawn duck process", Tag::Run, Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?
        .wait()
        .map_err(|err| (format!("{}{}{} couldn't wait for duck process", Tag::Run, Tag::IO, Tag::Err), RunErrKind::IOErr(err.kind())))?;

    println!("{}{}{} Successfully run executable output of current workspace which is located at {}", Tag::Dargo, Tag::Run, Tag::Check, full_path_name.to_string_lossy());

    Ok(())
}
