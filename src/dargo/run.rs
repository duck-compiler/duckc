use colored::Colorize;
use lazy_static::lazy_static;
use std::io::ErrorKind as IOErrKind;
use std::process::Command;

use crate::dargo::cli::{CompileArgs, RunArgs};
use crate::dargo::compile::{CompileErrKind, compile};
use crate::{
    dargo::build::{BuildErrKind, build},
    tags::Tag,
};

#[derive(Debug)]
pub enum RunErrKind {
    BuildErr(BuildErrKind),
    MissingTargetBinary,
    NoBinaryFound,
    CompileErr(CompileErrKind),
    IOErr(IOErrKind),
    Unknown(),
}

lazy_static! {
    static ref COMPILE_TAG: String = " compile ".on_bright_black().bright_white().to_string();
}

pub fn run(run_args: &RunArgs) -> Result<(), (String, RunErrKind)> {
    if run_args.file.is_some() {
        let run_args_file = run_args.file.clone().unwrap();
        let compile_result = compile(CompileArgs {
            file: run_args_file.clone(),
            output_name: None,
            optimize_go: run_args.optimize_go,
        })
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't compile the code\n{}",
                    Tag::Build,
                    Tag::Err,
                    err.0
                ),
                RunErrKind::CompileErr(err.1),
            )
        })?;

        let full_path_name = compile_result.binary_path.canonicalize().map_err(|err| {
            (
                format!(
                    "{}{} couldn't canonicalize path name of just compiled duck binary",
                    Tag::IO,
                    Tag::Err
                ),
                RunErrKind::IOErr(err.kind()),
            )
        })?;

        Command::new(full_path_name.clone())
            .spawn()
            .map_err(|err| {
                (
                    format!(
                        "{}{}{} couldn't spawn duck process",
                        Tag::Run,
                        Tag::IO,
                        Tag::Err
                    ),
                    RunErrKind::IOErr(err.kind()),
                )
            })?
            .wait()
            .map_err(|err| {
                (
                    format!(
                        "{}{}{} couldn't wait for process",
                        Tag::Run,
                        Tag::IO,
                        Tag::Err
                    ),
                    RunErrKind::IOErr(err.kind()),
                )
            })?;

        println!(
            "{}{}{} Successfully run executable output of {} which is located at {}",
            Tag::Dargo,
            Tag::Run,
            Tag::Check,
            run_args_file.to_string_lossy(),
            full_path_name.to_string_lossy()
        );

        return Ok(());
    }

    let build_result = build(&crate::dargo::cli::BuildArgs {
        bin: run_args.bin.clone(),
        output_name: None,
        optimize_go: run_args.optimize_go,
    })
    .map_err(|err| {
        (
            format!(
                "{}{} couldn't build the code\n{}",
                Tag::Build,
                Tag::Err,
                err.0
            ),
            RunErrKind::BuildErr(err.1),
        )
    })?;

    let binary_path = if build_result.binaries.len() > 1 {
        let Some(binary_name) = &run_args.bin else {
            return Err((
                format!(
                    "{}{} missing target binary to run. mutliple binaries are available, specify using --bin <binary_name>\n",
                    Tag::Build,
                    Tag::Err,
                ),
                RunErrKind::MissingTargetBinary
            ))
        };

        let binary = build_result.binaries.iter().find(|binary| *binary.0 == *binary_name);
        if binary.is_none() {
            return Err((
                format!(
                    "{}{} specified binary {} not found",
                    Tag::Build,
                    Tag::Err,
                    binary_name
                ),
                RunErrKind::NoBinaryFound
            ))
        }

        let binary = binary.unwrap();
        binary.1.clone()
    } else {
        let first_binary = build_result.binaries.first();
        if first_binary.is_none() {
            return Err((
                format!(
                    "{}{} missing target binary to run.",
                    Tag::Build,
                    Tag::Err,
                ),
                RunErrKind::NoBinaryFound
            ))
        }

        let first_binary = first_binary.unwrap();
        first_binary.1.clone()
    };

    let full_path_name = binary_path.canonicalize()
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't canonicalize path name of just compiled duck binary",
                    Tag::IO,
                    Tag::Err
                ),
                RunErrKind::IOErr(err.kind()),
            )
        })?;

    Command::new(full_path_name.clone())
        .spawn()
        .map_err(|err| {
            (
                format!(
                    "{}{}{} couldn't spawn duck process",
                    Tag::Run,
                    Tag::IO,
                    Tag::Err
                ),
                RunErrKind::IOErr(err.kind()),
            )
        })?
        .wait()
        .map_err(|err| {
            (
                format!(
                    "{}{}{} couldn't wait for process",
                    Tag::Run,
                    Tag::IO,
                    Tag::Err
                ),
                RunErrKind::IOErr(err.kind()),
            )
        })?;

    println!(
        "{}{}{} Successfully run executable output of current workspace which is located at {}",
        Tag::Dargo,
        Tag::Run,
        Tag::Check,
        full_path_name.to_string_lossy()
    );

    Ok(())
}
