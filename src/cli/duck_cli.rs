use std::path::PathBuf;
use clap::{Parser as CliParser, Subcommand};

use crate::{dargo::{self, compile::CompileErrKind, init::InitErrKind}, tags::Tag};

#[derive(CliParser, Debug)]
pub struct DuckCliParser {
    #[arg(long, short = 'v', global = true)]
    verbose: bool,

    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    Build(BuildArgs),
    Compile(CompileArgs),
    Init(InitArgs),
}

#[derive(clap::Args, Debug)]
pub struct BuildArgs {
    // Examples:
    // #[arg(long, short = 'o')]
    // optimize: bool,
    // #[arg(long, value_parser = ["x86", "arm"])]
    // arch: Option<String>
}

#[derive(clap::Args, Debug)]
pub struct CompileArgs {
    pub file: PathBuf,
}

#[derive(clap::Args, Debug)]
pub struct InitArgs {
    // Examples:
    // #[arg(long, short = 'o')]
    // optimize: bool,
    // #[arg(long, value_parser = ["x86", "arm"])]
    // arch: Option<String>
}

#[derive(Debug)]
pub enum CliErrKind {
    Init(InitErrKind),
    Compile(CompileErrKind)
}
pub fn run_cli() -> Result<(), (String, CliErrKind)> {

    let args = DuckCliParser::parse();
    match args.command {
        Commands::Build(_build_args) => {
        },
        Commands::Compile(compile_args) => {
            dargo::compile::compile(
                compile_args.file,
                None,
            ).map_err(|err| (
                format!(
                    "{}{}",
                    Tag::Dargo,
                    err.0
                ),
                CliErrKind::Compile(err.1)
            ))?
        },
        Commands::Init(_init_args) => {
            dargo::init::init_project(None)
                .map_err(|err| (format!("{}{}", Tag::Dargo, err.0), CliErrKind::Init(err.1)))?;
        }
    }

    Ok(())
}
