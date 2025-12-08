use clap::{Parser as CliParser, Subcommand};
use std::path::PathBuf;

use crate::{
    dargo::{self, compile::CompileErrKind, docs::DocsErrKind, init::InitErrKind, run::RunErrKind, test::TestErrKind},
    tags::Tag,
};

use super::{
    build::{self, BuildErrKind},
    clean::CleanErrKind,
};

#[derive(CliParser, Debug)]
pub struct DargoCliParser {
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
    Clean,
    Run(RunArgs),
    Test(TestArgs),
    Docs(DocsGenerateArgs),
}

#[derive(clap::Args, Debug)]
pub struct BuildArgs {
    // #[arg(long, value_parser = ["x86", "arm"])]
    // arch: Option<String>
    #[arg(long, short = 'b')]
    pub bin: Option<String>,
    #[arg(long, short = 'o')]
    pub output_name: Option<String>,
    #[arg(long, short = 'G')]
    pub optimize_go: bool,
}

#[derive(clap::Args, Debug)]
pub struct CompileArgs {
    pub file: PathBuf,
    #[arg(long, short = 'o')]
    pub output_name: Option<String>,
    #[arg(long, short = 'G')]
    pub optimize_go: bool,
}

#[derive(clap::Args, Debug)]
pub struct DocsGenerateArgs {
    pub file: PathBuf,
}

#[derive(clap::Args, Debug)]
pub struct RunArgs {
    pub file: Option<PathBuf>,
    #[arg(long, short = 'G')]
    pub optimize_go: bool,
    #[arg(long, short = 'b')]
    pub bin: Option<String>,
}

#[derive(clap::Args, Debug)]
pub struct TestArgs {
    pub file: Option<PathBuf>,
    #[arg(long, short = 'G')]
    pub optimize_go: bool,
    #[arg(long, short = 'b')]
    pub bin: Option<String>,
}

#[derive(clap::Args, Debug)]
pub struct InitArgs {
    pub project_name: Option<String>,
    // Examples:
    // #[arg(long, short = 'o')]
    // optimize: bool,
    // #[arg(long, value_parser = ["x86", "arm"])]
    // arch: Option<String>
}

#[derive(Debug)]
pub enum CliErrKind {
    Init(InitErrKind),
    Compile(CompileErrKind),
    Build(BuildErrKind),
    Clean(CleanErrKind),
    Run(RunErrKind),
    Test(TestErrKind),
    Docs(DocsErrKind),
}

pub fn run_cli() -> Result<(), (String, CliErrKind)> {
    let args = DargoCliParser::parse();
    match args.command {
        Commands::Build(build_args) => {
            build::build(&build_args).map_err(|err| {
                (
                    format!("{}{}{}", Tag::Dargo, Tag::Build, err.0,),
                    CliErrKind::Build(err.1),
                )
            })?;
        }
        Commands::Compile(compile_args) => {
            dargo::compile::compile(compile_args).map_err(|err| {
                (
                    format!("{}{}", Tag::Dargo, err.0),
                    CliErrKind::Compile(err.1),
                )
            })?;
        }
        Commands::Init(init_args) => {
            dargo::init::init_project(None, init_args)
                .map_err(|err| (format!("{}{}", Tag::Dargo, err.0), CliErrKind::Init(err.1)))?;
        }
        Commands::Clean => {
            dargo::clean::clean().map_err(|err| {
                (
                    format!("{}{} {}", Tag::Dargo, Tag::Clean, err.0,),
                    CliErrKind::Clean(err.1),
                )
            })?;
        }
        Commands::Run(run_args) => {
            dargo::run::run(&run_args).map_err(|err| {
                (
                    format!("{}{}{}", Tag::Dargo, Tag::Run, err.0,),
                    CliErrKind::Run(err.1),
                )
            })?;
        }
        Commands::Test(test_args) => {
            dargo::test::test(&test_args).map_err(|err| {
                (
                    format!("{}{}{}", Tag::Dargo, Tag::Run, err.0,),
                    CliErrKind::Test(err.1),
                )
            })?;
        }
        Commands::Docs(docs_generate_args) => {
            dargo::docs::generate(docs_generate_args)
                .map_err(|err| {
                    (
                        format!("{}{}{}", Tag::Dargo, Tag::Docs, err.0,),
                        CliErrKind::Docs(err.1)
                    )
                })?;
        }
    }

    Ok(())
}
