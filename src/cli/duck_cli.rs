use std::path::PathBuf;
use clap::{Parser as CliParser, Subcommand};
use colored::Colorize;

use crate::project::{self, loader::ProjectConfig};

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

pub fn run_cli() {
    // our ascii duck
    println!(
        "{}\n{}{}{}\n{}",
        " _,".bright_yellow(),
        "(".bright_yellow(),
        "o".blue(),
        "<".yellow(),
        "<_)".bright_yellow(),
    );

    let args = DuckCliParser::parse();
    match args.command {
        Commands::Build(build_args) => {
            initiate_build(&build_args);
        },
        Commands::Compile(compile_args) => {
            initiate_compile(&compile_args);
        },
    }
}

fn initiate_build(build_args: &BuildArgs) {
    // project::loader::load_project_env()
}

fn initiate_compile(compile_args: &CompileArgs) {}
