#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion
)]

use std::{error::Error, fs::{self, File}, io::Write, path::{Path, PathBuf}};

use chumsky::{error::Rich, Parser};
use colored::Colorize;
use parse::{lexer::{lexer, Token}, source_file_parser::SourceFile, Spanned};
use semantics::typechecker::{self, TypeEnv};
use tags::Tag;

use crate::parse::{
    Context, SS, make_input, parse_failure,
    source_file_parser::source_file_parser,
};

use lazy_static::lazy_static;

pub mod cli;
pub mod emit;
pub mod fixup;
pub mod parse;
pub mod semantics;
pub mod dargo;
pub mod tags;

lazy_static! {
    static ref DOT_DUCK_DIR: PathBuf = {
        let duck_dir = Path::new("./.duck");
        if duck_dir.exists() {
            return duck_dir.to_path_buf();
        }

        if let Err(err) = fs::create_dir(duck_dir) {
            dbg!(err);
            println!("Couldn't create .duck dir");
        }

        return duck_dir.to_path_buf();
    };
}

fn lex(
    file_name: &'static str,
    file_contents: &'static str,
) -> Vec<Spanned<Token>> {
    let (lex, lex_errors) = lexer(file_name, file_contents)
        .parse(file_contents)
        .into_output_errors();

    lex_errors.into_iter().for_each(|e| {
            parse_failure(
                file_name,
                &Rich::<&str, SS>::custom(
                    SS {
                        start: e.span().start,
                        end: e.span().end,
                        context: crate::parse::Context {
                            file_name,
                            file_contents,
                        },
                    },
                    format!("{}{} {}", Tag::Lexer, Tag::Err, e.reason()),
                ),
                &file_contents,
            );
        });

    lex.unwrap()
}

fn parse_src_file(
    src_file: &PathBuf,
    src_file_name: &'static str,
    src_file_file_contents: &'static str,
    tokens: Vec<Spanned<Token>>
) -> SourceFile {
    let (src_file, parse_errors) = source_file_parser(
           {
               let mut src_file_clone = src_file.clone();
               src_file_clone.pop();
               src_file_clone
           },
           make_input,
       )
       .parse(make_input(
           SS {
               start: 0,
               end: src_file_file_contents.len(),
               context: Context {
                   file_name: src_file_name,
                   file_contents: src_file_file_contents,
               },
           },
           &tokens,
       ))
       .into_output_errors();

    parse_errors.into_iter().for_each(|e| {
            parse_failure(src_file_name, &e, &src_file_file_contents);
        });

    src_file.unwrap().flatten()
}

fn typecheck(src_file_ast: &mut SourceFile) -> TypeEnv {
    let mut type_env = TypeEnv::default();
    typechecker::typeresolve_source_file(src_file_ast, &mut type_env);

    type_env
}

fn write_in_duck_dotdir(file_name: &str, content: &str) -> PathBuf {
    let target_file = {
        let mut target_file_path = DOT_DUCK_DIR.clone();
        target_file_path.push(file_name);
        target_file_path
    };

    let mut file = File::create(target_file.clone())
        .expect("couldn't create file in duck dot dir"); // TODO error handling
    file.write_all(content.as_bytes())
        .expect("couldn't write file in duck dot dir"); // TODO error handling

    target_file
}

fn main() -> Result<(), Box<dyn Error>> {
    println!(
        "{}\n{}{}{}  Oops, seems like there's something wrong!\n{}",
        " _,".bright_yellow().bold(),
        "(".bright_yellow().bold(),
        "o".blue().bold(),
        "<".yellow().bold(),
        "<_)".bright_yellow().bold(),
    );

    let cli_result = cli::duck_cli::run_cli();
    if let Err(err) = cli_result {
        println!("{}", err.0)
    }

    Ok(())
}
