#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion
)]

use std::{
    env, error::Error, fs::{self, File}, io::Write, path::{Path, PathBuf}
};

use chumsky::{Parser, error::Rich};
use colored::Colorize;
use parse::{
    Spanned,
    lexer::{Token, lexer},
    source_file_parser::SourceFile,
};
use semantics::typechecker::{self, TypeEnv};
use tags::Tag;

use crate::parse::{
    Context, SS, make_input, parse_failure, source_file_parser::source_file_parser,
};

use lazy_static::lazy_static;

pub mod cli;
pub mod dargo;
pub mod emit;
pub mod fixup;
pub mod parse;
pub mod semantics;
pub mod tags;

lazy_static! {
    static ref DARGO_DOT_DIR: PathBuf = {
        fn require_sub_dir(str: &str) {
            let Ok(current_dir) = env::current_dir() else {
                println!(
                    "{}{} coulnd't read current dir",
                    Tag::Dargo,
                    Tag::Err,
                );
                panic!()
            };

            let required_dir = dbg!({
                let mut current_dir_clone = current_dir.clone();
                current_dir_clone.push(str);
                current_dir_clone
            });

            if required_dir.exists() {
                return;
            }

            if let Err(err) = fs::create_dir(required_dir.clone()) {
                dbg!(err);
                println!(
                    "{}{} Couldn't create {} dot dir in current directory.",
                    Tag::Dargo,
                    Tag::Err,
                    required_dir.to_string_lossy()
                );
            }
        }

        let duck_dir = Path::new(".dargo");

        require_sub_dir(".dargo");
        require_sub_dir(".dargo/git");
        require_sub_dir(".dargo/project");

        return duck_dir.to_path_buf();
    };
}

fn lex(file_name: &'static str, file_contents: &'static str) -> Vec<Spanned<Token>> {
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
            file_contents,
        );
    });

    lex.unwrap()
}

fn parse_src_file(
    src_file: &Path,
    src_file_name: &'static str,
    src_file_file_contents: &'static str,
    tokens: Vec<Spanned<Token>>,
) -> SourceFile {
    let (src_file, parse_errors) = source_file_parser(
        {
            let mut src_file_clone = src_file.to_path_buf();
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
        parse_failure(src_file_name, &e, src_file_file_contents);
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
        let mut target_file_path = DARGO_DOT_DIR.clone();
        target_file_path.push(file_name);
        target_file_path
    };

    let mut file = File::create(target_file.clone()).expect("couldn't create file in duck dot dir"); // TODO error handling
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

    let cli_result = dargo::cli::run_cli();
    if let Err(err) = cli_result {
        println!("{}", err.0)
    }

    Ok(())
}
