#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion
)]

use std::{error::Error, ffi::OsString, fs, path::{Path, PathBuf}};

use chumsky::{error::Rich, Parser};
use clap::{Parser as CliParser, arg, command};
use parse::{lexer::{lexer, Token}, source_file_parser::SourceFile, Spanned};
use semantics::typechecker::{self, TypeEnv};

use crate::{
    emit::ir::join_ir,
    parse::{
        Context, SS, make_input, parse_failure,
        source_file_parser::source_file_parser,
    },
};

use lazy_static::lazy_static;

pub mod emit;
pub mod fixup;
pub mod parse;
pub mod semantics;

#[derive(CliParser, Debug)]
#[command(author, version, about, long_about = None)]
struct CompilerArgs {
    // input_files: Vec<PathBuf>,
    input_file: PathBuf,
    #[arg(short)]
    o: Option<String>,

    #[arg(long)]
    lex: bool,
    #[arg(long)]
    parse: bool,
    #[arg(long)]
    emit_go: bool,
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
                    "Lex Error",
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

fn main() -> Result<(), Box<dyn Error>> {
    let compiler_args = CompilerArgs::parse();

    let src_file: PathBuf = compiler_args.input_file;
    let src_file_name: &'static str = src_file
        .file_name()
        .expect("couldn't read file name")
        .to_str()
        .expect("invalid utf-8 string")
        .to_string()
        .leak();
    let src_file_file_contents: &'static str = fs::read_to_string(&src_file)
        .expect("couldn't read file")
        .to_string()
        .leak();

    let tokens = lex(src_file_name, src_file_file_contents);
    let mut src_file_ast = parse_src_file(&src_file, src_file_name, src_file_file_contents, tokens);
    let mut type_env = typecheck(&mut src_file_ast);
    let _go_code = join_ir(&src_file_ast.emit("main".into(), &mut type_env));

    let out_file = compiler_args
        .o
        .map(OsString::from)
        .unwrap_or(OsString::from("duck_out"));

    // Command::new("go")
    //     .args([
    //         OsString::from("build"),
    //         OsString::from("-o"),
    //         out_file.clone(),
    //         tmp_file.path().as_os_str().to_owned(),
    //     ])
    //     .spawn()?
    //     .wait()?;

    println!(
        "Successfully compiled binary {}",
        out_file.to_str().expect("Output File String is weird")
    );

    Ok(())
}
