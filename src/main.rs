#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion
)]

use std::{error::Error, ffi::OsString, io::Write, path::PathBuf, process::Command};

use chumsky::{Parser, error::Rich};
use clap::{Parser as CliParser, arg, command};
use parse::lexer::lexer;
use semantics::typechecker::{self, TypeEnv};
use tempfile::Builder;

use crate::{
    emit::ir::join_ir,
    parse::{
        Context, SS, make_input, parse_failure,
        source_file_parser::source_file_parser,
        value_parser::{empty_range, value_expr_parser},
    },
};

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

fn test_error_messages() {
    let src = "if (1";

    let out = lexer("test", src).parse(src).into_result().unwrap();
    let (_out, errors) = value_expr_parser(make_input)
        .parse(make_input(empty_range(), &out))
        .into_output_errors();

    errors.into_iter().for_each(|e| {
        parse_failure("test.duck", &e, src);
    });
}

fn main() -> Result<(), Box<dyn Error>> {
    if false {
        test_error_messages();
        return Ok(());
    }

    let compiler_args = CompilerArgs::parse();

    let target_path = compiler_args.input_file;
    let target_path_name = target_path
        .file_name()
        .expect("couldn't read file name")
        .to_str()
        .expect("invalid utf-8 string");

    let src = std::fs::read_to_string(&target_path)
        .expect("Could not read file")
        .to_string()
        .leak() as &'static str;
    let target_path_name = String::from(target_path_name).leak() as &'static str;
    let (lex, lex_errors) = lexer(target_path_name, src)
        .parse(&src)
        .into_output_errors();
    lex_errors.into_iter().for_each(|e| {
        parse_failure(
            target_path_name,
            &Rich::<&str, SS>::custom(
                SS {
                    start: e.span().start,
                    end: e.span().end,
                    context: crate::parse::Context {
                        file_name: target_path_name,
                        file_contents: src,
                    },
                },
                "Lex Error",
            ),
            &src,
        );
    });

    let lex = lex.unwrap();
    let (src_file, parse_errors) = source_file_parser(
        {
            let mut p = target_path.clone();
            p.pop();
            p
        },
        make_input,
    )
    .parse(make_input(
        SS {
            start: 0,
            end: src.len(),
            context: Context {
                file_name: target_path_name,
                file_contents: src,
            },
        },
        &lex,
    ))
    .into_output_errors();

    parse_errors.into_iter().for_each(|e| {
        parse_failure(target_path_name, &e, &src);
    });

    let mut source_file = dbg!(src_file.unwrap());
    source_file = source_file.flatten();
    for f in &source_file.function_definitions {
        dbg!(f.value_expr.1.context);
    }

    let mut type_env = TypeEnv::default();
    typechecker::typeresolve_source_file(&mut source_file, &mut type_env);

    let out_text = join_ir(&source_file.emit("main".into(), &mut type_env));

    let mut tmp_file = Builder::new()
        .rand_bytes(3)
        .prefix(
            target_path
                .file_name()
                .expect("didnt provide file name")
                .to_str()
                .expect("not valid utf 8"),
        )
        .suffix(".go")
        .tempfile_in(".")
        .expect("Could not create tempfile");

    tmp_file.disable_cleanup(!compiler_args.emit_go);

    write!(tmp_file, "{out_text}").expect("Could not write tmp file");
    tmp_file.flush().expect("Could not flush tmp file");

    let out_file = compiler_args
        .o
        .map(OsString::from)
        .unwrap_or(OsString::from("duck_out"));

    Command::new("go")
        .args([
            OsString::from("build"),
            OsString::from("-o"),
            out_file.clone(),
            tmp_file.path().as_os_str().to_owned(),
        ])
        .spawn()?
        .wait()?;

    println!(
        "Successfully compiled binary {}",
        out_file.to_str().expect("Output File String is weird")
    );

    Ok(())
}
