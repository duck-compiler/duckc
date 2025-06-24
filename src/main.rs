#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]
#![allow(
    clippy::needless_return,
    clippy::match_like_matches_macro,
    clippy::only_used_in_recursion
)]

use std::{error::Error, ffi::OsString, io::Write, path::PathBuf, process::Command};

use chumsky::Parser;
use parse::lexer::lexer;
use semantics::typechecker::{self, TypeEnv};
use tempfile::Builder;

use crate::parse::{
    make_input, parse_failure, source_file_parser::source_file_parser,
    use_statement_parser::UseStatement, value_parser::value_expr_parser,
};

pub mod emit;
pub mod parse;
pub mod semantics;

use emit::value::{EmitEnvironment, GoImport};

fn test_error_messages() {
    let src = "if (1";

    let out = lexer().parse(src).into_result().unwrap();
    let (_out, errors) = value_expr_parser(make_input)
        .parse(make_input((0..src.len()).into(), &out))
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
    let mut keep_go = false;
    let mut custom_out_name = None::<String>;
    let mut file_name = None::<String>;

    let args = std::env::args().skip(1).collect::<Vec<_>>();

    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];
        if arg == "--keep-go" {
            keep_go = true;
        } else if arg == "-o" {
            custom_out_name = Some(
                args.get(i + 1)
                    .expect("Did provide argument for -o")
                    .clone(),
            );
            i += 1;
        } else {
            if file_name.is_some() {
                panic!("Error: reprovided file name");
            }
            file_name = Some(arg.to_owned());
        }
        i += 1;
    }

    let target_path = file_name.expect("No file name provided");
    let p = PathBuf::from(&target_path);
    let src = std::fs::read_to_string(&p).expect("Could not read file");
    let (lex, lex_errors) = lexer().parse(&src).into_output_errors();
    lex_errors.into_iter().for_each(|e| {
        parse_failure(&target_path, &e, &src);
    });

    let lex = lex.unwrap();

    let (src_file, parse_errors) = source_file_parser(
        {
            let mut p = p.clone();
            p.pop();
            p
        },
        make_input,
    )
    .parse(make_input((0..src.len()).into(), &lex))
    .into_output_errors();

    parse_errors.into_iter().for_each(|e| {
        parse_failure(&target_path, &e, &src);
    });

    let mut source_file = src_file.unwrap();

    let mut type_env = TypeEnv::default();
    typechecker::typeresolve_source_file(&mut source_file, &mut type_env);

    println!("after resolve");
    source_file = dbg!(source_file);

    let env = EmitEnvironment::new();
    source_file
        .use_statements
        .iter()
        .filter_map(|x| match x.to_owned() {
            UseStatement::Go(path, alias) => Some(GoImport { path, alias }),
            _ => None,
        })
        .for_each(|x| {
            env.push_import(x);
        });

    let _functions = source_file
        .function_definitions
        .iter()
        .map(|x| x.emit(env.clone(), &mut type_env))
        .collect::<Vec<_>>()
        .join("\n");

    let out_text = source_file.emit("main".into(), &mut type_env);

    let mut tmp_file = Builder::new()
        .rand_bytes(0)
        .prefix(
            p.file_name()
                .expect("didnt provide file name")
                .to_str()
                .expect("not valid utf 8"),
        )
        .suffix(".go")
        .tempfile_in(".")
        .expect("Could not create tempfile");

    if keep_go {
        tmp_file.disable_cleanup(true);
    }

    write!(tmp_file, "{out_text}").expect("Could not write tmp file");
    tmp_file.flush().expect("Could not flush tmp file");

    Command::new("go")
        .args([
            OsString::from("build"),
            OsString::from("-o"),
            custom_out_name
                .map(OsString::from)
                .unwrap_or(OsString::from("duck_out")),
            tmp_file.path().as_os_str().to_owned(),
        ])
        .spawn()?
        .wait()?;
    Ok(())
}
