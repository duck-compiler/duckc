#![feature(let_chains)]
#![feature(impl_trait_in_bindings)]

use std::{error::Error, ffi::OsString, io::Write, path::PathBuf, process::Command};

use chumsky::Parser;
use parse::lexer::lexer;
use semantics::typechecker::{self, TypeEnv};
use tempfile::Builder;

use crate::parse::{source_file_parser::source_file_parser, use_statement_parser::UseStatement};

pub mod parse;
pub mod semantics;
pub mod emit;

use emit::value::{EmitEnvironment, GoImport};

fn main() -> Result<(), Box<dyn Error>> {
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
    let mut p = PathBuf::from(target_path);
    let src = std::fs::read_to_string(&p).expect("Could not read file");
    let lex = lexer().parse(&src).into_result().expect("Lex error");
    let mut source_file = source_file_parser({p.pop();p})
        .parse(&lex)
        .into_result()
        .expect("Parse error");

    println!("before resolve");
    source_file = dbg!(source_file);

    let mut type_env = TypeEnv::default();
    typechecker::typeresolve_source_file(&mut source_file, &mut type_env);

    println!("after resolve");
    source_file = dbg!(source_file);

    let env = EmitEnvironment::new();
    source_file.use_statements.iter().filter_map(|x| match x.to_owned() {
        UseStatement::Go(path, alias) => Some(GoImport {
            path,
            alias,
        }),
        _ => None,
    }).for_each(|x| {
        env.push_import(x);
    });

    let functions = source_file
        .function_definitions
        .iter()
        .map(|x| x.emit(env.clone()))
        .collect::<Vec<_>>()
        .join("\n");
    let out_text = format!(
        "package main\n{}\n{}\n",
        env.emit_imports_and_types(),
        functions
    );

    let mut tmp_file = Builder::new()
        .prefix("duck_gen")
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
