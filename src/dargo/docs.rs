use colored::Colorize;
use duckwind::EmitEnv;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{ffi::OsString, fs, path::{Path, PathBuf}, sync::mpsc, time::Duration};

use crate::{
    cli::go_cli::{self, GoCliErrKind}, dargo::cli::{CompileArgs, DocsGenerateArgs}, emit::{ir::join_ir, types::escape_string_for_go}, go_fixup::remove_unused_imports::cleanup_go_source, lex, parse::{value_parser::empty_range, Spanned}, parse_src_file, tags::Tag, typecheck, write_in_duck_dotdir, DARGO_DOT_DIR
};

#[derive(Debug)]
pub enum DocsErrKind {
    CorruptedFileName,
    TargetPathIsDirectory,
    FileNotFound,
    CannotReadFile,
    GoCli(GoCliErrKind),
}

lazy_static! {
    static ref COMPILE_TAG: String = " docs ".on_bright_black().bright_white().to_string();
}

pub struct DocsOutput {
    pub json_output_path: PathBuf,
    pub fn_docs: Vec<FunctionDoc>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionDoc {
    pub function_name: String,
    pub function_annotation: String,
    pub comments: Vec<String>,
}

pub fn generate(generate_args: DocsGenerateArgs) -> Result<DocsOutput, (String, DocsErrKind)> {
    let src_file: PathBuf = generate_args.file;
    if src_file.is_dir() {
        let message = format!(
            "{}{} the path you provided is a directory. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );

        return Err((message, DocsErrKind::TargetPathIsDirectory));
    }

    if src_file
        .extension()
        .ok_or_else(|| {
            format!(
                "{}{} couldn't extract file extension from provided source file",
                *COMPILE_TAG,
                Tag::Err,
            )
        })
        .unwrap()
        != "duck"
    {
        let message = format!(
            "{}{} the path you provided is not a valid duck source file. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );
        return Err((message, DocsErrKind::TargetPathIsDirectory));
    }

    let src_file_name: &'static str = src_file
        .file_name()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} couldn't get the filename from given ",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                DocsErrKind::CorruptedFileName,
            )
        })?
        .to_str()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} the filename is an invalid utf-8 string",
                    *COMPILE_TAG,
                    Tag::Err
                ),
                DocsErrKind::CorruptedFileName,
            )
        })?
        .to_string()
        .leak();

    let src_file_file_contents: &'static str = fs::read_to_string(&src_file)
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't read file '{}'. msg='{}'",
                    *COMPILE_TAG,
                    Tag::Err,
                    src_file.to_string_lossy().bright_blue(),
                    err.to_string().bright_red()
                ),
                DocsErrKind::CannotReadFile,
            )
        })?
        .to_string()
        .leak();

    let tokens = lex(src_file_name, src_file_file_contents);
    let mut src_file_ast = parse_src_file(&src_file, src_file_name, src_file_file_contents, tokens);

    let (tailwind_worker_send, tailwind_worker_receive) = mpsc::channel::<String>();
    let (tailwind_result_send, tailwind_result_receive) = mpsc::channel::<String>();

    let tailwind_prefix = None::<String>;

    std::thread::spawn(move || {
        let mut emit_env = EmitEnv::new_with_default_config();
        // emit_env.parse_full_string(src_file_file_contents);
        loop {
            let s = tailwind_worker_receive.recv();
            match s {
                Ok(s) => emit_env.parse_full_string(tailwind_prefix.as_deref(), s.as_str()),
                Err(_) => break,
            }
        }
        tailwind_result_send
            .send(emit_env.to_css_stylesheet(true))
            .expect("could not send css result");
    });

    let mut fn_docs = vec![];
    let type_env = typecheck(&mut src_file_ast, &tailwind_worker_send);
    type_env.function_definitions.iter().for_each(|function_def| {
        if !function_def.comments.is_empty() {
            fn_docs.push(FunctionDoc {
                function_name: function_def.name.clone(),
                function_annotation: function_def.type_expr().0.as_clean_user_faced_type_name(),
                comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
            });
            dbg!(&function_def);
            println!()
        }
    });

    println!(
        "{}{}{} Successfully generated docs",
        Tag::Dargo,
        *COMPILE_TAG,
        Tag::Check,
    );

    let json_output = serde_json::to_string(&fn_docs).unwrap();
    dbg!(json_output);

    return Ok(DocsOutput {
        json_output_path: Path::new("here").to_path_buf(),
        fn_docs: fn_docs,
    });
}
