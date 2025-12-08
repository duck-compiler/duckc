use colored::Colorize;
use duckwind::EmitEnv;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{fs::{self, File, OpenOptions}, io::Write, path::{Path, PathBuf}, sync::mpsc};

use crate::{
    cli::go_cli::GoCliErrKind, dargo::cli::DocsGenerateArgs, emit::function, lex, parse_src_file, tags::Tag, typecheck
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
    pub fn_docs: Vec<FunctionDoc>,
    pub struct_docs: Vec<StructDoc>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DocsField {
    // pub comments: Vec<String>,
    pub field_name: String,
    pub type_annotation: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StructDoc {
    pub struct_name: String,
    pub fields: Vec<DocsField>,
    pub comments: Vec<String>,
    pub function_docs: Vec<FunctionDoc>,
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
    let mut struct_docs = vec![];

    let type_env = typecheck(&mut src_file_ast, &tailwind_worker_send);
    type_env.struct_definitions.iter().for_each(|struct_definition| {
        let mut fn_docs = vec![];
        struct_definition
            .methods
            .iter()
            .for_each(|function_def| {
                if !function_def.comments.is_empty() {
                    fn_docs.push(FunctionDoc {
                        function_name: function_def.name.clone(),
                        function_annotation: function_def.type_expr().0.as_clean_user_faced_type_name(),
                        comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
                    });
                }
            });

        if !(fn_docs.is_empty() && struct_definition.doc_comments.is_empty()) {
            struct_docs.push(StructDoc {
                function_docs: fn_docs,
                struct_name: struct_definition.name.clone(),
                comments: struct_definition.doc_comments.iter().map(|c| c.0.clone()).collect(),
                fields: struct_definition.fields.iter().map(|field| DocsField {
                    field_name: field.name.clone(),
                    type_annotation: field.type_expr.0.as_clean_user_faced_type_name()
                }).collect()
            });
        }
    });

    type_env.function_definitions.iter().for_each(|function_def| {
        if !function_def.comments.is_empty() {
            fn_docs.push(FunctionDoc {
                function_name: function_def.name.clone(),
                function_annotation: function_def.type_expr().0.as_clean_user_faced_type_name(),
                comments: function_def.comments.iter().map(|c| c.0.clone()).collect(),
            });
            println!()
        }
    });

    println!(
        "{}{}{} Successfully generated docs",
        Tag::Dargo,
        *COMPILE_TAG,
        Tag::Check,
    );

    let json_output = serde_json::to_string(&struct_docs).unwrap();
    dbg!(json_output);
    let json_output = serde_json::to_string(&fn_docs).unwrap();
    dbg!(json_output);

    let html = layout_html(&fn_docs, &struct_docs);
    println!("{}", layout_html(&fn_docs, &struct_docs));

    let file = Path::new("./docs_output.html");
    fs::write(file, html).expect("couldn't write docs");

    return Ok(DocsOutput {
        json_output_path: Path::new("here").to_path_buf(),
        fn_docs: fn_docs,
        struct_docs: struct_docs,
    });
}

fn layout_html(fn_docs: &Vec<FunctionDoc>, struct_docs: &Vec<StructDoc>) -> String {
    let fn_docs_html = fn_docs
            .iter()
            .map(function_doc_to_html)
            .collect::<Vec<_>>()
            .join("\n");

    let mut duckwind_emit_env = duckwind::EmitEnv::new_with_default_config();
    duckwind_emit_env.parse_full_string(None, &fn_docs_html);

    let output_css = duckwind_emit_env.to_css_stylesheet(true);

    return format!("
        <!doctype html>
        <html>
            <head>
                <title>Duck Docs</title>
                <style>
                    {output_css}
                </style>
            </head>
            <body>
                {fn_docs_html}
            </body>
        </html>
    ")
}

fn function_doc_to_html(fn_doc: &FunctionDoc) -> String {
    return format!("
        <div class=\"text-black\">
            <div class='text-2xl'>
                {}
            </div>
        </div>
    ", fn_doc.function_name)
}
