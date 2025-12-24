use colored::Colorize;
use duckwind::EmitEnv;
use lazy_static::lazy_static;
use std::{ffi::OsString, fs, path::PathBuf, sync::mpsc, time::Duration};

use crate::{
    DARGO_DOT_DIR,
    cli::go_cli::{self, GoCliErrKind},
    dargo::cli::CompileArgs,
    emit::{ir::join_ir, types::escape_string_for_go},
    go_fixup::remove_unused_imports::cleanup_go_source,
    lex,
    parse::value_parser::empty_range,
    parse_src_file,
    tags::Tag,
    typecheck, write_in_duck_dotdir,
};

#[derive(Debug)]
pub enum CompileErrKind {
    CorruptedFileName,
    TargetPathIsDirectory,
    FileNotFound,
    CannotReadFile,
    GoCli(GoCliErrKind),
}

lazy_static! {
    static ref COMPILE_TAG: String = " compile ".on_bright_black().bright_white().to_string();
}

pub struct CompileOutput {
    pub binary_path: PathBuf,
}

pub fn compile(compile_args: CompileArgs) -> Result<CompileOutput, (String, CompileErrKind)> {
    let src_file: PathBuf = compile_args.file;
    let binary_output_name: Option<String> = compile_args.output_name;

    if src_file.is_dir() {
        let message = format!(
            "{}{} the path you provided is a directory. You need to provide a .duck file",
            *COMPILE_TAG,
            Tag::Err,
        );

        return Err((message, CompileErrKind::TargetPathIsDirectory));
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
        return Err((message, CompileErrKind::TargetPathIsDirectory));
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
                CompileErrKind::CorruptedFileName,
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
                CompileErrKind::CorruptedFileName,
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
                CompileErrKind::CannotReadFile,
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

        let _ = tailwind_result_send.send(emit_env.to_css_stylesheet(true));
    });
    let mut type_env = typecheck(&mut src_file_ast, &tailwind_worker_send);
    let mut go_code = join_ir(&src_file_ast.emit("main".into(), &mut type_env, empty_range()));

    // drop the sender here so that the thread knows it should emit the final tailwind
    drop(tailwind_worker_send);

    let css = tailwind_result_receive
        .recv_timeout(Duration::from_secs(30))
        .expect("tailwind timed out");

    go_code = cleanup_go_source(&go_code, true);
    go_code = format!(
        "{go_code}\nconst TAILWIND_STR = \"{}\"",
        escape_string_for_go(css.as_str())
    );

    let go_output_file = write_in_duck_dotdir(format!("{src_file_name}.gen.go").as_str(), &go_code);
    if compile_args.optimize_go {
        let _ = go_cli::format(go_output_file.as_path());
    }

    let compile_output_target = {
        let mut target_file = DARGO_DOT_DIR.clone();
        target_file.push(
            binary_output_name
                .map(OsString::from)
                .unwrap_or(OsString::from("duck_out")),
        );

        target_file
    };

    go_cli::build(&compile_output_target, &go_output_file).map_err(|err| {
        (
            format!("{}{}", *COMPILE_TAG, err.0),
            CompileErrKind::GoCli(err.1),
        )
    })?;

    println!(
        "{}{}{} Successfully compiled binary",
        Tag::Dargo,
        *COMPILE_TAG,
        Tag::Check,
    );

    return Ok(CompileOutput {
        binary_path: compile_output_target,
    });
}
