use chumsky::input::Input;
use colored::Colorize;
use lazy_static::lazy_static;
use std::{ffi::OsString, fs, path::PathBuf};

use crate::{
    cli::go_cli::{self, GoCliErrKind}, dargo::cli::CompileArgs, emit::ir::join_ir, lex, parse::{function_parser::FunctionDefintion, value_parser::empty_range}, parse_src_file, tags::Tag, typecheck, write_in_duck_dotdir, DARGO_DOT_DIR
};

#[derive(Debug)]
pub enum CompileTestErrKind {
    CorruptedFileName,
    TargetPathIsDirectory,
    FileNotFound,
    CannotReadFile,
    GoCli(GoCliErrKind),
}

lazy_static! {
    static ref COMPILE_TEST_TAG: String = " compile test ".on_bright_black().bright_white().to_string();
}

pub struct CompileOutput {
    pub binary_path: PathBuf,
}

pub fn compile(compile_args: CompileArgs) -> Result<CompileOutput, (String, CompileTestErrKind)> {
    let src_file: PathBuf = compile_args.file;
    let binary_output_name: Option<String> = compile_args.output_name;

    if src_file.is_dir() {
        let message = format!(
            "{}{} the path you provided is a directory. You need to provide a .duck file",
            *COMPILE_TEST_TAG,
            Tag::Err,
        );

        return Err((message, CompileTestErrKind::TargetPathIsDirectory));
    }

    if src_file
        .extension()
        .ok_or_else(|| {
            format!(
                "{}{} couldn't extract file extension from provided source file",
                *COMPILE_TEST_TAG,
                Tag::Err,
            )
        })
        .unwrap()
        != "duck"
    {
        let message = format!(
            "{}{} the path you provided is not a valid duck source file. You need to provide a .duck file",
            *COMPILE_TEST_TAG,
            Tag::Err,
        );
        return Err((message, CompileTestErrKind::TargetPathIsDirectory));
    }

    let src_file_name: &'static str = src_file
        .file_name()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} couldn't get the filename from given ",
                    *COMPILE_TEST_TAG,
                    Tag::Err
                ),
                CompileTestErrKind::CorruptedFileName,
            )
        })?
        .to_str()
        .ok_or_else(|| {
            (
                format!(
                    "{}{} the filename is an invalid utf-8 string",
                    *COMPILE_TEST_TAG,
                    Tag::Err
                ),
                CompileTestErrKind::CorruptedFileName,
            )
        })?
        .to_string()
        .leak();

    let src_file_file_contents: &'static str = fs::read_to_string(&src_file)
        .map_err(|err| {
            (
                format!(
                    "{}{} couldn't read file '{}'. msg='{}'",
                    *COMPILE_TEST_TAG,
                    Tag::Err,
                    src_file.to_string_lossy().bright_blue(),
                    err.to_string().bright_red()
                ),
                CompileTestErrKind::CannotReadFile,
            )
        })?
        .to_string()
        .leak();

    let tokens = lex(src_file_name, src_file_file_contents);
    let mut src_file_ast = parse_src_file(&src_file, src_file_name, src_file_file_contents, tokens);
    let mut type_env = typecheck(&mut src_file_ast);
    // the only change in this file is that the parameter is set to true
    let main_fun: &mut FunctionDefintion = src_file_ast.function_definitions.iter_mut()
        .find(|fun_def| fun_def.name == "main")
        .expect("compiler error: expected main function");

    main_fun.name = "normally_main".to_string();

    dbg!(&src_file_ast.test_cases);
    let test_source = src_file_ast.test_cases
        .iter()
        .map(|test_case| format!(
            r#"
                DuckTestCase {{
                    name: "{}",
                    test_case_fn: func() {{
                        fmt.Println("Running Test \"{}\"")
                        {}
                    }},
                }},
            "#,
            test_case.name,
            test_case.name,
            join_ir(&test_case.body.0.emit(&mut type_env, &mut crate::emit::value::ToIr { var_counter: 0 }, test_case.body.1).0)
        ))
        .collect::<Vec<_>>()
        .join("\n");

    let main_fn = format!(
        r#"
        type test_case_fn func()
        type DuckTestCase struct {{
            name string
            test_case_fn test_case_fn
        }}

        func main() {{
            fmt.Println("tests")

            tests := []DuckTestCase {{
                {test_source}
            }}

            for _, test := range tests {{
                defer func() {{
                    if r := recover(); r != nil {{
                        fmt.Println("test failed", test.name)
                    }}
                }}()

                test.test_case_fn()
                fmt.Println("test successful", test.name)
            }}
        }}"#,
    );

    let go_code = format!(
        "{}\n\n{main_fn}",
        join_ir(&src_file_ast.emit("main".into(), &mut type_env, empty_range(), true))
    );

    let go_output_file = write_in_duck_dotdir(format!("{src_file_name}.gen.test.go").as_str(), &go_code);
    if compile_args.optimize_go {
        let _ = go_cli::format(go_output_file.as_path());
    }

    let compile_output_target = {
        let mut target_file = DARGO_DOT_DIR.clone();
        target_file.push(
            binary_output_name
                .map(OsString::from)
                .unwrap_or(OsString::from("dargo_test_out")),
        );

        target_file
    };

    go_cli::build(&compile_output_target, &go_output_file).map_err(|err| {
        (
            format!("{}{}", *COMPILE_TEST_TAG, err.0),
            CompileTestErrKind::GoCli(err.1),
        )
    })?;

    println!(
        "{}{}{} Successfully compiled binary",
        Tag::Dargo,
        *COMPILE_TEST_TAG,
        Tag::Check,
    );

    return Ok(CompileOutput {
        binary_path: compile_output_target,
    });
}
