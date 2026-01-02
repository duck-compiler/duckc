use colored::Colorize;
use duckwind::EmitEnv;
use lazy_static::lazy_static;
use std::{ffi::OsString, fs, path::PathBuf, sync::mpsc, time::Duration};

use crate::{
    DARGO_DOT_DIR,
    cli::go_cli::{self, GoCliErrKind},
    dargo::cli::CompileArgs,
    emit::{ir::join_ir, types::escape_string_for_go},
    lex,
    parse::value_parser::empty_range,
    parse_src_file,
    tags::Tag,
    typecheck, write_in_duck_dotdir,
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
    static ref COMPILE_TEST_TAG: String = " compile test "
        .on_bright_black()
        .bright_white()
        .to_string();
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
    src_file_ast
        .use_statements
        .push(crate::parse::use_statement_parser::UseStatement::Go(
            "io".to_string(),
            None,
        ));
    src_file_ast
        .use_statements
        .push(crate::parse::use_statement_parser::UseStatement::Go(
            "bufio".to_string(),
            None,
        ));
    src_file_ast
        .use_statements
        .push(crate::parse::use_statement_parser::UseStatement::Go(
            "bytes".to_string(),
            None,
        ));
    src_file_ast
        .use_statements
        .push(crate::parse::use_statement_parser::UseStatement::Go(
            "sync".to_string(),
            None,
        ));

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

    let maybe_main_fn = src_file_ast
        .function_definitions
        .iter_mut()
        .find(|fun_def| fun_def.name == "main");

    if let Some(main_fn) = maybe_main_fn {
        main_fn.name = "____thrown_away_main_LOL".to_string();
    }

    let test_source = src_file_ast
        .test_cases
        .iter()
        .map(|test_case| {
            format!(
                r#"
                DuckTestCase {{
                    name: "{}",
                    test_case_fn: func() {{
                        fmt.Println("Running Test \"{}\"")
                        time.Sleep(1000 * time.Millisecond)
                        {}
                    }},
                }},
            "#,
                test_case.name,
                test_case.name,
                join_ir(
                    &test_case
                        .body
                        .0
                        .emit(
                            &mut type_env,
                            &mut crate::emit::value::ToIr {
                                var_counter: 0,
                                per_var_counter: vec![Default::default()],
                                labels: Vec::new(),
                            },
                            test_case.body.1
                        )
                        .0
                )
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let main_fn = format!(
        r#"
        type test_case_fn func()
        type DuckTestCase struct {{
            name string
            test_case_fn test_case_fn
        }}

        func spinner(wg *sync.WaitGroup, writer *bufio.Writer, stop chan bool, testName string) {{
            defer wg.Done()
            animation := `|/-\`
            i := 0
            for {{
                select {{
		        case <-stop:
					fmt.Fprintf(writer, "\r")
					writer.Flush()
					return
			    default:
			        fmt.Fprintf(writer, "\r[%c] Running test: %s", animation[i], testName)
					writer.Flush()

			        i = (i + 1) % len(animation)
			        time.Sleep(100 * time.Millisecond)
			    }}
		    }}
        }}

        func main() {{
            fmt.Println("tests")

            tests := []DuckTestCase {{
                {test_source}
            }}

            writer := bufio.NewWriter(os.Stdout)

            for _, test := range tests {{
                func (currentTest DuckTestCase) {{
                    var wg sync.WaitGroup

                    stop := make(chan bool)
                    wg.Add(1)
                    go spinner(&wg, writer, stop, currentTest.name)

                    originalStdout := os.Stdout
                    r, w, _ := os.Pipe()
                    os.Stdout = w

                    var testErr interface{{}}

                    func() {{
                        defer func() {{
                            testErr = recover()
                        }}()

                        currentTest.test_case_fn()
                    }}()

                    var capturedOutput bytes.Buffer

                    w.Close()
                    io.Copy(&capturedOutput, r)

                    os.Stdout = originalStdout

                    stop <- true
                    wg.Wait()

                    if testErr != nil {{
                        fmt.Fprintf(writer, "[\x1b[31m✘\x1b[0m] Test failed: %s\n", currentTest.name)
				    }} else {{
				        fmt.Fprintf(writer, "[\x1b[32m✔\x1b[0m] Test successful: %s\n", currentTest.name)
				    }}

                    fmt.Fprint(writer, capturedOutput.String())
                    writer.Flush()
                }}(test)
            }}
        }}"#,
    );

    let mut go_code = format!(
        "{}\n\n{main_fn}",
        join_ir(&src_file_ast.emit("main".into(), &mut type_env, empty_range()))
    );

    // drop the sender here so that the thread knows it should emit the final tailwind
    drop(tailwind_worker_send);
    let css = tailwind_result_receive
        .recv_timeout(Duration::from_secs(30))
        .expect("tailwind timed out");

    go_code = format!(
        "{go_code}\nconst TAILWIND_STR = \"{}\"",
        escape_string_for_go(css.as_str())
    );

    let go_output_file =
        write_in_duck_dotdir(format!("{src_file_name}.gen.test.go").as_str(), &go_code);
    if compile_args.optimize_go {
        let _ = go_cli::format(go_output_file.as_path());
    }

    let executable_path = go_cli::build(
        &DARGO_DOT_DIR,
        binary_output_name
            .map(OsString::from)
            .unwrap_or(OsString::from("duck_out"))
            .as_os_str(),
        &go_output_file,
    )
    .map_err(|err| {
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
        binary_path: executable_path,
    });
}
