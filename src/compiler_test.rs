use std::process::Command;

use bumpalo::Bump;

use crate::backend::gost::{emit_gost, translate};
use crate::backend::semantics::{analyze_module, context::SemanticsContext};
use crate::frontend::parser::parse_module;

fn compile_to_go(source: &str) -> String {
    let arena = Bump::new();

    let ast = match parse_module("pipeline_test.duck", source) {
        Ok(ast) => ast,
        Err(error) => panic!("unexpected parse error: {error}"),
    };

    let mut semantics_context = SemanticsContext::new(&arena);
    let module = semantics_context.add_module(ast);

    analyze_module(&mut semantics_context, module);
    assert!(
        semantics_context.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        semantics_context.diagnostics
    );

    emit_gost(translate(&semantics_context, module))
}

fn go_run(go_source: &str, test_dir: &str) -> String {
    let go_version = Command::new("go")
        .arg("version")
        .output()
        .expect("go must be installed in PATH to run pipeline tests");

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join(format!("{test_dir}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");
    std::fs::write(&file_path, go_source).expect("failed to write temp go file");

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to go run");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}\nsource: {go_source}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn assert_program_stdout(source: &str, test_dir: &str, expected: &str) {
    let go_source = compile_to_go(source);
    let stdout = go_run(&go_source, test_dir);

    assert_eq!(stdout, expected, "generated source: {go_source}");
}

#[test]
fn compile_and_run_hello_world() {
    assert_program_stdout(
        r#"
        use fmt;

        fn main() {
            fmt.Println("hello, world");
        }
        "#,
        "duckc-pipeline-hello-test",
        "hello, world",
    );
}

#[test]
fn compile_and_run_generic_struct_with_methods() {
    assert_program_stdout(
        r#"
        use fmt;
        use strconv;

        struct Box<T> {
            pub value: T,
        } impl {
            pub fn get() -> T {
                return self.value;
            }

            pub fn with<U>(other: U) -> U {
                return other;
            }
        }

        fn main() {
            let boxed = Box<string> { value: "duck" };
            fmt.Println(boxed.get());
            fmt.Println(strconv.Itoa(boxed.with<int>(7)));
        }
        "#,
        "duck-pipeline-generic-struct-test",
        "duck\n7",
    );
}

#[test]
fn compile_and_run_counter_using_pointers() {
    assert_program_stdout(
        r#"
        use fmt;
        use strconv;

        struct Counter {
            pub value: int,
        } impl {
            pub fn bump(by: int) {
                self.value = self.value + by;
            }
        }

        fn main() {
            let counter = Counter { value: 1 };
            let pointer = &counter;
            pointer.bump(4);
            fmt.Println(strconv.Itoa(counter.value));
        }
        "#,
        "duck-pipeline-counter_using_pointers",
        "5",
    );
}

#[test]
fn compile_and_run_tuple_swap() {
    assert_program_stdout(
        r#"
        use fmt;
        use strconv;

        fn swap(pair: (int, string)) -> (string, int) {
            return (pair.1, pair.0);
        }

        fn main() {
            let pair = (1, "one");
            let swapped = swap(pair);
            fmt.Println(swapped.0);

            let nested = ((1, 2), 3);
            fmt.Println(strconv.Itoa(nested.0.1));
        }
        "#,
        "duck-pipeline-tuple_swap",
        "one\n2",
    );
}

#[test]
fn compile_and_run_standalone_fn_and_struct_with_methods() {
    assert_program_stdout(
        r#"
        use fmt;
        use strconv;

        fn echo<T>(value: T) -> T {
            return value;
        }

        struct Bag {
            pub items: int[],
        } impl {
            pub fn total() -> int {
                let sum = 0;
                let index = 0;
                while index < 3 {
                    sum = sum + self.items[index];
                    index = index + 1;
                }

                return sum;
            }
        }

        fn main() {
            let bag = Bag { items: [1, 2, 3] };
            let total = bag.total();
            let label = if total > 5 { "big" } else { "small" };

            fmt.Println(strconv.Itoa(total));
            fmt.Println(echo<string>(label));
        }
        "#,
        "duck-pipeline-standalone_fn_and_struct_with_methods",
        "6\nbig",
    );
}
