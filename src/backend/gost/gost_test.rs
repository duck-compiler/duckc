use std::io::Write;
use std::process::Command;

use crate::ast::TypeExpression;
use crate::ast::expression::{BinaryOperator, UnaryOperator};
use crate::ast::builder::{array, array_index, assign, binary, bool_lit, break_stmt, continue_stmt, expr_stmt, field_access, field_call, fn_call, fn_def, if_else_expr, if_expr, int, mem_name, name_target, no_type, program, return_stmt, string, struct_def, struct_init, type_, unary, use_stmt, var_decl, while_expr};
use crate::backend::semantics::{analyze_module, context::SemanticsContext};
use crate::backend::gost::{emit_gost, translate};

#[test]
fn array_literal_and_index_translate_to_valid_go() {
    let mut context = SemanticsContext::new();

    let println_with_array_access_program = program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("arr", no_type(), Some(array(vec![string("a"), string("b")]))),
                var_decl("i", type_(TypeExpression::Int), None),
                expr_stmt(field_call("fmt", "Println", vec![array_index("arr", mem_name("i"))])),
            ],
        ),
    ]);

    let module = context.add_module(println_with_array_access_program);
    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let gou_source = emit_gost(gost_root);

    assert!(gou_source.contains(r#"[]string{"a", "b"}"#), "generated source: {gou_source}");
    assert!(gou_source.contains("arr[i]"), "generated source: {gou_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-array-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(gou_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "a");
}

#[test]
fn struct_literal_and_field_access_translate_to_valid_go() {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        struct_def("Point", vec![("x", TypeExpression::String), ("y", TypeExpression::String)]),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("p", no_type(), Some(struct_init("Point", vec![("x", string("a")), ("y", string("b"))]))),
                expr_stmt(field_call("fmt", "Println", vec![field_access(name_target("p"), "x")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("type Point struct"), "generated source: {go_source}");
    assert!(go_source.contains(r#"Point{x: "a", y: "b"}"#), "generated source: {go_source}");
    assert!(go_source.contains("p.x"), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-struct-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "a");
}

#[test]
fn go_stdlib_struct_return_type_synthesizes_a_duck_struct_and_translates_to_valid_go() {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("time", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(field_call("time", "Now", vec![])),
                expr_stmt(field_call("fmt", "Println", vec![string("ok")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("time.Now()"), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-go-struct-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "ok");
}

#[test]
fn control_flow_arithmetic_and_assignment_translate_to_valid_go() {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
                assign(name_target("flag"), bool_lit(false)),
                expr_stmt(if_expr(mem_name("flag"), vec![
                    expr_stmt(field_call("fmt", "Println", vec![string("if-branch")])),
                ])),
                var_decl("running", type_(TypeExpression::Bool), Some(bool_lit(true))),
                expr_stmt(while_expr(mem_name("running"), vec![
                    assign(name_target("running"), bool_lit(false)),
                    expr_stmt(field_call("fmt", "Println", vec![string("loop-body")])),
                ])),
                var_decl("_", no_type(), Some(binary(int(2), BinaryOperator::Add, binary(int(3), BinaryOperator::Mul, int(4))))),
                var_decl("_", no_type(), Some(unary(UnaryOperator::Bang, mem_name("flag")))),
                expr_stmt(field_call("fmt", "Println", vec![string("done")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("if flag"), "generated source: {go_source}");
    assert!(go_source.contains("for running"), "generated source: {go_source}");
    assert!(go_source.contains("flag = false"), "generated source: {go_source}");
    assert!(go_source.contains("(2 + (3 * 4))"), "generated source: {go_source}");
    assert!(go_source.contains("(!flag)"), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-control-flow-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "loop-body\ndone");
}

#[test]
fn if_else_used_as_a_value_translates_and_runs_correctly() {
    // `if`/`else` used as a value-producing expression (assigned into a
    // variable) rather than as a bare statement. Go has no if-expression, so
    // this must hoist a temporary, assign into it from both branches, and
    // reference the temporary as the expression's value - this is the actual
    // behavior under test, verified by running the real branch outcome
    // through `go run` for both the true and false case.
    let mut context = SemanticsContext::new();
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
                var_decl("label_true", no_type(), Some(if_else_expr(
                    mem_name("flag"),
                    vec![expr_stmt(string("yes"))],
                    vec![expr_stmt(string("no"))],
                ))),
                assign(name_target("flag"), bool_lit(false)),
                var_decl("label_false", no_type(), Some(if_else_expr(
                    mem_name("flag"),
                    vec![expr_stmt(string("yes"))],
                    vec![expr_stmt(string("no"))],
                ))),
                expr_stmt(field_call("fmt", "Println", vec![mem_name("label_true")])),
                expr_stmt(field_call("fmt", "Println", vec![mem_name("label_false")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("var __duck_if_0 string"), "generated source: {go_source}");
    assert!(go_source.contains("__duck_if_0 = \"yes\""), "generated source: {go_source}");
    assert!(go_source.contains("} else {"), "generated source: {go_source}");
    assert!(go_source.contains("var label_true") && go_source.contains("= __duck_if_0"), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-if-value-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "yes\nno");
}

#[test]
fn return_comparison_logic_and_loop_control_translate_and_run_correctly() {
    let mut context = SemanticsContext::new();

    let program = program(vec![
        use_stmt("fmt", None),
        fn_def(
            "is_valid",
            vec![("n", TypeExpression::Int)],
            type_(TypeExpression::Bool),
            vec![
                return_stmt(Some(binary(
                    binary(mem_name("n"), BinaryOperator::Greater, int(0)),
                    BinaryOperator::And,
                    binary(mem_name("n"), BinaryOperator::Less, int(10)),
                ))),
            ],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("i", type_(TypeExpression::Int), Some(int(0))),
                var_decl("result", type_(TypeExpression::String), Some(string("start"))),
                expr_stmt(while_expr(binary(mem_name("i"), BinaryOperator::Less, int(10)), vec![
                    assign(name_target("i"), binary(mem_name("i"), BinaryOperator::Add, int(1))),
                    expr_stmt(if_expr(binary(mem_name("i"), BinaryOperator::Eq, int(3)), vec![
                        continue_stmt(),
                    ])),
                    expr_stmt(if_expr(binary(mem_name("i"), BinaryOperator::Greater, int(5)), vec![
                        break_stmt(),
                    ])),
                    assign(name_target("result"), string("looped")),
                ])),
                var_decl("v", type_(TypeExpression::Bool), Some(fn_call("is_valid", vec![int(4)]))),
                expr_stmt(if_else_expr(
                    mem_name("v"),
                    vec![expr_stmt(field_call("fmt", "Println", vec![mem_name("result")]))],
                    vec![expr_stmt(field_call("fmt", "Println", vec![string("invalid")]))],
                )),
            ],
        ),
    ]);

    let module = context.add_module(program);

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("func is_valid(n int) bool"), "generated source: {go_source}");
    assert!(go_source.contains("(n > 0) && (n < 10)"), "generated source: {go_source}");
    assert!(go_source.contains("break"), "generated source: {go_source}");
    assert!(go_source.contains("continue"), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-return-control-flow-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "looped");
}

#[test]
fn if_with_diverging_branch_used_as_a_value_translates_and_runs_correctly() {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def(
            "classify",
            vec![("n", TypeExpression::Int)],
            type_(TypeExpression::String),
            vec![
                var_decl("label", no_type(), Some(if_else_expr(
                    binary(mem_name("n"), BinaryOperator::Less, int(0)),
                    vec![return_stmt(Some(string("negative")))],
                    vec![expr_stmt(string("non-negative"))],
                ))),
                return_stmt(Some(mem_name("label"))),
            ],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(field_call("fmt", "Println", vec![fn_call("classify", vec![int(5)])])),
                expr_stmt(field_call("fmt", "Println", vec![fn_call("classify", vec![unary(UnaryOperator::Neg, int(1))])])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("return \"negative\""), "generated source: {go_source}");
    assert!(!go_source.contains("= \"negative\""), "generated source: {go_source}");

    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skipping go build verification: `go` not found on PATH");
        return;
    };

    assert!(go_version.status.success());

    let dir = std::env::temp_dir().join("duckc-gost-never-type-test");
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");

    let file_path = dir.join("main.go");

    let mut file = std::fs::File::create(&file_path).expect("failed to create temp go file");
    file.write_all(go_source.as_bytes()).expect("failed to write temp go file");
    drop(file);

    let output = Command::new("go")
        .arg("run")
        .arg(&file_path)
        .output()
        .expect("failed to run `go run`");

    assert!(
        output.status.success(),
        "go run failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );

    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "non-negative\nnegative");
}
