use std::io::Write;
use std::process::Command;

use crate::ast::TypeExpression;
use crate::ast::builder::{array, array_index, expr_stmt, field_access, field_call, fn_def, mem_name, name_target, no_type, program, string, struct_def, struct_init, type_, use_stmt, var_decl};
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
