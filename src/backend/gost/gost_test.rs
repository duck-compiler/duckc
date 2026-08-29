use std::process::Command;

use bumpalo::Bump;

use crate::ast::{Statement, TypeExpression};
use crate::ast::struct_definition::{Method, MethodKind, Visibility};
use crate::ast::expression::{BinaryOperator, UnaryOperator};
use crate::ast::builder::{array, generic_field_access, generic_fn_call, generic_fn_def, generic_method, generic_struct_def, generic_type, named_type, array_index, assign, binary, bool_lit, break_stmt, continue_stmt, deref_target, float, expr_stmt, field_access, field_call, field_target, call, fn_call, fn_def, if_else_expr, if_expr, int, mem_name, name_target, no_type, pointer_type, priv_field, priv_method, program, pub_field, pub_method, pub_static_method, pub_struct_def, reference, return_stmt, string, struct_def_with_impl, struct_init, tuple, tuple_index, tuple_index_target, tuple_type, type_, unary, use_stmt, var_decl, while_expr};
use crate::backend::semantics::{analyze_module, context::SemanticsContext};
use crate::backend::gost::{emit_gost, translate};

fn go_run(go_source: &str, test_dir: &str) -> Option<String> {
    let Ok(go_version) = Command::new("go").arg("version").output() else {
        eprintln!("skip go build: go not found in PATH");
        return None;
    };

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

    Some(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

#[test]
fn array_literal_and_index_translate_to_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);

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
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains(r#"[]string{"a", "b"}"#), "generated source: {go_source}");
    assert!(go_source.contains("arr[i]"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-array-test") else { return };
    assert_eq!(stdout, "a");
}

#[test]
fn struct_literal_and_field_access_translate_to_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        pub_struct_def("Point", vec![("x", TypeExpression::String), ("y", TypeExpression::String)]),
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

    let Some(stdout) = go_run(&go_source, "duckc-gost-struct-test") else { return };
    assert_eq!(stdout, "a");
}

#[test]
fn go_stdlib_struct_return_type_synthesizes_a_duck_struct_and_translates_to_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
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

    let Some(stdout) = go_run(&go_source, "duckc-gost-go-struct-test") else { return };
    assert_eq!(stdout, "ok");
}

#[test]
fn control_flow_arithmetic_and_assignment_translate_to_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
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

    let Some(stdout) = go_run(&go_source, "duckc-gost-control-flow-test") else { return };
    assert_eq!(stdout, "loop-body\ndone");
}

#[test]
fn if_else_used_as_a_value_translates_and_runs_correctly() {
    // `if`/`else` used as a value-producing expression (assigned into a
    // variable) rather than as a bare statement. Go has no if-expression, so
    // this must hoist a temporary, assign into it from both branches, and
    // reference the temporary as the expression's value - this is the actual
    // behavior under test, verified by running the real branch outcome
    // through `go run` for both the true and false case.
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
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

    let Some(stdout) = go_run(&go_source, "duckc-gost-if-value-test") else { return };
    assert_eq!(stdout, "yes\nno");
}

#[test]
fn return_comparison_logic_and_loop_control_translate_and_run_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);

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

    let Some(stdout) = go_run(&go_source, "duckc-gost-return-control-flow-test") else { return };
    assert_eq!(stdout, "looped");
}

#[test]
fn if_with_diverging_branch_used_as_a_value_translates_and_runs_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
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

    let Some(stdout) = go_run(&go_source, "duckc-gost-never-type-test") else { return };
    assert_eq!(stdout, "non-negative\nnegative");
}

#[test]
fn function_value_stored_in_a_variable_translates_and_runs_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def("shout", vec![("s", TypeExpression::String)], type_(TypeExpression::String), vec![
            return_stmt(Some(binary(mem_name("s"), BinaryOperator::Add, string("!")))),
        ]),
        fn_def("whisper", vec![("s", TypeExpression::String)], type_(TypeExpression::String), vec![
            return_stmt(Some(mem_name("s"))),
        ]),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("loud", type_(TypeExpression::Bool), Some(bool_lit(true))),
                var_decl("op", no_type(), Some(if_else_expr(
                    mem_name("loud"),
                    vec![expr_stmt(mem_name("shout"))],
                    vec![expr_stmt(mem_name("whisper"))],
                ))),
                expr_stmt(field_call("fmt", "Println", vec![fn_call("op", vec![string("hi")])])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("func(string) string"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-fn-value-test") else { return };
    assert_eq!(stdout, "hi!");
}

#[test]
fn while_used_as_a_value_translates_and_runs_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("running", type_(TypeExpression::Bool), Some(bool_lit(true))),
                var_decl("_", no_type(), Some(while_expr(mem_name("running"), vec![
                    assign(name_target("running"), bool_lit(false)),
                ]))),
                expr_stmt(field_call("fmt", "Println", vec![string("done")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let gost_root = translate(&context, module);
    let go_source = emit_gost(gost_root);

    assert!(go_source.contains("struct{}{}"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-while-value-test") else { return };
    assert_eq!(stdout, "done");
}

#[test]
fn pointer_parameter_writes_through_to_the_callers_variable() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "set",
            vec![("p", pointer_type(TypeExpression::Int)), ("v", TypeExpression::Int)],
            no_type(),
            vec![assign(deref_target(mem_name("p")), mem_name("v"))],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("x", type_(TypeExpression::Int), Some(int(1))),
                expr_stmt(fn_call("set", vec![reference(mem_name("x")), int(42)])),
                expr_stmt(field_call("fmt", "Println", vec![field_call("strconv", "Itoa", vec![mem_name("x")])])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("func set(p *int, v int)"), "generated source: {go_source}");
    assert!(go_source.contains("(*p) = v"), "generated source: {go_source}");
    assert!(go_source.contains("set((&x), 42)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-pointer-param-test") else { return };
    assert_eq!(stdout, "42");
}

#[test]
fn pointer_to_struct_reads_and_writes_fields_through_auto_dereference() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        pub_struct_def("Point", vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)]),
        fn_def(
            "shift",
            vec![("p", pointer_type(named_type("Point")))],
            no_type(),
            vec![assign(
                field_target(name_target("p"), "x"),
                binary(field_access(name_target("p"), "x"), BinaryOperator::Add, int(10)),
            )],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("pt", no_type(), Some(struct_init("Point", vec![("x", int(1)), ("y", int(2))]))),
                expr_stmt(fn_call("shift", vec![reference(mem_name("pt"))])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![field_access(name_target("pt"), "x")]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("func shift(p *Point)"), "generated source: {go_source}");
    assert!(go_source.contains("p.x = (p.x + 10)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-pointer-struct-test") else { return };
    assert_eq!(stdout, "11");
}

#[test]
fn chained_pointer_fields_auto_dereference_and_run_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        pub_struct_def("Node", vec![
            ("value", TypeExpression::Int),
            ("next", pointer_type(named_type("Node"))),
        ]),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("third", type_(named_type("Node")), None),
                assign(field_target(name_target("third"), "value"), int(3)),
                var_decl("second", no_type(), Some(struct_init("Node", vec![
                    ("value", int(2)),
                    ("next", reference(mem_name("third"))),
                ]))),
                var_decl("first", no_type(), Some(struct_init("Node", vec![
                    ("value", int(1)),
                    ("next", reference(mem_name("second"))),
                ]))),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![
                        field_access(field_target(field_target(name_target("first"), "next"), "next"), "value"),
                    ]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("next *Node"), "generated source: {go_source}");
    assert!(go_source.contains("first.next.next.value"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-linked-list-test") else { return };
    assert_eq!(stdout, "3");
}

#[test]
fn sized_integer_declaration_emits_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("x", type_(TypeExpression::Int64), Some(int(5))),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "FormatInt", vec![mem_name("x"), int(10)]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var x int64 = 5"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-sized-int-test") else { return };
    assert_eq!(stdout, "5");
}

#[test]
fn sixteen_bit_declarations_emit_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("signed", type_(TypeExpression::Int16), Some(unary(UnaryOperator::Neg, int(32768)))),
                var_decl("unsigned", type_(TypeExpression::Uint16), Some(int(65535))),
                expr_stmt(if_else_expr(
                    binary(
                        binary(mem_name("signed"), BinaryOperator::Less, int(0)),
                        BinaryOperator::And,
                        binary(mem_name("unsigned"), BinaryOperator::Greater, int(0)),
                    ),
                    vec![expr_stmt(field_call("fmt", "Println", vec![string("sixteen-bit-ok")]))],
                    vec![expr_stmt(field_call("fmt", "Println", vec![string("sixteen-bit-wrong")]))],
                )),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var signed int16 = (-32768)"), "generated source: {go_source}");
    assert!(go_source.contains("var unsigned uint16 = 65535"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-sixteen-bit-test") else { return };
    assert_eq!(stdout, "sixteen-bit-ok");
}

#[test]
fn go_function_taking_a_pointer_to_a_sized_int_writes_back_into_the_duck_variable() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("flag", None),
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("count", type_(TypeExpression::Int64), Some(int(0))),
                expr_stmt(field_call("flag", "Int64Var", vec![
                    reference(mem_name("count")),
                    string("n"),
                    int(7),
                    string("how many"),
                ])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "FormatInt", vec![mem_name("count"), int(10)]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var count int64 = 0"), "generated source: {go_source}");
    assert!(go_source.contains(r#"flag.Int64Var((&count), "n", 7, "how many")"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-go-pointer-param-test") else { return };
    assert_eq!(stdout, "7");
}

#[test]
fn aliased_go_import_uses_the_alias_for_the_import_and_for_synthesized_type_names() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("image", Some("im")),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("_", no_type(), Some(array(vec![field_call("im", "Pt", vec![int(1), int(2)])]))),
                expr_stmt(field_call("fmt", "Println", vec![string("ok")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains(r#"import im "image""#), "generated source: {go_source}");
    assert!(go_source.contains("[]im.Point{im.Pt(1, 2)}"), "generated source: {go_source}");
    assert!(!go_source.contains("image.Point"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-aliased-import-test") else { return };
    assert_eq!(stdout, "ok");
}

#[test]
fn multi_segment_go_import_is_referenced_by_its_short_package_name() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("container/list", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("_", no_type(), Some(array(vec![field_call("list", "New", vec![])]))),
                expr_stmt(field_call("fmt", "Println", vec![string("ok")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains(r#"import "container/list""#), "generated source: {go_source}");
    assert!(go_source.contains("[]*list.List{list.New()}"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-multi-segment-import-test") else { return };
    assert_eq!(stdout, "ok");
}

#[test]
fn a_versioned_go_import_is_referenced_by_the_package_name_go_binds() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("math/rand/v2", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("_", no_type(), Some(field_call("rand", "IntN", vec![int(10)]))),
                expr_stmt(field_call("fmt", "Println", vec![string("ok")])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains(r#"import "math/rand/v2""#), "generated source: {go_source}");
    assert!(go_source.contains("rand.IntN(10)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-versioned-import-test") else { return };
    assert_eq!(stdout, "ok");
}

#[test]
fn float_literals_emit_as_untyped_constants_that_fit_float32_and_float64() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("narrow", type_(TypeExpression::Float32), Some(float(1.5))),
                var_decl("inferred", no_type(), Some(float(2.0))),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "FormatFloat", vec![mem_name("inferred"), int(102), int(1), int(64)]),
                ])),
                expr_stmt(if_else_expr(
                    binary(mem_name("narrow"), BinaryOperator::Greater, float(1.0)),
                    vec![expr_stmt(field_call("fmt", "Println", vec![string("narrow-ok")]))],
                    vec![expr_stmt(field_call("fmt", "Println", vec![string("narrow-wrong")]))],
                )),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var narrow float32 = 1.5"), "generated source: {go_source}");
    assert!(go_source.contains("2.0"), "generated source: {go_source}");
    assert!(go_source.contains("strconv.FormatFloat(inferred, 102, 1, 64)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-float-literal-test") else { return };
    assert_eq!(stdout, "2.0\nnarrow-ok");
}

#[test]
fn negative_and_branch_valued_sized_literals_emit_valid_go() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
                var_decl("negative", type_(TypeExpression::Int64), Some(unary(UnaryOperator::Neg, int(5)))),
                var_decl("branched", type_(TypeExpression::Int64), Some(if_else_expr(
                    mem_name("flag"),
                    vec![expr_stmt(int(7))],
                    vec![expr_stmt(int(9))],
                ))),
                var_decl("widened", type_(TypeExpression::Array { inner: Box::new(TypeExpression::Int64) }), Some(array(vec![int(1), int(2)]))),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "FormatInt", vec![
                        binary(
                            binary(mem_name("negative"), BinaryOperator::Add, mem_name("branched")),
                            BinaryOperator::Add,
                            array_index("widened", int(1)),
                        ),
                        int(10),
                    ]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var negative int64 = (-5)"), "generated source: {go_source}");
    assert!(go_source.contains("var __duck_if_0 int64"), "generated source: {go_source}");
    assert!(go_source.contains("[]int64{1, 2}"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-sized-literal-forms-test") else { return };
    assert_eq!(stdout, "4");
}

#[test]
fn an_instance_method_writes_through_self_and_the_change_survives_the_call() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        struct_def_with_impl(
            "Counter",
            vec![priv_field("value", TypeExpression::Int)],
            vec![
                priv_method("add", vec![("by", TypeExpression::Int)], no_type(), vec![
                    assign(
                        field_target(name_target("self"), "value"),
                        binary(field_access(name_target("self"), "value"), BinaryOperator::Add, mem_name("by")),
                    ),
                ]),
                pub_method("bump", vec![("by", TypeExpression::Int)], no_type(), vec![
                    expr_stmt(call(field_access(name_target("self"), "add"), vec![mem_name("by")])),
                ]),
                pub_method("get", vec![], type_(TypeExpression::Int), vec![
                    return_stmt(Some(field_access(name_target("self"), "value"))),
                ]),
            ],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("c", type_(named_type("Counter")), None),
                expr_stmt(call(field_access(name_target("c"), "bump"), vec![int(5)])),
                expr_stmt(call(field_access(name_target("c"), "bump"), vec![int(2)])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![call(field_access(name_target("c"), "get"), vec![])]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("func (self *Counter) bump(by int)"), "generated source: {go_source}");
    assert!(go_source.contains("self.value = (self.value + by)"), "generated source: {go_source}");
    assert!(go_source.contains("self.add(by)"), "generated source: {go_source}");
    assert!(go_source.contains("c.bump(5)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-instance-method-test") else { return };
    assert_eq!(stdout, "7");
}

#[test]
fn a_struct_with_private_state_a_static_constructor_and_public_methods_runs_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        struct_def_with_impl(
            "Point",
            vec![priv_field("x", TypeExpression::Int), priv_field("y", TypeExpression::Int)],
            vec![
                pub_static_method(
                    "new",
                    vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)],
                    type_(named_type("Point")),
                    vec![
                        return_stmt(Some(struct_init("Point", vec![
                            ("x", mem_name("x")),
                            ("y", mem_name("y")),
                        ]))),
                    ],
                ),
                pub_method("set_x", vec![("value", TypeExpression::Int)], no_type(), vec![
                    assign(field_target(name_target("self"), "x"), mem_name("value")),
                ]),
                pub_method("sum", vec![], type_(TypeExpression::Int), vec![
                    return_stmt(Some(binary(
                        field_access(name_target("self"), "x"),
                        BinaryOperator::Add,
                        field_access(name_target("self"), "y"),
                    ))),
                ]),
            ],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("p", no_type(), Some(call(field_access(name_target("Point"), "new"), vec![int(3), int(4)]))),
                expr_stmt(call(field_access(name_target("p"), "set_x"), vec![int(10)])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![call(field_access(name_target("p"), "sum"), vec![])]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("func Point_new(x int, y int) Point"), "generated source: {go_source}");
    assert!(go_source.contains("func (self *Point) set_x(value int)"), "generated source: {go_source}");
    assert!(go_source.contains("= Point_new(3, 4)"), "generated source: {go_source}");
    assert!(go_source.contains("p.set_x(10)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-struct-methods-test") else { return };
    assert_eq!(stdout, "14");
}

#[test]
fn tuple_survives_a_function_call() {
    let int_and_string = || tuple_type(vec![TypeExpression::Int, TypeExpression::String]);

    let arena = Bump::new();

    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "swap",
            vec![("p", int_and_string())],
            type_(tuple_type(vec![TypeExpression::String, TypeExpression::Int])),
            vec![return_stmt(Some(tuple(vec![
                tuple_index(name_target("p"), 1),
                tuple_index(name_target("p"), 0),
            ])))],
        ),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("pair", type_(int_and_string()), Some(tuple(vec![int(1), string("a")]))),
                assign(tuple_index_target(name_target("pair"), 0), int(7)),
                var_decl("swapped", no_type(), Some(fn_call("swap", vec![mem_name("pair")]))),
                expr_stmt(field_call("fmt", "Println", vec![tuple_index(name_target("swapped"), 0)])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![tuple_index(name_target("swapped"), 1)]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(
        go_source.contains("func swap(p struct { _0 int; _1 string }) struct { _0 string; _1 int }"),
        "generated source: {go_source}",
    );
    assert!(go_source.contains(r#"struct { _0 int; _1 string }{_0: 1, _1: "a"}"#), "generated source: {go_source}");
    assert!(go_source.contains("pair._0 = 7"), "generated source: {go_source}");
    assert!(go_source.contains("swapped._0"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-tuple-test") else { return };
    assert_eq!(stdout, "a\n7");
}

#[test]
fn go_multi_result_becomes_a_tuple() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("math", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("parts", no_type(), Some(field_call("math", "Frexp", vec![float(8.0)]))),
                expr_stmt(field_call("math", "Frexp", vec![float(2.0)])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "FormatFloat", vec![tuple_index(name_target("parts"), 0), int(102), int(1), int(64)]),
                ])),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![tuple_index(name_target("parts"), 1)]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("__duck_tuple_0, __duck_tuple_1 := math.Frexp(8.0)"), "generated source: {go_source}");
    assert!(
        go_source.contains("struct { _0 float64; _1 int }{_0: __duck_tuple_0, _1: __duck_tuple_1}"),
        "generated source: {go_source}",
    );
    assert!(go_source.contains("math.Frexp(2.0)"), "generated source: {go_source}");
    assert!(!go_source.contains("__duck_tuple_2"), "an unused go call must not be destructured: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-go-multi-result-test") else { return };
    assert_eq!(stdout, "0.5\n4");
}

#[test]
fn a_wide_tuple_emits_valid_go() {
    let width = 20;

    let arena = Bump::new();

    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl(
                    "wide",
                    type_(tuple_type((0..width).map(|_| TypeExpression::Int).collect())),
                    Some(tuple((0..width).map(int).collect())),
                ),
                expr_stmt(field_call("fmt", "Println", vec![
                    field_call("strconv", "Itoa", vec![tuple_index(name_target("wide"), width as usize - 1)]),
                ])),
            ],
        ),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("_19 int"), "generated source: {go_source}");
    assert!(go_source.contains("wide._19"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-wide-tuple-test") else { return };
    assert_eq!(stdout, "19");
}

fn box_of_t_with<'src>(methods: Vec<Method<'src>>) -> Statement<'src> {
    generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], methods)
}

#[test]
fn generic_struct_function_and_constructor_run() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        box_of_t_with(vec![
            pub_method("get", vec![], type_(named_type("T")), vec![
                return_stmt(Some(field_access(name_target("self"), "value"))),
            ]),
            generic_method(
                Visibility::Public,
                MethodKind::Static,
                "of",
                vec![],
                vec![("value", named_type("T"))],
                type_(generic_type("Box", vec![named_type("T")])),
                vec![return_stmt(Some(struct_init("Box", vec![("value", mem_name("value"))])))],
            ),
        ]),
        generic_fn_def("identity", vec!["V"], vec![("value", named_type("V"))], type_(named_type("V")), vec![
            return_stmt(Some(mem_name("value"))),
        ]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("boxed", no_type(), Some(call(field_access(name_target("Box"), "of"), vec![int(7)]))),
            var_decl("number", no_type(), Some(call(field_access(name_target("boxed"), "get"), vec![]))),
            var_decl("text", no_type(), Some(fn_call("identity", vec![string("ok")]))),
            expr_stmt(field_call("fmt", "Println", vec![binary(
                field_call("strconv", "Itoa", vec![mem_name("number")]),
                BinaryOperator::Add,
                mem_name("text"),
            )])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("type Box[T any] struct"), "generated source: {go_source}");
    assert!(go_source.contains("func (self *Box[T]) get() T"), "generated source: {go_source}");
    assert!(go_source.contains("func Box_of[T any](value T) Box[T]"), "generated source: {go_source}");
    assert!(go_source.contains("func identity[V any](value V) V"), "generated source: {go_source}");
    assert!(go_source.contains("Box_of[int](7)"), "generated source: {go_source}");
    assert!(go_source.contains("boxed.get()"), "generated source: {go_source}");
    assert!(go_source.contains(r#"identity[string]("ok")"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-generic-struct-test") else { return };
    assert_eq!(stdout, "7ok");
}

#[test]
fn generic_method_becomes_a_free_function() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        generic_struct_def(
            "Pair",
            vec!["A", "B"],
            vec![pub_field("first", named_type("A")), pub_field("second", named_type("B"))],
            vec![],
        ),
        box_of_t_with(vec![
            generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "with",
                vec!["U"],
                vec![("other", named_type("U"))],
                type_(generic_type("Pair", vec![named_type("T"), named_type("U")])),
                vec![return_stmt(Some(struct_init("Pair", vec![
                    ("first", field_access(name_target("self"), "value")),
                    ("second", mem_name("other")),
                ])))],
            ),
        ]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("boxed", no_type(), Some(struct_init("Box", vec![("value", int(3))]))),
            var_decl("paired", no_type(), Some(call(
                field_access(name_target("boxed"), "with"),
                vec![string("three")],
            ))),
            expr_stmt(field_call("fmt", "Println", vec![field_access(name_target("paired"), "second")])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(
        go_source.contains("func Box_with[T any, U any](self *Box[T], other U) Pair[T, U]"),
        "generated source: {go_source}",
    );
    assert!(go_source.contains("Pair[T, U]{first: self.value, second: other}"), "generated source: {go_source}");
    assert!(go_source.contains(r#"Box_with[int, string]((&boxed), "three")"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-generic-method-test") else { return };
    assert_eq!(stdout, "three");
}

#[test]
fn generic_methods_work_as_values() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        use_stmt("strconv", None),
        box_of_t_with(vec![
            pub_method("get", vec![], type_(named_type("T")), vec![
                return_stmt(Some(field_access(name_target("self"), "value"))),
            ]),
            generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "with",
                vec!["U"],
                vec![("other", named_type("U"))],
                type_(named_type("U")),
                vec![return_stmt(Some(mem_name("other")))],
            ),
        ]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("boxed", no_type(), Some(struct_init("Box", vec![("value", int(4))]))),
            var_decl("get", no_type(), Some(field_access(name_target("boxed"), "get"))),
            var_decl("with", no_type(), Some(generic_field_access(
                name_target("boxed"),
                "with",
                vec![TypeExpression::String],
            ))),
            expr_stmt(field_call("fmt", "Println", vec![binary(
                field_call("strconv", "Itoa", vec![call(mem_name("get"), vec![])]),
                BinaryOperator::Add,
                call(mem_name("with"), vec![string("!")]),
            )])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("= boxed.get"), "generated source: {go_source}");
    assert!(go_source.contains("__duck_receiver_0 *Box[int] = (&boxed)"), "generated source: {go_source}");
    assert!(
        go_source.contains("Box_with[int, string](__duck_receiver_0, __duck_arg_0_0)"),
        "generated source: {go_source}",
    );

    let Some(stdout) = go_run(&go_source, "duckc-gost-generic-method-value-test") else { return };
    assert_eq!(stdout, "4!");
}

#[test]
fn explicit_type_argument_reaches_the_go_call() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        generic_fn_def("empty", vec!["T"], vec![], type_(named_type("T")), vec![
            var_decl("zero", type_(named_type("T")), None),
            return_stmt(Some(mem_name("zero"))),
        ]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("text", no_type(), Some(generic_fn_call("empty", vec![TypeExpression::String], vec![]))),
            expr_stmt(field_call("fmt", "Println", vec![binary(
                mem_name("text"),
                BinaryOperator::Add,
                string("done"),
            )])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("func empty[T any]() T"), "generated source: {go_source}");
    assert!(go_source.contains("empty[string]()"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-explicit-type-arg-test") else { return };
    assert_eq!(stdout, "done");
}

#[test]
fn nested_generic_struct_instantiates_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        box_of_t_with(vec![]),
        generic_fn_def(
            "unwrap",
            vec!["T"],
            vec![("boxed", generic_type("Box", vec![named_type("T")]))],
            type_(named_type("T")),
            vec![return_stmt(Some(field_access(name_target("boxed"), "value")))],
        ),
        fn_def("main", vec![], no_type(), vec![
            var_decl("inner", no_type(), Some(struct_init("Box", vec![("value", string("deep"))]))),
            var_decl("outer", no_type(), Some(struct_init("Box", vec![("value", mem_name("inner"))]))),
            var_decl("unwrapped", no_type(), Some(fn_call("unwrap", vec![mem_name("outer")]))),
            expr_stmt(field_call("fmt", "Println", vec![fn_call("unwrap", vec![mem_name("unwrapped")])])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains(r#"Box[string]{value: "deep"}"#), "generated source: {go_source}");
    assert!(go_source.contains("Box[Box[string]]{value: inner}"), "generated source: {go_source}");
    assert!(go_source.contains("unwrap[Box[string]](outer)"), "generated source: {go_source}");
    assert!(go_source.contains("unwrap[string](unwrapped)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-nested-generic-test") else { return };
    assert_eq!(stdout, "deep");
}

#[test]
fn recursive_generic_call_instantiates_correctly() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        generic_fn_def(
            "twice",
            vec!["T"],
            vec![("value", named_type("T")), ("again", TypeExpression::Bool)],
            type_(named_type("T")),
            vec![
                expr_stmt(if_expr(mem_name("again"), vec![
                    return_stmt(Some(generic_fn_call(
                        "twice",
                        vec![named_type("T")],
                        vec![mem_name("value"), bool_lit(false)],
                    ))),
                ])),
                return_stmt(Some(mem_name("value"))),
            ],
        ),
        fn_def("main", vec![], no_type(), vec![
            expr_stmt(field_call("fmt", "Println", vec![fn_call("twice", vec![string("echo"), bool_lit(true)])])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("twice[T](value, false)"), "generated source: {go_source}");
    assert!(go_source.contains(r#"twice[string]("echo", true)"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-generic-recursion-test") else { return };
    assert_eq!(stdout, "echo");
}

#[test]
fn generic_method_runs_on_self_and_through_a_pointer() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        box_of_t_with(vec![
            generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "with",
                vec!["U"],
                vec![("other", named_type("U"))],
                type_(named_type("U")),
                vec![return_stmt(Some(mem_name("other")))],
            ),
            pub_method("shout", vec![("text", TypeExpression::String)], type_(TypeExpression::String), vec![
                return_stmt(Some(call(field_access(name_target("self"), "with"), vec![mem_name("text")]))),
            ]),
        ]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("boxed", no_type(), Some(struct_init("Box", vec![("value", int(1))]))),
            var_decl("pointer", no_type(), Some(reference(mem_name("boxed")))),
            var_decl("text", no_type(), Some(call(field_access(name_target("pointer"), "shout"), vec![string("hi")]))),
            expr_stmt(field_call("fmt", "Println", vec![mem_name("text")])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("Box_with[T, string](self, text)"), "generated source: {go_source}");
    assert!(go_source.contains("pointer.shout(\"hi\")"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-generic-self-call-test") else { return };
    assert_eq!(stdout, "hi");
}

#[test]
fn generic_method_on_a_plain_struct_becomes_a_free_function() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        struct_def_with_impl(
            "Printer",
            vec![pub_field("prefix", TypeExpression::String)],
            vec![generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "show",
                vec!["U"],
                vec![("value", named_type("U"))],
                type_(named_type("U")),
                vec![return_stmt(Some(mem_name("value")))],
            )],
        ),
        fn_def("main", vec![], no_type(), vec![
            var_decl("printer", no_type(), Some(struct_init("Printer", vec![("prefix", string(">"))]))),
            expr_stmt(field_call("fmt", "Println", vec![call(
                field_access(name_target("printer"), "show"),
                vec![string("shown")],
            )])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(
        go_source.contains("func Printer_show[U any](self *Printer, value U) U"),
        "generated source: {go_source}",
    );
    assert!(go_source.contains(r#"Printer_show[string]((&printer), "shown")"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-plain-struct-generic-method-test") else { return };
    assert_eq!(stdout, "shown");
}

#[test]
fn static_generic_method_works_as_a_value() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        box_of_t_with(vec![generic_method(
            Visibility::Public,
            MethodKind::Static,
            "of",
            vec![],
            vec![("value", named_type("T"))],
            type_(generic_type("Box", vec![named_type("T")])),
            vec![return_stmt(Some(struct_init("Box", vec![("value", mem_name("value"))])))],
        )]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("make", no_type(), Some(generic_field_access(
                name_target("Box"),
                "of",
                vec![TypeExpression::String],
            ))),
            var_decl("boxed", no_type(), Some(call(mem_name("make"), vec![string("made")]))),
            expr_stmt(field_call("fmt", "Println", vec![field_access(name_target("boxed"), "value")])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("= Box_of[string]"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-static-generic-value-test") else { return };
    assert_eq!(stdout, "made");
}

#[test]
fn a_closure_survives_being_a_branch_value() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        box_of_t_with(vec![generic_method(
            Visibility::Public,
            MethodKind::Instance,
            "with",
            vec!["U"],
            vec![("other", named_type("U"))],
            type_(named_type("U")),
            vec![return_stmt(Some(mem_name("other")))],
        )]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("first", no_type(), Some(struct_init("Box", vec![("value", int(1))]))),
            var_decl("second", no_type(), Some(struct_init("Box", vec![("value", int(2))]))),
            var_decl("take_first", type_(TypeExpression::Bool), Some(bool_lit(false))),
            var_decl("with", no_type(), Some(if_else_expr(
                mem_name("take_first"),
                vec![expr_stmt(generic_field_access(name_target("first"), "with", vec![TypeExpression::String]))],
                vec![expr_stmt(generic_field_access(name_target("second"), "with", vec![TypeExpression::String]))],
            ))),
            expr_stmt(field_call("fmt", "Println", vec![call(mem_name("with"), vec![string("branch")])])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("var __duck_if_0 func(string) string"), "generated source: {go_source}");
    assert!(go_source.contains("__duck_receiver_0 *Box[int] = (&first)"), "generated source: {go_source}");
    assert!(go_source.contains("__duck_receiver_1 *Box[int] = (&second)"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-closure-in-branch-test") else { return };
    assert_eq!(stdout, "branch");
}

#[test]
fn static_method_type_parameters_follow_the_structs_own() {
    let make = |type_args: Vec<TypeExpression<'static>>| {
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![generic_method(
            Visibility::Public,
            MethodKind::Static,
            "make",
            vec!["U"],
            vec![("value", named_type("T")), ("tag", named_type("U"))],
            type_(generic_type("Box", vec![named_type("T")])),
            vec![return_stmt(Some(struct_init("Box", vec![("value", mem_name("value"))])))],
        )])
    };

    let arena = Bump::new();

    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        make(vec![]),
        fn_def("main", vec![], no_type(), vec![
            var_decl("inferred", no_type(), Some(call(
                field_access(name_target("Box"), "make"),
                vec![string("one"), int(1)],
            ))),
            var_decl("explicit", no_type(), Some(call(
                generic_field_access(name_target("Box"), "make", vec![TypeExpression::String, TypeExpression::Int]),
                vec![string("two"), int(2)],
            ))),
            expr_stmt(field_call("fmt", "Println", vec![binary(
                field_access(name_target("inferred"), "value"),
                BinaryOperator::Add,
                field_access(name_target("explicit"), "value"),
            )])),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(
        go_source.contains("func Box_make[T any, U any](value T, tag U) Box[T]"),
        "generated source: {go_source}",
    );
    assert!(go_source.contains(r#"Box_make[string, int]("one", 1)"#), "generated source: {go_source}");
    assert!(go_source.contains(r#"Box_make[string, int]("two", 2)"#), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-static-own-type-param-test") else { return };
    assert_eq!(stdout, "onetwo");
}

#[test]
fn a_never_argument_does_not_poison_its_neighbors() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);
    let module = context.add_module(program(vec![
        use_stmt("fmt", None),
        generic_fn_def(
            "two",
            vec!["A", "B"],
            vec![("first", named_type("A")), ("second", named_type("B"))],
            type_(named_type("A")),
            vec![return_stmt(Some(mem_name("first")))],
        ),
        fn_def("main", vec![], no_type(), vec![
            var_decl("leave", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(field_call("fmt", "Println", vec![string("before")])),
            var_decl("_", no_type(), Some(fn_call("two", vec![
                int(5),
                if_else_expr(mem_name("leave"), vec![return_stmt(None)], vec![return_stmt(None)]),
            ]))),
        ]),
    ]));

    analyze_module(&mut context, module);
    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let go_source = emit_gost(translate(&context, module));

    assert!(go_source.contains("two[int, struct {"), "generated source: {go_source}");

    let Some(stdout) = go_run(&go_source, "duckc-gost-never-type-arg-test") else { return };
    assert_eq!(stdout, "before");
}
