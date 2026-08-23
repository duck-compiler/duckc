use super::{analyze_module, context::SemanticsContext, r#type::Type, symbol::SymbolKind};
use crate::ast::{AstRoot, Statement, TypeExpression, builder::{array, array_index, assign, expr_stmt, field_access, field_call, field_target, fn_call, fn_def, ident, int, name_target, no_type, program, string, struct_def, struct_init, type_, use_stmt, var_decl}};

fn analyze(program: AstRoot<'static>) -> SemanticsContext<'static> {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program);
    analyze_module(&mut context, module);
    context
}

fn has_error_code(context: &SemanticsContext, code: &str) -> bool {
    context.diagnostics.iter().any(|diagnostic| &*diagnostic.error_code == code)
}

fn main_fn<'src>(body: Vec<Statement<'src>>) -> Statement<'src> {
    fn_def("main", vec![], no_type(), body)
}

#[test]
fn hello_world_program_has_no_diagnostics_and_resolves_types() {
    let context = analyze(program(vec![
        use_stmt("fmt", None),
        main_fn(vec![expr_stmt(field_call("fmt", "Println", vec![string("Hello, World!")]))]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert!(context.symbols.iter().all(|symbol| {
        matches!(symbol.kind, SymbolKind::Module | SymbolKind::Struct) || symbol.type_.is_some()
    }));

    let fmt = context.symbols.iter().find(|s| s.name == "fmt").expect("fmt symbol");
    assert_eq!(fmt.kind, SymbolKind::Module);
    assert!(fmt.type_.is_none());
}

#[test]
fn wrong_arg_count_reports_t0003() {
    let context = analyze(program(vec![
        use_stmt("fmt", None),
        main_fn(vec![expr_stmt(field_call("fmt", "Println", vec![]))]),
    ]));

    assert!(has_error_code(&context, "T0003"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn type_mismatch_reports_t0001() {
    let context = analyze(program(vec![
        main_fn(vec![var_decl("x", type_(TypeExpression::Bool), Some(string("hello")))]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn calling_a_non_function_reports_t0002() {
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            expr_stmt(fn_call("x", vec![string("y")])),
        ]),
    ]));

    assert!(has_error_code(&context, "T0002"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn unknown_name_reports_s0001() {
    let context = analyze(program(vec![
        main_fn(vec![expr_stmt(fn_call("unknown", vec![]))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_literal_infers_array_of_element_type() {
    let context = analyze(program(vec![
        main_fn(vec![var_decl("arr", no_type(), Some(array(vec![string("a"), string("b")])))]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let array = context.symbols.iter().find(|s| s.name == "arr").expect("arr symbol");

    let array_type = context.types[array.type_.expect("arr should have a type").0 as usize].clone();
    match array_type {
        Type::Array(inner) => assert_eq!(context.types[inner.0 as usize], Type::String),
        other => panic!("expected Type::Array(String), found {:?}", other),
    }
}

#[test]
fn empty_array_literal_reports_t0005() {
    let context = analyze(program(vec![
        main_fn(vec![var_decl("arr", no_type(), Some(array(vec![])))]),
    ]));

    assert!(has_error_code(&context, "T0005"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_literal_with_mismatched_elements_reports_t0001() {
    let context = analyze(program(vec![
        main_fn(vec![var_decl("arr", no_type(), Some(array(vec![string("a"), int(1)])))]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_index_with_int_resolves_to_element_type() {
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("arr", no_type(), Some(array(vec![string("a"), string("b")]))),
            var_decl("elem", no_type(), Some(array_index("arr", int(0)))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let elem = context.symbols.iter().find(|s| s.name == "elem").expect("elem symbol");
    assert_eq!(context.types[elem.type_.expect("elem should have a type").0 as usize], Type::String);
}

#[test]
fn array_index_with_non_int_reports_t0001() {
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("arr", no_type(), Some(array(vec![string("a")]))),
            var_decl("elem", no_type(), Some(array_index("arr", string("not an int")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_a_non_array_reports_t0006() {
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            var_decl("elem", no_type(), Some(array_index("x", int(0)))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_literal_and_field_access_resolve_correctly() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1)), ("y", int(2))]))),
            var_decl("elem", no_type(), Some(field_access(name_target("p"), "x"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let elemet = context.symbols.iter().find(|s| s.name == "elem").expect("elem symbol");
    assert_eq!(context.types[elemet.type_.expect("elem should have a type").0 as usize], Type::Int);
}

#[test]
fn struct_literal_with_unknown_field_reports_t0008() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1)), ("z", int(2))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_literal_missing_field_reports_t0009() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0009"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_on_unknown_field_reports_t0008() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
            var_decl("elem", no_type(), Some(field_access(name_target("p"), "z"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_on_non_struct_reports_t0007() {
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            var_decl("elem", no_type(), Some(field_access(name_target("x"), "y"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0007"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_struct_field_access_resolves_correctly() {
    let context = analyze(program(vec![
        struct_def("Inner", vec![("value", TypeExpression::String)]),
        struct_def("Outer", vec![("inner", TypeExpression::Ident(ident("Inner")))]),
        main_fn(vec![
            var_decl("o", no_type(), Some(struct_init("Outer", vec![
                ("inner", struct_init("Inner", vec![("value", string("hi"))])),
            ]))),
            var_decl("elem", no_type(), Some(field_access(field_target(name_target("o"), "inner"), "value"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let elemet = context.symbols.iter().find(|s| s.name == "elem").expect("elem symbol");
    assert_eq!(context.types[elemet.type_.expect("elem should have a type").0 as usize], Type::String);
}

#[test]
fn struct_forward_reference_resolves_correctly() {
    let context = analyze(program(vec![
        struct_def("Outer", vec![("inner", TypeExpression::Ident(ident("Inner")))]),
        struct_def("Inner", vec![("value", TypeExpression::String)]),
        main_fn(vec![
            var_decl("o", no_type(), Some(struct_init("Outer", vec![
                ("inner", struct_init("Inner", vec![("value", string("hi"))])),
            ]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_field_assignment_type_mismatch_reports_t0001() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
            assign(field_target(name_target("p"), "x"), string("not an int")),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn go_stdlib_struct_return_type_resolves_exported_fields_through_the_real_toolchain() {
    let context = analyze(program(vec![
        use_stmt("image", None),
        main_fn(vec![
            var_decl("p", no_type(), Some(field_call("image", "Pt", vec![int(1), int(2)]))),
            var_decl("x", no_type(), Some(field_access(name_target("p"), "X"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let p = context.symbols.iter().find(|s| s.name == "p").expect("p symbol");
    let Type::Struct(struct_sym) = context.types[p.type_.expect("p should have a type").0 as usize] else {
        panic!("expected p to have a synthesized struct type, found {:?}", context.types[p.type_.unwrap().0 as usize]);
    };

    assert_eq!(context.symbols[struct_sym.0 as usize].name, "image.Point");

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn go_stdlib_struct_unexported_field_is_reported_as_unknown() {
    let context = analyze(program(vec![
        use_stmt("time", None),
        main_fn(vec![
            var_decl("now", no_type(), Some(field_call("time", "Now", vec![]))),
            var_decl("wall", no_type(), Some(field_access(name_target("now"), "wall"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}
