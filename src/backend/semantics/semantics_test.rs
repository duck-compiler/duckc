use bumpalo::Bump;

use super::{analyze_module, context::SemanticsContext, go_map::map_go_type, go_resolve::RawType, r#type::{Type, TypeId}, symbol::SymbolKind};
use crate::ast::{AstRoot, Expression, Statement, TypeExpression, expression::{BinaryOperator, UnaryOperator}, struct_definition::{Method, MethodKind, Visibility}, builder::{array, generic_call, generic_field_access, generic_fn_call, generic_fn_def, generic_method, generic_struct_def, generic_struct_init, generic_type, named_type, array_index, assign, binary, bool_lit, break_stmt, call, continue_stmt, dereference, deref_target, expr_stmt, field_access, field_call, field_target, float, fn_call, fn_def, if_else_expr, if_expr, int, mem_name, method, name_target, no_type, pointer_type, priv_field, priv_method, program, pub_field, pub_method, pub_static_method, pub_struct_def, reference, unary, return_stmt, string, struct_def, struct_def_with_impl, struct_init, tuple, tuple_index, tuple_index_target, tuple_type, type_, use_stmt, var_decl, while_expr}};

fn type_of<'src>(context: &SemanticsContext<'src>, name: &str) -> Type {
    let symbol = context.symbols.iter().find(|s| s.name == name).unwrap_or_else(|| panic!("no symbol `{name}`"));
    context.types[symbol.type_.unwrap_or_else(|| panic!("`{name}` should have a type")).0 as usize].clone()
}

fn tuple_element_types<'src>(context: &SemanticsContext<'src>, name: &str) -> Vec<Type> {
    let Type::Tuple(elements) = type_of(context, name) else {
        panic!("expected a tuple for `{name}`, found {:?}", type_of(context, name));
    };

    elements.iter().map(|element| context.types[element.0 as usize].clone()).collect()
}


fn analyze<'src>(arena: &'src Bump, program: AstRoot<'src>) -> SemanticsContext<'src> {
    let mut context = SemanticsContext::new(arena);
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

fn type_id_of<'src>(context: &SemanticsContext<'src>, name: &str) -> TypeId {
    let symbol = context.symbols
        .iter()
        .find(|s| s.name == name)
        .unwrap_or_else(|| panic!("no symbol `{name}`"));

    symbol.type_.unwrap_or_else(|| panic!("`{name}` should have a type"))
}

fn box_of_t<'src>() -> Statement<'src> {
    generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![])
}

fn identity_fn<'src>() -> Statement<'src> {
    generic_fn_def(
        "identity",
        vec!["T"],
        vec![("value", named_type("T"))],
        type_(named_type("T")),
        vec![return_stmt(Some(mem_name("value")))],
    )
}

fn generic_empty_fn<'src>() -> Statement<'src> {
    generic_fn_def("empty", vec!["T"], vec![], type_(named_type("T")), vec![
        var_decl("zero", type_(named_type("T")), None),
        return_stmt(Some(mem_name("zero"))),
    ])
}

#[test]
fn hello_world_program_has_no_diagnostics_and_resolves_types() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("fmt", None),
        main_fn(vec![expr_stmt(field_call("fmt", "Println", vec![]))]),
    ]));

    assert!(has_error_code(&context, "T0003"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn type_mismatch_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("x", type_(TypeExpression::Bool), Some(string("hello")))]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn calling_a_non_function_reports_t0002() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            expr_stmt(fn_call("x", vec![string("y")])),
        ]),
    ]));

    assert!(has_error_code(&context, "T0002"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn unknown_name_reports_s0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![expr_stmt(fn_call("unknown", vec![]))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_literal_infers_array_of_element_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("arr", no_type(), Some(array(vec![])))]),
    ]));

    assert!(has_error_code(&context, "T0005"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_literal_with_mismatched_elements_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("arr", no_type(), Some(array(vec![string("a"), int(1)])))]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn array_index_with_int_resolves_to_element_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("arr", no_type(), Some(array(vec![string("a")]))),
            var_decl("elem", no_type(), Some(array_index("arr", string("not an int")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_a_non_array_reports_t0006() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            var_decl("elem", no_type(), Some(array_index("x", int(0)))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_literal_and_field_access_resolve_correctly() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)]),
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1)), ("z", int(2))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_literal_missing_field_reports_t0009() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int), ("y", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0009"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_on_unknown_field_reports_t0008() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
            var_decl("elem", no_type(), Some(field_access(name_target("p"), "z"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_on_non_struct_reports_t0007() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            var_decl("elem", no_type(), Some(field_access(name_target("x"), "y"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0007"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_struct_field_access_resolves_correctly() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Inner", vec![("value", TypeExpression::String)]),
        pub_struct_def("Outer", vec![("inner", named_type("Inner"))]),
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Outer", vec![("inner", named_type("Inner"))]),
        pub_struct_def("Inner", vec![("value", TypeExpression::String)]),
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
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
            assign(field_target(name_target("p"), "x"), string("not an int")),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn go_stdlib_struct_return_type_resolves_exported_fields_through_the_real_toolchain() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("image", None),
        main_fn(vec![
            var_decl("p", no_type(), Some(field_call("image", "Pt", vec![int(1), int(2)]))),
            var_decl("x", no_type(), Some(field_access(name_target("p"), "X"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let p = context.symbols.iter().find(|s| s.name == "p").expect("p symbol");
    let Type::Struct(struct_sym, _) = context.types[p.type_.expect("p should have a type").0 as usize] else {
        panic!("expected p to have a synthesized struct type, found {:?}", context.types[p.type_.unwrap().0 as usize]);
    };

    assert_eq!(context.symbols[struct_sym.0 as usize].name, "image.Point");

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn go_stdlib_struct_unexported_field_is_reported_as_unknown() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("time", None),
        main_fn(vec![
            var_decl("now", no_type(), Some(field_call("time", "Now", vec![]))),
            var_decl("wall", no_type(), Some(field_access(name_target("now"), "wall"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn reference_then_dereference_round_trips_to_the_original_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::Int), Some(int(1))),
            var_decl("p", no_type(), Some(reference(mem_name("x")))),
            var_decl("y", no_type(), Some(dereference(mem_name("p")))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let int_type = context.types.iter().position(|t| *t == Type::Int).expect("int type");
    let p = context.symbols.iter().find(|s| s.name == "p").expect("p symbol");
    assert_eq!(
        context.types[p.type_.expect("p should have a type").0 as usize],
        Type::Pointer(TypeId(int_type as u32)),
    );

    let y = context.symbols.iter().find(|s| s.name == "y").expect("y symbol");
    assert_eq!(context.types[y.type_.expect("y should have a type").0 as usize], Type::Int);
}

#[test]
fn dereferencing_a_non_pointer_reports_t0015() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::Int), Some(int(1))),
            var_decl("y", no_type(), Some(dereference(mem_name("x")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0015"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn dereferencing_a_diverging_expression_does_not_report_t0015() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("y", no_type(), Some(dereference(if_else_expr(
                mem_name("flag"),
                vec![return_stmt(None)],
                vec![return_stmt(None)],
            )))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_literal_reports_t0016() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("p", no_type(), Some(reference(int(1))))]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_composite_literal_is_allowed() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(reference(struct_init("Point", vec![("x", int(1))])))),
            var_decl("a", no_type(), Some(reference(array(vec![int(1), int(2)])))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let pointee_of = |name: &str| {
        let symbol = context.symbols.iter().find(|s| s.name == name).expect("symbol");
        let Type::Pointer(inner) = context.types[symbol.type_.expect("should have a type").0 as usize] else {
            panic!("expected a pointer for `{name}`, found {:?}", context.types[symbol.type_.unwrap().0 as usize]);
        };
        context.types[inner.0 as usize].clone()
    };

    assert!(matches!(pointee_of("p"), Type::Struct(_, _)));
    assert_eq!(pointee_of("a"), Type::Array(TypeId(context.types.iter().position(|t| *t == Type::Int).expect("int type") as u32)));
}

#[test]
fn taking_the_address_of_a_duck_function_reports_t0016() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("helper", vec![], no_type(), vec![]),
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("helper"))))]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_go_package_function_reports_t0016() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("fmt", None),
        main_fn(vec![
            var_decl("p", no_type(), Some(reference(field_access(name_target("fmt"), "Println")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_an_unknown_name_reports_only_s0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("unknown"))))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_through_a_pointer_auto_dereferences() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
            var_decl("ptr", no_type(), Some(reference(mem_name("p")))),
            var_decl("x", no_type(), Some(field_access(name_target("ptr"), "x"))),
            assign(field_target(name_target("ptr"), "x"), int(2)),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn field_access_auto_dereferences_across_chained_pointer_fields() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Node", vec![
            ("value", TypeExpression::Int),
            ("next", pointer_type(named_type("Node")))
        ]),
        main_fn(vec![
            var_decl("tail", type_(named_type("Node")), None),
            var_decl("head", no_type(), Some(struct_init("Node", vec![
                ("value", int(1)),
                ("next", reference(mem_name("tail"))),
            ]))),
            var_decl("value", no_type(), Some(field_access(field_target(name_target("head"), "next"), "value"))),
            var_decl("nested", no_type(), Some(reference(field_access(
                field_target(field_target(name_target("head"), "next"), "next"),
                "value",
            )))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let value = context.symbols.iter().find(|s| s.name == "value").expect("value symbol");
    assert_eq!(context.types[value.type_.expect("value should have a type").0 as usize], Type::Int);

    let int_type = context.types.iter().position(|t| *t == Type::Int).expect("int type");
    let nested = context.symbols.iter().find(|s| s.name == "nested").expect("nested symbol");

    assert_eq!(
        context.types[nested.type_.expect("nested should have a type").0 as usize],
        Type::Pointer(TypeId(int_type as u32)),
    );
}

#[test]
fn indexing_through_a_pointer_to_an_array_reports_t0006() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("arr", no_type(), Some(array(vec![int(1)]))),
            var_decl("ptr", no_type(), Some(reference(mem_name("arr")))),
            var_decl("elem", no_type(), Some(array_index("ptr", int(0)))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn sized_numeric_annotation_resolves_to_its_own_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("small", type_(TypeExpression::Int64), Some(int(5))),
            var_decl("wide", type_(TypeExpression::Uint64), None),
            var_decl("ratio", type_(TypeExpression::Float32), None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let type_of = |name: &str| {
        let symbol = context.symbols.iter().find(|s| s.name == name).expect("symbol");
        context.types[symbol.type_.expect("should have a type").0 as usize].clone()
    };

    assert_eq!(type_of("small"), Type::Int64);
    assert_eq!(type_of("wide"), Type::Uint64);
    assert_eq!(type_of("ratio"), Type::Float32);
}

#[test]
fn a_sized_integer_is_not_compatible_with_plain_int() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Int64), None),
            var_decl("narrow", type_(TypeExpression::Int), Some(mem_name("wide"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn pointers_to_differently_sized_integers_are_distinct_types() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("takes_int64_pointer", vec![("p", pointer_type(TypeExpression::Int64))], no_type(), vec![]),
        main_fn(vec![
            var_decl("plain", type_(TypeExpression::Int), None),
            expr_stmt(fn_call("takes_int64_pointer", vec![reference(mem_name("plain"))])),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn go_pointer_to_sized_int_maps_through_the_real_toolchain() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("flag", None),
        main_fn(vec![
            var_decl("p", no_type(), Some(field_call("flag", "Int64", vec![string("n"), int(0), string("usage")]))),
            var_decl("v", no_type(), Some(dereference(mem_name("p")))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let int64_type = context.types.iter().position(|t| *t == Type::Int64).expect("int64 type");
    let p = context.symbols.iter().find(|s| s.name == "p").expect("p symbol");
    assert_eq!(
        context.types[p.type_.expect("p should have a type").0 as usize],
        Type::Pointer(TypeId(int64_type as u32)),
    );

    let v = context.symbols.iter().find(|s| s.name == "v").expect("v symbol");
    assert_eq!(context.types[v.type_.expect("v should have a type").0 as usize], Type::Int64);
}

fn go_struct_field_type(
    context: &mut SemanticsContext,
    package: &str,
    qualified_name: &str,
    field: &str,
) -> Type {
    let types = context.go_resolver.types_of(package).expect("package should resolve through the real toolchain");

    let mapped = map_go_type(context, package, &RawType::Named { r#ref: qualified_name.to_string() }, &types)
        .expect("named struct should map");

    let Type::Struct(struct_sym, _) = context.types[mapped.0 as usize] else {
        panic!("expected a struct, found {:?}", context.types[mapped.0 as usize]);
    };

    let field_type = context.struct_fields.get(&struct_sym)
        .expect("struct fields")
        .iter()
        .find(|(name, _, _)| *name == field)
        .map(|(_, type_id, _)| *type_id)
        .unwrap_or_else(|| panic!("no field `{field}` on `{qualified_name}`"));

    context.types[field_type.0 as usize].clone()
}

#[test]
fn go_sixteen_bit_fields_map_to_their_own_duck_types_through_the_real_toolchain() {
    let arena = Bump::new();
    let mut context = SemanticsContext::new(&arena);

    assert_eq!(go_struct_field_type(&mut context, "image/color", "image/color.Gray16", "Y"), Type::Uint16);
    assert_eq!(go_struct_field_type(&mut context, "database/sql", "database/sql.NullInt16", "Int16"), Type::Int16);
}

#[test]
fn go_time_duration_maps_to_int64_through_the_real_toolchain() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("time", None),
        main_fn(vec![
            var_decl("d", no_type(), Some(field_call("time", "Since", vec![field_call("time", "Now", vec![])]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let d = context.symbols.iter().find(|s| s.name == "d").expect("d symbol");
    assert_eq!(context.types[d.type_.expect("d should have a type").0 as usize], Type::Int64);
}

#[test]
fn if_else_used_as_a_value_resolves_to_the_branch_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("x", no_type(), Some(if_else_expr(
                mem_name("flag"),
                vec![expr_stmt(int(1))],
                vec![expr_stmt(int(2))],
            ))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn if_without_an_else_branch_used_as_a_value_reports_t0020() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("x", no_type(), Some(if_expr(mem_name("flag"), vec![expr_stmt(int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0020"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn if_without_an_else_branch_used_as_a_value_with_a_declared_type_reports_t0020() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("x", type_(TypeExpression::Int), Some(if_expr(mem_name("flag"), vec![expr_stmt(int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0020"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn if_without_an_else_branch_whose_body_produces_no_value_stays_unit() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("x", no_type(), Some(if_expr(mem_name("flag"), vec![
                var_decl("inner", no_type(), Some(int(1))),
            ]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Unit);
}

#[test]
fn if_without_an_else_branch_as_a_statement_may_produce_a_value() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(if_expr(mem_name("flag"), vec![expr_stmt(int(1))])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_if_without_an_else_branch_as_a_statement_may_produce_a_value() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(if_expr(mem_name("flag"), vec![
                expr_stmt(if_expr(mem_name("flag"), vec![expr_stmt(int(1))])),
            ])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn if_with_mismatched_branch_types_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("x", no_type(), Some(if_else_expr(
                mem_name("flag"),
                vec![expr_stmt(int(1))],
                vec![expr_stmt(string("two"))],
            ))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn top_level_expression_statement_reports_t0011() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        expr_stmt(int(1)),
    ]));

    assert!(has_error_code(&context, "T0011"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn return_with_matching_type_has_no_diagnostics() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("get_x", vec![], type_(TypeExpression::Int), vec![
            return_stmt(Some(int(5))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn return_with_mismatched_type_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("get_x", vec![], type_(TypeExpression::Int), vec![
            return_stmt(Some(string("not an int"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn bare_return_in_unit_function_has_no_diagnostics() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("do_nothing", vec![], no_type(), vec![
            return_stmt(None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn break_outside_loop_reports_t0012() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![break_stmt()]),
    ]));

    assert!(has_error_code(&context, "T0012"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn continue_outside_loop_reports_t0012() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![continue_stmt()]),
    ]));

    assert!(has_error_code(&context, "T0012"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn break_and_continue_inside_while_have_no_diagnostics() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("running", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(while_expr(mem_name("running"), vec![
                expr_stmt(if_expr(bool_lit(true), vec![continue_stmt()])),
                break_stmt(),
            ])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn break_inside_nested_if_inside_while_is_still_in_loop() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("running", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(while_expr(mem_name("running"), vec![
                expr_stmt(if_expr(bool_lit(true), vec![
                    expr_stmt(if_expr(bool_lit(true), vec![break_stmt()])),
                ])),
            ])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn comparison_operator_resolves_to_bool() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", no_type(), Some(binary(int(1), BinaryOperator::Less, int(2)))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Bool);
}

#[test]
fn comparison_result_can_drive_a_while_condition() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("i", type_(TypeExpression::Int), Some(int(0))),
            expr_stmt(while_expr(binary(mem_name("i"), BinaryOperator::Less, int(5)), vec![
                assign(name_target("i"), binary(mem_name("i"), BinaryOperator::Add, int(1))),
            ])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn logical_and_requires_bool_operands_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", no_type(), Some(binary(int(1), BinaryOperator::And, bool_lit(true)))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn logical_and_of_two_bools_resolves_to_bool() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", no_type(), Some(binary(bool_lit(true), BinaryOperator::And, bool_lit(false)))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Bool);
}

#[test]
fn if_with_return_in_one_branch_used_as_value_has_no_diagnostics() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("choose", vec![("flag", TypeExpression::Bool)], type_(TypeExpression::String), vec![
            var_decl("x", no_type(), Some(if_else_expr(
                mem_name("flag"),
                vec![return_stmt(Some(string("early")))],
                vec![expr_stmt(string("late"))],
            ))),
            return_stmt(Some(mem_name("x"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::String);
}

#[test]
fn if_where_both_branches_diverge_is_compatible_with_any_expected_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("choose", vec![("flag", TypeExpression::Bool)], type_(TypeExpression::String), vec![
            var_decl("x", type_(TypeExpression::Int), Some(if_else_expr(
                mem_name("flag"),
                vec![return_stmt(Some(string("a")))],
                vec![return_stmt(Some(string("b")))],
            ))),
            return_stmt(Some(string("done"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_top_level_struct_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        struct_def("Point", vec![("y", TypeExpression::Int)]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_top_level_function_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("helper", vec![], no_type(), vec![]),
        fn_def("helper", vec![], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn first_top_level_declaration_stays_resolvable_after_a_duplicate() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("Point", vec![("x", TypeExpression::Int)]),
        pub_struct_def("Point", vec![("y", TypeExpression::String)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0013"));
    assert!(!has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_function_parameter_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("add", vec![("x", TypeExpression::Int), ("x", TypeExpression::Int)], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_function_definition_reports_t0014() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            fn_def("inner", vec![], no_type(), vec![]),
        ]),
    ]));

    assert!(has_error_code(&context, "T0014"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_struct_definition_reports_t0014() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            struct_def("Inner", vec![("x", TypeExpression::Int)]),
        ]),
    ]));

    assert!(has_error_code(&context, "T0014"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn doubly_nested_function_definition_still_reports_t0014() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("outer", vec![], no_type(), vec![
            fn_def("middle", vec![], no_type(), vec![
                fn_def("inner", vec![], no_type(), vec![]),
            ]),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0014").count();
    assert_eq!(count, 2, "expected both nested definitions to be flagged: {:?}", context.diagnostics);
}

#[test]
fn an_int_literal_too_large_for_its_sized_type_reports_t0018() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("small", type_(TypeExpression::Uint8), Some(int(300))),
            var_decl("signed", type_(TypeExpression::Int8), Some(int(200))),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0018").count();
    assert_eq!(count, 2, "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "range errors must not cascade: {:?}", context.diagnostics);
}

#[test]
fn an_int_literal_out_of_range_for_a_sixteen_bit_type_reports_t0018() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("signed_max", type_(TypeExpression::Int16), Some(int(i16::MAX as u64))),
            var_decl("signed_min", type_(TypeExpression::Int16), Some(unary(UnaryOperator::Neg, int(32768)))),
            var_decl("unsigned_max", type_(TypeExpression::Uint16), Some(int(u16::MAX as u64))),
            var_decl("signed_over", type_(TypeExpression::Int16), Some(int(i16::MAX as u64 + 1))),
            var_decl("signed_under", type_(TypeExpression::Int16), Some(unary(UnaryOperator::Neg, int(32769)))),
            var_decl("unsigned_over", type_(TypeExpression::Uint16), Some(int(u16::MAX as u64 + 1))),
            var_decl("unsigned_negative", type_(TypeExpression::Uint16), Some(unary(UnaryOperator::Neg, int(1)))),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0018").count();
    assert_eq!(count, 4, "only the out of range literals may be flagged: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "range errors must not cascade: {:?}", context.diagnostics);
}

#[test]
fn an_int_literal_at_the_edge_of_its_sized_type_is_accepted() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("byte_max", type_(TypeExpression::Uint8), Some(int(255))),
            var_decl("wide_max", type_(TypeExpression::Uint64), Some(int(u64::MAX))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn arithmetic_between_a_sized_variable_and_a_literal_stays_in_the_sized_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Int64), Some(int(1))),
            var_decl("sum", no_type(), Some(binary(mem_name("wide"), BinaryOperator::Add, int(1)))),
            var_decl("flipped", no_type(), Some(binary(int(1), BinaryOperator::Add, mem_name("wide")))),
            var_decl("compared", no_type(), Some(binary(mem_name("wide"), BinaryOperator::Greater, int(0)))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let type_of = |name: &str| {
        let symbol = context.symbols.iter().find(|s| s.name == name).expect("symbol");
        context.types[symbol.type_.expect("should have a type").0 as usize].clone()
    };

    assert_eq!(type_of("sum"), Type::Int64);
    assert_eq!(type_of("flipped"), Type::Int64);
    assert_eq!(type_of("compared"), Type::Bool);
}

#[test]
fn an_int_literal_can_initialize_a_float() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Float), Some(int(5))),
            var_decl("narrow", type_(TypeExpression::Float32), Some(int(5))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn using_a_go_package_as_a_value_reports_t0017_instead_of_a_silent_type_error() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("fmt", None),
        main_fn(vec![var_decl("x", no_type(), Some(mem_name("fmt")))]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_go_package_reports_t0017_without_cascading() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("fmt", None),
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("fmt"))))]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn an_int_literal_too_large_for_plain_int_reports_t0018() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("annotated", type_(TypeExpression::Int), Some(int(u64::MAX))),
            var_decl("inferred", no_type(), Some(int(u64::MAX))),
            expr_stmt(int(u64::MAX)),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0018").count();
    assert_eq!(count, 3, "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_large_literal_represented_as_a_wider_type_is_not_also_reported_against_int() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        fn_def("takes_uint64", vec![("v", TypeExpression::Uint64)], no_type(), vec![]),
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Uint64), Some(int(u64::MAX))),
            expr_stmt(fn_call("takes_uint64", vec![int(u64::MAX)])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_float_literal_that_overflows_float32_reports_t0018() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("narrow", type_(TypeExpression::Float32), Some(float(1e300))),
            var_decl("wide", type_(TypeExpression::Float), Some(float(1e300))),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0018").count();
    assert_eq!(count, 1, "only the float32 declaration overflows: {:?}", context.diagnostics);
}

#[test]
fn a_declaration_without_type_or_initializer_reports_t0019() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("u", no_type(), None)]),
    ]));

    assert!(has_error_code(&context, "T0019"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn every_type_error_producing_path_reports_a_diagnostic() {
    // translation assumes a `Type::TypeError` never reaches it silently, because the driver
    // stops at the first error diagnostic. these are the paths that mint one.
    let cases: Vec<(&str, AstRoot<'static>)> = vec![
        ("declaration without type or initializer", program(vec![
            main_fn(vec![var_decl("u", no_type(), None)]),
        ])),
        ("unknown name", program(vec![
            main_fn(vec![var_decl("x", no_type(), Some(mem_name("nope")))]),
        ])),
        ("package used as a value", program(vec![
            use_stmt("fmt", None),
            main_fn(vec![var_decl("x", no_type(), Some(mem_name("fmt")))]),
        ])),
        ("struct init of an unresolved type", program(vec![
            main_fn(vec![var_decl("x", no_type(), Some(struct_init("Nope", vec![])))]),
        ])),
        ("annotation naming an unresolved type", program(vec![
            main_fn(vec![var_decl("x", type_(named_type("Nope")), None)]),
        ])),
    ];

    for (label, ast) in cases {
        let arena = Bump::new();
        let context = analyze(&arena, ast);
        assert!(
            !context.diagnostics.is_empty(),
            "`{label}` produced a type error without reporting anything",
        );
    }
}

#[test]
fn a_negative_literal_can_be_represented_as_the_expected_sized_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Int64), Some(unary(UnaryOperator::Neg, int(5)))),
            var_decl("edge", type_(TypeExpression::Int8), Some(unary(UnaryOperator::Neg, int(128)))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let wide = context.symbols.iter().find(|s| s.name == "wide").expect("wide symbol");
    assert_eq!(context.types[wide.type_.expect("wide should have a type").0 as usize], Type::Int64);
}

#[test]
fn a_negative_literal_out_of_range_reports_t0018() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("too_small", type_(TypeExpression::Int8), Some(unary(UnaryOperator::Neg, int(200)))),
            var_decl("unsigned", type_(TypeExpression::Uint8), Some(unary(UnaryOperator::Neg, int(1)))),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0018").count();
    assert_eq!(count, 2, "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn if_else_used_as_a_value_is_represented_as_the_expected_sized_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("wide", type_(TypeExpression::Int64), Some(if_else_expr(
                mem_name("flag"),
                vec![expr_stmt(int(1))],
                vec![expr_stmt(int(2))],
            ))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let wide = context.symbols.iter().find(|s| s.name == "wide").expect("wide symbol");
    assert_eq!(context.types[wide.type_.expect("wide should have a type").0 as usize], Type::Int64);
}

#[test]
fn array_elements_are_represented_as_the_element_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Int64), Some(int(1))),
            var_decl("from_first", no_type(), Some(array(vec![mem_name("wide"), int(5)]))),
            var_decl("from_annotation", type_(TypeExpression::Array { inner: Box::new(TypeExpression::Int64) }), Some(array(vec![int(1), int(2)]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let int64_type = context.types.iter().position(|t| *t == Type::Int64).expect("int64 type");
    let from_first = context.symbols.iter().find(|s| s.name == "from_first").expect("from_first symbol");
    assert_eq!(
        context.types[from_first.type_.expect("from_first should have a type").0 as usize],
        Type::Array(TypeId(int64_type as u32)),
    );
}

#[test]
fn a_wide_literal_on_the_left_of_a_binary_is_represented_as_the_right_hand_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Uint64), Some(int(1))),
            var_decl("sum", no_type(), Some(binary(int(u64::MAX), BinaryOperator::Add, mem_name("wide")))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let sum = context.symbols.iter().find(|s| s.name == "sum").expect("sum symbol");
    assert_eq!(context.types[sum.type_.expect("sum should have a type").0 as usize], Type::Uint64);
}

#[test]
fn calling_a_diverging_expression_does_not_report_t0002() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(call(
                if_else_expr(mem_name("flag"), vec![return_stmt(None)], vec![return_stmt(None)]),
                vec![],
            )),
        ]),
    ]));

    assert!(!has_error_code(&context, "T0002"), "diagnostics: {:?}", context.diagnostics);
}

fn point_with_methods<'src>(methods: Vec<Method<'src>>) -> Statement<'src> {
    struct_def_with_impl(
        "Point",
        vec![priv_field("x", TypeExpression::Int), pub_field("y", TypeExpression::Int)],
        methods,
    )
}

fn method_call<'src>(target: &'src str, method: &'src str, args: Vec<Expression<'src>>) -> Expression<'src> {
    call(field_access(name_target(target), method), args)
}

#[test]
fn a_instance_method_call_resolves_to_its_return_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("x", no_type(), Some(method_call("p", "get_x", vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn self_is_a_pointer_to_struct() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("me", vec![], no_type(), vec![
                var_decl("this", no_type(), Some(mem_name("self"))),
            ]),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let this = context.symbols.iter().find(|s| s.name == "this").expect("this symbol");
    let Type::Pointer(pointee) = context.types[this.type_.expect("this should have a type").0 as usize] else {
        panic!("expected `self` to be a pointer, found {:?}", context.types[this.type_.unwrap().0 as usize]);
    };

    let Type::Struct(struct_symbol, _) = context.types[pointee.0 as usize] else {
        panic!("expected `self` to point at a struct, found {:?}", context.types[pointee.0 as usize]);
    };

    assert_eq!(context.symbols[struct_symbol.0 as usize].name, "Point");
}

#[test]
fn an_instance_method_that_is_not_called_is_a_function_value() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("getter", no_type(), Some(field_access(name_target("p"), "get_x"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let getter = context.symbols.iter().find(|s| s.name == "getter").expect("getter symbol");
    let int_type = context.types.iter().position(|t| *t == Type::Int).expect("int type");
    assert_eq!(
        context.types[getter.type_.expect("getter should have a type").0 as usize],
        Type::Fn { params: vec![], return_type: TypeId(int_type as u32) },
    );
}

#[test]
fn an_instance_method_call_with_a_wrong_argument_type_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("shift", vec![("by", TypeExpression::Int)], no_type(), vec![]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            expr_stmt(method_call("p", "shift", vec![string("not an int")])),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_private_field_read_from_outside_the_impl_block_reports_t0021() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("x", no_type(), Some(field_access(name_target("p"), "x"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0021"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_private_field_initialized_from_outside_the_impl_block_reports_t0021() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1)), ("y", int(2))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0021"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_public_field_is_reachable_from_outside_the_impl_block() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("y", no_type(), Some(field_access(name_target("p"), "y"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn private_fields_and_methods_are_reachable_from_another_method_of_the_same_struct() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            priv_method("raw_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(method_call("self", "raw_x", vec![]))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("x", no_type(), Some(method_call("p", "get_x", vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let x = context.symbols.iter().find(|s| s.name == "x").expect("x symbol");
    assert_eq!(context.types[x.type_.expect("x should have a type").0 as usize], Type::Int);
}

#[test]
fn a_private_method_called_from_outside_the_impl_block_reports_t0021() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            priv_method("raw_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("x", no_type(), Some(method_call("p", "raw_x", vec![]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0021"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_static_method_called_on_the_struct_name_resolves_to_its_return_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_static_method("origin", vec![], type_(named_type("Point")), vec![
                return_stmt(Some(struct_init("Point", vec![("x", int(0)), ("y", int(0))]))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", no_type(), Some(method_call("Point", "origin", vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let p = context.symbols.iter().find(|s| s.name == "p").expect("p symbol");
    let Type::Struct(struct_symbol, _) = context.types[p.type_.expect("p should have a type").0 as usize] else {
        panic!("expected a struct, found {:?}", context.types[p.type_.unwrap().0 as usize]);
    };

    assert_eq!(context.symbols[struct_symbol.0 as usize].name, "Point");
}

#[test]
fn an_instance_method_called_on_the_struct_name_reports_t0022() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
        ]),
        main_fn(vec![
            var_decl("x", no_type(), Some(method_call("Point", "get_x", vec![]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0022"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_static_method_called_on_an_instance_reports_t0022() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_static_method("origin", vec![], type_(named_type("Point")), vec![
                return_stmt(Some(struct_init("Point", vec![("x", int(0)), ("y", int(0))]))),
            ]),
        ]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            var_decl("other", no_type(), Some(method_call("p", "origin", vec![]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0022"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_static_method_has_no_self_binding() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_static_method("broken", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
        ]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn assigning_to_a_method_reports_t0023() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(field_access(name_target("self"), "x"))),
            ]),
            pub_static_method("origin", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(int(0))),
            ]),
        ]),
        fn_def("zero", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            assign(field_target(name_target("p"), "get_x"), mem_name("zero")),
            assign(field_target(name_target("Point"), "origin"), mem_name("zero")),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0023").count();
    assert_eq!(count, 2, "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_field_reached_through_the_struct_name_reports_t0022() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("y", no_type(), Some(field_access(name_target("Point"), "y"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0022"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn an_unknown_member_on_a_struct_name_reports_t0017() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("nothing", no_type(), Some(field_access(name_target("Point"), "nope"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_private_field_written_from_outside_the_impl_block_reports_t0021() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![]),
        main_fn(vec![
            var_decl("p", type_(named_type("Point")), None),
            assign(field_target(name_target("p"), "x"), int(1)),
        ]),
    ]));

    assert!(has_error_code(&context, "T0021"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_private_static_method_called_from_outside_the_impl_block_reports_t0021() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            method(Visibility::Private, MethodKind::Static, "secret", vec![], type_(TypeExpression::Int), vec![
                return_stmt(Some(int(0))),
            ]),
        ]),
        main_fn(vec![
            var_decl("x", no_type(), Some(method_call("Point", "secret", vec![]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0021"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_static_method_that_collides_with_a_top_level_name_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_static_method("new", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
        ]),
        fn_def("Point_new", vec![], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn two_static_methods_that_mangle_to_the_same_top_level_name_report_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        struct_def_with_impl("A", vec![priv_field("v", TypeExpression::Int)], vec![
            pub_static_method("b_c", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
        ]),
        struct_def_with_impl("A_b", vec![priv_field("v", TypeExpression::Int)], vec![
            pub_static_method("c", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
        ]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn tuple_literal_infers_each_position() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("t", no_type(), Some(tuple(vec![int(1), string("x"), bool_lit(true)]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(tuple_element_types(&context, "t"), vec![Type::Int, Type::String, Type::Bool]);
}

#[test]
fn matching_tuples_share_a_type_id() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("annotated", type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])), None),
            var_decl("annotated_again", type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])), None),
            var_decl("inferred", no_type(), Some(tuple(vec![int(1), string("x")]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let type_id_of = |name: &str| {
        context.symbols.iter().find(|s| s.name == name).expect("symbol").type_.expect("should have a type")
    };

    assert_eq!(type_id_of("annotated"), type_id_of("annotated_again"));
    assert_eq!(type_id_of("annotated"), type_id_of("inferred"));
}

#[test]
fn tuple_works_everywhere_a_type_can_go() {
    let int_and_string = || tuple_type(vec![TypeExpression::Int, TypeExpression::String]);

    let arena = Bump::new();

    let context = analyze(&arena, program(vec![
        pub_struct_def("Holder", vec![("pair", int_and_string())]),
        fn_def(
            "swap",
            vec![("p", int_and_string())],
            type_(tuple_type(vec![TypeExpression::String, TypeExpression::Int])),
            vec![return_stmt(Some(tuple(vec![
                tuple_index(name_target("p"), 1),
                tuple_index(name_target("p"), 0),
            ])))],
        ),
        main_fn(vec![
            var_decl("pair", type_(int_and_string()), Some(tuple(vec![int(1), string("x")]))),
            var_decl("swapped", no_type(), Some(fn_call("swap", vec![mem_name("pair")]))),
            var_decl(
                "pairs",
                type_(TypeExpression::Array { inner: Box::new(int_and_string()) }),
                Some(array(vec![tuple(vec![int(2), string("y")])])),
            ),
            var_decl("holder", no_type(), Some(struct_init("Holder", vec![("pair", tuple(vec![int(3), string("z")]))]))),
            var_decl("held", no_type(), Some(field_access(name_target("holder"), "pair"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(tuple_element_types(&context, "swapped"), vec![Type::String, Type::Int]);
    assert_eq!(tuple_element_types(&context, "held"), vec![Type::Int, Type::String]);
    assert_eq!(
        type_of(&context, "pairs"),
        Type::Array(context.symbols.iter().find(|s| s.name == "pair").expect("pair symbol").type_.expect("type")),
    );
}

#[test]
fn tuple_index_reads_and_writes_its_position() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("pair", type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])), Some(tuple(vec![int(1), string("x")]))),
            var_decl("first", no_type(), Some(tuple_index(name_target("pair"), 0))),
            var_decl("second", no_type(), Some(tuple_index(name_target("pair"), 1))),
            assign(tuple_index_target(name_target("pair"), 0), int(7)),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "first"), Type::Int);
    assert_eq!(type_of(&context, "second"), Type::String);
}

#[test]
fn wrong_type_in_a_tuple_position_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("pair", type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])), Some(tuple(vec![int(1), string("x")]))),
            assign(tuple_index_target(name_target("pair"), 0), string("not an int")),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn tuple_index_out_of_range_reports_t0024() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("pair", no_type(), Some(tuple(vec![int(1), string("x")]))),
            var_decl("third", no_type(), Some(tuple_index(name_target("pair"), 2))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0024"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_a_non_tuple_reports_t0006() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::String), Some(string("s"))),
            var_decl("first", no_type(), Some(tuple_index(name_target("x"), 0))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_a_diverging_expression_reports_nothing() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            var_decl("first", no_type(), Some(tuple_index(
                deref_target(if_else_expr(mem_name("flag"), vec![return_stmt(None)], vec![return_stmt(None)])),
                0,
            ))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_an_unknown_name_reports_s0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![var_decl("first", no_type(), Some(tuple_index(name_target("unknown"), 0)))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0024"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn wrong_tuple_arity_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("pair", type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])), Some(tuple(vec![int(1)]))),
        ]),
    ]));

    let mismatch = context.diagnostics
        .iter()
        .find(|diagnostic| &*diagnostic.error_code == "T0001")
        .unwrap_or_else(|| panic!("expected a T0001, diagnostics: {:?}", context.diagnostics));

    assert!(mismatch.message.contains("(int, string)"), "message: {}", mismatch.message);
    assert!(mismatch.message.contains("(int)"), "message: {}", mismatch.message);
}

#[test]
fn expected_tuple_type_reaches_every_position() {
    let small_and_string = || tuple_type(vec![TypeExpression::Int8, TypeExpression::String]);

    let arena = Bump::new();

    let context = analyze(&arena, program(vec![
        fn_def("takes", vec![("p", small_and_string())], type_(TypeExpression::String), vec![
            return_stmt(Some(tuple_index(name_target("p"), 1))),
        ]),
        fn_def("makes", vec![], type_(small_and_string()), vec![
            return_stmt(Some(tuple(vec![int(3), string("m")]))),
        ]),
        main_fn(vec![
            var_decl("annotated", type_(small_and_string()), Some(tuple(vec![int(1), string("a")]))),
            var_decl("argument", no_type(), Some(fn_call("takes", vec![tuple(vec![int(2), string("b")])]))),
            var_decl(
                "in_array",
                type_(TypeExpression::Array { inner: Box::new(small_and_string()) }),
                Some(array(vec![tuple(vec![int(4), string("c")])])),
            ),
            var_decl("returned", no_type(), Some(fn_call("makes", vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(tuple_element_types(&context, "annotated"), vec![Type::Int8, Type::String]);
    assert_eq!(tuple_element_types(&context, "returned"), vec![Type::Int8, Type::String]);
    assert_eq!(type_of(&context, "argument"), Type::String);
}

#[test]
fn extra_tuple_values_are_still_typechecked() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("s", type_(TypeExpression::String), Some(string("s"))),
            var_decl(
                "pair",
                type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])),
                Some(tuple(vec![int(1), string("a"), tuple_index(name_target("s"), 0)])),
            ),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn indexing_through_a_pointer_reports_t0006() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        main_fn(vec![
            var_decl("pair", no_type(), Some(tuple(vec![int(1), string("a")]))),
            var_decl("p", no_type(), Some(reference(mem_name("pair")))),
            var_decl("first", no_type(), Some(tuple_index(name_target("p"), 0))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0006"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn go_multi_result_maps_to_a_tuple() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        use_stmt("math", None),
        main_fn(vec![
            var_decl("parts", no_type(), Some(field_call("math", "Frexp", vec![float(8.0)]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(tuple_element_types(&context, "parts"), vec![Type::Float, Type::Int]);
}

#[test]
fn a_method_that_reuses_a_field_or_method_name_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        point_with_methods(vec![
            pub_method("x", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
            pub_method("get_x", vec![], type_(TypeExpression::Int), vec![return_stmt(Some(int(0)))]),
        ]),
    ]));

    let count = context.diagnostics.iter().filter(|d| &*d.error_code == "T0013").count();
    assert_eq!(count, 2, "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn each_generic_instantiation_gets_its_own_type_id() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("strings", type_(generic_type("Box", vec![TypeExpression::String])), None),
            var_decl("more_ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    assert_ne!(type_id_of(&context, "ints"), type_id_of(&context, "strings"));
    assert_eq!(type_id_of(&context, "ints"), type_id_of(&context, "more_ints"));
}

#[test]
fn generic_function_infers_its_type_parameter() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        main_fn(vec![
            var_decl("text", no_type(), Some(fn_call("identity", vec![string("hi")]))),
            var_decl("number", no_type(), Some(fn_call("identity", vec![int(1)]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "text"), Type::String);
    assert_eq!(type_of(&context, "number"), Type::Int);
}

#[test]
fn an_uninferable_type_parameter_reports_t0025() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_empty_fn(),
        main_fn(vec![var_decl("value", no_type(), Some(fn_call("empty", vec![])))]),
    ]));

    assert!(has_error_code(&context, "T0025"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn explicit_type_argument_covers_what_inference_cannot() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_empty_fn(),
        main_fn(vec![
            var_decl("value", no_type(), Some(generic_fn_call("empty", vec![TypeExpression::Int], vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "value"), Type::Int);
}

#[test]
fn arguments_are_checked_against_the_explicit_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        main_fn(vec![
            var_decl("value", no_type(), Some(generic_fn_call("identity", vec![TypeExpression::String], vec![int(1)]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn too_many_type_arguments_reports_t0026() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        main_fn(vec![
            expr_stmt(generic_fn_call("identity", vec![TypeExpression::Int, TypeExpression::Bool], vec![int(1)])),
        ]),
    ]));

    assert!(has_error_code(&context, "T0026"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn missing_type_arguments_reports_t0026() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![var_decl("boxed", type_(named_type("Box")), None)]),
    ]));

    assert!(has_error_code(&context, "T0026"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn disagreeing_arguments_report_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "both",
            vec!["T"],
            vec![("first", named_type("T")), ("second", named_type("T"))],
            no_type(),
            vec![],
        ),
        main_fn(vec![expr_stmt(fn_call("both", vec![int(1), string("x")]))]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_type_parameter_bound_twice_reports_t0027() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "both",
            vec!["T"],
            vec![("pair", tuple_type(vec![named_type("T"), named_type("T")]))],
            no_type(),
            vec![],
        ),
        main_fn(vec![expr_stmt(fn_call("both", vec![tuple(vec![int(1), string("x")])]))]),
    ]));

    assert!(has_error_code(&context, "T0027"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "the conflict should be the only complaint: {:?}", context.diagnostics);
}

#[test]
fn generic_function_as_a_value_reports_t0028() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        main_fn(vec![var_decl("f", no_type(), Some(mem_name("identity")))]),
    ]));

    assert!(has_error_code(&context, "T0028"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0031"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_shadowing_type_parameter_reports_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        pub_struct_def("T", vec![("value", TypeExpression::Int)]),
        identity_fn(),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_type_parameter_names_report_t0013() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def("both", vec!["T", "T"], vec![("value", named_type("T"))], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn operator_on_a_type_parameter_reports_t0029() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "add",
            vec!["T"],
            vec![("first", named_type("T")), ("second", named_type("T"))],
            type_(named_type("T")),
            vec![return_stmt(Some(binary(mem_name("first"), BinaryOperator::Add, mem_name("second"))))],
        ),
    ]));

    assert!(has_error_code(&context, "T0029"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_type_parameter_is_not_a_value() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def("f", vec!["T"], vec![], no_type(), vec![
            var_decl("value", no_type(), Some(mem_name("T"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn generic_struct_field_has_its_declared_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("strings", type_(generic_type("Box", vec![TypeExpression::String])), None),
            var_decl("number", no_type(), Some(field_access(name_target("ints"), "value"))),
            var_decl("text", no_type(), Some(field_access(name_target("strings"), "value"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "number"), Type::Int);
    assert_eq!(type_of(&context, "text"), Type::String);
}

#[test]
fn wrong_type_in_a_generic_field_reports_t0001() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            assign(field_target(name_target("ints"), "value"), string("x")),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn struct_init_infers_type_arguments_from_fields() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![
            var_decl("inferred", no_type(), Some(struct_init("Box", vec![("value", string("x"))]))),
            var_decl("declared", type_(generic_type("Box", vec![TypeExpression::String])), None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_id_of(&context, "inferred"), type_id_of(&context, "declared"));
}

#[test]
fn nested_generic_struct_substitutes_recursively() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        generic_fn_def(
            "unwrap",
            vec!["T"],
            vec![("boxed", generic_type("Box", vec![named_type("T")]))],
            type_(named_type("T")),
            vec![return_stmt(Some(field_access(name_target("boxed"), "value")))],
        ),
        main_fn(vec![
            var_decl("inner", no_type(), Some(struct_init("Box", vec![("value", int(1))]))),
            var_decl("outer", no_type(), Some(struct_init("Box", vec![("value", mem_name("inner"))]))),
            var_decl("unwrapped", no_type(), Some(fn_call("unwrap", vec![mem_name("outer")]))),
            var_decl("number", no_type(), Some(field_access(name_target("unwrapped"), "value"))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_id_of(&context, "unwrapped"), type_id_of(&context, "inner"));
    assert_eq!(type_of(&context, "number"), Type::Int);
}

#[test]
fn a_type_argument_inside_an_array_is_inferred() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        generic_fn_def(
            "first",
            vec!["T"],
            vec![("values", TypeExpression::Array { inner: Box::new(named_type("T")) })],
            type_(named_type("T")),
            vec![return_stmt(Some(array_index("values", int(0))))],
        ),
        main_fn(vec![
            var_decl("text", no_type(), Some(fn_call("first", vec![array(vec![string("a")])]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "text"), Type::String);
}

#[test]
fn a_wrong_shaped_argument_reports_a_mismatch() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "first",
            vec!["T"],
            vec![("values", TypeExpression::Array { inner: Box::new(named_type("T")) })],
            type_(named_type("T")),
            vec![return_stmt(Some(array_index("values", int(0))))],
        ),
        main_fn(vec![
            var_decl("text", no_type(), Some(fn_call("first", vec![int(1)]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0025"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_broken_nested_argument_does_not_cascade() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "first",
            vec!["T"],
            vec![("values", TypeExpression::Array { inner: Box::new(named_type("T")) })],
            type_(named_type("T")),
            vec![return_stmt(Some(array_index("values", int(0))))],
        ),
        main_fn(vec![expr_stmt(fn_call("first", vec![fn_call("nope", vec![])]))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0025"), "the unknown name should be the only complaint: {:?}", context.diagnostics);
}

#[test]
fn wrong_argument_count_does_not_leak_a_type_parameter() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        identity_fn(),
        main_fn(vec![
            var_decl("value", no_type(), Some(fn_call("identity", vec![]))),
            expr_stmt(binary(mem_name("value"), BinaryOperator::Add, int(1))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0003"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0029"), "the type parameter must not escape the callee: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "the type parameter must not escape the callee: {:?}", context.diagnostics);
}

#[test]
fn generic_method_returns_its_instantiated_type() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
            pub_method("get", vec![], type_(named_type("T")), vec![
                return_stmt(Some(field_access(name_target("self"), "value"))),
            ]),
        ]),
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("strings", type_(generic_type("Box", vec![TypeExpression::String])), None),
            var_decl("number", no_type(), Some(call(field_access(name_target("ints"), "get"), vec![]))),
            var_decl("text", no_type(), Some(call(field_access(name_target("strings"), "get"), vec![]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "number"), Type::Int);
    assert_eq!(type_of(&context, "text"), Type::String);
}

#[test]
fn generic_method_binds_its_parameter_from_arguments() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
            generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "with",
                vec!["U"],
                vec![("other", named_type("U"))],
                type_(generic_type("Box", vec![named_type("U")])),
                vec![return_stmt(Some(generic_struct_init(
                    "Box",
                    vec![named_type("U")],
                    vec![("value", mem_name("other"))],
                )))],
            ),
        ]),
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("strings", no_type(), Some(call(field_access(name_target("ints"), "with"), vec![string("x")]))),
            var_decl("declared", type_(generic_type("Box", vec![TypeExpression::String])), None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_id_of(&context, "strings"), type_id_of(&context, "declared"));
}

#[test]
fn generic_method_as_a_value_reports_t0031() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
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
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("with", no_type(), Some(field_access(name_target("ints"), "with"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0031"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0028"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn generic_method_takes_an_explicit_type_argument() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
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
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            var_decl("with", no_type(), Some(generic_field_access(
                name_target("ints"),
                "with",
                vec![TypeExpression::String],
            ))),
            var_decl("text", no_type(), Some(call(mem_name("with"), vec![string("x")]))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_of(&context, "text"), Type::String);
}

#[test]
fn static_method_is_generic_over_the_struct() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
            method(
                Visibility::Public,
                MethodKind::Static,
                "of",
                vec![("value", named_type("T"))],
                type_(generic_type("Box", vec![named_type("T")])),
                vec![return_stmt(Some(struct_init("Box", vec![("value", mem_name("value"))])))],
            ),
        ]),
        main_fn(vec![
            var_decl("strings", no_type(), Some(call(field_access(name_target("Box"), "of"), vec![string("x")]))),
            var_decl("declared", type_(generic_type("Box", vec![TypeExpression::String])), None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert_eq!(type_id_of(&context, "strings"), type_id_of(&context, "declared"));
}

#[test]
fn operator_on_a_generic_struct_reports_t0029() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        generic_fn_def(
            "same",
            vec!["T"],
            vec![
                ("first", generic_type("Box", vec![named_type("T")])),
                ("second", generic_type("Box", vec![named_type("T")])),
            ],
            type_(TypeExpression::Bool),
            vec![return_stmt(Some(binary(mem_name("first"), BinaryOperator::Eq, mem_name("second"))))],
        ),
    ]));

    assert!(has_error_code(&context, "T0029"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn operator_on_a_generic_tuple_reports_t0029() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "same",
            vec!["T"],
            vec![
                ("first", tuple_type(vec![TypeExpression::Int, named_type("T")])),
                ("second", tuple_type(vec![TypeExpression::Int, named_type("T")])),
            ],
            type_(TypeExpression::Bool),
            vec![return_stmt(Some(binary(mem_name("first"), BinaryOperator::Eq, mem_name("second"))))],
        ),
    ]));

    assert!(has_error_code(&context, "T0029"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn operator_on_a_generic_array_reports_t0029() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "sum",
            vec!["T"],
            vec![
                ("first", TypeExpression::Array { inner: Box::new(named_type("T")) }),
                ("second", TypeExpression::Array { inner: Box::new(named_type("T")) }),
            ],
            type_(TypeExpression::Array { inner: Box::new(named_type("T")) }),
            vec![return_stmt(Some(binary(mem_name("first"), BinaryOperator::Add, mem_name("second"))))],
        ),
    ]));

    assert!(has_error_code(&context, "T0029"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn operator_on_a_generic_pointer_is_allowed() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_fn_def(
            "same",
            vec!["T"],
            vec![("first", pointer_type(named_type("T"))), ("second", pointer_type(named_type("T")))],
            type_(TypeExpression::Bool),
            vec![return_stmt(Some(binary(mem_name("first"), BinaryOperator::Eq, mem_name("second"))))],
        ),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn a_poisoned_type_argument_does_not_cascade() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        box_of_t(),
        main_fn(vec![
            var_decl(
                "boxed",
                type_(generic_type("Box", vec![TypeExpression::Int])),
                Some(struct_init("Box", vec![("value", fn_call("nope", vec![]))])),
            ),
        ]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0001"), "the unknown name should be the only complaint: {:?}", context.diagnostics);
}

#[test]
fn type_arguments_given_twice_report_t0030() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
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
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            expr_stmt(generic_call(
                generic_field_access(name_target("ints"), "with", vec![TypeExpression::String]),
                vec![TypeExpression::String],
                vec![string("x")],
            )),
        ]),
    ]));

    assert!(has_error_code(&context, "T0030"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn the_mangled_name_is_reused_not_rebuilt() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
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
        main_fn(vec![
            var_decl("ints", type_(generic_type("Box", vec![TypeExpression::Int])), None),
            expr_stmt(call(field_access(name_target("ints"), "with"), vec![string("a")])),
            expr_stmt(call(field_access(name_target("ints"), "with"), vec![string("b")])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);

    let names = context.modules[0].free_function_members.values().map(|member| member.name).collect::<Vec<_>>();
    assert_eq!(names.len(), 2, "both accesses should be recorded: {:?}", names);
    assert!(names.iter().all(|name| *name == "Box_with"));
    assert!(
        std::ptr::eq(names[0].as_ptr(), names[1].as_ptr()),
        "every access mangling its own copy of the name leaks one per access",
    );
}


#[test]
fn method_signatures_share_their_type_parameters() {
    let arena = Bump::new();
    let context = analyze(&arena, program(vec![
        generic_struct_def("Box", vec!["T"], vec![pub_field("value", named_type("T"))], vec![
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
    ]));

    let signature = context.struct_methods
        .values()
        .find_map(|methods| methods.get("with"))
        .expect("the method should have been collected");

    let taken_at_an_access = signature.clone();

    assert!(
        std::ptr::eq(signature.type_params.as_ptr(), taken_at_an_access.type_params.as_ptr()),
        "taking a signature at a member access must not copy its type parameters",
    );
}
