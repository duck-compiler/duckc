use super::{analyze_module, context::SemanticsContext, go_map::map_go_type, go_resolve::RawType, r#type::{Type, TypeId}, symbol::SymbolKind};
use crate::ast::{AstRoot, Statement, TypeExpression, expression::{BinaryOperator, UnaryOperator}, builder::{array, array_index, assign, binary, bool_lit, break_stmt, call, continue_stmt, dereference, expr_stmt, field_access, field_call, field_target, float, fn_call, fn_def, ident, if_else_expr, if_expr, int, mem_name, name_target, no_type, pointer_type, program, reference, unary, return_stmt, string, struct_def, struct_init, type_, use_stmt, var_decl, while_expr}};

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

#[test]
fn reference_then_dereference_round_trips_to_the_original_type() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("x", type_(TypeExpression::Int), Some(int(1))),
            var_decl("y", no_type(), Some(dereference(mem_name("x")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0015"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn dereferencing_a_diverging_expression_does_not_report_t0015() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![var_decl("p", no_type(), Some(reference(int(1))))]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_composite_literal_is_allowed() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
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

    assert!(matches!(pointee_of("p"), Type::Struct(_)));
    assert_eq!(pointee_of("a"), Type::Array(TypeId(context.types.iter().position(|t| *t == Type::Int).expect("int type") as u32)));
}

#[test]
fn taking_the_address_of_a_duck_function_reports_t0016() {
    let context = analyze(program(vec![
        fn_def("helper", vec![], no_type(), vec![]),
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("helper"))))]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_go_package_function_reports_t0016() {
    let context = analyze(program(vec![
        use_stmt("fmt", None),
        main_fn(vec![
            var_decl("p", no_type(), Some(reference(field_access(name_target("fmt"), "Println")))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_an_unknown_name_reports_only_s0001() {
    let context = analyze(program(vec![
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("unknown"))))]),
    ]));

    assert!(has_error_code(&context, "S0001"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn field_access_through_a_pointer_auto_dereferences() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
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
    let context = analyze(program(vec![
        struct_def("Node", vec![
            ("value", TypeExpression::Int),
            ("next", pointer_type(TypeExpression::Ident(ident("Node"))))
        ]),
        main_fn(vec![
            var_decl("tail", type_(TypeExpression::Ident(ident("Node"))), None),
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Int64), None),
            var_decl("narrow", type_(TypeExpression::Int), Some(mem_name("wide"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn pointers_to_differently_sized_integers_are_distinct_types() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    context: &mut SemanticsContext<'static>,
    package: &str,
    qualified_name: &str,
    field: &str,
) -> Type {
    let types = context.go_resolver.types_of(package).expect("package should resolve through the real toolchain");

    let mapped = map_go_type(context, package, &RawType::Named { r#ref: qualified_name.to_string() }, &types)
        .expect("named struct should map");

    let Type::Struct(struct_sym) = context.types[mapped.0 as usize] else {
        panic!("expected a struct, found {:?}", context.types[mapped.0 as usize]);
    };

    let field_type = context.struct_fields.get(&struct_sym)
        .expect("struct fields")
        .iter()
        .find(|(name, _)| *name == field)
        .map(|(_, type_id)| *type_id)
        .unwrap_or_else(|| panic!("no field `{field}` on `{qualified_name}`"));

    context.types[field_type.0 as usize].clone()
}

#[test]
fn go_sixteen_bit_fields_map_to_their_own_duck_types_through_the_real_toolchain() {
    let mut context = SemanticsContext::new();

    assert_eq!(go_struct_field_type(&mut context, "image/color", "image/color.Gray16", "Y"), Type::Uint16);
    assert_eq!(go_struct_field_type(&mut context, "database/sql", "database/sql.NullInt16", "Int16"), Type::Int16);
}

#[test]
fn go_time_duration_maps_to_int64_through_the_real_toolchain() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("flag", type_(TypeExpression::Bool), Some(bool_lit(true))),
            expr_stmt(if_expr(mem_name("flag"), vec![expr_stmt(int(1))])),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_if_without_an_else_branch_as_a_statement_may_produce_a_value() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        expr_stmt(int(1)),
    ]));

    assert!(has_error_code(&context, "T0011"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn return_with_matching_type_has_no_diagnostics() {
    let context = analyze(program(vec![
        fn_def("get_x", vec![], type_(TypeExpression::Int), vec![
            return_stmt(Some(int(5))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn return_with_mismatched_type_reports_t0001() {
    let context = analyze(program(vec![
        fn_def("get_x", vec![], type_(TypeExpression::Int), vec![
            return_stmt(Some(string("not an int"))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn bare_return_in_unit_function_has_no_diagnostics() {
    let context = analyze(program(vec![
        fn_def("do_nothing", vec![], no_type(), vec![
            return_stmt(None),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn break_outside_loop_reports_t0012() {
    let context = analyze(program(vec![
        main_fn(vec![break_stmt()]),
    ]));

    assert!(has_error_code(&context, "T0012"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn continue_outside_loop_reports_t0012() {
    let context = analyze(program(vec![
        main_fn(vec![continue_stmt()]),
    ]));

    assert!(has_error_code(&context, "T0012"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn break_and_continue_inside_while_have_no_diagnostics() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("x", no_type(), Some(binary(int(1), BinaryOperator::And, bool_lit(true)))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0001"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn logical_and_of_two_bools_resolves_to_bool() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        struct_def("Point", vec![("y", TypeExpression::Int)]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_top_level_function_reports_t0013() {
    let context = analyze(program(vec![
        fn_def("helper", vec![], no_type(), vec![]),
        fn_def("helper", vec![], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn first_top_level_declaration_stays_resolvable_after_a_duplicate() {
    let context = analyze(program(vec![
        struct_def("Point", vec![("x", TypeExpression::Int)]),
        struct_def("Point", vec![("y", TypeExpression::String)]),
        main_fn(vec![
            var_decl("p", no_type(), Some(struct_init("Point", vec![("x", int(1))]))),
        ]),
    ]));

    assert!(has_error_code(&context, "T0013"));
    assert!(!has_error_code(&context, "T0008"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn duplicate_function_parameter_reports_t0013() {
    let context = analyze(program(vec![
        fn_def("add", vec![("x", TypeExpression::Int), ("x", TypeExpression::Int)], no_type(), vec![]),
    ]));

    assert!(has_error_code(&context, "T0013"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_function_definition_reports_t0014() {
    let context = analyze(program(vec![
        main_fn(vec![
            fn_def("inner", vec![], no_type(), vec![]),
        ]),
    ]));

    assert!(has_error_code(&context, "T0014"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn nested_struct_definition_reports_t0014() {
    let context = analyze(program(vec![
        main_fn(vec![
            struct_def("Inner", vec![("x", TypeExpression::Int)]),
        ]),
    ]));

    assert!(has_error_code(&context, "T0014"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn doubly_nested_function_definition_still_reports_t0014() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("byte_max", type_(TypeExpression::Uint8), Some(int(255))),
            var_decl("wide_max", type_(TypeExpression::Uint64), Some(int(u64::MAX))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn arithmetic_between_a_sized_variable_and_a_literal_stays_in_the_sized_type() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
        main_fn(vec![
            var_decl("wide", type_(TypeExpression::Float), Some(int(5))),
            var_decl("narrow", type_(TypeExpression::Float32), Some(int(5))),
        ]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
}

#[test]
fn using_a_go_package_as_a_value_reports_t0017_instead_of_a_silent_type_error() {
    let context = analyze(program(vec![
        use_stmt("fmt", None),
        main_fn(vec![var_decl("x", no_type(), Some(mem_name("fmt")))]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn taking_the_address_of_a_go_package_reports_t0017_without_cascading() {
    let context = analyze(program(vec![
        use_stmt("fmt", None),
        main_fn(vec![var_decl("p", no_type(), Some(reference(mem_name("fmt"))))]),
    ]));

    assert!(has_error_code(&context, "T0017"), "diagnostics: {:?}", context.diagnostics);
    assert!(!has_error_code(&context, "T0016"), "diagnostics: {:?}", context.diagnostics);
}

#[test]
fn an_int_literal_too_large_for_plain_int_reports_t0018() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
            main_fn(vec![var_decl("x", type_(TypeExpression::Ident(ident("Nope"))), None)]),
        ])),
    ];

    for (label, ast) in cases {
        let context = analyze(ast);
        assert!(
            !context.diagnostics.is_empty(),
            "`{label}` produced a type error without reporting anything",
        );
    }
}

#[test]
fn a_negative_literal_can_be_represented_as_the_expected_sized_type() {
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
    let context = analyze(program(vec![
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
