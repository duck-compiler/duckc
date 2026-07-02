use super::{analyze_module, context::SemanticsContext, r#type::Type};
use crate::ast::{AstRoot, Statement, TypeExpression, builder::{expr_stmt, fn_call, fn_def, go_imm, no_type, program, string, type_, var_decl}};

fn analyze(program: AstRoot<'static>) -> SemanticsContext<'static> {
    let mut context = SemanticsContext::new();
    let module = context.add_module(program);
    analyze_module(&mut context, module);
    context
}

fn has_error_code(context: &SemanticsContext, code: &str) -> bool {
    context.diagnostics.iter().any(|diagnostic| &*diagnostic.error_code == code)
}

fn println_def<'src>() -> Statement<'src> {
    fn_def("println", vec![("msg", TypeExpression::String)], no_type(), vec![go_imm("fmt.Println(msg)")])
}

fn main_fn<'src>(body: Vec<Statement<'src>>) -> Statement<'src> {
    fn_def("main", vec![], no_type(), body)
}

#[test]
fn hello_world_program_has_no_diagnostics_and_resolves_types() {
    let context = analyze(program(vec![
        println_def(),
        main_fn(vec![expr_stmt(fn_call("println", vec![string("Hello, World!")]))]),
    ]));

    assert!(context.diagnostics.is_empty(), "unexpected diagnostics: {:?}", context.diagnostics);
    assert!(context.symbols.iter().all(|symbol| symbol.type_.is_some()));

    let msg = context.symbols.iter().find(|s| s.name == "msg").expect("msg symbol");
    assert_eq!(context.types[msg.type_.unwrap().0 as usize], Type::String);

    let println = context.symbols.iter().find(|s| s.name == "println").expect("println symbol");
    assert!(matches!(context.types[println.type_.unwrap().0 as usize], Type::Fn { .. }));
}

#[test]
fn wrong_arg_count_reports_t0003() {
    let context = analyze(program(vec![
        println_def(),
        main_fn(vec![expr_stmt(fn_call("println", vec![]))]),
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
