use super::resolver::{ResolveOutput, resolve};
use crate::parser2::parser::{DefId, DefKind, Parsed, SourceFile, parse};
use crate::parser2::tokenizer::tokenize_no_comments;

fn parse_src(src: &str) -> SourceFile<Parsed> {
    let (toks, lex_errs) = tokenize_no_comments(src, 1);
    assert!(lex_errs.is_empty(), "lex errors: {:?}", lex_errs);
    let (sf, parse_errs) = parse(toks, 0);
    assert!(parse_errs.is_empty(), "parse errors: {:?}", parse_errs);
    sf
}

fn resolve_ok(src: &str) -> ResolveOutput {
    let out = resolve(parse_src(src));
    assert!(
        out.errors.is_empty(),
        "unexpected resolve errors in {:?}: {:?}",
        src,
        out.errors.iter().map(|e| &e.msg).collect::<Vec<_>>()
    );
    out
}

fn resolve_err(src: &str) -> ResolveOutput {
    let out = resolve(parse_src(src));
    assert!(
        !out.errors.is_empty(),
        "expected resolve errors in {:?} but got none",
        src
    );
    out
}

fn find_sym<'a>(
    out: &'a ResolveOutput,
    name: &str,
) -> (DefId, &'a crate::parser2::parser::SymbolDef) {
    out.symbols
        .iter()
        .find(|(_, d)| d.name == name)
        .unwrap_or_else(|| panic!("no symbol named `{name}` in table"))
}

#[test]
fn empty_file_has_only_poison_entry() {
    let out = resolve_ok("");
    assert_eq!(out.symbols.len(), 1);
    assert!(out.global_scope.is_empty());
}

#[test]
fn function_registered_in_global_scope() {
    let out = resolve_ok("fn add(x: Int, y: Int) -> Int { x }");
    assert!(out.global_scope.contains_key("add"));
}

#[test]
fn type_alias_registered_in_global_scope() {
    let out = resolve_ok("type MyInt = Int;");
    assert!(out.global_scope.contains_key("MyInt"));
}

#[test]
fn struct_registered_in_global_scope() {
    let out = resolve_ok("struct Point { x: Int, y: Int }");
    assert!(out.global_scope.contains_key("Point"));
}

#[test]
fn multiple_top_level_items_all_in_global_scope() {
    let out = resolve_ok("fn foo() { } struct Bar { x: Int } type Baz = Int;");
    assert!(out.global_scope.contains_key("foo"));
    assert!(out.global_scope.contains_key("Bar"));
    assert!(out.global_scope.contains_key("Baz"));
}

#[test]
fn global_scope_depth_is_zero() {
    let out = resolve_ok("fn main() { }");
    let id = out.global_scope["main"];
    assert_eq!(out.symbols.get(id).scope_depth, 0);
}

#[test]
fn param_registered_with_param_kind() {
    let out = resolve_ok("fn f(x: Int) -> Int { x }");
    let (_, def) = find_sym(&out, "x");
    assert!(matches!(def.kind, DefKind::Param { .. }));
}

#[test]
fn param_scope_depth_is_one() {
    let out = resolve_ok("fn f(x: Int) -> Int { x }");
    let (_, def) = find_sym(&out, "x");
    assert_eq!(def.scope_depth, 1);
}

#[test]
fn generic_param_registered_with_generic_kind() {
    let out = resolve_ok("fn id<T>(x: T) -> T { x }");
    let (_, def) = find_sym(&out, "T");
    assert!(matches!(def.kind, DefKind::GenericParam));
}

#[test]
fn generic_param_scope_depth_is_one() {
    let out = resolve_ok("fn id<T>(x: T) -> T { x }");
    let (_, def) = find_sym(&out, "T");
    assert_eq!(def.scope_depth, 1);
}

#[test]
fn local_let_scope_depth_is_two() {
    let out = resolve_ok("fn f() { let x: Int = 1; x }");
    let (_, def) = find_sym(&out, "x");
    assert_eq!(def.scope_depth, 2);
}

#[test]
fn local_let_registered_with_local_kind() {
    let out = resolve_ok("fn f() { let x: Int = 1; x }");
    let (_, def) = find_sym(&out, "x");
    assert!(matches!(def.kind, DefKind::Local { .. }));
}

#[test]
fn mut_local_recorded_correctly() {
    let out = resolve_ok("fn f() { let mut x: Int = 1; x }");
    let (_, def) = find_sym(&out, "x");
    assert!(matches!(def.kind, DefKind::Local { is_mut: true }));
}

#[test]
fn undefined_name_produces_error() {
    let out = resolve_err("fn f() { undefined_var }");
    assert!(out.errors.iter().any(|e| e.msg.contains("undefined_var")));
}

#[test]
fn undefined_type_produces_error() {
    let out = resolve_err("fn f(x: NoSuchType) { x }");
    assert!(out.errors.iter().any(|e| e.msg.contains("NoSuchType")));
}

#[test]
fn forward_reference_between_functions() {
    resolve_ok("fn a() { b() } fn b() { }");
}

#[test]
fn mutual_recursion_resolves_without_error() {
    resolve_ok("fn even(n: Int) -> Bool { odd(n) } fn odd(n: Int) -> Bool { even(n) }");
}

#[test]
fn let_binding_visible_after_definition() {
    resolve_ok("fn f() { let x: Int = 1; x }");
}

#[test]
fn let_binding_not_visible_before_definition() {
    let out = resolve_err("fn f() { y; let y: Int = 1; }");
    assert!(out.errors.iter().any(|e| e.msg.contains("y")));
}

#[test]
fn let_rhs_resolves_against_outer_not_self() {
    resolve_ok("fn f() { let x: Int = 1; let x: Int = x; }");
}

#[test]
fn shadow_produces_two_distinct_def_ids() {
    let out = resolve_ok("fn f() { let x: Int = 1; let x: Int = 2; }");
    let xs: Vec<_> = out.symbols.iter().filter(|(_, d)| d.name == "x").collect();
    assert_eq!(xs.len(), 2);
    assert_ne!(xs[0].0, xs[1].0);
}

#[test]
fn block_scope_isolates_bindings() {
    let out = resolve_err("fn f() -> Int { { let inner: Int = 1; }; inner }");
    assert!(out.errors.iter().any(|e| e.msg.contains("inner")));
}

#[test]
fn for_loop_binding_visible_in_body() {
    resolve_ok("fn f() { for (x in [1, 2, 3]) { x } }");
}

#[test]
fn lambda_param_visible_in_lambda_body() {
    resolve_ok("fn f() { let g = fn(x: Int) -> Int { x }; g }");
}

#[test]
fn type_alias_resolves_in_function_signature() {
    resolve_ok("type MyInt = Int; fn f(x: MyInt) -> MyInt { x }");
}

#[test]
fn struct_resolves_in_type_position() {
    resolve_ok("struct Point { x: Int, y: Int } fn f(p: Point) { p }");
}

#[test]
fn struct_resolves_in_struct_lit() {
    resolve_ok("struct Point { x: Int, y: Int } fn make() -> Point { Point { x: 1, y: 2 } }");
}

#[test]
fn generic_param_visible_in_return_type_and_body() {
    resolve_ok("fn id<T>(x: T) -> T { x }");
}

#[test]
fn no_errors_on_deeply_nested_blocks() {
    resolve_ok("fn f() { let a: Int = 1; { let b: Int = 2; { let c: Int = a; c } } }");
}

#[test]
fn error_count_matches_number_of_undefined_names() {
    let out = resolve_err("fn f() { a; b; c }");
    assert_eq!(out.errors.len(), 3);
}

#[test]
fn global_scope_def_ids_match_symbol_table() {
    let out = resolve_ok("fn foo() { } fn bar() { }");
    for (name, &id) in &out.global_scope {
        assert_eq!(out.symbols.get(id).name, *name);
    }
}
