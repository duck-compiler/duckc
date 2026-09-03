use serde_json::Value;

use crate::ast::builder::{array, array_index, assign, binary, bool_lit, break_stmt, continue_stmt, deref_target, dereference, expr_stmt, field_access, field_access_on, field_call, field_target, float, fn_call, fn_def, generic_call, generic_field_access, generic_fn_call, generic_fn_def, generic_method, generic_struct_def, generic_struct_init, generic_type, if_else_expr, if_expr, int, mem_name, method, name_target, named_type, no_type, pointer_type, priv_field, program, pub_field, pub_method, pub_static_method, reference, return_stmt, string, struct_def_with_impl, struct_init, tuple, tuple_index, tuple_index_target, tuple_type, type_, unary, use_stmt, var_decl, while_expr};

use crate::ast::expression::{BinaryOperator, Expr, UnaryOperator};
use crate::ast::memory_target::MemTar;
use crate::ast::struct_definition::{MethodKind, Visibility};
use crate::ast::{AstRoot, NodeId, Stmt, TypeExpression, assign_generate_node_ids};
use crate::frontend::parser::parse_module;

fn parse<'src>(source: &'src str) -> AstRoot<'src> {
    match parse_module("parser_test.duck", source) {
        Ok(ast) => ast,
        Err(error) => panic!("unexpected parse error: {error}"),
    }
}

fn assert_parses_to_ast<'src>(source: &'src str, expected_ast: AstRoot<'src>) {
    let mut parsed = parse(source);
    let mut expected_ast = expected_ast;

    assign_generate_node_ids(&mut parsed);
    assign_generate_node_ids(&mut expected_ast);

    assert_eq!(
        ast_to_json_str(&parsed),
        ast_to_json_str(&expected_ast),
        "source: {source}",
    );
}

fn ast_to_json_str(ast: &AstRoot<'_>) -> String {
    let mut value = ast_to_json_value(ast);
    empty_spans(&mut value);

    serde_json::to_string_pretty(&value).expect("value should serialize")
}

fn ast_to_json_value(ast: &AstRoot<'_>) -> Value {
    serde_json::to_value(ast)
        .expect("ast should serialize")
}

fn empty_spans(value: &mut Value) {
    match value {
        Value::Object(members) => {
            for (key, member) in members.iter_mut() {
                match key.as_str() {
                    "span" => *member = Value::Null,
                    _ => empty_spans(member),
                }
            }
        }
        Value::Array(items) => items.iter_mut().for_each(empty_spans),
        _ => {}
    }
}

fn parse_error_message(source: &str) -> String {
    match parse_module("parser_test.duck", source) {
        Ok(_) => panic!("expected a parse error for: {source}"),
        Err(error) => error.message.to_string(),
    }
}

fn collect_node_ids(value: &Value, ids: &mut Vec<u64>) {
    match value {
        Value::Object(members) => {
            for (key, member) in members {
                match (key.as_str(), member.as_u64()) {
                    ("id", Some(id)) => ids.push(id),
                    _ => collect_node_ids(member, ids),
                }
            }
        }
        Value::Array(items) => {
            for item in items {
                collect_node_ids(item, ids);
            }
        }
        _ => {}
    }
}

#[test]
fn parse_veryify_that_every_node_gets_unique_id() {
    let ast = parse(
        r#"
        use fmt;

        struct Pair<A, B> {
            pub first: A,
            second: B,
        } impl {
            pub fn swap<C>(extra: C) -> C {
                let copy = self.first;
                return extra;
            }

            pub static fn make() -> int {
                let values = [1, 2, 3];
                let nested = (1, (2, 3));
                let first = nested.1.0;
                while first < 10 {
                    if first == 3 { break; } else { continue; }
                }
                return values[0];
            }
        }

        fn main() {
            let pair = Pair<int, int> { first: 1, second: 2 };
            fmt.Println(pair.first);
        }
        "#,
    );

    let mut node_ids = Vec::new();
    collect_node_ids(
        &ast_to_json_value(&ast),
        &mut node_ids
    );

    assert!(!node_ids.is_empty());
    assert!(
        node_ids.iter().all(|id| *id != NodeId::DUMMY.0 as u64),
        "parser left a dummy"
    );

    let mut unique = node_ids.clone();
    unique.sort_unstable();
    unique.dedup();

    assert_eq!(unique.len(), node_ids.len(), "parser duplicate node id");
}

// returns the spans of the exprs inside a main fn
fn main_fn_source_spans(source: &str) -> Vec<String> {
    let ast = parse(source);
    let Stmt::FunctionDefinition { body, .. } = &ast.statements[0].variant else {
        panic!("expected a fn def");
    };

    let Stmt::Expression { expr } = &body.statements[0].variant else {
        panic!("expected an expr statement");
    };

    let mut spans = vec![source[expr.span.start..expr.span.end].to_string()];
    match &*expr.variant {
        Expr::Binary { right, .. } => {
            spans.push(source[right.span.start..right.span.end].to_string());
        }
        Expr::MemoryTarget(target) => {
            spans.push(source[target.span.start..target.span.end].to_string());
            if let MemTar::TupleIndex { target: inner, .. } = &target.variant {
                spans.push(source[inner.span.start..inner.span.end].to_string());
            }
        }
        _ => {}
    }

    spans
}

#[test]
fn node_spans_cover_the_correct_source() {
    assert_eq!(
        main_fn_source_spans("fn main() { 1 + 2 * 3; }"),
        vec!["1 + 2 * 3", "2 * 3"],
    );
    assert_eq!(
        main_fn_source_spans("fn main() { boxed.with<Box<int>>; }"),
        vec!["boxed.with<Box<int>>", "boxed.with<Box<int>>"],
    );
    assert_eq!(
        main_fn_source_spans("fn main() { pair.0.1; }"),
        vec!["pair.0.1", "pair.0.1", "pair.0"],
    );
}

#[test]
fn parse_use_stmt_with_and_without_alias() {
    assert_parses_to_ast("use fmt;", program(vec![use_stmt("fmt", None)]));
    assert_parses_to_ast("use fmt as f;", program(vec![use_stmt("fmt", Some("f"))]));
    assert_parses_to_ast(
        r#"use "math/rand" as rand;"#,
        program(vec![use_stmt("math/rand", Some("rand"))]),
    );
}

#[test]
fn parse_fn_def_with_params_and_return_type() {
    assert_parses_to_ast(
        "fn add(a: int, b: int) -> int { return a + b; }",
        program(vec![fn_def(
            "add",
            vec![("a", TypeExpression::Int), ("b", TypeExpression::Int)],
            type_(TypeExpression::Int),
            vec![return_stmt(Some(binary(
                mem_name("a"),
                BinaryOperator::Add,
                mem_name("b"),
            )))],
        )]),
    );
}

#[test]
fn parse_fn_without_return_type() {
    assert_parses_to_ast(
        "fn main() { }",
        program(vec![fn_def("main", vec![], no_type(), vec![])]),
    );
}

#[test]
fn parse_generic_fn() {
    assert_parses_to_ast(
        "fn identity<T>(value: T) -> T { return value; }",
        program(vec![generic_fn_def(
            "identity",
            vec!["T"],
            vec![("value", named_type("T"))],
            type_(named_type("T")),
            vec![return_stmt(Some(mem_name("value")))],
        )]),
    );
}

#[test]
fn parse_struct_with_pub_and_private_fields() {
    assert_parses_to_ast(
        "struct Point { pub x: int, y: string }",
        program(vec![struct_def_with_impl(
            "Point",
            vec![
                pub_field("x", TypeExpression::Int),
                priv_field("y", TypeExpression::String),
            ],
            vec![],
        )]),
    );
}

#[test]
fn parse_struct_with_impl_block() {
    assert_parses_to_ast(
        r#"
        struct Counter {
            pub value: int
        } impl {
            pub fn bump() -> int {
                return self.value;
            }

            static fn zero() -> int {
                return 0;
            }
        }
        "#,
        program(vec![struct_def_with_impl(
            "Counter",
            vec![pub_field("value", TypeExpression::Int)],
            vec![
                pub_method(
                    "bump",
                    vec![],
                    type_(TypeExpression::Int),
                    vec![return_stmt(Some(field_access(name_target("self"), "value")))],
                ),
                method(
                    Visibility::Private,
                    MethodKind::Static,
                    "zero",
                    vec![],
                    type_(TypeExpression::Int),
                    vec![return_stmt(Some(int(0)))],
                ),
            ],
        )]),
    );
}

#[test]
fn parse_generic_struct_with_generic_method() {
    assert_parses_to_ast(
        r#"
        struct Box<T> {
            pub value: T
        } impl {
            pub fn map<A>(other: A) -> A {
                return other;
            }
        }
        "#,
        program(vec![generic_struct_def(
            "Box",
            vec!["T"],
            vec![pub_field("value", named_type("T"))],
            vec![generic_method(
                Visibility::Public,
                MethodKind::Instance,
                "map",
                vec!["A"],
                vec![("other", named_type("A"))],
                type_(named_type("A")),
                vec![return_stmt(Some(mem_name("other")))],
            )],
        )]),
    );
}

#[test]
fn parse_nested_generic_type_leaving_remainder() {
    assert_parses_to_ast(
        "fn main() { let boxed: Box<Box<int>>; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "boxed",
                type_(generic_type(
                    "Box",
                    vec![generic_type("Box", vec![TypeExpression::Int])],
                )),
                None,
            )],
        )]),
    );
}

#[test]
fn parse_nested_generic_type_args_on_call() {
    assert_parses_to_ast(
        "fn main() { make<Box<int>>(); }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(generic_fn_call(
                "make",
                vec![generic_type("Box", vec![TypeExpression::Int])],
                vec![],
            ))],
        )]),
    );
}

#[test]
fn parse_var_decls() {
    assert_parses_to_ast(
        "fn main() { let a: int = 1; let b = 2; let c: string; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl("a", type_(TypeExpression::Int), Some(int(1))),
                var_decl("b", no_type(), Some(int(2))),
                var_decl("c", type_(TypeExpression::String), None),
            ],
        )]),
    );
}

#[test]
fn parse_assign_stmt() {
    assert_parses_to_ast(
        "fn main() { x = 1; p.value = 2; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                assign(name_target("x"), int(1)),
                assign(field_target(name_target("p"), "value"), int(2)),
            ],
        )]),
    );
}

#[test]
fn parse_literals() {
    assert_parses_to_ast(
        r#"fn main() { 1; 1.5; true; "hi"; }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(int(1)),
                expr_stmt(float(1.5)),
                expr_stmt(bool_lit(true)),
                expr_stmt(string("hi")),
            ],
        )]),
    );
}

#[test]
fn parse_precedence() {
    assert_parses_to_ast(
        "fn main() { 1 + 2 * 3; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(binary(
                int(1),
                BinaryOperator::Add,
                binary(int(2), BinaryOperator::Mul, int(3)),
            ))],
        )]),
    );
}

#[test]
fn parse_binary_ops_left_associativity() {
    assert_parses_to_ast(
        "fn main() { 1 - 2 - 3; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(binary(
                binary(int(1), BinaryOperator::Sub, int(2)),
                BinaryOperator::Sub,
                int(3),
            ))],
        )]),
    );
}

#[test]
fn parse_full_precedence() {
    let comparison = binary(
        binary(int(1), BinaryOperator::Add, binary(int(2), BinaryOperator::Mul, int(3))),
        BinaryOperator::Less,
        int(4),
    );
    let equality = binary(comparison, BinaryOperator::Eq, bool_lit(true));
    let and = binary(equality, BinaryOperator::And, bool_lit(false));
    let or = binary(and, BinaryOperator::Or, bool_lit(true));

    assert_parses_to_ast(
        "fn main() { 1 + 2 * 3 < 4 == true && false || true; }",
        program(vec![fn_def("main", vec![], no_type(), vec![expr_stmt(or)])]),
    );
}

#[test]
fn parse_prefix_and_binary_ops_check_tightness() {
    assert_parses_to_ast(
        "fn main() { -1 * 2; !a; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(binary(
                    unary(UnaryOperator::Neg, int(1)),
                    BinaryOperator::Mul,
                    int(2),
                )),
                expr_stmt(unary(UnaryOperator::Bang, mem_name("a"))),
            ],
        )]),
    );
}

#[test]
fn parse_pointers() {
    assert_parses_to_ast(
        "fn main() { let p: *int = &value; let v = *p; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl(
                    "p",
                    type_(pointer_type(TypeExpression::Int)),
                    Some(reference(mem_name("value"))),
                ),
                var_decl("v", no_type(), Some(dereference(mem_name("p")))),
            ],
        )]),
    );
}

#[test]
fn parse_dereference_field_access() {
    assert_parses_to_ast(
        "fn main() { (*p).value; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(field_access(
                deref_target(mem_name("p")),
                "value",
            ))],
        )]),
    );
}

#[test]
fn parse_array_literals_and_array_index() {
    assert_parses_to_ast(
        r#"fn main() { let items: string[] = ["a", "b"]; items[0]; }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl(
                    "items",
                    type_(TypeExpression::Array { inner: Box::new(TypeExpression::String) }),
                    Some(array(vec![string("a"), string("b")])),
                ),
                expr_stmt(array_index("items", int(0))),
            ],
        )]),
    );
}

#[test]
fn parse_tuple_type_and_tuple_access() {
    assert_parses_to_ast(
        "fn main() { let pair: (int, string) = (1, \"a\"); pair.1; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                var_decl(
                    "pair",
                    type_(tuple_type(vec![TypeExpression::Int, TypeExpression::String])),
                    Some(tuple(vec![int(1), string("a")])),
                ),
                expr_stmt(tuple_index(name_target("pair"), 1)),
            ],
        )]),
    );
}

#[test]
fn parse_parens_around_expr_not_as_tuple() {
    assert_parses_to_ast(
        "fn main() { (1 + 2) * 3; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(binary(
                binary(int(1), BinaryOperator::Add, int(2)),
                BinaryOperator::Mul,
                int(3),
            ))],
        )]),
    );
}

#[test]
fn parse_chained_tuple_index() {
    assert_parses_to_ast(
        "fn main() { pair.0.1; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(tuple_index(
                tuple_index_target(name_target("pair"), 0),
                1,
            ))],
        )]),
    );
}

#[test]
fn parse_comparison() {
    assert_parses_to_ast(
        "fn main() { a < b; a > b; a < b > c; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(binary(mem_name("a"), BinaryOperator::Less, mem_name("b"))),
                expr_stmt(binary(mem_name("a"), BinaryOperator::Greater, mem_name("b"))),
                expr_stmt(binary(
                    binary(mem_name("a"), BinaryOperator::Less, mem_name("b")),
                    BinaryOperator::Greater,
                    mem_name("c"),
                )),
            ],
        )]),
    );
}

#[test]
fn parse_if() {
    assert_parses_to_ast(
        "fn main() { if a < b { } }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(if_expr(
                binary(mem_name("a"), BinaryOperator::Less, mem_name("b")),
                vec![],
            ))],
        )]),
    );
}

#[test]
fn parse_generic_call() {
    let generic_call_of_a_and_b = program(vec![fn_def(
        "main",
        vec![],
        no_type(),
        vec![expr_stmt(generic_fn_call(
            "a",
            vec![named_type("b")],
            vec![mem_name("c")],
        ))],
    )]);

    assert_parses_to_ast("fn main() { a<b>(c); }", generic_call_of_a_and_b);

    let spaced = program(vec![fn_def(
        "main",
        vec![],
        no_type(),
        vec![expr_stmt(generic_fn_call(
            "a",
            vec![named_type("b")],
            vec![mem_name("c")],
        ))],
    )]);

    assert_parses_to_ast("fn main() { a < b > (c); }", spaced);

    assert_parses_to_ast(
        "fn main() { (a < b) > (c); }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(binary(
                binary(mem_name("a"), BinaryOperator::Less, mem_name("b")),
                BinaryOperator::Greater,
                mem_name("c"),
            ))],
        )]),
    );
}

#[test]
fn parse_type_arged_fn_call() {
    assert_parses_to_ast(
        "fn main() { identity<int>(5); }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(generic_fn_call(
                "identity",
                vec![TypeExpression::Int],
                vec![int(5)],
            ))],
        )]),
    );
}

#[test]
fn parse_type_args_on_method_call() {
    assert_parses_to_ast(
        "fn main() { boxed.with<string>(other); }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(generic_call(
                generic_field_access(name_target("boxed"), "with", vec![TypeExpression::String]),
                vec![],
                vec![mem_name("other")],
            ))],
        )]),
    );
}

#[test]
fn parse_generic_method_as_value() {
    assert_parses_to_ast(
        "fn main() { let make = boxed.with<string>; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "make",
                no_type(),
                Some(generic_field_access(
                    name_target("boxed"),
                    "with",
                    vec![TypeExpression::String],
                )),
            )],
        )]),
    );
}

#[test]
fn parse_struct_init() {
    assert_parses_to_ast(
        r#"fn main() { let p = Point { x: 1, y: 2 }; }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "p",
                no_type(),
                Some(struct_init("Point", vec![("x", int(1)), ("y", int(2))])),
            )],
        )]),
    );
}

#[test]
fn parse_generic_struct_init() {
    assert_parses_to_ast(
        r#"fn main() { let b = Box<string> { value: "x" }; }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "b",
                no_type(),
                Some(generic_struct_init(
                    "Box",
                    vec![TypeExpression::String],
                    vec![("value", string("x"))],
                )),
            )],
        )]),
    );
}

#[test]
fn parse_fn_call_on_names_and_fields() {
    assert_parses_to_ast(
        r#"fn main() { greet(1); fmt.Println("hi"); }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(fn_call("greet", vec![int(1)])),
                expr_stmt(field_call("fmt", "Println", vec![string("hi")])),
            ],
        )]),
    );
}

#[test]
fn parse_if_else_as_value() {
    assert_parses_to_ast(
        "fn main() { let x = if a { 1 } else { 2 }; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "x",
                no_type(),
                Some(if_else_expr(
                    mem_name("a"),
                    vec![expr_stmt(int(1))],
                    vec![expr_stmt(int(2))],
                )),
            )],
        )]),
    );
}

#[test]
fn parse_else_if() {
    assert_parses_to_ast(
        "fn main() { if a { 1 } else if b { 2 } else { 3 } }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(if_else_expr(
                mem_name("a"),
                vec![expr_stmt(int(1))],
                vec![expr_stmt(if_else_expr(
                    mem_name("b"),
                    vec![expr_stmt(int(2))],
                    vec![expr_stmt(int(3))],
                ))],
            ))],
        )]),
    );
}

#[test]
fn parse_while_with_break_and_continue() {
    assert_parses_to_ast(
        "fn main() { while a { break; continue; } }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(while_expr(
                mem_name("a"),
                vec![break_stmt(), continue_stmt()],
            ))],
        )]),
    );
}

#[test]
fn parse_block_statement_no_semicolon() {
    assert_parses_to_ast(
        "fn main() { if a { } while b { } }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![
                expr_stmt(if_expr(mem_name("a"), vec![])),
                expr_stmt(while_expr(mem_name("b"), vec![])),
            ],
        )]),
    );
}

#[test]
fn parse_skip_comments() {
    assert_parses_to_ast(
        "// leading\nfn main() { // trailing\n }",
        program(vec![fn_def("main", vec![], no_type(), vec![])]),
    );
}

#[test]
fn parse_field_access_on_call_result() {
    assert_parses_to_ast(
        "fn main() { make().value; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(field_access_on(fn_call("make", vec![]), "value"))],
        )]),
    );
}

#[test]
fn parse_trailing_commas_in_list() {
    assert_parses_to_ast(
        r#"
        struct Pair<A, B,> { pub first: A, pub second: B, }

        fn take(a: int, b: int,) { }

        fn main() {
            take(1, 2,);
            let items = [1, 2,];
            let pair = Pair<int, int,> { first: 1, second: 2, };
        }
        "#,
        program(vec![
            generic_struct_def(
                "Pair",
                vec!["A", "B"],
                vec![
                    pub_field("first", named_type("A")),
                    pub_field("second", named_type("B")),
                ],
                vec![],
            ),
            fn_def(
                "take",
                vec![("a", TypeExpression::Int), ("b", TypeExpression::Int)],
                no_type(),
                vec![],
            ),
            fn_def(
                "main",
                vec![],
                no_type(),
                vec![
                    expr_stmt(fn_call("take", vec![int(1), int(2)])),
                    var_decl("items", no_type(), Some(array(vec![int(1), int(2)]))),
                    var_decl(
                        "pair",
                        no_type(),
                        Some(generic_struct_init(
                            "Pair",
                            vec![TypeExpression::Int, TypeExpression::Int],
                            vec![("first", int(1)), ("second", int(2))],
                        )),
                    ),
                ],
            ),
        ]),
    );
}

#[test]
fn parse_struct_init_with_type_args() {
    assert_parses_to_ast(
        "fn main() { let b = Box<Box<int>> { value: inner }; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "b",
                no_type(),
                Some(generic_struct_init(
                    "Box",
                    vec![generic_type("Box", vec![TypeExpression::Int])],
                    vec![("value", mem_name("inner"))],
                )),
            )],
        )]),
    );
}

#[test]
fn parse_trailing_expr_no_semicolon() {
    assert_eq!(
        parse_error_message("fn main() { let a = 1 }"),
        "expected `;`, found `}`",
    );
    assert_eq!(
        parse_error_message("fn main() { return 1 }"),
        "expected `;`, found `}`",
    );
    assert_eq!(
        parse_error_message("fn main() { a = 1 }"),
        "expected `;`, found `}`",
    );

    assert_parses_to_ast(
        "fn main() { 1 }",
        program(vec![fn_def("main", vec![], no_type(), vec![expr_stmt(int(1))])]),
    );
}

#[test]
fn parse_generic_before_assignment() {
    assert_parses_to_ast(
        "fn main() { let b: Box<int>= 1; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "b",
                type_(generic_type("Box", vec![TypeExpression::Int])),
                Some(int(1)),
            )],
        )]),
    );

    assert_parses_to_ast(
        "fn main() { let b: Box<Box<int>>= 1; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl(
                "b",
                type_(generic_type(
                    "Box",
                    vec![generic_type("Box", vec![TypeExpression::Int])],
                )),
                Some(int(1)),
            )],
        )]),
    );
}

#[test]
fn parse_shift() {
    assert_eq!(
        parse_error_message("fn main() { a >> b; }"),
        "the `>>` is not supported yet",
    );

    assert_parses_to_ast(
        "fn main() { a >= b; }",
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![expr_stmt(binary(
                mem_name("a"),
                BinaryOperator::GreaterEq,
                mem_name("b"),
            ))],
        )]),
    );
}

#[test]
fn parse_deep_nested_reports() {
    let nested_parens = format!("fn main() {{ {}1{}; }}", "(".repeat(4000), ")".repeat(4000));
    assert_eq!(parse_error_message(&nested_parens), "input is nested to deep");

    let nested_negs = format!("fn main() {{ {}true; }}", "!".repeat(4000));
    assert_eq!(parse_error_message(&nested_negs), "input is nested to deep");

    let nested_types = format!(
        "fn main() {{ let a: {}int{}; }}",
        "Box<".repeat(4000),
        ">".repeat(4000)
    );

    assert_eq!(parse_error_message(&nested_types), "input is nested to deep");

    let nested_blocks = format!("fn main() {{ {}{} }}", "if a { ".repeat(4000), "}".repeat(4000));
    assert_eq!(parse_error_message(&nested_blocks), "input is nested to deep");

    let else_if_chain = format!("fn main() {{ if a {{ }}{} }}", " else if b { }".repeat(4000));
    assert_eq!(parse_error_message(&else_if_chain), "input is nested to deep");
}

#[test]
fn parse_long_comp_chain_reports() {
    let chain = (0..4000)
        .map(|index| format!("a{index}"))
        .collect::<Vec<_>>()
        .join(" < ");

    let source = format!("fn main() {{ {chain}; }}");
    let ast = parse(&source);

    let Stmt::FunctionDefinition { body, .. } = &ast.statements[0].variant else {
        panic!("expected a function definition");
    };

    let Stmt::Expression { expr } = &body.statements[0].variant else {
        panic!("expected an expression statement");
    };

    let mut operands = Vec::new();
    let mut node = expr;

    while let Expr::Binary { left, op, right } = &*node.variant {
        assert!(matches!(op, BinaryOperator::Less));
        operands.push(&source[right.span.start..right.span.end]);
        node = left;
    }

    operands.push(&source[node.span.start..node.span.end]);
    operands.reverse();

    assert_eq!(operands.len(), 4000);
    assert_eq!(operands[0], "a0");
    assert_eq!(operands[3999], "a3999");
    assert_eq!(&source[expr.span.start..expr.span.end], chain);
}

#[test]
fn parse_octal_escape() {
    assert_eq!(
        parse_error_message(r#"fn main() { let a = "\777"; }"#),
        r"octal escape `\777` is out of range",
    );

    assert_parses_to_ast(
        r#"fn main() { let a = "\101\n\t"; }"#,
        program(vec![fn_def(
            "main",
            vec![],
            no_type(),
            vec![var_decl("a", no_type(), Some(string(r"\101\n\t")))],
        )]),
    );
}

#[test]
fn parse_type_args_on_function_reports() {
    assert_eq!(
        parse_error_message("fn main() { let g = identity<int>; }"),
        "type arguments on a function value are not supported yet",
    );
}

#[test]
fn parse_missing_semicolon_reports() {
    assert_eq!(
        parse_error_message("fn main() { let a = 1 let b = 2; }"),
        "expected `;`, found `let`",
    );
}

#[test]
fn parse_unbalanced_parens_reports() {
    assert_eq!(
        parse_error_message("fn main() { greet(1; }"),
        "expected `)`, found `;`",
    );
}

#[test]
fn parse_unbalanced_brace_reports() {
    assert_eq!(
        parse_error_message("fn main() { let a = 1;"),
        "expected `}`, found end of file",
    );
}

#[test]
fn parse_unexpected_token_reports() {
    assert_eq!(
        parse_error_message("fn main() { let = 1; }"),
        "expected a variable name, found `=`",
    );
}

#[test]
fn parse_invalid_char_reports() {
    assert_eq!(parse_error_message("fn main() { $ }"), "invalid character `$`");
}

#[test]
fn parse_invalid_assign_reports() {
    assert_eq!(
        parse_error_message("fn main() { 1 = 2; }"),
        "invalid assignment target",
    );
}

#[test]
fn parse_f_string_reports() {
    assert_eq!(
        parse_error_message(r#"fn main() { let a = f"{1}"; }"#),
        "f-strings are not supported yet",
    );
}

#[test]
fn parse_unsupported_infix_reports() {
    assert_eq!(
        parse_error_message("fn main() { 1 % 2; }"),
        "the `%` is not supported yet",
    );

    assert_eq!(
        parse_error_message("fn main() { 1 << 2; }"),
        "the `<<` is not supported yet",
    );

    assert_eq!(
        parse_error_message("fn main() { 1 >> 2; }"),
        "the `>>` is not supported yet",
    );

    assert_eq!(
        parse_error_message("fn main() { 1 & 2; }"),
        "the `&` is not supported yet",
    );

    assert_eq!(
        parse_error_message("fn main() { a += 1; }"),
        "operator assignments are not supported yet",
    );
}

#[test]
fn parse_const_reports() {
    assert_eq!(
        parse_error_message("fn main() { const a = 1; }"),
        "`const` not supported yet",
    );
}

#[test]
fn parse_block_expr_reports() {
    assert_eq!(
        parse_error_message("fn main() { let a = { 1 }; }"),
        "block expressions are not supported yet",
    );
}
