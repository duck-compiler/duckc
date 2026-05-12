use super::parser::*;
use crate::parser2::tokenizer::{Token, tokenize_no_comments};

fn lex(src: &str) -> Vec<WithSpan<Token>> {
    let (toks, errs) = tokenize_no_comments(src, 0);
    assert!(errs.is_empty(), "lex errors: {:?}", errs);
    toks
}

fn parse_ok(src: &str) -> SourceFile<Parsed> {
    let (source_file, errs) = parse(lex(src), 0);
    assert!(errs.is_empty(), "parse errors in {:?}: {:?}", src, errs);
    source_file
}

fn parse_expr_ok(src: &str) -> Expr<Parsed> {
    let (expr, errs) = parse_single_expr(lex(src), 0);
    assert!(errs.is_empty(), "parse errors in {:?}: {:?}", src, errs);
    expr.expect("expected expression")
}

#[test]
fn empty_file() {
    let source_file = parse_ok("");
    assert!(source_file.items.is_empty());
}

#[test]
fn simple_function() {
    let source_file = parse_ok("fn add(x: Int, y: Int) -> Int { x }");
    assert_eq!(source_file.items.len(), 1);
    let Item::Function(func) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(func.name.value, "add");
    assert_eq!(func.params.len(), 2);
    assert_eq!(func.params[0].name.value, "x");
    assert_eq!(func.params[1].name.value, "y");
    assert!(func.return_type.is_some());
}

#[test]
fn static_function() {
    let source_file = parse_ok("static fn make() -> Int { 0 }");
    let Item::Function(func) = &source_file.items[0] else {
        panic!()
    };
    assert!(func.is_static);
}

#[test]
fn type_alias() {
    let source_file = parse_ok("type Foo = Int;");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(type_alias.name.value, "Foo");
    assert!(matches!(type_alias.type_expr.desc, TypeDescription::Int));
}

#[test]
fn struct_decl() {
    let source_file = parse_ok("struct Point { x: Int, y: Int }");
    let Item::Struct(struct_decl) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(struct_decl.name.value, "Point");
    assert_eq!(struct_decl.fields.len(), 2);
}

#[test]
fn use_duck() {
    let source_file = parse_ok("use std::io;");
    let Item::Use(UseDecl::Duck(segs, _)) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(segs[0].value, "std");
    assert_eq!(segs[1].value, "io");
}

#[test]
fn use_go() {
    let source_file = parse_ok("use go \"fmt\";");
    assert!(matches!(
        &source_file.items[0],
        Item::Use(UseDecl::Go(_, _))
    ));
}

#[test]
fn extension_decl() {
    let source_file = parse_ok("extend Int with impl { fn double() -> Int { 0 } }");
    let Item::Extension(extension) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(extension.methods.len(), 1);
    assert_eq!(extension.methods[0].name.value, "double");
}

#[test]
fn expr_int() {
    let expr = parse_expr_ok("42");
    assert!(matches!(expr.kind, ExprKind::Int(42)));
}

#[test]
fn expr_float() {
    let expr = parse_expr_ok("3.14");
    assert!(matches!(expr.kind, ExprKind::Float(_)));
    let ExprKind::Float(float_val) = expr.kind else {
        panic!()
    };
    assert!((float_val - 3.14).abs() < 1e-9);
}

#[test]
fn expr_bool() {
    assert!(matches!(parse_expr_ok("true").kind, ExprKind::Bool(true)));
    assert!(matches!(parse_expr_ok("false").kind, ExprKind::Bool(false)));
}

#[test]
fn expr_string() {
    let expr = parse_expr_ok("\"hello\"");
    assert!(matches!(&expr.kind, ExprKind::String(s) if s == "hello"));
}

#[test]
fn expr_tag() {
    let expr = parse_expr_ok(".ok");
    assert!(matches!(&expr.kind, ExprKind::Tag(s) if s == "ok"));
}

#[test]
fn expr_unary() {
    assert!(matches!(parse_expr_ok("!true").kind, ExprKind::Not(_)));
    assert!(matches!(parse_expr_ok("-1").kind, ExprKind::Neg(_)));
    assert!(matches!(parse_expr_ok("~x").kind, ExprKind::BitNot(_)));
    assert!(matches!(parse_expr_ok("*ptr").kind, ExprKind::Deref(_)));
    assert!(matches!(parse_expr_ok("&x").kind, ExprKind::Ref(_)));
    assert!(matches!(parse_expr_ok("&mut x").kind, ExprKind::RefMut(_)));
}

#[test]
fn expr_binary() {
    assert!(matches!(parse_expr_ok("1 + 2").kind, ExprKind::Add(_, _)));
    assert!(matches!(parse_expr_ok("a == b").kind, ExprKind::Eq(_, _)));
    assert!(matches!(parse_expr_ok("a and b").kind, ExprKind::And(_, _)));
    assert!(matches!(parse_expr_ok("a or b").kind, ExprKind::Or(_, _)));
}

#[test]
fn expr_block() {
    let expr = parse_expr_ok("{ 1; 2 }");
    let ExprKind::Block(stmts) = expr.kind else {
        panic!()
    };
    assert_eq!(stmts.len(), 2);
}

#[test]
fn expr_block_trailing_semi() {
    let expr = parse_expr_ok("{ 1; }");
    let ExprKind::Block(stmts) = expr.kind else {
        panic!()
    };
    assert_eq!(stmts.len(), 2); // int + unit tuple
    assert!(matches!(stmts[1].kind, ExprKind::Tuple(ref v) if v.is_empty()));
}

#[test]
fn expr_if() {
    let expr = parse_expr_ok("if (x) { 1 } else { 2 }");
    assert!(matches!(
        expr.kind,
        ExprKind::If {
            else_branch: Some(_),
            ..
        }
    ));
}

#[test]
fn expr_while() {
    let expr = parse_expr_ok("while (true) { 1 }");
    assert!(matches!(expr.kind, ExprKind::While { .. }));
}

#[test]
fn expr_for() {
    let expr = parse_expr_ok("for (x in xs) { x }");
    assert!(matches!(expr.kind, ExprKind::For { .. }));
}

#[test]
fn expr_return() {
    assert!(matches!(
        parse_expr_ok("return 42").kind,
        ExprKind::Return(Some(_))
    ));
    assert!(matches!(
        parse_expr_ok("return").kind,
        ExprKind::Return(None)
    ));
}

#[test]
fn expr_call() {
    let expr = parse_expr_ok("foo(1, 2)");
    assert!(matches!(expr.kind, ExprKind::Call { .. }));
}

#[test]
fn expr_field_access() {
    let expr = parse_expr_ok("foo.bar");
    assert!(matches!(&expr.kind, ExprKind::Field { field, .. } if field.value == "bar"));
}

#[test]
fn expr_index() {
    let expr = parse_expr_ok("arr[0]");
    assert!(matches!(expr.kind, ExprKind::Index { .. }));
}

#[test]
fn expr_duck_lit() {
    let expr = parse_expr_ok("{ x: 1, y: 2 }");
    let ExprKind::DuckLit(fields) = expr.kind else {
        panic!()
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].0.value, "x");
}

#[test]
fn expr_struct_lit() {
    let expr = parse_expr_ok("Point { x: 1, y: 2 }");
    assert!(matches!(expr.kind, ExprKind::StructLit { .. }));
}

#[test]
fn expr_array() {
    let expr = parse_expr_ok("[1, 2, 3]");
    let ExprKind::Array(elems) = expr.kind else {
        panic!()
    };
    assert_eq!(elems.len(), 3);
}

#[test]
fn expr_tuple() {
    let expr = parse_expr_ok("(1, 2)");
    let ExprKind::Tuple(elems) = expr.kind else {
        panic!()
    };
    assert_eq!(elems.len(), 2);
}

#[test]
fn expr_match() {
    let expr = parse_expr_ok("match x { Int => 1, else => 0 }");
    let ExprKind::Match { arms, else_arm, .. } = expr.kind else {
        panic!()
    };
    assert_eq!(arms.len(), 1);
    assert!(else_arm.is_some());
}

#[test]
fn expr_lambda() {
    let expr = parse_expr_ok("fn(x: Int) -> Int { x }");
    assert!(matches!(expr.kind, ExprKind::Lambda { .. }));
}

#[test]
fn expr_let_decl() {
    let expr = parse_expr_ok("let x: Int = 42");
    let ExprKind::Let { name, .. } = &expr.kind else {
        panic!()
    };
    assert_eq!(name.value, "x");
}

#[test]
fn expr_assign() {
    let expr = parse_expr_ok("x = 1");
    assert!(matches!(expr.kind, ExprKind::Assign { .. }));
}

#[test]
fn expr_pipeline() {
    let expr = parse_expr_ok("x -> foo()");
    let ExprKind::Call { args, .. } = expr.kind else {
        panic!()
    };
    assert_eq!(args.len(), 1); // x prepended
}

#[test]
fn expr_defer_async() {
    assert!(matches!(
        parse_expr_ok("defer foo()").kind,
        ExprKind::Defer(_)
    ));
    assert!(matches!(
        parse_expr_ok("async foo()").kind,
        ExprKind::Async(_)
    ));
}

#[test]
fn type_primitives() {
    let source_file = parse_ok("type A = Int; type B = String; type C = Bool;");
    assert_eq!(source_file.items.len(), 3);
    assert!(
        matches!(&source_file.items[0], Item::TypeAlias(type_alias) if matches!(type_alias.type_expr.desc, TypeDescription::Int))
    );
}

#[test]
fn type_array() {
    let source_file = parse_ok("type A = [Int];");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert!(matches!(
        &type_alias.type_expr.desc,
        TypeDescription::Array(_)
    ));
}

#[test]
fn type_fun() {
    let source_file = parse_ok("type F = fn(x: Int) -> Bool;");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert!(matches!(
        &type_alias.type_expr.desc,
        TypeDescription::Fun { .. }
    ));
}

#[test]
fn type_union() {
    let source_file = parse_ok("type U = Int | String;");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert!(matches!(&type_alias.type_expr.desc, TypeDescription::Or(_)));
}

#[test]
fn type_duck() {
    let source_file = parse_ok("type D = duck { x: Int, y: String };");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert!(matches!(
        &type_alias.type_expr.desc,
        TypeDescription::Duck(_)
    ));
}

#[test]
fn type_ref() {
    let source_file = parse_ok("type R = &Int;");
    let Item::TypeAlias(type_alias) = &source_file.items[0] else {
        panic!()
    };
    assert!(matches!(
        &type_alias.type_expr.desc,
        TypeDescription::Ref(_)
    ));
}

#[test]
fn spans_non_zero() {
    let expr = parse_expr_ok("foo(1, 2)");
    assert_ne!(expr.span.start, expr.span.end);
}

#[test]
fn generics_in_function() {
    let source_file = parse_ok("fn id<T>(x: T) -> T { x }");
    let Item::Function(func) = &source_file.items[0] else {
        panic!()
    };
    assert_eq!(func.generics.len(), 1);
    assert_eq!(func.generics[0].value.name.value, "T");
}

#[test]
fn inline_go_expr() {
    let expr = parse_expr_ok("go { some go code }");
    assert!(matches!(expr.kind, ExprKind::InlineGo(_)));
}

#[test]
fn program_test() {
    let source_file =
        parse_ok("fn main() {\n    const x = fn (f: String) {\n        let y = 10;\n    }\n}");

    // one top-level item: `fn main`
    assert_eq!(source_file.items.len(), 1);
    let Item::Function(main_fn) = &source_file.items[0] else {
        panic!("expected fn")
    };
    assert_eq!(main_fn.name.value, "main");
    assert_eq!(main_fn.params.len(), 0);
    assert!(main_fn.return_type.is_none());

    // body is a block with one statement: the const binding
    let ExprKind::Block(body_stmts) = &main_fn.body.kind else {
        panic!("expected block")
    };
    assert_eq!(
        body_stmts.len(),
        1,
        "body should have exactly one statement"
    );
    let ExprKind::Const {
        name,
        type_ann,
        value,
    } = &body_stmts[0].kind
    else {
        panic!("expected const binding")
    };
    assert_eq!(name.value, "x");
    assert!(type_ann.is_none());

    // value is a lambda `fn (f: String) { let y = 10; }`
    let ExprKind::Lambda {
        is_mut,
        params,
        return_type,
        body,
    } = &value.kind
    else {
        panic!("expected lambda")
    };
    assert!(!is_mut);
    assert_eq!(params.len(), 1);
    assert_eq!(params[0].name.value, "f");
    assert!(matches!(
        &params[0].type_expr.desc,
        TypeDescription::String(_)
    ));
    assert!(return_type.is_none());

    // lambda body: block with `let y = 10` then implicit unit from trailing `;`
    let ExprKind::Block(lambda_stmts) = &body.kind else {
        panic!("expected block")
    };
    assert_eq!(
        lambda_stmts.len(),
        2,
        "let + implicit unit from trailing semicolon"
    );
    let ExprKind::Let {
        name: y_name,
        is_mut: y_mut,
        type_ann: y_ann,
        value: y_val,
    } = &lambda_stmts[0].kind
    else {
        panic!("expected let binding")
    };
    assert_eq!(y_name.value, "y");
    assert!(!y_mut);
    assert!(y_ann.is_none());
    assert!(matches!(y_val.kind, ExprKind::Int(10)));
    assert!(matches!(lambda_stmts[1].kind, ExprKind::Tuple(ref v) if v.is_empty()));
}
