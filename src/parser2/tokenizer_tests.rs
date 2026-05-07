use super::tokenizer::{FmtStringPart, tokenize, tokenize_no_comments, Token};
use super::parser::Span;

fn lex(src: &str) -> Vec<Token> {
    let (toks, errs) = tokenize(src, 0);
    assert!(errs.is_empty(), "lex errors in {:?}: {:?}", src, errs);
    toks.into_iter().map(|t| t.value).collect()
}

fn lex_no_comments(src: &str) -> Vec<Token> {
    let (toks, errs) = tokenize_no_comments(src, 0);
    assert!(errs.is_empty(), "lex errors in {:?}: {:?}", src, errs);
    toks.into_iter().map(|t| t.value).collect()
}

fn ctrl(c: char) -> Token { Token::Ctrl(c) }

#[test]
fn keywords() {
    let cases = [
        ("fn", Token::Fn), ("let", Token::Let), ("const", Token::Const),
        ("type", Token::Type), ("struct", Token::Struct), ("use", Token::Use),
        ("impl", Token::Impl), ("extend", Token::Extend), ("with", Token::With),
        ("mut", Token::Mut), ("static", Token::Static), ("return", Token::Return),
        ("if", Token::If), ("else", Token::Else), ("while", Token::While),
        ("for", Token::For), ("in", Token::In), ("break", Token::Break),
        ("continue", Token::Continue), ("match", Token::Match), ("as", Token::As),
        ("and", Token::And), ("or", Token::Or), ("duck", Token::Duck),
        ("schema", Token::Schema), ("module", Token::Module),
        ("typeof", Token::Typeof), ("keyof", Token::Keyof),
        ("test", Token::Test), ("component", Token::Component),
        ("template", Token::Template), ("async", Token::Async), ("defer", Token::Defer),
    ];
    for (src, expected) in cases {
        assert_eq!(lex(src), vec![expected], "keyword {src:?}");
    }
}

#[test]
fn bool_literals() {
    assert_eq!(lex("true"),   vec![Token::Bool(true)]);
    assert_eq!(lex("false"),  vec![Token::Bool(false)]);
    assert_eq!(lex("truex"),  vec![Token::Ident("truex".into())]);
    assert_eq!(lex("ifelse"), vec![Token::Ident("ifelse".into())]);
}

#[test]
fn integers() {
    assert_eq!(lex("1"),     vec![Token::Int(1)]);
    assert_eq!(lex("2003"),  vec![Token::Int(2003)]);
    assert_eq!(lex("-1"),    vec![ctrl('-'), Token::Int(1)]);
    assert_eq!(lex("-2003"), vec![ctrl('-'), Token::Int(2003)]);
}

#[test]
fn strings() {
    assert_eq!(lex("\"\""),        vec![Token::String("".into())]);
    assert_eq!(lex("\"XX\""),      vec![Token::String("XX".into())]);
    assert_eq!(lex("\"X\\\"X\""),  vec![Token::String("X\"X".into())]);
    assert_eq!(lex("\"\\n\\n\""),  vec![Token::String("\n\n".into())]);
    assert_eq!(lex("\"\\0\""),     vec![Token::String("\0".into())]);
}

#[test]
fn char_literals() {
    assert_eq!(lex("'c'"),    vec![Token::Char('c')]);
    assert_eq!(lex("'\\n'"),  vec![Token::Char('\n')]);
    assert_eq!(lex("'\\\\'"), vec![Token::Char('\\')]);
}

#[test]
fn operators_two_char() {
    let cases = [
        ("==", Token::EqEq), ("!=", Token::BangEq), ("<=", Token::LtEq),
        (">=", Token::GtEq), ("::", Token::ScopeRes), ("->", Token::ThinArrow),
        ("=>", Token::ThickArrow), ("+=", Token::PlusEq), ("-=", Token::SubEq),
        ("*=", Token::MulEq), ("/=", Token::DivEq), ("%=", Token::ModEq),
    ];
    for (src, expected) in cases {
        assert_eq!(lex(src), vec![expected], "op {src:?}");
    }
    assert_eq!(lex(">>="), vec![Token::ShrEq]);
    assert_eq!(lex("<<="), vec![Token::ShlEq]);
}

#[test]
fn ref_mut_token() {
    assert_eq!(lex("&mut x"), vec![Token::RefMut, Token::Ident("x".into())]);
    assert_eq!(lex("&mut"),   vec![ctrl('&'), Token::Mut]);
}

#[test]
fn comments_and_doc_comments() {
    assert_eq!(lex("// hello"),  vec![Token::Comment("hello".into())]);
    assert_eq!(lex("/// hello"), vec![Token::DocComment("hello".into())]);
    assert_eq!(lex("//"),        vec![Token::Comment("".into())]);
    assert_eq!(lex("///"),       vec![Token::DocComment("".into())]);
    assert_eq!(lex("//  trim  "),vec![Token::Comment("trim".into())]);
    assert_eq!(lex("// first\n// second"), vec![
        Token::Comment("first".into()),
        Token::Comment("second".into()),
    ]);
    assert_eq!(lex_no_comments("type // comment"), vec![Token::Type]);
    assert_eq!(lex_no_comments("// only"), vec![]);
}

#[test]
fn unicode_ident() {
    assert_eq!(lex("let π = 3;"), vec![
        Token::Let, Token::Ident("π".into()), ctrl('='), Token::Int(3), ctrl(';'),
    ]);
}

#[test]
fn float_is_int_dot_int() {
    assert_eq!(lex("1.1"), vec![Token::Int(1), ctrl('.'), Token::Int(1)]);
}

#[test]
fn fmt_string_simple() {
    assert_eq!(lex("f\"\""), vec![Token::FmtString(vec![])]);

    let toks = lex("f\"{1}\"");
    let Token::FmtString(parts) = &toks[0] else { panic!() };
    assert!(matches!(&parts[0], FmtStringPart::Tokens(ts) if {
        matches!(ts[0].value, Token::Ctrl('{')) &&
        matches!(ts[1].value, Token::Int(1)) &&
        matches!(ts[2].value, Token::Ctrl('}'))
    }));
}

#[test]
fn fmt_string_mixed() {
    let toks = lex("f\"FMT {var}\"");
    let Token::FmtString(parts) = &toks[0] else { panic!() };
    assert!(matches!(&parts[0], FmtStringPart::Literal(s) if s == "FMT "));
    assert!(matches!(&parts[1], FmtStringPart::Tokens(_)));
}

#[test]
fn fmt_string_nested_fstring() {
    let toks = lex("f\"outer {f\"inner {y}\"} end\"");
    let Token::FmtString(parts) = &toks[0] else { panic!() };
    assert!(matches!(&parts[0], FmtStringPart::Literal(s) if s == "outer "));
    let FmtStringPart::Tokens(inner_toks) = &parts[1] else { panic!() };
    assert!(matches!(&inner_toks[1].value, Token::FmtString(_)));
    assert!(matches!(&parts[2], FmtStringPart::Literal(s) if s == " end"));
}

#[test]
fn inline_go() {
    assert_eq!(lex("go {}"),       vec![Token::InlineGo("".into())]);
    assert_eq!(lex("go { xx }"),   vec![Token::InlineGo(" xx ".into())]);
    assert_eq!(lex("go { {} }"),   vec![Token::InlineGo(" {} ".into())]);
    assert_eq!(lex("go {{}{}{}}"), vec![Token::InlineGo("{}{}{}".into())]);
}

#[test]
fn inline_jsx() {
    assert_eq!(
        lex("jsx {console.log(\"hi\")}"),
        vec![Token::InlineJsx("console.log(\"hi\")".into())],
    );
}

#[test]
fn spans_file_id_propagated() {
    let src = "let x";
    let (toks, _) = tokenize(src, 7);
    assert_eq!(toks[0].span.file_id, 7);
    assert_eq!(toks[1].span.file_id, 7);
    assert_eq!(toks[0].span, Span::new(0, 3, 7));
    assert_eq!(toks[1].span, Span::new(4, 5, 7));
}

#[test]
fn type_duck_statement() {
    let toks = lex_no_comments("type Y = duck { x: String };");
    assert_eq!(toks, vec![
        Token::Type, Token::Ident("Y".into()), ctrl('='), Token::Duck,
        ctrl('{'), Token::Ident("x".into()), ctrl(':'),
        Token::Ident("String".into()), ctrl('}'), ctrl(';'),
    ]);
}

#[test]
fn adjacent_literals() {
    let toks = lex("\"a\"f\"b\"'c'");
    assert!(matches!(&toks[0], Token::String(s) if s == "a"));
    assert!(matches!(&toks[1], Token::FmtString(_)));
    assert!(matches!(&toks[2], Token::Char('c')));
}

#[test]
fn number_then_ident() {
    assert_eq!(lex("123testing"), vec![Token::Int(123), Token::Ident("testing".into())]);
}
