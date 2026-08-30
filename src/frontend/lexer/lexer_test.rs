use crate::ast::{Identifier, NodeId, Span};
use crate::frontend::lexer::{LexDiag, LexDiagnostic, LexState, StrPart, StringPart, Tok, Token};

const DUMMY_FILE: &str = "test.duck";

fn empty_span<'src>() -> Span<'src> {
    Span {
        file_path: DUMMY_FILE,
        start: 0,
        end: 0,
    }
}

fn without_spans(variant: Tok<'_>) -> Tok<'_> {
    match variant {
        Tok::Identifier(identifier) => Tok::Identifier(Identifier {
            span: empty_span(),
            ..identifier
        }),
        Tok::StringLiteral(parts) => {
            Tok::StringLiteral(parts.into_iter().map(part_without_spans).collect())
        }
        other => other,
    }
}

fn part_without_spans(part: StringPart<'_>) -> StringPart<'_> {
    let variant = match part.variant {
        StrPart::Interpolation(tokens) => {
            StrPart::Interpolation(tokens.into_iter().map(token_without_spans).collect())
        }
        other => other,
    };

    StringPart {
        variant,
        span: empty_span(),
    }
}

fn token_without_spans(token: Token<'_>) -> Token<'_> {
    Token {
        variant: without_spans(token.variant),
        span: empty_span(),
    }
}

fn lex(source: &str) -> Vec<Tok<'_>> {
    let mut state = LexState::init(DUMMY_FILE, source);
    let mut tokens = Vec::new();

    loop {
        let token = state
            .lex_single()
            .unwrap_or_else(|diagnostic| panic!("unexpected {diagnostic:?} in `{source}`"));

        let is_eof = matches!(token.variant, Tok::EOF);
        tokens.push(without_spans(token.variant));

        if is_eof {
            return tokens;
        }
    }
}

fn lex_first(source: &str) -> Tok<'_> {
    lex(source).into_iter().next().expect("at least one token")
}

fn lex_diagnostic(source: &str) -> LexDiagnostic {
    let mut state = LexState::init(DUMMY_FILE, source);

    loop {
        match state.lex_single() {
            Err(diagnostic) => return diagnostic,
            Ok(token) if matches!(token.variant, Tok::EOF) => {
                panic!("expected a diagnostic in `{source}`")
            }
            Ok(_) => continue,
        }
    }
}

fn ident(name: &str) -> Tok<'_> {
    Tok::Identifier(Identifier {
        id: NodeId::DUMMY,
        ident: name,
        span: empty_span(),
    })
}

fn text(value: &str) -> StringPart<'_> {
    part(StrPart::Text(value))
}

fn part(variant: StrPart<'_>) -> StringPart<'_> {
    StringPart {
        variant,
        span: empty_span(),
    }
}

fn interpolation(variants: Vec<Tok<'_>>) -> StringPart<'_> {
    part(StrPart::Interpolation(
        variants
            .into_iter()
            .map(|variant| Token {
                variant,
                span: empty_span(),
            })
            .collect(),
    ))
}

fn part_variants(variant: Tok<'_>) -> std::vec::IntoIter<StringPart<'_>> {
    match variant {
        Tok::StringLiteral(parts) => parts.into_iter(),
        other => panic!("expected a string literal, found {other:?}"),
    }
}


fn string_parts(source: &str) -> Vec<StringPart<'_>> {
    part_variants(lex_first(source)).collect()
}

fn raw_string_parts(source: &str) -> Vec<StringPart<'_>> {
    let mut state = LexState::init(DUMMY_FILE, source);
    let token = state.lex_single().expect("string literal");

    part_variants(token.variant).collect()
}

fn spans_of(parts: &[StringPart<'_>]) -> Vec<(usize, usize)> {
    parts
        .iter()
        .map(|part| (part.span.start, part.span.end))
        .collect()
}

#[test]
fn lex_keyword() {
    let keywords = [
        ("let", Tok::Let),
        ("const", Tok::Const),
        ("fn", Tok::Func),
        ("as", Tok::As),
        ("return", Tok::Return),
        ("if", Tok::If),
        ("else", Tok::Else),
        ("while", Tok::While),
        ("continue", Tok::Continue),
        ("break", Tok::Break),
        ("struct", Tok::Struct),
        ("impl", Tok::Impl),
        ("pub", Tok::Pub),
        ("use", Tok::Use),
        ("static", Tok::Static),
    ];

    for (source, expected) in keywords {
        assert_eq!(lex_first(source), expected, "lexing `{source}`");
    }
}

#[test]
fn lex_type_keywords() {
    let keywords = [
        ("int", Tok::Int),
        ("int8", Tok::Int8),
        ("int16", Tok::Int16),
        ("int32", Tok::Int32),
        ("int64", Tok::Int64),
        ("uint", Tok::Uint),
        ("uint8", Tok::Uint8),
        ("uint16", Tok::Uint16),
        ("uint32", Tok::Uint32),
        ("uint64", Tok::Uint64),
        ("bool", Tok::Bool),
        ("float", Tok::Float),
        ("float32", Tok::Float32),
        ("string", Tok::String),
    ];

    for (source, expected) in keywords {
        assert_eq!(lex_first(source), expected, "lexing `{source}`");
    }
}

#[test]
fn lex_bool() {
    assert_eq!(lex_first("true"), Tok::BoolLiteral(true));
    assert_eq!(lex_first("false"), Tok::BoolLiteral(false));
}

#[test]
fn lex_self_ident() {
    assert_eq!(lex_first("self"), ident("self"));
}

#[test]
fn lex_idents() {
    for name in ["foo", "_private", "x1", "Box", "snake_case_2"] {
        assert_eq!(lex_first(name), ident(name));
    }
}

#[test]
fn lex_keyword_like_idents() {
    for name in ["iffy", "int8_t", "returns", "_let", "structure"] {
        assert_eq!(lex_first(name), ident(name));
    }
}

#[test]
fn lex_parens_near_to_ident() {
    assert_eq!(
        lex("foo(bar)"),
        vec![
            ident("foo"),
            Tok::LeftParen,
            ident("bar"),
            Tok::RightParen,
            Tok::EOF,
        ],
    );
}

#[test]
fn identifier_span() {
    let mut state = LexState::init(DUMMY_FILE, "  value = 1;");
    let token = state.lex_single().expect("identifier");

    assert_eq!(token.span.start, 2);
    assert_eq!(token.span.end, 7);

    let Tok::Identifier(identifier) = token.variant else {
        panic!("expected an identifier, found {:?}", token.variant);
    };

    assert_eq!(identifier.span.start, 2);
    assert_eq!(identifier.span.end, 7);
}

#[test]
fn skip_whitespace() {
    let source = format!("{}let{}", " \n\t".repeat(4096), " ".repeat(4096));

    assert_eq!(lex(&source), vec![Tok::Let, Tok::EOF]);
}

#[test]
fn lex_int_without_suffix() {
    assert_eq!(lex_first("42"), Tok::IntLiteral(42));
    assert_eq!(lex_first("0"), Tok::IntLiteral(0));
    assert_eq!(
        lex("5i8"),
        vec![Tok::IntLiteral(5), ident("i8"), Tok::EOF],
    );
}

#[test]
fn float_needs_digits_on_both_sides() {
    assert_eq!(lex_first("1.5"), Tok::FloatLiteral(1.5));
    assert_eq!(lex_first("0.25"), Tok::FloatLiteral(0.25));
    assert_eq!(lex("1."), vec![Tok::IntLiteral(1), Tok::Dot, Tok::EOF]);
    assert_eq!(
        lex("tuple.0"),
        vec![ident("tuple"), Tok::Dot, Tok::IntLiteral(0), Tok::EOF],
    );
}

#[test]
fn int_literals_out_of_range() {
    let diagnostic = lex_diagnostic("99999999999999999999999");

    assert_eq!(diagnostic.variant, LexDiag::IntLiteralOutOfRange);
    assert_eq!(diagnostic.pos, 0);
    assert_eq!(diagnostic.len, 23);
}

#[test]
fn lex_operators() {
    let punctuation = [
        (",", Tok::Comma),
        (".", Tok::Dot),
        (":", Tok::Colon),
        ("->", Tok::Arrow),
        (";", Tok::Semicolon),
        ("~", Tok::Tilde),
        ("&", Tok::Ampersand),
        ("|", Tok::Bar),
        ("&&", Tok::And),
        ("||", Tok::Or),
        ("!", Tok::Bang),
    ];

    for (source, expected) in punctuation {
        assert_eq!(lex_first(source), expected, "lexing `{source}`");
    }
}

#[test]
fn lex_composed_operators() {
    assert_eq!(lex_first("-"), Tok::Minus);
    assert_eq!(lex_first("-="), Tok::MinusAssign);
    assert_eq!(lex_first("->"), Tok::Arrow);
    assert_eq!(
        lex("- > ->"),
        vec![Tok::Minus, Tok::Greater, Tok::Arrow, Tok::EOF],
    );
}

#[test]
fn lex_percent_operators() {
    assert_eq!(lex_first("%"), Tok::Percent);
    assert_eq!(lex_first("%="), Tok::PercentAssign);
    assert_eq!(
        lex("a % b"),
        vec![ident("a"), Tok::Percent, ident("b"), Tok::EOF],
    );
}

#[test]
fn lex_comp_and_shift_operators() {
    assert_eq!(lex_first("<="), Tok::LessEquals);
    assert_eq!(lex_first(">="), Tok::GreaterEquals);
    assert_eq!(lex_first("<<"), Tok::ShiftLeft);
    assert_eq!(lex_first(">>"), Tok::ShiftRight);
    assert_eq!(lex_first("<<="), Tok::ShiftLeftAssign);
    assert_eq!(lex_first(">>="), Tok::ShiftRightAssign);
}

#[test]
fn lex_crocodile_operators() {
    assert_eq!(lex_first("<"), Tok::Less);
    assert_eq!(lex_first(">"), Tok::Greater);
    assert_eq!(
        lex("a < b > c"),
        vec![
            ident("a"),
            Tok::Less,
            ident("b"),
            Tok::Greater,
            ident("c"),
            Tok::EOF,
        ],
    );
    assert_eq!(lex(">>>"), vec![Tok::ShiftRight, Tok::Greater, Tok::EOF]);
}

#[test]
fn lex_parens_and_braces() {
    assert_eq!(
        lex("([{}])"),
        vec![
            Tok::LeftParen,
            Tok::LeftSquare,
            Tok::LeftBrace,
            Tok::RightBrace,
            Tok::RightSquare,
            Tok::RightParen,
            Tok::EOF,
        ],
    );
}

#[test]
fn lex_string_with_parts() {
    assert_eq!(
        string_parts("\"a\\tb\\nc\""),
        vec![
            text("a"),
            part(StrPart::Tab),
            text("b"),
            part(StrPart::Newline),
            text("c"),
        ],
    );
}

#[test]
fn lex_octal_escape() {
    assert_eq!(
        string_parts("\"\\101ab\""),
        vec![part(StrPart::Octal("101")), text("ab")],
    );
}

#[test]
fn lex_incomplete_octal_escape() {
    let source = "\"\\1\" + 5";
    let mut state = LexState::init(DUMMY_FILE, source);

    let string = state.lex_single().expect("string literal");
    assert_eq!(without_spans(string.variant), Tok::StringLiteral(vec![]));
    assert_eq!(string.span.end, 4);

    assert_eq!(state.lex_single().expect("plus").variant, Tok::Plus);
    assert_eq!(
        state.lex_single().expect("int literal").variant,
        Tok::IntLiteral(5),
    );

    assert_eq!(state.non_fail_diagnostics.len(), 1);
    assert_eq!(
        state.non_fail_diagnostics[0].variant,
        LexDiag::InvalidEscapeSequence,
    );
}

#[test]
fn lex_incomplete_octal_escape_2() {
    let diagnostic = lex_diagnostic("\"\\1\nabc\"");

    assert_eq!(diagnostic.variant, LexDiag::NewlineInString);
    assert_eq!(diagnostic.pos, 3);
}

#[test]
fn lex_unknown_escape_no_fail() {
    let mut state = LexState::init(DUMMY_FILE, "\"a\\qb\"");

    let string = state.lex_single().expect("string literal");
    assert_eq!(
        without_spans(string.variant),
        Tok::StringLiteral(vec![text("a"), text("b")]),
    );

    assert_eq!(state.non_fail_diagnostics.len(), 1);
    assert_eq!(
        state.non_fail_diagnostics[0].variant,
        LexDiag::InvalidEscapeSequence,
    );
}

#[test]
fn lex_unclosed_string_reports_dignostic() {
    let diagnostic = lex_diagnostic("\"abc");

    assert_eq!(diagnostic.variant, LexDiag::UnclosedString);
    assert_eq!(diagnostic.pos, 0);
    assert_eq!(diagnostic.len, 4);
}

#[test]
fn lex_invalid_escape_reports_correct() {
    let mut state = LexState::init(DUMMY_FILE, "\"ab\\qc\"");

    state.lex_single().expect("string literal");

    assert_eq!(state.non_fail_diagnostics.len(), 1);
    assert_eq!(state.non_fail_diagnostics[0].pos, 3);
    assert_eq!(state.non_fail_diagnostics[0].len, 2);
}

#[test]
fn lex_multi_byte_char_keep_their_pos() {
    assert_eq!(string_parts("\"äöü\""), vec![text("äöü")]);
    assert_eq!(
        string_parts("\"ä\\101ö\""),
        vec![text("ä"), part(StrPart::Octal("101")), text("ö")],
    );
    assert_eq!(spans_of(&raw_string_parts("\"äöü\"")), vec![(1, 7)]);
}

#[test]
fn lex_multi_byte_escape() {
    let mut state = LexState::init(DUMMY_FILE, "\"a\\éb\"");

    let token = state.lex_single().expect("string literal");

    assert_eq!(
        without_spans(token.variant),
        Tok::StringLiteral(vec![text("a"), text("b")]),
    );
    assert_eq!(state.non_fail_diagnostics.len(), 1);
    assert_eq!(state.non_fail_diagnostics[0].pos, 2);
    assert_eq!(state.non_fail_diagnostics[0].len, 3);
}

#[test]
fn string_parts_spans_correct() {
    let parts = raw_string_parts("f\"ab\\t\\101{x}c\"");

    assert_eq!(
        spans_of(&parts),
        vec![(2, 4), (4, 6), (6, 10), (10, 13), (13, 14)],
    );
}

#[test]
fn lex_f_string() {
    assert_eq!(string_parts("f\"hallo\""), vec![text("hallo")]);
    assert_eq!(string_parts("f\"hallo\""), string_parts("\"hallo\""));
}

#[test]
fn lex_f() {
    assert_eq!(
        lex("f + f \"x\""),
        vec![
            ident("f"),
            Tok::Plus,
            ident("f"),
            Tok::StringLiteral(vec![text("x")]),
            Tok::EOF,
        ],
    );
}

#[test]
fn lex_f_string_spans_check() {
    let mut state = LexState::init(DUMMY_FILE, "f\"hi\"");
    let token = state.lex_single().expect("string literal");

    assert_eq!(token.span.start, 0);
    assert_eq!(token.span.end, 5);
}

#[test]
fn lex_f_string_with_interpolation() {
    assert_eq!(
        string_parts("f\"hallo {name}\""),
        vec![text("hallo "), interpolation(vec![ident("name")])],
    );
}

#[test]
fn lex_f_string_with_multiple_interoplations() {
    assert_eq!(
        string_parts("f\"{a}{b}{c}\""),
        vec![
            interpolation(vec![ident("a")]),
            interpolation(vec![ident("b")]),
            interpolation(vec![ident("c")]),
        ],
    );
}

#[test]
fn lex_f_string_with_multiple_interoplations_and_text() {
    assert_eq!(
        string_parts("f\"{a} und {b}!\""),
        vec![
            interpolation(vec![ident("a")]),
            text(" und "),
            interpolation(vec![ident("b")]),
            text("!"),
        ],
    );
}

#[test]
fn interpolation_ends_at_closingbrace() {
    assert_eq!(
        string_parts("f\"{Point{x: 1}}\""),
        vec![interpolation(vec![
            ident("Point"),
            Tok::LeftBrace,
            ident("x"),
            Tok::Colon,
            Tok::IntLiteral(1),
            Tok::RightBrace,
        ])],
    );
    assert_eq!(
        string_parts("f\"{(1 + 2) * 3}\""),
        vec![interpolation(vec![
            Tok::LeftParen,
            Tok::IntLiteral(1),
            Tok::Plus,
            Tok::IntLiteral(2),
            Tok::RightParen,
            Tok::Star,
            Tok::IntLiteral(3),
        ])],
    );
}

#[test]
fn braces_dont_kill_interpolation() {
    assert_eq!(
        string_parts("f\"{fmt(\"}\")}\""),
        vec![interpolation(vec![
            ident("fmt"),
            Tok::LeftParen,
            Tok::StringLiteral(vec![text("}")]),
            Tok::RightParen,
        ])],
    );
}

#[test]
fn lex_f_strings_nest_their_interpolations() {
    assert_eq!(
        string_parts("f\"{f\"{x}\"}\""),
        vec![interpolation(vec![Tok::StringLiteral(vec![
            interpolation(vec![ident("x")]),
        ])])],
    );
}

#[test]
fn lex_f_strings_mixed_escapes_and_interpolations() {
    assert_eq!(
        string_parts("f\"zeile1\\n{wert}\\tende\""),
        vec![
            text("zeile1"),
            part(StrPart::Newline),
            interpolation(vec![ident("wert")]),
            part(StrPart::Tab),
            text("ende"),
        ],
    );
}

#[test]
fn lex_text_in_braces_normal_text() {
    assert_eq!(string_parts("\"{name}\""), vec![text("{name}")]);
}

#[test]
fn lex_empty_f_string() {
    assert_eq!(string_parts("f\"\""), vec![]);
    assert_eq!(string_parts("f\"{}\""), vec![interpolation(vec![])]);
}

#[test]
fn lex_multiline_f_strings() {
    assert_eq!(
        string_parts("f\"{\n  a\n}\""),
        vec![interpolation(vec![ident("a")])],
    );
    assert_eq!(
        lex_diagnostic("f\"a\nb\"").variant,
        LexDiag::NewlineInString,
    );
}

#[test]
fn lex_unclosed_interpolation_reports() {
    let diagnostic = lex_diagnostic("f\"{name");
    assert_eq!(diagnostic.variant, LexDiag::InterpolationUnclosed);
}

#[test]
fn lex_unclosed_interpolation_leaves_eof() {
    let mut state = LexState::init(DUMMY_FILE, "f\"{name");

    assert_eq!(
        state.lex_single().expect_err("interpolation diagnostic").variant,
        LexDiag::InterpolationUnclosed,
    );

    assert_eq!(state.lex_single().expect("eof").variant, Tok::EOF);
}

#[test]
fn lex_interpolation_to_deep_reports() {
    let diagnostic = lex_diagnostic(&"f\"{".repeat(4096));

    assert_eq!(diagnostic.variant, LexDiag::InterpolationTooDeep);
}

#[test]
fn lex_unclosed_f_string_reports() {
    let diagnostic = lex_diagnostic("f\"{name} rest");

    assert_eq!(diagnostic.variant, LexDiag::UnclosedString);
    assert_eq!(diagnostic.pos, 0);
}

#[test]
fn lex_comments() {
    assert_eq!(
        lex("// a comment\nlet"),
        vec![Tok::Comment(" a comment"), Tok::Let, Tok::EOF],
    );
    assert_eq!(lex("// trailing"), vec![Tok::Comment(" trailing"), Tok::EOF]);
}

#[test]
fn lex_eof_is_end_of_file() {
    let mut state = LexState::init(DUMMY_FILE, "");

    assert_eq!(state.lex_single().expect("eof").variant, Tok::EOF);
    assert_eq!(
        state.lex_single().expect_err("eof diagnostic").variant,
        LexDiag::EOF,
    );
}

#[test]
fn lex_generic_fn() {
    assert_eq!(
        lex("fn add<T>(a: T, b: T) -> T { return a + b; }"),
        vec![
            Tok::Func,
            ident("add"),
            Tok::Less,
            ident("T"),
            Tok::Greater,
            Tok::LeftParen,
            ident("a"),
            Tok::Colon,
            ident("T"),
            Tok::Comma,
            ident("b"),
            Tok::Colon,
            ident("T"),
            Tok::RightParen,
            Tok::Arrow,
            ident("T"),
            Tok::LeftBrace,
            Tok::Return,
            ident("a"),
            Tok::Plus,
            ident("b"),
            Tok::Semicolon,
            Tok::RightBrace,
            Tok::EOF,
        ],
    );
}

#[test]
fn lex_struct_def() {
    assert_eq!(
        lex("pub struct Vec2 { pub x: float32, y: int8 }"),
        vec![
            Tok::Pub,
            Tok::Struct,
            ident("Vec2"),
            Tok::LeftBrace,
            Tok::Pub,
            ident("x"),
            Tok::Colon,
            Tok::Float32,
            Tok::Comma,
            ident("y"),
            Tok::Colon,
            Tok::Int8,
            Tok::RightBrace,
            Tok::EOF,
        ],
    );
}

#[test]
fn lex_var_decl() {
    assert_eq!(
        lex("let ok = self.rate >= 1.5 % 2 == true;"),
        vec![
            Tok::Let,
            ident("ok"),
            Tok::SingleEquals,
            ident("self"),
            Tok::Dot,
            ident("rate"),
            Tok::GreaterEquals,
            Tok::FloatLiteral(1.5),
            Tok::Percent,
            Tok::IntLiteral(2),
            Tok::DoubleEquals,
            Tok::BoolLiteral(true),
            Tok::Semicolon,
            Tok::EOF,
        ],
    );
}
