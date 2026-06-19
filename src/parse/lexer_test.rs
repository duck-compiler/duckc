use crate::{lex, parse::value_parser::empty_range};

use crate::parse::lexer::*;

fn all_empty(v: Vec<Token>) -> Vec<Spanned<Token>> {
    v.iter().map(|x| x.into_empty_span()).collect()
}

fn ctrl(c: char) -> Token {
    Token::ControlChar(c)
}

fn left_brace() -> Token {
    ctrl('{')
}

fn right_brace() -> Token {
    ctrl('}')
}

#[test]
fn test_lex() {
    let test_cases = vec![
        (
            "duckx {<>{{}}</>}",
            vec![Token::InlineDuckx(vec![
                Token::ControlChar('{').into_empty_span(),
                Token::HtmlString(vec![
                    HtmlStringContents::String("<>".to_string()),
                    HtmlStringContents::Tokens(vec![
                        Token::ControlChar('{').into_empty_span(),
                        Token::ControlChar('{').into_empty_span(),
                        Token::ControlChar('}').into_empty_span(),
                        Token::ControlChar('}').into_empty_span(),
                    ]),
                    HtmlStringContents::String("</>".to_string()),
                ])
                .into_empty_span(),
                Token::ControlChar('}').into_empty_span(),
            ])],
        ),
        (
            "duckx {<Counter initial={10} />}",
            vec![Token::InlineDuckx(
                vec![
                    Token::ControlChar('{'),
                    Token::HtmlString(vec![
                        HtmlStringContents::String("<Counter initial=".to_string()),
                        HtmlStringContents::Tokens(all_empty(vec![
                            Token::ControlChar('{'),
                            Token::IntLiteral(10),
                            Token::ControlChar('}'),
                        ])),
                        HtmlStringContents::String(" />".to_string()),
                    ]),
                    Token::ControlChar('}'),
                ]
                .into_iter()
                .map(|x| x.into_empty_span())
                .collect(),
            )],
        ),
        (
            "duckx {let hello = <> <!doctype html>{<Counter initial={100}/>} </>;}",
            vec![Token::InlineDuckx(all_empty(vec![
                left_brace(),
                Token::Let,
                Token::Ident("hello".to_string()),
                ctrl('='),
                Token::HtmlString(vec![
                    HtmlStringContents::String("<> <!doctype html>".to_string()),
                    HtmlStringContents::Tokens(all_empty(vec![
                        left_brace(),
                        Token::HtmlString(vec![
                            HtmlStringContents::String("<Counter initial=".to_string()),
                            HtmlStringContents::Tokens(all_empty(vec![
                                left_brace(),
                                Token::IntLiteral(100),
                                right_brace(),
                            ])),
                            HtmlStringContents::String("/>".to_string()),
                        ]),
                        right_brace(),
                    ])),
                    HtmlStringContents::String(" </>".to_string()),
                ]),
                ctrl(';'),
                right_brace(),
            ]))],
        ),
        (
            "duckx {let hello = <> {ti <span id={props.id} hello={123}></span> tle} <h1> hallo moin  123</h1> abc </>;}",
            vec![Token::InlineDuckx(all_empty(vec![
                left_brace(),
                Token::Let,
                Token::Ident("hello".to_string()),
                ctrl('='),
                Token::HtmlString(vec![
                    HtmlStringContents::String("<> ".to_string()),
                    HtmlStringContents::Tokens(all_empty(vec![
                        left_brace(),
                        Token::Ident("ti".to_string()),
                        Token::HtmlString(vec![
                            HtmlStringContents::String("<span id=".to_string()),
                            HtmlStringContents::Tokens(all_empty(vec![
                                left_brace(),
                                Token::Ident("props".to_string()),
                                ctrl('.'),
                                Token::Ident("id".to_string()),
                                right_brace(),
                            ])),
                            HtmlStringContents::String(" hello=".to_string()),
                            HtmlStringContents::Tokens(all_empty(vec![
                                left_brace(),
                                Token::IntLiteral(123),
                                right_brace(),
                            ])),
                            HtmlStringContents::String("></span>".to_string()),
                        ]),
                        Token::Ident("tle".to_string()),
                        right_brace(),
                    ])),
                    HtmlStringContents::String(" <h1> hallo moin  123</h1> abc </>".to_string()),
                ]),
                ctrl(';'),
                right_brace(),
            ]))],
        ),
        (
            "duckx {let hello = {1};}",
            vec![Token::InlineDuckx(all_empty(vec![
                left_brace(),
                Token::Let,
                Token::Ident("hello".to_string()),
                ctrl('='),
                left_brace(),
                Token::IntLiteral(1),
                right_brace(),
                ctrl(';'),
                right_brace(),
            ]))],
        ),
        (
            "f\"{{{1}}}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "f\"{{{-1}}}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('-'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "type /// hallo ich bin ein dokkommentar",
            vec![
                Token::Type,
                Token::DocComment("hallo ich bin ein dokkommentar".to_string()),
            ],
        ),
        ("component", vec![Token::Component]),
        (
            "jsx {console.log(\"Hallo, Welt\")}",
            vec![Token::InlineJsx("console.log(\"Hallo, Welt\")".to_string())],
        ),
        (
            "jsx {{console.log(\"Hallo, Welt\")}}",
            vec![Token::InlineJsx(
                "{console.log(\"Hallo, Welt\")}".to_string(),
            )],
        ),
        (
            "jsx {<MyComponent name=\"lol\"/>\n<h1>hallo</h1>}",
            vec![Token::InlineJsx(
                "<MyComponent name=\"lol\"/>\n<h1>hallo</h1>".to_string(),
            )],
        ),
        (
            "/// hallo ich bin ein dokkommentar",
            vec![Token::DocComment(
                "hallo ich bin ein dokkommentar".to_string(),
            )],
        ),
        (
            "// hallo ich bin ein kommentar",
            vec![Token::Comment("hallo ich bin ein kommentar".to_string())],
        ),
        ("//", vec![Token::Comment("".to_string())]),
        ("///", vec![Token::DocComment("".to_string())]),
        ("//    ", vec![Token::Comment("".to_string())]),
        ("///    ", vec![Token::DocComment("".to_string())]),
        (
            "//  leading and trailing whitespace  ",
            vec![Token::Comment(
                "leading and trailing whitespace".to_string(),
            )],
        ),
        (
            "///  leading and trailing whitespace  ",
            vec![Token::DocComment(
                "leading and trailing whitespace".to_string(),
            )],
        ),
        (
            "//// this is a doc comment",
            vec![Token::DocComment("/ this is a doc comment".to_string())],
        ),
        (
            "// an /// inner doc comment",
            vec![Token::Comment("an /// inner doc comment".to_string())],
        ),
        (
            "/// a // regular inner comment",
            vec![Token::DocComment("a // regular inner comment".to_string())],
        ),
        (
            "// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
            vec![Token::Comment(
                "a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string(),
            )],
        ),
        (
            "/// a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?",
            vec![Token::DocComment(
                "a doc comment with !@#$%^&*()_+-=[]{}|;':\",./<>?".to_string(),
            )],
        ),
        (
            "// Hallo World 🌍",
            vec![Token::Comment("Hallo World 🌍".to_string())],
        ),
        (
            "/// Hallo World 🌍",
            vec![Token::DocComment("Hallo World 🌍".to_string())],
        ),
        (
            "// first line\n// second line",
            vec![
                Token::Comment("first line".to_string()),
                Token::Comment("second line".to_string()),
            ],
        ),
        (
            "/// doc line 1\n/// doc line 2\n// regular line 3",
            vec![
                Token::DocComment("doc line 1".to_string()),
                Token::DocComment("doc line 2".to_string()),
                Token::Comment("regular line 3".to_string()),
            ],
        ),
        (
            "// a comment with a // nested one",
            vec![Token::Comment("a comment with a // nested one".to_string())],
        ),
        (
            "/// a doc comment with a // nested regular one",
            vec![Token::DocComment(
                "a doc comment with a // nested regular one".to_string(),
            )],
        ),
        (
            "// a comment with a /// nested doc one",
            vec![Token::Comment(
                "a comment with a /// nested doc one".to_string(),
            )],
        ),
        (
            "//\tcomment with a tab before content",
            vec![Token::Comment(
                "comment with a tab before content".to_string(),
            )],
        ),
        (
            "///\t doc comment with a tab before content",
            vec![Token::DocComment(
                "doc comment with a tab before content".to_string(),
            )],
        ),
        (
            "// comment with mixed languages: Привет мир, こんにちは世界",
            vec![Token::Comment(
                "comment with mixed languages: Привет мир, こんにちは世界".to_string(),
            )],
        ),
        (
            "//\n///",
            vec![
                Token::Comment("".to_string()),
                Token::DocComment("".to_string()),
            ],
        ),
        (
            "f\"{1}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "f\"{-1}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('-'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "type Y = duck {};",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "typeY=duck{};",
            vec![
                Token::Ident("typeY".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "type Y = duck {} & duck {};",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar('&'),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "typeof {};",
            vec![
                Token::TypeOf,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "type Y = duck { x: String, y: String };",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar(','),
                Token::Ident("y".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        ("()", vec![Token::ControlChar('('), Token::ControlChar(')')]),
        ("schema", vec![Token::Schema]),
        ("->", vec![Token::ThinArrow]),
        ("impl", vec![Token::Impl]),
        ("fn", vec![Token::Function]),
        ("\"\"", vec![Token::StringLiteral(String::from(""))]),
        ("\"XX\"", vec![Token::StringLiteral(String::from("XX"))]),
        (
            "\"X\\\"X\"",
            vec![Token::StringLiteral(String::from("X\"X"))],
        ),
        (
            "\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\"",
            vec![Token::StringLiteral(String::from(
                "Hallo ich bin ein String\n\n\nNeue Zeile",
            ))],
        ),
        ("1", vec![Token::IntLiteral(1)]),
        ("-1", vec![Token::ControlChar('-'), Token::IntLiteral(1)]),
        ("2003", vec![Token::IntLiteral(2003)]),
        (
            "-2003",
            vec![Token::ControlChar('-'), Token::IntLiteral(2003)],
        ),
        ("true", vec![Token::BoolLiteral(true)]),
        ("false", vec![Token::BoolLiteral(false)]),
        ("go { {} }", vec![Token::InlineGo(String::from(" {} "))]),
        ("go { xx }", vec![Token::InlineGo(String::from(" xx "))]),
        ("go {}", vec![Token::InlineGo(String::from(""))]),
        ("go {{}{}{}}", vec![Token::InlineGo(String::from("{}{}{}"))]),
        (
            "if (true) {}",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        (
            "if (true) {} else {}",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        ("\"\\0\"", vec![Token::StringLiteral("\0".to_string())]),
        (
            "if (true) {} else if {} else if {} else {}",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        ("'c'", vec![Token::CharLiteral('c')]),
        ("'\\n'", vec![Token::CharLiteral('\n')]),
        (
            "1.1",
            vec![
                Token::IntLiteral(1),
                Token::ControlChar('.'),
                Token::IntLiteral(1),
            ],
        ),
        (
            "let x: {};",
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "f\"FMT {var}\"",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::String("FMT ".into()),
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::Ident("var".into()), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ]),
            ])],
        ),
        (
            // Adjacent strings and f-strings to test greedy tokenizing.
            "\"a\"f\"b\"'c'",
            vec![
                Token::StringLiteral("a".to_string()),
                Token::FormatStringLiteral(vec![FmtStringContents::String("b".into())]),
                Token::CharLiteral('c'),
            ],
        ),
        (
            "123testing",
            vec![Token::IntLiteral(123), Token::Ident("testing".to_string())],
        ),
        ("ifelse", vec![Token::Ident("ifelse".to_string())]),
        (
            "let π = 3;",
            vec![
                Token::Let,
                Token::Ident("π".to_string()),
                Token::ControlChar('='),
                Token::IntLiteral(3),
                Token::ControlChar(';'),
            ],
        ),
        (
            // todo: discuss if π should be an valid identifier
            "let π = -3;",
            vec![
                Token::Let,
                Token::Ident("π".to_string()),
                Token::ControlChar('='),
                Token::ControlChar('-'),
                Token::IntLiteral(3),
                Token::ControlChar(';'),
            ],
        ),
        (
            // todo: divide token
            "5 / 2",
            vec![
                Token::IntLiteral(5),
                Token::ControlChar('/'),
                Token::IntLiteral(2),
            ],
        ),
        (
            // todo: divide token
            "-5 / -2",
            vec![
                Token::ControlChar('-'),
                Token::IntLiteral(5),
                Token::ControlChar('/'),
                Token::ControlChar('-'),
                Token::IntLiteral(2),
            ],
        ),
        (
            "fn(x){if(true)1 else 0;}",
            vec![
                Token::Function,
                Token::ControlChar('('),
                Token::Ident("x".to_string()),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::IntLiteral(1),
                Token::Else,
                Token::IntLiteral(0),
                Token::ControlChar(';'),
                Token::ControlChar('}'),
            ],
        ),
        (
            "fn(x){if(true)-1 else -0;}",
            vec![
                Token::Function,
                Token::ControlChar('('),
                Token::Ident("x".to_string()),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('-'),
                Token::IntLiteral(1),
                Token::Else,
                Token::ControlChar('-'),
                Token::IntLiteral(0),
                Token::ControlChar(';'),
                Token::ControlChar('}'),
            ],
        ),
        (
            "1.0// a float-like thing",
            vec![
                Token::IntLiteral(1),
                Token::ControlChar('.'),
                Token::IntLiteral(0),
                Token::Comment("a float-like thing".to_string()),
            ],
        ),
        (
            "-1.0// a float-like thing",
            vec![
                Token::ControlChar('-'),
                Token::IntLiteral(1),
                Token::ControlChar('.'),
                Token::IntLiteral(0),
                Token::Comment("a float-like thing".to_string()),
            ],
        ),
        (
            "f\" outer {\"inner\"} outer \"",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::String(" outer ".into()),
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::StringLiteral("inner".to_string()), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ]),
                FmtStringContents::String(" outer ".into()),
            ])],
        ),
        ("f\"\"", vec![Token::FormatStringLiteral(vec![])]),
        (
            "f\"{}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "f\"result is {calc(1, 2)}\"",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::String("result is ".into()),
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::Ident("calc".to_string()), empty_range()),
                    (Token::ControlChar('('), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar(','), empty_range()),
                    (Token::IntLiteral(2), empty_range()),
                    (Token::ControlChar(')'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ]),
            ])],
        ),
        (
            "f\"outer {f\"inner {y}\"} end\"",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::String("outer ".into()),
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (
                        Token::FormatStringLiteral(vec![
                            FmtStringContents::String("inner ".into()),
                            FmtStringContents::Tokens(vec![
                                (Token::ControlChar('{'), empty_range()),
                                (Token::Ident("y".to_string()), empty_range()),
                                (Token::ControlChar('}'), empty_range()),
                            ]),
                        ]),
                        empty_range(),
                    ),
                    (Token::ControlChar('}'), empty_range()),
                ]),
                FmtStringContents::String(" end".into()),
            ])],
        ),
        (
            "f\"{1+1}\"",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('+'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "f\"{1+1}a\"",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('+'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ]),
                FmtStringContents::String("a".into()),
            ])],
        ),
    ];

    for (src, expected_tokens) in test_cases {
        let parse_result = lex_parser("test", src).parse(src);

        assert_eq!(parse_result.has_errors(), false, "{}", src);
        assert_eq!(parse_result.has_output(), true, "{}", src);

        let output: Vec<Token> = parse_result
            .output()
            .expect(&src)
            .iter()
            .map(|token| {
                let mut cloned = token.clone();
                token_empty_range(&mut cloned);
                cloned.0
            })
            .collect();

        assert_eq!(output, expected_tokens, "{}", src);
    }
}

#[test]
fn test_lex_comment_filtered() {
    let test_cases = vec![
        (
            "f\"{{{1}}}\" // check",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        ("type // hallo ich bin ein dokkommentar", vec![Token::Type]),
        ("// hallo ich bin ein dokkommentar", vec![]),
        ("// hallo ich bin ein kommentar", vec![]),
        ("//", vec![]),
        ("//", vec![]),
        ("//    ", vec![]),
        ("//    ", vec![]),
        ("//  leading and trailing whitespace  ", vec![]),
        ("//  leading and trailing whitespace  ", vec![]),
        ("//this is a doc comment", vec![]),
        ("// an /// inner doc comment", vec![]),
        ("// a // regular inner comment", vec![]),
        ("// a comment with !@#$%^&*()_+-=[]{}|;':\",./<>?", vec![]),
        ("// Hallo World 🌍", vec![]),
        ("// first line\n// second line", vec![]),
        ("// a comment with a // nested one", vec![]),
        ("// a comment with a /// nested doc one", vec![]),
        ("//\tcomment with a tab before content", vec![]),
        (
            "// comment with mixed languages: Привет мир, こんにちは世界",
            vec![],
        ),
        ("//\n//", vec![]),
        (
            "f\"{1}\" // check",
            vec![Token::FormatStringLiteral(vec![FmtStringContents::Tokens(
                vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::IntLiteral(1), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ],
            )])],
        ),
        (
            "type Y = duck {}; // check",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "typeY=duck{}; // check",
            vec![
                Token::Ident("typeY".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "type Y = duck {} & duck {}; // check",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar('&'),
                Token::Duck,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "type Y = duck { x: String, y: String }; // check",
            vec![
                Token::Type,
                Token::Ident("Y".to_string()),
                Token::ControlChar('='),
                Token::Duck,
                Token::ControlChar('{'),
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar(','),
                Token::Ident("y".to_string()),
                Token::ControlChar(':'),
                Token::Ident("String".to_string()),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "() // check",
            vec![Token::ControlChar('('), Token::ControlChar(')')],
        ),
        ("-> // check", vec![Token::ThinArrow]),
        ("fn // check", vec![Token::Function]),
        (
            "\"\" // check",
            vec![Token::StringLiteral(String::from(""))],
        ),
        (
            "\"XX\" // check",
            vec![Token::StringLiteral(String::from("XX"))],
        ),
        (
            "\"X\\\"X\" // check",
            vec![Token::StringLiteral(String::from("X\"X"))],
        ),
        (
            "\"Hallo ich bin ein String\\n\\n\\nNeue Zeile\" // check",
            vec![Token::StringLiteral(String::from(
                "Hallo ich bin ein String\n\n\nNeue Zeile",
            ))],
        ),
        ("1 // check", vec![Token::IntLiteral(1)]),
        ("2003 // check", vec![Token::IntLiteral(2003)]),
        ("true // check", vec![Token::BoolLiteral(true)]),
        ("false // check", vec![Token::BoolLiteral(false)]),
        (
            "go { {} } // check",
            vec![Token::InlineGo(String::from(" {} "))],
        ),
        (
            "go { xx } // check",
            vec![Token::InlineGo(String::from(" xx "))],
        ),
        ("go {} // check", vec![Token::InlineGo(String::from(""))]),
        (
            "go {{}{}{}} // check",
            vec![Token::InlineGo(String::from("{}{}{}"))],
        ),
        (
            "if (true) {} // check",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        (
            "if (true) {} else {} // check",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        (
            "if (true) {} else if {} else if {} else {} // check",
            vec![
                Token::If,
                Token::ControlChar('('),
                Token::BoolLiteral(true),
                Token::ControlChar(')'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::If,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::Else,
                Token::ControlChar('{'),
                Token::ControlChar('}'),
            ],
        ),
        ("'c' // check", vec![Token::CharLiteral('c')]),
        ("'\\n'//w", vec![Token::CharLiteral('\n')]),
        (
            "1.1 // check",
            vec![
                Token::IntLiteral(1),
                Token::ControlChar('.'),
                Token::IntLiteral(1),
            ],
        ),
        (
            "let x: {}; // with comment",
            vec![
                Token::Let,
                Token::Ident("x".to_string()),
                Token::ControlChar(':'),
                Token::ControlChar('{'),
                Token::ControlChar('}'),
                Token::ControlChar(';'),
            ],
        ),
        (
            "f\"FMT {var}\" // check",
            vec![Token::FormatStringLiteral(vec![
                FmtStringContents::String("FMT ".into()),
                FmtStringContents::Tokens(vec![
                    (Token::ControlChar('{'), empty_range()),
                    (Token::Ident("var".into()), empty_range()),
                    (Token::ControlChar('}'), empty_range()),
                ]),
            ])],
        ),
        ("async", vec![Token::Async]),
        ("test", vec![Token::Test]),
        ("keyof", vec![Token::KeyOf]),
        ("typeof", vec![Token::TypeOf]),
        ("extend", vec![Token::Extend]),
        ("with", vec![Token::With]),
    ];

    for (src, expected_tokens) in test_cases {
        let parse_result = lex("test", src);

        let output: Vec<Token> = parse_result
            .iter()
            .map(|token| {
                let mut cloned = token.clone();
                token_empty_range(&mut cloned);
                cloned.0
            })
            .collect();

        assert_eq!(output, expected_tokens, "{}", src);
    }
}
