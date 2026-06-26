use crate::{
    ast::Span,
    frontend::lexer::{LexState, StrPart, StringPart, Tok, Token},
};

fn make(variant: Tok, start: usize, len: usize) -> Token {
    Token::new(variant, "test", start, len)
}

fn make_string_part(variant: StrPart, start: usize, len: usize) -> StringPart {
    StringPart {
        variant,
        span: Span {
            file_path: "test",
            start,
            end: start + len,
        },
    }
}

#[test]
fn test_success_lex() {
    let file = "test";

    let cases = {
        use Tok::*;
        [
            (
                "const x;",
                &[
                    Token::new(Tok::Const, file, 0, 5),
                    Token::new(Tok::Identifier("x"), file, 6, 1),
                    Token::new(Tok::Semicolon, file, 7, 1),
                    Token::new(Tok::EOF, file, 7, 1),
                ] as &[Token],
            ),
            (
                "let x = 5",
                &[
                    make(Let, 0, 3),
                    make(Identifier("x"), 4, 1),
                    make(SingleEquals, 5, 1),
                    make(IntLiteral(5), 7, 1),
                    make(EOF, 8, 1),
                ],
            ),
            (
                "<< <<= >> >>= == != += -=\n*= /= %= &= |=",
                &[
                    make(ShiftLeft, 0, 2),
                    make(ShiftLeftAssign, 3, 3),
                    make(ShiftRight, 7, 2),
                    make(ShiftRightAssign, 10, 3),
                    make(DoubleEquals, 14, 2),
                    make(NotEquals, 17, 2),
                    make(PlusAssign, 20, 2),
                    make(MinusAssign, 23, 2),
                    make(MulAssign, 26, 2),
                    make(DivAssign, 29, 2),
                    make(PercentAssign, 32, 2),
                    make(AmpersandAssign, 35, 2),
                    make(BarAssign, 38, 2),
                    make(EOF, 40, 1),
                ],
            ),
            (
                r#""""#,
                &[
                    make(StringLiteral(vec![].into_boxed_slice()), 0, 2),
                    make(EOF, 2, 1),
                ],
            ),
            (
                r#""abc""#,
                &[
                    make(
                        StringLiteral(
                            vec![make_string_part(StrPart::Text("abc"), 1, 3)].into_boxed_slice(),
                        ),
                        0,
                        5,
                    ),
                    make(EOF, 5, 1),
                ],
            ),
        ]
    };

    for case in cases {
        let mut lex_state = LexState::init(file, case.0);
        let v = lex_state.lex_all();
        
        assert!(v.1.is_empty(), "diagnostic not empty: {} {:?}", case.0, v.1);
        assert_eq!(case.1, v.0.as_ref(), "{}", case.0);
    }
}
