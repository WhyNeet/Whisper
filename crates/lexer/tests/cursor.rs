use lexer::token::{Base, LiteralKind, Token, TokenKind};

#[test]
pub fn cursor_works() {
    let input = "2 * 32 + 4";
    let mut stream = lexer::tokenize(input);

    assert_eq!(
        stream.next(),
        Some(Token {
            start: 0,
            end: 1,
            kind: TokenKind::Literal {
                kind: LiteralKind::Int {
                    base: Base::Decimal,
                    empty_int: false
                }
            }
        })
    );

    assert_eq!(
        stream.next(),
        Some(Token {
            start: 2,
            end: 3,
            kind: TokenKind::Star
        })
    );

    assert_eq!(
        stream.next(),
        Some(Token {
            start: 4,
            end: 6,
            kind: TokenKind::Literal {
                kind: LiteralKind::Int {
                    base: Base::Decimal,
                    empty_int: false
                }
            }
        })
    );

    assert_eq!(
        stream.next(),
        Some(Token {
            start: 7,
            end: 8,
            kind: TokenKind::Plus
        })
    );

    assert_eq!(
        stream.next(),
        Some(Token {
            start: 9,
            end: 10,
            kind: TokenKind::Literal {
                kind: LiteralKind::Int {
                    base: Base::Decimal,
                    empty_int: false
                }
            }
        })
    );
}
