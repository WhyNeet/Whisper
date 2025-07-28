#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub start: usize,
    pub end: usize,
    pub kind: TokenKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    /// A line comment, e.g. `// comment`.
    LineComment,

    /// A block comment, e.g. `/* block comment */`.
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment { terminated: bool },

    /// An identifier.
    Ident,

    /// A keyword
    Keyword(Keyword),

    /// Literals, e.g. `12u8`, `1.0e-40`, `b"123"`. Note that `_` is an invalid
    /// suffix, but may be present here on string and float literals. Users of
    /// this type will need to check for and reject that case.
    ///
    /// See [LiteralKind] for more details.
    Literal { kind: LiteralKind },

    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `@`
    At,
    /// `#`
    Pound,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `::`
    ColonColon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `==`
    EqEq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `&&`
    AndAnd,
    /// `|`
    Or,
    /// `||`
    OrOr,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,

    /// ->
    RArrow,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// `12_u8`, `0o100`, `0b120i99`, `1f32`.
    Int {
        base: Base,
        empty_int: bool,
    },
    /// `12.34f32`, `1e3`, but not `1f32`.
    Float {
        base: Base,
        empty_exponent: bool,
    },
    /// `'a'`, `'\\'`, `'''`, `';`
    Char {
        terminated: bool,
    },
    /// `"abc"`, `"abc`
    Str {
        terminated: bool,
    },
    Bool,
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0b".
    Binary = 2,
    /// Literal starts with "0o".
    Octal = 8,
    /// Literal doesn't contain a prefix.
    Decimal = 10,
    /// Literal starts with "0x".
    Hexadecimal = 16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Fn,
    Mut,
    Struct,
    Pub,
    Default,
    Impl,
    Extern,
    Namespace,
    Import,
}

impl TryFrom<&str> for Keyword {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "fn" => Ok(Self::Fn),
            "let" => Ok(Self::Let),
            "mut" => Ok(Self::Mut),
            "struct" => Ok(Self::Struct),
            "pub" => Ok(Self::Pub),
            "default" => Ok(Self::Default),
            "impl" => Ok(Self::Impl),
            "extern" => Ok(Self::Extern),
            "namespace" => Ok(Self::Namespace),
            "import" => Ok(Self::Import),
            _ => Err("not a keyword."),
        }
    }
}
