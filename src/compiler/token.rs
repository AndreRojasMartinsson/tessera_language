use crate::atoms::KwAtom;

use super::span::Span;

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub struct Token<'a> {
    pub span: Span,
    pub kind: Kind,
    pub value: Value<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Kind {
    // Keywords
    KwAs,
    KwBreak,
    KwConst,
    KwContinue,
    KwElse,
    KwExtern,
    KwFn,
    KwFor,
    KwIf,
    KwIn,
    KwMatch,
    KwPub,
    KwRef,
    KwReturn,
    KwWhile,
    KwCrate,
    KwSuper,
    KwMut,
    KwSelf,
    Kwself,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    Underscore,
    RBrace,
    LBracket,
    Label,
    RBracket,
    Comma,
    PathSep,
    Dot,
    Semicolon,
    RArrow,
    Assign,
    FatArrow,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Divide,
    Modulus,
    Concat,
    LShift,
    RShift,
    Eq,
    Neq,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,
    BitXor,
    BitOr,
    BitNot,
    Ampersand,
    LogOr,
    LogAnd,
    Bang,
    DotDot,
    DotDotDot,
    DotDotEq,

    // Compound operators
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    ConcatEq,
    LShiftEq,
    RShiftEq,
    BitXorEq,
    BitOrEq,
    BitAndEq,

    // Literals
    IntLiteral,
    StringLiteral,
    CharLiteral,
    FloatLiteral,
    BoolLiteral,

    // Misc
    PrimitiveType,
    Identifier,
    Eof,
    #[default]
    Illegal,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub enum Value<'a> {
    Int(i64),
    Float(f64),
    String(&'a str),
    Ident(KwAtom),
    Bool(bool),
    Char(char),

    #[default]
    None,
}

impl TryFrom<f64> for Value<'_> {
    type Error = ();

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        Ok(Self::Float(value))
    }
}

impl TryFrom<i64> for Value<'_> {
    type Error = ();

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        Ok(Self::Int(value))
    }
}

impl TryFrom<KwAtom> for Value<'_> {
    type Error = ();

    fn try_from(value: KwAtom) -> Result<Self, Self::Error> {
        Ok(Self::Ident(value))
    }
}

impl TryFrom<bool> for Value<'_> {
    type Error = ();

    fn try_from(value: bool) -> Result<Self, Self::Error> {
        Ok(Self::Bool(value))
    }
}

impl TryFrom<char> for Value<'_> {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(Self::Char(value))
    }
}
