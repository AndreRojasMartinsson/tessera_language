#![allow(clippy::inconsistent_digit_grouping)]

use crate::lexer::*;

macro_rules! assert_lex {
    ($lexer:ident, $kind:expr) => {
        let token = $lexer.read_next_token();
        pretty_assertions::assert_eq!(token.kind, $kind);
    };

    ($lexer:ident, $kind:expr, $value:expr) => {
        let token = $lexer.read_next_token();
        pretty_assertions::assert_eq!((token.kind, token.value), ($kind, $value));
    };
}

macro_rules! assert_lex_panic {
    ($lexer:ident) => {
        let token = std::panic::catch_unwind(move || $lexer.read_next_token());
        pretty_assertions::assert_eq!(token.is_err(), true);
    };
}

#[cfg(test)]
mod tests {
    use crate::token::{Kind, Token, Value};

    use super::*;

    #[test]
    fn test_booleans() {
        let src = "true false; bool a := true";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::BoolLiteral, Value::Bool(true));
        assert_lex!(lex, Kind::BoolLiteral, Value::Bool(false));
        assert_lex!(lex, Kind::Semicolon);
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident(atom!("bool")));

        lex.read_next_token();
        lex.read_next_token();

        assert_lex!(lex, Kind::BoolLiteral, Value::Bool(true));
    }

    #[test]
    fn test_strings() {
        let src = r#""Hello, world!"; "Enter \x20 something\n" "Blud"#;

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::StringLiteral, Value::String("Hello, world!"));

        lex.read_next_token();

        assert_lex!(
            lex,
            Kind::StringLiteral,
            Value::String("Enter \\x20 something\\n")
        );

        assert_lex_panic!(lex);
    }

    #[test]
    fn test_floats() {
        let src = "5. 8. 20.539 0.49 0.5e2 5e9 0.2e-2 0.7e+3 2_500_10e2";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::FloatLiteral, Value::Float(5.));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(8.));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(20.539));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(0.49));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(0.5e2));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(5e9));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(0.2e-2));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(0.7e+3));
        assert_lex!(lex, Kind::FloatLiteral, Value::Float(2_500_10e2));
    }

    #[test]
    fn test_ints() {
        let src = "5 6498 39 5_000_500";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::IntLiteral, Value::Int(5));
        assert_lex!(lex, Kind::IntLiteral, Value::Int(6498));
        assert_lex!(lex, Kind::IntLiteral, Value::Int(39));
        assert_lex!(lex, Kind::IntLiteral, Value::Int(5_000_500));
    }

    #[test]
    fn test_chars() {
        let src = "'a' 'b' '\\x20'";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::CharLiteral, Value::Char('a'));
        assert_lex!(lex, Kind::CharLiteral, Value::Char('b'));
        // TODO
        assert_lex!(lex, Kind::CharLiteral, Value::Char('\x20'));
    }

    #[test]
    fn test_operators() {
        let src = "+ - ^ & | ~ ! % * / << >> < > >= <= = != || && .. ..= ... :=";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::Plus);
        assert_lex!(lex, Kind::Minus);
        assert_lex!(lex, Kind::BitXor);
        assert_lex!(lex, Kind::Ampersand);
        assert_lex!(lex, Kind::BitOr);
        assert_lex!(lex, Kind::BitNot);
        assert_lex!(lex, Kind::Bang);
        assert_lex!(lex, Kind::Modulus);
        assert_lex!(lex, Kind::Asterisk);
        assert_lex!(lex, Kind::Divide);
        assert_lex!(lex, Kind::LShift);
        assert_lex!(lex, Kind::RShift);
        assert_lex!(lex, Kind::LessThan);
        assert_lex!(lex, Kind::GreaterThan);
        assert_lex!(lex, Kind::GreaterEquals);
        assert_lex!(lex, Kind::LessEquals);
        assert_lex!(lex, Kind::Eq);
        assert_lex!(lex, Kind::Neq);
        assert_lex!(lex, Kind::LogOr);
        assert_lex!(lex, Kind::LogAnd);
        assert_lex!(lex, Kind::DotDot);
        assert_lex!(lex, Kind::DotDotEq);
        assert_lex!(lex, Kind::DotDotDot);
        assert_lex!(lex, Kind::Assign);
    }

    #[test]
    fn test_compound_operators() {
        let src = "+= -= ^= &= |= %= *= /= <<= >>=";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::AddEq);
        assert_lex!(lex, Kind::SubEq);
        assert_lex!(lex, Kind::BitXorEq);
        assert_lex!(lex, Kind::BitAndEq);
        assert_lex!(lex, Kind::BitOrEq);
        assert_lex!(lex, Kind::ModEq);
        assert_lex!(lex, Kind::MulEq);
        assert_lex!(lex, Kind::DivEq);
        assert_lex!(lex, Kind::LShiftEq);
        assert_lex!(lex, Kind::RShiftEq);
    }

    #[test]
    fn test_delimiters() {
        let src = "{}()[].:;->=>$label:";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::LBrace);
        assert_lex!(lex, Kind::RBrace);
        assert_lex!(lex, Kind::LParen);
        assert_lex!(lex, Kind::RParen);
        assert_lex!(lex, Kind::LBracket);
        assert_lex!(lex, Kind::RBracket);
        assert_lex!(lex, Kind::Dot);
        assert_lex!(lex, Kind::PathSep);
        assert_lex!(lex, Kind::Semicolon);
        assert_lex!(lex, Kind::RArrow);
        assert_lex!(lex, Kind::FatArrow);
        assert_lex!(lex, Kind::Label, Value::Ident("label".into()));
    }

    #[test]
    fn test_keywords() {
        let src = "
            fn as const continue else break extern for if in match
            pub ref while crate super self Self mut |>";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::KwFn);
        assert_lex!(lex, Kind::KwAs);
        assert_lex!(lex, Kind::KwConst);
        assert_lex!(lex, Kind::KwContinue);
        assert_lex!(lex, Kind::KwElse);
        assert_lex!(lex, Kind::KwBreak);
        assert_lex!(lex, Kind::KwExtern);
        assert_lex!(lex, Kind::KwFor);
        assert_lex!(lex, Kind::KwIf);
        assert_lex!(lex, Kind::KwIn);
        assert_lex!(lex, Kind::KwMatch);
        assert_lex!(lex, Kind::KwPub);
        assert_lex!(lex, Kind::KwRef);
        assert_lex!(lex, Kind::KwWhile);
        assert_lex!(lex, Kind::KwCrate);
        assert_lex!(lex, Kind::KwSuper);
        assert_lex!(lex, Kind::Kwself);
        assert_lex!(lex, Kind::KwSelf);
        assert_lex!(lex, Kind::KwMut);
        assert_lex!(lex, Kind::KwReturn);
    }

    #[test]
    fn test_types() {
        let src = "
            isz usz i8 i16 i32 i64 i128 u8 u16 u32
            u64 u128 char bool infer str float double
            ";

        let mut lex = Lexer::new(src);

        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("isz".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("usz".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("i8".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("i16".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("i32".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("i64".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("i128".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("u8".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("u16".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("u32".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("u64".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("u128".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("char".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("bool".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("infer".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("str".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("float".into()));
        assert_lex!(lex, Kind::PrimitiveType, Value::Ident("double".into()));
    }
}
