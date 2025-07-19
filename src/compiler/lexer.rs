use std::str::Chars;

use crate::atoms::KwAtom;

use super::{
    span::Span,
    token::{Kind, Token, Value},
};

const VALID_STIRNG_ESCAPES: &[char] = &['n', 'r', 't', '"', '\'', '\\', 'u', 'x', '0'];

pub struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    start: usize,
    //
    // token_errors: Vec<(Span, LexerError)>,
}

pub trait Lex<'a> {
    fn read_next_token(&mut self) -> Token<'a>;
}

impl<'a> Lex<'a> for Lexer<'a> {
    fn read_next_token(&mut self) -> Token<'a> {
        self.start = self.offset();

        let (kind, value) = self.read_next();
        let end = self.offset();

        // let errors = std::mem::take(&mut self.token_errors);

        Token {
            span: Span::new(self.start, end),
            kind,
            value,
            // errors,
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars(),
            start: 0,
            // token_errors: Vec::new(),
        }
    }

    fn read_next(&mut self) -> (Kind, Value<'a>) {
        while let Some(c) = self.advance(1) {
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    self.start = self.offset();
                    continue;
                }

                // Line comment
                '/' if self.peek() == Some('/') => {
                    self.skip_line_comment();
                    continue;
                }

                /* Block comment */
                '/' if self.peek() == Some('*') => {
                    self.skip_block_comment();
                    continue;
                }

                // Literals
                '"' => return self.read_string_literal(),
                '\'' => return self.read_single_quote(),
                '$' => return self.read_label(),
                '0'..='9' => return self.read_numeric_literal(),

                '_' => match self.peek() {
                    Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => return self.read_identifier(),
                    _ => return (Kind::Underscore, Value::None),
                },

                '_' | 'a'..='z' | 'A'..='Z' => return self.read_identifier(),

                // Delimiters & operators
                ',' => return (Kind::Comma, Value::None),
                ';' => return (Kind::Semicolon, Value::None),
                '(' => return (Kind::LParen, Value::None),
                ')' => return (Kind::RParen, Value::None),
                '{' => return (Kind::LBrace, Value::None),
                '}' => return (Kind::RBrace, Value::None),
                '[' => return (Kind::LBracket, Value::None),
                ']' => return (Kind::RBracket, Value::None),
                '=' => match self.peek() {
                    Some('>') => {
                        self.advance(1);
                        return (Kind::FatArrow, Value::None);
                    }
                    _ => return (Kind::Eq, Value::None),
                },
                '!' => match self.peek() {
                    Some('=') => {
                        self.advance(1);
                        return (Kind::Neq, Value::None);
                    }
                    _ => return (Kind::Bang, Value::None),
                },

                '.' => match self.peek() {
                    // ..
                    Some('.') => {
                        self.advance(1);
                        match self.peek() {
                            // ..=
                            Some('=') => {
                                self.advance(1);
                                return (Kind::DotDotEq, Value::None);
                            }
                            // ...
                            Some('.') => {
                                self.advance(1);
                                return (Kind::DotDotDot, Value::None);
                            }
                            _ => return (Kind::DotDot, Value::None),
                        }
                    }
                    _ => return (Kind::Dot, Value::None),
                },
                '<' => match self.peek() {
                    // <>
                    Some('>') => {
                        self.advance(1);
                        match self.peek() {
                            // <>=
                            Some('=') => {
                                self.advance(1);
                                return (Kind::ConcatEq, Value::None);
                            }
                            _ => return (Kind::Concat, Value::None),
                        }
                    }
                    // <<
                    Some('<') => {
                        self.advance(1);
                        match self.peek() {
                            // <<=
                            Some('=') => {
                                self.advance(1);
                                return (Kind::LShiftEq, Value::None);
                            }
                            _ => return (Kind::LShift, Value::None),
                        }
                    }
                    // <=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::LessEquals, Value::None);
                    }
                    _ => return (Kind::LessThan, Value::None),
                },
                '>' => match self.peek() {
                    // >>
                    Some('>') => {
                        self.advance(1);
                        match self.peek() {
                            // >>=
                            Some('=') => {
                                self.advance(1);
                                return (Kind::RShiftEq, Value::None);
                            }
                            _ => return (Kind::RShift, Value::None),
                        }
                    }
                    // >=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::GreaterEquals, Value::None);
                    }
                    _ => return (Kind::GreaterThan, Value::None),
                },

                ':' => match self.peek() {
                    Some('=') => {
                        self.advance(1);
                        return (Kind::Assign, Value::None);
                    }
                    _ => return (Kind::PathSep, Value::None),
                },

                '+' => match self.peek() {
                    // +=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::AddEq, Value::None);
                    }
                    _ => return (Kind::Plus, Value::None),
                },

                '*' => match self.peek() {
                    // *=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::MulEq, Value::None);
                    }
                    _ => return (Kind::Asterisk, Value::None),
                },

                '/' => match self.peek() {
                    // /=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::DivEq, Value::None);
                    }
                    _ => return (Kind::Divide, Value::None),
                },

                '%' => match self.peek() {
                    // %=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::ModEq, Value::None);
                    }
                    _ => return (Kind::Modulus, Value::None),
                },

                '~' => return (Kind::BitNot, Value::None),

                '^' => match self.peek() {
                    // ^=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::BitXorEq, Value::None);
                    }
                    _ => return (Kind::BitXor, Value::None),
                },

                '|' => match self.peek() {
                    // |>
                    Some('>') => {
                        self.advance(1);
                        return (Kind::KwReturn, Value::None);
                    }
                    // ||
                    Some('|') => {
                        self.advance(1);
                        return (Kind::LogOr, Value::None);
                    }
                    // |=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::BitOrEq, Value::None);
                    }
                    _ => return (Kind::BitOr, Value::None),
                },

                '&' => match self.peek() {
                    // &&
                    Some('&') => {
                        self.advance(1);
                        return (Kind::LogAnd, Value::None);
                    }
                    // &=
                    Some('=') => {
                        self.advance(1);
                        return (Kind::BitAndEq, Value::None);
                    }
                    _ => return (Kind::Ampersand, Value::None),
                },

                '-' => match self.peek() {
                    Some('=') => {
                        self.advance(1);
                        return (Kind::SubEq, Value::None);
                    }
                    Some('>') => {
                        self.advance(1);
                        return (Kind::RArrow, Value::None);
                    }
                    _ => return (Kind::Minus, Value::None),
                },

                _ => {
                    return (Kind::Illegal, Value::None);
                }
            }
        }

        (Kind::Eof, Value::None)
    }

    fn read_numeric_literal(&mut self) -> (Kind, Value<'a>) {
        let start = self.offset() - 1;

        let mut is_float = false;

        while let Some(c) = self.peek() {
            match c {
                '.' => {
                    is_float = true;
                    self.advance(1);
                    self.read_decimals();
                    break;
                }
                '_' | '0'..='9' => {
                    self.advance(1);
                    continue;
                }
                'e' | 'E' => {
                    is_float = true;
                    self.advance(1);
                    self.read_exponent();
                    continue;
                }
                _ => break,
            }
        }

        let end = self.offset();

        let lexeme = &self.source[start..end].replace("_", "");

        if is_float {
            match lexeme.parse::<f64>() {
                Ok(value) => (Kind::FloatLiteral, Value::Float(value)),
                Err(_) => {
                    panic!()
                }
            }
        } else {
            match lexeme.parse::<i64>() {
                Ok(value) => (Kind::IntLiteral, Value::Int(value)),
                Err(_) => {
                    panic!()
                }
            }
        }
    }

    fn read_decimals(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {
                    self.advance(1);
                    continue;
                }
                'e' | 'E' => {
                    self.advance(1);
                    self.read_exponent();
                }
                _ => break,
            };
        }
    }

    fn read_exponent(&mut self) {
        let mut found_sign = false;

        while let Some(c) = self.peek() {
            match c {
                '-' | '+' if !found_sign => {
                    found_sign = true;
                    self.advance(1);
                }
                '-' | '+' => {
                    self.advance(1);
                    panic!()
                }
                '.' => {
                    self.advance(1);

                    panic!()
                }
                '0'..='9' => {
                    self.advance(1);
                    continue;
                }
                _ => break,
            }
        }
    }

    fn read_identifier(&mut self) -> (Kind, Value<'a>) {
        let start = self.offset() - 1;

        while let Some(c) = self.peek() {
            match c {
                '_' | 'a'..='z' | 'A'..='Z' | '0'..='9' => {
                    self.advance(1);
                    continue;
                }
                _ => break,
            }
        }

        let end = self.offset();
        let lexeme = KwAtom::from(&self.source[start..end]);

        if lexeme.len() < 2 || lexeme.len() > 8 {
            return (Kind::Identifier, Value::Ident(lexeme));
        }

        let kind = match lexeme {
            atom!("fn") => Kind::KwFn,
            atom!("as") => Kind::KwAs,
            atom!("const") => Kind::KwConst,
            atom!("continue") => Kind::KwContinue,
            atom!("break") => Kind::KwBreak,
            atom!("else") => Kind::KwElse,
            atom!("extern") => Kind::KwExtern,
            atom!("for") => Kind::KwFor,
            atom!("if") => Kind::KwIf,
            atom!("in") => Kind::KwIn,
            atom!("match") => Kind::KwMatch,
            atom!("pub") => Kind::KwPub,
            atom!("ref") => Kind::KwRef,
            atom!("while") => Kind::KwWhile,
            atom!("crate") => Kind::KwCrate,
            atom!("super") => Kind::KwSuper,
            atom!("self") => Kind::Kwself,
            atom!("Self") => Kind::KwSelf,
            atom!("mut") => Kind::KwMut,

            atom!("isz")
            | atom!("usz")
            | atom!("u8")
            | atom!("u16")
            | atom!("u32")
            | atom!("u64")
            | atom!("u128")
            | atom!("i8")
            | atom!("i16")
            | atom!("i32")
            | atom!("void")
            | atom!("i64")
            | atom!("i128")
            | atom!("char")
            | atom!("bool")
            | atom!("float")
            | atom!("double")
            | atom!("str")
            | atom!("infer") => Kind::PrimitiveType,

            atom!("true") | atom!("false") => Kind::BoolLiteral,

            _ => Kind::Identifier,
        };

        match kind {
            Kind::Identifier => (kind, Value::Ident(lexeme)),
            Kind::BoolLiteral => (
                kind,
                Value::Bool(match lexeme {
                    atom!("true") => true,
                    atom!("false") => false,
                    _ => unreachable!(),
                }),
            ),
            Kind::PrimitiveType => (kind, Value::Ident(lexeme)),
            _ => (kind, Value::None),
        }
    }

    fn read_label(&mut self) -> (Kind, Value<'a>) {
        self.advance(1);

        let (kind, value) = self.read_identifier();

        if !matches!(kind, Kind::Identifier) {
            panic!()
        }

        (Kind::Label, value)
    }

    fn read_single_quote(&mut self) -> (Kind, Value<'a>) {
        let start = self.offset() - 1;
        let mut terminated = false;

        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }

            match c {
                '\'' => {
                    self.advance(1);
                    terminated = true;
                    break;
                }
                '\\' => {
                    self.advance(1);

                    if self.advance(1).is_none() {
                        break;
                    }
                }
                _ => {
                    self.advance(1);
                }
            };
        }

        let end = self.offset();

        let lexeme = &self.source[start..end];
        let span = Span::new(start, end);

        if !terminated {
            panic!()
        }

        let inner = if terminated {
            &lexeme[1..lexeme.len() - 1]
        } else {
            &lexeme[1..]
        };

        self.find_invalid_escapes(inner, VALID_STIRNG_ESCAPES)
            .into_iter()
            .for_each(|err| {
                let byte_pos = start + 1 + err.0;
                let len = 1 + err.1.map(|_| 1).unwrap_or(0);
                let bad_span = Span::new(byte_pos, byte_pos + len);

                panic!()
            });

        match inner.parse::<char>() {
            Ok(inner) => (Kind::CharLiteral, Value::Char(inner)),
            Err(_) => {
                panic!()
            }
        }
    }

    fn read_string_literal(&mut self) -> (Kind, Value<'a>) {
        let start = self.offset() - 1;
        let mut terminated = false;

        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }

            self.advance(1);

            match c {
                '"' => {
                    terminated = true;
                    break;
                }
                '\\' => {
                    if self.advance(1).is_none() {
                        break;
                    }
                }
                _ => continue,
            }
        }

        let end = self.offset();
        let span = Span::new(start, end);

        let lexeme = &self.source[span.range()];

        if !terminated {
            // Emit error
            panic!()
        }

        let inner = if terminated {
            &lexeme[1..lexeme.len() - 1]
        } else {
            &lexeme[1..]
        };

        self.find_invalid_escapes(inner, VALID_STIRNG_ESCAPES)
            .into_iter()
            .for_each(|err| {
                let byte_pos = start + 1 + err.0;
                let len = 1 + err.1.map(|_| 1).unwrap_or(0);
                let bad_span = Span::new(byte_pos, byte_pos + len);

                panic!()
            });

        (Kind::StringLiteral, Value::String(inner))
    }

    fn find_invalid_escapes(
        &self,
        content: &str,
        valid_escapes: &[char],
    ) -> Vec<(usize, Option<char>)> {
        let mut errors = vec![];
        let mut iter = content.char_indices().peekable();

        while let Some((idx, ch)) = iter.next() {
            if ch == '\\' {
                if let Some(&(_, next_ch)) = iter.peek() {
                    if next_ch == ' ' {
                        errors.push((idx, None));
                    } else if !valid_escapes.contains(&next_ch) {
                        errors.push((idx, Some(next_ch)));
                    }

                    iter.next();
                } else {
                    errors.push((idx, None));
                }
            }
        }

        errors
    }

    fn skip_block_comment(&mut self) {
        self.chars.next(); // Consume leading asterisk

        let mut found_asterisk = false;

        for c in self.chars.by_ref() {
            match c {
                '/' if found_asterisk => break,
                '*' => found_asterisk = true,
                _ => found_asterisk = false,
            }
        }

        self.start = self.offset();
    }

    fn advance(&mut self, n: usize) -> Option<char> {
        self.chars.nth(n - 1)
    }

    fn skip_line_comment(&mut self) {
        self.chars.next(); // Consume leading slash

        while !matches!(self.peek(), Some('\n')) {
            self.advance(1);
        }

        self.start = self.offset();
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn offset(&self) -> usize {
        self.source.len() - self.chars.as_str().len()
    }
}
