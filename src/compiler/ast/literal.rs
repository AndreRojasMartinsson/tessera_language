use crate::{choice, node};

use bumpalo::boxed::Box;

choice!(Literal, {
    Str(Box<'a, StrLiteral<'a>>),
    Char(Box<'a, CharLiteral<'a>>),
    Bool(Box<'a, BoolLiteral<'a>>),
    Float(Box<'a, FloatLiteral<'a>>),
    Int(Box<'a, IntLiteral<'a>>),
});

choice!(NegativeLiteral, {
    Int(Box<'a, IntLiteral<'a>>),
    Float(Box<'a, FloatLiteral<'a>>),
});

node!(StrLiteral, {
    value: &'a str,
});

node!(CharLiteral, {
    value: char,
});

node!(BoolLiteral, {
    value: bool,
});

node!(FloatLiteral, {
    value: f64,
});

node!(IntLiteral, {
    value: i64,
});
