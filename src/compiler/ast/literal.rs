use crate::{choice, node};

choice!(Literal, {
    Str(Box<StrLiteral<'a>>),
    Char(Box<CharLiteral<'a>>),
    Bool(Box<BoolLiteral<'a>>),
    Float(Box<FloatLiteral<'a>>),
    Int(Box<IntLiteral<'a>>),
});

choice!(NegativeLiteral, {
    Int(Box<IntLiteral<'a>>),
    Float(Box<FloatLiteral<'a>>),
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
