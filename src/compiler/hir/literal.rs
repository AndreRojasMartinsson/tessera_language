use std::rc::Rc;

use ordered_float::OrderedFloat;

use crate::{choice, node};

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Literal<'hir> {
    Str(StrLiteral<'hir>),
    Char(CharLiteral),
    Bool(BoolLiteral),
    Float(FloatLiteral),
    Int(IntLiteral),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct StrLiteral<'hir> {
    value: &'hir str,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct CharLiteral {
    value: char,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BoolLiteral {
    value: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FloatLiteral {
    value: OrderedFloat<f64>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct IntLiteral {
    value: i64,
}
