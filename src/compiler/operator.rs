use clap::ValueEnum;

use super::token::Kind;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Concat,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Neq,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    LShift,
    RShift,
}

impl TryFrom<Kind> for BinaryOperator {
    type Error = ();

    fn try_from(value: Kind) -> Result<Self, Self::Error> {
        match value {
            Kind::Plus => Ok(Self::Add),
            Kind::Minus => Ok(Self::Subtract),
            Kind::Asterisk => Ok(Self::Multiply),
            Kind::Divide => Ok(Self::Divide),
            Kind::Modulus => Ok(Self::Remainder),
            Kind::Concat => Ok(Self::Concat),
            Kind::LShift => Ok(Self::LShift),
            Kind::RShift => Ok(Self::RShift),
            Kind::Eq => Ok(Self::Eq),
            Kind::Neq => Ok(Self::Neq),
            Kind::LessThan => Ok(Self::LessThan),
            Kind::GreaterThan => Ok(Self::GreaterThan),
            Kind::LessEquals => Ok(Self::LessEqual),
            Kind::GreaterEquals => Ok(Self::GreaterEqual),
            Kind::BitXor => Ok(Self::BitXor),
            Kind::BitOr => Ok(Self::BitOr),
            Kind::Ampersand => Ok(Self::BitAnd),
            Kind::LogOr => Ok(Self::Or),
            Kind::LogAnd => Ok(Self::And),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CompoundOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Concat,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
}

impl TryFrom<Kind> for CompoundOperator {
    type Error = ();

    fn try_from(value: Kind) -> Result<Self, Self::Error> {
        match value {
            Kind::AddEq => Ok(Self::Add),
            Kind::SubEq => Ok(Self::Subtract),
            Kind::MulEq => Ok(Self::Multiply),
            Kind::DivEq => Ok(Self::Divide),
            Kind::ModEq => Ok(Self::Remainder),
            Kind::ConcatEq => Ok(Self::Concat),
            Kind::LShiftEq => Ok(Self::LShift),
            Kind::RShiftEq => Ok(Self::RShift),
            Kind::BitXorEq => Ok(Self::BitXor),
            Kind::BitOrEq => Ok(Self::BitOr),
            Kind::BitAndEq => Ok(Self::BitAnd),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RangeOperator {
    Exclusive,
    Inclusive,
}

impl TryFrom<Kind> for RangeOperator {
    type Error = ();

    fn try_from(value: Kind) -> Result<Self, Self::Error> {
        match value {
            Kind::DotDot => Ok(Self::Exclusive),
            Kind::DotDotEq => Ok(Self::Inclusive),

            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Negate,
    Deref,
    Not,
    BitNot,
}

impl From<Kind> for UnaryOperator {
    fn from(value: Kind) -> Self {
        match value {
            Kind::Asterisk => Self::Deref,
            Kind::Minus => Self::Negate,
            Kind::Bang => Self::Not,
            Kind::BitNot => Self::BitNot,

            c => unreachable!("{c:?}"),
        }
    }
}

#[repr(i8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Precedence {
    Call = 16,
    Field = 15,
    Reference = 14,
    Unary = 13,
    Cast = 12,
    Multiplicative = 11,
    Additive = 10,
    Concat = 9,
    Shift = 8,
    BitAnd = 7,
    BitXor = 6,
    BitOr = 5,
    Comparative = 4,
    And = 3,
    Or = 2,
    Range = 1,
    Assign = 0,
    Closure = -1,
}

impl Precedence {
    #[inline(always)]
    pub fn value(self) -> i8 {
        self as i8
    }
}

impl Ord for Precedence {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (*self as i8).cmp(&(*other as i8))
    }

    #[inline(always)]
    fn max(self, other: Self) -> Self {
        if self >= other { self } else { other }
    }

    #[inline(always)]
    fn min(self, other: Self) -> Self {
        if self <= other { self } else { other }
    }
}

impl PartialOrd for Precedence {
    #[inline(always)]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<CompoundOperator> for Precedence {
    #[inline(always)]
    fn from(value: CompoundOperator) -> Self {
        Self::Assign
    }
}

impl From<BinaryOperator> for Precedence {
    #[inline(always)]
    fn from(value: BinaryOperator) -> Self {
        match value {
            BinaryOperator::Add | BinaryOperator::Subtract => Self::Additive,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder => {
                Self::Multiplicative
            }
            BinaryOperator::Concat => Self::Concat,
            BinaryOperator::And => Self::And,
            BinaryOperator::Or => Self::Or,
            BinaryOperator::BitAnd => Self::BitAnd,
            BinaryOperator::BitOr => Self::BitOr,
            BinaryOperator::BitXor => Self::BitXor,
            BinaryOperator::Eq
            | BinaryOperator::Neq
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual => Self::Comparative,
            BinaryOperator::LShift | BinaryOperator::RShift => Self::Shift,
        }
    }
}

impl From<UnaryOperator> for Precedence {
    #[inline(always)]
    fn from(value: UnaryOperator) -> Self {
        Self::Unary
    }
}

impl From<RangeOperator> for Precedence {
    #[inline(always)]
    fn from(value: RangeOperator) -> Self {
        Self::Range
    }
}
