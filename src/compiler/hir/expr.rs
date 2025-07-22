use bumpalo::collections::Vec;

use crate::{
    ast::{CompoundAssignmentExpr, Expr, Pattern},
    atoms::KwAtom,
    hir::{
        identifiers::{ExprId, StmtId},
        ty::HirType,
    },
    operator::{BinaryOperator, CompoundOperator, RangeOperator, UnaryOperator},
    span::Span,
};

pub enum HirExpr<'hir> {
    /// `a + 7`
    Binary {
        op: BinaryOperator,
        left: ExprId,
        right: ExprId,
    },
    /// `!a` or `~b`
    Unary {
        op: UnaryOperator,
        operand: ExprId,
    },
    /// Inclusive: `infer i := 'a' ..= 'z'`
    /// Exclusive: `infer e := 'a' .. 'z'`
    RangeFull {
        op: RangeOperator,
        from: ExprId,
        to: ExprId,
    },
    /// `infer a := 5..`
    RangeFrom {
        from: ExprId,
    },
    /// `infer a := (..5)`
    RangeTo {
        to: ExprId,
    },
    /// Desugared while loop and/or for loop
    /// Example: `while true {}` -> `loop {}`
    /// Example: `while a > 8 { if a > 15 { continue; } a += 1;}` -> `loop { if a <= 8 {break;} if
    /// a > 15 {continue;} a += 1; }`
    Loop {
        to: ExprId,
    },

    Match {
        value: ExprId,
        arms: Vec<'hir, MatchArm<'hir>>,
    },

    Literal(LiteralKind<'hir>),

    Path(QPath<'hir>),

    Assign {
        left: ExprId,
        right: ExprId,
    },

    CompoundAssign {
        left: ExprId,
        right: ExprId,
        op: CompoundOperator,
    },

    Call {
        callee: ExprId,
        args: Vec<'hir, ExprId>,
    },

    If {
        cond: ExprId,
        then_block: StmtId,
        else_block: Option<StmtId>,
    },

    Block {
        stmts: Vec<'hir, StmtId>,
        final_expr: Option<ExprId>,
    },

    TypeCast {
        ty: HirType<'hir>,
        operand: ExprId,
    },

    Return(Option<ExprId>),

    Break {
        label: Option<KwAtom>,
    },

    Continue {
        label: Option<KwAtom>,
    },
}

pub type QPath<'hir> = (Option<&'hir HirType<'hir>>, &'hir Path<'hir>);

pub struct Path<'hir, R = Res> {
    pub span: Span,
    pub res: R,
    pub segments: &'hir [PathSegment<'hir>],
}

pub struct PathSegment<'hir> {
    pub ident: Ident,
    pub res: Res,
    pub args: Option<&'hir GenericArgs<'hir>>,
    pub infer_args: bool,
}

pub struct GenericArgs<'hir> {
    pub args: &'hir [GenericArg<'hir>],
    pub span: Span,
}

pub enum GenericArg<'hir> {
    Type(&'hir HirType<'hir, AmbigArg>),
    Infer(InferArg),
}

pub enum AmbigArg {}

pub struct Ident {
    pub symbol: KwAtom,
}

pub struct InferArg {
    pub span: Span,
}

pub enum Res {
    Def(DefKind, StmtId),
    PrimTy(PrimTy),
    Local(ExprId),
    Err,
}

pub enum PrimTy {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
    Bool,
    Char,
}
pub enum IntTy {
    Isz,
    I8,
    I16,
    I32,
    I64,
    I128,
}

pub enum UintTy {
    Usz,
    U8,
    U16,
    U32,
    U64,
    U128,
}

pub enum FloatTy {
    Float,
    Double,
}

pub enum DefKind {
    Module,
    // TODO
    TyAlias,
    TyParam,
    Fn,
    Const,
    Using,
    Field,
    Closure,
}

pub struct MatchArm<'hir> {
    pub pat: Pattern<'hir>,
    pub guard: Option<ExprId>,
    pub expr: ExprId,
}

pub enum LiteralKind<'hir> {
    Str(&'hir str),
    Ident(KwAtom),
    Char(char),
    Float(f64),
    Int(i64),
    Bool(bool),
}
