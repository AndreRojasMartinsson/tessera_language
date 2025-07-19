use crate::{
    hir::{
        item::{Path, PathSegment},
        literal::Literal,
        stmt::Stmt,
        types::{ByRef, Mutability, Ty},
        utils::{HirId, Ident},
    },
    operator::{BinaryOperator, CompoundOperator, UnaryOperator},
    span::Span,
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Expr<'hir> {
    pub hir_id: HirId,
    pub kind: ExprKind<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ExprKind<'hir> {
    // Array(&'hir [Expr<'hir>]),
    // MethodCall(&'hir PathSegment<'hir>, &'hir Expr<'hir>, &'hir [Expr<'hir>], Span),
    // Tup(&'hir [Expr<'hir>]),
    Call(&'hir Expr<'hir>, &'hir [Expr<'hir>]),
    Using(&'hir Expr<'hir>, Span),
    Binary(BinaryOperator, &'hir Expr<'hir>, &'hir Expr<'hir>),
    Unary(UnaryOperator, &'hir Expr<'hir>),
    Lit(Literal<'hir>),
    Cast(&'hir Expr<'hir>, &'hir Ty<'hir>),
    Type(&'hir Expr<'hir>, &'hir Ty<'hir>),
    If(&'hir Expr<'hir>, &'hir Expr<'hir>, Option<&'hir Expr<'hir>>),
    Loop(&'hir Block<'hir>, Option<Label>, LoopSource, Span),
    Match(&'hir Expr<'hir>, &'hir [Arm<'hir>], MatchSource),
    // Closure(&'hir Closure<'hir>),
    Block(&'hir Block<'hir>, Option<Label>),
    Assign(&'hir Expr<'hir>, &'hir Expr<'hir>, Span),
    CompoundAssign(CompoundOperator, &'hir Expr<'hir>, &'hir Expr<'hir>),
    // Field(&'hir Expr<'hir>, Ident),
    // Index(&'hir Expr<'hir>, &'hir Expr<'hir>, Span),
    Path(QPath<'hir>),
    Ref(Mutability, &'hir Expr<'hir>),
    Break(Destination, Option<&'hir Expr<'hir>>),
    Continue(Destination),
    Return(Option<&'hir Expr<'hir>>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Destination {
    pub label: Option<Label>,
    pub target_id: Result<HirId, LoopIdError>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum LoopIdError {
    OutsideLoopScope,
    UnlabeledCfInWhileCondition,
    UnresolvedLabel,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Arm<'hir> {
    pub hir_id: HirId,
    pub span: Span,
    pub pat: &'hir Pat<'hir>,
    pub guard: Option<&'hir Expr<'hir>>,
    pub body: &'hir Expr<'hir>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Pat<'hir> {
    pub hir_id: HirId,
    pub kind: PatKind<'hir>,
    pub span: Span,
    pub default_binding_modes: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PatKind<'hir> {
    Missing,
    Wild,
    Binding(BindingMode, HirId, Ident, Option<&'hir Pat<'hir>>),
    Or(&'hir [Pat<'hir>]),
    Never,
    Box(&'hir Pat<'hir>),
    Deref(&'hir Pat<'hir>),
    Ref(&'hir Pat<'hir>, Mutability),
    Expr(&'hir PatExpr<'hir>),
    Guard(&'hir Pat<'hir>, &'hir Expr<'hir>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct PatExpr<'hir> {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: PatExprKind<'hir>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PatExprKind<'hir> {
    Lit { lit: Literal<'hir>, negated: bool },
    Path(QPath<'hir>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BindingMode(pub ByRef, pub Mutability);

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum QPath<'hir> {
    Resolved(Option<&'hir Ty<'hir>>, &'hir Path<'hir>),
    TypeRelative(&'hir Ty<'hir>, &'hir PathSegment<'hir>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Block<'hir> {
    pub stmts: &'hir [Stmt<'hir>],
    pub expr: Option<&'hir Expr<'hir>>,
    pub hir_id: HirId,
    pub span: Span,
    pub targeted_by_break: bool,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Label {
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum LoopSource {
    Loop,
    While,
    ForLoop,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum MatchSource {
    Normal,
    Postfix,
    ForLoopDesugar,
    TryDesugar(HirId),
}
