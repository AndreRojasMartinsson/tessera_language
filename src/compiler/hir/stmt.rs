use crate::{
    hir::{
        expr::{Block, Expr},
        item::Path,
        types::Ty,
        utils::HirId,
    },
    span::Span,
};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Stmt<'hir> {
    pub hir_id: HirId,
    pub kind: StmtKind<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum StmtKind<'hir> {
    Var(&'hir VarStmt<'hir>),
    Item(HirId),
    Expr(&'hir Expr<'hir>),
    Semi(&'hir Expr<'hir>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct VarStmt<'hir> {
    pub path: &'hir Path<'hir>,
    pub ty: Option<&'hir Ty<'hir>>,
    pub init: Option<&'hir Expr<'hir>>,
    pub els: Option<&'hir Block<'hir>>,
    pub hir_id: HirId,
    pub span: Span,
}
