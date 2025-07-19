use std::marker::PhantomData;

use crate::{
    atoms::KwAtom,
    hir::{expr::*, item::*, stmt::*, types::*, utils::*},
    span::Span,
};

pub mod expr;
pub mod item;
pub mod literal;
pub mod scope;
pub mod stmt;
pub mod types;
pub mod utils;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Node<'hir> {
    Param(&'hir Param<'hir>),
    Item(&'hir Item<'hir>),
    Expr(&'hir Expr<'hir>),
    Stmt(&'hir Stmt<'hir>),
    PathSegment(&'hir PathSegment<'hir>),
    Ty(&'hir Ty<'hir>),
    Pat(&'hir Pat<'hir>),
    PatExpr(&'hir PatExpr<'hir>),
    Arm(&'hir Arm<'hir>),
    Block(&'hir Block<'hir>),
    VarStmt(&'hir VarStmt<'hir>),
    Infer(&'hir InferArg),
    Synthetic,
    Err(Span),
}

impl<'hir> Node<'hir> {
    pub fn ident(&self) -> Option<Ident> {
        match self {
            Self::Item(item) => item.kind.ident(),
            Node::PathSegment(PathSegment { ident, .. }) => Some(ident.clone()),

            Node::Param(..)
            | Node::Expr(..)
            | Node::Stmt(..)
            | Node::Block(..)
            | Node::Pat(..)
            | Node::PatExpr(..)
            | Node::Arm(..)
            | Node::VarStmt(..)
            | Node::Ty(..)
            | Node::Infer(..)
            | Node::Synthetic
            | Node::Err(..) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Param<'hir> {
    pub hir_id: HirId,
    pub pat: &'hir Pat<'hir>,
    pub ty_span: Span,
    pub span: Span,
}
