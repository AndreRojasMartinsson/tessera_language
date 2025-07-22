use std::hash::Hash;

use bumpalo::{Bump, collections::Vec};
use gxhash::HashMap;

use crate::hir::{
    expr::HirExpr,
    identifiers::{ExprId, ItemId, StmtId},
    item::HirItem,
    stmt::HirStmt,
};

mod expr;
mod identifiers;
mod item;
mod stmt;
mod top_level;
mod ty;

pub struct Hir<'hir> {
    pub exprs: Vec<'hir, HirExpr<'hir>>,
    pub stmts: Vec<'hir, HirStmt>,
    pub items: Vec<'hir, HirItem<'hir>>,
    arena: &'hir Bump,
}

impl<'hir> Hir<'hir> {
    pub fn new(arena: &'hir Bump) -> Self {
        Self {
            exprs: Vec::new_in(&arena),
            items: Vec::new_in(&arena),
            stmts: Vec::new_in(&arena),
            arena,
        }
    }

    pub fn alloc_expr(&mut self, expr: HirExpr<'hir>) -> ExprId {
        let idx = self.exprs.len();
        self.exprs.push(expr);
        ExprId(idx)
    }

    pub fn alloc_stmt(&mut self, stmt: HirStmt) -> StmtId {
        let idx = self.stmts.len();
        self.stmts.push(stmt);
        StmtId(idx)
    }

    pub fn alloc_item(&mut self, item: HirItem<'hir>) -> ItemId {
        let idx = self.items.len();
        self.items.push(item);
        ItemId(idx)
    }
}
