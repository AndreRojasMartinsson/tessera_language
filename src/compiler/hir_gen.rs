use crate::hir::Node;
use crate::hir::scope::ScopeBuilder;

use crate::ast::*;

pub struct HirGen<'a, 'hir> {
    scope_builder: ScopeBuilder,
}

impl HirGen<'_, '_> {
    pub fn new() -> Self {
        Self {
            scope_builder: ScopeBuilder::new(),
        }
    }
}

impl<'a, 'hir> visitor::AstVisitor<'a, ()> for HirGen<'a, 'hir> {}
