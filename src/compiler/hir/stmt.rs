use crate::{
    atoms::KwAtom,
    hir::{identifiers::ExprId, ty::HirType},
};

pub enum HirStmt<'hir> {
    Var {
        name: KwAtom,
        ty: HirType<'hir>,
        init: Option<ExprId>,
        mutable: bool,
    },
    Expr {
        expr: ExprId,
    },
    Semi,
}
