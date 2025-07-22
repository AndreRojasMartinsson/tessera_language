use std::{path::Path, rc::Rc};

use bumpalo::collections::Vec;

use crate::{
    ast::Modifiers,
    atoms::KwAtom,
    hir::{
        Hir,
        expr::{HirType, QPath},
        identifiers::{ExprId, ItemId, StmtId},
    },
};

pub enum HirItem<'hir> {
    Func {
        name: KwAtom,
        params: Vec<'hir, (KwAtom, HirType<'hir>)>,
        ret: HirType<'hir>,
        body: StmtId,
        public: bool,
        constant: bool,
    },
    ExternFunc {
        name: KwAtom,
        params: Vec<'hir, (KwAtom, HirType<'hir>)>,
        ret: HirType<'hir>,
        constant: bool,
    },
    Const {
        name: KwAtom,
        ty: HirType<'hir>,
        value: ExprId,
    },
    Import {
        qualified_path: QPath<'hir>,
        file_path: Option<&'hir Path>,
        tree: Rc<Hir<'hir>>,
    },
    Module {
        name: KwAtom,
        items: Vec<'hir, ItemId>,
    },
}
