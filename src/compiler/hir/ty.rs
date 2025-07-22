use crate::hir::expr::QPath;

#[repr(C)]
pub enum HirType<'hir, Unambig = ()> {
    Array(&'hir HirType<'hir>),
    Ref(MutTy<'hir>),
    Path(QPath<'hir>),
    Infer(Unambig),
    Never,
    Void,
    Err,
}

pub struct MutTy<'hir> {
    pub mutable: bool,
    pub ty: &'hir HirType<'hir>,
}
