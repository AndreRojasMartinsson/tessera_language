use crate::hir::types::*;
use crate::hir::utils::*;
use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Item<'hir> {
    pub hir_id: HirId,
    pub kind: ItemKind<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ItemKind<'hir> {
    Using(&'hir ImportPath<'hir>, ImportKind),
    Const(Ident, &'hir Ty<'hir>, BodyId),
    Fn {
        sig: FnSig<'hir>,
        ident: Ident,
        generics: &'hir GenericArgs<'hir>,
        body: BodyId,
        has_body: bool,
    },
    TyAlias(Ident, &'hir Ty<'hir>),
}

impl ItemKind<'_> {
    pub fn ident(&self) -> Option<Ident> {
        match self.clone() {
            ItemKind::Using(_, ImportKind::Single(ident))
            | ItemKind::Const(ident, ..)
            | ItemKind::Fn { ident, .. }
            | ItemKind::TyAlias(ident, ..) => Some(ident),

            ItemKind::Using(_, ImportKind::Glob) => None,
        }
    }

    pub fn generics(&self) -> Option<&GenericArgs<'_>> {
        Some(match self {
            ItemKind::Fn { generics, .. } => generics,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FnSig<'hir> {
    pub constness: bool,
    pub decl: &'hir FnDecl<'hir>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FnDecl<'hir> {
    pub inputs: &'hir [Ty<'hir>],
    pub output: FnRetTy<'hir>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ImportPath<'hir> {
    pub span: Span,
    pub res: PerNS<Option<Res>>,
    pub segments: &'hir [PathSegment<'hir>],
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct PathSegment<'hir> {
    pub ident: Ident,
    pub hir_id: HirId,
    pub res: Res,
    pub args: Option<&'hir GenericArgs<'hir>>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Path<'hir, R = Res> {
    pub span: Span,
    pub res: R,
    pub segments: &'hir [PathSegment<'hir>],
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ImportKind {
    Single(Ident),
    Glob,
}
