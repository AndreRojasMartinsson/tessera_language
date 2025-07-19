use crate::atoms::KwAtom;
use crate::hir::types::PrimTy;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct HirId(u64);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BodyId {
    pub hir_id: HirId,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DefId {
    pub index: u64,
    pub krate: u64,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum AmbigArg {}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct PerNS<T> {
    pub value_ns: T,
    pub type_ns: T,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Res<Id = HirId> {
    Def(DefKind, DefId),
    PrimTy(PrimTy),
    Local(Id),
    SelfTyAlias, // The `Self` type
    Err,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum DefKind {
    Mod,
    TyAlias,
    ForeignTy,
    TyParam,
    Fn,
    Const,
    Using,
    Field,
    OpaqueTy,
    Closure,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Ident {
    pub symbol: KwAtom,
    pub span: Span,
}
