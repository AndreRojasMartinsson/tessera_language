use std::marker::PhantomData;

use crate::{
    hir::utils::{AmbigArg, HirId},
    span::Span,
};

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Ty<'hir, Unambig = ()> {
    pub hir_id: HirId,
    pub span: Span,
    pub kind: TyKind<'hir, Unambig>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum TyKind<'hir, Unambig = ()> {
    // Array(&'hir Ty<'hir>, &'hir ConstArg<'hir>),
    // FnPtr(&'hir FnPtrTy<'hir>),
    // UnsafeBinder(&'hir UnsafeBinderTy<'hir>),
    // Path(QPath<'hir>),
    // Err(ErrorGuaranteed),
    Never,
    Ref(MutTy<'hir>),
    Infer(Unambig),
    Marker(PhantomData<&'hir str>),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct MutTy<'hir> {
    pub ty: &'hir Ty<'hir>,
    pub mutbl: Mutability,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum ByRef {
    Yes(Mutability),
    No,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct GenericArgs<'hir> {
    pub args: &'hir [GenericArg<'hir>],
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum GenericArg<'hir> {
    Type(&'hir Ty<'hir, AmbigArg>),
    Infer(InferArg),
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InferArg {
    pub hir_id: HirId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PrimTy {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
    Bool,
    Char,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum FnRetTy<'hir> {
    DefaultReturn(Span),
    Return(&'hir Ty<'hir>),
}
