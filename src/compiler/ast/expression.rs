use bitflags::bitflags;
use clap::Id;

use bumpalo::boxed::Box;
use bumpalo::collections::Vec;

use crate::{
    atoms::KwAtom,
    choice, node,
    operator::{BinaryOperator, CompoundOperator, RangeOperator, UnaryOperator},
    span::Span,
};

use super::{
    literal::{
        BoolLiteral, CharLiteral, FloatLiteral, IntLiteral, Literal, NegativeLiteral, StrLiteral,
    },
    utils::Punctuated,
};

bitflags! {
    #[derive(Debug, PartialEq, PartialOrd, Clone)]
    pub struct Modifiers: u8 {
        const MUTABLE = 1;
        const CONST = 1 << 1;
        const PUB = 1 << 2;
        const REFERENCE = 1 << 3;
    }
}

node!(SourceFile, {
    stmts: Vec<'a, Stmt<'a>>,
});

choice!(Stmt, {
    Decl(DeclStmt<'a>),
    Expr(ExprStmt<'a>),
    Empty(Span),
});

choice!(DeclStmt, {
    Const(Box<'a, ConstItem<'a>>),
    Empty(Box<'a, EmptyStmt<'a>>),
    Import(Box<'a, ImportItem<'a>>),
    Func(Box<'a, FuncItem<'a>>),
    Extern(Box<'a, ExternFunc<'a>>),
    Var(Box<'a, VarItem<'a>>),
});

node!(ImportItem, {
    tree: ImportTree<'a>,
});

choice!(ImportTree, {
    Path(Box<'a, ImportPath<'a>>),
    Group(Box<'a, ImportGroup<'a>>),
    Name(Box<'a, Identifier<'a>>),
    Glob
});

node!(ImportGroup, {
    items: Punctuated<'a, ImportTree<'a>>
});

node!(ImportPath, {
    ident: Identifier<'a>,
    tree: ImportTree<'a>,
});

node!(EmptyStmt, {});

node!(ConstItem, {
    modifiers: Modifiers,
    ty: Type<'a>,
    name: Identifier<'a>,
    value: Expr<'a>,
});

node!(ExternFunc, {
    modifiers: Modifiers,
    generic_args: Option<GenericArgs<'a>>,
    return_ty: Type<'a>,
    name: Identifier<'a>,
    parameters: Punctuated<'a, Parameter<'a>>,
});

node!(FuncItem, {
    modifiers: Modifiers,
    generic_args: Option<GenericArgs<'a>>,
    return_ty: Type<'a>,
    name: Identifier<'a>,
    parameters: Punctuated<'a, Parameter<'a>>,
    body: Block<'a>
});

choice!(Parameter, {
    Ident(Box<'a, IdentParameter<'a>>),
    SelfParam(Box<'a, SelfParameter<'a>>),
    Variadic(Box<'a, VariadicParameter<'a>>),
});

node!(VariadicParameter, {
    ty: Type<'a>,
    modifiers: Modifiers,
    name: Identifier<'a>,
});

node!(SelfParameter, {
    modifiers: Modifiers,
});

node!(IdentParameter, {
    ty: Type<'a>,
    name: Identifier<'a>,
});

node!(VarItem, {
    modifiers: Modifiers,
    ty: Type<'a>,
    name: Identifier<'a>,
    value: Option<Expr<'a>>,
});

choice!(ExprStmt, {
    Expr(Box<'a, Expr<'a>>),
    ExprEndingWithBlock(Box<'a, ExprEndingWithBlock<'a>>),
});

choice!(ExprEndingWithBlock, {
    Block(Box<'a, Block<'a>>),
    If(Box<'a, IfExpr<'a>>),
    Match(Box<'a, MatchExpr<'a>>),
    While(Box<'a, WhileExpr<'a>>),
    For(Box<'a, ForExpr<'a>>),
    ForIn(Box<'a, ForInExpr<'a>>),
});

node!(Block, {
    label: Option<Label<'a>>,
    stmts: Vec<'a, Stmt<'a>>,
    expr: Option<Expr<'a>>,
});

node!(Label, {
    name: Identifier<'a>,
});

choice!(ElseClause, {
    Block(Box<'a, Block<'a>>),
    If(Box<'a, IfExpr<'a>>),
});

node!(IfExpr, {
    condition: Expr<'a>,
    consequence: Block<'a>,
    alternative: Option<ElseClause<'a>>,
});

node!(MatchExpr, {
    value: Expr<'a>,
    body: MatchBlock<'a>
});

node!(MatchBlock, {
    match_arms: Punctuated<'a, MatchArm<'a>>,
});

node!(MatchArm, {
    pattern: MatchPattern<'a>,
    value: ExprStmt<'a>,
});

node!(LastMatchArm, {
    pattern: MatchPattern<'a>,
    value: Expr<'a>,
});

node!(MatchPattern, {
    pattern: Pattern<'a>,
    guard: Option<ArmGuard<'a>>
});

node!(ArmGuard, {
    condition: Expr<'a>,
});

choice!(Pattern, {
    Lit(Box<'a, LitPat<'a>>),
    Ident(Box<'a, Identifier<'a>>),
    Name(Box<'a, NameExpr<'a>>),
    Ref(Box<'a, RefPattern<'a>>),
    Reference(Box<'a, ReferencePattern<'a>>),
    Mut(Box<'a, MutPattern<'a>>),
    Range(Box<'a, RangePattern<'a>>),
    Or(Box<'a, OrPattern<'a>>),
    Ignore
});

node!(OrPattern, {
    left: Pattern<'a>,
    right: Pattern<'a>,
});

node!(RangePattern, {
    lit_pat: LitPat<'a>,
    pattern: RangePatternKind<'a>,
});

node!(RefPattern, {
    pattern: Pattern<'a>,
});

node!(ReferencePattern, {
    mutable: bool,
    pattern: Pattern<'a>,
});

node!(MutPattern, {
    pattern: Pattern<'a>,
});

choice!(RangePatternKind, {
    To(Box<'a, RangeToPattern<'a>>),
    Full(Box<'a, RangeFullPattern<'a>>),
});

node!(RangeToPattern, {
    lit_pat: LitPat<'a>,
});

node!(RangeFullPattern, {
    from: LitPat<'a>,
    operator: RangeOperator,
    to: LitPat<'a>,
});

choice!(LitPat, {
    Str(Box<'a, StrLiteral<'a>>),
    Char(Box<'a, CharLiteral<'a>>),
    Bool(Box<'a, BoolLiteral<'a>>),
    Float(Box<'a, FloatLiteral<'a>>),
    Int(Box<'a, IntLiteral<'a>>),
    Neg(Box<'a, NegativeLiteral<'a>>),
});

node!(WhileExpr, {
    label: Option<Label<'a>>,
    condition: Expr<'a>,
    body: Block<'a>,
});

node!(ForExpr, {
    label: Option<Label<'a>>,
    initializer: VarItem<'a>,
    condition: Expr<'a>,
    updater: Expr<'a>,
    body: Block<'a>,
});

node!(ForInExpr, {
    label: Option<Label<'a>>,
    ty: Type<'a>,
    name: Identifier<'a>,
    expr: Expr<'a>,
    body: Block<'a>,
});

choice!(ExprWithoutRange, {
    Name(Box<'a, NameExpr<'a>>),
    Unary(Box<'a, UnaryExpr<'a>>),
    Reference(Box<'a, RefExpr<'a>>),
    Binary(Box<'a, BinaryExpr<'a>>),
    Assignment(Box<'a, AssignmentExpr<'a>>),
    CompoundAssignment(Box<'a, CompoundAssignmentExpr<'a>>),
    TypeCast(Box<'a, TypeCastExpr<'a>>),
    Call(Box<'a, CallExpr<'a>>),
    Return(Box<'a, ReturnExpr<'a>>),
    Literal(Box<'a, Literal<'a>>),
    Break(Box<'a, BreakExpr<'a>>),
    Continue(Box<'a, ContinueExpr<'a>>),
    Paren(Box<'a, Expr<'a>>),
    EndingWithBlock(Box<'a, ExprEndingWithBlock<'a>>),
    SelfExpr,
});

node!(ContinueExpr, {
    label: Option<Label<'a>>,
});

node!(BreakExpr, {
    label: Option<Label<'a>>,
});

node!(UnaryExpr, {
    operator: UnaryOperator,
    operand: Expr<'a>,
});

node!(BinaryExpr, {
    operator: BinaryOperator,
    left: Expr<'a>,
    right: Expr<'a>,
});

node!(RefExpr, {
    mutable: bool,
    operand: Expr<'a>
});

node!(AssignmentExpr, {
    left: Expr<'a>,
    right: Expr<'a>,
});

node!(CompoundAssignmentExpr, {
    left: Expr<'a>,
    operator: CompoundOperator,
    right: Expr<'a>,

});

node!(TypeCastExpr, {
    operand: Expr<'a>,
    ty: Type<'a>
});

node!(CallExpr, {
    callee: Expr<'a>,
    generic_args: Option<GenericArgs<'a>>,
    arguments: Punctuated<'a, Expr<'a>>,
});

node!(ReturnExpr, {
    value: Expr<'a>,
});

choice!(RangeExpr, {
    From(Box<'a, RangeFromExpr<'a>>),
    To(Box<'a, RangeToExpr<'a>>),
    Full(Box<'a, RangeFullExpr<'a>>),
});

node!(RangeFromExpr, {
    from: Expr<'a>,
});
node!(RangeToExpr, {
    to: Expr<'a>,
});

node!(RangeFullExpr, {
    operator: RangeOperator,
    from: Expr<'a>,
    to: Expr<'a>,
});

choice!(Expr, {
    ExceptRange(Box<'a, ExprWithoutRange<'a>>),
    Range(Box<'a, RangeExpr<'a>>)
});

node!(Identifier, {
    name: KwAtom,
});

choice!(NameExpr, {
    /// e.g. `std:io`
    Path(Box<'a, Path<'a>>),
    /// e.g. `<base>.member` - where `base` is *itself* another NameExpr
    Field(Box<'a, Field<'a>>),
    /// e.g. `base`
    Ident(Box<'a, Identifier<'a>>),
});

node!(Field, {
    base: FieldBase<'a>,
    member: Identifier<'a>,
});

choice!(FieldBase, {
    Ident(Box<'a, Identifier<'a>>),
    Field(Box<'a, Field<'a>>),
});

node!(Path, {
    segments: Punctuated<'a, PathSegment<'a>>,
});

node!(PathSegment, {
    ident: Identifier<'a>,
    arguments: PathArguments<'a>
});

choice!(PathArguments, {
    None,
    Generic(Box<'a, GenericArgs<'a>>)
});

node!(GenericArgs, {
     args: Punctuated<'a, GenericArg<'a>>
});

choice!(GenericArg, {
    Ty(Box<'a, Type<'a>>),
});

node!(Type, {
    path: TypePath<'a>
});

node!(TypePath, {
    segments: Punctuated<'a, TypePathSegment<'a>>,
});

node!(TypePathSegment, {
    ty: Box<'a, Ty<'a>>,
});

choice!(Ty, {
    Ptr(Box<'a, Ty<'a>>),
    Ref(Box<'a, Ty<'a>>),
    MutRef(Box<'a, Ty<'a>>),
    Mut(Box<'a, Ty<'a>>),
    Isz,
    I8,
    I16,
    I32,
    I64,
    I128,
    Usz,
    U8,
    U16,
    Void,
    U32,
    U64,
    U128,
    Float,
    Double,
    Bool,
    Str,
    Char,
    Infer,
    TySelf,
    Tyself,
    Ident(Identifier<'a>)
});

node!(ConstExpr, {
    literal: Literal<'a>
});
