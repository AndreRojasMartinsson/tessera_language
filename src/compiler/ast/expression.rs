use bitflags::bitflags;
use clap::Id;

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
    stmts: Vec<Stmt<'a>>,
});

choice!(Stmt, {
    Decl(DeclStmt<'a>),
    Expr(ExprStmt<'a>),
    Empty(Span),
});

choice!(DeclStmt, {
    Const(Box<ConstItem<'a>>),
    Empty(Box<EmptyStmt<'a>>),
    Func(Box<FuncItem<'a>>),
    Extern(Box<ExternFunc<'a>>),
    Var(Box<VarItem<'a>>),
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
    Ident(Box<IdentParameter<'a>>),
    SelfParam(Box<SelfParameter<'a>>),
    Variadic(Box<VariadicParameter<'a>>),
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
    Expr(Box<Expr<'a>>),
    ExprEndingWithBlock(Box<ExprEndingWithBlock<'a>>),
});

choice!(ExprEndingWithBlock, {
    Block(Box<Block<'a>>),
    If(Box<IfExpr<'a>>),
    Match(Box<MatchExpr<'a>>),
    While(Box<WhileExpr<'a>>),
    For(Box<ForExpr<'a>>),
    ForIn(Box<ForInExpr<'a>>),
});

node!(Block, {
    label: Option<Label<'a>>,
    stmts: Vec<Stmt<'a>>,
    expr: Option<Expr<'a>>,
});

node!(Label, {
    name: Identifier<'a>,
});

choice!(ElseClause, {
    Block(Box<Block<'a>>),
    If(Box<IfExpr<'a>>),
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
    Lit(Box<LitPat<'a>>),
    Ident(Box<Identifier<'a>>),
    Name(Box<NameExpr<'a>>),
    Ref(Box<RefPattern<'a>>),
    Reference(Box<ReferencePattern<'a>>),
    Mut(Box<MutPattern<'a>>),
    Range(Box<RangePattern<'a>>),
    Or(Box<OrPattern<'a>>),
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
    To(Box<RangeToPattern<'a>>),
    Full(Box<RangeFullPattern<'a>>),
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
    Str(Box<StrLiteral<'a>>),
    Char(Box<CharLiteral<'a>>),
    Bool(Box<BoolLiteral<'a>>),
    Float(Box<FloatLiteral<'a>>),
    Int(Box<IntLiteral<'a>>),
    Neg(Box<NegativeLiteral<'a>>),
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
    Name(Box<NameExpr<'a>>),
    Unary(Box<UnaryExpr<'a>>),
    Reference(Box<RefExpr<'a>>),
    Binary(Box<BinaryExpr<'a>>),
    Assignment(Box<AssignmentExpr<'a>>),
    CompoundAssignment(Box<CompoundAssignmentExpr<'a>>),
    TypeCast(Box<TypeCastExpr<'a>>),
    Call(Box<CallExpr<'a>>),
    Return(Box<ReturnExpr<'a>>),
    Literal(Box<Literal<'a>>),
    Break(Box<BreakExpr<'a>>),
    Continue(Box<ContinueExpr<'a>>),
    Paren(Box<Expr<'a>>),
    EndingWithBlock(Box<ExprEndingWithBlock<'a>>),
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
    From(Box<RangeFromExpr<'a>>),
    To(Box<RangeToExpr<'a>>),
    Full(Box<RangeFullExpr<'a>>),
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
    ExceptRange(Box<ExprWithoutRange<'a>>),
    Range(Box<RangeExpr<'a>>)
});

node!(Identifier, {
    name: KwAtom,
});

choice!(NameExpr, {
    /// e.g. `std:io`
    Path(Box<Path<'a>>),
    /// e.g. `<base>.member` - where `base` is *itself* another NameExpr
    Field(Box<Field<'a>>),
    /// e.g. `base`
    Ident(Box<Identifier<'a>>),
});

node!(Field, {
    base: FieldBase<'a>,
    member: Identifier<'a>,
});

choice!(FieldBase, {
    Ident(Box<Identifier<'a>>),
    Field(Box<Field<'a>>),
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
    Generic(Box<GenericArgs<'a>>)
});

node!(GenericArgs, {
     args: Punctuated<'a, GenericArg<'a>>
});

choice!(GenericArg, {
    Ty(Box<Type<'a>>),
});

node!(Type, {
    path: TypePath<'a>
});

node!(TypePath, {
    segments: Punctuated<'a, TypePathSegment<'a>>,
});

node!(TypePathSegment, {
    ty: Ty<'a>,
});

choice!(Ty, {
    Ptr(Box<Ty<'a>>),
    Ref(Box<Ty<'a>>),
    MutRef(Box<Ty<'a>>),
    Mut(Box<Ty<'a>>),
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
