use super::items::{ConstItem, ExternFunc, FuncItem, ImportItem};
use crate::ast::{ExprStmt, ModuleItem, VarItem};
use crate::{choice, node, span::Span};
use bumpalo::boxed::Box;
use bumpalo::collections::Vec;

node!(SourceFile, {
    stmts: Vec<'a, TopLevelStmt<'a>>,
});

choice!(TopLevelStmt, {
    Import(Box<'a, ImportItem<'a>>),
    Module(Box<'a, ModuleItem<'a>>),
    Stmt(Box<'a, Stmt<'a>>),
    Empty(Span),
});

choice!(Stmt, {
    Decl(DeclStmt<'a>),
    Expr(ExprStmt<'a>),
    Empty(Span),
});

choice!(DeclStmt, {
    Const(Box<'a, ConstItem<'a>>),
    Empty(Box<'a, EmptyStmt<'a>>),
    Func(Box<'a, FuncItem<'a>>),
    Extern(Box<'a, ExternFunc<'a>>),
    Var(Box<'a, VarItem<'a>>),
});

node!(EmptyStmt, {});
