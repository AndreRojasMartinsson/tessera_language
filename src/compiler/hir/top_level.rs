use std::path::Path;

use crate::atoms::KwAtom;
use crate::hir::HirId;
use crate::{choice, node, span::Span};
use bumpalo::boxed::Box;
use bumpalo::collections::Vec;

node!(SourceFile, {
    imports: Vec<'a, SourceImport<'a>>,
    exports: Vec<'a, Node<'a>>,
    stmts: Vec<'a, TopLevelStmt<'a>>,
});

choice!(Node, {});

node!(SourceImport, {
    span: Span,
    /// The file containing the specific imports
    file: &'a Path,
    /// The HirId pointing to the specific symbol in that file
    /// Using the HirId we can get the type of the symbol for type
    /// resolution, as well as any other information.
    hir_id: HirId,
});

choice!(TopLevelStmt, {
    Stmt(Box<'a, Stmt<'a>>),
    Empty(Span),
});

choice!(Stmt, {
    Decl(DeclStmt<'a>),
    Empty(Span),
});

choice!(DeclStmt, {});
