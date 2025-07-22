use crate::{ast::*, choice, node, span::Span};
use bumpalo::boxed::Box;
use bumpalo::collections::Vec;

node!(ModuleItem, {
    name: Identifier<'a>,
    body: Vec<'a, TopLevelStmt<'a>>,
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

node!(VarItem, {
    modifiers: Modifiers,
    ty: Type<'a>,
    name: Identifier<'a>,
    value: Option<Expr<'a>>,
});
