use crate::{atoms::KwAtom, node};

mod expression;
mod literal;
mod utils;

pub use {expression::*, literal::*, utils::*};

pub mod visitor {
    use super::*;

    pub trait AstVisitor<'a, T> {
        fn visit_identifier(&mut self, ident: &Identifier<'a>) -> T;
        fn visit_literal(&mut self, lit: &Literal<'a>) -> T;
        fn visit_negative_literal(&mut self, lit: &NegativeLiteral) -> T;
        fn visit_punctuated<U>(&mut self, n: &Punctuated<'a, U>) -> T;
        fn visit_source_file(&mut self, n: &SourceFile<'a>) -> T;
        fn visit_stmt(&mut self, n: &Stmt<'a>) -> T;
        fn visit_decl_stmt(&mut self, n: &DeclStmt<'a>) -> T;
        fn visit_import_item(&mut self, n: &ImportItem<'a>) -> T;
        fn visit_empty_stmt(&mut self, n: &EmptyStmt<'a>) -> T;
        fn visit_const_item(&mut self, n: &ConstItem<'a>) -> T;
        fn visit_extern_func(&mut self, n: &ExternFunc<'a>) -> T;
        fn visit_func_item(&mut self, n: &FuncItem<'a>) -> T;
        fn visit_parameter(&mut self, n: &Parameter<'a>) -> T;
        fn visit_var_item(&mut self, n: &VarItem<'a>) -> T;
        fn visit_expr_stmt(&mut self, n: &ExprStmt<'a>) -> T;
        fn visit_expr_ending_with_block(&mut self, n: &ExprEndingWithBlock<'a>) -> T;
        fn visit_block(&mut self, n: &Block<'a>) -> T;
        fn visit_label(&mut self, n: &Label<'a>) -> T;
        fn visit_if_expr(&mut self, n: &IfExpr<'a>) -> T;
        fn visit_match_expr(&mut self, n: &MatchExpr<'a>) -> T;
        fn visit_pattern(&mut self, n: &Pattern<'a>) -> T;
        fn visit_lit_pat(&mut self, n: &LitPat<'a>) -> T;
        fn visit_while_expr(&mut self, n: &WhileExpr<'a>) -> T;
        fn visit_for_expr(&mut self, n: &ForExpr<'a>) -> T;
        fn visit_for_in_expr(&mut self, n: &ForInExpr<'a>) -> T;
        fn visit_expr_wo_range(&mut self, n: &ExprWithoutRange<'a>) -> T;
        fn visit_continue_expr(&mut self, n: &ContinueExpr<'a>) -> T;
        fn visit_break_expr(&mut self, n: &BreakExpr<'a>) -> T;
        fn visit_unary_expr(&mut self, n: &UnaryExpr<'a>) -> T;
        fn visit_binary_expr(&mut self, n: &BinaryExpr<'a>) -> T;
        fn visit_assignment_expr(&mut self, n: &AssignmentExpr<'a>) -> T;
        fn visit_compound_assignment_expr(&mut self, n: &CompoundAssignmentExpr<'a>) -> T;
        fn visit_type_cast_expr(&mut self, n: &TypeCastExpr<'a>) -> T;
        fn visit_call_expr(&mut self, n: &CallExpr<'a>) -> T;
        fn visit_return_expr(&mut self, n: &ReturnExpr<'a>) -> T;
        fn visit_expr(&mut self, n: &Expr<'a>) -> T;
        fn visit_name_expr(&mut self, n: &NameExpr<'a>) -> T;
        fn visit_ty(&mut self, n: &Ty<'a>) -> T;
        fn visit_type(&mut self, n: &Type<'a>) -> T;
    }
}
