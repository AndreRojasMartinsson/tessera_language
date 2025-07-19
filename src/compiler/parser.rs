use std::{cell::Ref, hint::cold_path, marker::PhantomData, path};

use bumpalo::{Bump, boxed::Box, collections::Vec};

use clap::Id;
use codespan_reporting::{
    diagnostic::{self, Diagnostic},
    files::Files,
    term::{self, termcolor},
};
use color_eyre::{
    eyre::{self, Context, Report, Result as EyreResult, bail, eyre},
    owo_colors::OwoColorize,
};
use log::{debug, warn};

type Result<T, E = ParseError> = EyreResult<T, E>;

use crate::{atoms::KwAtom, errors::ParseError, operator::RangeOperator};

use super::{
    ast::*,
    lexer::{Lex, Lexer},
    operator::{BinaryOperator, CompoundOperator, Precedence, UnaryOperator},
    span::{self, Span},
    token::{Kind, Token, Value},
};

pub struct Parser<'a, T: Lex<'a>, F: Files<'a>> {
    source: &'a str,
    lexer: T,
    cur_token: Token<'a>,
    prev_token_end: usize,
    arena: &'a Bump,

    // error handling
    diagnostics: std::vec::Vec<Diagnostic<F::FileId>>,
    files: F,
    file_id: F::FileId,
}

macro_rules! shortcircuit {
    ($value:expr) => {
        return Err($value.into())
    };
}

impl<'a, T, F> Parser<'a, T, F>
where
    T: Lex<'a>,
    F: Files<'a>,
{
    pub fn new(arena: &'a Bump, source: &'a str, lexer: T, files: F, file_id: F::FileId) -> Self {
        Self {
            source,
            lexer,
            cur_token: Token::default(),
            prev_token_end: 0,
            files,
            file_id,
            arena,
            diagnostics: vec![],
        }
    }

    /// Entry point: produce a SourceFile AST with top-level items.
    /// We loop until EOF to allow streaming parsing of multiple items.
    pub fn parse(&'a mut self) -> Result<SourceFile<'a>, Report> {
        let mut items = bumpalo::vec![in &self.arena;];

        // We prime the first token since at this point cur_token is `Token::default` as laid out
        // in the constructor.
        self.advance();

        loop {
            if self.at(Kind::Eof) {
                cold_path();
                break;
            }

            let span = self.start_span();

            items.push(self.parse_statement());
        }

        let writer = termcolor::StandardStream::stderr(termcolor::ColorChoice::Auto);
        let config = term::Config::default();

        for diag in std::mem::take(&mut self.diagnostics) {
            term::emit(&mut writer.lock(), &config, &self.files, &diag)?;
        }

        Ok(SourceFile::new(Span::new(0, self.source.len()), items))
    }

    fn heap_alloc<V>(&self, value: V) -> Box<'a, V> {
        Box::new_in(value, self.arena)
    }

    fn parse_statement_inner(&mut self) -> Result<Stmt<'a>> {
        let span = self.start_span();

        let stmt = match self.cur_kind() {
            // Treat stray semicolons as empty declarations to keep AST uniform
            Kind::Semicolon => {
                let span = self.start_span();
                self.advance();

                Stmt::Decl(DeclStmt::Empty(
                    self.heap_alloc(EmptyStmt::new(self.finish_span(span))),
                ))
            }

            // Declaration starts: functions, constants, externs, identifiers start variable decls
            Kind::KwFn
            | Kind::KwPub
            | Kind::KwConst
            | Kind::KwExtern
            | Kind::Identifier
            | Kind::PrimitiveType
            | Kind::KwMut => {
                let decl = self.parse_decl_stmt()?;
                Stmt::Decl(decl)
            }

            // Block forms as expression statements allow constructs like `{ ... }` in ExprStmt context
            kind if Self::is_block_start(kind) => {
                let blk = self.parse_expr_ending_with_block()?;

                Stmt::Expr(ExprStmt::ExprEndingWithBlock(self.heap_alloc(blk)))
            }

            // Fallback to generic expression statements
            _ => {
                let expr = self.parse_expr_statement()?;

                Stmt::Expr(expr)
            }
        };

        Ok(stmt)
    }

    fn parse_statement(&mut self) -> Stmt<'a> {
        let start = self.start_span();
        const SYNC: &[Kind] = &[Kind::Semicolon, Kind::RBrace, Kind::Eof];

        let arena = self.arena;

        self.recoverable(
            start,
            SYNC,
            |p| p.parse_statement_inner(),
            |span| Stmt::Decl(DeclStmt::Empty(Box::new_in(EmptyStmt::new(span), arena))),
        )
    }

    /// Parse a standalone expression statement; blocks are special-cased to preserve trailing block semantics.
    fn parse_expr_statement(&mut self) -> Result<ExprStmt<'a>> {
        let arena = self.arena;

        if Self::is_block_start(self.cur_kind()) {
            // Preserve block structure when used as statement
            return Ok(ExprStmt::ExprEndingWithBlock(Box::new_in(
                self.parse_expr_ending_with_block()?,
                arena,
            )));
        }

        // For plain expressions, we delegate to the precedence-driven expr parser
        Ok(ExprStmt::Expr(Box::new_in(self.parse_expr(0)?, arena)))
    }

    fn parse_expr_ending_with_block(&mut self) -> Result<ExprEndingWithBlock<'a>> {
        let expr = match self.cur_kind() {
            Kind::LBrace => {
                let blk = self.parse_block()?;
                ExprEndingWithBlock::Block(self.heap_alloc(blk))
            }
            Kind::KwIf => {
                let expr = self.parse_if_expr()?;
                ExprEndingWithBlock::If(self.heap_alloc(expr))
            }
            Kind::KwMatch => {
                let expr = self.parse_match_expr()?;
                ExprEndingWithBlock::Match(self.heap_alloc(expr))
            }
            Kind::KwWhile => {
                let expr = self.parse_while_expr()?;
                ExprEndingWithBlock::While(self.heap_alloc(expr))
            }
            Kind::KwFor => return self.parse_for_expr_with_lookahead(),
            // SAFETY: We checked via is_block_start that only these kinds can occur
            _ => unreachable!(),
        };

        Ok(expr)
    }

    fn parse_if_expr(&mut self) -> Result<IfExpr<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwIf)?;

        let condition = self.parse_expr(0)?;
        let body = self.parse_block()?;

        let arena = self.arena;

        let alternative = if self.eat(Kind::KwElse) {
            if self.at(Kind::KwIf) {
                Some(ElseClause::If(Box::new_in(self.parse_if_expr()?, arena)))
            } else {
                Some(ElseClause::Block(Box::new_in(self.parse_block()?, arena)))
            }
        } else {
            None
        };

        Ok(IfExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            condition,
            consequence: body,
            alternative,
        })
    }

    fn parse_match_expr(&mut self) -> Result<MatchExpr<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwMatch)?;

        let value = self.parse_expr(0)?;
        let body = self.parse_match_body()?;

        Ok(MatchExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            value,
            body,
        })
    }

    fn parse_match_body(&mut self) -> Result<MatchBlock<'a>> {
        let span = self.start_span();
        self.expect(Kind::LBrace)?;

        let mut arms = self.parse_punctuated(Self::parse_match_arm, Kind::Comma, Kind::RBrace)?;

        self.expect(Kind::RBrace)?;

        Ok(MatchBlock {
            _marker: PhantomData,
            loc: self.finish_span(span),
            match_arms: arms,
        })
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm<'a>> {
        let span = self.start_span();

        let pattern = self.parse_match_pattern()?;

        self.expect(Kind::RArrow)?;

        let value = self.parse_expr_statement()?;

        Ok(MatchArm::new(self.finish_span(span), pattern, value))
    }

    fn parse_match_pattern(&mut self) -> Result<MatchPattern<'a>> {
        let span = self.start_span();

        let pattern = self.parse_pattern()?;

        let guard = self.parse_node_if(Kind::KwIf, Self::parse_arm_guard)?;

        Ok(MatchPattern::new(self.finish_span(span), pattern, guard))
    }

    fn parse_arm_guard(&mut self) -> Result<ArmGuard<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwIf)?;

        let condition = self.parse_expr(0)?;

        Ok(ArmGuard::new(self.finish_span(span), condition))
    }

    fn parse_pattern(&mut self) -> Result<Pattern<'a>> {
        let span = self.start_span();
        let arena = self.arena;

        let mut lhs = match self.cur_kind() {
            Kind::Underscore => {
                self.advance();
                Pattern::Ignore
            }

            Kind::BoolLiteral
            | Kind::IntLiteral
            | Kind::CharLiteral
            | Kind::FloatLiteral
            | Kind::StringLiteral => Pattern::Lit(Box::new_in(self.parse_lit_pattern()?, arena)),

            Kind::Identifier => Pattern::Ident(Box::new_in(self.parse_identifier()?, arena)),
            Kind::KwRef => Pattern::Ref(Box::new_in(self.parse_ref_pattern()?, arena)),
            Kind::Ampersand => {
                Pattern::Reference(Box::new_in(self.parse_reference_pattern()?, arena))
            }
            Kind::KwMut => Pattern::Mut(Box::new_in(self.parse_mut_pattern()?, arena)),

            _ => todo!(),
        };

        // Parse or patterns
        while self.at(Kind::BitOr) {
            self.expect(Kind::BitOr)?;

            let rhs = self.parse_pattern()?;

            lhs = Pattern::Or(self.heap_alloc(OrPattern::new(self.finish_span(span), lhs, rhs)));
        }

        Ok(lhs)
    }

    fn parse_reference_pattern(&mut self) -> Result<ReferencePattern<'a>> {
        let span = self.start_span();

        self.expect(Kind::Ampersand)?;

        let mutable = self.eat(Kind::KwMut);

        let pattern = self.parse_pattern()?;

        Ok(ReferencePattern::new(
            self.finish_span(span),
            mutable,
            pattern,
        ))
    }

    fn parse_mut_pattern(&mut self) -> Result<MutPattern<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwMut)?;

        let pattern = self.parse_pattern()?;

        Ok(MutPattern::new(self.finish_span(span), pattern))
    }

    fn parse_ref_pattern(&mut self) -> Result<RefPattern<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwRef)?;

        let pattern = self.parse_pattern()?;

        Ok(RefPattern::new(self.finish_span(span), pattern))
    }

    fn parse_lit_pattern(&mut self) -> Result<LitPat<'a>> {
        let start = self.start_span();
        let arena = self.arena;

        let lit = match self.cur_value() {
            &Value::Int(value) => LitPat::Int(Box::new_in(self.parse_int_literal()?, arena)),
            &Value::Float(value) => LitPat::Float(Box::new_in(self.parse_float_literal()?, arena)),
            &Value::Bool(value) => LitPat::Bool(Box::new_in(self.parse_bool_literal()?, arena)),
            &Value::String(value) => LitPat::Str(Box::new_in(self.parse_string_literal()?, arena)),
            &Value::Char(value) => LitPat::Char(Box::new_in(self.parse_char_literal()?, arena)),

            _ if self.cur_kind() == Kind::Minus => {
                LitPat::Neg(Box::new_in(self.parse_negative_literal()?, arena))
            }

            other => {
                cold_path();
                return Err(ParseError::UnexpectedToken {
                    found: self.cur_kind(),
                    expected: vec![
                        Kind::IntLiteral,
                        Kind::BoolLiteral,
                        Kind::FloatLiteral,
                        Kind::StringLiteral,
                        Kind::CharLiteral,
                        Kind::Minus,
                    ],
                    span: start,
                });
            }
        };

        Ok(lit)
    }

    fn parse_negative_literal(&mut self) -> Result<NegativeLiteral<'a>> {
        let start = self.start_span();
        self.expect(Kind::Minus)?;

        let arena = self.arena;

        let lit = match self.cur_value() {
            &Value::Int(value) => {
                NegativeLiteral::Int(Box::new_in(self.parse_int_literal()?, arena))
            }
            &Value::Float(value) => {
                NegativeLiteral::Float(Box::new_in(self.parse_float_literal()?, arena))
            }

            other => {
                cold_path();
                return Err(ParseError::UnexpectedToken {
                    found: self.cur_kind(),
                    expected: vec![Kind::IntLiteral, Kind::FloatLiteral],
                    span: start,
                });
            }
        };

        Ok(lit)
    }

    fn parse_int_literal(&mut self) -> Result<IntLiteral<'a>> {
        let span = self.start_span();
        let token = self.advance();

        let lit = match token.value {
            Value::Int(value) => IntLiteral::new(self.finish_span(span), value),

            // SAFETY: We made sure value can only be a Int in parse_lit_pattern
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_string_literal(&mut self) -> Result<StrLiteral<'a>> {
        let span = self.start_span();
        let token = self.advance();

        let lit = match token.value {
            Value::String(value) => StrLiteral::new(self.finish_span(span), value),

            // SAFETY: We made sure value can only be a Str in parse_lit_pattern
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_bool_literal(&mut self) -> Result<BoolLiteral<'a>> {
        let span = self.start_span();
        let token = self.advance();

        let lit = match token.value {
            Value::Bool(value) => BoolLiteral::new(self.finish_span(span), value),

            // SAFETY: We made sure value can only be a Bool in parse_lit_pattern
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_char_literal(&mut self) -> Result<CharLiteral<'a>> {
        let span = self.start_span();
        let token = self.advance();

        let lit = match token.value {
            Value::Char(value) => CharLiteral::new(self.finish_span(span), value),

            // SAFETY: We made sure value can only be a Char in parse_lit_pattern
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_float_literal(&mut self) -> Result<FloatLiteral<'a>> {
        let span = self.start_span();
        let token = self.advance();

        let lit = match token.value {
            Value::Float(value) => FloatLiteral::new(self.finish_span(span), value),

            // SAFETY: We made sure value can only be a Float in parse_lit_pattern
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_while_expr(&mut self) -> Result<WhileExpr<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwWhile)?;

        let condition = self.parse_expr(0)?;
        let body = self.parse_block()?;

        Ok(WhileExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            label: None,
            condition,
            body,
        })
    }

    fn parse_for_expr_with_lookahead(&mut self) -> Result<ExprEndingWithBlock<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwFor)?;

        let ty = self.parse_type()?;
        let name = self.parse_identifier()?;

        if self.eat(Kind::Assign) {
            let for_expr = self._parse_for_expr(span, ty, name)?;

            return Ok(ExprEndingWithBlock::For(self.heap_alloc(for_expr)));
        }

        self.expect(Kind::KwIn)?;

        let expr = self.parse_expr(0)?;

        let body = self.parse_block()?;

        let for_in = ForInExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            label: None,
            ty,
            name,
            expr,
            body,
        };

        Ok(ExprEndingWithBlock::ForIn(self.heap_alloc(for_in)))
    }

    fn _parse_for_expr(
        &mut self,
        span: Span,
        ty: Type<'a>,
        name: Identifier<'a>,
    ) -> Result<ForExpr<'a>> {
        let initializer = VarItem {
            _marker: PhantomData,
            loc: self.finish_span(span),
            modifiers: Modifiers::empty(),
            ty,
            name,
            value: Some(self.parse_expr(0)?),
        };

        self.expect(Kind::Comma)?;

        let condition = self.parse_expr(0)?;

        self.expect(Kind::Comma)?;

        let updater = self.parse_expr(0)?;

        let body = self.parse_block()?;

        Ok(ForExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            label: None,
            initializer,
            condition,
            updater,
            body,
        })
    }

    /// Identify tokens that introduce block-based constructs; ensures consistent parsing of flows.
    fn is_block_start(kind: Kind) -> bool {
        matches!(
            kind,
            Kind::LBrace | Kind::KwIf | Kind::KwMatch | Kind::KwWhile | Kind::KwFor
        )
    }

    /// Handle declaration statements: variables, constants, functions. Modifiers allowed change variants.
    fn parse_decl_stmt(&mut self) -> Result<DeclStmt<'a>> {
        let start = self.start_span();
        let mut modifiers = Modifiers::empty();
        let arena = self.arena;

        // If starting with type or mut, parse var decl directly since variable declarations are the only
        // node to start with mut.
        match self.cur_kind() {
            Kind::PrimitiveType | Kind::Identifier | Kind::KwMut => {
                return Ok(DeclStmt::Var(Box::new_in(self.parse_var_item()?, arena)));
            }
            _ => {
                cold_path();
            }
        };

        if self.eat(Kind::KwExtern) {
            return Ok(DeclStmt::Extern(Box::new_in(
                self.parse_extern_func()?,
                arena,
            )));
        }

        // Public, extern, const modifiers adjust subsequent function or const decl
        if self.eat(Kind::KwPub) {
            modifiers |= Modifiers::PUB;
        }

        if self.eat(Kind::KwConst) {
            modifiers |= Modifiers::CONST;
        }

        match self.cur_kind() {
            Kind::PrimitiveType => Ok(DeclStmt::Const(Box::new_in(
                self.parse_const_item(modifiers)?,
                arena,
            ))),
            Kind::KwFn => Ok(DeclStmt::Func(Box::new_in(
                self.parse_function_item(modifiers)?,
                arena,
            ))),

            other => {
                cold_path();
                shortcircuit!(ParseError::UnexpectedToken {
                    found: other,
                    expected: vec![Kind::PrimitiveType, Kind::KwFn,],
                    span: self.finish_span(start),
                })
            }
        }
    }

    fn parse_extern_func(&mut self) -> Result<ExternFunc<'a>> {
        let span = self.start_span();

        let mut modifiers = Modifiers::empty();

        if self.eat(Kind::KwConst) {
            modifiers |= Modifiers::CONST;
        }

        self.expect(Kind::KwFn);

        let return_ty = self.parse_type()?;
        let name = self.parse_identifier()?;
        let generic_args = self.parse_node_if(Kind::LessThan, Self::parse_generic_args)?;
        let parameters = self.parse_parameters()?;

        self.expect(Kind::Semicolon)?;

        Ok(ExternFunc {
            _marker: PhantomData,
            loc: self.finish_span(span),
            parameters,
            modifiers,
            generic_args,
            return_ty,
            name,
        })
    }

    /// Parse variable declarations; we capture mutability modifier before type to associate it with the VarItem.
    fn parse_var_item(&mut self) -> Result<VarItem<'a>> {
        let span = self.start_span();
        let modifiers = Modifiers::empty();
        let modifiers = if self.eat(Kind::KwMut) {
            modifiers | Modifiers::MUTABLE
        } else {
            modifiers
        };

        let ty = self.parse_type()?;
        let name = self.parse_identifier()?;

        let value = self.parse_node_if(Kind::Assign, Self::parse_var_initializer)?;

        Ok(VarItem {
            _marker: PhantomData,
            loc: self.finish_span(span),
            modifiers,
            ty,
            name,
            value,
        })
    }

    fn parse_var_initializer(&mut self) -> Result<Expr<'a>> {
        self.expect(Kind::Assign)?;

        self.parse_expr(Precedence::Assign.value())
    }

    /// Const items must end with semicolon; we enforce this to separate consts cleanly from expressions.
    fn parse_const_item(&mut self, modifiers: Modifiers) -> Result<ConstItem<'a>> {
        let span = self.start_span();

        let ty = self.parse_type()?;
        let name = self.parse_identifier()?;

        self.expect(Kind::Assign)?;

        let value = self.parse_expr(Precedence::Assign.value())?;

        self.expect(Kind::Semicolon)?;

        Ok(ConstItem {
            _marker: PhantomData,
            loc: self.finish_span(span),
            modifiers,
            ty,
            name,
            value,
        })
    }

    /// Pratt parser parser: respects operator binding power to correctly nest AST nodes.
    fn parse_expr(&mut self, min_bp: i8) -> Result<Expr<'a>> {
        // Parse LHS atomically before handling binary or assignment operators
        let start = self.start_span();
        let mut lhs = self.parse_primary()?;

        lhs = self.parse_postfix(lhs)?;

        // We parse assignment expressions before the loop since the language
        // does not support assignment expressions as atomic expressions that can occur
        // inside other expressons.
        match self.cur_kind() {
            Kind::Assign => {
                let assign = self.parse_assignment_expr(lhs)?;
                lhs = Expr::ExceptRange(
                    self.heap_alloc(ExprWithoutRange::Assignment(self.heap_alloc(assign))),
                )
            }

            kind if CompoundOperator::try_from(kind).is_ok() => {
                let assign = self.parse_compound_assignment_expr(lhs)?;
                lhs = Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::CompoundAssignment(
                    self.heap_alloc(assign),
                )))
            }

            _ => {}
        }

        loop {
            if self.at(Kind::Eof) {
                break;
            }

            // Stop when no binary operator with sufficient precedence found
            let kind = self.cur_kind();

            if let Ok(op) = RangeOperator::try_from(kind) {
                // Left-binding ensures left-assoc operators group as (a-b)-c
                let l_bp = Precedence::from(op).value();
                if l_bp < min_bp {
                    break;
                }

                self.advance(); // Consume operator token

                // Since currently every binary operator is an infix left-assosciative operator
                // we bind the right side of the expr 1 higher than the left side.
                //
                // For a left‑associative operator ⊕, if you see lhs ⊕ rest,
                // you parse rest with precedence > l_bp (so you use r_bp = l_bp + 1).
                //
                // left‑associative operators must not let the next same‑precedence op bind to the RHS,
                // so we bump the RHS’s min_bp to l_bp+1 to force (a - b) - c, not a - (b - c)
                let r_bp = l_bp + 1;
                let rhs = self.parse_expr(r_bp)?;
                let span = self.finish_span(start);
                let full = RangeFullExpr::new(span, op, lhs, rhs);

                lhs = Expr::Range(self.heap_alloc(RangeExpr::Full(self.heap_alloc(full))));
                continue;
            }

            let op = match BinaryOperator::try_from(kind) {
                Ok(op) => op,
                _ => {
                    cold_path();
                    break;
                }
            };

            // Left-binding ensures left-assoc operators group as (a-b)-c
            let l_bp = Precedence::from(op).value();

            // Since currently every binary operator is an infix left-assosciative operator
            // we bind the right side of the expr 1 higher than the left side.
            //
            // For a left‑associative operator ⊕, if you see lhs ⊕ rest,
            // you parse rest with precedence > l_bp (so you use r_bp = l_bp + 1).
            //
            // left‑associative operators must not let the next same‑precedence op bind to the RHS,
            // so we bump the RHS’s min_bp to l_bp+1 to force (a - b) - c, not a - (b - c)
            let r_bp = l_bp + 1;
            if l_bp < min_bp {
                break;
            }

            let span = self.start_span();
            self.advance(); // eat the operator token

            let rhs = self.parse_expr(r_bp)?;

            let binary = BinaryExpr::new(self.finish_span(span), op, lhs, rhs);
            lhs = Expr::ExceptRange(
                self.heap_alloc(ExprWithoutRange::Binary(self.heap_alloc(binary))),
            );
        }

        Ok(lhs)
    }

    fn parse_postfix(&mut self, mut lhs: Expr<'a>) -> Result<Expr<'a>> {
        let arena = self.arena;
        loop {
            lhs = match self.cur_kind() {
                Kind::KwAs => Expr::ExceptRange(Box::new_in(
                    ExprWithoutRange::TypeCast(Box::new_in(self.parse_type_cast_expr(lhs)?, arena)),
                    arena,
                )),

                Kind::LParen | Kind::LessThan => Expr::ExceptRange(Box::new_in(
                    ExprWithoutRange::Call(Box::new_in(self.parse_call_expr(lhs)?, arena)),
                    arena,
                )),

                _ => break Ok(lhs),
            };
        }
    }

    fn parse_call_expr(&mut self, lhs: Expr<'a>) -> Result<CallExpr<'a>> {
        let span = self.start_span();

        let generic_args = self.parse_node_if(Kind::LessThan, Self::parse_generic_args)?;
        let arguments = self.parse_arguments()?;

        Ok(CallExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            callee: lhs,
            generic_args,
            arguments,
        })
    }

    fn parse_arguments(&mut self) -> Result<Punctuated<'a, Expr<'a>>> {
        self.expect(Kind::LParen)?;

        let list = self.parse_punctuated(|p| p.parse_expr(0), Kind::Comma, Kind::RParen)?;

        self.expect(Kind::RParen)?;

        Ok(list)
    }

    fn parse_type_cast_expr(&mut self, lhs: Expr<'a>) -> Result<TypeCastExpr<'a>> {
        let span = self.start_span();
        self.expect(Kind::KwAs)?;

        let ty = self.parse_type()?;

        Ok(TypeCastExpr::new(self.finish_span(span), lhs, ty))
    }

    fn parse_assignment_expr(&mut self, lhs: Expr<'a>) -> Result<AssignmentExpr<'a>> {
        let span = self.start_span();

        self.advance(); // Consume operator

        let rhs = self.parse_expr(0)?;

        Ok(AssignmentExpr::new(self.finish_span(span), lhs, rhs))
    }

    fn parse_compound_assignment_expr(
        &mut self,
        lhs: Expr<'a>,
    ) -> Result<CompoundAssignmentExpr<'a>> {
        let span = self.start_span();
        let op_tok = self.advance();

        // SAFETY: We checked in the parent match expression that this operator
        // is a compound operator.
        let op = unsafe { CompoundOperator::try_from(op_tok.kind).unwrap_unchecked() };

        let rhs = self.parse_expr(0)?;

        Ok(CompoundAssignmentExpr::new(
            self.finish_span(span),
            lhs,
            op,
            rhs,
        ))
    }

    /// Parse literals, identifiers, parenthesis, and unary operators; this bootstraps parse_expr.
    fn parse_primary(&mut self) -> Result<Expr<'a>> {
        let start = self.start_span();
        let token = self.cur_token();

        let expr = match token.kind {
            Kind::BoolLiteral
            | Kind::IntLiteral
            | Kind::CharLiteral
            | Kind::FloatLiteral
            | Kind::StringLiteral => {
                let lit = self.parse_literal()?;
                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Literal(self.heap_alloc(lit))))
            }

            Kind::KwContinue => {
                let expr = self.parse_continue_expr()?;
                Expr::ExceptRange(
                    self.heap_alloc(ExprWithoutRange::Continue(self.heap_alloc(expr))),
                )
            }

            Kind::KwBreak => {
                let expr = self.parse_break_expr()?;
                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Break(self.heap_alloc(expr))))
            }

            Kind::KwReturn => {
                let expr = self.parse_return_expr()?;
                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Return(self.heap_alloc(expr))))
            }

            Kind::Identifier => {
                let expr = self.parse_name_expr()?;
                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Name(self.heap_alloc(expr))))
            }

            Kind::LParen => {
                let expr = self.parse_paren_expr()?;
                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Paren(self.heap_alloc(expr))))
            }

            Kind::Ampersand | Kind::LogAnd => {
                let expr = self.parse_ref_expr()?;
                Expr::ExceptRange(
                    self.heap_alloc(ExprWithoutRange::Reference(self.heap_alloc(expr))),
                )
            }

            // Unary Expressions
            Kind::Minus | Kind::Bang | Kind::BitNot | Kind::Asterisk => {
                let expr = self.parse_unary_expr()?;

                Expr::ExceptRange(self.heap_alloc(ExprWithoutRange::Unary(self.heap_alloc(expr))))
            }

            other => {
                cold_path();
                shortcircuit!(ParseError::UnexpectedToken {
                    found: other,
                    expected: vec![
                        Kind::BoolLiteral,
                        Kind::IntLiteral,
                        Kind::CharLiteral,
                        Kind::FloatLiteral,
                        Kind::StringLiteral,
                        Kind::KwReturn,
                        Kind::Identifier,
                        Kind::LParen,
                        Kind::Minus,
                        Kind::Bang,
                        Kind::BitNot,
                        Kind::Asterisk,
                    ],
                    span: self.finish_span(start),
                })
            }
        };

        Ok(expr)
    }

    fn parse_ref_expr(&mut self) -> Result<RefExpr<'a>> {
        let span = self.start_span();

        let mut amp_count = 0;
        let mut mutable = false;

        while self.at(Kind::Ampersand) || self.at(Kind::LogAnd) {
            // '&&' lexes to LogAnd → two ampersands
            if self.eat(Kind::LogAnd) {
                amp_count += 2;
            } else {
                // Single '&'
                self.advance();
                amp_count += 1;
            }

            if self.eat(Kind::KwMut) {
                mutable = true;
            }
        }

        let mut expr = self.parse_expr(Precedence::Unary.value())?;

        for _ in 0..amp_count {
            let loc = self.finish_span(span);
            let node = RefExpr::new(loc, mutable, expr);
            expr = Expr::ExceptRange(
                self.heap_alloc(ExprWithoutRange::Reference(self.heap_alloc(node))),
            );
        }

        let value = match_deref::match_deref! {
            match &expr {
                Expr::ExceptRange(Deref @ ExprWithoutRange::Reference(Deref @ outer)) => Ok(outer),
                _ => unreachable!()
            }
        }?;

        Ok(RefExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            mutable: value.mutable,
            operand: expr,
        })
    }

    fn parse_name_expr(&mut self) -> Result<NameExpr<'a>> {
        let span = self.start_span();
        let root_identifier = self.parse_identifier()?;
        let arena = self.arena;

        let value = match self.cur_kind() {
            Kind::PathSep => {
                NameExpr::Path(Box::new_in(self.parse_path(Some(root_identifier))?, arena))
            }
            Kind::Dot => NameExpr::Field(self.parse_field(root_identifier)?),
            _ => NameExpr::Ident(self.heap_alloc(root_identifier)),
        };

        Ok(value)
    }

    fn parse_field(&mut self, base_identifier: Identifier<'a>) -> Result<Box<'a, Field<'a>>> {
        let span = self.start_span();

        let mut base = FieldBase::Ident(self.heap_alloc(base_identifier));

        while self.eat(Kind::Dot) {
            let ident = self.parse_identifier()?;

            let field = Field::new(self.finish_span(span), base, ident);
            base = FieldBase::Field(self.heap_alloc(field));
        }

        match base {
            FieldBase::Field(field) => Ok(field),

            _ => unreachable!(),
        }
    }

    fn parse_continue_expr(&mut self) -> Result<ContinueExpr<'a>> {
        let span = self.start_span();
        self.expect(Kind::KwContinue);

        let label = self.parse_node_if(Kind::Label, Self::parse_label)?;

        Ok(ContinueExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            label,
        })
    }

    fn parse_break_expr(&mut self) -> Result<BreakExpr<'a>> {
        let span = self.start_span();
        self.expect(Kind::KwBreak);

        let label = self.parse_node_if(Kind::Label, Self::parse_label)?;

        Ok(BreakExpr {
            _marker: PhantomData,
            loc: self.finish_span(span),
            label,
        })
    }

    fn parse_label(&mut self) -> Result<Label<'a>> {
        let span = self.start_span();
        let label_tok = self.expect(Kind::Label)?;

        match label_tok.value {
            Value::Ident(atom) => Ok(Label::new(
                self.finish_span(span),
                Identifier::new(self.finish_span(span), atom),
            )),
            // SAFETY: Label kind can only have a ident value
            _ => unreachable!(),
        }
    }

    /// Return statements must end with semicolon, capturing trailing spans for tooling.
    fn parse_return_expr(&mut self) -> Result<ReturnExpr<'a>> {
        let span = self.start_span();
        self.expect(Kind::KwReturn)?;

        let value = self.parse_expr(0)?;

        self.expect(Kind::Semicolon)?;

        Ok(ReturnExpr::new(self.finish_span(span), value))
    }

    fn parse_paren_expr(&mut self) -> Result<Expr<'a>> {
        self.expect(Kind::LParen)?; // Consume leading paren
        let expr = self.parse_expr(0)?;
        self.expect(Kind::RParen)?; // Consume trailing paren

        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<UnaryExpr<'a>> {
        let span = self.start_span();

        // TODO: Should probably not hardcode this
        let operator =
            self.expect_any_of(&[Kind::Minus, Kind::Bang, Kind::BitNot, Kind::Asterisk])?;

        let rhs = self.parse_expr(Precedence::Unary.value())?;

        Ok(UnaryExpr::new(
            self.finish_span(span),
            UnaryOperator::from(operator.kind),
            rhs,
        ))
    }

    fn parse_literal(&mut self) -> Result<Literal<'a>> {
        let span = self.start_span();

        // TODO: Should probably not hardcode this
        let token = self.expect_any_of(&[
            Kind::BoolLiteral,
            Kind::IntLiteral,
            Kind::CharLiteral,
            Kind::FloatLiteral,
            Kind::StringLiteral,
        ])?;

        let span = self.finish_span(span);

        let lit = match token.kind {
            Kind::BoolLiteral => Literal::Bool(self.heap_alloc(BoolLiteral::new(
                span,
                match token.value {
                    Value::Bool(value) => value,
                    // SAFETY: A bool literal can only contain a Value::Bool
                    _ => unreachable!(),
                },
            ))),
            Kind::IntLiteral => Literal::Int(self.heap_alloc(IntLiteral::new(
                span,
                match token.value {
                    Value::Int(value) => value,
                    // SAFETY: A bool literal can only contain a Value::Int
                    _ => unreachable!(),
                },
            ))),
            Kind::CharLiteral => Literal::Char(self.heap_alloc(CharLiteral::new(
                span,
                match token.value {
                    Value::Char(value) => value,
                    // SAFETY: A char literal can only contain a Value::Char
                    _ => unreachable!(),
                },
            ))),
            Kind::FloatLiteral => Literal::Float(self.heap_alloc(FloatLiteral::new(
                span,
                match token.value {
                    Value::Float(value) => value,
                    // SAFETY: A float literal can only contain a Value::Float
                    _ => unreachable!(),
                },
            ))),
            Kind::StringLiteral => Literal::Str(self.heap_alloc(StrLiteral::new(
                span,
                match token.value {
                    Value::String(value) => value,
                    // SAFETY: A string literal can only contain a Value::String
                    _ => unreachable!(),
                },
            ))),
            // SAFETY: We checked that only the kind's above are possible here.
            _ => unreachable!(),
        };

        Ok(lit)
    }

    fn parse_function_item(&mut self, modifiers: Modifiers) -> Result<FuncItem<'a>> {
        let span = self.start_span();

        self.expect(Kind::KwFn);

        let return_ty = self.parse_type()?;
        let name = self.parse_identifier()?;
        let generic_args = self.parse_node_if(Kind::LessThan, Self::parse_generic_args)?;
        let parameters = self.parse_parameters()?;
        let body = self.parse_block()?;

        Ok(FuncItem {
            _marker: PhantomData,
            loc: self.finish_span(span),
            parameters,
            body,
            modifiers,
            generic_args,
            return_ty,
            name,
        })
    }

    fn parse_block(&mut self) -> Result<Block<'a>> {
        let span = self.start_span();
        self.expect(Kind::LBrace)?;

        let mut final_expr = None;
        let mut items = bumpalo::vec![in &self.arena;];

        while !self.at(Kind::Eof) && !self.at(Kind::RBrace) {
            // The tail expression of a block, needs to well be a expression
            // and not a declaration or statement. So only if the node is a expr
            // do we set final expresison if it does not end with a semicolon.

            if self.is_start_of_expr() {
                let expr = self.parse_expr(0)?;

                if self.eat(Kind::Semicolon) {
                    items.push(Stmt::Expr(ExprStmt::Expr(self.heap_alloc(expr))));
                    continue;
                } else {
                    final_expr = Some(expr);
                    break;
                }
            }

            let span = self.start_span();

            items.push(self.parse_statement());
        }

        self.expect(Kind::RBrace)?;

        Ok(Block::new(self.finish_span(span), None, items, final_expr))
    }

    #[inline(always)]
    fn is_start_of_expr(&mut self) -> bool {
        let kind = self.cur_kind();

        matches!(
            kind,
            Kind::Identifier
                | Kind::BoolLiteral
                | Kind::IntLiteral
                | Kind::FloatLiteral
                | Kind::CharLiteral
                | Kind::StringLiteral
                | Kind::KwReturn
                | Kind::LParen
                | Kind::Minus
                | Kind::Bang
                | Kind::Asterisk
                | Kind::LBrace
        )
    }

    fn parse_parameters(&mut self) -> Result<Punctuated<'a, Parameter<'a>>> {
        self.expect(Kind::LParen);

        let params = self.parse_punctuated(Self::parse_parameter, Kind::Comma, Kind::RParen)?;

        self.expect(Kind::RParen);

        Ok(params)
    }

    fn parse_parameter(&mut self) -> Result<Parameter<'a>> {
        let span = self.start_span();
        let ty = self.parse_type()?;

        if self.eat(Kind::Kwself) {
            return Ok(Parameter::SelfParam(self.heap_alloc(SelfParameter::new(
                self.finish_span(span),
                Modifiers::empty(),
            ))));
        }

        let name = self.parse_identifier()?;

        Ok(Parameter::Ident(self.heap_alloc(IdentParameter::new(
            self.finish_span(span),
            ty,
            name,
        ))))
    }

    fn parse_generic_args(&mut self) -> Result<GenericArgs<'a>> {
        let span = self.start_span();

        self.expect(Kind::LessThan);

        let args =
            self.parse_punctuated(Self::parse_generic_arg, Kind::Comma, Kind::GreaterThan)?;

        self.expect(Kind::GreaterThan);

        Ok(GenericArgs::new(self.finish_span(span), args))
    }

    #[inline(always)]
    fn parse_generic_arg(&mut self) -> Result<GenericArg<'a>> {
        let ty = self.parse_type()?;

        Ok(GenericArg::Ty(self.heap_alloc(ty)))
    }

    #[inline(always)]
    fn parse_type(&mut self) -> Result<Type<'a>> {
        let span = self.start_span();

        let path = self.parse_type_path()?;

        Ok(Type::new(self.finish_span(span), path))
    }

    fn parse_type_path(&mut self) -> Result<TypePath<'a>> {
        let span = self.start_span();

        let mut path_segments: Vec<'a, TypePathSegment<'a>> = bumpalo::vec![in &self.arena;];

        loop {
            if self.at(Kind::Eof) {
                break;
            }

            let span = self.start_span();

            let reference = self.eat(Kind::Ampersand);
            let mutable_specifier = self.eat(Kind::KwMut);

            let ty = self.parse_ty()?;

            let inner = self.heap_alloc(ty);

            let ty: Box<'a, Ty<'a>> = match (reference, mutable_specifier) {
                (true, true) => self.heap_alloc(Ty::MutRef(inner)),
                (true, false) => self.heap_alloc(Ty::Ref(inner)),
                (false, true) => self.heap_alloc(Ty::Mut(inner)),
                (false, false) => inner,
            };

            if !self.eat(Kind::PathSep) {
                path_segments.push(TypePathSegment::new(self.finish_span(span), ty));

                break;
            }

            path_segments.push(TypePathSegment::new(self.finish_span(span), ty));

            self.eat(Kind::PathSep);
        }

        Ok(TypePath::new(
            self.finish_span(span),
            Punctuated::new(self.finish_span(span), path_segments),
        ))
    }

    #[inline(always)]
    fn parse_ty(&mut self) -> Result<Ty<'a>> {
        let token = self.cur_token.clone();

        match token.kind {
            Kind::PrimitiveType => {
                self.advance();
                Ok(self.parse_primitive_type_value(token.value))
            }
            Kind::Identifier => Ok(Ty::Ident(self.parse_identifier()?)),
            Kind::Kwself => Ok(Ty::Tyself),
            Kind::KwSelf => Ok(Ty::TySelf),

            c => unreachable!(),
        }
    }

    #[inline(always)]
    fn parse_primitive_type_value(&mut self, value: Value) -> Ty<'a> {
        match value {
            Value::Ident(atom) => match atom {
                atom!("isz") => Ty::Isz,
                atom!("usz") => Ty::Usz,
                atom!("u8") => Ty::U8,
                atom!("u16") => Ty::U16,
                atom!("u32") => Ty::U32,
                atom!("u64") => Ty::U64,
                atom!("u128") => Ty::U128,
                atom!("i8") => Ty::I8,
                atom!("i16") => Ty::I16,
                atom!("i32") => Ty::I32,
                atom!("void") => Ty::Void,
                atom!("i64") => Ty::I64,
                atom!("i128") => Ty::I128,
                atom!("char") => Ty::Char,
                atom!("str") => Ty::Str,
                atom!("infer") => Ty::Infer,
                atom!("float") => Ty::Float,
                atom!("double") => Ty::Double,
                // SAFETY: There are no other nor can exist any other primitive types
                _ => unreachable!(),
            },

            // SAFETY: The primitive_type token kind cannot have any other
            // value other than `Ident`.
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    fn parse_path(&mut self, root_identifier: Option<Identifier<'a>>) -> Result<Path<'a>> {
        let mut span: Span;

        let mut path_segments = bumpalo::vec![in &self.arena;];

        if let Some(ident) = root_identifier {
            span = ident.loc;
            path_segments.push(self.parse_path_segment(Some(ident))?);
        } else {
            cold_path(); // We mostly call the parse_path function with a root identifier

            let seg = self.parse_path_segment(None)?;
            span = seg.loc;
            path_segments.push(seg)
        }

        while self.at(Kind::Identifier) {
            let span = self.start_span();

            path_segments.push(self.parse_path_segment(None)?);
        }

        Ok(Path::new(
            self.finish_span(span),
            Punctuated::new(self.finish_span(span), path_segments),
        ))
    }

    #[inline(always)]
    fn parse_path_segment(&mut self, ident: Option<Identifier<'a>>) -> Result<PathSegment<'a>> {
        let span = self.start_span();

        let ident = if let Some(ident) = ident {
            Identifier::new(ident.loc, ident.name)
        } else {
            self.parse_identifier()?
        };

        if !self.eat(Kind::PathSep) {
            return Ok(PathSegment::new(
                self.finish_span(span),
                ident,
                PathArguments::None,
            ));
        }

        let arguments = self
            .parse_node_if(Kind::LessThan, Self::parse_path_arguments)?
            .unwrap_or_else(|| PathArguments::None);

        Ok(PathSegment::new(self.finish_span(span), ident, arguments))
    }

    #[inline(always)]
    fn parse_path_arguments(&mut self) -> Result<PathArguments<'a>> {
        let span = self.start_span();
        let args = self.parse_generic_args()?;

        self.eat(Kind::PathSep);

        Ok(PathArguments::Generic(self.heap_alloc(args)))
    }

    #[inline(always)]
    fn parse_identifier(&mut self) -> Result<Identifier<'a>> {
        let token = self.expect(Kind::Identifier)?;

        match token.value {
            Value::Ident(atom) => Ok(Identifier::new(token.span, atom)),

            // SAFETY: The identifier token kind cannot have any other
            // value other than `Ident`.
            c => unreachable!(),
        }
    }

    #[inline(always)]
    fn recoverable<U, ParseFn, Dummy>(
        &mut self,
        start_span: Span,
        sync_kinds: &[Kind],
        parse_fn: ParseFn,
        dummy_factory: Dummy,
    ) -> U
    where
        ParseFn: FnOnce(&mut Self) -> Result<U>,
        Dummy: FnOnce(Span) -> U,
    {
        match parse_fn(self) {
            Ok(value) => value,
            Err(err) => {
                cold_path();

                let diag = err.into_diagnostic::<'a, F>(self.file_id);

                self.diagnostics.push(diag);

                self.skip_to(sync_kinds);

                dummy_factory(self.finish_span(start_span))
            }
        }
    }

    #[inline(always)]
    fn skip_to(&mut self, sync_set: &[Kind]) {
        while !sync_set.contains(&self.cur_kind()) && !self.at(Kind::Eof) {
            debug!("Skipping {:?}", self.cur_kind());
            self.advance();
        }

        // if we’re on a sync token, consume it so we don’t loop forever
        if self.cur_kind() != Kind::Eof {
            self.advance();
        }
    }

    #[inline(always)]
    fn parse_punctuated<P, U>(
        &mut self,
        parse_fn: P,
        delimiter: Kind,
        closing_kind: Kind,
    ) -> Result<Punctuated<'a, U>>
    where
        P: Fn(&mut Self) -> Result<U>,
    {
        let span = self.start_span();

        let mut nodes = bumpalo::vec![in &self.arena;];

        if self.at(closing_kind) {
            return Ok(Punctuated::new(self.finish_span(span), nodes));
        };

        nodes.push(parse_fn(self)?);

        while self.eat(delimiter) {
            nodes.push(parse_fn(self)?);
        }

        Ok(Punctuated::new(self.finish_span(span), nodes))
    }

    #[inline(always)]
    fn parse_node_if<P, U>(&mut self, clause: Kind, parse_fn: P) -> Result<Option<U>>
    where
        P: FnOnce(&mut Self) -> Result<U>,
    {
        if self.at(clause) {
            let node = parse_fn(self)?;
            Ok(Some(node))
        } else {
            Ok(None)
        }
    }

    #[inline(always)]
    fn start_span(&self) -> Span {
        let token = self.cur_token();
        Span::new(token.span.start, 0)
    }

    #[inline(always)]
    fn finish_span(&self, span: Span) -> Span {
        Span::new(span.start, self.prev_token_end)
    }

    #[inline(always)]
    fn cur_token(&self) -> &Token<'a> {
        &self.cur_token
    }

    #[inline(always)]
    fn cur_kind(&self) -> Kind {
        self.cur_token.kind
    }

    #[inline(always)]
    fn cur_value(&self) -> &Value<'a> {
        &self.cur_token.value
    }

    #[inline(always)]
    fn at(&self, kind: Kind) -> bool {
        self.cur_kind() == kind
    }

    #[inline(always)]
    fn bump(&mut self, kind: Kind) -> Option<Token<'a>> {
        if self.at(kind) {
            return Some(self.advance());
        }
        None
    }

    #[inline(always)]
    fn eat(&mut self, kind: Kind) -> bool {
        if self.at(kind) {
            self.advance();
            return true;
        }
        false
    }

    #[inline(always)]
    fn expect_any_of(&mut self, expected_kinds: &[Kind]) -> Result<Token<'a>> {
        let start = self.start_span();
        let cur_token = self.cur_token.clone();

        let kind = self.cur_kind();
        if expected_kinds.contains(&kind) {
            self.advance();
            return Ok(cur_token);
        }

        cold_path();

        shortcircuit!(ParseError::UnexpectedToken {
            found: kind,
            expected: expected_kinds.to_vec(),
            span: self.finish_span(start),
        })
    }

    #[inline(always)]
    fn expect(&mut self, expected_kind: Kind) -> Result<Token<'a>> {
        let start = self.start_span();
        let cur_token = self.cur_token.clone();

        if self.at(expected_kind) {
            self.advance();
            return Ok(cur_token);
        }

        cold_path();

        let kind = self.cur_kind();

        shortcircuit!(ParseError::UnexpectedToken {
            found: kind,
            expected: vec![expected_kind],
            span: self.finish_span(start),
        })
    }

    #[inline(always)]
    fn advance(&mut self) -> Token<'a> {
        let cur_token = self.cur_token.clone();

        let token = self.lexer.read_next_token();
        self.prev_token_end = self.cur_token.span.end;
        self.cur_token = token.clone();

        cur_token
    }
}
