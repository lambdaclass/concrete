//! # Parser for item declarations
//!
//! Items can appear either standalone or within traits or impl blocks:
//!   - Type aliases: `AliasDecl` and `AliasDef`.
//!   - Constants: `ConstDecl` and `ConstDef`.
//!   - Functions: `FuncDecl` and `FuncDef`.
//!
//! The `Decl` parsers are used for declarations, and may or may not have a default value. The `Def`
//! variants must always have a value.

use super::{Field, GenericsDecl, TypeRef, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::{
            exprs::{BlockExpr, Expression},
            utils::{CommaSep, Parens},
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

pub struct AliasDecl;

impl ParseNode for AliasDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwType))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwType)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        let has_value = context.next_if(TokenKind::SymAssign);
        if has_value {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(has_value as usize)
    }
}

pub struct AliasDef;

impl ParseNode for AliasDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwType))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwType)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<TypeRef>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}

pub struct ConstDecl;

impl ParseNode for ConstDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwConst))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwConst)?;
        context.next_of(TokenKind::Ident)?;
        context.next_of(TokenKind::SymColon)?;
        context.parse::<TypeRef>()?;
        let has_value = context.next_if(TokenKind::SymAssign);
        if has_value {
            context.parse::<Expression>()?;
        }
        context.next_of(TokenKind::SymSemi)?;

        Ok(has_value as usize)
    }
}

pub struct ConstDef;

impl ParseNode for ConstDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwConst))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwConst)?;
        context.next_of(TokenKind::Ident)?;
        context.next_of(TokenKind::SymColon)?;
        context.parse::<TypeRef>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<Expression>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}

pub struct FuncDecl;

impl ParseNode for FuncDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::KwExtern) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| {
            (kind == Some(TokenKind::KwFn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default()
        })
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Parens<CommaSep<Field<TypeRef>>>>()?;
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        // TODO: Disable optional body for FfiBlock functions.
        // TODO: Maybe merge with `FuncDef` by making the body mandatory.
        let has_body = BlockExpr::check(context.peek()).is_always();
        if has_body {
            context.parse::<BlockExpr>()?;
        }

        Ok((has_return_type as usize) | ((has_body as usize) << 1))
    }
}

pub struct FuncDef;

impl ParseNode for FuncDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::KwExtern) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| {
            (kind == Some(TokenKind::KwFn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default()
        })
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Parens<CommaSep<Field<TypeRef>>>>()?;
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        context.parse::<BlockExpr>()?;

        Ok(has_return_type as usize)
    }
}
