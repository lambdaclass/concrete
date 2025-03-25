//! # Parser for item declarations
//!
//! Items can appear either standalone or within traits or impl blocks:
//!   - Type aliases: `AliasDecl` and `AliasDef`.
//!   - Constants: `ConstDecl` and `ConstDef`.
//!   - Functions: `FuncDecl` and `FuncDef`.
//!
//! The `Decl` parsers are used for declarations, and may or may not have a default value. The `Def`
//! variants must always have a value.

use super::{GenericsDecl, TypeRef, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::exprs::{BlockExpr, Expression},
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

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwType);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        let has_value = context.next_if(TokenKind::SymAssign);
        if has_value {
            context.parse::<TypeRef>();
        }
        context.parse::<WhereClause>();
        context.next_of(TokenKind::SymSemi);

        has_value as usize
    }
}

pub struct AliasDef;

impl ParseNode for AliasDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwType))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwType);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        context.next_of(TokenKind::SymAssign);
        context.parse::<TypeRef>();
        context.next_of(TokenKind::SymSemi);

        0
    }
}

pub struct ConstDecl;

impl ParseNode for ConstDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwConst))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwConst);
        context.next_of(TokenKind::Ident);
        context.next_of(TokenKind::SymColon);
        context.parse::<TypeRef>();
        let has_value = context.next_if(TokenKind::SymAssign);
        if has_value {
            context.parse::<Expression>();
        }
        context.next_of(TokenKind::SymSemi);

        has_value as usize
    }
}

pub struct ConstDef;

impl ParseNode for ConstDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwConst))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwConst);
        context.next_of(TokenKind::Ident);
        context.next_of(TokenKind::SymColon);
        context.parse::<TypeRef>();
        context.next_of(TokenKind::SymAssign);
        context.parse::<Expression>();
        context.next_of(TokenKind::SymSemi);

        0
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

    fn parse(context: &mut ParseContext) -> usize {
        let has_abi = context.next_if(TokenKind::KwExtern);
        if has_abi {
            context.next_of(TokenKind::LitString);
        }
        context.next_of(TokenKind::KwFn);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        // TODO: context.parse::<Parens<CommaSep<FieldDecl>>>();
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>();
        }
        context.parse::<WhereClause>();
        let has_body = BlockExpr::check(context.peek()).is_always();
        if has_body {
            context.parse::<BlockExpr>();
        }

        has_abi as usize | ((has_return_type as usize) << 1) | ((has_body as usize) << 2)
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

    fn parse(context: &mut ParseContext) -> usize {
        let has_abi = context.next_if(TokenKind::KwExtern);
        if has_abi {
            context.next_of(TokenKind::LitString);
        }
        context.next_of(TokenKind::KwFn);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        // TODO: context.parse::<Parens<CommaSep<FieldDecl>>>();
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>();
        }
        context.parse::<WhereClause>();
        context.parse::<BlockExpr>();

        has_abi as usize | ((has_return_type as usize) << 1)
    }
}
