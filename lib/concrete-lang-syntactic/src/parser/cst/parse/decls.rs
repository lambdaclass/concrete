//! Subcategories:
//!   - Alias, Const, Function: Are trait/impl items.
//!   - Enum, Struct, Union: Are data structures.
//!   - Trait, Impl: Are interfaces.

pub use self::{behavior::*, mixed::*, structure::*};
use super::{
    exprs::Expression,
    utils::{Angles, Braces, CommaSep, check_enum},
};
use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

mod behavior;
mod mixed;
mod structure;

pub struct TypeRef;

impl ParseNode for TypeRef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        todo!()
    }
}

/// A generics declaration, optionally supporting defaults.
///
/// Declares available generics for an item, with optional defaults.
///
/// > Note: Not to be confused with `GenericsDef`.
// TODO: Check proper WITH_DEFAULTS setting.
pub struct GenericsDecl<const WITH_DEFAULTS: bool = false>;

impl<const WITH_DEFAULTS: bool> ParseNode for GenericsDecl<WITH_DEFAULTS> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Angles::<CommaSep<GenericDecl>>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Angles<CommaSep<GenericDecl>>>()?;
        Ok(0)
    }
}

/// A generic declaration item.
pub struct GenericDecl<const WITH_DEFAULTS: bool = false>;

impl<const WITH_DEFAULTS: bool> ParseNode for GenericDecl<WITH_DEFAULTS> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            // TODO: Lifetimes.
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            (kind == Some(TokenKind::KwConst))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        // TODO: Check declaration order (lifetimes, then generic types, then generic consts).
        // TODO: Check defaults order (without defaults, then with defaults).
        Ok(match Self::check(context.peek()) {
            // TODO: Lifetimes.
            CheckResult::Always(0) => {
                context.next_of(TokenKind::Ident)?;
                if WITH_DEFAULTS && context.next_if(TokenKind::SymAssign) {
                    context.parse::<TypeRef>()?;
                }
                0
            }
            CheckResult::Always(1) => {
                context.next_of(TokenKind::KwConst)?;
                context.next_of(TokenKind::Ident)?;
                if WITH_DEFAULTS && context.next_if(TokenKind::SymAssign) {
                    context.parse::<Expression>()?;
                }
                1
            }
            CheckResult::Always(_) => unreachable!(),
            CheckResult::Empty(_) => todo!(),
            CheckResult::Never => todo!(),
        })
    }
}

/// A generics definition.
///
/// Defines the value of available generics for an item.
///
/// > Note: Not to be confused with `GenericsDecl`.
pub struct GenericsDef;

impl ParseNode for GenericsDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Angles::<CommaSep<GenericDef>>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Angles<CommaSep<GenericDef>>>()?;
        Ok(0)
    }
}

/// A generic definition item.
pub struct GenericDef;

impl ParseNode for GenericDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            // TODO: Lifetimes.
            TypeRef::check(kind),
            Braces::<Expression>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        // TODO: Check declaration order (lifetimes, then generic types, then generic consts).
        // TODO: Check defaults order (without defaults, then with defaults).
        Ok(match Self::check(context.peek()) {
            // TODO: Lifetimes.
            CheckResult::Always(0) => {
                context.parse::<TypeRef>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<Braces<Expression>>()?;
                1
            }
            CheckResult::Always(_) => unreachable!(),
            CheckResult::Empty(_) => todo!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub struct WhereClause;

impl ParseNode for WhereClause {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwWhere))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwWhere)?;
        todo!()
    }
}
