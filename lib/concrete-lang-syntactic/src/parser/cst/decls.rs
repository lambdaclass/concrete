//! Subcategories:
//!   - Alias, Const, Function: Are trait/impl items.
//!   - Enum, Struct, Union: Are data structures.
//!   - Trait, Impl: Are interfaces.

pub use self::{behavior::*, mixed::*, structure::*};
use super::{
    exprs::Expression,
    utils::{Angles, Brackets, CommaSep, Ident, Parens, check_enum},
};
use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
        storage::TreeNodeVisit,
    },
};
use std::marker::PhantomData;

mod behavior;
mod mixed;
mod structure;

pub struct TypeRef;

impl ParseNode for TypeRef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            ArrayType::check(kind),
            PathType::check(kind),
            PtrType::check(kind),
            RefType::check(kind),
            TupleType::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<ArrayType>()?;
                0
            }
            Some(1) => {
                context.parse::<PathType>()?;
                1
            }
            Some(2) => {
                context.parse::<PtrType>()?;
                2
            }
            Some(3) => {
                context.parse::<RefType>()?;
                3
            }
            Some(4) => {
                context.parse::<TupleType>()?;
                4
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub struct TypeRefVisit<'storage>(TreeNodeVisit<'storage>);

pub struct PathType;

impl ParseNode for PathType {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::SymScope) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| PathSegment::check(kind))
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_if(TokenKind::SymScope);
        loop {
            context.parse::<PathSegment>()?;
            if !context.next_if(TokenKind::SymScope) {
                break;
            }
        }

        Ok(0)
    }
}

pub struct PathTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub struct PathSegment;

impl ParseNode for PathSegment {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            Angles::<GenericsDef>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.next_of(TokenKind::Ident)?;
                0
            }
            Some(1) => {
                context.parse::<Angles<GenericsDef>>()?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub struct PathSegmentVisit<'storage>(TreeNodeVisit<'storage>);

pub struct ArrayType;

impl ParseNode for ArrayType {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Brackets::<ArrayDef>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Brackets<ArrayDef>>()?;
        Ok(0)
    }
}

pub struct ArrayTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub struct TupleType;

impl ParseNode for TupleType {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Parens::<CommaSep<TypeRef>>::check(kind).map(0)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Parens<CommaSep<TypeRef>>>()?;
        Ok(0)
    }
}

pub struct TupleTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub struct RefType;

impl ParseNode for RefType {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpBitAnd))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpBitAnd)?;
        // TODO: Lifetime.
        let is_mut = context.next_if(TokenKind::KwMut);
        context.parse::<TypeRef>()?;

        Ok(is_mut as usize)
    }
}

pub struct RefTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub struct PtrType;

impl ParseNode for PtrType {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymOpMul))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymOpMul)?;
        let is_mut = if context.next_if(TokenKind::KwConst) {
            0
        } else if context.next_if(TokenKind::KwMut) {
            1
        } else {
            todo!()
        };
        context.parse::<TypeRef>()?;

        Ok(is_mut as usize)
    }
}

pub struct PtrTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub type GenericsDecl<const ALLOW_DEFAULT: bool = false> = Generics<Ident, ALLOW_DEFAULT>;
pub type GenericsDef = Generics<TypeRef, false>;

pub struct Generics<T, const ALLOW_DEFAULT: bool>(PhantomData<T>);

impl<T, const ALLOW_DEFAULTS: bool> ParseNode for Generics<T, ALLOW_DEFAULTS>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // TODO: CommaSep::<Lifetime>::check(kind)
        CommaSep::<GenericType<T, ALLOW_DEFAULTS>>::check(kind)
            .followed_by(|| CommaSep::<GenericConst<ALLOW_DEFAULTS>>::check(kind))
            .followed_by(|| CheckResult::Empty(0))
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        type CommaSepNoTrail<T, const ALLOW_DEFAULTS: bool> =
            CommaSep<GenericType<T, ALLOW_DEFAULTS>, { usize::MIN }, { usize::MAX }, false>;

        // TODO: Lifetimes.
        // if check_lifetime {
        //     parse_lifetime();
        //     if !context.next_if(TokenKind::SymComma) {
        //         return Ok(0);
        //     }
        // }

        if GenericType::<T, ALLOW_DEFAULTS>::check(context.peek()).is_always() {
            context.parse::<CommaSepNoTrail<GenericType<T, ALLOW_DEFAULTS>, ALLOW_DEFAULTS>>()?;
            if !context.next_if(TokenKind::SymComma) {
                return Ok(0);
            }
        }

        if GenericConst::<ALLOW_DEFAULTS>::check(context.peek()).is_always() {
            context.parse::<CommaSepNoTrail<GenericConst<ALLOW_DEFAULTS>, ALLOW_DEFAULTS>>()?;
            context.next_if(TokenKind::SymComma);
        }

        Ok(0)
    }
}

pub struct GenericsVisit<'storage>(TreeNodeVisit<'storage>);

pub struct GenericType<T, const ALLOW_DEFAULTS: bool>(PhantomData<T>);

impl<T, const ALLOW_DEFAULTS: bool> ParseNode for GenericType<T, ALLOW_DEFAULTS>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        T::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<T>()?;
        Ok(if ALLOW_DEFAULTS && context.next_if(TokenKind::SymAssign) {
            context.parse::<Expression>()?;
            1
        } else {
            0
        })
    }
}

pub struct GenericTypeVisit<'storage>(TreeNodeVisit<'storage>);

pub struct GenericConst<const ALLOW_DEFAULTS: bool>;

impl<const ALLOW_DEFAULTS: bool> ParseNode for GenericConst<ALLOW_DEFAULTS> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwConst))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwConst)?;
        context.next_of(TokenKind::Ident)?;

        Ok(0)
    }
}

pub struct GenericConstVisit<'storage>(TreeNodeVisit<'storage>);

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

pub struct WhereClauseVisit<'storage>(TreeNodeVisit<'storage>);
