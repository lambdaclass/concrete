use super::{GenericsDecl, TypeRef, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::utils::{Braces, CommaSep, Parens, check_enum},
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};
use std::marker::PhantomData;

pub struct EnumDef;

impl ParseNode for EnumDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwEnum))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwEnum)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Option<WhereClause>>()?;
        context.parse::<Braces<CommaSep<NamedFields<TypeRef>>>>()?;

        Ok(0)
    }
}

pub struct StructDef;

impl ParseNode for StructDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwStruct))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwStruct)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Option<WhereClause>>()?;
        if context.parse::<Fields<TypeRef>>()? != 2 {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok(0)
    }
}

pub struct UnionDef;

impl ParseNode for UnionDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwUnion))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwStruct)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Option<WhereClause>>()?;
        if context.parse::<Fields<TypeRef>>()? != 0 {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok(0)
    }
}

pub struct NamedFields<T>(PhantomData<T>);

impl<T> ParseNode for NamedFields<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        let is_braced = context.parse::<Option<Fields<T>>>()? == 0;

        Ok(is_braced as usize)
    }
}

pub struct Fields<T>(PhantomData<T>);

impl<T> ParseNode for Fields<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([Braces::<T>::check(kind), Parens::<Field<T>>::check(kind)])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Braces<T>>()?;
                0
            }
            Some(1) => {
                context.parse::<Parens<Field<T>>>()?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub struct Field<T>(PhantomData<T>);

impl<T> ParseNode for Field<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        context.next_of(TokenKind::SymColon)?;
        context.parse::<T>()?;

        Ok(0)
    }
}
