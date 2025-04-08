use super::{GenericsDecl, TypeRef, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::{
            exprs::Expression,
            utils::{Angles, Braces, CommaSep, Parens, check_enum},
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
        storage::TreeNodeVisit,
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
        context.parse::<Option<Angles<GenericsDecl<true>>>>()?;
        context.parse::<Option<WhereClause>>()?;
        context.parse::<Braces<CommaSep<NamedFields<TypeRef>>>>()?;

        Ok(0)
    }
}

pub struct EnumDefVisit<'storage>(TreeNodeVisit<'storage>);

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
        context.parse::<Option<Angles<GenericsDecl<true>>>>()?;
        context.parse::<Option<WhereClause>>()?;
        if context.parse::<Fields<TypeRef>>()? != 0 {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok(0)
    }
}

pub struct StructDefVisit<'storage>(TreeNodeVisit<'storage>);

pub struct UnionDef;

impl ParseNode for UnionDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwUnion))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwUnion)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<Angles<GenericsDecl<true>>>>()?;
        context.parse::<Option<WhereClause>>()?;
        if context.parse::<Fields<TypeRef>>()? != 0 {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok(0)
    }
}

pub struct UnionDefVisit<'storage>(TreeNodeVisit<'storage>);

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

pub struct NamedFieldsVisit<'storage, T>(TreeNodeVisit<'storage>, PhantomData<T>);

pub struct Fields<T>(PhantomData<T>);

impl<T> ParseNode for Fields<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            Braces::<CommaSep<Field<T>>>::check(kind),
            Parens::<CommaSep<T>>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Braces<CommaSep<Field<T>>>>()?;
                0
            }
            Some(1) => {
                context.parse::<Parens<CommaSep<T>>>()?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub struct FieldsVisit<'storage>(TreeNodeVisit<'storage>);

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

pub struct FieldVisit<'storage, T>(TreeNodeVisit<'storage>, PhantomData<T>);

pub struct ArrayDef;

impl ParseNode for ArrayDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        TypeRef::check(kind).map(0)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<TypeRef>()?;
        context.next_of(TokenKind::SymSemi)?;
        context.parse::<Expression>()?;

        Ok(0)
    }
}

pub struct ArrayDefVisit<'storage>(TreeNodeVisit<'storage>);
