use super::{GenericsDecl, TypeRef, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::{
            exprs::Expression,
            utils::{Braces, CommaSep, Parens, check_enum},
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

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
        context.parse::<GenericsDecl>()?;
        context.parse::<WhereClause>()?;
        context.parse::<Braces<CommaSep<VariantDecl>>>()?;

        Ok(0)
    }
}

pub struct VariantDecl;

impl ParseNode for VariantDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        context.parse::<FieldsDecl>()?;

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
        context.parse::<GenericsDecl>()?;
        context.parse::<WhereClause>()?;
        if context.parse::<FieldsDecl>()? != 2 {
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
        context.parse::<GenericsDecl>()?;
        context.parse::<WhereClause>()?;
        context.parse::<FieldsDecl>()?;
        if context.parse::<FieldsDecl>()? != 2 {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok(0)
    }
}

pub struct FieldsDecl;

impl ParseNode for FieldsDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            CheckResult::Empty(0),
            Parens::<CommaSep<TypeRef>>::check(kind),
            Braces::<CommaSep<FieldDecl>>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Empty(0) => 0,
            CheckResult::Always(1) => {
                context.parse::<CommaSep<TypeRef>>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<CommaSep<FieldDecl>>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub struct FieldDecl;

impl ParseNode for FieldDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        context.next_of(TokenKind::SymColon)?;
        context.parse::<TypeRef>()?;

        Ok(0)
    }
}
