//! # Parser for item declarations
//!
//! Items can appear either standalone or within traits or impl blocks:
//!   - Type aliases: `AliasDecl` and `AliasDef`.
//!   - Constants: `ConstDecl` and `ConstDef`.
//!   - Functions: `FuncDecl` and `FuncDef`.
//!
//! The `Decl` parsers are used for declarations, and may or may not have a default value. The `Def`
//! variants must always have a value.

use super::{Field, FieldVisit, GenericsDecl, TypeRef, TypeRefVisit, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::{
            exprs::{BlockExpr, BlockExprVisit, Expression, ExpressionVisit},
            utils::{Angles, CommaSep, CommaSepVisit, Parens, ParensVisit},
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
        storage::TreeNodeVisit,
    },
};
use std::fmt;

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
        context.parse::<Option<Angles<GenericsDecl>>>()?;
        let has_value = context.next_if(TokenKind::SymAssign);
        if has_value {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(has_value as usize)
    }
}

pub struct AliasDeclVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> AliasDeclVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn target(&self) -> Option<TypeRefVisit<'storage>> {
        todo!()
    }

    // TODO: WhereClause.
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
        context.parse::<Option<Angles<GenericsDecl>>>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<TypeRef>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}

pub struct AliasDefVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> AliasDefVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn target(&self) -> TypeRefVisit<'storage> {
        todo!()
    }

    // TODO: WhereClause.
}

impl<'storage> From<TreeNodeVisit<'storage>> for AliasDefVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        todo!()
    }
}

impl fmt::Display for AliasDefVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
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

pub struct ConstDeclVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> ConstDeclVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn r#type(&self) -> TypeRefVisit<'storage> {
        todo!()
    }

    pub fn value(&self) -> Option<ExpressionVisit<'storage>> {
        todo!()
    }

    // TODO: WhereClause.
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

pub struct ConstDefVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> ConstDefVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn r#type(&self) -> TypeRefVisit<'storage> {
        todo!()
    }

    pub fn value(&self) -> ExpressionVisit<'storage> {
        todo!()
    }

    // TODO: WhereClause.
}

impl<'storage> From<TreeNodeVisit<'storage>> for ConstDefVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        todo!()
    }
}

impl fmt::Display for ConstDefVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

pub struct FuncDecl;

impl ParseNode for FuncDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwFn))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<Angles<GenericsDecl>>>()?;
        context.parse::<Parens<CommaSep<Field<TypeRef>>>>()?;
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        // TODO: Maybe merge with `FuncDef` by making the body mandatory.
        let has_body = BlockExpr::check(context.peek()).is_always();
        if has_body {
            context.parse::<BlockExpr>()?;
        } else {
            context.next_of(TokenKind::SymSemi)?;
        }

        Ok((has_return_type as usize) | ((has_body as usize) << 1))
    }
}

pub struct FuncDeclVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> FuncDeclVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn args(
        &self,
    ) -> ParensVisit<'storage, CommaSepVisit<'storage, FieldVisit<'storage, TypeRefVisit<'storage>>>>
    {
        todo!()
    }

    pub fn return_type(&self) -> Option<TypeRefVisit<'storage>> {
        todo!()
    }

    // TODO: WhereClause.

    pub fn body(&self) -> Option<BlockExprVisit<'storage>> {
        todo!()
    }
}

pub struct FuncDef;

impl ParseNode for FuncDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwFn))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<Angles<GenericsDecl>>>()?;
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

pub struct FuncDefVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> FuncDefVisit<'storage> {
    pub fn ident(&self) -> &str {
        todo!()
    }

    // TODO: Generics.

    pub fn args(
        &self,
    ) -> ParensVisit<'storage, CommaSepVisit<'storage, FieldVisit<'storage, TypeRefVisit<'storage>>>>
    {
        todo!()
    }

    pub fn return_type(&self) -> Option<TypeRefVisit<'storage>> {
        todo!()
    }

    // TODO: WhereClause.

    pub fn body(&self) -> BlockExprVisit<'storage> {
        todo!()
    }
}

impl<'storage> From<TreeNodeVisit<'storage>> for FuncDefVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        Self(value)
    }
}

impl fmt::Display for FuncDefVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}
