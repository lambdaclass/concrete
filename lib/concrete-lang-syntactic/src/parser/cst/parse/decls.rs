//! Subcategories:
//!   - Alias, Const, Function: Are trait/impl items.
//!   - Enum, Struct, Union: Are data structures.
//!   - Trait, Impl: Are interfaces.

pub use self::{
    behavior::{ImplBlock, Statement, TraitDecl},
    mixed::{AliasDecl, AliasDef, ConstDecl, ConstDef, FuncDecl, FuncDef},
    structure::{EnumDef, StructDef, UnionDef},
};
use crate::{
    lexer::TokenKind,
    parser::parse::{CheckResult, ParseContext, ParseNode},
};

mod behavior;
mod mixed;
mod structure;

pub struct TypeRef;

impl ParseNode for TypeRef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> usize {
        todo!()
    }
}

pub struct GenericsDecl;

impl ParseNode for GenericsDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> usize {
        todo!()
    }
}

pub struct GenericsDef;

impl ParseNode for GenericsDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> usize {
        todo!()
    }
}

pub struct WhereClause;

impl ParseNode for WhereClause {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> usize {
        todo!()
    }
}
