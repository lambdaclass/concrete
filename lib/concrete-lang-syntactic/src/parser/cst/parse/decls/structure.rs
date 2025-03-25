use super::{GenericsDecl, WhereClause};
use crate::{
    lexer::TokenKind,
    parser::parse::{CheckResult, ParseContext, ParseNode},
};

pub struct EnumDef;

impl ParseNode for EnumDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwEnum))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwEnum);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        context.parse::<WhereClause>();
        context.next_of(TokenKind::LBrace);
        // TODO: Variants.
        context.next_of(TokenKind::RBrace);

        0
    }
}

pub struct StructDef;

impl ParseNode for StructDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwStruct))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwStruct);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        context.parse::<WhereClause>();
        // TODO: Option<FieldsDecl>.

        0
    }
}

pub struct UnionDef;

impl ParseNode for UnionDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwUnion))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwStruct);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        context.parse::<WhereClause>();
        // TODO: FieldsDecl.
        context.next_of(TokenKind::SymSemi);

        0
    }
}
