use super::{
    AliasDecl, AliasDef, ConstDecl, ConstDef, FuncDecl, FuncDef, GenericsDecl, TypeRef, WhereClause,
};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::utils::check_enum,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

pub struct TraitDecl;

impl ParseNode for TraitDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwTrait))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwTrait);
        context.next_of(TokenKind::Ident);
        context.parse::<GenericsDecl>();
        context.parse::<WhereClause>();
        context.next_of(TokenKind::LBrace);
        // TODO: Seq<TraitItemDecl>.
        context.next_of(TokenKind::RBrace);

        0
    }
}

pub struct ImplBlock;

impl ParseNode for ImplBlock {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwImpl))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> usize {
        context.next_of(TokenKind::KwImpl);
        context.parse::<GenericsDecl>();

        // context.parse::<TypeRef>();
        // if context.next_if(TokenKind::KwFor) {
        //     // TODO: TypeRef
        // }
        //
        // Trait: PathTypeRef 'for' TypeRef
        // Type : TypeRef (incl. PathTypeRef)

        context.parse::<WhereClause>();
        context.next_of(TokenKind::LBrace);
        // TODO: TraitItemDef.
        context.next_of(TokenKind::RBrace);

        0
    }
}

pub struct ImplItemDecl;

impl ParseNode for ImplItemDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDecl::check(kind),
            ConstDecl::check(kind),
            FuncDecl::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> usize {
        match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDecl>();

                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDecl>();

                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDecl>();

                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        }
    }
}

pub struct ImplItemDef;

impl ParseNode for ImplItemDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDef::check(kind),
            ConstDef::check(kind),
            FuncDef::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> usize {
        match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDef>();

                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDef>();

                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDef>();

                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        }
    }
}

pub struct Statement;

impl ParseNode for Statement {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // TODO: Let binding.
        // TODO: Let destructure.
        // TODO: Expression.

        todo!()
    }

    fn parse(context: &mut ParseContext) -> usize {
        todo!()
    }
}
