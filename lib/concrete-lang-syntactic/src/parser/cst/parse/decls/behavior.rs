use super::{
    AliasDecl, AliasDef, ConstDecl, ConstDef, FuncDecl, FuncDef, GenericsDecl, NamedFields,
    TypeRef, WhereClause,
};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::{
            exprs::Expression,
            mods::ModuleItem,
            utils::{Braces, Seq, check_enum},
        },
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

pub struct TraitDef;

impl ParseNode for TraitDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwTrait))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwTrait)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Option<GenericsDecl>>()?;
        context.parse::<Option<WhereClause>>()?;
        context.parse::<Braces<Seq<ImplItemDecl>>>()?;

        Ok(0)
    }
}

pub struct ImplBlock;

impl ParseNode for ImplBlock {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwImpl))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwImpl)?;
        context.parse::<Option<GenericsDecl>>()?;
        if context.parse::<TypeRef>()? == 0 && context.next_if(TokenKind::KwFor) {
            context.parse::<TypeRef>()?;
        }
        context.parse::<Option<WhereClause>>()?;
        context.parse::<Braces<Seq<ImplItemDef>>>()?;

        Ok(0)
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

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDecl>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDecl>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDecl>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
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

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<FuncDef>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub struct Statement;

impl ParseNode for Statement {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            ModuleItem::check(kind),
            LetStmt::check(kind),
            <Expression>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<ModuleItem>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<LetStmt>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<Expression>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub struct LetStmt;

impl ParseNode for LetStmt {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwLet))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwLet)?;
        context.parse::<AssignTarget>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<Expression>()?;

        Ok(0)
    }
}

pub struct AssignTarget;

impl ParseNode for AssignTarget {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            NamedFields::<AssignTarget>::check(kind),
            // TODO: Destruct of arrays and tuples.
            // TODO: Destruct of enums (with else guard if may conflict).
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<NamedFields<AssignTarget>>()?;
                0
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub struct FfiBlock;

impl ParseNode for FfiBlock {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::KwExtern) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| Braces::<FfiDecl>::check(kind))
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        todo!()
    }
}

pub struct FfiDecl;

impl ParseNode for FfiDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        todo!()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        todo!()
    }
}
