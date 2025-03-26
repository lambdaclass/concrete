use super::{
    AliasDecl, AliasDef, ConstDecl, ConstDef, Field, FuncDecl, FuncDef, GenericsDecl, NamedFields,
    TypeRef, WhereClause,
};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::{
            exprs::Expression,
            mods::ModuleItem,
            utils::{Braces, Parens, Seq, WithVis, check_enum},
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
        context.parse::<Option<GenericsDecl<true>>>()?;
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
        let is_trait_impl = if context.parse::<TypeRef>()? == 0 && context.next_if(TokenKind::KwFor)
        {
            context.parse::<TypeRef>()?;
            true
        } else {
            false
        };
        context.parse::<Option<WhereClause>>()?;
        if is_trait_impl {
            context.parse::<Braces<Seq<ImplItemDef<false>>>>()?;
        } else {
            context.parse::<Braces<Seq<ImplItemDef>>>()?;
        }

        Ok(is_trait_impl as usize)
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

pub struct ImplItemDef<const WITH_VIS: bool = true>;

impl<const WITH_VIS: bool> ParseNode for ImplItemDef<WITH_VIS> {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if WITH_VIS {
            check_enum([
                AliasDef::check(kind),
                ConstDef::check(kind),
                FuncDef::check(kind),
                WithVis::<ImplItemDef<false>>::check(kind),
            ])
        } else {
            check_enum([
                AliasDef::check(kind),
                ConstDef::check(kind),
                FuncDef::check(kind),
            ])
        }
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
            CheckResult::Always(3) if WITH_VIS => {
                context.parse::<WithVis<ImplItemDef<false>>>()?;
                3
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
            (kind == Some(TokenKind::KwReturn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
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
                context.next_of(TokenKind::KwReturn)?;
                context.parse::<Expression>()?;
                context.next_of(TokenKind::SymSemi)?;
                2
            }
            CheckResult::Always(3) => {
                context.parse::<Expression>()?;
                context.next_of(TokenKind::SymSemi)?;
                3
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
            // TODO: Destructure arrays and tuples.
            // TODO: Destructure enums (with else guard if may conflict).
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

pub struct FfiDecl;

impl ParseNode for FfiDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        if kind == Some(TokenKind::KwPub) {
            CheckResult::Always(0)
        } else {
            CheckResult::Empty(0)
        }
        .followed_by(|| {
            (kind == Some(TokenKind::KwFn))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default()
        })
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_if(TokenKind::KwPub);
        context.next_of(TokenKind::KwFn)?;
        context.next_of(TokenKind::Ident)?;
        context.parse::<Parens<Field<TypeRef>>>()?;
        let has_return_type = context.next_if(TokenKind::SymArrow);
        if has_return_type {
            context.parse::<TypeRef>()?;
        }
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}
