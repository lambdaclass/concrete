use super::{
    AliasDecl, AliasDef, ConstDecl, ConstDef, FuncDecl, FuncDef, GenericsDecl, TypeRef, WhereClause,
};
use crate::{
    lexer::TokenKind,
    parser::{
        cst::parse::{
            exprs::Expression,
            mods::ModuleItem,
            utils::{Braces, CommaSep, Parens, Seq, check_enum},
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
        context.parse::<GenericsDecl>()?;
        context.parse::<WhereClause>()?;
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
        context.parse::<GenericsDecl>()?;
        if context.parse::<TypeRef>()? == 0 && context.next_if(TokenKind::KwFor) {
            context.parse::<TypeRef>()?;
        }
        context.parse::<WhereClause>()?;
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
            // TODO: IfStmt, ForStmt, WhileStmt, LoopStmt...
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
        context.next_of(TokenKind::Ident)?;
        context.parse::<FieldsRef>()?;
        context.next_of(TokenKind::SymAssign)?;
        context.parse::<Expression>()?;

        Ok(0)
    }
}

// let <AssignTarget> = ...;
//
// Contains:
//   - Ident
//   - Destruct for tuples
//   - Destruct for arrays
//   - Destruct for structs (struct & tuple, but not unit since those are idents).
//   - Later on: Enums? `let X::X(x) = x else ...;`
pub struct AssignTarget;

impl ParseNode for AssignTarget {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // TODO: Destruct for arrays and tuples.
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        match context.peek() {
            Some(TokenKind::LBrace) => {
                context.parse::<Parens<AssignTarget>>()?;
                Ok(1)
            }
            Some(TokenKind::LBracket) => todo!(),
            Some(TokenKind::LParen) => {
                context.parse::<Braces<AssignTarget>>()?;
                Ok(2)
            }
            _ => Ok(0),
        }
    }
}

// TODO: I think this can be collapsed into `Fields<T>`.
pub struct FieldsDef;

impl ParseNode for FieldsDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            CheckResult::Empty(0),
            Parens::<CommaSep<Expression>>::check(kind),
            Braces::<CommaSep<FieldDef>>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Empty(0) => 0,
            CheckResult::Always(1) => {
                context.parse::<CommaSep<Expression>>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<CommaSep<FieldDef>>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub struct FieldDef;

impl ParseNode for FieldDef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::Ident))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::Ident)?;
        if context.next_if(TokenKind::SymColon) {
            context.parse::<Expression>()?;
        }

        Ok(0)
    }
}

pub struct FieldsRef;

impl ParseNode for FieldsRef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // (kind == Some(TokenKind::Ident)).then_some(CheckResult::Always(()))
        todo!()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        todo!()
    }
}

pub struct FieldRef;

impl ParseNode for FieldRef {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            CheckResult::Empty(0),
            Parens::<CommaSep<FieldsRef>>::check(kind),
            Braces::<CommaSep<FieldDecl>>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        todo!()
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
