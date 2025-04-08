use super::{
    decls::{
        AliasDef, AliasDefVisit, ConstDef, ConstDefVisit, EnumDef, EnumDefVisit, FfiDecl,
        FfiDeclVisit, FuncDef, FuncDefVisit, ImplBlock, ImplBlockVisit, StructDef, StructDefVisit,
        TraitDef, TraitDefVisit, UnionDef, UnionDefVisit,
    },
    utils::{
        Braces, BracesVisit, CommaSep, CommaSepVisit, Seq, SeqVisit, WithAbi, WithAbiVisit,
        WithDoc, WithDocVisit, WithVis, WithVisVisit, check_enum,
    },
};
use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
        storage::TreeNodeVisit,
    },
};
use std::fmt;

/// A module declaration.
///
/// # Example
///
/// ```text
/// mod external_module;
/// mod inline_module {}
/// ```
pub struct ModuleDecl;

impl ParseNode for ModuleDecl {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwMod))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwMod)?;

        Ok(
            match check_enum([
                (context.peek() == Some(TokenKind::SymSemi))
                    .then_some(CheckResult::Always(0))
                    .unwrap_or_default(),
                Braces::<Seq<ModuleItem>>::check(context.peek()),
            ]) {
                CheckResult::Always(0) => {
                    context.next_of(TokenKind::SymSemi)?;
                    0
                }
                CheckResult::Always(1) => {
                    context.parse::<Braces<Seq<ModuleItem>>>()?;
                    1
                }
                CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
                CheckResult::Never => todo!(),
            },
        )
    }
}

pub struct ModuleDeclVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> ModuleDeclVisit<'storage> {
    pub fn items(&self) -> SeqVisit<ModuleItemVisit<'storage>> {
        self.0.iter_children().next().unwrap().into()
    }
}

/// A module item.
pub struct ModuleItem;

impl ParseNode for ModuleItem {
    //     WithDoc
    //       WithVis
    //         WithAbi
    // [x]       FuncDef
    // [x]     AliasDef
    // [x]     ConstDef
    // [x]     EnumDef
    // [x]     StructDef
    // [x]     TraitDef
    // [x]     UnionDef
    // [ ]   WithAbi<FfiBlock>
    // [ ]   ImplBlock
    //     WithVis
    // [x]   ImportStmt

    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDef::check(kind),
            ConstDef::check(kind),
            EnumDef::check(kind),
            FuncDef::check(kind),
            ImplBlock::check(kind),
            ImportStmt::check(kind),
            StructDef::check(kind),
            TraitDef::check(kind),
            UnionDef::check(kind),
            WithAbi::<ModuleAbiItem>::check(kind),
            WithDoc::<ModuleDocItem>::check(kind),
            WithVis::<ModuleVisItem>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            Some(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            Some(2) => {
                context.parse::<EnumDef>()?;
                2
            }
            Some(3) => {
                context.parse::<FuncDef>()?;
                3
            }
            Some(4) => {
                context.parse::<ImplBlock>()?;
                4
            }
            Some(5) => {
                context.parse::<ImportStmt>()?;
                5
            }
            Some(6) => {
                context.parse::<StructDef>()?;
                6
            }
            Some(7) => {
                context.parse::<TraitDef>()?;
                7
            }
            Some(8) => {
                context.parse::<UnionDef>()?;
                8
            }
            Some(9) => {
                context.parse::<WithAbi<ModuleAbiItem>>()?;
                9
            }
            Some(10) => {
                context.parse::<WithDoc<ModuleDocItem>>()?;
                10
            }
            Some(11) => {
                context.parse::<WithVis<ModuleVisItem>>()?;
                11
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum ModuleItemVisit<'storage> {
    AliasDef(AliasDefVisit<'storage>),
    ConstDef(ConstDefVisit<'storage>),
    EnumDef(EnumDefVisit<'storage>),
    FuncDef(FuncDefVisit<'storage>),
    ImplBlock(ImplBlockVisit<'storage>),
    ImportStmt(ImportStmtVisit<'storage>),
    StructDef(StructDefVisit<'storage>),
    TraitDef(TraitDefVisit<'storage>),
    UnionDef(UnionDefVisit<'storage>),
    WithAbi(WithAbiVisit<'storage, ModuleAbiItemVisit<'storage>>),
    WithDoc(WithDocVisit<'storage, ModuleDocItemVisit<'storage>>),
    WithVis(WithVisVisit<'storage, ModuleVisItemVisit<'storage>>),
}

impl<'storage> From<TreeNodeVisit<'storage>> for ModuleItemVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        match value.extra() {
            0 => Self::AliasDef(value.into()),
            1 => Self::ConstDef(value.into()),
            2 => Self::EnumDef(value.into()),
            3 => Self::FuncDef(value.into()),
            4 => Self::ImplBlock(value.into()),
            5 => Self::ImportStmt(value.into()),
            6 => Self::StructDef(value.into()),
            7 => Self::TraitDef(value.into()),
            8 => Self::UnionDef(value.into()),
            9 => Self::WithAbi(value.into()),
            10 => Self::WithDoc(value.into()),
            11 => Self::WithVis(value.into()),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for ModuleItemVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AliasDef(x) => fmt::Display::fmt(x, f),
            Self::ConstDef(x) => fmt::Display::fmt(x, f),
            Self::EnumDef(x) => fmt::Display::fmt(x, f),
            Self::FuncDef(x) => fmt::Display::fmt(x, f),
            Self::ImplBlock(x) => fmt::Display::fmt(x, f),
            Self::ImportStmt(x) => fmt::Display::fmt(x, f),
            Self::StructDef(x) => fmt::Display::fmt(x, f),
            Self::TraitDef(x) => fmt::Display::fmt(x, f),
            Self::UnionDef(x) => fmt::Display::fmt(x, f),
            Self::WithAbi(x) => fmt::Display::fmt(x, f),
            Self::WithDoc(x) => fmt::Display::fmt(x, f),
            Self::WithVis(x) => fmt::Display::fmt(x, f),
        }
    }
}

pub struct ModuleAbiItem;

impl ParseNode for ModuleAbiItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([Braces::<FfiDecl>::check(kind), FuncDef::check(kind)])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<Braces<FfiDecl>>()?;
                0
            }
            Some(1) => {
                context.parse::<FuncDef>()?;
                1
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum ModuleAbiItemVisit<'storage> {
    FfiDecl(BracesVisit<'storage, FfiDeclVisit<'storage>>),
    FuncDef(FuncDefVisit<'storage>),
}

impl fmt::Display for ModuleAbiItemVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ModuleAbiItemVisit::FfiDecl(x) => fmt::Display::fmt(x, f),
            ModuleAbiItemVisit::FuncDef(x) => fmt::Display::fmt(x, f),
        }
    }
}

impl<'storage> From<TreeNodeVisit<'storage>> for ModuleAbiItemVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        match value.extra() {
            0 => Self::FfiDecl(value.iter_children().next().unwrap().into()),
            1 => todo!(),
            _ => unreachable!(),
        }
    }
}

/// A module item that can be documented.
pub struct ModuleDocItem;

impl ParseNode for ModuleDocItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDef::check(kind),
            ConstDef::check(kind),
            EnumDef::check(kind),
            FuncDef::check(kind),
            ImplBlock::check(kind),
            StructDef::check(kind),
            TraitDef::check(kind),
            UnionDef::check(kind),
            WithAbi::<ModuleAbiItem>::check(kind),
            WithVis::<ModuleDocVisItem>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            Some(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            Some(2) => {
                context.parse::<EnumDef>()?;
                2
            }
            Some(3) => {
                context.parse::<FuncDef>()?;
                3
            }
            Some(4) => {
                context.parse::<ImplBlock>()?;
                4
            }
            Some(5) => {
                context.parse::<StructDef>()?;
                5
            }
            Some(6) => {
                context.parse::<TraitDef>()?;
                6
            }
            Some(7) => {
                context.parse::<UnionDef>()?;
                7
            }
            Some(8) => {
                context.parse::<WithAbi<ModuleAbiItem>>()?;
                8
            }
            Some(9) => {
                context.parse::<WithVis<ModuleDocVisItem>>()?;
                9
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum ModuleDocItemVisit<'storage> {
    AliasDef(AliasDefVisit<'storage>),
    ConstDef(ConstDefVisit<'storage>),
    EnumDef(EnumDefVisit<'storage>),
    FuncDef(FuncDefVisit<'storage>),
    ImplBlock(ImplBlockVisit<'storage>),
    StructDef(StructDefVisit<'storage>),
    TraitDef(TraitDefVisit<'storage>),
    UnionDef(UnionDefVisit<'storage>),
    WithAbi(WithAbiVisit<'storage, ModuleAbiItemVisit<'storage>>),
    WithVis(WithVisVisit<'storage, ModuleDocVisItemVisit<'storage>>),
}

impl fmt::Display for ModuleDocItemVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

/// A module item that can be documented and have a visibility modifier.
pub struct ModuleDocVisItem;

impl ParseNode for ModuleDocVisItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDef::check(kind),
            ConstDef::check(kind),
            EnumDef::check(kind),
            FuncDef::check(kind),
            StructDef::check(kind),
            TraitDef::check(kind),
            UnionDef::check(kind),
            WithAbi::<FuncDef>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            Some(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            Some(2) => {
                context.parse::<EnumDef>()?;
                2
            }
            Some(3) => {
                context.parse::<FuncDef>()?;
                3
            }
            Some(4) => {
                context.parse::<StructDef>()?;
                4
            }
            Some(5) => {
                context.parse::<TraitDef>()?;
                5
            }
            Some(6) => {
                context.parse::<UnionDef>()?;
                6
            }
            Some(7) => {
                context.parse::<WithAbi<FuncDef>>()?;
                7
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum ModuleDocVisItemVisit<'storage> {
    AliasDef(AliasDefVisit<'storage>),
    ConstDef(ConstDefVisit<'storage>),
    EnumDef(EnumDefVisit<'storage>),
    FuncDef(FuncDefVisit<'storage>),
    StructDef(StructDefVisit<'storage>),
    TraitDef(TraitDefVisit<'storage>),
    UnionDef(UnionDefVisit<'storage>),
    WithAbi(WithAbiVisit<'storage, FuncDefVisit<'storage>>),
}

pub struct ModuleVisItem;

impl ParseNode for ModuleVisItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            AliasDef::check(kind),
            ConstDef::check(kind),
            EnumDef::check(kind),
            FuncDef::check(kind),
            ImportStmt::check(kind),
            StructDef::check(kind),
            TraitDef::check(kind),
            UnionDef::check(kind),
            WithAbi::<FuncDef>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(0) => {
                context.parse::<AliasDef>()?;
                0
            }
            Some(1) => {
                context.parse::<ConstDef>()?;
                1
            }
            Some(2) => {
                context.parse::<EnumDef>()?;
                2
            }
            Some(3) => {
                context.parse::<FuncDef>()?;
                3
            }
            Some(4) => {
                context.parse::<ImportStmt>()?;
                4
            }
            Some(5) => {
                context.parse::<StructDef>()?;
                5
            }
            Some(6) => {
                context.parse::<TraitDef>()?;
                6
            }
            Some(7) => {
                context.parse::<UnionDef>()?;
                7
            }
            Some(8) => {
                context.parse::<WithAbi<FuncDef>>()?;
                8
            }
            Some(_) => unreachable!(),
            None => todo!(),
        })
    }
}

pub enum ModuleVisItemVisit<'storage> {
    AliasDef(AliasDefVisit<'storage>),
    ConstDef(ConstDefVisit<'storage>),
    EnumDef(EnumDefVisit<'storage>),
    FuncDef(FuncDefVisit<'storage>),
    ImportStmt(ImportStmtVisit<'storage>),
    StructDef(StructDefVisit<'storage>),
    TraitDef(TraitDefVisit<'storage>),
    UnionDef(UnionDefVisit<'storage>),
    WithAbi(WithAbiVisit<'storage, FuncDefVisit<'storage>>),
}

impl fmt::Display for ModuleVisItemVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

/// An import statement.
///
/// # Example
///
/// ```text
/// import a;
/// import a.b;
/// import a.{b, c};
/// import {a, b};
/// ```
pub struct ImportStmt;

impl ParseNode for ImportStmt {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::KwImport))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::KwImport)?;
        context.parse::<ImportItem>()?;
        context.next_of(TokenKind::SymSemi)?;

        Ok(0)
    }
}

pub struct ImportStmtVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> From<TreeNodeVisit<'storage>> for ImportStmtVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        todo!()
    }
}

impl fmt::Display for ImportStmtVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}

/// An import item.
///
/// It corresponds to the `a`, `a.b`, `a.{b, c}` and `{a, b}` in the `ImportStmt`'s example.
pub struct ImportItem;

impl ParseNode for ImportItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            Braces::<CommaSep<ImportItem>>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.next_of(TokenKind::Ident)?;
                if context.next_if(TokenKind::SymPeriod) {
                    context.parse::<ImportItem>()?;
                    1
                } else {
                    0
                }
            }
            CheckResult::Always(1) => {
                context.parse::<Braces<CommaSep<ImportItem>>>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

pub enum ImportItemVisit<'storage> {
    Item,
    Path,
    Group(BracesVisit<'storage, CommaSepVisit<'storage, ImportItemVisit<'storage>>>),
}
