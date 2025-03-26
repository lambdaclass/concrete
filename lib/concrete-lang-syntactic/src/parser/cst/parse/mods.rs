use super::{
    decls::{
        AliasDef, ConstDef, EnumDef, FfiBlock, FuncDef, ImplBlock, StructDef, TraitDef, UnionDef,
    },
    utils::{Braces, CommaSep, Seq, WithAbi, WithDoc, WithVis, check_enum},
};
use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};

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

/// A module item.
///
/// May be one of the following:
///   - `WithDoc<ModuleDocItem>`, which contains `AliasDef`, `ConstDef`, `EnumDef`, `FfiBlock`,
///     `FuncDef`, `ImplBlock`, `StructDef, `TraitDef` and `UnionDef`.
///   - `WithVis<ModuleVisItem>`, which contains `ImportStmt`.
pub struct ModuleItem;

impl ParseNode for ModuleItem {
    // WithDoc
    //   WithVis
    //     WithAbi
    //       FuncDef
    //     AliasDef
    //     ConstDef
    //     EnumDef
    //     StructDef
    //     TraitDef
    //     UnionDef
    //   WithAbi
    //     FfiBlock
    //   ImplBlock
    // WithVis
    //   ImportStmt

    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            WithDoc::<ModuleDocItem>::check(kind),
            WithVis::<ImportStmt>::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<WithDoc<ModuleDocItem>>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<WithVis<ImportStmt>>()?;
                1
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

/// A module item that can be documented.
///
/// May be one of the following:
///   - `WithVis<ModuleDocVisItem>`, which contains `AliasDef`, `ConstDef`, `EnumDef`, `FuncDef`,
///     `StructDef`, `TraitDef`, `UnionDef`.
///   - `WithAbi<FfiBlock>`
///   - `ImplBlock`
pub struct ModuleDocItem;

impl ParseNode for ModuleDocItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            WithVis::<ModuleDocVisItem>::check(kind),
            WithAbi::<FfiBlock>::check(kind),
            ImplBlock::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<WithVis<ModuleDocVisItem>>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<WithAbi<FfiBlock>>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<ImplBlock>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}

/// A module item that can be documented and have a visibility modifier.
///
/// May be one of the following:
///   - `WithVis<FuncDef>`
///   - `AliasDef`
///   - `ConstDef`
///   - `EnumDef`
///   - `StructDef`
///   - `TraitDef`
///   - `UnionDef`
pub struct ModuleDocVisItem;

impl ParseNode for ModuleDocVisItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        check_enum([
            WithAbi::<FuncDef>::check(kind),
            AliasDef::check(kind),
            ConstDef::check(kind),
            EnumDef::check(kind),
            StructDef::check(kind),
            TraitDef::check(kind),
            UnionDef::check(kind),
        ])
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()) {
            CheckResult::Always(0) => {
                context.parse::<WithAbi<FuncDef>>()?;
                0
            }
            CheckResult::Always(1) => {
                context.parse::<AliasDef>()?;
                1
            }
            CheckResult::Always(2) => {
                context.parse::<ConstDef>()?;
                2
            }
            CheckResult::Always(3) => {
                context.parse::<EnumDef>()?;
                3
            }
            CheckResult::Always(4) => {
                context.parse::<StructDef>()?;
                4
            }
            CheckResult::Always(5) => {
                context.parse::<TraitDef>()?;
                5
            }
            CheckResult::Always(6) => {
                context.parse::<UnionDef>()?;
                6
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => todo!(),
            CheckResult::Never => todo!(),
        })
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

/// An import item.
///
/// It corresponds to the `a`, `a.b`, `a.{b, c}` and `{a, b}` in the `ImportStmt`'s example.
pub struct ImportItem;

impl ParseNode for ImportItem {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        // The second variant is always `CheckResult::Never` because it's an extension of the first.
        check_enum([
            (kind == Some(TokenKind::Ident))
                .then_some(CheckResult::Always(0))
                .unwrap_or_default(),
            CheckResult::Never,
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
            CheckResult::Always(2) => {
                context.parse::<Braces<CommaSep<ImportItem>>>()?;
                2
            }
            CheckResult::Always(_) | CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => todo!(),
        })
    }
}
