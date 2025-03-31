use self::{mods::ModuleItem, utils::Seq};
use super::{
    error::Result,
    parse::{CheckResult, ParseContext, ParseNode},
    storage::TreeNodeVisit,
};
use crate::lexer::TokenKind;

mod decls;
mod exprs;
mod mods;
mod utils;

/// Parser for an entire source file.
pub struct SourceFile;

impl ParseNode for SourceFile {
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Seq::<ModuleItem>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.parse::<Seq<ModuleItem>>()?;
        if !context.is_end() {
            todo!();
        }

        Ok(0)
    }
}
