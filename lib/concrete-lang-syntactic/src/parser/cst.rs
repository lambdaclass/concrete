use self::{mods::ModuleItem, utils::Seq};
use super::{
    error::Result,
    parse::{CheckResult, ParseContext, ParseNode},
    storage::TreeNodeVisit,
};
use crate::lexer::TokenKind;
use mods::ModuleItemVisit;
use std::fmt;
use utils::SeqVisit;

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

pub struct SourceFileVisit<'storage>(TreeNodeVisit<'storage>);

impl<'storage> SourceFileVisit<'storage> {
    pub fn items(&self) -> SeqVisit<'storage, ModuleItemVisit<'storage>> {
        self.0.iter_children().next().unwrap().into()
    }
}

impl<'storage> From<TreeNodeVisit<'storage>> for SourceFileVisit<'storage> {
    fn from(value: TreeNodeVisit<'storage>) -> Self {
        Self(value)
    }
}

impl fmt::Display for SourceFileVisit<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("items", &DebugWithDisplay(self.items()))
            .finish()
    }
}

#[repr(transparent)]
struct DebugWithDisplay<T>(pub T);

impl<T> fmt::Debug for DebugWithDisplay<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <T as fmt::Display>::fmt(&self.0, f)
    }
}
