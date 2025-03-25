pub use self::{parse::TreeNodeParse, visit::TreeNodeVisit};
use super::parse::ParseContext;
use std::ops::Range;

mod parse;
mod visit;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TreeNode {
    range: Range<usize>,
    extra: usize,
    len: usize,

    update: fn(&mut ParseContext) -> usize,
}

impl TreeNode {
    #[doc(hidden)]
    pub(crate) fn new_at(offset: usize, update: fn(&mut ParseContext) -> usize) -> Self {
        Self {
            range: offset..offset,
            extra: (),
            len: (),
            update,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn update(&self) -> fn(&mut ParseContext) -> usize {
        self.update
    }
}
