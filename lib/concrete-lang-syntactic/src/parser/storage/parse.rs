use super::TreeNode;
use crate::parser::parse::ParseNode;
use std::ops::Range;

#[derive(Debug)]
pub struct TreeNodeParse<'storage> {
    values: &'storage mut Vec<TreeNode>,
    current: usize,
}

impl<'storage> TreeNodeParse<'storage> {
    #[doc(hidden)]
    pub(crate) fn new(values: &'storage mut Vec<TreeNode>, current: usize) -> Self {
        Self { values, current }
    }

    pub fn range(&self) -> Range<usize> {
        self.values[self.current].range.clone()
    }

    pub fn len(&self) -> usize {
        self.values[self.current].len
    }

    pub fn set_extra(&mut self, extra: usize) {
        self.values[self.current].extra = extra;
    }

    pub fn push_token(&mut self) {
        self.values[self.current].range.end += 1;
    }

    pub fn push_child<T>(&mut self) -> TreeNodeParse
    where
        T: ParseNode,
    {
        self.values[self.current].len += 1;

        let current = self.values.len();
        let offset = self.values[self.current].range.end;
        self.values.push(TreeNode {
            range: offset..offset,
            extra: 0,
            len: 0,

            update: T::parse,
        });

        TreeNodeParse {
            values: self.values,
            current,
        }
    }

    // Note: Cannot pass the entire `TreeNodeParse` without double mutable borrows.
    pub fn finish_child(&mut self, range: Range<usize>, len: usize) {
        let value = &mut self.values[self.current];

        debug_assert_eq!(value.range.end, range.start);
        value.range.end = range.end;
        value.len += len;
    }
}
