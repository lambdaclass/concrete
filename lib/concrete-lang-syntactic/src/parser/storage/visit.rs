use super::TreeNode;
use std::ops::Range;

pub struct TreeNodeVisit<'storage> {
    current: &'storage TreeNode,
    children: &'storage [TreeNode],
}

impl TreeNodeVisit<'_> {
    pub fn range(&self) -> Range<usize> {
        self.current.range.clone()
    }

    pub fn extra(&self) -> usize {
        self.current.extra
    }

    pub fn len(&self) -> usize {
        self.current.len
    }

    pub fn iter_children(&self) -> impl Iterator<Item = Self> {
        struct ChildrenIter<'storage>(&'storage [TreeNode]);

        impl<'storage> Iterator for ChildrenIter<'storage> {
            type Item = TreeNodeVisit<'storage>;

            fn next(&mut self) -> Option<Self::Item> {
                let current;
                (current, self.0) = self.0.split_first()?;

                let children;
                (children, self.0) = self.0.split_at(current.len);

                Some(TreeNodeVisit { current, children })
            }
        }

        ChildrenIter(self.children)
    }
}
