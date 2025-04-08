use super::TreeNode;
use crate::lexer::{Token, TokenStream};
use std::ops::Range;

pub struct TreeNodeVisit<'storage> {
    current: &'storage TreeNode,
    children: &'storage [TreeNode],

    stream: &'storage TokenStream,
}

impl<'storage> TreeNodeVisit<'storage> {
    #[doc(hidden)]
    pub(crate) fn new(storage: &'storage [TreeNode], stream: &'storage TokenStream) -> Self {
        let (first, rest) = storage.split_first().unwrap();
        assert_eq!(first.len(), rest.len());

        Self {
            current: first,
            children: rest,

            stream,
        }
    }

    pub fn range(&self) -> Range<usize> {
        self.current.range.clone()
    }

    pub fn extra(&self) -> usize {
        self.current.extra
    }

    pub fn len(&self) -> usize {
        self.current.len
    }

    pub fn token(&self, offset: usize) -> &Token {
        assert!(self.current.range().start + offset < self.current.range().end);
        &self.stream[self.current.range().start + offset]
    }

    pub fn iter_children(&self) -> impl Iterator<Item = Self> {
        struct ChildrenIter<'storage>(&'storage [TreeNode], &'storage TokenStream);

        impl<'storage> Iterator for ChildrenIter<'storage> {
            type Item = TreeNodeVisit<'storage>;

            fn next(&mut self) -> Option<Self::Item> {
                let current;
                (current, self.0) = self.0.split_first()?;

                let children;
                (children, self.0) = self.0.split_at(current.len);

                Some(TreeNodeVisit {
                    current,
                    children,
                    stream: self.1,
                })
            }
        }

        ChildrenIter(self.children, self.stream)
    }
}
