use self::{
    cst::SourceFile,
    parse::{ParseContext, ParseNode},
    storage::{TreeNode, TreeNodeParse},
};
use crate::{UpdateResult, lexer::TokenStream};
use cst::SourceFileVisit;
use std::ops::Range;
use storage::TreeNodeVisit;

mod cst;
pub mod error;
mod parse;
mod storage;

#[derive(Debug, Default)]
pub struct Parser {
    nodes: Vec<TreeNode>,
}

impl Parser {
    pub fn update(
        &mut self,
        range: Range<usize>,
        length: usize,
        stream: &TokenStream,
    ) -> UpdateResult {
        let mut offset_stack = find_update_node_offset(&self.nodes, range.clone());
        if offset_stack.is_empty() {
            offset_stack.push(0);
        }

        let original_cst = self.nodes.clone();
        #[allow(clippy::never_loop)]
        while let Some(offset) = offset_stack.pop() {
            self.nodes.truncate(offset);

            let (range_end, len, update) = match original_cst.get(offset) {
                Some(node) => (node.range().end, node.len(), node.update()),
                None => (0, 0, SourceFile::parse as _),
            };
            self.nodes.push(TreeNode::new_at(range_end, update));

            let mut context =
                ParseContext::new(stream, TreeNodeParse::new(&mut self.nodes, offset));
            update(&mut context).unwrap();

            // TODO: Update remaining nodes' offsets.

            break;
        }

        UpdateResult { range, length }
    }

    pub fn visit<'storage>(
        &'storage self,
        stream: &'storage TokenStream,
    ) -> SourceFileVisit<'storage> {
        TreeNodeVisit::new(&self.nodes, stream).into()
    }
}

fn find_update_node_offset(nodes: &[TreeNode], range: Range<usize>) -> Vec<usize> {
    let mut stack = Vec::new();

    let mut offset = 0;
    if let Some((first, mut rest)) = nodes.split_first() {
        debug_assert!(range.start >= first.range().start);
        debug_assert!(range.end <= first.range().end);

        stack.push(offset);
        offset += 1;

        while !rest.is_empty() {
            let child;
            let children;
            (child, rest) = rest.split_first().unwrap();
            (children, _) = rest.split_at(child.len());

            if range.start >= child.range().start && range.end <= child.range().end {
                stack.push(offset);
                rest = children;
            } else if range.end <= child.range().start {
                break;
            }

            offset += 1;
        }
    }

    stack
}
