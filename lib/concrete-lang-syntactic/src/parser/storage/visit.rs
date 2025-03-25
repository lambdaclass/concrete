use super::TreeNode;

pub struct TreeNodeVisit<'storage> {
    current: &'storage TreeNode,
    children: &'storage [TreeNode],

    offset: usize,
}

impl TreeNodeVisit<'_> {}
