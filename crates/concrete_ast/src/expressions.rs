use crate::{operations::Operation, statements::Statement};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Match(MatchExpr),
    Operation(Operation),
    // Block(Vec<Statement>),
}

pub struct BlockExpr {
    pub statements: Vec<Statement>
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    pub value: Operation,
    pub variants: Vec<MatchVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchVariant {
    pub case: Operation,
    pub block: Vec<Statement>,
}
