use crate::{operations::Operation, statements::Statement};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Match(MatchExpr),
    If(IfExpr),
    Operation(Operation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    pub value: Operation,
    pub variants: Vec<MatchVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub value: Operation,
    pub contents: Vec<Statement>,
    pub r#else: Option<Vec<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchVariant {
    pub case: Operation,
    pub block: Vec<Statement>,
}
