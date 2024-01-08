use crate::{
    common::Ident,
    expressions::{Expression, MatchExpr, IfExpr},
    operations::{Operation, PathOp},
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assign(AssignStmt),
    Match(MatchExpr),
    For(ForStmt),
    If(IfExpr),
    Let(LetStmt),
    Return(ReturnStmt),
    While(WhileStmt),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LetStmtTarget {
    Simple { name: Ident, r#type: TypeSpec },
    Destructure(Vec<Binding>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetStmt {
    pub is_mutable: bool,
    pub target: LetStmtTarget,
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignStmt {
    pub target: PathOp,
    pub value: Expression,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Binding {
    pub name: Ident,
    pub rename: Option<Ident>,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStmt {
    pub name: Ident,
    pub from: Operation,
    pub to: Operation,
    pub contents: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStmt {
    pub value: Operation,
    pub contents: Vec<Statement>,
}
