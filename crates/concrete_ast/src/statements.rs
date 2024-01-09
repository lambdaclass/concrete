use crate::{
    common::Ident,
    expressions::{Expression, FnCallOp, IfExpr, MatchExpr, PathOp},
    types::TypeSpec,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Assign(AssignStmt),
    Match(MatchExpr),
    For(ForStmt),
    If(IfExpr),
    Let(LetStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    FnCall(FnCallOp),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LetStmtTarget {
    Simple { name: Ident, r#type: TypeSpec },
    Destructure(Vec<Binding>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LetStmt {
    pub is_mutable: bool,
    pub target: LetStmtTarget,
    pub value: Expression,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnStmt {
    pub value: Expression,
}

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForStmt {
    pub name: Ident,
    pub from: Expression,
    pub to: Expression,
    pub contents: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhileStmt {
    pub value: Expression,
    pub contents: Vec<Statement>,
}
