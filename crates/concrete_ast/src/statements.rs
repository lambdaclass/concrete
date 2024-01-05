use crate::{
    common::Ident,
    expressions::{Expression, PathExpr},
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assign(AssignStmt),
    Match(MatchStmt),
    For(ForStmt),
    If(IfStmt),
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Expression,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignStmt {
    pub target: PathExpr,
    pub value: Expression,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Binding {
    pub name: Ident,
    pub rename: Option<Ident>,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchStmt {
    pub value: Expression,
    pub variants: Vec<MatchVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchVariant {
    pub case: Expression,
    pub block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForStmt {
    pub name: Ident,
    pub from: Expression,
    pub to: Expression,
    pub contents: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfStmt {
    pub value: Expression,
    pub contents: Vec<Statement>,
    pub r#else: Option<Vec<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileStmt {
    pub value: Expression,
    pub contents: Vec<Statement>,
}
