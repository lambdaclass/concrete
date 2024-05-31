use crate::{
    common::{Ident, Span},
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
    Simple { id: Ident, r#type: TypeSpec },
    Destructure(Vec<Binding>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LetStmt {
    pub is_mutable: bool,
    pub lvalue: LetStmtTarget,
    pub rvalue: Expression,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AssignStmt {
    pub lvalue: PathOp,
    pub derefs: usize,
    pub rvalue: Expression,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Binding {
    pub id: Ident,
    pub rename: Option<Ident>,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForStmt {
    pub init: Option<LetStmt>,
    pub cond: Option<Expression>,
    pub post: Option<AssignStmt>,
    pub block_stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WhileStmt {
    pub cond: Expression,
    pub block_stmts: Vec<Statement>,
}
