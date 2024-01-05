use std::collections::HashMap;

use crate::{
    common::Ident,
    statements::{IfStmt, MatchStmt, ReturnStmt},
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    FieldAccess(Ident),
    ArrayIndex(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathExpr {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Compound(CompoundExpr),
    Atomic(AtomicExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompoundExpr {
    Compare(CmpExpr),
    Logic(LogicExpr),
    Arith(ArithExpr),
    Cast(CastExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AtomicExpr {
    ConstBool(bool),
    ConstChar(char),
    ConstInt(u64),
    ConstFloat(f64),
    ConstStr(String),
    FnCall(FnCallExpr),
    Path(PathExpr),
    RefPath(PathExpr),
    Paren(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementExpr {
    Match(MatchStmt),
    If(IfStmt),
    Return(ReturnStmt),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CmpExpr {
    Eq(AtomicExpr, AtomicExpr),
    NotEq(AtomicExpr, AtomicExpr),
    Lt(AtomicExpr, AtomicExpr),
    LtEq(AtomicExpr, AtomicExpr),
    Gt(AtomicExpr, AtomicExpr),
    GtEq(AtomicExpr, AtomicExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LogicExpr {
    And(AtomicExpr, AtomicExpr),
    Or(AtomicExpr, AtomicExpr),
    Not(AtomicExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithExpr {
    Add(AtomicExpr, AtomicExpr),
    Sub(AtomicExpr, AtomicExpr),
    Mul(AtomicExpr, AtomicExpr),
    Div(AtomicExpr, AtomicExpr),
    Mod(AtomicExpr, AtomicExpr),
    Neg(AtomicExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CastExpr {
    pub value: AtomicExpr,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCallExpr {
    pub target: Ident,
    pub args: Vec<Expression>,
}
