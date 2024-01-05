use crate::{common::Ident, types::TypeSpec};

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    FieldAccess(Ident),
    ArrayIndex(Box<Operation>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathOp {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operation {
    Compound(CompoundOp),
    Atomic(AtomicOp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompoundOp {
    Compare(CmpOp),
    Logic(LogicOp),
    Arith(ArithOp),
    Cast(CastOp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AtomicOp {
    ConstBool(bool),
    ConstChar(char),
    ConstInt(u64),
    ConstFloat(f64),
    ConstStr(String),
    FnCall(FnCallOp),
    Path(PathOp),
    Paren(Box<Operation>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum CmpOp {
    Eq(AtomicOp, AtomicOp),
    NotEq(AtomicOp, AtomicOp),
    Lt(AtomicOp, AtomicOp),
    LtEq(AtomicOp, AtomicOp),
    Gt(AtomicOp, AtomicOp),
    GtEq(AtomicOp, AtomicOp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum LogicOp {
    And(AtomicOp, AtomicOp),
    Or(AtomicOp, AtomicOp),
    Not(AtomicOp),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithOp {
    Add(AtomicOp, AtomicOp),
    Sub(AtomicOp, AtomicOp),
    Mul(AtomicOp, AtomicOp),
    Div(AtomicOp, AtomicOp),
    Mod(AtomicOp, AtomicOp),
    Neg(AtomicOp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CastOp {
    pub value: AtomicOp,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCallOp {
    pub target: Ident,
    pub args: Vec<Operation>,
}
