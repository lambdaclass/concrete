use crate::{
    common::Ident,
    statements::Statement,
    types::{RefType, TypeSpec},
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Value(ValueExpr),
    FnCall(FnCallOp),
    Match(MatchExpr),
    If(IfExpr),
    UnaryOp(UnaryOp, Box<Self>),
    BinaryOp(Box<Self>, BinaryOp, Box<Self>),
    Deref(Box<Self>),
    AsRef(Box<Self>, RefType),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueExpr {
    ConstBool(bool),
    ConstChar(char),
    ConstInt(u128),
    ConstFloat(String),
    ConstStr(String),
    Path(PathOp),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOp {
    ArithNeg,
    LogicalNot,
    BitwiseNot,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Arith(ArithOp),
    Logic(LogicOp),
    Compare(CmpOp),
    Bitwise(BitwiseOp),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CmpOp {
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MatchExpr {
    pub value: Box<Expression>,
    pub variants: Vec<MatchVariant>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfExpr {
    pub value: Box<Expression>,
    pub contents: Vec<Statement>,
    pub r#else: Option<Vec<Statement>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MatchVariant {
    pub case: ValueExpr,
    pub block: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathSegment {
    FieldAccess(Ident),
    ArrayIndex(ValueExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathOp {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CastOp {
    pub value: Expression,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnCallOp {
    pub target: Ident,
    pub args: Vec<Expression>,
}
