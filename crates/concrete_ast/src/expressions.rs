use crate::{common::Ident, statements::Statement, types::TypeSpec};

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Simple(SimpleExpr),
    FnCall(FnCallOp),
    Match(MatchExpr),
    If(IfExpr),
    UnaryOp(UnaryOp, Box<Self>),
    BinaryOp(Box<Self>, BinaryOp, Box<Self>),
}

// needed for match variants and array accesses
#[derive(Clone, Debug, PartialEq)]
pub enum SimpleExpr {
    ConstBool(bool),
    ConstChar(char),
    ConstInt(u64),
    ConstFloat(f64),
    ConstStr(String),
    Path(PathOp),
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum UnaryOp {
    ArithNeg,
    LogicalNot,
    BitwiseNot,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum BinaryOp {
    Arith(ArithOp),
    Logic(LogicOp),
    Compare(CmpOp),
    Bitwise(BitwiseOp),
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum CmpOp {
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchExpr {
    pub value: Box<Expression>,
    pub variants: Vec<MatchVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExpr {
    pub value: Box<Expression>,
    pub contents: Vec<Statement>,
    pub r#else: Option<Vec<Statement>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchVariant {
    pub case: SimpleExpr,
    pub block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PathSegment {
    FieldAccess(Ident),
    ArrayIndex(SimpleExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathOp {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CastOp {
    pub value: Expression,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnCallOp {
    pub target: Ident,
    pub args: Vec<Expression>,
}
