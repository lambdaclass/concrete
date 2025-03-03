use std::collections::HashMap;

use super::{
    common::{Ident, Span, TypeName},
    statements::Statement,
    types::TypeDescriptor,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Value(ValueExpr, Span),
    // Type#func()
    AssocMethodCall(AssocMethodCall),
    FnCall(FnCallOp),
    Match(MatchExpr),
    If(IfExpr),
    UnaryOp(UnaryOp, Box<Self>),
    BinaryOp(Box<Self>, BinaryOp, Box<Self>),
    StructInit(StructInitExpr),
    EnumInit(EnumInitExpr),
    ArrayInit(ArrayInitExpr),
    Deref(Box<Self>, Span),
    AsRef(Box<Self>, bool, Span),
    Cast(Box<Self>, TypeDescriptor, Span),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ValueExpr {
    ConstBool(bool, Span),
    ConstChar(char, Span),
    ConstInt(u128, Span),
    ConstFloat(String, Span),
    ConstStr(String, Span),
    Path(PathOp),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInitExpr {
    pub name: TypeName,
    pub fields: HashMap<Ident, StructInitField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInitField {
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssocMethodCall {
    pub assoc_type: TypeName,
    pub fn_call: FnCallOp,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumInitExpr {
    pub name: TypeName,
    pub variant: Ident,
    pub fields: HashMap<Ident, StructInitField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumMatchExpr {
    pub name: TypeName,
    pub variant: Ident,
    pub field_values: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayInitExpr {
    pub values: Vec<Expression>,
    pub span: Span,
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
    pub expr: Box<Expression>,
    pub variants: Vec<MatchVariant>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub block_stmts: Vec<Statement>,
    pub else_stmts: Option<Vec<Statement>>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MatchVariant {
    pub case: MatchCaseExpr,
    pub block: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MatchCaseExpr {
    Value(ValueExpr),
    Enum(EnumMatchExpr),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathSegment {
    FieldAccess(Ident, Span),
    ArrayIndex(ValueExpr, Span),
    MethodCall(FnCallOp, Span),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathOp {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FnCallOp {
    pub path: Vec<Ident>,
    pub target: Ident,
    pub generics: Vec<TypeName>,
    pub args: Vec<Expression>,
    pub span: Span,
}
