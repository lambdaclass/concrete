use std::num::NonZeroU8;

use concrete_ast::{
    common::{Ident, Span},
    functions::{FunctionDef},
    statements::{self, LetStmtTarget},
    types::{RefType, TypeSpec},
};
use generational_arena::{Arena, Index};

mod common;

pub type BlockIndex = Index;
pub type TypeIndex = Index;
pub type LocalIndex = Index;
pub type SymbolIndex = Index;

pub struct MirCtx {
    pub blocks: Arena<BasicBlock>,
    pub locals: Arena<LocalKind>,
    pub types: Arena<TyKind>,
    pub symbols: Arena<Ident>,
}



pub fn build_mir(ctx: &mut MirCtx, func: &FunctionDef) {

    let mut body = Body {
        basic_blocks: Vec::new(),
        locals: Vec::new(),
    };

    /*
    if let Some(ret_type) = func.decl.ret_type.as_ref() {
        let ty = type_spec_to_tykind(ret_type);
        body.locals
            .push((Local::new(None, LocalKind::ReturnPointer), ty));
    }

    for arg in &func.decl.params {
        let ty = type_spec_to_tykind(&arg.r#type);
        body.locals
            .push((Local::new(Some(arg.name.span), LocalKind::Arg), ty));
    }

    // Get all locals
    for stmt in &func.body {
        if let statements::Statement::Let(info) = stmt {
            match &info.target {
                LetStmtTarget::Simple { name, r#type } => {
                    let ty = type_spec_to_tykind(r#type);
                    body.locals
                        .push((Local::new(Some(name.span), LocalKind::Temp), ty));
                }
                LetStmtTarget::Destructure(_) => todo!(),
            }
        }
    }

    let mut cur_stmts = Vec::new();

    for stmt in &func.body {
        match stmt {
            statements::Statement::Assign(_) => todo!(),
            statements::Statement::Match(_) => todo!(),
            statements::Statement::For(_) => todo!(),
            statements::Statement::If(_) => todo!(),
            statements::Statement::Let(info) => {

            },
            statements::Statement::Return(_) => todo!(),
            statements::Statement::While(_) => todo!(),
            statements::Statement::FnCall(_) => todo!(),
        }
    }
    */
}

pub fn type_spec_to_tykind(spec: &TypeSpec) -> Ty {
    match spec {
        TypeSpec::Simple { name, is_ref, span } => match is_ref {
            Some(RefType::Borrow) => Ty::new(
                span,
                TyKind::Ref(Box::new(name_to_tykind(&name.name)), Mutability::Not),
            ),
            Some(RefType::MutBorrow) => Ty::new(
                span,
                TyKind::Ref(Box::new(name_to_tykind(&name.name)), Mutability::Mut),
            ),
            None => Ty::new(span, name_to_tykind(&name.name)),
        },
        TypeSpec::Generic {
            name,
            is_ref,
            type_params,
            span,
        } => {
            todo!()
        }
        TypeSpec::Array {
            of_type,
            size,
            is_ref,
            span,
        } => {
            let inner = TyKind::Array(
                Box::new(type_spec_to_tykind(of_type)),
                Box::new(ConstData {
                    ty: TyKind::Uint(UintTy::Usize),
                    data: ConstKind::Value(ValueTree::Leaf((*size).into())),
                }),
            );
            match is_ref {
                Some(RefType::Borrow) => {
                    Ty::new(span, TyKind::Ref(Box::new(inner), Mutability::Not))
                }
                Some(RefType::MutBorrow) => {
                    Ty::new(span, TyKind::Ref(Box::new(inner), Mutability::Mut))
                }
                None => Ty::new(span, inner),
            }
        }
    }
}

pub fn name_to_tykind(name: &str) -> TyKind {
    match name {
        "isize" => TyKind::Int(IntTy::Isize),
        "i64" => TyKind::Int(IntTy::I64),
        "i32" => TyKind::Int(IntTy::I32),
        "i16" => TyKind::Int(IntTy::I16),
        "i8" => TyKind::Int(IntTy::I8),
        "usize" => TyKind::Uint(UintTy::Usize),
        "u64" => TyKind::Uint(UintTy::U64),
        "u32" => TyKind::Uint(UintTy::U32),
        "u16" => TyKind::Uint(UintTy::U16),
        "u8" => TyKind::Uint(UintTy::U8),
        "f32" => TyKind::Float(FloatTy::F32),
        "f64" => TyKind::Float(FloatTy::F64),
        _ => todo!(),
    }
}

/// Function body
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Body {
    pub basic_blocks: Vec<BlockIndex>,
    pub locals: Vec<(LocalIndex, TypeIndex)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Box<Terminator>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    StorageLive(LocalIndex),
    StorageDead(LocalIndex),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TerminatorKind {
    Goto {
        target: BasicBlock,
    },
    Return,
    Unreachable,
    Call {
        func: Operand,
        args: Vec<Operand>,
        destination: Place,         // where return value is stored
        target: Option<BasicBlock>, // where to jump after call, if none diverges
    },
    SwitchInt {
        discriminator: Operand,
        targets: SwitchTargets,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SwitchTargets {
    pub values: Vec<u128>,
    pub targets: Vec<BasicBlock>, // last target is the otherwise block
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rvalue {
    BinaryOp(BinOp, Box<(Operand, Operand)>),
    UnaryOp(UnOp, Operand),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    Place(Place),
    Const(ConstData),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Place {
    pub local: LocalIndex,
    pub projection: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PlaceElem {
    Deref,
    Field(usize, TypeIndex),
    Index(LocalIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Local {
    pub span: Option<Span>,
    pub kind: LocalKind,
}

impl Local {
    pub fn new(span: Option<Span>, kind: LocalKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LocalKind {
    /// User-declared variable binding or compiler-introduced temporary.
    Temp,
    /// Function argument.
    Arg,
    /// Location of function's return value.
    ReturnPointer,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl Ty {
    pub fn new(span: &Span, kind: TyKind) -> Self {
        Self { span: *span, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TyKind {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    String,
    Array(Box<Ty>, Box<ConstData>),
    Ref(Box<Self>, Mutability),
    // Type param <T>
    Param {
        index: usize,
        name: String, // todo: change me?
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UintTy {
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstData {
    pub ty: TyKind,
    pub data: ConstKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstKind {
    Param(ParamConst),
    Value(ValueTree),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParamConst {
    pub index: usize,
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/consts/valtree/enum.ValTree.html
pub enum ValueTree {
    Leaf(ScalarInt),
    Branch(Vec<Self>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScalarInt {
    pub data: u128,
    pub size: NonZeroU8,
}

impl From<u64> for ScalarInt {
    fn from(value: u64) -> Self {
        Self {
            data: value.into(),
            size: NonZeroU8::new(8).unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Expr {
    Binop(BinOp, ConstData, ConstData),
    UnOp(UnOp, ConstData),
    FunctionCall(ConstData, Vec<ConstData>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOp {
    Not,
    Neg,
}
