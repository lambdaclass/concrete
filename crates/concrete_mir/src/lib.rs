use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroU8,
};

use common::FnBodyBuilder;
use concrete_ast::{
    common::{Ident, Span},
    expressions::{Expression, ValueExpr},
    functions::FunctionDef,
    modules::{Module, ModuleDefItem},
    statements::{self, LetStmt, LetStmtTarget, ReturnStmt},
    types::{RefType, TypeSpec},
    Program,
};

mod common;

type LocalIndex = usize;
type BlockIndex = usize;
type TypeIndex = usize;

pub fn build_mir(program: &Program) -> ProgramBody {
    let mut modules = BTreeMap::default();

    for mod_def in &program.modules {
        modules.insert(mod_def.name.name.clone(), build_mir_module(mod_def));
    }

    ProgramBody { modules }
}

fn build_mir_module(module: &Module) -> ModuleBody {
    let mut modules = BTreeMap::new();
    let mut functions = BTreeMap::new();

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(fn_def) => {
                functions.insert(fn_def.decl.name.name.clone(), build_fn(fn_def));
            }
            ModuleDefItem::Struct(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(mod_def) => {
                modules.insert(mod_def.name.name.clone(), build_mir_module(mod_def));
            }
        }
    }

    ModuleBody {
        name: module.name.name.clone(),
        functions,
        modules,
    }
}

fn build_fn(func: &FunctionDef) -> FnBody {
    let mut builder = FnBodyBuilder {
        body: FnBody {
            name: func.decl.name.name.clone(),
            basic_blocks: Vec::new(),
            locals: Vec::new(),
        },
        ret_local: None,
        local_map: HashMap::new(),
        current_block: 0,
    };

    if let Some(ret_type) = func.decl.ret_type.as_ref() {
        let ty = type_spec_to_tykind(ret_type);
        builder.ret_local = Some(builder.body.locals.len());
        builder
            .body
            .locals
            .push((Local::new(None, LocalKind::ReturnPointer), ty));
    }

    for arg in &func.decl.params {
        let ty = type_spec_to_tykind(&arg.r#type);
        builder
            .local_map
            .insert(arg.name.name.clone(), builder.body.locals.len());
        builder
            .body
            .locals
            .push((Local::new(Some(arg.name.span), LocalKind::Arg), ty));
    }

    // Get all locals
    for stmt in &func.body {
        if let statements::Statement::Let(info) = stmt {
            match &info.target {
                LetStmtTarget::Simple { name, r#type } => {
                    let ty = type_spec_to_tykind(r#type);
                    builder
                        .local_map
                        .insert(name.name.clone(), builder.body.locals.len());
                    builder
                        .body
                        .locals
                        .push((Local::new(Some(name.span), LocalKind::Temp), ty));
                }
                LetStmtTarget::Destructure(_) => todo!(),
            }
        }
    }

    builder.current_block = builder.body.basic_blocks.len();
    builder.body.basic_blocks.push(BasicBlock {
        statements: Vec::new(),
        terminator: None,
    });

    for stmt in &func.body {
        match stmt {
            statements::Statement::Assign(_) => todo!(),
            statements::Statement::Match(_) => todo!(),
            statements::Statement::For(_) => todo!(),
            statements::Statement::If(_) => todo!(),
            statements::Statement::Let(info) => build_let(&mut builder, info),
            statements::Statement::Return(info) => {
                build_return(
                    &mut builder,
                    info,
                    func.decl
                        .ret_type
                        .as_ref()
                        .map(|x| type_spec_to_tykind(x).kind),
                );
            }
            statements::Statement::While(_) => todo!(),
            statements::Statement::FnCall(_) => todo!(),
        }
    }

    builder.body
}

fn build_let(builder: &mut FnBodyBuilder, info: &LetStmt) {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let ty = type_spec_to_tykind(r#type);
            let rvalue = build_expr(builder, &info.value, Some(ty.kind));
            let cur_block = &mut builder.body.basic_blocks[builder.current_block];
            let local_idx = builder.local_map.get(&name.name).copied().unwrap();
            cur_block.statements.push(Statement {
                span: name.span,
                kind: StatementKind::StorageLive(local_idx),
            });
            cur_block.statements.push(Statement {
                span: name.span,
                kind: StatementKind::Assign(
                    Place {
                        local: local_idx,
                        projection: vec![],
                    },
                    rvalue,
                ),
            });
        }
        LetStmtTarget::Destructure(_) => todo!(),
    }
}

fn build_return(builder: &mut FnBodyBuilder, info: &ReturnStmt, type_hint: Option<TyKind>) {
    let value = build_expr(builder, &info.value, type_hint);
    builder.body.basic_blocks[builder.current_block]
        .statements
        .push(Statement {
            span: Span::new(0, 0), // todo: good span
            kind: StatementKind::Assign(
                Place {
                    local: builder.ret_local.unwrap(),
                    projection: vec![],
                },
                value,
            ),
        });
    builder.body.basic_blocks[builder.current_block].terminator = Some(Box::new(Terminator {
        span: Span::new(0, 0), // todo: good span
        kind: TerminatorKind::Return,
    }))
}

fn build_expr(builder: &mut FnBodyBuilder, info: &Expression, type_hint: Option<TyKind>) -> Rvalue {
    match info {
        Expression::Value(info) => build_value(builder, info, type_hint),
        Expression::FnCall(_) => todo!(),
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(_, _, _) => todo!(),
    }
}

fn build_value(builder: &mut FnBodyBuilder, info: &ValueExpr, type_hint: Option<TyKind>) -> Rvalue {
    match info {
        ValueExpr::ConstBool(value) => Rvalue::Use(Operand::Const(ConstData {
            ty: TyKind::Bool,
            data: ConstKind::Value(ValueTree::Leaf((*value).into())),
        })),
        ValueExpr::ConstChar(value) => Rvalue::Use(Operand::Const(ConstData {
            ty: TyKind::Char,
            data: ConstKind::Value(ValueTree::Leaf((*value).into())),
        })),
        ValueExpr::ConstInt(value) => Rvalue::Use(Operand::Const(match type_hint {
            Some(ty) => ConstData {
                ty,
                data: ConstKind::Value(ValueTree::Leaf((*value).into())),
            },
            None => ConstData {
                ty: TyKind::Int(IntTy::I64),
                data: ConstKind::Value(ValueTree::Leaf((*value).into())),
            },
        })),
        ValueExpr::ConstFloat(value) => Rvalue::Use(Operand::Const(match type_hint {
            Some(ty) => ConstData {
                ty,
                data: ConstKind::Value(ValueTree::Leaf((*value).into())),
            },
            None => ConstData {
                ty: TyKind::Int(IntTy::I64),
                data: ConstKind::Value(ValueTree::Leaf((*value).into())),
            },
        })),
        ValueExpr::ConstStr(_) => todo!(),
        ValueExpr::Path(_) => todo!(),
        ValueExpr::Deref(_) => todo!(),
        ValueExpr::AsRef { path, ref_type } => todo!(),
    }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ProgramBody {
    pub modules: BTreeMap<String, ModuleBody>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleBody {
    pub name: String,
    pub functions: BTreeMap<String, FnBody>,
    pub modules: BTreeMap<String, ModuleBody>,
}

/// Function body
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FnBody {
    pub name: String,
    pub basic_blocks: Vec<BasicBlock>,
    pub locals: Vec<(Local, Ty)>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Option<Box<Terminator>>, // should be some once mir is built
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
        target: Option<BlockIndex>, // where to jump after call, if none diverges
    },
    SwitchInt {
        discriminator: Operand,
        targets: SwitchTargets,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SwitchTargets {
    pub values: Vec<u128>,
    pub targets: Vec<BlockIndex>, // last target is the otherwise block
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOp, Box<(Operand, Operand)>),
    UnaryOp(UnOp, Operand),
    Ref(Mutability, Place),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ScalarInt {
    pub data: u128,
    pub size: NonZeroU8,
}

impl From<u64> for ScalarInt {
    fn from(value: u64) -> Self {
        Self {
            data: value.into(),
            size: NonZeroU8::new(std::mem::size_of_val(&value) as u8).unwrap(),
        }
    }
}

impl From<bool> for ScalarInt {
    fn from(value: bool) -> Self {
        Self {
            data: value.into(),
            size: NonZeroU8::new(std::mem::size_of_val(&value) as u8).unwrap(),
        }
    }
}

impl From<char> for ScalarInt {
    fn from(value: char) -> Self {
        Self {
            data: value.into(),
            size: NonZeroU8::new(std::mem::size_of_val(&value) as u8).unwrap(),
        }
    }
}
