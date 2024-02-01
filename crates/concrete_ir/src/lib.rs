use std::collections::{BTreeMap, HashMap};

use common::{BuildCtx, FnBodyBuilder, IdGenerator, ModuleCtx};
use concrete_ast::{
    common::{Ident, Span},
    expressions::{Expression, PathOp, ValueExpr},
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
type FieldIndex = usize;

pub fn lower_program(program: &Program) -> ProgramBody {
    let mut modules = BTreeMap::default();

    let mut ctx = BuildCtx::default();

    for mod_def in &program.modules {
        ctx.module_name_to_id
            .insert(mod_def.name.name.clone(), ctx.module_id_counter);
        ctx.modules.insert(
            ctx.module_id_counter,
            ModuleCtx {
                id: ctx.module_id_counter,
                func_name_to_id: Default::default(),
                gen: IdGenerator::new(ctx.module_id_counter),
            },
        );
        ctx.module_id_counter += 1;
    }

    for mod_def in &program.modules {
        let id = *ctx
            .module_name_to_id
            .get(&mod_def.name.name)
            .expect("module should exist");

        let (new_ctx, module) = lower_module(ctx, mod_def, id);
        modules.insert(id, module);
        ctx = new_ctx;
    }

    ProgramBody {
        module_names: ctx.module_name_to_id.into_iter().collect(),
        modules,
    }
}

fn lower_module(mut ctx: BuildCtx, module: &Module, id: usize) -> (BuildCtx, ModuleBody) {
    let mut modules = BTreeMap::new();
    let mut functions = BTreeMap::new();

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(fn_def) => {
                let m = ctx.modules.get_mut(&id).expect("module should exist");
                let next_id = m.gen.next_defid();
                m.func_name_to_id
                    .insert(fn_def.decl.name.name.clone(), next_id);
            }
            ModuleDefItem::Struct(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(_) => todo!(),
        }
    }

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(fn_def) => {
                let (new_ctx, func) = lower_func(ctx, fn_def, id);
                functions.insert(fn_def.decl.name.name.clone(), func);
                ctx = new_ctx;
            }
            ModuleDefItem::Struct(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(mod_def) => {
                ctx.module_id_counter += 1;
                let id = ctx.module_id_counter;
                let (new_ctx, module) = lower_module(ctx, mod_def, id);
                modules.insert(mod_def.name.name.clone(), module);
                ctx = new_ctx;
            }
        }
    }

    (
        ctx,
        ModuleBody {
            name: module.name.name.clone(),
            id,
            functions,
            modules,
        },
    )
}

fn lower_func(mut ctx: BuildCtx, func: &FunctionDef, module_id: usize) -> (BuildCtx, FnBody) {
    let mut builder = FnBodyBuilder {
        body: FnBody {
            name: func.decl.name.name.clone(),
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            id: {
                let cur_mod = ctx.modules.get(&module_id).expect("module should exist");
                *cur_mod
                    .func_name_to_id
                    .get(&func.decl.name.name)
                    .expect("function not found")
            },
        },
        local_module: module_id,
        ret_local: None,
        local_map: HashMap::new(),
        current_block: 0,
        ctx,
    };

    if let Some(ret_type) = func.decl.ret_type.as_ref() {
        let ty = lower_type(ret_type);
        builder.ret_local = Some(builder.body.locals.len());
        builder
            .body
            .locals
            .push((Local::new(None, LocalKind::ReturnPointer), ty));
    }

    for arg in &func.decl.params {
        let ty = lower_type(&arg.r#type);
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
                    let ty = lower_type(r#type);
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
            statements::Statement::Let(info) => lower_let(&mut builder, info),
            statements::Statement::Return(info) => {
                lower_return(
                    &mut builder,
                    info,
                    func.decl.ret_type.as_ref().map(|x| lower_type(x).kind),
                );
            }
            statements::Statement::While(_) => todo!(),
            statements::Statement::FnCall(_) => todo!(),
        }
    }

    (builder.ctx, builder.body)
}

fn lower_let(builder: &mut FnBodyBuilder, info: &LetStmt) {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let ty = lower_type(r#type);
            let rvalue = lower_expression(builder, &info.value, Some(ty.kind));
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

fn lower_return(builder: &mut FnBodyBuilder, info: &ReturnStmt, type_hint: Option<TyKind>) {
    let value = lower_expression(builder, &info.value, type_hint);
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

fn lower_expression(
    builder: &mut FnBodyBuilder,
    info: &Expression,
    type_hint: Option<TyKind>,
) -> Rvalue {
    match info {
        Expression::Value(info) => lower_value_expr(builder, info, type_hint),
        Expression::FnCall(_) => todo!(),
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(_, _, _) => todo!(),
    }
}

fn lower_value_expr(
    builder: &mut FnBodyBuilder,
    info: &ValueExpr,
    type_hint: Option<TyKind>,
) -> Rvalue {
    match info {
        ValueExpr::ConstBool(value) => Rvalue::Use(Operand::Const(ConstData {
            ty: TyKind::Bool,
            data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(*value))),
        })),
        ValueExpr::ConstChar(value) => Rvalue::Use(Operand::Const(ConstData {
            ty: TyKind::Char,
            data: ConstKind::Value(ValueTree::Leaf(ConstValue::U32((*value) as u32))),
        })),
        ValueExpr::ConstInt(value) => Rvalue::Use(Operand::Const(match type_hint {
            Some(ty) => ConstData {
                ty: ty.clone(),
                data: ConstKind::Value(ValueTree::Leaf(match ty {
                    TyKind::Int(ty) => match ty {
                        IntTy::I8 => {
                            ConstValue::I8((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I16 => {
                            ConstValue::I16((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I32 => {
                            ConstValue::I32((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I64 => {
                            ConstValue::I64((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I128 => {
                            ConstValue::I128((*value).try_into().expect("value out of range"))
                        }
                    },
                    TyKind::Uint(ty) => match ty {
                        UintTy::U8 => {
                            ConstValue::U8((*value).try_into().expect("value out of range"))
                        }
                        UintTy::U16 => {
                            ConstValue::U16((*value).try_into().expect("value out of range"))
                        }
                        UintTy::U32 => {
                            ConstValue::U32((*value).try_into().expect("value out of range"))
                        }
                        UintTy::U64 => {
                            ConstValue::U64((*value).try_into().expect("value out of range"))
                        }
                        UintTy::U128 => ConstValue::U128(*value),
                    },
                    _ => unreachable!(),
                })),
            },
            None => ConstData {
                ty: TyKind::Int(IntTy::I64),
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::I64(
                    (*value).try_into().expect("value out of range"),
                ))),
            },
        })),
        ValueExpr::ConstFloat(value) => Rvalue::Use(Operand::Const(match type_hint {
            Some(ty) => ConstData {
                ty,
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::F32(
                    value.parse().expect("error parsing float"),
                ))),
            },
            None => ConstData {
                ty: TyKind::Int(IntTy::I64),
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::F64(
                    value.parse().expect("error parsing float"),
                ))),
            },
        })),
        ValueExpr::ConstStr(_) => todo!(),
        ValueExpr::Path(info) => {
            let place = lower_path(builder, info);
            Rvalue::Use(Operand::Place(place))
        }
        ValueExpr::Deref(path) => {
            let mut place = lower_path(builder, path);
            place.projection.push(PlaceElem::Deref);

            Rvalue::Use(Operand::Place(place))
        }
        ValueExpr::AsRef { path, ref_type } => todo!(),
    }
}

pub fn lower_path(builder: &mut FnBodyBuilder, info: &PathOp) -> Place {
    let local = *builder
        .local_map
        .get(&info.first.name)
        .expect("local not found");

    Place {
        local,
        projection: Default::default(), // todo, field array deref
    }
}

pub fn lower_type(spec: &TypeSpec) -> Ty {
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
                Box::new(lower_type(of_type)),
                Box::new(ConstData {
                    ty: TyKind::Uint(UintTy::U64),
                    data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(
                        (*size).try_into().unwrap(),
                    ))),
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
        "i64" => TyKind::Int(IntTy::I64),
        "i32" => TyKind::Int(IntTy::I32),
        "i16" => TyKind::Int(IntTy::I16),
        "i8" => TyKind::Int(IntTy::I8),
        "u64" => TyKind::Uint(UintTy::U64),
        "u32" => TyKind::Uint(UintTy::U32),
        "u16" => TyKind::Uint(UintTy::U16),
        "u8" => TyKind::Uint(UintTy::U8),
        "f32" => TyKind::Float(FloatTy::F32),
        "f64" => TyKind::Float(FloatTy::F64),
        _ => todo!(),
    }
}

#[derive(Debug, Clone)]
pub struct ProgramBody {
    pub module_names: BTreeMap<String, usize>,
    pub modules: BTreeMap<usize, ModuleBody>,
}

#[derive(Debug, Clone)]
pub struct ModuleBody {
    pub name: String,
    pub id: usize,
    pub functions: BTreeMap<String, FnBody>,
    pub modules: BTreeMap<String, ModuleBody>,
}

/// Function body
#[derive(Debug, Clone)]
pub struct FnBody {
    pub name: String,
    pub id: DefId,
    pub basic_blocks: Vec<BasicBlock>,
    pub locals: Vec<(Local, Ty)>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Option<Box<Terminator>>, // should be some once mir is built
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    StorageLive(LocalIndex),
    StorageDead(LocalIndex),
}

#[derive(Debug, Clone)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct SwitchTargets {
    pub values: Vec<u128>,
    pub targets: Vec<BlockIndex>, // last target is the otherwise block
}

#[derive(Debug, Clone)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOp, Box<(Operand, Operand)>),
    UnaryOp(UnOp, Operand),
    Ref(Mutability, Place),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Place(Place),
    Const(ConstData),
}

/// A place in memory
#[derive(Debug, Clone)]
pub struct Place {
    pub local: LocalIndex,
    pub projection: Vec<PlaceElem>,
}

#[derive(Debug, Clone)]
pub enum PlaceElem {
    /// Dereference
    Deref,
    /// Get a field
    Field(FieldIndex, TypeIndex),
    /// array index
    Index(LocalIndex),
}

#[derive(Debug, Clone, Copy)]
pub struct Local {
    pub span: Option<Span>,
    pub kind: LocalKind,
}

impl Local {
    pub fn new(span: Option<Span>, kind: LocalKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LocalKind {
    /// User-declared variable binding or compiler-introduced temporary.
    Temp,
    /// Function argument.
    Arg,
    /// Location of function's return value.
    ReturnPointer,
}

/// Aggregate data type: struct, enum, tuple..
pub struct AdtBody {
    pub name: DefId,
    pub variants: Vec<VariantDef>,
}

// Definition of a variant, a struct field or enum variant.
pub struct VariantDef {
    pub id: DefId,
    // The relative position in the aggregate structure.
    pub discriminant: usize,
}

// A definition id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    pub module_id: usize,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub span: Span,
    pub kind: TyKind,
}

impl Ty {
    pub fn new(span: &Span, kind: TyKind) -> Self {
        Self { span: *span, kind }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Copy)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct ConstData {
    pub ty: TyKind,
    pub data: ConstKind,
}

#[derive(Debug, Clone)]
pub enum ConstKind {
    Param(ParamConst),
    Value(ValueTree),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct ParamConst {
    pub index: usize,
    pub ident: Ident,
}

#[derive(Debug, Clone)]
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/consts/valtree/enum.ValTree.html
pub enum ValueTree {
    Leaf(ConstValue),
    Branch(Vec<Self>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binop(BinOp, ConstData, ConstData),
    UnOp(UnOp, ConstData),
    FunctionCall(ConstData, Vec<ConstData>),
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstValue {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}
