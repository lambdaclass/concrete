use std::collections::{BTreeMap, HashMap};

use common::{BuildCtx, FnBodyBuilder, IdGenerator, ModuleCtx};
use concrete_ast::{
    common::Span,
    expressions::{
        ArithOp, BinaryOp, BitwiseOp, CmpOp, Expression, FnCallOp, LogicOp, PathOp, ValueExpr,
    },
    functions::FunctionDef,
    modules::{Module, ModuleDefItem},
    statements::{self, LetStmt, LetStmtTarget, ReturnStmt},
    types::{RefType, TypeSpec},
    Program,
};

use crate::{
    BasicBlock, BinOp, ConstData, ConstKind, ConstValue, DefId, FloatTy, FnBody, IntTy, Local,
    LocalKind, ModuleBody, Mutability, Operand, Place, PlaceElem, ProgramBody, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind, Ty, TyKind, UintTy, ValueTree,
};

pub mod common;
pub mod prepass;

pub fn lower_program(program: &Program) -> ProgramBody {
    let mut ctx = BuildCtx {
        body: ProgramBody {
            module_names: Default::default(),
            modules: Default::default(),
        },
        gen: IdGenerator::default(),
        module_ctxs: Default::default(),
    };


    // resolve symbols
    for module in &program.modules {
        ctx = prepass::prepass_module(ctx, module);
    }

    for mod_def in &program.modules {
        let id = *ctx
            .body
            .module_names
            .get(&mod_def.name.name)
            .expect("module should exist");

        ctx = lower_module(ctx, mod_def, id, &[]);
    }

    ctx.body
}

fn lower_module(mut ctx: BuildCtx, module: &Module, id: DefId, parent_ids: &[DefId]) -> BuildCtx {
    let mut gen = ctx.gen;

    let mut body = if parent_ids.is_empty() {
        ctx.body.modules.get_mut(&id).unwrap()
    } else {
        ctx.get_module_mut(parent_ids).unwrap()
    };

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(fn_def) => {
                let (new_ctx, func) = lower_func(ctx, fn_def, id);
                body.functions.insert(fn_def.decl.name.name.clone(), func);
                ctx = new_ctx;
            }
            ModuleDefItem::Struct(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(mod_def) => {
                let submodule_id = ctx.gen.next_module_defid();
                ctx.module_ctxs.insert(submodule_id, ModuleCtx { id: submodule_id, func_name_to_id: , functions: (), gen: () })
                let (new_ctx, module) = lower_module(ctx, mod_def, submodule_id);
                body.modules.insert(mod_def.name.name.clone(), module);
                ctx = new_ctx;
            }
        }
    }

    ctx.gen = gen;
    ctx
}

fn lower_func(mut ctx: Mod, func: &FunctionDef, module_id: DefId) -> BuildCtx {
    todo!()
}

/*

fn lower_func(mut ctx: BuildCtx, func: &FunctionDef, module_id: DefId) -> (BuildCtx, FnBody) {
    let mut builder = FnBodyBuilder {
        body: FnBody {
            name: func.decl.name.name.clone(),
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            id: {
                let cur_mod = ctx.module_ctxs.get(&module_id).expect("module should exist");
                *cur_mod
                    .func_name_to_id
                    .get(&func.decl.name.name)
                    .expect("function not found")
            },
        },
        local_module: module_id,
        ret_local: None,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ctx,
    };

    if let Some(ret_type) = func.decl.ret_type.as_ref() {
        let ty = lower_type(ret_type);
        builder.ret_local = Some(builder.body.locals.len());
        builder
            .body
            .locals
            .push(Local::new(None, LocalKind::ReturnPointer, ty));
    }

    for arg in &func.decl.params {
        let ty = lower_type(&arg.r#type);
        builder
            .name_to_local
            .insert(arg.name.name.clone(), builder.body.locals.len());
        builder
            .body
            .locals
            .push(Local::new(Some(arg.name.span), LocalKind::Arg, ty));
    }

    // Get all locals
    for stmt in &func.body {
        if let statements::Statement::Let(info) = stmt {
            match &info.target {
                LetStmtTarget::Simple { name, r#type } => {
                    let ty = lower_type(r#type);
                    builder
                        .name_to_local
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
            let local_idx = builder.name_to_local.get(&name.name).copied().unwrap();
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
        Expression::FnCall(info) => todo!(),
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(lhs, op, rhs) => lower_binary_op(builder, lhs, *op, rhs, type_hint),
    }
}

fn lower_fn_call(builder: &mut FnBodyBuilder, info: &FnCallOp) {
    let fn_id = *builder
        .get_current_module()
        .func_name_to_id
        .get(&info.target.name)
        .expect("function name not found");

    let (arg_types, ret_type) = {
        let m = builder
            .ctx
            .module_ctxs
            .get(&fn_id.module_id)
            .expect("failed to find fn module");
        m.functions.get(&fn_id).cloned().unwrap()
    };

    let mut args = Vec::new();

    for (arg, arg_ty) in info.args.iter().zip(arg_types) {
        let rvalue = lower_expression(builder, arg, Some(arg_ty.kind.clone()));
        args.push(rvalue);
    }

    let target_block = builder.body.basic_blocks.len();

    let temp_local = if let Some(ret_type) = ret_type {
        builder.body.locals.push((
            Local {
                span: None,
                kind: LocalKind::Temp,
            },
            ret_type,
        ));
        Some(Place {
            local: builder.body.locals.len() - 1,
            projection: Vec::new(),
        })
    } else {
        None
    };

    // todo: check if function is diverging such as exit().
    let kind = TerminatorKind::Call {
        func: fn_id,
        args,
        destination: temp_local,
        target: Some(target_block),
    };

    builder.body.basic_blocks[builder.current_block].terminator = Some(Box::new(Terminator {
        span: info.target.span, // todo: good span
        kind,
    }));

    builder.body.basic_blocks.push(BasicBlock::default());
}

fn lower_binary_op(
    builder: &mut FnBodyBuilder,
    lhs: &Expression,
    op: BinaryOp,
    rhs: &Expression,
    type_hint: Option<TyKind>,
) -> Rvalue {
    let expr_type = type_hint.clone().expect("type hint needed");
    let lhs = lower_expression(builder, lhs, type_hint.clone());
    let rhs = lower_expression(builder, rhs, type_hint.clone());

    match op {
        BinaryOp::Arith(op) => match op {
            ArithOp::Add => Rvalue::BinaryOp(BinOp::Add, Box::new((lhs, rhs))),
            ArithOp::Sub => Rvalue::BinaryOp(BinOp::Sub, Box::new((lhs, rhs))),
            ArithOp::Mul => Rvalue::BinaryOp(BinOp::Mul, Box::new((lhs, rhs))),
            ArithOp::Div => Rvalue::BinaryOp(BinOp::Div, Box::new((lhs, rhs))),
            ArithOp::Mod => Rvalue::BinaryOp(BinOp::Mod, Box::new((lhs, rhs))),
        },
        BinaryOp::Logic(op) => match op {
            LogicOp::And => Rvalue::BinaryOp(
                BinOp::Ne,
                Box::new((
                    Rvalue::BinaryOp(BinOp::BitAnd, Box::new((lhs, rhs))),
                    Rvalue::Use(Operand::Const(ConstData {
                        ty: expr_type.clone(),
                        data: ConstKind::Value(expr_type.get_falsy_value()),
                    })),
                )),
            ),
            LogicOp::Or => Rvalue::BinaryOp(
                BinOp::Ne,
                Box::new((
                    Rvalue::BinaryOp(BinOp::BitOr, Box::new((lhs, rhs))),
                    Rvalue::Use(Operand::Const(ConstData {
                        ty: expr_type.clone(),
                        data: ConstKind::Value(expr_type.get_falsy_value()),
                    })),
                )),
            ),
        },
        BinaryOp::Compare(op) => match op {
            CmpOp::Eq => Rvalue::BinaryOp(BinOp::Eq, Box::new((lhs, rhs))),
            CmpOp::NotEq => Rvalue::BinaryOp(BinOp::Ne, Box::new((lhs, rhs))),
            CmpOp::Lt => Rvalue::BinaryOp(BinOp::Lt, Box::new((lhs, rhs))),
            CmpOp::LtEq => Rvalue::BinaryOp(BinOp::Le, Box::new((lhs, rhs))),
            CmpOp::Gt => Rvalue::BinaryOp(BinOp::Gt, Box::new((lhs, rhs))),
            CmpOp::GtEq => Rvalue::BinaryOp(BinOp::Ge, Box::new((lhs, rhs))),
        },
        BinaryOp::Bitwise(op) => match op {
            BitwiseOp::And => Rvalue::BinaryOp(BinOp::BitAnd, Box::new((lhs, rhs))),
            BitwiseOp::Or => Rvalue::BinaryOp(BinOp::BitXor, Box::new((lhs, rhs))),
            BitwiseOp::Xor => Rvalue::BinaryOp(BinOp::BitXor, Box::new((lhs, rhs))),
        },
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
        .name_to_local
        .get(&info.first.name)
        .expect("local not found");

    Place {
        local,
        projection: Default::default(), // todo, field array deref
    }
}

*/

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
        "string" => TyKind::String,
        _ => todo!(),
    }
}
