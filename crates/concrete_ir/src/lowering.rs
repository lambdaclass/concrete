use std::collections::HashMap;

use common::{BuildCtx, FnBodyBuilder, IdGenerator};
use concrete_ast::{
    expressions::{
        ArithOp, BinaryOp, BitwiseOp, CmpOp, Expression, FnCallOp, IfExpr, LogicOp, PathOp,
        ValueExpr,
    },
    functions::FunctionDef,
    modules::{Module, ModuleDefItem},
    statements::{self, AssignStmt, LetStmt, LetStmtTarget, ReturnStmt},
    types::{RefType, TypeSpec},
    Program,
};

use crate::{
    BasicBlock, BinOp, ConstData, ConstKind, ConstValue, DefId, FloatTy, FnBody, IntTy, Local,
    LocalKind, LogOp, ModuleBody, Mutability, Operand, Place, PlaceElem, ProgramBody, Rvalue,
    Statement, StatementKind, SwitchTargets, Terminator, TerminatorKind, Ty, TyKind, UintTy,
    ValueTree,
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
    let body = if parent_ids.is_empty() {
        ctx.body.modules.get_mut(&id).unwrap()
    } else {
        ctx.get_module_mut(parent_ids).unwrap()
    };

    // fill fn sigs
    for content in &module.contents {
        if let ModuleDefItem::Function(fn_def) = content {
            let fn_id = *body.symbols.functions.get(&fn_def.decl.name.name).unwrap();

            let mut args = Vec::new();
            let ret_type;

            for arg in &fn_def.decl.params {
                let ty = lower_type(&arg.r#type);
                args.push(ty);
            }

            if let Some(ty) = &fn_def.decl.ret_type {
                ret_type = lower_type(ty);
            } else {
                ret_type = Ty {
                    span: None,
                    kind: TyKind::Unit,
                };
            }

            body.function_signatures.insert(fn_id, (args, ret_type));
        }
    }

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(fn_def) => {
                // todo: avoid clone
                *body = lower_func(body.clone(), fn_def, id);
            }
            ModuleDefItem::Struct(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(_mod_def) => {}
        }
    }

    ctx
}

fn lower_func(ctx: ModuleBody, func: &FunctionDef, module_id: DefId) -> ModuleBody {
    let mut builder = FnBodyBuilder {
        body: FnBody {
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            id: *ctx.symbols.functions.get(&func.decl.name.name).unwrap(),
        },
        local_module: module_id,
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ctx,
    };

    let fn_id = *builder
        .ctx
        .symbols
        .functions
        .get(&func.decl.name.name)
        .unwrap();
    let (args_ty, ret_ty) = builder.ctx.function_signatures.get(&fn_id).unwrap().clone();

    builder.ret_local = builder.body.locals.len();
    builder
        .body
        .locals
        .push(Local::new(None, LocalKind::ReturnPointer, ret_ty));

    for (arg, ty) in func.decl.params.iter().zip(args_ty) {
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
                        .push(Local::new(Some(name.span), LocalKind::Temp, ty));
                }
                LetStmtTarget::Destructure(_) => todo!(),
            }
        }
    }

    for stmt in &func.body {
        lower_statement(
            &mut builder,
            stmt,
            func.decl.ret_type.as_ref().map(|x| lower_type(x).kind),
        );
    }

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.functions.insert(body.id, body);

    ctx
}

fn lower_statement(
    builder: &mut FnBodyBuilder,
    info: &concrete_ast::statements::Statement,
    ret_type: Option<TyKind>,
) {
    match info {
        statements::Statement::Assign(info) => lower_assign(builder, info),
        statements::Statement::Match(_) => todo!(),
        statements::Statement::For(_) => todo!(),
        statements::Statement::If(info) => lower_if_statement(builder, info),
        statements::Statement::Let(info) => lower_let(builder, info),
        statements::Statement::Return(info) => {
            lower_return(builder, info, ret_type);
        }
        statements::Statement::While(_) => todo!(),
        statements::Statement::FnCall(info) => {
            lower_fn_call(builder, info);
        }
    }
}

fn lower_if_statement(builder: &mut FnBodyBuilder, info: &IfExpr) {
    let discriminator = lower_expression(builder, &info.value, Some(TyKind::Bool));

    let local = builder.add_local(Local {
        span: None,
        ty: Ty {
            span: None,
            kind: TyKind::Bool,
        },
        kind: LocalKind::Temp,
    });
    let place = Place {
        local,
        projection: vec![],
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(place.clone(), discriminator),
    });

    // keep idx to change terminator
    let current_block_idx = builder.body.basic_blocks.len();

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });

    // keep idx for switch targets
    let first_then_block_idx = builder.body.basic_blocks.len();

    for stmt in &info.contents {
        lower_statement(
            builder,
            stmt,
            Some(builder.body.locals[builder.ret_local].ty.kind.clone()),
        );
    }

    // keet idx to change terminator
    let last_then_block_idx = builder.body.basic_blocks.len();
    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });

    let first_else_block_idx = builder.body.basic_blocks.len();

    if let Some(contents) = &info.r#else {
        for stmt in contents {
            lower_statement(
                builder,
                stmt,
                Some(builder.body.locals[builder.ret_local].ty.kind.clone()),
            );
        }
    }

    let last_else_block_idx = builder.body.basic_blocks.len();
    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });

    // Needed to ease codegen
    let otherwise_block_idx = builder.body.basic_blocks.len();
    builder.body.basic_blocks.push(BasicBlock {
        statements: vec![],
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });

    let targets = SwitchTargets {
        values: vec![
            ValueTree::Leaf(ConstValue::Bool(true)),
            ValueTree::Leaf(ConstValue::Bool(false)),
        ],
        targets: vec![
            first_then_block_idx,
            first_else_block_idx,
            otherwise_block_idx,
        ],
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[current_block_idx].terminator.kind = kind;

    let next_block_idx = builder.body.basic_blocks.len();
    builder.body.basic_blocks[last_then_block_idx]
        .terminator
        .kind = TerminatorKind::Goto {
        target: next_block_idx,
    };
    builder.body.basic_blocks[last_else_block_idx]
        .terminator
        .kind = TerminatorKind::Goto {
        target: next_block_idx,
    };
}

fn lower_let(builder: &mut FnBodyBuilder, info: &LetStmt) {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let ty = lower_type(r#type);
            let rvalue = lower_expression(builder, &info.value, Some(ty.kind));
            let local_idx = builder.name_to_local.get(&name.name).copied().unwrap();
            builder.statements.push(Statement {
                span: Some(name.span),
                kind: StatementKind::StorageLive(local_idx),
            });
            builder.statements.push(Statement {
                span: Some(name.span),
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

fn lower_assign(builder: &mut FnBodyBuilder, info: &AssignStmt) {
    let local = *builder.name_to_local.get(&info.target.first.name).unwrap();
    let ty = builder.body.locals[local].ty.clone();
    let rvalue = lower_expression(builder, &info.value, Some(ty.kind.clone()));
    let place = lower_path(builder, &info.target);

    builder.statements.push(Statement {
        span: Some(info.target.first.span),
        kind: StatementKind::Assign(place, rvalue),
    })
}

fn lower_return(builder: &mut FnBodyBuilder, info: &ReturnStmt, type_hint: Option<TyKind>) {
    let value = lower_expression(builder, &info.value, type_hint);
    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(
            Place {
                local: builder.ret_local,
                projection: vec![],
            },
            value,
        ),
    });

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Return,
        }),
    });
}

fn lower_expression(
    builder: &mut FnBodyBuilder,
    info: &Expression,
    type_hint: Option<TyKind>,
) -> Rvalue {
    match info {
        Expression::Value(info) => lower_value_expr(builder, info, type_hint),
        Expression::FnCall(info) => lower_fn_call(builder, info),
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(lhs, op, rhs) => lower_binary_op(builder, lhs, *op, rhs, type_hint),
    }
}

fn lower_fn_call(builder: &mut FnBodyBuilder, info: &FnCallOp) -> Rvalue {
    let fn_id = *builder
        .ctx
        .symbols
        .functions
        .get(&info.target.name)
        .unwrap();
    let (args_ty, ret_ty) = builder.ctx.function_signatures.get(&fn_id).unwrap().clone();

    let mut args = Vec::new();

    for (arg, arg_ty) in info.args.iter().zip(args_ty) {
        let rvalue = lower_expression(builder, arg, Some(arg_ty.kind.clone()));
        args.push(rvalue);
    }

    let dest_local = builder.add_local(Local {
        span: None,
        kind: LocalKind::Temp,
        ty: ret_ty,
    });

    let dest_place = Place {
        local: dest_local,
        projection: Vec::new(),
    };

    let target_block = builder.body.basic_blocks.len() + 1;

    // todo: check if function is diverging such as exit().
    let kind = TerminatorKind::Call {
        func: fn_id,
        args,
        destination: dest_place.clone(),
        target: Some(target_block),
    };

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: Some(info.target.span),
            kind,
        }),
    });

    Rvalue::Use(Operand::Place(dest_place))
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

    let local_ty = Ty {
        span: None,
        kind: expr_type.clone(),
    };
    let lhs_local = builder.add_local(Local {
        span: None,
        ty: local_ty.clone(),
        kind: LocalKind::Temp,
    });
    let rhs_local = builder.add_local(Local {
        span: None,
        ty: local_ty.clone(),
        kind: LocalKind::Temp,
    });
    let lhs_place = Place {
        local: lhs_local,
        projection: vec![],
    };
    let rhs_place = Place {
        local: lhs_local,
        projection: vec![],
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::StorageLive(lhs_local),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(lhs_place.clone(), lhs),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::StorageLive(rhs_local),
    });

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(rhs_place.clone(), rhs),
    });

    let lhs = Operand::Place(lhs_place);
    let rhs = Operand::Place(rhs_place);

    match op {
        BinaryOp::Arith(op) => match op {
            ArithOp::Add => Rvalue::BinaryOp(BinOp::Add, (lhs, rhs)),
            ArithOp::Sub => Rvalue::BinaryOp(BinOp::Sub, (lhs, rhs)),
            ArithOp::Mul => Rvalue::BinaryOp(BinOp::Mul, (lhs, rhs)),
            ArithOp::Div => Rvalue::BinaryOp(BinOp::Div, (lhs, rhs)),
            ArithOp::Mod => Rvalue::BinaryOp(BinOp::Mod, (lhs, rhs)),
        },
        BinaryOp::Logic(op) => match op {
            LogicOp::And => Rvalue::LogicOp(LogOp::And, (lhs, rhs)),
            LogicOp::Or => Rvalue::LogicOp(LogOp::Or, (lhs, rhs)),
        },
        BinaryOp::Compare(op) => match op {
            CmpOp::Eq => Rvalue::BinaryOp(BinOp::Eq, (lhs, rhs)),
            CmpOp::NotEq => Rvalue::BinaryOp(BinOp::Ne, (lhs, rhs)),
            CmpOp::Lt => Rvalue::BinaryOp(BinOp::Lt, (lhs, rhs)),
            CmpOp::LtEq => Rvalue::BinaryOp(BinOp::Le, (lhs, rhs)),
            CmpOp::Gt => Rvalue::BinaryOp(BinOp::Gt, (lhs, rhs)),
            CmpOp::GtEq => Rvalue::BinaryOp(BinOp::Ge, (lhs, rhs)),
        },
        BinaryOp::Bitwise(op) => match op {
            BitwiseOp::And => Rvalue::BinaryOp(BinOp::BitAnd, (lhs, rhs)),
            BitwiseOp::Or => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
            BitwiseOp::Xor => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
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
                    TyKind::Bool => ConstValue::Bool(*value != 0),
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
        ValueExpr::AsRef {
            path: _,
            ref_type: _,
        } => todo!(),
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
        TypeSpec::Generic { .. } => {
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
        "bool" => TyKind::Bool,
        "string" => TyKind::String,
        _ => todo!(),
    }
}
