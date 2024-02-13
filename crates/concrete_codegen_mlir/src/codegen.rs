use std::{collections::HashMap, error::Error};

use concrete_ir::{
    BinOp, DefId, FnBody, LocalKind, ModuleBody, Operand, Place, ProgramBody, Rvalue, Span, Ty,
    TyKind, ValueTree,
};
use concrete_session::Session;
use melior::{
    dialect::{arith, cf, func, llvm, memref},
    ir::{
        attribute::{FlatSymbolRefAttribute, FloatAttribute, StringAttribute, TypeAttribute},
        r#type::{FunctionType, IntegerType, MemRefType},
        Attribute, Block, Location, Module as MeliorModule, Region, Type, Value,
    },
    Context as MeliorContext,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct CodegenCtx<'a> {
    pub mlir_context: &'a MeliorContext,
    pub mlir_module: &'a MeliorModule<'a>,
    pub session: &'a Session,
    pub program: &'a ProgramBody,
}

#[derive(Debug, Clone, Copy)]
struct ModuleCodegenCtx<'a> {
    pub ctx: CodegenCtx<'a>,
    pub module_id: DefId,
}

impl<'a> ModuleCodegenCtx<'a> {
    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx
            .program
            .modules
            .get(&self.module_id)
            .expect("module should exist")
    }

    pub fn get_location(&self, span: Option<Span>) -> Location {
        if let Some(span) = span {
            let (_, line, col) = self.ctx.session.source.get_offset_line(span.from).unwrap();
            Location::new(
                self.ctx.mlir_context,
                self.ctx
                    .session
                    .file_path
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap(),
                line + 1,
                col + 1,
            )
        } else {
            Location::unknown(self.ctx.mlir_context)
        }
    }
}

pub fn compile_program(ctx: CodegenCtx) -> Result<(), Box<dyn Error>> {
    for module_id in &ctx.program.top_level_modules {
        let ctx = ModuleCodegenCtx {
            ctx,
            module_id: *module_id,
        };
        compile_module(ctx)?;
    }
    Ok(())
}

fn compile_module(ctx: ModuleCodegenCtx) -> Result<(), Box<dyn Error>> {
    let body = ctx.get_module_body();

    for fn_id in body.functions.iter() {
        let ctx = FunctionCodegenCtx {
            module_ctx: ctx,
            function_id: *fn_id,
        };
        compile_function(ctx)?;
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct FunctionCodegenCtx<'a> {
    pub module_ctx: ModuleCodegenCtx<'a>,
    pub function_id: DefId,
}

impl<'a> FunctionCodegenCtx<'a> {
    pub fn get_fn_body(&self) -> &FnBody {
        self.module_ctx
            .ctx
            .program
            .functions
            .get(&self.function_id)
            .expect("function body should exist")
    }

    pub fn get_fn_sig(&self) -> &(Vec<Ty>, Ty) {
        self.module_ctx
            .ctx
            .program
            .function_signatures
            .get(&self.function_id)
            .expect("function body should exist")
    }

    pub fn context(&self) -> &MeliorContext {
        self.module_ctx.ctx.mlir_context
    }
}

fn compile_function(ctx: FunctionCodegenCtx) -> Result<(), Box<dyn std::error::Error>> {
    let body = ctx.get_fn_body();
    let body_sig = ctx.get_fn_sig();

    let region = Region::new();

    let params = body.get_params();

    let params_ty: Vec<_> = params
        .iter()
        .map(|x| {
            (
                compile_type(ctx.module_ctx, &x.ty),
                ctx.module_ctx.get_location(x.span),
            )
        })
        .collect();

    {
        let entry_block = region.append_block(Block::new(&params_ty));

        let mut locals = HashMap::new();
        let mut ret_local = None;

        let mut arg_counter = 0;

        for (index, local) in body.locals.iter().enumerate() {
            let ty = compile_type(ctx.module_ctx, &local.ty);
            match local.kind {
                LocalKind::Temp => {
                    let ptr: Value = entry_block
                        .append_operation(memref::alloca(
                            ctx.context(),
                            MemRefType::new(ty, &[], None, None),
                            &[],
                            &[],
                            None,
                            Location::unknown(ctx.context()),
                        ))
                        .result(0)?
                        .into();
                    locals.insert(index, ptr);
                }
                LocalKind::Arg => {
                    let arg_value: Value = entry_block.argument(arg_counter)?.into();
                    arg_counter += 1;
                    let ptr: Value = entry_block
                        .append_operation(memref::alloca(
                            ctx.context(),
                            MemRefType::new(ty, &[], None, None),
                            &[],
                            &[],
                            None,
                            Location::unknown(ctx.context()),
                        ))
                        .result(0)?
                        .into();

                    entry_block.append_operation(memref::store(
                        arg_value,
                        ptr,
                        &[],
                        Location::unknown(ctx.context()),
                    ));

                    locals.insert(index, ptr);
                }
                LocalKind::ReturnPointer => {
                    if let TyKind::Unit = local.ty.kind {
                    } else {
                        ret_local = Some(index);
                        let ptr: Value = entry_block
                            .append_operation(memref::alloca(
                                ctx.context(),
                                MemRefType::new(ty, &[], None, None),
                                &[],
                                &[],
                                None,
                                Location::unknown(ctx.context()),
                            ))
                            .result(0)?
                            .into();
                        locals.insert(index, ptr);
                    }
                }
            }
        }

        let mut blocks = Vec::with_capacity(body.basic_blocks.len());

        for _ in body.basic_blocks.iter() {
            let mlir_block = region.append_block(Block::new(&[]));
            blocks.push(mlir_block);
        }

        entry_block.append_operation(cf::br(&blocks[0], &[], Location::unknown(ctx.context())));

        for (block, mlir_block) in body.basic_blocks.iter().zip(blocks.iter()) {
            for statement in &block.statements {
                match &statement.kind {
                    concrete_ir::StatementKind::Assign(place, rvalue) => {
                        let (value, _ty) = compile_rvalue(&ctx, mlir_block, rvalue, &mut locals);
                        compile_store_place(&ctx, mlir_block, place, value, &locals);
                    }
                    concrete_ir::StatementKind::StorageLive(_) => {}
                    concrete_ir::StatementKind::StorageDead(_) => {}
                }
            }

            match &block.terminator.kind {
                concrete_ir::TerminatorKind::Goto { target } => {
                    mlir_block.append_operation(cf::br(
                        &blocks[*target],
                        &[],
                        Location::unknown(ctx.context()),
                    ));
                }
                concrete_ir::TerminatorKind::Return => {
                    if let Some(ret_local) = ret_local {
                        let ptr = locals.get(&ret_local).unwrap();
                        let value = mlir_block
                            .append_operation(memref::load(
                                *ptr,
                                &[],
                                Location::unknown(ctx.context()),
                            ))
                            .result(0)?
                            .into();
                        mlir_block.append_operation(func::r#return(
                            &[value],
                            Location::unknown(ctx.context()),
                        ));
                    } else {
                        mlir_block.append_operation(func::r#return(
                            &[],
                            Location::unknown(ctx.context()),
                        ));
                    }
                }
                concrete_ir::TerminatorKind::Unreachable => {
                    mlir_block
                        .append_operation(llvm::unreachable(Location::unknown(ctx.context())));
                }
                concrete_ir::TerminatorKind::Call {
                    func,
                    args,
                    destination,
                    target,
                } => {
                    let target_fn_body = ctx.module_ctx.ctx.program.functions.get(func).unwrap();
                    let target_fn_body_sig = ctx
                        .module_ctx
                        .ctx
                        .program
                        .function_signatures
                        .get(func)
                        .unwrap();
                    let args: Vec<Value> = args
                        .iter()
                        .map(|x| compile_rvalue(&ctx, mlir_block, x, &mut locals).0)
                        .collect();
                    let fn_symbol =
                        FlatSymbolRefAttribute::new(ctx.context(), &target_fn_body.name); // todo: good name resolution
                    let ret_type = match &target_fn_body_sig.1.kind {
                        TyKind::Unit => None,
                        _ => Some(compile_type(ctx.module_ctx, &target_fn_body_sig.1)),
                    };
                    let result = mlir_block.append_operation(func::call(
                        ctx.context(),
                        fn_symbol,
                        &args,
                        ret_type.as_slice(),
                        Location::unknown(ctx.context()),
                    ));

                    if result.result_count() > 0 {
                        compile_store_place(
                            &ctx,
                            mlir_block,
                            destination,
                            result.result(0)?.into(),
                            &locals,
                        );
                    }

                    if let Some(target) = target {
                        mlir_block.append_operation(cf::br(
                            &blocks[*target],
                            &[],
                            Location::unknown(ctx.context()),
                        ));
                    }
                }
                concrete_ir::TerminatorKind::SwitchInt { .. } => todo!(),
            }
        }
    }

    let param_types: Vec<_> = params_ty.iter().map(|x| x.0).collect();
    let ret_type = match &body_sig.1.kind {
        TyKind::Unit => None,
        _ => Some(compile_type(ctx.module_ctx, &body_sig.1)),
    };
    let func_type = FunctionType::new(ctx.context(), &param_types, ret_type.as_slice());

    let func = func::func(
        ctx.context(),
        StringAttribute::new(ctx.context(), &body.name),
        TypeAttribute::new(func_type.into()),
        region,
        &[],
        Location::unknown(ctx.context()),
    );

    ctx.module_ctx.ctx.mlir_module.body().append_operation(func);

    Ok(())
}

fn compile_rvalue<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Rvalue,
    locals: &mut HashMap<usize, Value<'c, '_>>,
) -> (Value<'c, 'b>, TyKind) {
    match info {
        Rvalue::Use(info) => compile_load_operand(ctx, block, info, locals),
        Rvalue::LogicOp(_, _) => todo!(),
        Rvalue::BinaryOp(op, (lhs, rhs)) => compile_binop(ctx, block, op, lhs, rhs, locals),
        Rvalue::UnaryOp(_, _) => todo!(),
        Rvalue::Ref(_, _) => todo!(),
    }
}

fn compile_binop<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    op: &BinOp,
    lhs: &Operand,
    rhs: &Operand,
    locals: &mut HashMap<usize, Value<'c, '_>>,
) -> (Value<'c, 'b>, TyKind) {
    let (lhs, lhs_ty) = compile_load_operand(ctx, block, lhs, locals);
    let (rhs, _rhs_ty) = compile_load_operand(ctx, block, rhs, locals);
    let location = Location::unknown(ctx.context());

    let is_float = matches!(lhs_ty, TyKind::Float(_));
    let is_signed = matches!(lhs_ty, TyKind::Int(_));

    match op {
        BinOp::Add => {
            let value = if is_float {
                block
                    .append_operation(arith::addf(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else {
                block
                    .append_operation(arith::addi(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Sub => {
            let value = if is_float {
                block
                    .append_operation(arith::subf(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else {
                block
                    .append_operation(arith::subi(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Mul => {
            let value = if is_float {
                block
                    .append_operation(arith::mulf(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else {
                block
                    .append_operation(arith::muli(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Div => {
            let value = if is_float {
                block
                    .append_operation(arith::divf(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::divsi(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else {
                block
                    .append_operation(arith::divui(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Mod => {
            let value = if is_float {
                block
                    .append_operation(arith::remf(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::remsi(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            } else {
                block
                    .append_operation(arith::remui(lhs, rhs, location))
                    .result(0)
                    .unwrap()
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::BitXor => todo!(),
        BinOp::BitAnd => todo!(),
        BinOp::BitOr => todo!(),
        BinOp::Shl => todo!(),
        BinOp::Shr => todo!(),
        BinOp::Eq => todo!(),
        BinOp::Lt => todo!(),
        BinOp::Le => todo!(),
        BinOp::Ne => todo!(),
        BinOp::Ge => todo!(),
        BinOp::Gt => todo!(),
    }
}

fn compile_load_operand<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Operand,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> (Value<'c, 'b>, TyKind) {
    match info {
        Operand::Place(info) => compile_load_place(ctx, block, info, locals),
        Operand::Const(data) => match &data.data {
            concrete_ir::ConstKind::Param(_) => todo!(),
            concrete_ir::ConstKind::Value(value) => {
                (compile_value_tree(ctx, block, value), data.ty.clone())
            }
            concrete_ir::ConstKind::Expr(_) => todo!(),
        },
    }
}

fn compile_store_place<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Place,
    value: Value<'c, 'b>,
    locals: &HashMap<usize, Value<'c, '_>>,
) {
    // todo: resolve projection too
    let ptr = locals.get(&info.local).unwrap();
    block.append_operation(memref::store(
        value,
        *ptr,
        &[],
        Location::unknown(ctx.context()),
    ));
}

fn compile_load_place<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Place,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> (Value<'c, 'b>, TyKind) {
    // todo: resolve projection too
    let ptr = locals.get(&info.local).unwrap();
    let body = ctx.get_fn_body();
    let local_ty = body.locals[info.local].ty.kind.clone();
    let val = block
        .append_operation(memref::load(*ptr, &[], Location::unknown(ctx.context())))
        .result(0)
        .unwrap()
        .into();
    (val, local_ty)
}

fn compile_value_tree<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    value: &ValueTree,
) -> Value<'c, 'b> {
    match value {
        ValueTree::Leaf(value) => match value {
            concrete_ir::ConstValue::Bool(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i1", (*value) as u8)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::F32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    FloatAttribute::new(
                        ctx.context(),
                        (*value).into(),
                        Type::float32(ctx.context()),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::F64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    FloatAttribute::new(ctx.context(), *value, Type::float64(ctx.context())).into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
        },
        ValueTree::Branch(_) => todo!(),
    }
}

fn compile_type<'c>(ctx: ModuleCodegenCtx<'c>, ty: &Ty) -> Type<'c> {
    match &ty.kind {
        concrete_ir::TyKind::Unit => todo!(),
        concrete_ir::TyKind::Bool => IntegerType::new(ctx.ctx.mlir_context, 1).into(),
        concrete_ir::TyKind::Char => IntegerType::new(ctx.ctx.mlir_context, 32).into(),
        concrete_ir::TyKind::Int(int_ty) => match int_ty {
            concrete_ir::IntTy::I8 => IntegerType::new(ctx.ctx.mlir_context, 8).into(),
            concrete_ir::IntTy::I16 => IntegerType::new(ctx.ctx.mlir_context, 16).into(),
            concrete_ir::IntTy::I32 => IntegerType::new(ctx.ctx.mlir_context, 32).into(),
            concrete_ir::IntTy::I64 => IntegerType::new(ctx.ctx.mlir_context, 64).into(),
            concrete_ir::IntTy::I128 => IntegerType::new(ctx.ctx.mlir_context, 128).into(),
        },
        concrete_ir::TyKind::Uint(uint_ty) => match uint_ty {
            concrete_ir::UintTy::U8 => IntegerType::new(ctx.ctx.mlir_context, 8).into(),
            concrete_ir::UintTy::U16 => IntegerType::new(ctx.ctx.mlir_context, 16).into(),
            concrete_ir::UintTy::U32 => IntegerType::new(ctx.ctx.mlir_context, 32).into(),
            concrete_ir::UintTy::U64 => IntegerType::new(ctx.ctx.mlir_context, 64).into(),
            concrete_ir::UintTy::U128 => IntegerType::new(ctx.ctx.mlir_context, 128).into(),
        },
        concrete_ir::TyKind::Float(float_ty) => match float_ty {
            concrete_ir::FloatTy::F32 => Type::float32(ctx.ctx.mlir_context),
            concrete_ir::FloatTy::F64 => Type::float64(ctx.ctx.mlir_context),
        },
        concrete_ir::TyKind::String => todo!(),
        concrete_ir::TyKind::Array(_, _) => todo!(),
        concrete_ir::TyKind::Ref(_, _) => todo!(),
        concrete_ir::TyKind::Param { .. } => todo!(),
    }
}
