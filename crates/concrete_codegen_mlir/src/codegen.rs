use std::{collections::HashMap, error::Error};

use bumpalo::Bump;
use concrete_check::ast_helper::{AstHelper, ModuleInfo};
use concrete_ir::{
    DefId, FnBody, LocalKind, ModuleBody, Operand, Place, ProgramBody, Rvalue, Statement, Ty,
    TyKind, ValueTree,
};
use concrete_session::Session;
use melior::{
    dialect::{
        arith::{self, CmpiPredicate},
        cf, func, llvm, memref,
    },
    ir::{
        attribute::{
            FlatSymbolRefAttribute, FloatAttribute, IntegerAttribute, StringAttribute,
            TypeAttribute,
        },
        r#type::{FunctionType, IntegerType, MemRefType},
        Block, BlockRef, Location, Module as MeliorModule, Operation, Region, Type, Value,
        ValueLike,
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

pub fn compile_module(ctx: ModuleCodegenCtx) -> Result<(), Box<dyn Error>> {
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

pub fn compile_function(ctx: FunctionCodegenCtx) -> Result<(), Box<dyn std::error::Error>> {
    let module = ctx.module_ctx.get_module_body();
    let body = ctx.get_fn_body();
    let (param_types, ret_type) = ctx.get_fn_sig();

    let region = Region::new();

    let params = body.get_params();
    let params_ty: Vec<_> = params
        .iter()
        .map(|x| {
            (
                compile_type(ctx.module_ctx, &x.ty),
                Location::unknown(ctx.context()),
            )
        })
        .collect();

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
                    let (value, _ty) = compile_rvalue(&ctx, mlir_block, rvalue, &locals);
                    compile_store_place(
                        &ctx,
                        mlir_block,
                        place,
                        *locals.get(&place.local).unwrap(),
                        value,
                    );
                }
                concrete_ir::StatementKind::StorageLive(_) => todo!(),
                concrete_ir::StatementKind::StorageDead(_) => todo!(),
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
                        .append_operation(memref::load(*ptr, &[], Location::unknown(ctx.context())))
                        .result(0)?
                        .into();
                    mlir_block.append_operation(func::r#return(
                        &[value],
                        Location::unknown(ctx.context()),
                    ));
                }
            }
            concrete_ir::TerminatorKind::Unreachable => {
                mlir_block.append_operation(llvm::unreachable(Location::unknown(ctx.context())));
            }
            concrete_ir::TerminatorKind::Call {
                func,
                args,
                destination,
                target,
            } => todo!(),
            concrete_ir::TerminatorKind::SwitchInt {
                discriminator,
                targets,
            } => todo!(),
        }
    }

    Ok(())
}

fn compile_rvalue<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Rvalue,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> (Value<'c, 'b>, TyKind) {
    match info {
        Rvalue::Use(info) => compile_load_operand(ctx, block, info, locals),
        Rvalue::LogicOp(_, _) => todo!(),
        Rvalue::BinaryOp(_, _) => todo!(),
        Rvalue::UnaryOp(_, _) => todo!(),
        Rvalue::Ref(_, _) => todo!(),
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
    ptr: Value<'c, 'b>,
    value: Value<'c, 'b>,
) {
    block.append_operation(memref::store(
        value,
        ptr,
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
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 1).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 8).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 16).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 32).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new((*value), IntegerType::new(ctx.context(), 64).into())
                        .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::I128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 128).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 8).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 16).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 32).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 64).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::U128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    IntegerAttribute::new(
                        (*value) as i64,
                        IntegerType::new(ctx.context(), 128).into(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)
                .unwrap()
                .into(),
            concrete_ir::ConstValue::F32(_) => todo!(),
            concrete_ir::ConstValue::F64(_) => todo!(),
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
