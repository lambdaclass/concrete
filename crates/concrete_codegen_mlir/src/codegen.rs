use std::collections::HashMap;

use concrete_ir::{
    BinOp, DefId, FnBody, LocalKind, ModuleBody, Operand, Place, PlaceElem, ProgramBody, Rvalue,
    Span, Ty, TyKind, ValueTree,
};
use concrete_session::Session;
use melior::{
    dialect::{arith, cf, func, llvm, memref},
    ir::{
        attribute::{FlatSymbolRefAttribute, FloatAttribute, StringAttribute, TypeAttribute},
        r#type::{FunctionType, IntegerType, MemRefType},
        Attribute, Block, Location, Module as MeliorModule, Region, Type, Value, ValueLike,
    },
    Context as MeliorContext,
};

use crate::errors::CodegenError;

/// Global codegen context
#[derive(Debug, Clone, Copy)]
pub(crate) struct CodegenCtx<'a> {
    /// The MLIR context.
    pub mlir_context: &'a MeliorContext,
    /// The MLIR module.
    pub mlir_module: &'a MeliorModule<'a>,
    /// The compile session info.
    pub session: &'a Session,
    /// The program IR.
    pub program: &'a ProgramBody,
}

/// Codegen context for a module
#[derive(Debug, Clone, Copy)]
struct ModuleCodegenCtx<'a> {
    pub ctx: CodegenCtx<'a>,
    /// The id of the module.
    pub module_id: DefId,
}

impl<'a> ModuleCodegenCtx<'a> {
    /// Gets the module IR body.
    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx
            .program
            .modules
            .get(&self.module_id)
            .expect("module should exist")
    }

    /// Gets a MLIR location from the given span, or unknown if the span is `None`.
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

/// Compiles the program within the context.
pub(crate) fn compile_program(ctx: CodegenCtx) -> Result<(), CodegenError> {
    for module_id in &ctx.program.top_level_modules {
        let ctx = ModuleCodegenCtx {
            ctx,
            module_id: *module_id,
        };
        compile_module(ctx)?;
    }
    Ok(())
}

/// Compiles the given module within the context.
fn compile_module(ctx: ModuleCodegenCtx) -> Result<(), CodegenError> {
    let body = ctx.get_module_body();

    for fn_id in body.functions.iter() {
        let ctx = FunctionCodegenCtx {
            module_ctx: ctx,
            function_id: *fn_id,
        };
        compile_function(ctx)?;
    }

    for mod_id in body.modules.iter() {
        let mut sub_ctx = ctx;
        sub_ctx.module_id = *mod_id;
        compile_module(sub_ctx)?;
    }

    Ok(())
}

/// Context used when compiling code within a function body.
#[derive(Debug, Clone, Copy)]
struct FunctionCodegenCtx<'a> {
    pub module_ctx: ModuleCodegenCtx<'a>,
    pub function_id: DefId,
}

impl<'a> FunctionCodegenCtx<'a> {
    /// Gets the function IR body.
    pub fn get_fn_body(&self) -> &FnBody {
        self.module_ctx
            .ctx
            .program
            .functions
            .get(&self.function_id)
            .expect("function body should exist")
    }

    /// Gets the function argument types and return type.
    pub fn get_fn_signature(&self) -> &(Vec<Ty>, Ty) {
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

/// Compiles the given function IR.
fn compile_function(ctx: FunctionCodegenCtx) -> Result<(), CodegenError> {
    let body = ctx.get_fn_body();
    let body_signature = ctx.get_fn_signature();

    // Functions only have 1 region with multiple blocks within.
    let region = Region::new();

    let params = body.get_params();

    // Compile the parameter types.
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
        // The entry block doesn't exist in the IR, its where we create all the stack allocations for the locals.
        let entry_block = region.append_block(Block::new(&params_ty));

        let mut locals = HashMap::new();
        // Store the return local index for easier use.
        let mut return_local = None;

        // Keep track of the parameter index so we can get it from the block argument.
        // Since all the locals are together in a unspecified order in the IR.
        let mut param_index = 0;

        for (index, local) in body.locals.iter().enumerate() {
            // Get the MLIR local type.
            let local_mlir_type = compile_type(ctx.module_ctx, &local.ty);

            match local.kind {
                // User-declared variable binding or compiler-introduced temporary.
                LocalKind::Temp => {
                    let ptr: Value = entry_block
                        .append_operation(memref::alloca(
                            ctx.context(),
                            MemRefType::new(local_mlir_type, &[], None, None),
                            &[],
                            &[],
                            None,
                            Location::unknown(ctx.context()),
                        ))
                        .result(0)?
                        .into();
                    locals.insert(index, ptr);
                }
                // Argument local.
                LocalKind::Arg => {
                    let arg_value: Value = entry_block.argument(param_index)?.into();
                    param_index += 1;

                    let ptr: Value = entry_block
                        .append_operation(memref::alloca(
                            ctx.context(),
                            MemRefType::new(local_mlir_type, &[], None, None),
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
                // Return pointer.
                LocalKind::ReturnPointer => {
                    if let TyKind::Unit = local.ty.kind {
                    } else {
                        return_local = Some(index);
                        let ptr: Value = entry_block
                            .append_operation(memref::alloca(
                                ctx.context(),
                                MemRefType::new(local_mlir_type, &[], None, None),
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

        // Create in advance all the needed blocks, 1 block per IR block.
        // Since we use stack allocas we don't need to handle block arguments within.
        // The optimizer takes care of deciding if this allocas are better kept in the stack or in a register
        // based on register allocation and whether the address of the allocas is used somewhere.
        // Since register variables can't have their address taken.
        let mut blocks = Vec::with_capacity(body.basic_blocks.len());

        for _ in body.basic_blocks.iter() {
            let mlir_block = region.append_block(Block::new(&[]));
            blocks.push(mlir_block);
        }

        // Jump from the entry block to the first IR block.
        entry_block.append_operation(cf::br(&blocks[0], &[], Location::unknown(ctx.context())));

        // Process each block.
        for (block, mlir_block) in body.basic_blocks.iter().zip(blocks.iter()) {
            // Within blocks there is no control flow, so we simply give the current block to the
            // codegen functions.
            for statement in &block.statements {
                match &statement.kind {
                    concrete_ir::StatementKind::Assign(place, rvalue) => {
                        let (value, _ty) = compile_rvalue(&ctx, mlir_block, rvalue, &locals)?;
                        compile_store_place(&ctx, mlir_block, place, value, &locals)?;
                    }
                    concrete_ir::StatementKind::StorageLive(_) => {}
                    concrete_ir::StatementKind::StorageDead(_) => {}
                }
            }

            // Jump based on the terminator.
            match &block.terminator.kind {
                concrete_ir::TerminatorKind::Goto { target } => {
                    mlir_block.append_operation(cf::br(
                        &blocks[*target],
                        &[],
                        Location::unknown(ctx.context()),
                    ));
                }
                concrete_ir::TerminatorKind::Return => {
                    // Load the return value from the return local and return it.
                    if let Some(ret_local) = return_local {
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
                // Function calls are terminators because a function may be diverging (i.e it doesn't return).
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
                        .map(|x| compile_rvalue(&ctx, mlir_block, x, &locals).map(|x| x.0))
                        .collect::<Result<_, _>>()?;
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
                        )?;
                    }

                    if let Some(target) = target {
                        mlir_block.append_operation(cf::br(
                            &blocks[*target],
                            &[],
                            Location::unknown(ctx.context()),
                        ));
                    } else {
                        mlir_block
                            .append_operation(llvm::unreachable(Location::unknown(ctx.context())));
                    }
                }
                // A switch int is used for branching by matching against 1 or multiple values.
                concrete_ir::TerminatorKind::SwitchInt {
                    discriminator,
                    targets,
                } => {
                    let (condition, _condition_ty) =
                        compile_load_operand(&ctx, mlir_block, discriminator, &locals)?;

                    // Case constant values to match against.
                    let mut case_values = Vec::new();
                    // MLIR detail to tell how many arguments we pass, since it can take a dynamic amount of arguments.
                    let mut cases_operands_count = Vec::new();
                    // The block destinations.
                    let mut dests: Vec<(&Block, _)> = Vec::new();

                    cases_operands_count.push(0);

                    for (value, target) in targets.values.iter().zip(targets.targets.iter()) {
                        let target = *target;
                        let block = &blocks[target];
                        let value = value_tree_to_int(value)
                            .expect("constant value can't be converted to int");
                        case_values.push(value);
                        cases_operands_count.push(1);
                        dests.push((block, [].as_slice()));
                    }

                    mlir_block.append_operation(cf::switch(
                        ctx.context(),
                        &case_values,
                        condition,
                        condition.r#type(),
                        // Default dest block
                        (&blocks[*targets.targets.last().unwrap()], &[]),
                        // Destinations
                        &dests,
                        Location::unknown(ctx.context()),
                    )?);
                }
            }
        }
    }

    let param_types: Vec<_> = params_ty.iter().map(|x| x.0).collect();
    // If the return type is unit, pass a empty slice to mlir.
    let return_type = match &body_signature.1.kind {
        TyKind::Unit => None,
        _ => Some(compile_type(ctx.module_ctx, &body_signature.1)),
    };

    // Create the function mlir attribute.
    let func_type = FunctionType::new(ctx.context(), &param_types, return_type.as_slice());

    let func_op = func::func(
        ctx.context(),
        StringAttribute::new(ctx.context(), &body.name),
        TypeAttribute::new(func_type.into()),
        region,
        &[],
        Location::unknown(ctx.context()),
    );

    ctx.module_ctx
        .ctx
        .mlir_module
        .body()
        .append_operation(func_op);

    Ok(())
}

/// Compiles a rvalue.
fn compile_rvalue<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Rvalue,
    locals: &'b HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TyKind), CodegenError> {
    Ok(match info {
        Rvalue::Use(info) => compile_load_operand(ctx, block, info, locals)?,
        Rvalue::LogicOp(_, _) => todo!(),
        Rvalue::BinaryOp(op, (lhs, rhs)) => compile_binop(ctx, block, op, lhs, rhs, locals)?,
        Rvalue::UnaryOp(_, _) => todo!(),
        Rvalue::Ref(mutability, place) => {
            let mut value = locals[&place.local];
            let mut local_ty = ctx.get_fn_body().locals[place.local].ty.kind.clone();

            // Handle the projection.
            for projection in &place.projection {
                match projection {
                    PlaceElem::Deref => {
                        value = block
                            .append_operation(memref::load(
                                value,
                                &[],
                                Location::unknown(ctx.context()),
                            ))
                            .result(0)?
                            .into();
                        local_ty = match local_ty {
                            TyKind::Ref(inner, _) => *(inner.clone()),
                            _ => unreachable!(),
                        }
                    }
                    PlaceElem::Field(_, _) => todo!(),
                    PlaceElem::Index(_) => todo!(),
                }
            }

            (value, TyKind::Ref(Box::new(local_ty), *mutability))
        }
    })
}

/// Compiles a binary operation.
fn compile_binop<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    op: &BinOp,
    lhs: &Operand,
    rhs: &Operand,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TyKind), CodegenError> {
    let (lhs, lhs_ty) = compile_load_operand(ctx, block, lhs, locals)?;
    let (rhs, _rhs_ty) = compile_load_operand(ctx, block, rhs, locals)?;
    let location = Location::unknown(ctx.context());

    let is_float = matches!(lhs_ty, TyKind::Float(_));
    let is_signed = matches!(lhs_ty, TyKind::Int(_));

    Ok(match op {
        BinOp::Add => {
            let value = if is_float {
                block
                    .append_operation(arith::addf(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::addi(lhs, rhs, location))
                    .result(0)?
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Sub => {
            let value = if is_float {
                block
                    .append_operation(arith::subf(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::subi(lhs, rhs, location))
                    .result(0)?
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Mul => {
            let value = if is_float {
                block
                    .append_operation(arith::mulf(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::muli(lhs, rhs, location))
                    .result(0)?
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Div => {
            let value = if is_float {
                block
                    .append_operation(arith::divf(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::divsi(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::divui(lhs, rhs, location))
                    .result(0)?
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::Mod => {
            let value = if is_float {
                block
                    .append_operation(arith::remf(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::remsi(lhs, rhs, location))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::remui(lhs, rhs, location))
                    .result(0)?
                    .into()
            };
            (value, lhs_ty)
        }
        BinOp::BitXor => todo!(),
        BinOp::BitAnd => todo!(),
        BinOp::BitOr => todo!(),
        BinOp::Shl => todo!(),
        BinOp::Shr => todo!(),
        BinOp::Eq => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::Oeq,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Eq,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
        BinOp::Lt => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::Olt,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Slt,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Ult,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
        BinOp::Le => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::Ole,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Sle,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Ule,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
        BinOp::Ne => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::One,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Ult,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
        BinOp::Ge => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::Ole,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Sge,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Uge,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
        BinOp::Gt => {
            let value = if is_float {
                block
                    .append_operation(arith::cmpf(
                        ctx.context(),
                        arith::CmpfPredicate::Ogt,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else if is_signed {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Sgt,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            } else {
                block
                    .append_operation(arith::cmpi(
                        ctx.context(),
                        arith::CmpiPredicate::Ugt,
                        lhs,
                        rhs,
                        location,
                    ))
                    .result(0)?
                    .into()
            };
            (value, TyKind::Bool)
        }
    })
}

fn compile_load_operand<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Operand,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TyKind), CodegenError> {
    Ok(match info {
        Operand::Place(info) => compile_load_place(ctx, block, info, locals)?,
        Operand::Const(data) => match &data.data {
            concrete_ir::ConstKind::Param(_) => todo!(),
            concrete_ir::ConstKind::Value(value) => {
                (compile_value_tree(ctx, block, value)?, data.ty.clone())
            }
            concrete_ir::ConstKind::Expr(_) => todo!(),
        },
    })
}

/// Compiles a store to a given place in memory.
fn compile_store_place<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Place,
    value: Value<'c, 'b>,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(), CodegenError> {
    let mut ptr = locals[&info.local];

    for proj in &info.projection {
        match proj {
            PlaceElem::Deref => {
                ptr = block
                    .append_operation(memref::load(ptr, &[], Location::unknown(ctx.context())))
                    .result(0)?
                    .into();
            }
            PlaceElem::Field(_, _) => todo!(),
            PlaceElem::Index(_) => todo!(),
        }
    }

    block.append_operation(memref::store(
        value,
        ptr,
        &[],
        Location::unknown(ctx.context()),
    ));

    Ok(())
}

/// Compiles a load to a place.
fn compile_load_place<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Place,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TyKind), CodegenError> {
    let ptr = locals[&info.local];
    let body = ctx.get_fn_body();

    let mut local_ty = body.locals[info.local].ty.kind.clone();

    let mut value = block
        .append_operation(memref::load(ptr, &[], Location::unknown(ctx.context())))
        .result(0)?
        .into();

    for projection in &info.projection {
        match projection {
            PlaceElem::Deref => {
                value = block
                    .append_operation(memref::load(value, &[], Location::unknown(ctx.context())))
                    .result(0)?
                    .into();
                local_ty = match local_ty {
                    TyKind::Ref(inner, _) => *(inner.clone()),
                    _ => unreachable!(),
                }
            }
            PlaceElem::Field(_, _) => todo!(),
            PlaceElem::Index(_) => todo!(),
        }
    }
    Ok((value, local_ty))
}

/// Used in switch
/// Converts a constant value to a int.
fn value_tree_to_int(value: &ValueTree) -> Option<i64> {
    match value {
        ValueTree::Leaf(value) => match value {
            concrete_ir::ConstValue::Bool(value) => Some((*value) as i64),
            concrete_ir::ConstValue::I8(value) => Some((*value) as i64),
            concrete_ir::ConstValue::I16(value) => Some((*value) as i64),
            concrete_ir::ConstValue::I32(value) => Some((*value) as i64),
            concrete_ir::ConstValue::I64(value) => Some(*value),
            concrete_ir::ConstValue::I128(value) => Some((*value) as i64),
            concrete_ir::ConstValue::U8(value) => Some((*value) as i64),
            concrete_ir::ConstValue::U16(value) => Some((*value) as i64),
            concrete_ir::ConstValue::U32(value) => Some((*value) as i64),
            concrete_ir::ConstValue::U64(value) => Some((*value) as i64),
            concrete_ir::ConstValue::U128(value) => Some((*value) as i64),
            concrete_ir::ConstValue::F32(_) => None,
            concrete_ir::ConstValue::F64(_) => None,
        },
        ValueTree::Branch(_) => None,
    }
}

/// compiles constant data
fn compile_value_tree<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    value: &ValueTree,
) -> Result<Value<'c, 'b>, CodegenError> {
    Ok(match value {
        ValueTree::Leaf(value) => match value {
            concrete_ir::ConstValue::Bool(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i1", (*value) as u8)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::I8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::I16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::I32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::I64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::I128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::U8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::U16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::U32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::U64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            concrete_ir::ConstValue::U128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
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
                .result(0)?
                .into(),
            concrete_ir::ConstValue::F64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    FloatAttribute::new(ctx.context(), *value, Type::float64(ctx.context())).into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
        },
        ValueTree::Branch(_) => todo!(),
    })
}

fn compile_type<'c>(ctx: ModuleCodegenCtx<'c>, ty: &Ty) -> Type<'c> {
    match &ty.kind {
        concrete_ir::TyKind::Unit => Type::none(ctx.ctx.mlir_context),
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
        concrete_ir::TyKind::Ref(inner_ty, _) => {
            let inner = compile_type(
                ctx,
                &Ty {
                    span: None,
                    kind: *(inner_ty).clone(),
                },
            );
            MemRefType::new(inner, &[], None, None).into()
        }
        concrete_ir::TyKind::Param { .. } => todo!(),
    }
}
