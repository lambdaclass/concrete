use std::collections::HashMap;

use crate::ir::{
    BinOp, ConstValue, FnBody, FnIndex, LocalKind, ModuleBody, ModuleIndex, Operand, Place,
    PlaceElem, ProgramBody, Rvalue, Span, TyKind, TypeIndex, ValueTree,
};
use ariadne::Source;
use melior::ir::BlockLike;
use melior::{
    dialect::{
        arith, cf, func,
        llvm::{self, r#type::pointer, AllocaOptions, LoadStoreOptions},
        ods,
    },
    ir::{
        attribute::{
            DenseI32ArrayAttribute, FlatSymbolRefAttribute, FloatAttribute, IntegerAttribute,
            StringAttribute, TypeAttribute,
        },
        r#type::{FunctionType, IntegerType},
        Attribute, Block, Identifier, Location, Module as MeliorModule, Region, Type, Value,
        ValueLike,
    },
    Context as MeliorContext,
};
use tracing::info;

use super::errors::CodegenError;

/// Global codegen context
#[derive(Debug, Clone, Copy)]
pub(crate) struct CodegenCtx<'a> {
    /// The MLIR context.
    pub mlir_context: &'a MeliorContext,
    /// The MLIR module.
    pub mlir_module: &'a MeliorModule<'a>,
    /// The program IR.
    pub program: &'a ProgramBody,
}

/// Codegen context for a module
#[derive(Debug, Clone, Copy)]
struct ModuleCodegenCtx<'a> {
    pub ctx: CodegenCtx<'a>,
    /// The id of the module.
    pub module_id: ModuleIndex,
}

impl ModuleCodegenCtx<'_> {
    /// Gets the module IR body.
    pub fn get_module_body(&self) -> &ModuleBody {
        &self.ctx.program.modules[self.module_id]
    }

    /// Gets a MLIR location from the given span, or unknown if the span is `None`.
    pub fn get_location(&self, span: Option<Span>) -> Location {
        if let Some(span) = span {
            let source = std::fs::read_to_string(&self.get_module_body().file_path).unwrap();
            let (_, line, col) = Source::from(source).get_offset_line(span.from).unwrap();
            Location::new(
                self.ctx.mlir_context,
                self.get_module_body()
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

    pub fn get_type(&self, ty: TypeIndex) -> TyKind {
        self.ctx.program.types[ty].clone().unwrap()
    }

    pub fn get_fn_signature(&self, id: FnIndex) -> (Vec<TyKind>, TyKind) {
        let body = self.ctx.program.functions[id].as_ref().unwrap();
        let mut args = Vec::new();

        for arg_idx in &body.args {
            let ty = self.get_type(*arg_idx);
            args.push(ty);
        }

        let ret_ty = self.get_type(body.ret_ty);

        (args, ret_ty)
    }
}

/// Compiles the program within the context.
pub(crate) fn compile_program(ctx: CodegenCtx) -> Result<(), CodegenError> {
    info!("compiling program");
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
    info!("compiling module {:?}", body.name);

    for fn_id in body.functions.iter() {
        let ctx = FunctionCodegenCtx {
            module: ctx,
            fn_idx: *fn_id,
        };
        compile_function(ctx)?;
    }

    for mod_id in body.modules.values() {
        let mut sub_ctx = ctx;
        sub_ctx.module_id = *mod_id;
        compile_module(sub_ctx)?;
    }

    Ok(())
}

/// Context used when compiling code within a function body.
#[derive(Debug, Clone, Copy)]
struct FunctionCodegenCtx<'a> {
    pub module: ModuleCodegenCtx<'a>,
    pub fn_idx: FnIndex,
}

impl FunctionCodegenCtx<'_> {
    /// Gets the function IR body.
    pub fn get_fn_body(&self) -> &FnBody {
        self.module.ctx.program.functions[self.fn_idx]
            .as_ref()
            .unwrap()
    }

    /// Gets the function argument types and return type.
    pub fn get_fn_signature(&self) -> (Vec<TyKind>, TyKind) {
        self.module.get_fn_signature(self.fn_idx)
    }

    pub fn context(&self) -> &MeliorContext {
        self.module.ctx.mlir_context
    }
}

/// Compiles the given function IR.
fn compile_function(ctx: FunctionCodegenCtx) -> Result<(), CodegenError> {
    let body = ctx.get_fn_body();

    // Only codegen once.
    if !body.module_idx.eq(&ctx.module.module_id) {
        return Ok(());
    }

    let body_signature = ctx.get_fn_signature();

    info!("compiling function {}", body.name);

    // Functions only have 1 region with multiple blocks within.
    let region = Region::new();

    // Compile the parameter types.
    let params_ty: Vec<_> = body_signature
        .0
        .iter()
        .map(|x| {
            (
                compile_type(ctx.module, x),
                ctx.module.get_location(None), // Todo: add arg span vec to ir
            )
        })
        .collect();

    if !body.is_extern {
        // The entry block doesn't exist in the IR, its where we create all the stack allocations for the locals.
        let entry_block = region.append_block(Block::new(&params_ty));

        let mut locals = HashMap::new();
        // Store the return local index for easier use.
        let mut return_local = None;

        // Keep track of the parameter index so we can get it from the block argument.
        // Since all the locals are together in a unspecified order in the IR.
        let mut param_index = 0;

        let location = Location::unknown(ctx.context());

        let const1 = entry_block
            .append_operation(arith::constant(
                ctx.context(),
                IntegerAttribute::new(IntegerType::new(ctx.context(), 64).into(), 1).into(),
                location,
            ))
            .result(0)?
            .into();

        for (index, local) in body.locals.iter().enumerate() {
            // Get the MLIR local type.
            let local_ty = ctx.module.get_type(local.ty);
            let local_mlir_type = compile_type(ctx.module, &local_ty);

            match local.kind {
                // User-declared variable binding or compiler-introduced temporary.
                LocalKind::Temp => {
                    let ptr: Value = entry_block
                        .append_operation(llvm::alloca(
                            ctx.context(),
                            const1,
                            pointer(ctx.context(), 0),
                            location,
                            AllocaOptions::new()
                                .elem_type(Some(TypeAttribute::new(local_mlir_type))),
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
                        .append_operation(llvm::alloca(
                            ctx.context(),
                            const1,
                            pointer(ctx.context(), 0),
                            location,
                            AllocaOptions::new()
                                .elem_type(Some(TypeAttribute::new(local_mlir_type))),
                        ))
                        .result(0)?
                        .into();

                    entry_block.append_operation(llvm::store(
                        ctx.context(),
                        arg_value,
                        ptr,
                        location,
                        LoadStoreOptions::default(),
                    ));

                    locals.insert(index, ptr);
                }
                // Return pointer.
                LocalKind::ReturnPointer => {
                    let ty = ctx.module.get_type(local.ty);
                    if let TyKind::Unit = ty {
                    } else {
                        return_local = Some(index);
                        let ptr: Value = entry_block
                            .append_operation(llvm::alloca(
                                ctx.context(),
                                const1,
                                pointer(ctx.context(), 0),
                                location,
                                AllocaOptions::new()
                                    .elem_type(Some(TypeAttribute::new(local_mlir_type))),
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
                    crate::ir::StatementKind::Assign(place, rvalue) => {
                        let (value, _ty) = compile_rvalue(&ctx, mlir_block, rvalue, &locals)?;
                        compile_store_place(&ctx, mlir_block, place, value, &locals)?;
                    }
                    crate::ir::StatementKind::StorageLive(_) => {}
                    crate::ir::StatementKind::StorageDead(_) => {}
                }
            }

            // Jump based on the terminator.
            match &block.terminator.kind {
                crate::ir::TerminatorKind::Goto { target } => {
                    mlir_block.append_operation(cf::br(
                        &blocks[*target],
                        &[],
                        Location::unknown(ctx.context()),
                    ));
                }
                crate::ir::TerminatorKind::Return => {
                    // Load the return value from the return local and return it.
                    if let Some(ret_local) = return_local {
                        let ptr = locals.get(&ret_local).unwrap();
                        let ret_ty = ctx.module.get_type(body.locals[ret_local].ty);
                        let value = mlir_block
                            .append_operation(llvm::load(
                                ctx.context(),
                                *ptr,
                                compile_type(ctx.module, &ret_ty),
                                Location::unknown(ctx.context()),
                                LoadStoreOptions::default(),
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
                crate::ir::TerminatorKind::Unreachable => {
                    mlir_block
                        .append_operation(llvm::unreachable(Location::unknown(ctx.context())));
                }
                // Function calls are terminators because a function may be diverging (i.e it doesn't return).
                crate::ir::TerminatorKind::Call {
                    func,
                    args,
                    destination,
                    target,
                } => {
                    let target_fn_body = ctx.module.ctx.program.functions[*func].as_ref().unwrap();
                    let target_fn_body_sig = ctx.module.get_fn_signature(*func);
                    let args: Vec<Value> = args
                        .iter()
                        .map(|x| compile_rvalue(&ctx, mlir_block, x, &locals).map(|x| x.0))
                        .collect::<Result<_, _>>()?;
                    let fn_symbol = FlatSymbolRefAttribute::new(
                        ctx.context(),
                        &target_fn_body.get_mangled_name(),
                    );
                    let ret_type = target_fn_body_sig.1;
                    let ret_type = match &ret_type {
                        TyKind::Unit => None,
                        _ => Some(compile_type(ctx.module, &ret_type)),
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
                crate::ir::TerminatorKind::SwitchInt {
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
    let return_type = match &body_signature.1 {
        TyKind::Unit => None,
        _ => Some(compile_type(ctx.module, &body_signature.1)),
    };

    // Create the function mlir attribute.
    let func_type = FunctionType::new(ctx.context(), &param_types, return_type.as_slice());

    let mut fn_attributes = vec![];

    if body.is_extern {
        // extern declared functions need private visibility
        fn_attributes.push((
            Identifier::new(ctx.context(), "sym_visibility"),
            StringAttribute::new(ctx.context(), "private").into(),
        ));
    }

    let func_op = func::func(
        ctx.context(),
        StringAttribute::new(ctx.context(), &body.get_mangled_name()),
        TypeAttribute::new(func_type.into()),
        region,
        &fn_attributes,
        Location::unknown(ctx.context()),
    );

    ctx.module.ctx.mlir_module.body().append_operation(func_op);

    Ok(())
}

/// Compiles a rvalue.
fn compile_rvalue<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Rvalue,
    locals: &'b HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TypeIndex), CodegenError> {
    Ok(match info {
        Rvalue::Use(info) => compile_load_operand(ctx, block, info, locals)?,
        Rvalue::LogicOp(_, _) => todo!(),
        Rvalue::BinaryOp(op, (lhs, rhs)) => compile_binop(ctx, block, op, lhs, rhs, locals)?,
        Rvalue::UnaryOp(_, _) => todo!(),
        Rvalue::Ref(_mutability, place) => {
            let mut value = locals[&place.local];
            let mut local_type_idx = ctx.get_fn_body().locals[place.local].ty;
            let mut local_ty = ctx.module.get_type(local_type_idx);

            // Handle the projection.
            for projection in &place.projection {
                match projection {
                    PlaceElem::Deref => {
                        local_type_idx = local_ty.get_inner_type().expect("should have inner type");
                        local_ty = ctx.module.get_type(local_type_idx);

                        value = block
                            .append_operation(llvm::load(
                                ctx.context(),
                                value,
                                compile_type(ctx.module, &local_ty),
                                Location::unknown(ctx.context()),
                                LoadStoreOptions::default(),
                            ))
                            .result(0)?
                            .into();
                    }
                    PlaceElem::Field(_) => todo!(),
                    PlaceElem::Index(_) => todo!(),
                    PlaceElem::ConstantIndex(_) => todo!(),
                }
            }

            (value, local_type_idx)
        }
        Rvalue::Cast(op, target_type_idx, _span) => {
            let location = Location::unknown(ctx.context());
            let target_type_idx = *target_type_idx;
            let target_ty = ctx.module.get_type(target_type_idx);
            let target_mlir_ty = compile_type(ctx.module, &target_ty);
            let (value, current_type_idx) = compile_load_operand(ctx, block, op, locals)?;
            let current_ty = ctx.module.get_type(current_type_idx);
            let is_signed = current_ty.is_signed();

            if target_ty.is_ptr_like() {
                // int to ptr
                if current_ty.is_int() {
                    let value = block
                        .append_operation(
                            ods::llvm::inttoptr(ctx.context(), target_mlir_ty, value, location)
                                .into(),
                        )
                        .result(0)?
                        .into();
                    (value, target_type_idx)
                } else if current_ty.is_ptr_like() {
                    (value, target_type_idx)
                } else if current_ty.is_array() {
                    // Cast from fixed size array to pointer.
                    // We need to create a alloca and store the array there, because we have it by-value.
                    let k1 = block
                        .append_operation(arith::constant(
                            ctx.context(),
                            IntegerAttribute::new(IntegerType::new(ctx.context(), 64).into(), 1)
                                .into(),
                            location,
                        ))
                        .result(0)?
                        .into();
                    let ptr = block
                        .append_operation(
                            ods::llvm::alloca(
                                ctx.context(),
                                pointer(ctx.context(), 0),
                                k1,
                                TypeAttribute::new(compile_type(ctx.module, &current_ty)),
                                location,
                            )
                            .into(),
                        )
                        .result(0)?
                        .into();
                    block.append_operation(
                        ods::llvm::store(ctx.context(), value, ptr, location).into(),
                    );

                    // Return the alloca ptr, making this "the cast".

                    (ptr, target_type_idx)
                } else {
                    unreachable!(
                        "cast from {:?} to ptr",
                        current_ty.display(ctx.module.ctx.program)
                    )
                }
            } else if target_ty.is_int() {
                if current_ty.is_int() {
                    // int to int casts
                    match current_ty
                        .get_bit_width()
                        .unwrap()
                        .cmp(&target_ty.get_bit_width().unwrap())
                    {
                        std::cmp::Ordering::Less => {
                            if is_signed {
                                let value = block
                                    .append_operation(arith::extsi(value, target_mlir_ty, location))
                                    .result(0)?
                                    .into();
                                (value, target_type_idx)
                            } else {
                                let value = block
                                    .append_operation(arith::extui(value, target_mlir_ty, location))
                                    .result(0)?
                                    .into();
                                (value, target_type_idx)
                            }
                        }
                        std::cmp::Ordering::Equal => (value, target_type_idx),
                        std::cmp::Ordering::Greater => {
                            let value = block
                                .append_operation(arith::trunci(value, target_mlir_ty, location))
                                .result(0)?
                                .into();
                            (value, target_type_idx)
                        }
                    }
                } else if current_ty.is_float() {
                    // float to int
                    if is_signed {
                        let value = block
                            .append_operation(arith::fptosi(value, target_mlir_ty, location))
                            .result(0)?
                            .into();
                        (value, target_type_idx)
                    } else {
                        let value = block
                            .append_operation(arith::fptoui(value, target_mlir_ty, location))
                            .result(0)?
                            .into();
                        (value, target_type_idx)
                    }
                } else if current_ty.is_ptr_like() {
                    // ptr to int
                    let value = block
                        .append_operation(
                            ods::llvm::ptrtoint(ctx.context(), target_mlir_ty, value, location)
                                .into(),
                        )
                        .result(0)?
                        .into();
                    (value, target_type_idx)
                } else {
                    todo!(
                        "cast from {:?} to {:?}",
                        current_ty.display(ctx.module.ctx.program),
                        target_ty.display(ctx.module.ctx.program)
                    )
                }
            } else {
                todo!(
                    "cast from {:?} to {:?}",
                    current_ty.display(ctx.module.ctx.program),
                    target_ty.display(ctx.module.ctx.program)
                )
            }
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
) -> Result<(Value<'c, 'b>, TypeIndex), CodegenError> {
    let (lhs, lhs_type_idx) = compile_load_operand(ctx, block, lhs, locals)?;
    let (rhs, _rhs_type_idx) = compile_load_operand(ctx, block, rhs, locals)?;
    let location = Location::unknown(ctx.context());
    let lhs_ty = ctx.module.get_type(lhs_type_idx);

    let is_float = matches!(lhs_ty, TyKind::Float(_));
    let is_signed = matches!(lhs_ty, TyKind::Int(_));
    let is_ptr = if let TyKind::Ptr(inner, _) = &lhs_ty {
        Some(*inner)
    } else {
        None
    };

    Ok(match op {
        BinOp::Add => {
            let value = if let Some(inner_type_idx) = is_ptr {
                let inner = ctx.module.get_type(inner_type_idx);
                let inner_ty = compile_type(ctx.module, &inner);
                block
                    .append_operation(
                        ods::llvm::getelementptr(
                            ctx.context(),
                            pointer(ctx.context(), 0),
                            lhs,
                            &[rhs],
                            DenseI32ArrayAttribute::new(ctx.context(), &[i32::MIN]),
                            TypeAttribute::new(inner_ty),
                            location,
                        )
                        .into(),
                    )
                    .result(0)?
                    .into()
            } else if is_float {
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
            (value, lhs_type_idx)
        }
        BinOp::Sub => {
            if is_ptr.is_some() {
                return Err(CodegenError::NotImplemented(
                    "substracting from a pointer is not yet implemented".to_string(),
                ));
            }
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
            (value, lhs_type_idx)
        }
        BinOp::Mul => {
            if is_ptr.is_some() {
                return Err(CodegenError::NotImplemented(
                    "multiplying a pointer is not yet implemented".to_string(),
                ));
            }
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
            (value, lhs_type_idx)
        }
        BinOp::Div => {
            if is_ptr.is_some() {
                return Err(CodegenError::NotImplemented(
                    "dividing a pointer is not yet implemented".to_string(),
                ));
            }
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
            (value, lhs_type_idx)
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
            (value, lhs_type_idx)
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
            (value, ctx.module.ctx.program.get_bool_ty())
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
            (value, ctx.module.ctx.program.get_bool_ty())
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
            (value, ctx.module.ctx.program.get_bool_ty())
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
            (value, ctx.module.ctx.program.get_bool_ty())
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
            (value, ctx.module.ctx.program.get_bool_ty())
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
            (value, ctx.module.ctx.program.get_bool_ty())
        }
    })
}

fn compile_load_operand<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Operand,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TypeIndex), CodegenError> {
    Ok(match info {
        Operand::Place(info) => compile_load_place(ctx, block, info, locals)?,
        Operand::Const(data) => match &data.data {
            crate::ir::ConstKind::Value(value) => (compile_value_tree(ctx, block, value)?, data.ty),
            crate::ir::ConstKind::Expr(_) => todo!(),
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
    let local = &ctx.get_fn_body().locals[info.local];
    let mut local_type_idx = local.ty;
    let mut local_ty = ctx.module.get_type(local_type_idx);

    for proj in &info.projection {
        match proj {
            PlaceElem::Deref => {
                ptr = block
                    .append_operation(llvm::load(
                        ctx.context(),
                        ptr,
                        compile_type(ctx.module, &local_ty),
                        Location::unknown(ctx.context()),
                        LoadStoreOptions::default(),
                    ))
                    .result(0)?
                    .into();

                local_type_idx = local_ty.get_inner_type().expect("should have inner");
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::Field(field_idx) => {
                ptr = block
                    .append_operation(llvm::get_element_ptr(
                        ctx.context(),
                        ptr,
                        DenseI32ArrayAttribute::new(
                            ctx.context(),
                            &[0, (*field_idx).try_into().unwrap()],
                        ),
                        compile_type(ctx.module, &local_ty),
                        pointer(ctx.context(), 0),
                        Location::unknown(ctx.context()),
                    ))
                    .result(0)?
                    .into();
                local_type_idx = match local_ty {
                    TyKind::Struct(id) => {
                        let strc = ctx.module.ctx.program.structs[id].as_ref().unwrap();
                        strc.variants[*field_idx].ty
                    }
                    _ => unreachable!(),
                };
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::Index(local) => {
                let place = Place {
                    local: *local,
                    projection: vec![],
                };

                let (index, _) = compile_load_place(ctx, block, &place, locals)?;

                ptr = block
                    .append_operation(
                        {
                            let mut op = ods::llvm::getelementptr(
                                ctx.context(),
                                pointer(ctx.context(), 0),
                                ptr,
                                &[index],
                                DenseI32ArrayAttribute::new(ctx.context(), &[0, i32::MIN]),
                                TypeAttribute::new(compile_type(ctx.module, &local_ty)),
                                Location::unknown(ctx.context()),
                            );
                            op.set_inbounds(Attribute::unit(ctx.context()));
                            op
                        }
                        .into(),
                    )
                    .result(0)?
                    .into();

                local_type_idx = local_ty.get_inner_type().expect("should have inner");
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::ConstantIndex(index) => {
                ptr = block
                    .append_operation(
                        {
                            let mut op = ods::llvm::getelementptr(
                                ctx.context(),
                                pointer(ctx.context(), 0),
                                ptr,
                                &[],
                                DenseI32ArrayAttribute::new(
                                    ctx.context(),
                                    &[0, (*index).try_into().unwrap()],
                                ),
                                TypeAttribute::new(compile_type(ctx.module, &local_ty)),
                                Location::unknown(ctx.context()),
                            );
                            op.set_inbounds(Attribute::unit(ctx.context()));
                            op
                        }
                        .into(),
                    )
                    .result(0)?
                    .into();

                local_type_idx = local_ty.get_inner_type().expect("should have inner");
                local_ty = ctx.module.get_type(local_type_idx);
            }
        }
    }

    block.append_operation(llvm::store(
        ctx.context(),
        value,
        ptr,
        Location::unknown(ctx.context()),
        LoadStoreOptions::default(),
    ));

    Ok(())
}

/// Compiles a load to a place.
fn compile_load_place<'c: 'b, 'b>(
    ctx: &'c FunctionCodegenCtx,
    block: &'b Block<'c>,
    info: &Place,
    locals: &HashMap<usize, Value<'c, '_>>,
) -> Result<(Value<'c, 'b>, TypeIndex), CodegenError> {
    let mut ptr = locals[&info.local];
    let body = ctx.get_fn_body();

    let mut local_type_idx = body.locals[info.local].ty;
    let mut local_ty = ctx.module.get_type(local_type_idx);

    for projection in &info.projection {
        match projection {
            PlaceElem::Deref => {
                ptr = block
                    .append_operation(llvm::load(
                        ctx.context(),
                        ptr,
                        compile_type(ctx.module, &local_ty),
                        Location::unknown(ctx.context()),
                        LoadStoreOptions::default(),
                    ))
                    .result(0)?
                    .into();

                local_type_idx = local_ty
                    .get_inner_type()
                    .expect("should always have inner type");
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::Field(field_idx) => {
                local_type_idx = match local_ty {
                    TyKind::Struct(id) => {
                        let struct_body = ctx.module.ctx.program.structs[id].as_ref().unwrap();
                        let variant_type_idx = struct_body.variants[*field_idx].ty;
                        ptr = block
                            .append_operation(llvm::get_element_ptr(
                                ctx.context(),
                                ptr,
                                DenseI32ArrayAttribute::new(
                                    ctx.context(),
                                    &[0, (*field_idx).try_into().unwrap()],
                                ),
                                compile_type(ctx.module, &local_ty),
                                pointer(ctx.context(), 0),
                                Location::unknown(ctx.context()),
                            ))
                            .result(0)?
                            .into();
                        variant_type_idx
                    }
                    _ => unreachable!(),
                };
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::Index(local) => {
                let place = Place {
                    local: *local,
                    projection: Default::default(),
                };

                let (index, _index_type_idx) = compile_load_place(ctx, block, &place, locals)?;

                ptr = block
                    .append_operation(
                        {
                            let mut op = ods::llvm::getelementptr(
                                ctx.context(),
                                pointer(ctx.context(), 0),
                                ptr,
                                &[index],
                                DenseI32ArrayAttribute::new(ctx.context(), &[0, i32::MIN]),
                                TypeAttribute::new(compile_type(ctx.module, &local_ty)),
                                Location::unknown(ctx.context()),
                            );
                            op.set_inbounds(Attribute::unit(ctx.context()));
                            op
                        }
                        .into(),
                    )
                    .result(0)?
                    .into();

                local_type_idx = local_ty.get_inner_type().expect("should have inner");
                local_ty = ctx.module.get_type(local_type_idx);
            }
            PlaceElem::ConstantIndex(index) => {
                ptr = block
                    .append_operation(
                        {
                            let mut op = ods::llvm::getelementptr(
                                ctx.context(),
                                pointer(ctx.context(), 0),
                                ptr,
                                &[],
                                DenseI32ArrayAttribute::new(
                                    ctx.context(),
                                    &[0, (*index).try_into().unwrap()],
                                ),
                                TypeAttribute::new(compile_type(ctx.module, &local_ty)),
                                Location::unknown(ctx.context()),
                            );
                            op.set_inbounds(Attribute::unit(ctx.context()));
                            op
                        }
                        .into(),
                    )
                    .result(0)?
                    .into();

                local_type_idx = local_ty.get_inner_type().expect("should have inner");
                local_ty = ctx.module.get_type(local_type_idx);
            }
        }
    }

    let value = block
        .append_operation(llvm::load(
            ctx.context(),
            ptr,
            compile_type(ctx.module, &local_ty),
            Location::unknown(ctx.context()),
            LoadStoreOptions::default(),
        ))
        .result(0)?
        .into();

    Ok((value, local_type_idx))
}

/// Used in switch
/// Converts a constant value to a int.
fn value_tree_to_int(value: &ValueTree) -> Option<i64> {
    match value {
        ValueTree::Leaf(value) => match value {
            crate::ir::ConstValue::Bool(value) => Some((*value) as i64),
            crate::ir::ConstValue::I8(value) => Some((*value) as i64),
            crate::ir::ConstValue::I16(value) => Some((*value) as i64),
            crate::ir::ConstValue::I32(value) => Some((*value) as i64),
            crate::ir::ConstValue::I64(value) => Some(*value),
            crate::ir::ConstValue::I128(value) => Some((*value) as i64),
            crate::ir::ConstValue::U8(value) => Some((*value) as i64),
            crate::ir::ConstValue::Char(value) => Some((*value) as i64),
            crate::ir::ConstValue::U16(value) => Some((*value) as i64),
            crate::ir::ConstValue::U32(value) => Some((*value) as i64),
            crate::ir::ConstValue::U64(value) => Some((*value) as i64),
            crate::ir::ConstValue::U128(value) => Some((*value) as i64),
            crate::ir::ConstValue::F32(_) => None,
            crate::ir::ConstValue::F64(_) => None,
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
            crate::ir::ConstValue::Bool(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i1", (*value) as u8)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::I8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::I16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::I32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::I64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::I128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::Char(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::U8(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i8", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::U16(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i16", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::U32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i32", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::U64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i64", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::U128(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    Attribute::parse(ctx.context(), &format!("{} : i128", value)).unwrap(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::F32(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    FloatAttribute::new(
                        ctx.context(),
                        Type::float32(ctx.context()),
                        value.parse().unwrap(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
            crate::ir::ConstValue::F64(value) => block
                .append_operation(arith::constant(
                    ctx.context(),
                    FloatAttribute::new(
                        ctx.context(),
                        Type::float64(ctx.context()),
                        value.parse().unwrap(),
                    )
                    .into(),
                    Location::unknown(ctx.context()),
                ))
                .result(0)?
                .into(),
        },
        ValueTree::Branch(_) => todo!(),
    })
}

fn compile_type<'c>(ctx: ModuleCodegenCtx<'c>, ty: &TyKind) -> Type<'c> {
    match ty {
        crate::ir::TyKind::Unit => Type::none(ctx.ctx.mlir_context),
        crate::ir::TyKind::Bool => IntegerType::new(ctx.ctx.mlir_context, 1).into(),
        crate::ir::TyKind::Char => IntegerType::new(ctx.ctx.mlir_context, 8).into(),
        crate::ir::TyKind::Int(int_ty) => match int_ty {
            crate::ir::IntTy::I8 => IntegerType::new(ctx.ctx.mlir_context, 8).into(),
            crate::ir::IntTy::I16 => IntegerType::new(ctx.ctx.mlir_context, 16).into(),
            crate::ir::IntTy::I32 => IntegerType::new(ctx.ctx.mlir_context, 32).into(),
            crate::ir::IntTy::I64 => IntegerType::new(ctx.ctx.mlir_context, 64).into(),
            crate::ir::IntTy::I128 => IntegerType::new(ctx.ctx.mlir_context, 128).into(),
        },
        crate::ir::TyKind::Uint(uint_ty) => match uint_ty {
            crate::ir::UintTy::U8 => IntegerType::new(ctx.ctx.mlir_context, 8).into(),
            crate::ir::UintTy::U16 => IntegerType::new(ctx.ctx.mlir_context, 16).into(),
            crate::ir::UintTy::U32 => IntegerType::new(ctx.ctx.mlir_context, 32).into(),
            crate::ir::UintTy::U64 => IntegerType::new(ctx.ctx.mlir_context, 64).into(),
            crate::ir::UintTy::U128 => IntegerType::new(ctx.ctx.mlir_context, 128).into(),
        },
        crate::ir::TyKind::Float(float_ty) => match float_ty {
            crate::ir::FloatTy::F32 => Type::float32(ctx.ctx.mlir_context),
            crate::ir::FloatTy::F64 => Type::float64(ctx.ctx.mlir_context),
        },
        crate::ir::TyKind::String => todo!(),
        crate::ir::TyKind::Array(inner_type_idx, length) => {
            let inner_type = ctx.get_type(*inner_type_idx);
            let inner_type = compile_type(ctx, &inner_type);
            let length = match length.data {
                crate::ir::ConstKind::Value(ValueTree::Leaf(ConstValue::U64(length))) => length,
                _ => unimplemented!(),
            };

            melior::dialect::llvm::r#type::array(inner_type, length as u32)
        }
        crate::ir::TyKind::Ref(_inner_ty, _) | crate::ir::TyKind::Ptr(_inner_ty, _) => {
            llvm::r#type::pointer(ctx.ctx.mlir_context, 0)
        }
        crate::ir::TyKind::Struct(id) => {
            let body = ctx.ctx.program.structs[*id].as_ref().unwrap();

            let mut fields = Vec::new();

            for field in &body.variants {
                let field_ty = ctx.get_type(field.ty);
                let ty = compile_type(ctx, &field_ty);
                fields.push(ty);
            }

            let ty = melior::dialect::llvm::r#type::r#struct(ctx.ctx.mlir_context, &fields, false);

            ty
        }
    }
}
