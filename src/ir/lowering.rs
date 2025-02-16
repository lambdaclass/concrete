use std::collections::HashMap;

use crate::ast::{
    common::{Span, TypeName},
    constants::ConstantDef,
    expressions::{
        ArithOp, BinaryOp, BitwiseOp, CmpOp, Expression, FnCallOp, IfExpr, LogicOp, PathOp,
        PathSegment, ValueExpr,
    },
    functions::{FunctionDecl, FunctionDef},
    modules::{Module, ModuleDefItem},
    statements::{self, AssignStmt, ForStmt, LetStmt, LetStmtTarget, ReturnStmt, WhileStmt},
    structs::StructDecl,
    types::TypeDescriptor,
    Program,
};
use common::{BuildCtx, FnBodyBuilder, GenericFn, GenericStruct, IdGenerator};
use tracing::{debug, instrument};

use crate::ir::{
    AdtBody, BasicBlock, BinOp, ConcreteIntrinsic, ConstBody, ConstData, ConstKind, ConstValue,
    DefId, FloatTy, FnBody, IntTy, Local, LocalKind, LogOp, Mutability, Operand, Place, PlaceElem,
    ProgramBody, Rvalue, Statement, StatementKind, SwitchTargets, Terminator, TerminatorKind, Ty,
    TyKind, UintTy, ValueTree, VariantDef,
};

use self::errors::LoweringError;

pub mod common;
pub mod errors;
pub mod prepass;

pub fn lower_programs(program: &[Program]) -> Result<ProgramBody, LoweringError> {
    let mut ctx = BuildCtx {
        body: ProgramBody::default(),
        gen: IdGenerator::default(),
        unresolved_function_signatures: Default::default(),
        generic_fn_bodies: Default::default(),
        generic_functions: Default::default(),
        generic_structs: Default::default(),
        generic_struct_bodies: Default::default(),
    };

    for (program_id, program) in program.iter().enumerate() {
        ctx.gen.program_id = program_id;
        ctx.gen.current_id = 0;
        let file_path = program.file_path.as_ref().ok_or_else(|| {
            LoweringError::InternalError("Missing program file path".to_string(), program_id)
        })?;
        ctx.body.file_paths.insert(program_id, file_path.clone());

        // resolve symbols
        for module in &program.modules {
            ctx = prepass::prepass_module(ctx, module)?;
        }

        // resolve imports
        for module in &program.modules {
            ctx = prepass::prepass_imports(ctx, module)?;
        }

        for mod_def in &program.modules {
            let id = *ctx
                .body
                .top_level_module_names
                .get(&mod_def.name.name)
                .ok_or_else(|| LoweringError::ModuleNotFound {
                    span: mod_def.span,
                    module: mod_def.name.name.clone(),
                    program_id,
                })?;

            ctx = lower_module(ctx, mod_def, id)?;
        }
    }

    Ok(ctx.body)
}

#[instrument(level = "debug", skip_all, fields(module = ?module.name.name))]
fn lower_module(mut ctx: BuildCtx, module: &Module, id: DefId) -> Result<BuildCtx, LoweringError> {
    debug!("lowering module");
    // lower first structs, constants, types
    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(info) => {
                ctx = lower_constant(ctx, info, id)?;
            }
            ModuleDefItem::Struct(info) => {
                if info.generics.is_empty() {
                    let def_id = {
                        let body = ctx.body.modules.get(&id).unwrap();
                        *body.symbols.structs.get(&info.name.name).unwrap()
                    };
                    lower_struct(&mut ctx, info, def_id, id, None)?;
                }
            }
            ModuleDefItem::Type(_) => todo!(),
            _ => {}
        }
    }

    // todo: needed for borrowck, maybe use a rc refcell to avoid expensive clones
    let body = ctx.body.modules.get(&id).unwrap().clone();

    // fill fn sigs
    for content in module.contents.iter() {
        match content {
            ModuleDefItem::Function(fn_def) => {
                let fn_id = *body
                    .symbols
                    .functions
                    .get(&fn_def.decl.name.name)
                    .ok_or_else(|| LoweringError::FunctionNotFound {
                        span: fn_def.span,
                        function: fn_def.decl.name.name.clone(),
                        program_id: body.id.program_id,
                    })?;

                if !fn_def.decl.generic_params.is_empty() {
                    continue;
                }

                let mut args = Vec::new();
                let ret_type;

                for arg in &fn_def.decl.params {
                    let ty = lower_type(&mut ctx, &arg.r#type, id, None)?;
                    args.push(ty);
                }

                if let Some(ty) = &fn_def.decl.ret_type {
                    ret_type = lower_type(&mut ctx, ty, id, None)?;
                } else {
                    ret_type = Ty {
                        span: None,
                        kind: TyKind::Unit,
                    };
                }

                ctx.body.function_signatures.insert(fn_id, (args, ret_type));
                ctx.unresolved_function_signatures.remove(&fn_id);
            }
            ModuleDefItem::FunctionDecl(fn_decl) => {
                let fn_id = *body
                    .symbols
                    .functions
                    .get(&fn_decl.name.name)
                    .ok_or_else(|| LoweringError::FunctionNotFound {
                        span: fn_decl.span,
                        function: fn_decl.name.name.clone(),
                        program_id: body.id.program_id,
                    })?;

                let mut args = Vec::new();
                let ret_type;

                for arg in &fn_decl.params {
                    let ty = lower_type(&mut ctx, &arg.r#type, id, None)?;
                    args.push(ty);
                }

                if let Some(ty) = &fn_decl.ret_type {
                    ret_type = lower_type(&mut ctx, ty, id, None)?;
                } else {
                    ret_type = Ty {
                        span: None,
                        kind: TyKind::Unit,
                    };
                }

                ctx.body.function_signatures.insert(fn_id, (args, ret_type));
                ctx.unresolved_function_signatures.remove(&fn_id);
            }
            ModuleDefItem::Impl(impl_block) => {
                for fn_def in &impl_block.methods {
                    let fn_id = *body
                        .symbols
                        .methods
                        .get(&(impl_block.target.clone(), fn_def.decl.name.name.clone()))
                        .ok_or_else(|| LoweringError::FunctionNotFound {
                            span: fn_def.span,
                            function: fn_def.decl.name.name.clone(),
                            program_id: body.id.program_id,
                        })?;

                    let mut args = Vec::new();
                    let ret_type;

                    let target_tykind = lower_type(&mut ctx, &impl_block.target, id, None)?.kind;

                    let mut first_is_self = false;
                    if let Some(self_ty) = fn_def.decl.params.first() {
                        if let TypeDescriptor::SelfType { is_ref, is_mut } = self_ty.r#type {
                            let mut target_ty = lower_type(&mut ctx, &impl_block.target, id, None)?;
                            if is_ref {
                                target_ty = Ty {
                                    span: target_ty.span,
                                    kind: TyKind::Ref(
                                        Box::new(target_ty),
                                        if is_mut {
                                            Mutability::Mut
                                        } else {
                                            Mutability::Not
                                        },
                                    ),
                                };
                            }
                            args.push(target_ty);
                            first_is_self = true;
                        }
                    }

                    let mut arg_iter = fn_def.decl.params.iter();
                    if first_is_self {
                        arg_iter.next();
                    }
                    for arg in arg_iter {
                        let ty = lower_type(&mut ctx, &arg.r#type, id, None)?;
                        args.push(ty);
                    }

                    if let Some(ty) = &fn_def.decl.ret_type {
                        ret_type = lower_type(&mut ctx, ty, id, None)?;
                    } else {
                        ret_type = Ty {
                            span: None,
                            kind: TyKind::Unit,
                        };
                    }

                    ctx.body.function_signatures.insert(fn_id, (args, ret_type));
                    ctx.body
                        .methods
                        .entry(target_tykind.clone())
                        .or_default()
                        .insert(fn_def.decl.name.name.clone(), fn_id);
                    ctx.unresolved_function_signatures.remove(&fn_id);
                }
            }
            _ => {}
        }
    }

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => { /* already processed */ }
            ModuleDefItem::Function(fn_def) => {
                if fn_def.decl.generic_params.is_empty() {
                    ctx = lower_func(ctx, fn_def, id, None, None)?.0;
                }
            }
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Module(mod_def) => {
                let body = ctx.body.modules.get(&id).unwrap();
                let id = *body.symbols.modules.get(&mod_def.name.name).unwrap();
                ctx = lower_module(ctx, mod_def, id)?;
            }
            ModuleDefItem::Struct(_) => { /* already processed */ }
            ModuleDefItem::Union(_) => todo!(),
            ModuleDefItem::Enum(_) => todo!(),
            ModuleDefItem::FunctionDecl(fn_decl) => {
                ctx = lower_func_decl(ctx, fn_decl, id)?;
            }
            ModuleDefItem::Impl(impl_block) => {
                for info in &impl_block.methods {
                    ctx = lower_func(ctx, info, id, Some(&impl_block.target), None)?.0;
                }
            }
        }
    }

    Ok(ctx)
}

#[instrument(level = "debug", skip_all)]
fn lower_constant(
    mut ctx: BuildCtx,
    info: &ConstantDef,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    debug!("lowering constant");
    let name = info.decl.name.name.clone();

    let id = {
        let module = ctx
            .body
            .modules
            .get(&module_id)
            .expect("module should exist");
        *module
            .symbols
            .constants
            .get(&name)
            .expect("constant should exist")
    };

    let value_ty = lower_type(&mut ctx, &info.decl.r#type, module_id, None)?;

    let value = lower_constant_expression(&info.value, value_ty)?;

    let body = ConstBody { id, name, value };

    ctx.body.constants.insert(body.id, body);

    Ok(ctx)
}

#[instrument(level = "debug", skip_all)]
fn lower_constant_expression(expression: &Expression, ty: Ty) -> Result<ConstData, LoweringError> {
    debug!("lowering const expression");
    let data = match expression {
        Expression::Value(value, _) => match value {
            ValueExpr::ConstBool(value, _) => {
                ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(*value)))
            }
            ValueExpr::ConstChar(value, _) => {
                ConstKind::Value(ValueTree::Leaf(ConstValue::U32((*value) as u32)))
            }
            ValueExpr::ConstInt(value, _) => ConstKind::Value(ValueTree::Leaf(match ty.kind {
                TyKind::Int(ty) => match ty {
                    IntTy::I8 => ConstValue::I8((*value).try_into().expect("value out of range")),
                    IntTy::I16 => ConstValue::I16((*value).try_into().expect("value out of range")),
                    IntTy::I32 => ConstValue::I32((*value).try_into().expect("value out of range")),
                    IntTy::I64 => ConstValue::I64((*value).try_into().expect("value out of range")),
                    IntTy::I128 => {
                        ConstValue::I128((*value).try_into().expect("value out of range"))
                    }
                },
                TyKind::Uint(ty) => match ty {
                    UintTy::U8 => ConstValue::U8((*value).try_into().expect("value out of range")),
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
                x => unreachable!("{:?}", x),
            })),
            ValueExpr::ConstFloat(value, _) => ConstKind::Value(ValueTree::Leaf(match &ty.kind {
                TyKind::Float(ty) => match ty {
                    FloatTy::F32 => ConstValue::F32(value.parse().expect("error parsing float")),
                    FloatTy::F64 => ConstValue::F64(value.parse().expect("error parsing float")),
                },
                x => unreachable!("{:?}", x),
            })),
            ValueExpr::ConstStr(_, _) => todo!(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    Ok(ConstData { ty, data })
}

#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
fn lower_struct(
    ctx: &mut BuildCtx,
    info: &StructDecl,
    id: DefId,
    module_id: DefId,
    generics: Option<&HashMap<String, TypeName>>,
) -> Result<(), LoweringError> {
    debug!("lowering struct");
    let mut body = AdtBody {
        def_id: id,
        is_pub: true, // todo struct pub
        name: info.name.name.clone(),
        variants: Vec::new(),
        name_to_idx: Default::default(),
        span: info.span,
    };

    for (i, field) in info.fields.iter().enumerate() {
        let variant = VariantDef {
            def_id: ctx.gen.next_defid(),
            name: field.name.name.clone(),
            ty: lower_type(ctx, &field.r#type, module_id, generics)?,
            discriminant: i,
        };
        body.variants.push(variant);
        body.name_to_idx
            .insert(field.name.name.clone(), body.variants.len() - 1);
    }

    ctx.body.structs.insert(body.def_id, body);
    Ok(())
}

#[instrument(level = "debug", skip_all, fields(name = func.decl.name.name))]
fn lower_func(
    mut ctx: BuildCtx,
    func: &FunctionDef,
    module_id: DefId,
    has_self: Option<&TypeDescriptor>,
    generics: Option<&HashMap<String, TypeName>>,
) -> Result<(BuildCtx, DefId), LoweringError> {
    debug!("lowering function");
    let is_intrinsic: Option<ConcreteIntrinsic> = None;

    // TODO: parse insintrics here.

    let mut fn_id = {
        let body = ctx.body.modules.get(&module_id).unwrap();
        if let Some(self_ty) = has_self {
            *body
                .symbols
                .methods
                .get(&(self_ty.clone(), func.decl.name.name.clone()))
                .unwrap()
        } else {
            *body.symbols.functions.get(&func.decl.name.name).unwrap()
        }
    };
    debug!("function id: {:?}", fn_id);

    // Check if its a generic function
    // If it is, lower it and save its id, to avoid lowering the same monomorphized function again.
    if let Some(generics) = generics {
        debug!("function is generic over {} parameters", generics.len());
        let gfn = GenericFn {
            id: fn_id,
            generics: generics.iter().map(|x| x.0).cloned().collect(),
        };

        if let Some(generic_defid) = ctx.generic_functions.get(&gfn).cloned() {
            debug!("generic function already monomorphized");
            return Ok((ctx, generic_defid));
        } else {
            debug!("monomorphizing generic function");
            let next_id = ctx.gen.next_defid();

            ctx.generic_functions.insert(gfn, next_id);

            let args_ty: Vec<_> = func
                .decl
                .params
                .iter()
                .map(|param| lower_type(&mut ctx, &param.r#type, module_id, Some(generics)))
                .collect::<Result<_, _>>()?;
            let ret_ty = func
                .decl
                .ret_type
                .as_ref()
                .map(|x| lower_type(&mut ctx, x, module_id, Some(generics)))
                .unwrap_or_else(|| {
                    Ok(Ty {
                        span: None,
                        kind: TyKind::Unit,
                    })
                })?;

            fn_id = next_id;
            ctx.body
                .function_signatures
                .insert(fn_id, (args_ty, ret_ty));
            debug!("created with id={fn_id:?}");
            ctx.body
                .modules
                .get_mut(&module_id)
                .expect("module should exist")
                .functions
                .insert(fn_id);
        }

        debug!("new function id: {:?}", fn_id);
    }

    let mut builder = FnBodyBuilder {
        body: FnBody {
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            is_extern: func.decl.is_extern,
            is_intrinsic,
            name: if !func.decl.is_extern && func.decl.name.name != "main" {
                ctx.get_mangled_name(module_id, &func.decl.name.name, fn_id)
                    .expect("failed to get mangled name")
            } else {
                func.decl.name.name.clone()
            },
            id: fn_id,
        },
        local_module: module_id,
        local_exists: Default::default(),
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        generic_map: generics.cloned(),
        ctx,
    };

    // A extern fn cannot have a body.
    if !func.body.is_empty() && func.decl.is_extern {
        return Err(LoweringError::ExternFnWithBody {
            span: func.span,
            name: func.decl.name.name.clone(),
            program_id: module_id.program_id,
        });
    }

    let (args_ty, ret_ty) = builder
        .ctx
        .body
        .function_signatures
        .get(&fn_id)
        .unwrap()
        .clone();

    // Add the return local.
    builder.ret_local = builder.body.locals.len();
    builder.body.locals.push(Local::new(
        None,
        LocalKind::ReturnPointer,
        ret_ty,
        None,
        false,
    ));

    // Add argument locals.
    for (arg, ty) in func.decl.params.iter().zip(args_ty) {
        builder
            .name_to_local
            .insert(arg.name.name.clone(), builder.body.locals.len());
        builder.local_exists.insert(builder.body.locals.len());
        builder.body.locals.push(Local::new(
            Some(arg.name.span),
            LocalKind::Arg,
            ty,
            Some(arg.name.name.clone()),
            false,
        ));
    }

    // Get all top level locals
    for stmt in &func.body {
        get_locals(&mut builder, stmt)?;
    }

    let ret_type = func
        .decl
        .ret_type
        .as_ref()
        .map(|r| {
            lower_type(
                &mut builder.ctx,
                r,
                builder.local_module,
                builder.generic_map.as_ref(),
            )
        })
        .transpose()?
        .unwrap_or(Ty {
            span: None,
            kind: TyKind::Unit,
        });

    for stmt in &func.body {
        lower_statement(&mut builder, stmt, ret_type.clone())?;
    }

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Return,
        }),
    });

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.id);
    debug!("added function {} with id {:?} to ir", body.name, body.id);
    ctx.body.functions.insert(body.id, body);

    Ok((ctx, fn_id))
}

/// Get and map names to locals.
///
/// Should be called on each new scope.
fn get_locals(
    builder: &mut FnBodyBuilder,
    stmt: &crate::ast::statements::Statement,
) -> Result<(), LoweringError> {
    match stmt {
        statements::Statement::Assign(_assign_stmt) => {}
        statements::Statement::Match(_match_expr) => todo!(),
        statements::Statement::For(info) => {
            if let Some(info) = &info.init {
                match &info.target {
                    LetStmtTarget::Simple { id: name, r#type } => {
                        let ty = lower_type(
                            &mut builder.ctx,
                            r#type,
                            builder.local_module,
                            builder.generic_map.as_ref(),
                        )?;
                        builder
                            .name_to_local
                            .insert(name.name.clone(), builder.body.locals.len());
                        builder.body.locals.push(Local::new(
                            Some(name.span),
                            LocalKind::Temp,
                            ty,
                            Some(name.name.clone()),
                            info.is_mutable,
                        ));
                    }
                    LetStmtTarget::Destructure(_) => todo!(),
                }
            }
        }
        // handled in the lower function
        statements::Statement::If(_info) => {}
        statements::Statement::Let(info) => match &info.target {
            LetStmtTarget::Simple { id: name, r#type } => {
                let ty = lower_type(
                    &mut builder.ctx,
                    r#type,
                    builder.local_module,
                    builder.generic_map.as_ref(),
                )?;
                builder
                    .name_to_local
                    .insert(name.name.clone(), builder.body.locals.len());
                builder.body.locals.push(Local::new(
                    Some(name.span),
                    LocalKind::Temp,
                    ty,
                    Some(name.name.clone()),
                    info.is_mutable,
                ));
            }
            LetStmtTarget::Destructure(_) => todo!(),
        },
        statements::Statement::Return(_return_stmt) => {}
        statements::Statement::While(_while_stmt) => {}
        statements::Statement::FnCall(_fn_call_op) => {}
        statements::Statement::PathOp(_path_op) => {}
    }

    Ok(())
}

#[instrument(level = "debug", skip_all, fields(name = ?func.name.name))]
fn lower_func_decl(
    ctx: BuildCtx,
    func: &FunctionDecl,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    debug!("lowering function declaration");
    let is_intrinsic: Option<ConcreteIntrinsic> = None;

    // TODO: parse insintrics here.

    let builder = FnBodyBuilder {
        body: FnBody {
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            is_extern: func.is_extern,
            is_intrinsic,
            name: func.name.name.clone(),
            id: {
                let body = ctx.body.modules.get(&module_id).unwrap();
                *body.symbols.functions.get(&func.name.name).unwrap()
            },
        },
        local_module: module_id,
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        generic_map: Default::default(),
        local_exists: Default::default(),
        ctx,
    };

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.id);
    ctx.body.functions.insert(body.id, body);

    Ok(ctx)
}

fn lower_statement(
    builder: &mut FnBodyBuilder,
    info: &crate::ast::statements::Statement,
    ret_type: Ty,
) -> Result<(), LoweringError> {
    match info {
        statements::Statement::Assign(info) => lower_assign(builder, info)?,
        statements::Statement::Match(_) => todo!(),
        statements::Statement::For(info) => {
            lower_for(builder, info)?;
            assert!(builder.statements.is_empty());
        }
        statements::Statement::If(info) => {
            lower_if_statement(builder, info)?;
            assert!(builder.statements.is_empty());
        }
        statements::Statement::Let(info) => lower_let(builder, info)?,
        statements::Statement::Return(info) => {
            lower_return(builder, info, ret_type)?;
        }
        statements::Statement::While(info) => {
            lower_while(builder, info)?;
            assert!(builder.statements.is_empty());
        }
        statements::Statement::FnCall(info) => {
            lower_fn_call(builder, info, None, None)?;
        }
        statements::Statement::PathOp(info) => {
            lower_path(builder, info)?;
        }
    }
    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_while(builder: &mut FnBodyBuilder, info: &WhileStmt) -> Result<(), LoweringError> {
    debug!("lowering while");
    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Goto {
                target: builder.body.basic_blocks.len() + 1,
            },
        }),
    });

    let (discriminator, discriminator_type, _disc_span) =
        lower_expression(builder, &info.condition, None)?;

    let local = builder.add_temp_local(TyKind::Bool);
    let place = Place {
        local,
        projection: vec![],
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(place.clone(), discriminator),
    });

    // keep idx to change terminator
    let check_block_idx = builder.body.basic_blocks.len();

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

    for stmt in &info.block_stmts {
        lower_statement(
            builder,
            stmt,
            builder.body.locals[builder.ret_local].ty.clone(),
        )?;
    }

    builder.body.basic_blocks.len();
    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Goto {
                target: check_block_idx,
            },
        }),
    });

    let otherwise_block_idx = builder.body.basic_blocks.len();

    let targets = SwitchTargets {
        values: vec![discriminator_type.kind.get_falsy_value()],
        targets: vec![otherwise_block_idx, first_then_block_idx],
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[check_block_idx].terminator.kind = kind;

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_for(builder: &mut FnBodyBuilder, info: &ForStmt) -> Result<(), LoweringError> {
    debug!("lowering for");
    if let Some(init) = &info.init {
        lower_let(builder, init)?;
    }

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Goto {
                target: builder.body.basic_blocks.len() + 1,
            },
        }),
    });

    let (discriminator, discriminator_type, _disc_span) = if let Some(condition) = &info.condition {
        let (discriminator, discriminator_type, span) = lower_expression(builder, condition, None)?;

        (discriminator, discriminator_type, Some(span))
    } else {
        // todo: don't use discriminator when no loop condition
        let discriminator_type = Ty {
            span: None,
            kind: TyKind::Bool,
        };

        let discriminator = Rvalue::Use(Operand::Const(ConstData {
            ty: discriminator_type.clone(),
            data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(true))),
        }));

        (discriminator, discriminator_type, None)
    };

    let local = builder.add_temp_local(TyKind::Bool);
    let place = Place {
        local,
        projection: vec![],
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(place.clone(), discriminator),
    });

    // keep idx to change terminator
    let check_block_idx = builder.body.basic_blocks.len();

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

    for stmt in &info.block_stmts {
        lower_statement(
            builder,
            stmt,
            builder.body.locals[builder.ret_local].ty.clone(),
        )?;
    }

    if let Some(post) = &info.post {
        lower_assign(builder, post)?;
    }

    builder.body.basic_blocks.len();
    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Goto {
                target: check_block_idx,
            },
        }),
    });

    let otherwise_block_idx = builder.body.basic_blocks.len();

    let targets = SwitchTargets {
        values: vec![discriminator_type.kind.get_falsy_value()],
        targets: vec![otherwise_block_idx, first_then_block_idx],
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[check_block_idx].terminator.kind = kind;

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_if_statement(builder: &mut FnBodyBuilder, info: &IfExpr) -> Result<(), LoweringError> {
    debug!("begin lowering if");
    let (discriminator, discriminator_type, _disc_span) =
        lower_expression(builder, &info.cond, None)?;

    let local = builder.add_temp_local(TyKind::Bool);
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

    let old_name_to_local = builder.name_to_local.clone();

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

    for stmt in &info.block_stmts {
        get_locals(builder, stmt)?;
        lower_statement(
            builder,
            stmt,
            builder.body.locals[builder.ret_local].ty.clone(),
        )?;
    }

    // keet idx to change terminator
    let last_then_block_idx = {
        builder.body.basic_blocks.len();
        let statements = std::mem::take(&mut builder.statements);
        builder.body.basic_blocks.push(BasicBlock {
            statements,
            terminator: Box::new(Terminator {
                span: None,
                kind: TerminatorKind::Unreachable,
            }),
        });
        builder.body.basic_blocks.len() - 1
    };

    let first_else_block_idx = builder.body.basic_blocks.len();

    builder.name_to_local = old_name_to_local.clone();

    if let Some(contents) = &info.else_stmts {
        for stmt in contents {
            get_locals(builder, stmt)?;
            lower_statement(
                builder,
                stmt,
                builder.body.locals[builder.ret_local].ty.clone(),
            )?;
        }

        let statements = std::mem::take(&mut builder.statements);
        builder.body.basic_blocks.push(BasicBlock {
            statements,
            terminator: Box::new(Terminator {
                span: None,
                kind: TerminatorKind::Goto {
                    target: builder.body.basic_blocks.len() + 1,
                },
            }),
        });
    }

    builder.name_to_local = old_name_to_local;

    let targets = SwitchTargets {
        values: vec![discriminator_type.kind.get_falsy_value()],
        targets: vec![first_else_block_idx, first_then_block_idx],
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

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_let(builder: &mut FnBodyBuilder, info: &LetStmt) -> Result<(), LoweringError> {
    debug!("begin lowering let");
    match &info.target {
        LetStmtTarget::Simple { id: name, r#type } => {
            debug!("let target is simple");
            let ty = lower_type(
                &mut builder.ctx,
                r#type,
                builder.local_module,
                builder.generic_map.as_ref(),
            )?;
            debug!("let target type: {:?}", ty.kind);
            let (rvalue, rvalue_ty, rvalue_span) =
                lower_expression(builder, &info.value, Some(ty.clone()))?;
            debug!("let rvalue type: {:?}", rvalue_ty.kind);

            if ty.kind != rvalue_ty.kind {
                return Err(LoweringError::UnexpectedType {
                    span: rvalue_span,
                    found: rvalue_ty,
                    expected: ty.clone(),
                    program_id: builder.local_module.program_id,
                });
            }

            let local_idx = builder.name_to_local.get(&name.name).copied().unwrap();
            builder.local_exists.insert(local_idx);

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
        LetStmtTarget::Destructure(_) => {
            debug!("let target is a destructure");
            todo!()
        }
    };
    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_assign(builder: &mut FnBodyBuilder, info: &AssignStmt) -> Result<(), LoweringError> {
    debug!("begin lowering assign");
    let (mut place, mut ty, _path_span) = lower_path(builder, &info.lvalue)?;

    if !builder.body.locals[place.local].is_mutable() {
        return Err(LoweringError::NotMutable {
            span: info.span,
            declare_span: builder.body.locals[place.local].span,
            program_id: builder.body.id.program_id,
        });
    }

    for _ in 0..info.derefs {
        match &ty.kind {
            TyKind::Ref(inner, is_mut) | TyKind::Ptr(inner, is_mut) => {
                if matches!(is_mut, Mutability::Not) {
                    Err(LoweringError::BorrowNotMutable {
                        span: info.lvalue.first.span,
                        name: info.lvalue.first.name.clone(),
                        type_span: ty.span,
                        program_id: builder.local_module.program_id,
                    })?;
                }
                ty = *inner.clone();
            }
            _ => unreachable!(),
        }
        place.projection.push(PlaceElem::Deref);
    }

    let (rvalue, rvalue_ty, rvalue_span) =
        lower_expression(builder, &info.rvalue, Some(ty.clone()))?;

    if ty.kind != rvalue_ty.kind {
        return Err(LoweringError::UnexpectedType {
            span: rvalue_span,
            found: rvalue_ty,
            expected: ty.clone(),
            program_id: builder.local_module.program_id,
        });
    }

    builder.statements.push(Statement {
        span: Some(info.lvalue.first.span),
        kind: StatementKind::Assign(place, rvalue),
    });

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_return(
    builder: &mut FnBodyBuilder,
    info: &ReturnStmt,
    ret_type: Ty,
) -> Result<(), LoweringError> {
    debug!("begin lowering return");
    if let Some(value_exp) = &info.value {
        debug!("return has value");
        let (value, value_ty, _exp_span) =
            lower_expression(builder, value_exp, Some(ret_type.clone()))?;

        if ret_type.kind != value_ty.kind {
            return Err(LoweringError::UnexpectedType {
                span: info.span,
                found: value_ty,
                expected: ret_type.clone(),
                program_id: builder.local_module.program_id,
            });
        }

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
    }

    let statements = std::mem::take(&mut builder.statements);
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Return,
        }),
    });

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn find_expression_type(
    builder: &mut FnBodyBuilder,
    info: &Expression,
) -> Result<Option<Ty>, LoweringError> {
    debug!("finding expression type");
    Ok(match info {
        Expression::Value(value, _) => match value {
            ValueExpr::ConstBool(_, span) => Some(Ty {
                span: Some(*span),
                kind: TyKind::Bool,
            }),
            ValueExpr::ConstChar(_, span) => Some(Ty {
                span: Some(*span),
                kind: TyKind::Char,
            }),
            ValueExpr::ConstInt(_, _span) => None,
            ValueExpr::ConstFloat(_, _span) => None,
            ValueExpr::ConstStr(_, span) => Some(Ty {
                span: Some(*span),
                kind: TyKind::String,
            }),
            ValueExpr::Path(path) => {
                let local = builder.get_local(&path.first.name).unwrap(); // todo handle segments
                Some(local.ty.clone())
            }
        },
        Expression::FnCall(info) => {
            let fn_id = {
                let mod_body = builder.get_module_body();

                if let Some(id) = mod_body.symbols.functions.get(&info.target.name) {
                    *id
                } else {
                    *mod_body
                        .imports
                        .get(&info.target.name)
                        .expect("function call not found")
                }
            };
            let fn_sig = builder.ctx.body.function_signatures.get(&fn_id).unwrap();
            Some(fn_sig.1.clone())
        }
        Expression::Match(_) => None,
        Expression::If(_) => None,
        Expression::UnaryOp(_, info) => find_expression_type(builder, info)?,
        Expression::BinaryOp(lhs, op, rhs) => {
            if matches!(op, BinaryOp::Logic(_)) {
                Some(Ty {
                    span: None,
                    kind: TyKind::Bool,
                })
            } else {
                find_expression_type(builder, lhs)?.or(find_expression_type(builder, rhs)?)
            }
        }
        Expression::Deref(_, _) => {
            todo!()
        }
        Expression::AsRef(_, _, _) => todo!(),
        Expression::StructInit(info) => {
            let mut id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name.name)
                .or_else(|| builder.get_module_body().imports.get(&info.name.name.name))
                .expect("struct not found");
            let module_id = builder.local_module;

            let mut generics = Vec::new();

            if !info.name.generics.is_empty() {
                generics = info
                    .name
                    .generics
                    .iter()
                    .map(|x| {
                        lower_type(
                            &mut builder.ctx,
                            &TypeDescriptor::Type {
                                name: x.clone(),
                                span: x.span,
                            },
                            module_id,
                            builder.generic_map.as_ref(),
                        )
                    })
                    .collect::<Result<_, _>>()?;

                let generic_struct_id = GenericStruct {
                    id,
                    generics: generics.clone(),
                };

                if let Some(mono_id) = builder.ctx.generic_structs.get(&generic_struct_id) {
                    id = *mono_id;
                } else {
                    let next_id = builder.ctx.gen.next_defid();
                    let body = builder
                        .ctx
                        .generic_struct_bodies
                        .get(&id)
                        .expect("generic body not found")
                        .clone();
                    let mut generic_map = builder.generic_map.clone().unwrap_or_default();

                    for (gen_ty, gen_name) in info.name.generics.iter().zip(body.generics.iter()) {
                        if generic_map.contains_key(&gen_name.name.name) {
                            debug!(
                                "Generic map already countains a record: {}, wanted to add {}",
                                gen_name.name.name, gen_ty
                            );
                            continue;
                        }
                        generic_map.insert(gen_name.name.name.clone(), gen_ty.clone());
                    }

                    lower_struct(
                        &mut builder.ctx,
                        &body,
                        next_id,
                        module_id,
                        Some(&generic_map),
                    )?;
                    id = next_id;
                    builder
                        .ctx
                        .generic_structs
                        .insert(generic_struct_id, next_id);
                }
            }

            let ty = Ty {
                span: Some(info.span),
                kind: TyKind::Struct {
                    id,
                    name: info.name.name.name.clone(),
                    generics,
                },
            };
            Some(ty)
        }
        Expression::Cast(_, _, _) => todo!(),
        Expression::ArrayInit(info) => {
            let first_element = info.values.first();

            if first_element.is_none() {
                return Ok(None);
            }

            let first_element = first_element.unwrap();

            let first_type = find_expression_type(builder, first_element)?;

            if first_type.is_none() {
                return Ok(None);
            }

            let first_type = first_type.unwrap();

            let length = info.values.len() as u64;

            Some(Ty {
                span: Some(info.span),
                kind: TyKind::Array(
                    Box::new(first_type),
                    Box::new(ConstData {
                        ty: Ty {
                            span: None,
                            kind: TyKind::Uint(UintTy::U64),
                        },
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(length))),
                    }),
                ),
            })
        }
    })
}

#[instrument(level = "debug", skip_all)]
fn lower_expression(
    builder: &mut FnBodyBuilder,
    info: &Expression,
    type_hint: Option<Ty>,
) -> Result<(Rvalue, Ty, Span), LoweringError> {
    Ok(match info {
        Expression::Value(info, span) => {
            debug!("lowering value");
            let value = lower_value_expr(builder, info, type_hint)?;
            (value.0, value.1, *span)
        }
        Expression::FnCall(info) => {
            debug!("lowering fncall");
            lower_fn_call(builder, info, None, None)?
        }
        Expression::Match(_) => {
            debug!("lowering match");
            todo!()
        }
        Expression::If(_) => {
            debug!("lowering if expression");
            todo!()
        }
        Expression::UnaryOp(_, _) => {
            debug!("lowering unary op");
            todo!()
        }
        Expression::BinaryOp(lhs, op, rhs) => lower_binary_op(builder, lhs, *op, rhs, type_hint)?,
        Expression::Deref(info, deref_span) => {
            debug!("lowering deref");
            let (value, ty, _span) = lower_expression(builder, info, type_hint)?;

            let mut place = match value {
                Rvalue::Ref(_, place) => place,
                Rvalue::Use(op) => match op {
                    Operand::Place(place) => place,
                    Operand::Const(_) => todo!("deref to constant data not yet implemented"),
                },
                value => todo!("deref not implemented for {value:?}"),
            };

            let ty = Ty {
                span: Some(*deref_span),
                kind: match ty.kind {
                    TyKind::Ref(inner, _) => inner.kind.clone(),
                    TyKind::Ptr(inner, _) => inner.kind.clone(),
                    _ => todo!(),
                },
            };

            place.projection.push(PlaceElem::Deref);

            (Rvalue::Use(Operand::Place(place)), ty, *deref_span)
        }
        Expression::AsRef(inner, mutable, asref_span) => {
            debug!("lowering asref");
            let type_hint = match type_hint {
                Some(inner) => match inner.kind {
                    TyKind::Ref(inner, _) => Some(*inner.clone()),
                    _ => unreachable!(),
                },
                None => None,
            };
            let (value, ty, _ref_target_span) = lower_expression(builder, inner, type_hint)?;

            if let Some(local) = value.get_local() {
                if *mutable && !builder.body.locals[local].mutable {
                    return Err(LoweringError::CantTakeMutableBorrow {
                        span: *asref_span,
                        declare_span: builder.body.locals[local].span,
                        program_id: builder.body.id.program_id,
                    });
                }
            }

            let mutability = match mutable {
                false => Mutability::Not,
                true => Mutability::Mut,
            };

            // check if its a use directly, to avoid a temporary.
            let rvalue = match value {
                Rvalue::Use(op) => Rvalue::Ref(
                    mutability,
                    match op {
                        Operand::Place(place) => place,
                        Operand::Const(_) => todo!("reference to literals not implemented yet"),
                    },
                ),
                value => {
                    let inner_local = builder.add_local(Local::temp(ty.clone()));
                    let inner_place = Place {
                        local: inner_local,
                        projection: Default::default(),
                    };

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::StorageLive(inner_local),
                    });

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::Assign(inner_place.clone(), value),
                    });
                    Rvalue::Ref(mutability, inner_place)
                }
            };

            let ty = Ty {
                span: Some(*asref_span),
                kind: TyKind::Ref(Box::new(ty.clone()), mutability),
            };

            (rvalue, ty, *asref_span)
        }
        Expression::StructInit(info) => {
            debug!("lowering struct init for struct {}", info.name);
            let mut id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name.name)
                .or_else(|| builder.get_module_body().imports.get(&info.name.name.name))
                .expect("struct not found");
            let module_id = builder.local_module;

            let mut generics = Vec::new();

            // For now a generic struct needs to be instanced specifying the generic types.
            if !info.name.generics.is_empty() {
                generics = info
                    .name
                    .generics
                    .iter()
                    .map(|x| {
                        lower_type(
                            &mut builder.ctx,
                            &TypeDescriptor::Type {
                                name: x.clone(),
                                span: x.span,
                            },
                            module_id,
                            builder.generic_map.as_ref(),
                        )
                    })
                    .collect::<Result<_, _>>()?;

                let generic_struct_id = GenericStruct {
                    id,
                    generics: generics.clone(),
                };

                debug!(
                    "struct has generics, checking generic struct id: {:?}",
                    generic_struct_id
                );

                if let Some(mono_id) = builder.ctx.generic_structs.get(&generic_struct_id) {
                    debug!("monomorphized struct id found: {mono_id:?}");
                    id = *mono_id;
                } else {
                    let next_id = builder.ctx.gen.next_defid();
                    debug!("monomorphized id not found, lowering with id {next_id:?}");
                    let body = builder
                        .ctx
                        .generic_struct_bodies
                        .get(&id)
                        .expect("generic body not found")
                        .clone();
                    let mut generic_map = builder.generic_map.clone().unwrap_or_default();

                    for (gen_ty, gen_name) in info.name.generics.iter().zip(body.generics.iter()) {
                        if generic_map.contains_key(&gen_name.name.name) {
                            debug!(
                                "Generic map already countains a record: {}, wanted to add {}",
                                gen_name.name.name, gen_ty
                            );
                            continue;
                        }
                        generic_map.insert(gen_name.name.name.clone(), gen_ty.clone());
                    }

                    lower_struct(
                        &mut builder.ctx,
                        &body,
                        next_id,
                        module_id,
                        Some(&generic_map),
                    )?;
                    id = next_id;
                    builder
                        .ctx
                        .generic_structs
                        .insert(generic_struct_id, next_id);
                }
            }
            let struct_body = builder.ctx.body.structs.get(&id).unwrap().clone();

            let ty = Ty {
                span: Some(info.span),
                kind: TyKind::Struct {
                    id,
                    name: info.name.name.name.clone(),
                    generics,
                },
            };
            let struct_local = builder.add_local(Local::temp(ty.clone()));

            let place = Place {
                local: struct_local,
                projection: Default::default(),
            };

            builder.statements.push(Statement {
                span: None,
                kind: StatementKind::StorageLive(struct_local),
            });

            for (field, value) in info.fields.iter() {
                let idx = *struct_body
                    .name_to_idx
                    .get(&field.name)
                    .expect("failed to find field");
                let mut field_place = place.clone();
                field_place.projection.push(PlaceElem::Field(idx));

                let variant = &struct_body.variants[idx].ty;

                let (value, _value_ty, _field_span) =
                    lower_expression(builder, &value.value, Some(variant.clone()))?;

                builder.statements.push(Statement {
                    span: Some(info.span),
                    kind: StatementKind::Assign(field_place, value),
                });
            }

            (Rvalue::Use(Operand::Place(place)), ty, info.span)
        }
        Expression::Cast(value, cast_ty, span) => {
            let (value, ty, _span) = lower_expression(builder, value, None)?;

            let new_ty = lower_type(
                &mut builder.ctx,
                cast_ty,
                builder.local_module,
                builder.generic_map.as_ref(),
            )?;

            // todo: check if the cast is valid

            // check if its a use directly, to avoid a temporary.
            let rvalue = match value {
                Rvalue::Use(op) => Rvalue::Cast(op, new_ty.clone(), *span),
                value => {
                    let inner_local = builder.add_local(Local::temp(ty.clone()));
                    let inner_place = Place {
                        local: inner_local,
                        projection: Default::default(),
                    };

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::StorageLive(inner_local),
                    });

                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::Assign(inner_place.clone(), value),
                    });
                    Rvalue::Cast(Operand::Place(inner_place), new_ty.clone(), *span)
                }
            };

            (rvalue, new_ty, *span)
        }
        Expression::ArrayInit(info) => {
            let element_type_hint = type_hint.and_then(|type_hint| match type_hint.kind {
                TyKind::Array(type_hint, _) => Some(*type_hint),
                _ => None,
            });

            let mut values = info.values.iter().enumerate();

            // Extract the first value from the array init. It's type will be used as type hint for the following elements.
            let (first_idx, first_element) = values.next().expect("array init cannot be empty");
            let (first_value, element_type, _element_span) =
                lower_expression(builder, first_element, element_type_hint)?;

            let length = info.values.len() as u64;

            let ty = Ty {
                span: Some(info.span),
                kind: TyKind::Array(
                    Box::new(element_type.clone()),
                    Box::new(ConstData {
                        ty: Ty {
                            span: None,
                            kind: TyKind::Uint(UintTy::U64),
                        },
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(length))),
                    }),
                ),
            };

            // Create and init local for the array init expression.
            let array_local = builder.add_local(Local::temp(ty.clone()));
            let place = Place {
                local: array_local,
                projection: Default::default(),
            };
            builder.statements.push(Statement {
                span: None,
                kind: StatementKind::StorageLive(array_local),
            });

            // Assign the first value of the expression
            let mut first_place = place.clone();
            first_place
                .projection
                .push(PlaceElem::ConstantIndex(first_idx as u64));
            builder.statements.push(Statement {
                span: Some(info.span),
                kind: StatementKind::Assign(first_place, first_value),
            });

            // Loop over the remaining values and assign them
            for (idx, element) in values {
                let mut element_place = place.clone();
                element_place
                    .projection
                    .push(PlaceElem::ConstantIndex(idx as u64));

                let (value, _value_ty, _field_span) =
                    lower_expression(builder, element, Some(element_type.clone()))?;

                builder.statements.push(Statement {
                    span: Some(info.span),
                    kind: StatementKind::Assign(element_place, value),
                });
            }

            (Rvalue::Use(Operand::Place(place)), ty, info.span)
        }
    })
}

#[instrument(level = "debug", skip_all)]
fn lower_fn_call(
    builder: &mut FnBodyBuilder,
    info: &FnCallOp,
    self_value: Option<(Place, Ty)>,
    // The id of the fn to call, in case its a method.
    fn_id: Option<DefId>,
) -> Result<(Rvalue, Ty, Span), LoweringError> {
    debug!("lowering fn call");
    let mut fn_id = {
        let mod_body = builder.get_module_body();

        if let Some(id) = fn_id {
            id
        } else if let Some(id) = mod_body.symbols.functions.get(&info.target.name) {
            *id
        } else {
            *mod_body
                .imports
                .get(&info.target.name)
                .ok_or(LoweringError::FunctionNotFound {
                    span: info.target.span,
                    function: info.target.name.clone(),
                    program_id: builder.local_module.program_id,
                })?
        }
    };

    let mut generic_map = HashMap::new();

    if let Some(generic_fn_def) = builder.ctx.generic_fn_bodies.get(&fn_id) {
        // todo: fix this clone

        if info.generics.len() != generic_fn_def.decl.generic_params.len() {
            // todo: this check will be removed/refactored when we have inference for generics from the arguments used.
            return Err(LoweringError::GenericCountMismatch {
                span: info.span,
                found: info.generics.len(),
                needs: generic_fn_def.decl.generic_params.len(),
                program_id: builder.local_module.program_id,
            });
        }

        debug!("lowering call to generic function");

        for (generic_ty, generic_param) in info
            .generics
            .iter()
            .zip(generic_fn_def.decl.generic_params.iter())
        {
            generic_map.insert(generic_param.name.name.clone(), generic_ty.clone());
        }

        (builder.ctx, fn_id) = lower_func(
            builder.ctx.clone(),
            &generic_fn_def.clone(),
            builder.local_module,
            None,
            Some(&generic_map),
        )?;
    }

    let (args_ty, ret_ty) = {
        if let Some(x) = builder.ctx.body.function_signatures.get(&fn_id) {
            x.clone()
        } else {
            let (args, ret) = builder
                .ctx
                .unresolved_function_signatures
                .get(&fn_id)
                .unwrap()
                .clone();

            let args: Vec<Ty> = args
                .iter()
                .map(|arg| {
                    lower_type_with_self(
                        &mut builder.ctx,
                        arg,
                        builder.local_module,
                        self_value.as_ref().map(|x| &x.1),
                        Some(&generic_map),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ret = ret
                .as_ref()
                .map(|arg| {
                    lower_type(
                        &mut builder.ctx,
                        arg,
                        builder.local_module,
                        Some(&generic_map),
                    )
                })
                .unwrap_or(Ok(Ty {
                    span: None,
                    kind: TyKind::Unit,
                }))?;
            builder
                .ctx
                .body
                .function_signatures
                .insert(fn_id, (args.clone(), ret.clone()));
            (args, ret)
        }
    };

    if args_ty.len()
        != info.args.len() + {
            if self_value.is_some() {
                1
            } else {
                0
            }
        }
    {
        return Err(LoweringError::CallParamCountMismatch {
            span: info.span,
            found: info.args.len(),
            needs: args_ty.len(),
            program_id: builder.get_module_body().id.program_id,
        });
    }

    let mut args = Vec::new();

    let mut args_ty_iter = args_ty.into_iter();

    // Add the self value if there is one.
    if let Some((arg, _arg_ty)) = self_value {
        // Here arg_ty is the type without references.
        // We should use the type from the fn sig to know if it needs a reference.
        let expected_ty = args_ty_iter.next().expect("self ty should be there");
        match expected_ty.kind {
            TyKind::Ref(_, mutability) => {
                args.push(Rvalue::Ref(mutability, arg.clone()));
            }
            _ => {
                args.push(Rvalue::Use(Operand::Place(arg.clone())));
            }
        }
    }

    for (arg, arg_ty) in info.args.iter().zip(args_ty_iter) {
        let (rvalue, rvalue_ty, arg_span) = lower_expression(builder, arg, Some(arg_ty.clone()))?;

        if rvalue_ty.kind != arg_ty.kind {
            return Err(LoweringError::UnexpectedType {
                span: arg_span,
                found: rvalue_ty,
                expected: arg_ty,
                program_id: builder.get_module_body().id.program_id,
            });
        }

        args.push(rvalue);
    }

    let dest_local = builder.add_local(Local::temp(ret_ty.clone()));

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

    Ok((
        Rvalue::Use(Operand::Place(dest_place)),
        ret_ty.clone(),
        info.span,
    ))
}

fn lower_binary_op(
    builder: &mut FnBodyBuilder,
    lhs: &Expression,
    op: BinaryOp,
    rhs: &Expression,
    type_hint: Option<Ty>,
) -> Result<(Rvalue, Ty, Span), LoweringError> {
    let (lhs, lhs_ty, lhs_span) = if type_hint.is_none() {
        let ty = find_expression_type(builder, lhs)
            .transpose()
            .or_else(|| find_expression_type(builder, rhs).transpose())
            .transpose()?;

        let ty = if let Some(ty) = ty {
            ty
        } else {
            // Default to i32 if cant infer type.
            // Should be ok because at other points if the i32 doesn't match the expected type
            // a error will be thrown, forcing user to specify types.
            debug!("can't infer type, defaulting to i32");
            Ty {
                span: None,
                kind: TyKind::Int(IntTy::I32),
            }
        };

        lower_expression(builder, lhs, Some(ty))?
    } else {
        lower_expression(builder, lhs, type_hint.clone())?
    };

    // We must handle the special case where you can do ptr + offset.
    let is_lhs_ptr = matches!(lhs_ty.kind, TyKind::Ptr(_, _));

    let (rhs, rhs_ty, rhs_span) = if type_hint.is_none() {
        let ty = find_expression_type(builder, rhs)?.unwrap_or(lhs_ty.clone());
        lower_expression(builder, rhs, if is_lhs_ptr { None } else { Some(ty) })?
    } else {
        lower_expression(
            builder,
            rhs,
            if is_lhs_ptr { None } else { type_hint.clone() },
        )?
    };

    if !is_lhs_ptr && lhs_ty != rhs_ty {
        return Err(LoweringError::UnexpectedType {
            span: rhs_span,
            found: rhs_ty,
            expected: lhs_ty,
            program_id: builder.local_module.program_id,
        });
    }

    let lhs_local = builder.add_local(Local::temp(lhs_ty.clone()));
    let rhs_local = builder.add_local(Local::temp(rhs_ty));
    let lhs_place = Place {
        local: lhs_local,
        projection: vec![],
    };
    let rhs_place = Place {
        local: rhs_local,
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

    let full_span = Span::new(lhs_span.from, rhs_span.to);

    Ok(match op {
        BinaryOp::Arith(op) => (
            match op {
                ArithOp::Add => Rvalue::BinaryOp(BinOp::Add, (lhs, rhs)),
                ArithOp::Sub => Rvalue::BinaryOp(BinOp::Sub, (lhs, rhs)),
                ArithOp::Mul => Rvalue::BinaryOp(BinOp::Mul, (lhs, rhs)),
                ArithOp::Div => Rvalue::BinaryOp(BinOp::Div, (lhs, rhs)),
                ArithOp::Mod => Rvalue::BinaryOp(BinOp::Mod, (lhs, rhs)),
            },
            lhs_ty,
            full_span,
        ),
        BinaryOp::Logic(op) => (
            match op {
                LogicOp::And => Rvalue::LogicOp(LogOp::And, (lhs, rhs)),
                LogicOp::Or => Rvalue::LogicOp(LogOp::Or, (lhs, rhs)),
            },
            Ty {
                span: Some(full_span),
                kind: TyKind::Bool,
            },
            full_span,
        ),
        BinaryOp::Compare(op) => (
            match op {
                CmpOp::Eq => Rvalue::BinaryOp(BinOp::Eq, (lhs, rhs)),
                CmpOp::NotEq => Rvalue::BinaryOp(BinOp::Ne, (lhs, rhs)),
                CmpOp::Lt => Rvalue::BinaryOp(BinOp::Lt, (lhs, rhs)),
                CmpOp::LtEq => Rvalue::BinaryOp(BinOp::Le, (lhs, rhs)),
                CmpOp::Gt => Rvalue::BinaryOp(BinOp::Gt, (lhs, rhs)),
                CmpOp::GtEq => Rvalue::BinaryOp(BinOp::Ge, (lhs, rhs)),
            },
            Ty {
                span: Some(full_span),
                kind: TyKind::Bool,
            },
            full_span,
        ),
        BinaryOp::Bitwise(op) => (
            match op {
                BitwiseOp::And => Rvalue::BinaryOp(BinOp::BitAnd, (lhs, rhs)),
                BitwiseOp::Or => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
                BitwiseOp::Xor => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
            },
            lhs_ty,
            full_span,
        ),
    })
}

fn lower_value_expr(
    builder: &mut FnBodyBuilder,
    info: &ValueExpr,
    type_hint: Option<Ty>,
) -> Result<(Rvalue, Ty), LoweringError> {
    Ok(match info {
        ValueExpr::ConstBool(value, const_span) => (
            Rvalue::Use(Operand::Const(ConstData {
                ty: Ty {
                    span: Some(*const_span),
                    kind: TyKind::Bool,
                },
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(*value))),
            })),
            Ty {
                span: Some(*const_span),
                kind: TyKind::Bool,
            },
        ),
        ValueExpr::ConstChar(value, const_span) => (
            Rvalue::Use(Operand::Const(ConstData {
                ty: Ty {
                    span: Some(*const_span),
                    kind: TyKind::Char,
                },
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::Char((*value) as u8))),
            })),
            Ty {
                span: Some(*const_span),
                kind: TyKind::Char,
            },
        ),
        ValueExpr::ConstInt(value, const_span) => {
            let (data, ty) = match type_hint {
                Some(ty) => (
                    ConstData {
                        ty: ty.clone(),
                        data: ConstKind::Value(ValueTree::Leaf(match ty.kind {
                            TyKind::Int(ty) => match ty {
                                IntTy::I8 => {
                                    ConstValue::I8((*value).try_into().expect("value out of range"))
                                }
                                IntTy::I16 => ConstValue::I16(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                IntTy::I32 => ConstValue::I32(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                IntTy::I64 => ConstValue::I64(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                IntTy::I128 => ConstValue::I128(
                                    (*value).try_into().expect("value out of range"),
                                ),
                            },
                            TyKind::Uint(ty) => match ty {
                                UintTy::U8 => {
                                    ConstValue::U8((*value).try_into().expect("value out of range"))
                                }
                                UintTy::U16 => ConstValue::U16(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                UintTy::U32 => ConstValue::U32(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                UintTy::U64 => ConstValue::U64(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                UintTy::U128 => ConstValue::U128(*value),
                            },
                            TyKind::Bool => ConstValue::Bool(*value != 0),
                            TyKind::Ptr(ref _inner, _mutable) => {
                                ConstValue::I64((*value).try_into().expect("value out of range"))
                            }
                            x => unreachable!("{:?}", x),
                        })),
                    },
                    ty,
                ),
                None => (
                    ConstData {
                        ty: Ty {
                            span: Some(*const_span),
                            kind: TyKind::Int(IntTy::I64),
                        },
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::I64(
                            (*value).try_into().expect("value out of range"),
                        ))),
                    },
                    Ty {
                        span: Some(*const_span),
                        kind: TyKind::Int(IntTy::I64),
                    },
                ),
            };

            (Rvalue::Use(Operand::Const(data)), ty)
        }
        ValueExpr::ConstFloat(value, const_span) => {
            let (data, ty) = match type_hint {
                Some(ty) => (
                    ConstData {
                        ty: ty.clone(),
                        data: ConstKind::Value(ValueTree::Leaf(match &ty.kind {
                            TyKind::Float(ty) => match ty {
                                FloatTy::F32 => {
                                    ConstValue::F32(value.parse().expect("error parsing float"))
                                }
                                FloatTy::F64 => {
                                    ConstValue::F64(value.parse().expect("error parsing float"))
                                }
                            },
                            _ => unreachable!(),
                        })),
                    },
                    ty,
                ),
                None => (
                    ConstData {
                        ty: Ty {
                            span: Some(*const_span),
                            kind: TyKind::Float(FloatTy::F64),
                        },
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::F64(
                            value.parse().expect("error parsing float"),
                        ))),
                    },
                    Ty {
                        span: Some(*const_span),
                        kind: TyKind::Float(FloatTy::F64),
                    },
                ),
            };

            (Rvalue::Use(Operand::Const(data)), ty)
        }
        ValueExpr::ConstStr(_, _) => todo!(),
        ValueExpr::Path(info) => {
            if builder.name_to_local.contains_key(&info.first.name) {
                let (place, place_ty, _span) = lower_path(builder, info)?;
                (Rvalue::Use(Operand::Place(place.clone())), place_ty)
            } else {
                let (constant_value, ty) = lower_constant_ref(builder, info)?;
                (Rvalue::Use(Operand::Const(constant_value)), ty)
            }
        }
    })
}

fn lower_constant_ref(
    builder: &mut FnBodyBuilder,
    info: &PathOp,
) -> Result<(ConstData, Ty), LoweringError> {
    let mod_body = builder.get_module_body();

    let Some(&constant_id) = mod_body.symbols.constants.get(&info.first.name) else {
        return Err(LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            program_id: builder.local_module.program_id,
        });
    };

    let constant_value = builder
        .ctx
        .body
        .constants
        .get(&constant_id)
        .expect("constant should exist")
        .value
        .clone();

    let ty = constant_value.ty.clone();

    Ok((constant_value, ty))
}

pub fn lower_path(
    builder: &mut FnBodyBuilder,
    info: &PathOp,
) -> Result<(Place, Ty, Span), LoweringError> {
    let mut local = *builder.name_to_local.get(&info.first.name).ok_or(
        LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            program_id: builder.local_module.program_id,
        },
    )?;

    if !builder.local_exists.contains(&local) {
        Err(LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            program_id: builder.local_module.program_id,
        })?;
    }

    let ty = builder.body.locals[local].ty.clone();
    let mut ty = ty;
    let mut projection = Vec::new();

    for segment in &info.extra {
        match segment {
            PathSegment::FieldAccess(name, field_span) => {
                // auto deref
                while let TyKind::Ref(inner, _) = ty.kind {
                    projection.push(PlaceElem::Deref);
                    ty = *inner;
                }

                if let TyKind::Struct {
                    id,
                    generics: _,
                    name: _,
                } = ty.kind
                {
                    let struct_body = builder.ctx.body.structs.get(&id).unwrap();
                    let idx = *struct_body.name_to_idx.get(&name.name).ok_or_else(|| {
                        LoweringError::StructFieldNotFound {
                            span: *field_span,
                            name: name.name.clone(),
                            program_id: builder.local_module.program_id,
                        }
                    })?;
                    projection.push(PlaceElem::Field(idx));
                    ty = struct_body.variants[idx].ty.clone();
                }
            }
            PathSegment::ArrayIndex(expression, _) => {
                while let TyKind::Ref(inner, _) = ty.kind {
                    projection.push(PlaceElem::Deref);
                    ty = *inner;
                }

                if let TyKind::Array(element_type, _) = ty.kind {
                    // Assign the index expression to a temporary local
                    let (index, index_ty) = lower_value_expr(builder, expression, None)?;
                    let index_local = builder.add_temp_local(index_ty.kind);
                    let index_place = Place {
                        local: index_local,
                        projection: vec![],
                    };
                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::StorageLive(index_local),
                    });
                    builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::Assign(index_place.clone(), index),
                    });

                    // Use the local's value as index of the array
                    projection.push(PlaceElem::Index(index_local));

                    ty = *element_type;
                }
            }
            PathSegment::MethodCall(fn_call_op, _span) => {
                // auto deref

                loop {
                    if let Some(methods) = builder.ctx.body.methods.get(&ty.kind) {
                        if let Some(defid) = methods.get(&fn_call_op.target.name) {
                            let (value, new_ty, _span) = lower_fn_call(
                                builder,
                                fn_call_op,
                                Some((
                                    Place {
                                        local,
                                        projection: projection.clone(),
                                    },
                                    ty.clone(),
                                )),
                                Some(*defid),
                            )?;

                            ty = new_ty;

                            match value {
                                Rvalue::Use(operand) => match operand {
                                    Operand::Place(place) => {
                                        local = place.local;
                                        projection = place.projection;
                                    }
                                    Operand::Const(_const_data) => todo!(),
                                },
                                Rvalue::Ref(_mutability, _place) => todo!(),
                                _ => unreachable!(),
                            }

                            break;
                        }
                    } else if let TyKind::Ref(inner, _) = ty.kind {
                        projection.push(PlaceElem::Deref);
                        ty = *inner;
                    } else {
                        Err(LoweringError::FunctionNotFound {
                            span: fn_call_op.target.span,
                            function: fn_call_op.target.name.clone(),
                            program_id: builder.local_module.program_id,
                        })?;
                    }
                }
            }
        }
    }

    Ok((Place { local, projection }, ty, info.span))
}

pub fn lower_type_with_self(
    ctx: &mut BuildCtx,
    ty: &TypeDescriptor,
    module_id: DefId,
    self_ty: Option<&Ty>,
    generic_map: Option<&HashMap<String, TypeName>>,
) -> Result<Ty, LoweringError> {
    if let TypeDescriptor::SelfType { is_ref, is_mut } = ty {
        let self_ty = self_ty.unwrap().clone();
        if *is_ref {
            return Ok(Ty {
                span: self_ty.span,
                kind: TyKind::Ref(
                    Box::new(self_ty),
                    if *is_mut {
                        Mutability::Mut
                    } else {
                        Mutability::Not
                    },
                ),
            });
        }
        Ok(self_ty)
    } else {
        lower_type(ctx, ty, module_id, generic_map)
    }
}

/// Lowers a type to it's IR version.
///
/// If a generic map is given, it will be used to resolve a generic type name like "T"
/// to its proper non-generic type.
#[instrument(level = "debug", skip_all, fields(ty = %ty))]
pub fn lower_type(
    ctx: &mut BuildCtx,
    ty: &TypeDescriptor,
    module_id: DefId,
    // A local context map to resolve generics to specific types
    generic_map: Option<&HashMap<String, TypeName>>,
) -> Result<Ty, LoweringError> {
    debug!("lowering type");
    Ok(match ty {
        TypeDescriptor::Type { name, span } => match name.name.name.as_str() {
            "i64" => Ty::new(span, TyKind::Int(IntTy::I64)),
            "i32" => Ty::new(span, TyKind::Int(IntTy::I32)),
            "i16" => Ty::new(span, TyKind::Int(IntTy::I16)),
            "i8" => Ty::new(span, TyKind::Int(IntTy::I8)),
            "u64" => Ty::new(span, TyKind::Uint(UintTy::U64)),
            "u32" => Ty::new(span, TyKind::Uint(UintTy::U32)),
            "u16" => Ty::new(span, TyKind::Uint(UintTy::U16)),
            "u8" => Ty::new(span, TyKind::Uint(UintTy::U8)),
            "f32" => Ty::new(span, TyKind::Float(FloatTy::F32)),
            "f64" => Ty::new(span, TyKind::Float(FloatTy::F64)),
            "bool" => Ty::new(span, TyKind::Bool),
            "string" => Ty::new(span, TyKind::String),
            "char" => Ty::new(span, TyKind::Char),
            other => {
                let module = ctx.body.modules.get(&module_id).expect("module not found");

                // Check if the type name exists in the generic map.
                if let Some(inner_generic_map) = generic_map {
                    if let Some(generic_ty_name) = inner_generic_map.get(other) {
                        debug!("Type found in generic map: {}", generic_ty_name);
                        return lower_type(
                            ctx,
                            &TypeDescriptor::Type {
                                name: generic_ty_name.clone(),
                                span: generic_ty_name.span,
                            },
                            module_id,
                            generic_map,
                        );
                    }
                }

                // Find on imports or local module
                let mut def_id = *module
                    .imports
                    .get(other)
                    .or_else(|| module.symbols.structs.get(other))
                    .ok_or_else(|| LoweringError::UnrecognizedType {
                        span: *span,
                        name: other.to_string(),
                        program_id: module_id.program_id,
                    })?;

                debug!("Found def_id={def_id:?}");

                let mut generic_tys = Vec::new();
                for generic in &name.generics {
                    generic_tys.push(lower_type(
                        ctx,
                        &TypeDescriptor::Type {
                            name: generic.clone(),
                            span: generic.span,
                        },
                        module_id,
                        generic_map,
                    )?);
                }

                debug!("generic_tys: {:?}", &generic_tys);

                let generic_struct = GenericStruct {
                    id: def_id,
                    generics: generic_tys.clone(),
                };

                if let Some(body) = ctx.generic_structs.get(&generic_struct) {
                    debug!("Found monomorphized struct generic: {body:?}");
                    def_id = *body;
                }

                // Check if its a struct.
                if ctx.body.structs.contains_key(&def_id) {
                    return Ok(Ty::new(
                        span,
                        TyKind::Struct {
                            id: def_id,
                            name: other.to_string(),
                            generics: generic_tys,
                        },
                    ));
                } else if let Some(adt_body) = ctx.generic_struct_bodies.get(&def_id).cloned() {
                    let next_id = ctx.gen.next_defid();
                    // the struct with the specific generic types is not yet monomorphized, so we monomorphize it here.

                    let mut generic_map = generic_map.cloned().unwrap_or_default();

                    assert_eq!(adt_body.generics.len(), name.generics.len());
                    for (gen_ty, gen_name) in name.generics.iter().zip(adt_body.generics.iter()) {
                        if generic_map.contains_key(&gen_name.name.name) {
                            debug!(
                                "Generic map already countains a record: {}, wanted to add {}",
                                gen_name.name.name, gen_ty
                            );
                            continue;
                        }
                        generic_map.insert(gen_name.name.name.clone(), gen_ty.clone());
                    }

                    lower_struct(ctx, &adt_body, next_id, module_id, Some(&generic_map))?;

                    ctx.generic_structs.insert(generic_struct, next_id);

                    return Ok(Ty::new(
                        span,
                        TyKind::Struct {
                            id: next_id,
                            name: other.to_string(),
                            generics: generic_tys,
                        },
                    ));
                } else {
                    Err(LoweringError::UnrecognizedType {
                        span: *span,
                        name: other.to_string(),
                        program_id: module_id.program_id,
                    })?
                }
            }
        },
        TypeDescriptor::Ref { of, span } => Ty::new(
            span,
            TyKind::Ref(
                Box::new(lower_type(ctx, of, module_id, generic_map)?),
                Mutability::Not,
            ),
        ),
        TypeDescriptor::MutRef { of, span } => Ty::new(
            span,
            TyKind::Ref(
                Box::new(lower_type(ctx, of, module_id, generic_map)?),
                Mutability::Mut,
            ),
        ),
        TypeDescriptor::ConstPtr { of, span } => Ty::new(
            span,
            TyKind::Ptr(
                Box::new(lower_type(ctx, of, module_id, generic_map)?),
                Mutability::Not,
            ),
        ),
        TypeDescriptor::MutPtr { of, span } => Ty::new(
            span,
            TyKind::Ptr(
                Box::new(lower_type(ctx, of, module_id, generic_map)?),
                Mutability::Mut,
            ),
        ),
        TypeDescriptor::Array { of, size, span } => Ty::new(
            span,
            TyKind::Array(
                Box::new(lower_type(ctx, of, module_id, generic_map)?),
                Box::new(ConstData {
                    ty: Ty {
                        span: Some(*span),
                        kind: TyKind::Uint(UintTy::U64),
                    },
                    data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(*size))),
                }),
            ),
        ),
        TypeDescriptor::SelfType {
            is_ref: _,
            is_mut: _,
        } => todo!(),
    })
}
