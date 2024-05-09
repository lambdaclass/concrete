use std::collections::HashMap;

use common::{BuildCtx, FnBodyBuilder, IdGenerator};
use concrete_ast::{
    common::Span,
    constants::ConstantDef,
    expressions::{
        ArithOp, BinaryOp, BitwiseOp, CmpOp, Expression, FnCallOp, IfExpr, LogicOp, PathOp,
        PathSegment, ValueExpr,
    },
    functions::{FunctionDecl, FunctionDef},
    modules::{Module, ModuleDefItem},
    statements::{self, AssignStmt, ForStmt, LetStmt, LetStmtTarget, ReturnStmt, WhileStmt},
    structs::StructDecl,
    types::{TypeQualifier, TypeSpec},
    Program,
};

use crate::{
    AdtBody, BasicBlock, BinOp, ConstBody, ConstData, ConstKind, ConstValue, DefId, FloatTy,
    FnBody, IntTy, Local, LocalKind, LogOp, Mutability, Operand, Place, PlaceElem, ProgramBody,
    Rvalue, Statement, StatementKind, SwitchTargets, Terminator, TerminatorKind, Ty, TyKind,
    UintTy, ValueTree, VariantDef,
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

fn lower_module(mut ctx: BuildCtx, module: &Module, id: DefId) -> Result<BuildCtx, LoweringError> {
    // lower first structs, constants, types
    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(info) => {
                ctx = lower_constant(ctx, info, id)?;
            }
            ModuleDefItem::Struct(info) => {
                ctx = lower_struct(ctx, info, id)?;
            }
            ModuleDefItem::Type(_) => todo!(),
            _ => {}
        }
    }

    let body = ctx.body.modules.get(&id).unwrap();

    // fill fn sigs
    for content in &module.contents {
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

                let mut args = Vec::new();
                let ret_type;

                for arg in &fn_def.decl.params {
                    let ty = lower_type(&ctx, &arg.r#type, id)?;
                    args.push(ty);
                }

                if let Some(ty) = &fn_def.decl.ret_type {
                    ret_type = lower_type(&ctx, ty, id)?;
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
                    let ty = lower_type(&ctx, &arg.r#type, id)?;
                    args.push(ty);
                }

                if let Some(ty) = &fn_decl.ret_type {
                    ret_type = lower_type(&ctx, ty, id)?;
                } else {
                    ret_type = Ty {
                        span: None,
                        kind: TyKind::Unit,
                    };
                }

                ctx.body.function_signatures.insert(fn_id, (args, ret_type));
                ctx.unresolved_function_signatures.remove(&fn_id);
            }
            _ => {}
        }
    }

    for content in &module.contents {
        match content {
            ModuleDefItem::Constant(_) => { /* already processed */ }
            ModuleDefItem::Function(fn_def) => {
                ctx = lower_func(ctx, fn_def, id)?;
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
        }
    }

    Ok(ctx)
}

fn lower_constant(
    mut ctx: BuildCtx,
    info: &ConstantDef,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
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

    let value_ty = lower_type(&ctx, &info.decl.r#type, module_id)?;

    let value = lower_constant_expression(&info.value, value_ty)?;

    let body = ConstBody { id, name, value };

    ctx.body.constants.insert(body.id, body);

    Ok(ctx)
}

fn lower_constant_expression(expression: &Expression, ty: Ty) -> Result<ConstData, LoweringError> {
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

fn lower_struct(
    mut ctx: BuildCtx,
    info: &StructDecl,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mut body = AdtBody {
        def_id: {
            let body = ctx.body.modules.get(&module_id).unwrap();
            *body.symbols.structs.get(&info.name.name).unwrap()
        },
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
            ty: lower_type(&ctx, &field.r#type, module_id)?,
            discriminant: i,
        };
        body.variants.push(variant);
        body.name_to_idx
            .insert(field.name.name.clone(), body.variants.len() - 1);
    }

    ctx.body.structs.insert(body.def_id, body);
    Ok(ctx)
}

fn lower_func(
    ctx: BuildCtx,
    func: &FunctionDef,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mut builder = FnBodyBuilder {
        body: FnBody {
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            is_extern: func.decl.is_extern,
            name: func.decl.name.name.clone(),
            id: {
                let body = ctx.body.modules.get(&module_id).unwrap();
                *body.symbols.functions.get(&func.decl.name.name).unwrap()
            },
        },
        local_module: module_id,
        ret_local: 0,
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ctx,
    };

    if !func.body.is_empty() && func.decl.is_extern {
        return Err(LoweringError::ExternFnWithBody {
            span: func.span,
            name: func.decl.name.name.clone(),
            program_id: module_id.program_id,
        });
    }

    let fn_id = *builder
        .get_module_body()
        .symbols
        .functions
        .get(&func.decl.name.name)
        .unwrap();
    let (args_ty, ret_ty) = builder
        .ctx
        .body
        .function_signatures
        .get(&fn_id)
        .unwrap()
        .clone();

    builder.ret_local = builder.body.locals.len();
    builder
        .body
        .locals
        .push(Local::new(None, LocalKind::ReturnPointer, ret_ty, None));

    for (arg, ty) in func.decl.params.iter().zip(args_ty) {
        builder
            .name_to_local
            .insert(arg.name.name.clone(), builder.body.locals.len());
        builder.body.locals.push(Local::new(
            Some(arg.name.span),
            LocalKind::Arg,
            ty,
            Some(arg.name.name.clone()),
        ));
    }

    // Get all locals
    for stmt in &func.body {
        if let statements::Statement::Let(info) = stmt {
            match &info.target {
                LetStmtTarget::Simple { name, r#type } => {
                    let ty = lower_type(&builder.ctx, r#type, builder.local_module)?;
                    builder
                        .name_to_local
                        .insert(name.name.clone(), builder.body.locals.len());
                    builder.body.locals.push(Local::new(
                        Some(name.span),
                        LocalKind::Temp,
                        ty,
                        Some(name.name.clone()),
                    ));
                }
                LetStmtTarget::Destructure(_) => todo!(),
            }
        } else if let statements::Statement::For(info) = stmt {
            if let Some(info) = &info.init {
                match &info.target {
                    LetStmtTarget::Simple { name, r#type } => {
                        let ty = lower_type(&builder.ctx, r#type, builder.local_module)?;
                        builder
                            .name_to_local
                            .insert(name.name.clone(), builder.body.locals.len());
                        builder.body.locals.push(Local::new(
                            Some(name.span),
                            LocalKind::Temp,
                            ty,
                            Some(name.name.clone()),
                        ));
                    }
                    LetStmtTarget::Destructure(_) => todo!(),
                }
            }
        }
    }

    let ret_type = func
        .decl
        .ret_type
        .as_ref()
        .map(|r| lower_type(&builder.ctx, r, builder.local_module))
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
    ctx.body.functions.insert(body.id, body);

    Ok(ctx)
}

fn lower_func_decl(
    ctx: BuildCtx,
    func: &FunctionDecl,
    module_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let builder = FnBodyBuilder {
        body: FnBody {
            basic_blocks: Vec::new(),
            locals: Vec::new(),
            is_extern: func.is_extern,
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
        ctx,
    };

    let (mut ctx, body) = (builder.ctx, builder.body);
    ctx.unresolved_function_signatures.remove(&body.id);
    ctx.body.functions.insert(body.id, body);

    Ok(ctx)
}

fn lower_statement(
    builder: &mut FnBodyBuilder,
    info: &concrete_ast::statements::Statement,
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
            lower_fn_call(builder, info)?;
        }
    }
    Ok(())
}

fn lower_while(builder: &mut FnBodyBuilder, info: &WhileStmt) -> Result<(), LoweringError> {
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
        lower_expression(builder, &info.value, None)?;

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

    for stmt in &info.contents {
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

fn lower_for(builder: &mut FnBodyBuilder, info: &ForStmt) -> Result<(), LoweringError> {
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

    for stmt in &info.contents {
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

fn lower_if_statement(builder: &mut FnBodyBuilder, info: &IfExpr) -> Result<(), LoweringError> {
    let (discriminator, discriminator_type, _disc_span) =
        lower_expression(builder, &info.value, None)?;

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

    if let Some(contents) = &info.r#else {
        for stmt in contents {
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

fn lower_let(builder: &mut FnBodyBuilder, info: &LetStmt) -> Result<(), LoweringError> {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let ty = lower_type(&builder.ctx, r#type, builder.local_module)?;
            let (rvalue, rvalue_ty, _exp_span) =
                lower_expression(builder, &info.value, Some(ty.clone()))?;

            if ty.kind != rvalue_ty.kind {
                return Err(LoweringError::UnexpectedType {
                    span: info.span,
                    found: rvalue_ty,
                    expected: ty.clone(),
                    program_id: builder.local_module.program_id,
                });
            }

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
    };
    Ok(())
}

fn lower_assign(builder: &mut FnBodyBuilder, info: &AssignStmt) -> Result<(), LoweringError> {
    let (mut place, mut ty, _path_span) = lower_path(builder, &info.target)?;

    for _ in 0..info.derefs {
        match &ty.kind {
            TyKind::Ref(inner, is_mut) | TyKind::Ptr(inner, is_mut) => {
                if matches!(is_mut, Mutability::Not) {
                    Err(LoweringError::BorrowNotMutable {
                        span: info.target.first.span,
                        name: info.target.first.name.clone(),
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

    let (rvalue, _rvalue_ty, _exp_span) = lower_expression(builder, &info.value, Some(ty.clone()))?;

    builder.statements.push(Statement {
        span: Some(info.target.first.span),
        kind: StatementKind::Assign(place, rvalue),
    });

    Ok(())
}

fn lower_return(
    builder: &mut FnBodyBuilder,
    info: &ReturnStmt,
    ret_type: Ty,
) -> Result<(), LoweringError> {
    if let Some(value_exp) = &info.value {
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

fn find_expression_type(builder: &mut FnBodyBuilder, info: &Expression) -> Option<Ty> {
    match info {
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
        Expression::UnaryOp(_, info) => find_expression_type(builder, info),
        Expression::BinaryOp(lhs, op, rhs) => {
            if matches!(op, BinaryOp::Logic(_)) {
                Some(Ty {
                    span: None,
                    kind: TyKind::Bool,
                })
            } else {
                find_expression_type(builder, lhs).or(find_expression_type(builder, rhs))
            }
        }
        Expression::Deref(_, _) => {
            todo!()
        }
        Expression::AsRef(_, _, _) => todo!(),
        Expression::StructInit(info) => {
            let id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name)
                .expect("struct not found");

            // todo: struct generics
            Some(Ty {
                span: Some(info.span),
                kind: TyKind::Struct {
                    id,
                    generics: vec![],
                },
            })
        }
        Expression::Cast(_, _, _) => todo!(),
        Expression::ArrayInit(info) => {
            let first_element = info.values.first()?;

            let first_type = find_expression_type(builder, first_element)?;

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
    }
}

fn lower_expression(
    builder: &mut FnBodyBuilder,
    info: &Expression,
    type_hint: Option<Ty>,
) -> Result<(Rvalue, Ty, Span), LoweringError> {
    Ok(match info {
        Expression::Value(info, span) => {
            let value = lower_value_expr(builder, info, type_hint)?;
            (value.0, value.1, *span)
        }
        Expression::FnCall(info) => lower_fn_call(builder, info)?,
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(lhs, op, rhs) => lower_binary_op(builder, lhs, *op, rhs, type_hint)?,
        Expression::Deref(info, deref_span) => {
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
            let type_hint = match type_hint {
                Some(inner) => match inner.kind {
                    TyKind::Ref(inner, _) => Some(*inner.clone()),
                    _ => unreachable!(),
                },
                None => None,
            };
            let (value, ty, _span) = lower_expression(builder, inner, type_hint)?;

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
            let id = *builder
                .get_module_body()
                .symbols
                .structs
                .get(&info.name.name)
                .expect("struct not found");
            let struct_body = builder.ctx.body.structs.get(&id).unwrap().clone();
            let ty = Ty {
                span: Some(info.span),
                kind: TyKind::Struct {
                    id,
                    generics: vec![],
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

            let new_ty = lower_type(&builder.ctx, cast_ty, builder.local_module)?;

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

fn lower_fn_call(
    builder: &mut FnBodyBuilder,
    info: &FnCallOp,
) -> Result<(Rvalue, Ty, Span), LoweringError> {
    let fn_id = {
        let mod_body = builder.get_module_body();

        if let Some(id) = mod_body.symbols.functions.get(&info.target.name) {
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
    let (args_ty, ret_ty) = {
        if let Some(x) = builder.ctx.body.function_signatures.get(&fn_id) {
            x.clone()
        } else {
            let (args, ret) = builder
                .ctx
                .unresolved_function_signatures
                .get(&fn_id)
                .unwrap();

            let args: Vec<Ty> = args
                .iter()
                .map(|arg| lower_type(&builder.ctx, arg, builder.local_module))
                .collect::<Result<Vec<_>, _>>()?;
            let ret = ret
                .as_ref()
                .map(|arg| lower_type(&builder.ctx, arg, builder.local_module))
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

    let mut args = Vec::new();

    for (arg, arg_ty) in info.args.iter().zip(args_ty) {
        let rvalue = lower_expression(builder, arg, Some(arg_ty.clone()))?;
        args.push(rvalue.0);
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
        let ty = find_expression_type(builder, lhs).unwrap_or_else(|| {
            find_expression_type(builder, rhs).expect(
                "couldn't find the expression type, this shouldnt happen and it's a compiler bug",
            )
        });
        lower_expression(builder, lhs, Some(ty))?
    } else {
        lower_expression(builder, lhs, type_hint.clone())?
    };
    let (rhs, rhs_ty, rhs_span) = if type_hint.is_none() {
        let ty = find_expression_type(builder, rhs).unwrap_or(lhs_ty.clone());
        lower_expression(builder, rhs, Some(ty))?
    } else {
        lower_expression(builder, rhs, type_hint.clone())?
    };

    if lhs_ty != rhs_ty {
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
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::U32((*value) as u32))),
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
                let mod_body = builder.get_module_body();

                let constant_id =
                    if let Some(constant_id) = mod_body.symbols.constants.get(&info.first.name) {
                        *constant_id
                    } else {
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

                (Rvalue::Use(Operand::Const(constant_value)), ty)
            }
        }
    })
}

pub fn lower_path(
    builder: &mut FnBodyBuilder,
    info: &PathOp,
) -> Result<(Place, Ty, Span), LoweringError> {
    let local = *builder.name_to_local.get(&info.first.name).ok_or(
        LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            program_id: builder.local_module.program_id,
        },
    )?;

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

                if let TyKind::Struct { id, generics: _ } = ty.kind {
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
        }
    }

    Ok((Place { local, projection }, ty, info.span))
}

pub fn lower_type(ctx: &BuildCtx, spec: &TypeSpec, module_id: DefId) -> Result<Ty, LoweringError> {
    Ok(match spec {
        TypeSpec::Simple {
            name,
            qualifiers,
            span,
        } => {
            let mut ty = Ty::new(span, name_to_tykind(ctx, &name.name, *span, module_id)?);

            for qual in qualifiers.iter().rev() {
                ty = match qual {
                    TypeQualifier::Ref => Ty::new(span, TyKind::Ref(Box::new(ty), Mutability::Not)),
                    TypeQualifier::RefMut => {
                        Ty::new(span, TyKind::Ref(Box::new(ty), Mutability::Mut))
                    }
                    TypeQualifier::Ptr => Ty::new(span, TyKind::Ptr(Box::new(ty), Mutability::Not)),
                    TypeQualifier::PtrMut => {
                        Ty::new(span, TyKind::Ptr(Box::new(ty), Mutability::Mut))
                    }
                }
            }
            ty
        }
        TypeSpec::Generic { span, .. } => Err(LoweringError::NotYetImplemented {
            span: *span,
            message: "Generics not yet implemented",
            program_id: module_id.program_id,
        })?,
        TypeSpec::Array {
            of_type,
            size,
            qualifiers,
            span,
        } => {
            let mut ty = Ty {
                span: Some(*span),
                kind: TyKind::Array(
                    Box::new(lower_type(ctx, of_type, module_id)?),
                    Box::new(ConstData {
                        ty: Ty {
                            span: Some(*span),
                            kind: TyKind::Uint(UintTy::U64),
                        },
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(*size))),
                    }),
                ),
            };

            for qual in qualifiers.iter().rev() {
                ty = match qual {
                    TypeQualifier::Ref => Ty::new(span, TyKind::Ref(Box::new(ty), Mutability::Not)),
                    TypeQualifier::RefMut => {
                        Ty::new(span, TyKind::Ref(Box::new(ty), Mutability::Mut))
                    }
                    TypeQualifier::Ptr => Ty::new(span, TyKind::Ptr(Box::new(ty), Mutability::Not)),
                    TypeQualifier::PtrMut => {
                        Ty::new(span, TyKind::Ptr(Box::new(ty), Mutability::Mut))
                    }
                }
            }
            ty
        }
    })
}

pub fn name_to_tykind(
    ctx: &BuildCtx,
    name: &str,
    span: Span,
    module_id: DefId,
) -> Result<TyKind, LoweringError> {
    Ok(match name {
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
        "char" => TyKind::Char,
        other => {
            let module = ctx.body.modules.get(&module_id).expect("module not found");
            if let Some(struct_id) = module.symbols.structs.get(other) {
                TyKind::Struct {
                    id: *struct_id,
                    generics: vec![],
                }
            } else {
                Err(LoweringError::UnrecognizedType {
                    span,
                    name: other.to_string(),
                    program_id: module_id.program_id,
                })?
            }
        }
    })
}
