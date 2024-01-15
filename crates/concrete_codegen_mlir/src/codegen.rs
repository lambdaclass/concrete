use std::{collections::HashMap, error::Error};

use bumpalo::Bump;
use concrete_ast::{
    common::{Ident, Span},
    expressions::{ArithOp, BinaryOp, CmpOp, Expression, IfExpr, LogicOp, PathOp, SimpleExpr},
    functions::FunctionDef,
    modules::{Module, ModuleDefItem},
    statements::{AssignStmt, LetStmt, LetStmtTarget, ReturnStmt, Statement},
    types::TypeSpec,
    Program,
};
use concrete_session::Session;
use melior::{
    dialect::{
        arith::{self, CmpiPredicate},
        cf, func, memref,
    },
    ir::{
        attribute::{FlatSymbolRefAttribute, IntegerAttribute, StringAttribute, TypeAttribute},
        r#type::{FunctionType, IntegerType, MemRefType},
        Block, BlockRef, Location, Module as MeliorModule, Operation, Region, Type, Value,
        ValueLike,
    },
    Context as MeliorContext,
};

pub fn compile_program(
    session: &Session,
    ctx: &MeliorContext,
    mlir_module: &MeliorModule,
    program: &Program,
) -> Result<(), Box<dyn Error>> {
    for module in &program.modules {
        compile_module(session, ctx, mlir_module, module)?;
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub struct LocalVar<'ctx, 'parent: 'ctx> {
    pub type_spec: TypeSpec,
    // If it's none its on a register, otherwise allocated on the stack.
    pub alloca: bool,
    pub value: Value<'ctx, 'parent>,
}

impl<'ctx, 'parent: 'ctx> LocalVar<'ctx, 'parent> {
    pub fn param(value: Value<'ctx, 'parent>, type_spec: TypeSpec) -> Self {
        Self {
            value,
            type_spec,
            alloca: false,
        }
    }

    pub fn alloca(value: Value<'ctx, 'parent>, type_spec: TypeSpec) -> Self {
        Self {
            value,
            type_spec,
            alloca: true,
        }
    }
}

#[derive(Debug, Clone)]
struct ScopeContext<'c, 'parent: 'c> {
    pub locals: HashMap<String, LocalVar<'c, 'parent>>,
    pub functions: HashMap<String, FunctionDef>,
}

struct BlockHelper<'ctx, 'region: 'ctx> {
    region: &'region Region<'ctx>,
    blocks_arena: &'region Bump,
}

impl<'ctx, 'region> BlockHelper<'ctx, 'region> {
    pub fn append_block(&self, block: Block<'ctx>) -> &'region BlockRef<'ctx, 'region> {
        let block = self.region.append_block(block);

        let block_ref: &'region mut BlockRef<'ctx, 'region> = self.blocks_arena.alloc(block);

        block_ref
    }
}

impl<'ctx, 'parent> ScopeContext<'ctx, 'parent> {
    fn resolve_type(
        &self,
        context: &'ctx MeliorContext,
        name: &str,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        Ok(match name {
            "u64" | "i64" => IntegerType::new(context, 64).into(),
            "u32" | "i32" => IntegerType::new(context, 32).into(),
            "u16" | "i16" => IntegerType::new(context, 16).into(),
            "u8" | "i8" => IntegerType::new(context, 8).into(),
            "f32" => Type::float32(context),
            "f64" => Type::float64(context),
            "bool" => IntegerType::new(context, 1).into(),
            _ => todo!("custom type lookup"),
        })
    }

    fn resolve_type_spec(
        &self,
        context: &'ctx MeliorContext,
        spec: &TypeSpec,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        Ok(match spec {
            TypeSpec::Simple { name } => self.resolve_type(context, &name.name)?,
            TypeSpec::Generic {
                name,
                type_params: _,
            } => self.resolve_type(context, &name.name)?,
        })
    }
}

fn compile_module(
    session: &Session,
    context: &MeliorContext,
    mlir_module: &MeliorModule,
    module: &Module,
) -> Result<(), Box<dyn Error>> {
    // todo: handle imports

    let body = mlir_module.body();

    let mut scope_ctx: ScopeContext = ScopeContext {
        functions: Default::default(),
        locals: Default::default(),
    };

    // save all function signatures
    for statement in &module.contents {
        if let ModuleDefItem::Function(info) = statement {
            scope_ctx
                .functions
                .insert(info.decl.name.name.clone(), info.clone());
        }
    }

    for statement in &module.contents {
        match statement {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(info) => {
                let op = compile_function_def(session, context, &scope_ctx, info)?;
                body.append_operation(op);
            }
            ModuleDefItem::Record(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
        }
    }

    Ok(())
}

fn get_location<'ctx>(
    context: &'ctx MeliorContext,
    session: &Session,
    offset: usize,
) -> Location<'ctx> {
    let (_, line, col) = session.source.get_offset_line(offset).unwrap();
    Location::new(
        context,
        &session.file_path.display().to_string(),
        line + 1,
        col + 1,
    )
}

fn get_named_location<'ctx>(context: &'ctx MeliorContext, name: &str) -> Location<'ctx> {
    Location::name(context, name, Location::unknown(context))
}

fn compile_function_def<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &ScopeContext<'ctx, 'parent>,
    info: &FunctionDef,
) -> Result<Operation<'ctx>, Box<dyn Error>> {
    tracing::debug!("compiling function {:?}", info.decl.name.name);
    let location = get_location(context, session, info.decl.name.span.from);

    // Setup function arguments
    let mut args = Vec::with_capacity(info.decl.params.len());
    let mut fn_args_types = Vec::with_capacity(info.decl.params.len());

    for param in &info.decl.params {
        let param_type = scope_ctx.resolve_type_spec(context, &param.r#type)?;
        let loc = get_location(context, session, param.name.span.from);
        args.push((param_type, loc));
        fn_args_types.push(param_type);
    }

    // Create the function context
    let region = Region::new();

    let return_type = if let Some(ret_type) = &info.decl.ret_type {
        vec![scope_ctx.resolve_type_spec(context, ret_type)?]
    } else {
        vec![]
    };

    let func_type =
        TypeAttribute::new(FunctionType::new(context, &fn_args_types, &return_type).into());

    {
        let mut scope_ctx = scope_ctx.clone();
        let fn_block = &region.append_block(Block::new(&args));

        let blocks_arena = Bump::new();
        let helper = BlockHelper {
            region: &region,
            blocks_arena: &blocks_arena,
        };

        // Push arguments into locals
        for (i, param) in info.decl.params.iter().enumerate() {
            scope_ctx.locals.insert(
                param.name.name.clone(),
                LocalVar::param(fn_block.argument(i)?.into(), param.r#type.clone()),
            );
        }

        let mut fn_block = Some(fn_block);

        for stmt in &info.body {
            if let Some(block) = fn_block {
                match stmt {
                    Statement::Assign(info) => {
                        compile_assign_stmt(session, context, &mut scope_ctx, &helper, block, info)?
                    }
                    Statement::Match(_) => todo!(),
                    Statement::For(_) => todo!(),
                    Statement::If(info) => {
                        fn_block = compile_if_expr(
                            session,
                            context,
                            &mut scope_ctx,
                            &helper,
                            block,
                            info,
                        )?;
                    }
                    Statement::Let(info) => {
                        compile_let_stmt(session, context, &mut scope_ctx, &helper, block, info)?
                    }
                    Statement::Return(info) => {
                        compile_return_stmt(session, context, &mut scope_ctx, &helper, block, info)?
                    }
                    Statement::While(_) => todo!(),
                    Statement::FnCall(_) => todo!(),
                }
            }
        }
    }

    Ok(func::func(
        context,
        StringAttribute::new(context, &info.decl.name.name),
        func_type,
        region,
        &[],
        location,
    ))
}

fn compile_if_expr<'c, 'this: 'c>(
    session: &Session,
    context: &'c MeliorContext,
    scope_ctx: &mut ScopeContext<'c, 'this>,
    helper: &BlockHelper<'c, 'this>,
    block: &'this Block<'c>,
    info: &IfExpr,
) -> Result<Option<&'this BlockRef<'c, 'this>>, Box<dyn Error>> {
    let condition = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.value,
        Some(&TypeSpec::Simple {
            name: Ident {
                name: "bool".to_string(),
                span: Span::new(0, 0),
            },
        }),
    )?;

    let then_successor = helper.append_block(Block::new(&[]));
    let else_successor = helper.append_block(Block::new(&[]));

    block.append_operation(cf::cond_br(
        context,
        condition,
        then_successor,
        else_successor,
        &[],
        &[],
        get_named_location(context, "if"),
    ));

    let mut then_successor = Some(then_successor);
    let mut else_successor = Some(else_successor);

    {
        let mut then_scope_ctx = scope_ctx.clone();
        for stmt in &info.contents {
            if let Some(then_successor_block) = then_successor {
                match stmt {
                    Statement::Assign(info) => compile_assign_stmt(
                        session,
                        context,
                        &mut then_scope_ctx,
                        helper,
                        then_successor_block,
                        info,
                    )?,
                    Statement::Match(_) => todo!(),
                    Statement::For(_) => todo!(),
                    Statement::If(info) => {
                        then_successor = compile_if_expr(
                            session,
                            context,
                            &mut then_scope_ctx,
                            helper,
                            then_successor_block,
                            info,
                        )?;
                    }
                    Statement::Let(info) => compile_let_stmt(
                        session,
                        context,
                        &mut then_scope_ctx,
                        helper,
                        then_successor_block,
                        info,
                    )?,
                    Statement::Return(info) => compile_return_stmt(
                        session,
                        context,
                        &mut then_scope_ctx,
                        helper,
                        then_successor_block,
                        info,
                    )?,
                    Statement::While(_) => todo!(),
                    Statement::FnCall(_) => todo!(),
                }
            }
        }
    }

    if let Some(else_contents) = info.r#else.as_ref() {
        let mut else_scope_ctx = scope_ctx.clone();
        for stmt in else_contents {
            if let Some(else_successor_block) = else_successor {
                match stmt {
                    Statement::Assign(info) => compile_assign_stmt(
                        session,
                        context,
                        &mut else_scope_ctx,
                        helper,
                        else_successor_block,
                        info,
                    )?,
                    Statement::Match(_) => todo!(),
                    Statement::For(_) => todo!(),
                    Statement::If(info) => {
                        else_successor = compile_if_expr(
                            session,
                            context,
                            &mut else_scope_ctx,
                            helper,
                            else_successor_block,
                            info,
                        )?;
                    }
                    Statement::Let(info) => compile_let_stmt(
                        session,
                        context,
                        &mut else_scope_ctx,
                        helper,
                        else_successor_block,
                        info,
                    )?,
                    Statement::Return(info) => compile_return_stmt(
                        session,
                        context,
                        &mut else_scope_ctx,
                        helper,
                        else_successor_block,
                        info,
                    )?,
                    Statement::While(_) => todo!(),
                    Statement::FnCall(_) => todo!(),
                }
            }
        }
    }

    Ok(match (then_successor, else_successor) {
        (None, None) => None,
        (None, Some(else_successor)) => {
            if else_successor.terminator().is_some() {
                None
            } else {
                let final_block = helper.append_block(Block::new(&[]));
                else_successor.append_operation(cf::br(
                    final_block,
                    &[],
                    Location::unknown(context),
                ));
                Some(final_block)
            }
        }
        (Some(then_successor), None) => {
            if then_successor.terminator().is_some() {
                None
            } else {
                let final_block = helper.append_block(Block::new(&[]));
                then_successor.append_operation(cf::br(
                    final_block,
                    &[],
                    Location::unknown(context),
                ));
                Some(final_block)
            }
        }
        (Some(then_successor), Some(else_successor)) => {
            if then_successor.terminator().is_some() && else_successor.terminator().is_some() {
                None
            } else {
                let final_block = helper.append_block(Block::new(&[]));
                if then_successor.terminator().is_none() {
                    then_successor.append_operation(cf::br(
                        final_block,
                        &[],
                        Location::unknown(context),
                    ));
                }

                if else_successor.terminator().is_none() {
                    else_successor.append_operation(cf::br(
                        final_block,
                        &[],
                        Location::unknown(context),
                    ));
                }

                Some(final_block)
            }
        }
    })
}

fn compile_let_stmt<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    info: &LetStmt,
) -> Result<(), Box<dyn Error>> {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let value = compile_expression(
                session,
                context,
                scope_ctx,
                helper,
                block,
                &info.value,
                Some(r#type),
            )?;

            let location = get_location(context, session, name.span.from);

            let memref_type = MemRefType::new(value.r#type(), &[1], None, None);

            let alloca: Value = block
                .append_operation(memref::alloca(
                    context,
                    memref_type,
                    &[],
                    &[],
                    None,
                    location,
                ))
                .result(0)?
                .into();
            let k0 = block
                .append_operation(arith::constant(
                    context,
                    IntegerAttribute::new(0, Type::index(context)).into(),
                    location,
                ))
                .result(0)?
                .into();
            block.append_operation(memref::store(value, alloca, &[k0], location));

            scope_ctx
                .locals
                .insert(name.name.clone(), LocalVar::alloca(alloca, r#type.clone()));

            Ok(())
        }
        LetStmtTarget::Destructure(_) => todo!(),
    }
}

fn compile_assign_stmt<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    info: &AssignStmt,
) -> Result<(), Box<dyn Error>> {
    // todo: implement properly for structs, right now only really works for simple variables.

    let local = scope_ctx
        .locals
        .get(&info.target.first.name)
        .expect("local should exist")
        .clone();

    assert!(local.alloca, "can only mutate local stack variables");

    let location = get_location(context, session, info.target.first.span.from);

    let value = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.value,
        Some(&local.type_spec),
    )?;

    let k0 = block
        .append_operation(arith::constant(
            context,
            IntegerAttribute::new(0, Type::index(context)).into(),
            location,
        ))
        .result(0)?
        .into();
    block.append_operation(memref::store(value, local.value, &[k0], location));

    Ok(())
}

fn compile_return_stmt<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    info: &ReturnStmt,
) -> Result<(), Box<dyn Error>> {
    let value = compile_expression(
        session,
        context,
        scope_ctx,
        helper,
        block,
        &info.value,
        None,
    )?;
    block.append_operation(func::r#return(&[value], Location::unknown(context)));
    Ok(())
}

fn compile_expression<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    _helper: &BlockHelper<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    info: &Expression,
    type_info: Option<&TypeSpec>,
) -> Result<Value<'ctx, 'parent>, Box<dyn Error>> {
    let location = Location::unknown(context);
    match info {
        Expression::Simple(simple) => match simple {
            SimpleExpr::ConstBool(value) => {
                let value =
                    IntegerAttribute::new((*value).into(), IntegerType::new(context, 1).into());
                Ok(block
                    .append_operation(arith::constant(context, value.into(), location))
                    .result(0)?
                    .into())
            }
            SimpleExpr::ConstChar(value) => {
                let value =
                    IntegerAttribute::new((*value) as i64, IntegerType::new(context, 32).into());
                Ok(block
                    .append_operation(arith::constant(context, value.into(), location))
                    .result(0)?
                    .into())
            }
            SimpleExpr::ConstInt(value) => {
                let int_type = if let Some(type_info) = type_info {
                    scope_ctx.resolve_type_spec(context, type_info)?
                } else {
                    IntegerType::new(context, 64).into()
                };
                let value = IntegerAttribute::new((*value) as i64, int_type);
                Ok(block
                    .append_operation(arith::constant(context, value.into(), location))
                    .result(0)?
                    .into())
            }
            SimpleExpr::ConstFloat(_) => todo!(),
            SimpleExpr::ConstStr(_) => todo!(),
            SimpleExpr::Path(value) => compile_path_op(session, context, scope_ctx, block, value),
        },
        Expression::FnCall(value) => {
            let mut args = Vec::with_capacity(value.args.len());
            let location = get_location(context, session, value.target.span.from);

            let target_fn = scope_ctx
                .functions
                .get(&value.target.name)
                .expect("function not found")
                .clone();

            assert_eq!(
                value.args.len(),
                target_fn.decl.params.len(),
                "parameter length doesnt match"
            );

            for (arg, arg_info) in value.args.iter().zip(&target_fn.decl.params) {
                let value = compile_expression(
                    session,
                    context,
                    scope_ctx,
                    _helper,
                    block,
                    arg,
                    Some(&arg_info.r#type),
                )?;
                args.push(value);
            }

            let return_type = if let Some(ret_type) = &target_fn.decl.ret_type {
                vec![scope_ctx.resolve_type_spec(context, ret_type)?]
            } else {
                vec![]
            };

            Ok(block
                .append_operation(func::call(
                    context,
                    FlatSymbolRefAttribute::new(context, &value.target.name),
                    &args,
                    &return_type,
                    location,
                ))
                .result(0)?
                .into())
        }
        Expression::Match(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::UnaryOp(_, _) => todo!(),
        Expression::BinaryOp(lhs, op, rhs) => {
            let lhs =
                compile_expression(session, context, scope_ctx, _helper, block, lhs, type_info)?;
            let rhs =
                compile_expression(session, context, scope_ctx, _helper, block, rhs, type_info)?;

            let op = match op {
                // todo: check signedness
                BinaryOp::Arith(arith_op) => match arith_op {
                    ArithOp::Add => arith::addi(lhs, rhs, location),
                    ArithOp::Sub => arith::subi(lhs, rhs, location),
                    ArithOp::Mul => arith::muli(lhs, rhs, location),
                    ArithOp::Div => arith::divsi(lhs, rhs, location),
                    ArithOp::Mod => arith::remsi(lhs, rhs, location),
                },
                BinaryOp::Logic(logic_op) => match logic_op {
                    LogicOp::And => {
                        let const_true = block
                            .append_operation(arith::constant(
                                context,
                                IntegerAttribute::new(1, IntegerType::new(context, 1).into())
                                    .into(),
                                location,
                            ))
                            .result(0)?
                            .into();
                        let lhs_bool = block
                            .append_operation(arith::cmpi(
                                context,
                                CmpiPredicate::Eq,
                                lhs,
                                const_true,
                                location,
                            ))
                            .result(0)?
                            .into();
                        let rhs_bool = block
                            .append_operation(arith::cmpi(
                                context,
                                CmpiPredicate::Eq,
                                rhs,
                                const_true,
                                location,
                            ))
                            .result(0)?
                            .into();
                        arith::andi(lhs_bool, rhs_bool, location)
                    }
                    LogicOp::Or => {
                        let const_true = block
                            .append_operation(arith::constant(
                                context,
                                IntegerAttribute::new(1, IntegerType::new(context, 1).into())
                                    .into(),
                                location,
                            ))
                            .result(0)?
                            .into();
                        let lhs_bool = block
                            .append_operation(arith::cmpi(
                                context,
                                CmpiPredicate::Eq,
                                lhs,
                                const_true,
                                location,
                            ))
                            .result(0)?
                            .into();
                        let rhs_bool = block
                            .append_operation(arith::cmpi(
                                context,
                                CmpiPredicate::Eq,
                                rhs,
                                const_true,
                                location,
                            ))
                            .result(0)?
                            .into();
                        arith::ori(lhs_bool, rhs_bool, location)
                    }
                },
                BinaryOp::Compare(cmp_op) => match cmp_op {
                    CmpOp::Eq => arith::cmpi(context, CmpiPredicate::Eq, lhs, rhs, location),
                    CmpOp::NotEq => arith::cmpi(context, CmpiPredicate::Ne, lhs, rhs, location),
                    CmpOp::Lt => arith::cmpi(context, CmpiPredicate::Slt, lhs, rhs, location),
                    CmpOp::LtEq => arith::cmpi(context, CmpiPredicate::Sle, lhs, rhs, location),
                    CmpOp::Gt => arith::cmpi(context, CmpiPredicate::Sgt, lhs, rhs, location),
                    CmpOp::GtEq => arith::cmpi(context, CmpiPredicate::Sge, lhs, rhs, location),
                },
                BinaryOp::Bitwise(_) => todo!(),
            };

            let value = block.append_operation(op).result(0)?.into();

            Ok(value)
        }
    }
}

fn compile_path_op<'ctx, 'parent: 'ctx>(
    session: &Session,
    context: &'ctx MeliorContext,
    scope_ctx: &mut ScopeContext<'ctx, 'parent>,
    block: &'parent Block<'ctx>,
    path: &PathOp,
) -> Result<Value<'ctx, 'parent>, Box<dyn Error>> {
    // For now only simple variables work.
    // TODO: implement properly, this requires having structs implemented.

    let local = scope_ctx
        .locals
        .get(&path.first.name)
        .expect("local not found");

    let location = get_location(context, session, path.first.span.from);

    if local.alloca {
        let k0 = block
            .append_operation(arith::constant(
                context,
                IntegerAttribute::new(0, Type::index(context)).into(),
                location,
            ))
            .result(0)?
            .into();
        let value = block
            .append_operation(memref::load(local.value, &[k0], location))
            .result(0)?
            .into();
        Ok(value)
    } else {
        Ok(local.value)
    }
}
