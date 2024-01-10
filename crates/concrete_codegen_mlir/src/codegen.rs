use std::{collections::HashMap, error::Error};

use concrete_ast::{
    common::Span,
    expressions::{ArithOp, BinaryOp, CmpOp, Expression, LogicOp, PathOp, SimpleExpr},
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
        func, memref,
    },
    ir::{
        attribute::{FlatSymbolRefAttribute, IntegerAttribute, StringAttribute, TypeAttribute},
        r#type::{FunctionType, IntegerType, MemRefType},
        Block, Location, Module as MeliorModule, Region, Type, Value, ValueLike,
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
pub struct LocalVar<'c, 'op> {
    pub type_spec: TypeSpec,
    // If it's none its on a register, otherwise allocated on the stack.
    pub memref_type: Option<MemRefType<'c>>,
    pub value: Value<'c, 'op>,
}

#[derive(Debug, Clone, Default)]
struct CompilerContext<'c, 'op> {
    pub locals: HashMap<String, LocalVar<'c, 'op>>,
    pub functions: HashMap<String, FunctionDef>,
}

impl<'c, 'op> CompilerContext<'c, 'op> {
    fn resolve_type(
        &self,
        context: &'c MeliorContext,
        name: &str,
    ) -> Result<Type<'c>, Box<dyn Error>> {
        Ok(match name {
            "u64" | "i64" => IntegerType::new(context, 64).into(),
            "u32" | "i32" => IntegerType::new(context, 32).into(),
            "u16" | "i16" => IntegerType::new(context, 16).into(),
            "u8" | "i8" => IntegerType::new(context, 8).into(),
            "bool" => IntegerType::new(context, 1).into(),
            _ => todo!("custom type lookup"),
        })
    }

    fn resolve_type_spec(
        &self,
        context: &'c MeliorContext,
        spec: &TypeSpec,
    ) -> Result<Type<'c>, Box<dyn Error>> {
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

    let mut compiler_ctx: CompilerContext = Default::default();

    let body = mlir_module.body();

    // save all function signatures
    for statement in &module.contents {
        if let ModuleDefItem::Function(info) = statement {
            compiler_ctx
                .functions
                .insert(info.decl.name.name.clone(), info.clone());
        }
    }

    for statement in &module.contents {
        match statement {
            ModuleDefItem::Constant(_) => todo!(),
            ModuleDefItem::Function(info) => {
                compile_function_def(session, context, &mut compiler_ctx, &body, info)?;
            }
            ModuleDefItem::Record(_) => todo!(),
            ModuleDefItem::Type(_) => todo!(),
        }
    }

    Ok(())
}

fn get_location<'c>(context: &'c MeliorContext, session: &Session, span: &Span) -> Location<'c> {
    let (line, col) = session.get_line_and_column(span.from);
    Location::new(context, &session.file_path.display().to_string(), line, col)
}

fn compile_function_def<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    info: &FunctionDef,
) -> Result<(), Box<dyn Error>> {
    let region = Region::new();

    let location = get_location(context, session, &info.decl.name.span);

    // Setup function arguments
    let mut args = Vec::with_capacity(info.decl.params.len());
    let mut fn_args_types = Vec::with_capacity(info.decl.params.len());

    for param in &info.decl.params {
        let param_type = compiler_ctx.resolve_type_spec(context, &param.r#type)?;
        let loc = get_location(context, session, &param.name.span);
        args.push((param_type, loc));
        fn_args_types.push(param_type);
    }

    let fn_block = Block::new(&args);

    // Create the function context
    let mut fn_compiler_ctx = compiler_ctx.clone();

    // Push arguments into locals
    for (i, param) in info.decl.params.iter().enumerate() {
        fn_compiler_ctx.locals.insert(
            param.name.name.clone(),
            LocalVar {
                type_spec: param.r#type.clone(),
                value: fn_block.argument(i)?.into(),
                memref_type: None,
            },
        );
    }

    let return_type = if let Some(ret_type) = &info.decl.ret_type {
        vec![fn_compiler_ctx.resolve_type_spec(context, ret_type)?]
    } else {
        vec![]
    };

    let func_type =
        TypeAttribute::new(FunctionType::new(context, &fn_args_types, &return_type).into());

    for stmt in &info.body {
        match stmt {
            Statement::Assign(info) => {
                compile_assign_stmt(session, context, &mut fn_compiler_ctx, &fn_block, info)?
            }
            Statement::Match(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Let(info) => {
                compile_let_stmt(session, context, &mut fn_compiler_ctx, &fn_block, info)?
            }
            Statement::Return(info) => {
                compile_return_stmt(session, context, &mut fn_compiler_ctx, &fn_block, info)?
            }
            Statement::While(_) => todo!(),
            Statement::FnCall(_) => todo!(),
        }
    }

    region.append_block(fn_block);

    block.append_operation(func::func(
        context,
        StringAttribute::new(context, &info.decl.name.name),
        func_type,
        region,
        &[],
        location,
    ));

    Ok(())
}

fn compile_let_stmt<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    info: &LetStmt,
) -> Result<(), Box<dyn Error>> {
    match &info.target {
        LetStmtTarget::Simple { name, r#type } => {
            let value = compile_expression(
                session,
                context,
                compiler_ctx,
                block,
                &info.value,
                Some(r#type),
            )?;

            let location = get_location(context, session, &name.span);

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

            compiler_ctx.locals.insert(
                name.name.clone(),
                LocalVar {
                    type_spec: r#type.clone(),
                    memref_type: Some(memref_type),
                    value: alloca,
                },
            );

            Ok(())
        }
        LetStmtTarget::Destructure(_) => todo!(),
    }
}

fn compile_assign_stmt<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    info: &AssignStmt,
) -> Result<(), Box<dyn Error>> {
    // todo: implement properly for structs, right now only really works for simple variables.

    let local = compiler_ctx
        .locals
        .get(&info.target.first.name)
        .expect("local should exist")
        .clone();

    assert!(
        local.memref_type.is_some(),
        "can only mutate local stack variables"
    );

    let location = get_location(context, session, &info.target.first.span);

    let value = compile_expression(
        session,
        context,
        compiler_ctx,
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

fn compile_return_stmt<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    info: &ReturnStmt,
) -> Result<(), Box<dyn Error>> {
    let value = compile_expression(session, context, compiler_ctx, block, &info.value, None)?;
    block.append_operation(func::r#return(&[value], Location::unknown(context)));
    Ok(())
}

fn compile_expression<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    info: &Expression,
    type_info: Option<&TypeSpec>,
) -> Result<Value<'c, 'op>, Box<dyn Error>> {
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
                    compiler_ctx.resolve_type_spec(context, type_info)?
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
            SimpleExpr::Path(value) => {
                compile_path_op(session, context, compiler_ctx, block, value)
            }
        },
        Expression::FnCall(value) => {
            let mut args = Vec::with_capacity(value.args.len());
            let location = get_location(context, session, &value.target.span);

            let target_fn = compiler_ctx
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
                    compiler_ctx,
                    block,
                    arg,
                    Some(&arg_info.r#type),
                )?;
                args.push(value);
            }

            let return_type = if let Some(ret_type) = &target_fn.decl.ret_type {
                vec![compiler_ctx.resolve_type_spec(context, ret_type)?]
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
            let lhs = compile_expression(session, context, compiler_ctx, block, lhs, type_info)?;
            let rhs = compile_expression(session, context, compiler_ctx, block, rhs, type_info)?;

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

fn compile_path_op<'c, 'op>(
    session: &Session,
    context: &'c MeliorContext,
    compiler_ctx: &mut CompilerContext<'c, 'op>,
    block: &'op Block<'c>,
    path: &PathOp,
) -> Result<Value<'c, 'op>, Box<dyn Error>> {
    // For now only simple variables work.
    // TODO: implement properly, this requires having structs implemented.

    let local = compiler_ctx
        .locals
        .get(&path.first.name)
        .expect("local not found");

    let location = get_location(context, session, &path.first.span);

    if let Some(_memref_type) = local.memref_type {
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
