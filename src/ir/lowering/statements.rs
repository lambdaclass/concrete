use tracing::{debug, instrument};

use crate::{
    ast::{
        expressions::IfExpr,
        statements::{self, AssignStmt, ForStmt, LetStmt, LetStmtTarget, ReturnStmt, WhileStmt},
    },
    ir::{
        lowering::{expressions::lower_expression, functions::get_locals, types::lower_type},
        BasicBlock, ConstData, ConstKind, ConstValue, Mutability, Operand, Place, PlaceElem,
        Rvalue, Statement, StatementKind, SwitchTargets, Terminator, TerminatorKind, Type,
        ValueTree,
    },
};

use super::{
    errors::LoweringError, expressions::lower_path, functions::lower_fn_call, ir::TypeIndex,
    FnIrBuilder,
};

pub(crate) fn lower_statement(
    builder: &mut FnIrBuilder,
    info: &crate::ast::statements::Statement,
    ret_type: TypeIndex,
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

#[instrument(level = "debug", skip_all, fields(name, variant))]
fn lower_let(builder: &mut FnIrBuilder, info: &LetStmt) -> Result<(), LoweringError> {
    debug!("begin lowering let");
    match &info.target {
        LetStmtTarget::Simple { id: name, r#type } => {
            tracing::Span::current().record("name", &name.name);
            tracing::Span::current().record("variant", "simple");

            let type_idx = lower_type(builder.builder, r#type)?;

            let ty = builder.builder.ir.types[type_idx].clone().unwrap();

            debug!(
                "let target type: {}",
                ty.display(&builder.builder.ir).unwrap()
            );
            let (rvalue, rvalue_type_idx, rvalue_span) =
                lower_expression(builder, &info.value, Some(type_idx))?;

            let rvalue_ty = builder.builder.ir.types[rvalue_type_idx].clone().unwrap();
            debug!(
                "let rvalue type: {}",
                rvalue_ty.display(&builder.builder.ir).unwrap()
            );

            if !ty.is_equal(&rvalue_ty, &builder.builder.ir) {
                return Err(LoweringError::UnexpectedType {
                    found_span: rvalue_span,
                    found: rvalue_ty.display(&builder.builder.ir).unwrap(),
                    expected: ty.display(&builder.builder.ir).unwrap(),
                    expected_span: Some(r#type.get_span()),
                    path: builder.get_file_path().clone(),
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
fn lower_assign(builder: &mut FnIrBuilder, info: &AssignStmt) -> Result<(), LoweringError> {
    debug!("begin lowering assign");
    let (mut place, mut type_idx, path_span) = lower_path(builder, &info.lvalue)?;

    if !builder.body.locals[place.local].is_mutable(&builder.builder.ir.types) {
        return Err(LoweringError::NotMutable {
            span: info.span,
            declare_span: builder.body.locals[place.local].span,
            path: builder.get_file_path().clone(),
        });
    }

    let mut ty = builder.builder.get_type(type_idx).clone();

    for _ in 0..info.derefs {
        match &ty {
            Type::Ref(inner, is_mut) | Type::Ptr(inner, is_mut) => {
                if matches!(is_mut, Mutability::Not) {
                    Err(LoweringError::BorrowNotMutable {
                        span: info.lvalue.first.span,
                        name: info.lvalue.first.name.clone(),
                        type_span: Some(info.span),
                        path: builder.get_file_path().clone(),
                    })?;
                }
                type_idx = *inner;
                ty = builder.builder.get_type(type_idx).clone();
            }
            _ => unreachable!(),
        }
        place.projection.push(PlaceElem::Deref);
    }

    let (rvalue, rvalue_type_idx, rvalue_span) =
        lower_expression(builder, &info.rvalue, Some(type_idx))?;

    let rvalue_ty = builder.builder.get_type(rvalue_type_idx).clone();

    if !ty.is_equal(&rvalue_ty, &builder.builder.ir) {
        return Err(LoweringError::UnexpectedType {
            found_span: rvalue_span,
            found: rvalue_ty.display(&builder.builder.ir).unwrap(),
            expected: ty.display(&builder.builder.ir).unwrap(),
            expected_span: Some(path_span),
            path: builder.get_file_path().clone(),
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
    builder: &mut FnIrBuilder,
    info: &ReturnStmt,
    ret_type: TypeIndex,
) -> Result<(), LoweringError> {
    debug!("begin lowering return");
    if let Some(value_exp) = &info.value {
        debug!("return has value");
        let (value, value_type_idx, exp_span) =
            lower_expression(builder, value_exp, Some(ret_type))?;

        let ret_ty = builder.builder.get_type(ret_type);
        let value_ty = builder.builder.get_type(value_type_idx);

        if !ret_ty.is_equal(value_ty, &builder.builder.ir) {
            return Err(LoweringError::UnexpectedType {
                found_span: exp_span,
                found: value_ty.display(&builder.builder.ir).unwrap(),
                expected: ret_ty.display(&builder.builder.ir).unwrap(),
                expected_span: Some(info.span), // todo: change this span to point to the "-> x" return type in fn
                path: builder.get_file_path().clone(),
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
fn lower_if_statement(builder: &mut FnIrBuilder, info: &IfExpr) -> Result<(), LoweringError> {
    debug!("begin lowering if");
    let (discriminator, discriminator_type_idx, _disc_span) =
        lower_expression(builder, &info.cond, None)?;

    let local = builder.add_temp_local(builder.builder.ir.get_bool_ty());
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

    let outer_scope_locals = builder.name_to_local.clone();

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
        lower_statement(builder, stmt, builder.body.locals[builder.ret_local].ty)?;
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

    builder.name_to_local = outer_scope_locals.clone();

    if let Some(contents) = &info.else_stmts {
        for stmt in contents {
            get_locals(builder, stmt)?;
            lower_statement(builder, stmt, builder.body.locals[builder.ret_local].ty)?;
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

    builder.name_to_local = outer_scope_locals;

    let discriminator_type = builder.builder.get_type(discriminator_type_idx);

    let targets = SwitchTargets {
        values: vec![discriminator_type.get_falsy_value()],
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
fn lower_while(builder: &mut FnIrBuilder, info: &WhileStmt) -> Result<(), LoweringError> {
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

    let (discriminator, discriminator_type_idx, _disc_span) =
        lower_expression(builder, &info.condition, None)?;

    let local = builder.add_temp_local(builder.builder.ir.get_bool_ty());
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

    let outer_scope_locals = builder.name_to_local.clone();

    for stmt in &info.block_stmts {
        get_locals(builder, stmt)?;
        lower_statement(builder, stmt, builder.body.locals[builder.ret_local].ty)?;
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

    let discriminator_type = builder.builder.get_type(discriminator_type_idx);
    let targets = SwitchTargets {
        values: vec![discriminator_type.get_falsy_value()],
        targets: vec![otherwise_block_idx, first_then_block_idx],
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[check_block_idx].terminator.kind = kind;
    builder.name_to_local = outer_scope_locals;

    Ok(())
}

#[instrument(level = "debug", skip_all)]
fn lower_for(builder: &mut FnIrBuilder, info: &ForStmt) -> Result<(), LoweringError> {
    debug!("lowering for");

    let outer_scope_locals = builder.name_to_local.clone();

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

    let (discriminator, discriminator_type_idx, _disc_span) = if let Some(condition) =
        &info.condition
    {
        let (discriminator, discriminator_type, span) = lower_expression(builder, condition, None)?;

        (discriminator, discriminator_type, Some(span))
    } else {
        // todo: don't use discriminator when no loop condition
        let discriminator_type_idx = builder.builder.ir.get_bool_ty();

        let discriminator = Rvalue::Use(Operand::Const(ConstData {
            ty: discriminator_type_idx,
            span: info.span,
            data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(true))),
        }));

        (discriminator, discriminator_type_idx, None)
    };

    let local = builder.add_temp_local(builder.builder.ir.get_bool_ty());
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
        lower_statement(builder, stmt, builder.body.locals[builder.ret_local].ty)?;
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

    let discriminator_type = builder.builder.get_type(discriminator_type_idx);

    let targets = SwitchTargets {
        values: vec![discriminator_type.get_falsy_value()],
        targets: vec![otherwise_block_idx, first_then_block_idx],
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[check_block_idx].terminator.kind = kind;
    builder.name_to_local = outer_scope_locals;

    Ok(())
}
