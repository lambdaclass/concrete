use tracing::{debug, instrument};

use crate::{
    ast::{
        common::TypeName,
        expressions::{IfExpr, MatchCaseExpr, MatchExpr, ValueExpr},
        statements::{self, AssignStmt, ForStmt, LetStmt, LetStmtTarget, ReturnStmt, WhileStmt},
        types::TypeDescriptor,
    },
    ir::{
        AdtKind, BasicBlock, ConstData, ConstKind, ConstValue, Local, LocalKind, Mutability,
        Operand, Place, PlaceElem, Rvalue, Statement, StatementKind, SwitchTargets, Terminator,
        TerminatorKind, Type, ValueTree,
        lowering::{
            Symbol, expressions::lower_expression, functions::get_locals, types::lower_type,
        },
    },
};

use super::{
    FnIrBuilder, errors::LoweringError, expressions::lower_path, functions::lower_fn_call,
    ir::TypeIndex,
};

pub(crate) fn lower_statement(
    builder: &mut FnIrBuilder,
    info: &crate::ast::statements::Statement,
    ret_type: TypeIndex,
) -> Result<(), LoweringError> {
    match info {
        statements::Statement::Assign(info) => lower_assign(builder, info)?,
        statements::Statement::Match(info) => lower_match(builder, info)?,
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
                builder.builder.display_typename(type_idx)
            );
            let (rvalue, rvalue_type_idx, rvalue_span) =
                lower_expression(builder, &info.value, Some(type_idx))?;

            let rvalue_ty = builder.builder.ir.types[rvalue_type_idx].clone().unwrap();
            debug!(
                "let rvalue type: {}",
                builder.builder.display_typename(rvalue_type_idx)
            );

            if !ty.is_equal(&rvalue_ty, &builder.builder.ir) {
                return Err(LoweringError::UnexpectedType {
                    found_span: rvalue_span,
                    found: builder.builder.display_typename(rvalue_type_idx),
                    expected: builder.builder.display_typename(type_idx),
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
            found: builder.builder.display_typename(rvalue_type_idx),
            expected: builder.builder.display_typename(type_idx),
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
                found: builder.builder.display_typename(value_type_idx),
                expected: builder.builder.display_typename(ret_type),
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
fn lower_match(builder: &mut FnIrBuilder, info: &MatchExpr) -> Result<(), LoweringError> {
    debug!("begin lowering match");

    let (mut discriminator, mut discriminator_type_idx, disc_span) =
        lower_expression(builder, &info.expr, None)?;

    debug!(
        "Discriminator type: {}",
        builder.builder.display_typename(discriminator_type_idx)
    );

    let mut enum_place_ty = None;

    let mut discriminator_type = builder.builder.get_type(discriminator_type_idx);
    let mut discriminator_place = discriminator.get_place().unwrap();

    // auto deref
    while let Type::Ref(inner, _) = discriminator_type {
        discriminator_place.projection.push(PlaceElem::Deref);
        discriminator_type_idx = *inner;
        discriminator_type = builder.builder.get_type(discriminator_type_idx);
    }

    discriminator = Rvalue::Use(Operand::Place(discriminator_place.clone()));

    debug!(
        "Discriminator type after auto deref: {}",
        builder.builder.display_typename(discriminator_type_idx)
    );

    match discriminator_type {
        Type::Adt(index) => {
            let adt = builder.builder.get_adt(*index);

            match adt.kind {
                AdtKind::Struct => {
                    // a struct in a match makes no sense
                    return Err(LoweringError::UnexpectedType {
                        found_span: disc_span,
                        found: builder.builder.display_typename(discriminator_type_idx),
                        expected: "Any integral or enum type.".to_string(),
                        expected_span: Some(info.span),
                        path: builder.get_file_path().clone(),
                    });
                }
                AdtKind::Enum => {
                    let enum_place = discriminator.get_place().unwrap();
                    enum_place_ty =
                        Some((discriminator.get_place().unwrap(), discriminator_type_idx));
                    let mut tag_place = enum_place.clone();
                    tag_place.projection.push(PlaceElem::GetVariant);
                    discriminator = Rvalue::Use(Operand::Place(tag_place));
                    discriminator_type_idx = builder.builder.ir.get_u32_ty();
                }
                AdtKind::Union => todo!(),
            }
        }
        Type::Array(_, _) => todo!(),
        _ => {}
    }

    let local = builder.add_temp_local(discriminator_type_idx);
    let place = Place {
        local,
        projection: vec![],
    };

    builder.statements.push(Statement {
        span: None,
        kind: StatementKind::Assign(place.clone(), discriminator.clone()),
    });

    let statements = std::mem::take(&mut builder.statements);
    // keep idx to change terminator
    let cond_block_idx = builder.body.basic_blocks.len();
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });

    let mut targets = Vec::new();
    let mut targets_last_block = Vec::new();
    let mut target_conds = Vec::new();

    let mut is_enum_match: Option<Option<TypeName>> = None;

    let outer_scope_locals = builder.name_to_local.clone();
    let outer_scope_local_exists = builder.local_exists.clone();

    for variant in info.variants.iter() {
        debug!("lowering variant");
        // keep idx for switch targets
        let current_block_idx = builder.body.basic_blocks.len();
        targets.push(current_block_idx);

        match &variant.case {
            MatchCaseExpr::Value(value_expr) => {
                let idx = match value_expr {
                    ValueExpr::ConstBool(v, _) => (*v) as u32,
                    ValueExpr::ConstChar(v, _) => (*v) as u32,
                    ValueExpr::ConstInt(v, _) => (*v) as u32,
                    ValueExpr::ConstFloat(_, _) => {
                        return Err(LoweringError::InvalidMatch {
                            span: info.span,
                            reason: "Can't use match on floats".to_string(),
                            path: builder.get_file_path().clone(),
                        });
                    }
                    ValueExpr::ConstStr(_, _) => {
                        return Err(LoweringError::Unimplemented {
                            span: info.span,
                            reason: "Match on strings is not yet implemented.".to_string(),
                            path: builder.get_file_path().clone(),
                        });
                    }
                    ValueExpr::Path(p) => {
                        // We use path as a catch all variable binding, so it doesn't make sense if it has extras.
                        if !p.extra.is_empty() {
                            return Err(LoweringError::InvalidMatch {
                                span: info.span,
                                reason: "Unrecognized pattern.".to_string(),
                                path: builder.get_file_path().clone(),
                            });
                        }

                        return Err(LoweringError::Unimplemented {
                            span: info.span,
                            reason: "Match catch all not implemented yet.".to_string(),
                            path: builder.get_file_path().clone(),
                        });
                    }
                };
                target_conds.push(ValueTree::Leaf(ConstValue::U32(idx)));
                is_enum_match = Some(None);
            }
            MatchCaseExpr::Enum(enum_match_expr) => {
                debug!("lowering enum variant {}", enum_match_expr.name.name.name);
                if let Some(None) = is_enum_match {
                    panic!("enum on a non enum match")
                }

                if let Some(Some(name)) = &is_enum_match {
                    if name.name.name != enum_match_expr.name.name.name {
                        panic!(
                            "mismatched enum types \n{:?}, \n{:?}",
                            name, enum_match_expr.name
                        )
                    }
                } else {
                    is_enum_match = Some(Some(enum_match_expr.name.clone()));
                }

                // monomorphic enum should already be lowered here.

                // If generics where specified, lower them.
                let mut generics = Vec::new();
                for gen_ty in enum_match_expr.name.generics.iter() {
                    let ty = lower_type(
                        builder.builder,
                        &TypeDescriptor::Type {
                            name: gen_ty.clone(),
                            span: gen_ty.span,
                        },
                    )?;
                    generics.push(ty);
                }
                let sym = Symbol {
                    name: enum_match_expr.name.name.name.clone(),
                    method_of: None,
                    generics,
                };

                // Find the expected adt id with the given generics (if any).
                let expected_adt_id_if_generics =
                    builder.get_symbols_table().aggregates.get(&sym).cloned();

                let (enum_place, enum_ty) = enum_place_ty.as_ref().unwrap();

                let adt_id = match builder.builder.get_type(*enum_ty) {
                    Type::Adt(index) => *index,
                    _ => unreachable!(),
                };

                if !sym.generics.is_empty() {
                    if let Some(expected_adt_id_if_generics) = expected_adt_id_if_generics {
                        if expected_adt_id_if_generics != adt_id {
                            return Err(LoweringError::UnexpectedType {
                                found_span: disc_span,
                                found: builder.builder.display_typename(*enum_ty),
                                expected: enum_match_expr.name.to_string(),
                                expected_span: Some(enum_match_expr.span),
                                path: builder.get_file_path().clone(),
                            });
                        }
                    } else {
                        // Type not found, meaning it may not be lowered, but nonetheless doesn't match the expected type.
                        // Because generics where specified explicitly.
                        return Err(LoweringError::UnexpectedType {
                            found_span: enum_match_expr.name.span,
                            found: enum_match_expr.name.to_string(),
                            expected: builder.builder.display_typename(*enum_ty),
                            expected_span: Some(disc_span),
                            path: builder.get_file_path().clone(),
                        });
                    }
                }

                let adt_body = builder.builder.get_adt(adt_id);

                let variant_idx = *adt_body
                    .variant_names
                    .get(&enum_match_expr.variant.name)
                    .expect("variant not found");

                let variant_value = ValueTree::Leaf(ConstValue::U32(variant_idx as u32));

                target_conds.push(variant_value);

                for field_value in &enum_match_expr.field_values {
                    debug!("lowering variant field {}", field_value.name);
                    let field_idx = *adt_body.variants[variant_idx]
                        .field_names
                        .get(&field_value.name)
                        .expect("field not found");
                    let ty_idx = adt_body.variants[variant_idx].fields[field_idx].ty;

                    let field_local_idx = builder.body.locals.len();
                    builder
                        .name_to_local
                        .insert(field_value.name.clone(), field_local_idx);
                    builder.local_exists.insert(field_local_idx);

                    let field_local = Local::new(
                        Some(field_value.span),
                        LocalKind::Temp,
                        ty_idx,
                        Some(field_value.name.clone()),
                        false,
                    );

                    builder.body.locals.push(field_local);
                    builder.local_exists.insert(field_local_idx);

                    let field_place = Place {
                        local: field_local_idx,
                        projection: Vec::new(),
                    };

                    // todo: maybe add a "mut ref" keyword to be able to modify the field in the match

                    let enum_field_place = {
                        let mut place = enum_place.clone();
                        place.projection.push(PlaceElem::Variant(variant_idx));
                        place.projection.push(PlaceElem::Field(field_idx));
                        place
                    };

                    builder.statements.push(Statement {
                        span: Some(enum_match_expr.span),
                        kind: StatementKind::Assign(
                            field_place,
                            Rvalue::Use(Operand::Place(enum_field_place)),
                        ),
                    });

                    builder.statements.push(Statement {
                        span: Some(enum_match_expr.span),
                        kind: StatementKind::StorageLive(field_local_idx),
                    });
                }
            }
        }

        for stmt in &variant.block {
            get_locals(builder, stmt)?;
            lower_statement(builder, stmt, builder.body.locals[builder.ret_local].ty)?;
        }

        // keep idx to change terminator
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
        targets_last_block.push(last_then_block_idx);

        builder.name_to_local = outer_scope_locals.clone();
        builder.local_exists = outer_scope_local_exists.clone();
    }

    // todo: add otherwise block

    // otherwise block
    // todo: maybe its not needed if user adds a catch all.
    // todo: identify catch all, maybe path
    let statements = std::mem::take(&mut builder.statements);
    let otherwise_block_idx = builder.body.basic_blocks.len();
    builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Unreachable,
        }),
    });
    targets.push(otherwise_block_idx);

    let targets = SwitchTargets {
        values: target_conds,
        targets,
    };

    let kind = TerminatorKind::SwitchInt {
        discriminator: Operand::Place(place),
        targets,
    };
    builder.body.basic_blocks[cond_block_idx].terminator.kind = kind;
    builder.name_to_local = outer_scope_locals;

    let next_block_idx = builder.body.basic_blocks.len();

    for blockidx in &targets_last_block {
        if matches!(
            builder.body.basic_blocks[*blockidx].terminator.kind,
            TerminatorKind::Unreachable
        ) {
            builder.body.basic_blocks[*blockidx].terminator.kind = TerminatorKind::Goto {
                target: next_block_idx,
            };
        }
    }

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
