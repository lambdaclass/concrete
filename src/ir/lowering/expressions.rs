use std::{collections::HashSet, sync::Arc};

use tracing::{debug, instrument};

use crate::{
    ast::expressions::{
        ArithOp, BinaryOp, BitwiseOp, CmpOp, Expression, LogicOp, PathOp, PathSegment, ValueExpr,
    },
    ir::{
        lowering::{functions::lower_fn_call, structs::lower_struct},
        ConstKind, ConstValue, FloatTy, IntTy, Local, Mutability, Operand, Place, PlaceElem, Span,
        Statement, StatementKind, Type, UintTy, ValueTree,
    },
};

use super::{
    constants::lower_constant_ref,
    errors::LoweringError,
    ir::{BinOp, ConstData, LogOp, Rvalue, TypeIndex},
    types::lower_type,
    FnIrBuilder,
};

#[instrument(level = "debug", skip_all)]
pub(crate) fn lower_expression(
    builder: &mut FnIrBuilder,
    info: &Expression,
    type_hint: Option<TypeIndex>,
) -> Result<(Rvalue, TypeIndex, Span), LoweringError> {
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
            let (value, type_idx, _span) = lower_expression(builder, info, type_hint)?;

            let ty = builder.builder.get_type(type_idx);

            let mut place = match value {
                Rvalue::Ref(_, place) => place,
                Rvalue::Use(op) => match op {
                    Operand::Place(place) => place,
                    Operand::Const(_) => todo!("deref to constant data not yet implemented"),
                },
                value => todo!("deref not implemented for {value:?}"),
            };

            assert!(ty.is_ptr_like(), "not deferenceable");
            let type_idx = ty.get_inner_type().expect("should have inner");

            place.projection.push(PlaceElem::Deref);

            (Rvalue::Use(Operand::Place(place)), type_idx, *deref_span)
        }
        Expression::AsRef(inner, mutable, asref_span) => {
            debug!("lowering asref");
            let type_hint =
                type_hint.and_then(|type_idx| builder.builder.get_type(type_idx).get_inner_type());

            let (value, ty, _ref_target_span) = lower_expression(builder, inner, type_hint)?;

            if let Some(local) = value.get_local() {
                if *mutable && !builder.body.locals[local].mutable {
                    return Err(LoweringError::CantTakeMutableBorrow {
                        span: *asref_span,
                        declare_span: builder.body.locals[local].span,
                        path: builder.get_file_path().clone(),
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
                    let inner_local = builder.add_local(Local::temp(ty));
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

            let ref_ty = builder
                .builder
                .ir
                .types
                .insert(Some(Type::Ref(ty, mutability)));

            (rvalue, ref_ty, *asref_span)
        }
        Expression::StructInit(info) => {
            debug!("lowering struct init for struct {}", info.name);

            let struct_idx = builder.builder.get_or_lower_for_struct_init(info)?;
            let struct_body = builder.builder.get_struct(struct_idx).clone();

            let struct_ty = builder
                .builder
                .ir
                .types
                .insert(Some(Type::Struct(struct_idx)));
            let struct_local = builder.add_local(Local::temp(struct_ty));

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
                    .variant_names
                    .get(&field.name)
                    .expect("failed to find field");
                let mut field_place = place.clone();
                field_place.projection.push(PlaceElem::Field(idx));

                let variant = struct_body.variants[idx].ty;

                let (value, _value_ty, _field_span) =
                    lower_expression(builder, &value.value, Some(variant))?;

                builder.statements.push(Statement {
                    span: Some(info.span),
                    kind: StatementKind::Assign(field_place, value),
                });
            }

            (Rvalue::Use(Operand::Place(place)), struct_ty, info.span)
        }
        Expression::Cast(value, cast_ty, span) => {
            let (value, ty, _span) = lower_expression(builder, value, None)?;

            let new_ty = lower_type(builder.builder, cast_ty)?;

            // todo: check if the cast is valid

            // check if its a use directly, to avoid a temporary.
            let rvalue = match value {
                Rvalue::Use(op) => Rvalue::Cast(op, new_ty, *span),
                value => {
                    let inner_local = builder.add_local(Local::temp(ty));
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
                    Rvalue::Cast(Operand::Place(inner_place), new_ty, *span)
                }
            };

            (rvalue, new_ty, *span)
        }
        Expression::ArrayInit(info) => {
            let element_type_hint =
                type_hint.and_then(|x| builder.builder.get_type(x).get_inner_type());

            let mut values = info.values.iter().enumerate();

            // Extract the first value from the array init. It's type will be used as type hint for the following elements.
            let (first_idx, first_element) = values.next().expect("array init cannot be empty");
            let (first_value, element_type, _element_span) =
                lower_expression(builder, first_element, element_type_hint)?;

            let length = info.values.len() as u64;

            let ty_kind = Type::Array(
                element_type,
                Arc::new(ConstData {
                    ty: builder.builder.ir.get_u64_ty(),
                    span: info.span,
                    data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(length))),
                }),
            );
            let ty = builder.builder.ir.types.insert(Some(ty_kind));

            // Create and init local for the array init expression.
            let array_local = builder.add_local(Local::temp(ty));
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
                    lower_expression(builder, element, Some(element_type))?;

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
pub(crate) fn find_expression_type(
    fn_builder: &mut FnIrBuilder,
    info: &Expression,
) -> Result<Option<TypeIndex>, LoweringError> {
    debug!("finding expression type");
    Ok(match info {
        Expression::Value(value, _) => match value {
            ValueExpr::ConstBool(_, _) => Some(fn_builder.builder.ir.get_bool_ty()),
            ValueExpr::ConstChar(_, _) => Some(fn_builder.builder.ir.get_char_ty()),
            ValueExpr::ConstInt(_, _) => None,
            ValueExpr::ConstFloat(_, _) => None,
            ValueExpr::ConstStr(_, _) => Some(fn_builder.builder.ir.get_string_ty()),
            ValueExpr::Path(path) => {
                let local = fn_builder.get_local(&path.first.name).unwrap(); // todo handle segments
                Some(local.ty)
            }
        },
        Expression::FnCall(info) => {
            let (poly_fn_id, mono_fn_id) = fn_builder.get_id_for_fn_call(info, None)?;
            let ret_ty = fn_builder.builder.ir.functions[mono_fn_id.unwrap_or(poly_fn_id)]
                .as_ref()
                .unwrap()
                .ret_ty;
            Some(ret_ty)
        }
        Expression::Match(_) => None,
        Expression::If(_) => None,
        Expression::UnaryOp(_, info) => find_expression_type(fn_builder, info)?,
        Expression::BinaryOp(lhs, op, rhs) => {
            if matches!(op, BinaryOp::Logic(_)) {
                Some(fn_builder.builder.ir.get_bool_ty())
            } else {
                find_expression_type(fn_builder, lhs)?.or(find_expression_type(fn_builder, rhs)?)
            }
        }
        Expression::Deref(inner_type_idx, _) => {
            let inner_type_idx = find_expression_type(fn_builder, inner_type_idx)?;

            if let Some(inner_type_idx) = inner_type_idx {
                let inner_ty = fn_builder.builder.get_type(inner_type_idx).clone();

                if let Type::Ref(inner, _) = inner_ty {
                    Some(inner)
                } else {
                    None
                }
            } else {
                None
            }
        }
        Expression::AsRef(inner, mutable, _) => {
            let inner_ty = find_expression_type(fn_builder, inner)?;

            if let Some(inner_ty) = inner_ty {
                let tykind = Type::Ref(
                    inner_ty,
                    if *mutable {
                        Mutability::Mut
                    } else {
                        Mutability::Not
                    },
                );
                Some(fn_builder.builder.ir.types.insert(Some(tykind)))
            } else {
                None
            }
        }
        Expression::StructInit(info) => {
            debug!("lowering struct init for struct {}", info.name);

            let struct_idx = fn_builder.builder.get_or_lower_for_struct_init(info)?;

            fn_builder.builder.get_struct(struct_idx);

            let struct_ty = fn_builder
                .builder
                .ir
                .types
                .insert(Some(Type::Struct(struct_idx)));

            Some(struct_ty)
        }
        Expression::Cast(_, cast_ty, _) => {
            let new_ty = lower_type(fn_builder.builder, cast_ty)?;
            Some(new_ty)
        }
        Expression::ArrayInit(info) => {
            let first_element = info.values.first();

            if first_element.is_none() {
                return Ok(None);
            }

            let first_element = first_element.unwrap();

            let first_type = find_expression_type(fn_builder, first_element)?;

            if first_type.is_none() {
                return Ok(None);
            }

            let first_type = first_type.unwrap();

            let length = info.values.len() as u64;

            let tykind = Type::Array(
                first_type,
                Arc::new(ConstData {
                    ty: fn_builder.builder.ir.get_u64_ty(),
                    span: info.span,
                    data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(length))),
                }),
            );

            let ty = fn_builder.builder.ir.types.insert(Some(tykind));

            Some(ty)
        }
    })
}

pub(crate) fn lower_value_expr(
    fn_builder: &mut FnIrBuilder,
    info: &ValueExpr,
    type_hint: Option<TypeIndex>,
) -> Result<(Rvalue, TypeIndex), LoweringError> {
    Ok(match info {
        ValueExpr::ConstBool(value, const_span) => (
            Rvalue::Use(Operand::Const(ConstData {
                ty: fn_builder.builder.ir.get_bool_ty(),
                span: *const_span,
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(*value))),
            })),
            fn_builder.builder.ir.get_bool_ty(),
        ),
        ValueExpr::ConstChar(value, const_span) => (
            Rvalue::Use(Operand::Const(ConstData {
                ty: fn_builder.builder.ir.get_char_ty(),
                span: *const_span,
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::Char((*value) as u8))),
            })),
            fn_builder.builder.ir.get_char_ty(),
        ),
        ValueExpr::ConstInt(value, const_span) => {
            let (data, ty) = match type_hint {
                Some(type_idx) => (
                    {
                        let ty = fn_builder.builder.get_type(type_idx);
                        ConstData {
                            ty: type_idx,
                            span: *const_span,
                            data: ConstKind::Value(ValueTree::Leaf(match ty {
                                Type::Int(ty) => match ty {
                                    IntTy::I8 => ConstValue::I8(
                                        (*value).try_into().expect("value out of range"),
                                    ),
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
                                Type::Uint(ty) => match ty {
                                    UintTy::U8 => ConstValue::U8(
                                        (*value).try_into().expect("value out of range"),
                                    ),
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
                                Type::Bool => ConstValue::Bool(*value != 0),
                                Type::Ptr(ref _inner, _mutable) => ConstValue::I64(
                                    (*value).try_into().expect("value out of range"),
                                ),
                                x => unreachable!("{:?}", x),
                            })),
                        }
                    },
                    type_idx,
                ),
                None => (
                    ConstData {
                        ty: fn_builder.builder.ir.get_i64_ty(),
                        span: *const_span,
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::I64(
                            (*value).try_into().expect("value out of range"),
                        ))),
                    },
                    fn_builder.builder.ir.get_i64_ty(),
                ),
            };

            (Rvalue::Use(Operand::Const(data)), ty)
        }
        ValueExpr::ConstFloat(value, const_span) => {
            let (data, ty) = match type_hint {
                Some(type_idx) => (
                    {
                        let ty = fn_builder.builder.get_type(type_idx);
                        ConstData {
                            ty: type_idx,
                            span: *const_span,
                            data: ConstKind::Value(ValueTree::Leaf(match &ty {
                                Type::Float(ty) => match ty {
                                    FloatTy::F32 => {
                                        ConstValue::F32(value.parse().expect("error parsing float"))
                                    }
                                    FloatTy::F64 => {
                                        ConstValue::F64(value.parse().expect("error parsing float"))
                                    }
                                },
                                _ => unreachable!(),
                            })),
                        }
                    },
                    type_idx,
                ),
                None => (
                    ConstData {
                        ty: fn_builder.builder.ir.get_f64_ty(),
                        span: *const_span,
                        data: ConstKind::Value(ValueTree::Leaf(ConstValue::F64(
                            value.parse().expect("error parsing float"),
                        ))),
                    },
                    fn_builder.builder.ir.get_f64_ty(),
                ),
            };

            (Rvalue::Use(Operand::Const(data)), ty)
        }
        ValueExpr::ConstStr(_, _) => todo!(),
        ValueExpr::Path(info) => {
            if fn_builder.name_to_local.contains_key(&info.first.name) {
                let (place, place_ty, _span) = lower_path(fn_builder, info)?;
                (Rvalue::Use(Operand::Place(place.clone())), place_ty)
            } else {
                let (constant_value, ty) = lower_constant_ref(fn_builder, info)?;
                (Rvalue::Use(Operand::Const(constant_value)), ty)
            }
        }
    })
}

#[instrument(level = "debug", skip_all, fields(first = ?info.first.name))]
pub(crate) fn lower_path(
    fn_builder: &mut FnIrBuilder,
    info: &PathOp,
) -> Result<(Place, TypeIndex, Span), LoweringError> {
    let mut local = *fn_builder.name_to_local.get(&info.first.name).ok_or(
        LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            path: fn_builder.get_file_path().clone(),
        },
    )?;

    if !fn_builder.local_exists.contains(&local) {
        Err(LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            path: fn_builder.get_file_path().clone(),
        })?;
    }

    let mut type_idx = fn_builder.body.locals[local].ty;
    let mut ty = fn_builder.builder.get_type(type_idx).clone();
    let mut projection = Vec::new();
    let old_generics = fn_builder.builder.current_generics_map.clone();

    if let Type::Struct(struct_index) = ty {
        let poly_idx = fn_builder
            .builder
            .mono_type_to_poly
            .get(&type_idx)
            .copied()
            .unwrap_or(type_idx);
        let poly_struct_idx = if let Type::Struct(id) = fn_builder.builder.get_type(poly_idx) {
            *id
        } else {
            panic!("poly struct not found")
        };
        let struct_body = fn_builder
            .builder
            .bodies
            .structs
            .get(&poly_struct_idx)
            .unwrap()
            .clone();

        let generics: HashSet<String> = struct_body
            .generics
            .iter()
            .map(|x| x.name.name.clone())
            .collect();

        for field in &struct_body.fields {
            if let Some(name) = field.r#type.get_name() {
                if generics.contains(&name) {
                    let struct_adt = fn_builder.builder.get_struct(struct_index); // borrowck
                    let field_index = *struct_adt.variant_names.get(&field.name.name).unwrap();
                    let field_ty = struct_adt.variants[field_index].ty;
                    let field_type = fn_builder.builder.get_type(field_ty);
                    let mut map_ty = field_ty;
                    if let Some(inner) = field_type.get_inner_type() {
                        map_ty = inner;
                    }
                    debug!(
                        "Adding field type to generics mapping {} -> {}",
                        name,
                        fn_builder
                            .builder
                            .get_type(map_ty)
                            .display(&fn_builder.builder.ir)
                            .unwrap()
                    );
                    fn_builder.builder.current_generics_map.insert(name, map_ty);
                }
            }
        }
    }

    for segment in &info.extra {
        match segment {
            PathSegment::FieldAccess(name, field_span) => {
                // auto deref
                while let Type::Ref(inner, _) = ty {
                    projection.push(PlaceElem::Deref);
                    type_idx = inner;
                    ty = fn_builder.builder.get_type(type_idx).clone();
                }

                if let Type::Struct(mut id) = ty {
                    if fn_builder.builder.ir.structs[id].is_none() {
                        id = lower_struct(
                            fn_builder.builder,
                            &fn_builder.builder.bodies.structs.get(&id).unwrap().clone(),
                        )?;
                    }

                    let struct_body = fn_builder.builder.get_struct(id);
                    let idx = *struct_body.variant_names.get(&name.name).ok_or_else(|| {
                        LoweringError::StructFieldNotFound {
                            span: *field_span,
                            name: name.name.clone(),
                            path: fn_builder.get_file_path().clone(),
                        }
                    })?;
                    projection.push(PlaceElem::Field(idx));
                    type_idx = struct_body.variants[idx].ty;
                    ty = fn_builder.builder.get_type(type_idx).clone();
                }
            }
            PathSegment::ArrayIndex(expression, _) => {
                while let Type::Ref(inner, _) = ty {
                    projection.push(PlaceElem::Deref);
                    type_idx = inner;
                    ty = fn_builder.builder.get_type(type_idx).clone();
                }

                if let Type::Array(element_type, _) = ty {
                    // Assign the index expression to a temporary local
                    let (index, index_type_idx) = lower_value_expr(fn_builder, expression, None)?;
                    let index_local = fn_builder.add_temp_local(index_type_idx);
                    let index_place = Place {
                        local: index_local,
                        projection: vec![],
                    };
                    fn_builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::StorageLive(index_local),
                    });
                    fn_builder.statements.push(Statement {
                        span: None,
                        kind: StatementKind::Assign(index_place.clone(), index),
                    });

                    // Use the local's value as index of the array
                    projection.push(PlaceElem::Index(index_local));

                    type_idx = element_type;
                    ty = fn_builder.builder.get_type(type_idx).clone();
                }
            }
            PathSegment::MethodCall(fn_call_op, _span) => {
                // TODO: auto deref?
                let (value, new_type_idx, _span) = lower_fn_call(
                    fn_builder,
                    fn_call_op,
                    Some((
                        Place {
                            local,
                            projection: projection.clone(),
                        },
                        type_idx,
                    )),
                    Some(type_idx),
                )?;

                type_idx = new_type_idx;
                ty = fn_builder.builder.get_type(type_idx).clone();

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
            }
        }
    }

    fn_builder.builder.current_generics_map = old_generics;

    Ok((Place { local, projection }, type_idx, info.span))
}

pub(crate) fn lower_binary_op(
    builder: &mut FnIrBuilder,
    lhs: &Expression,
    op: BinaryOp,
    rhs: &Expression,
    type_hint: Option<TypeIndex>,
) -> Result<(Rvalue, TypeIndex, Span), LoweringError> {
    let (lhs, lhs_type_idx, lhs_span) = if type_hint.is_none() {
        let ty = find_expression_type(builder, lhs)?.or(find_expression_type(builder, rhs)?);

        let ty = if let Some(ty) = ty {
            ty
        } else {
            // Default to i32 if cant infer type.
            // Should be ok because at other points if the i32 doesn't match the expected type
            // a error will be thrown, forcing user to specify types.
            debug!("can't infer type, defaulting to i32");
            builder.builder.ir.get_i32_ty()
        };
        lower_expression(builder, lhs, Some(ty))?
    } else {
        lower_expression(builder, lhs, type_hint)?
    };

    let lhs_ty = builder.builder.get_type(lhs_type_idx).clone();

    // We must handle the special case where you can do ptr + offset.
    let is_lhs_ptr = matches!(lhs_ty, Type::Ptr(_, _));

    let (rhs, rhs_type_idx, rhs_span) = if type_hint.is_none() {
        let ty = find_expression_type(builder, rhs)?.unwrap_or(lhs_type_idx);
        lower_expression(builder, rhs, if is_lhs_ptr { None } else { Some(ty) })?
    } else {
        lower_expression(builder, rhs, if is_lhs_ptr { None } else { type_hint })?
    };

    let rhs_ty = builder.builder.get_type(rhs_type_idx);

    if !is_lhs_ptr && !lhs_ty.is_equal(rhs_ty, &builder.builder.ir) {
        return Err(LoweringError::UnexpectedType {
            found_span: rhs_span,
            found: rhs_ty.display(&builder.builder.ir).unwrap(),
            expected: lhs_ty.display(&builder.builder.ir).unwrap(),
            expected_span: Some(lhs_span),
            path: builder.get_file_path().clone(),
        });
    }

    let lhs_local = builder.add_local(Local::temp(lhs_type_idx));
    let rhs_local = builder.add_local(Local::temp(rhs_type_idx));
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
            lhs_type_idx,
            full_span,
        ),
        BinaryOp::Logic(op) => (
            match op {
                LogicOp::And => Rvalue::LogicOp(LogOp::And, (lhs, rhs)),
                LogicOp::Or => Rvalue::LogicOp(LogOp::Or, (lhs, rhs)),
            },
            builder.builder.ir.get_bool_ty(),
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
            builder.builder.ir.get_bool_ty(),
            full_span,
        ),
        BinaryOp::Bitwise(op) => (
            match op {
                BitwiseOp::And => Rvalue::BinaryOp(BinOp::BitAnd, (lhs, rhs)),
                BitwiseOp::Or => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
                BitwiseOp::Xor => Rvalue::BinaryOp(BinOp::BitXor, (lhs, rhs)),
            },
            lhs_type_idx,
            full_span,
        ),
    })
}
