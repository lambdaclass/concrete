use tracing::{debug, instrument};

use crate::{
    ast::{
        constants::ConstantDef,
        expressions::{Expression, PathOp, ValueExpr},
    },
    ir::{ConstKind, ConstValue, FloatTy, IntTy, Type, UintTy, ValueTree},
};

use super::{
    errors::LoweringError,
    ir::{ConstBody, ConstData, TypeIndex},
    types::lower_type,
    FnIrBuilder, IRBuilder,
};

pub(crate) fn lower_constant(
    builder: &mut IRBuilder,
    info: &ConstantDef,
) -> Result<(), LoweringError> {
    let module_idx = builder.get_current_module_idx();

    let idx = *builder.symbols[&module_idx]
        .constants
        .get(&info.decl.name.name)
        .expect("should exist");

    if builder.ir.constants[idx].is_some() {
        return Ok(());
    }

    let value_ty = lower_type(builder, &info.decl.r#type)?;
    let value = lower_constant_expression(builder, &info.value, value_ty)?;

    let body = ConstBody {
        name: info.decl.name.name.clone(),
        value,
        span: info.decl.name.span,
    };

    builder.ir.constants[idx] = Some(body);
    builder.ir.modules[module_idx].constants.insert(idx);

    Ok(())
}

#[instrument(level = "debug", skip_all)]
pub(crate) fn lower_constant_expression(
    builder: &IRBuilder,
    expression: &Expression,
    type_idx: TypeIndex,
) -> Result<ConstData, LoweringError> {
    let ty = builder.ir.types[type_idx].as_ref().expect("should exist");
    debug!("lowering const expression");
    let data = match expression {
        Expression::Value(value, _) => match value {
            ValueExpr::ConstBool(value, span) => ConstData {
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::Bool(*value))),
                ty: type_idx,
                span: *span,
            },
            ValueExpr::ConstChar(value, span) => ConstData {
                data: ConstKind::Value(ValueTree::Leaf(ConstValue::U32((*value) as u32))),
                ty: type_idx,
                span: *span,
            },
            ValueExpr::ConstInt(value, span) => ConstData {
                data: ConstKind::Value(ValueTree::Leaf(match &ty {
                    Type::Int(ty) => match ty {
                        IntTy::I8 => {
                            ConstValue::I8((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I16 => {
                            ConstValue::I16((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I32 => {
                            ConstValue::I32((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I64 => {
                            ConstValue::I64((*value).try_into().expect("value out of range"))
                        }
                        IntTy::I128 => {
                            ConstValue::I128((*value).try_into().expect("value out of range"))
                        }
                    },
                    Type::Uint(ty) => match ty {
                        UintTy::U8 => {
                            ConstValue::U8((*value).try_into().expect("value out of range"))
                        }
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
                    Type::Bool => ConstValue::Bool(*value != 0),
                    x => unreachable!("{:?}", x),
                })),
                ty: type_idx,
                span: *span,
            },
            ValueExpr::ConstFloat(value, span) => ConstData {
                data: ConstKind::Value(ValueTree::Leaf(match &ty {
                    Type::Float(ty) => match ty {
                        FloatTy::F32 => {
                            ConstValue::F32(value.parse().expect("error parsing float"))
                        }
                        FloatTy::F64 => {
                            ConstValue::F64(value.parse().expect("error parsing float"))
                        }
                    },
                    x => unreachable!("{:?}", x),
                })),
                ty: type_idx,
                span: *span,
            },
            ValueExpr::ConstStr(_, _) => todo!(),
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    Ok(data)
}

pub(crate) fn lower_constant_ref(
    fn_builder: &mut FnIrBuilder,
    info: &PathOp,
) -> Result<(ConstData, TypeIndex), LoweringError> {
    let symbols = fn_builder.get_symbols_table();
    let Some(&constant_id) = symbols.constants.get(&info.first.name) else {
        return Err(LoweringError::UseOfUndeclaredVariable {
            span: info.span,
            name: info.first.name.clone(),
            path: fn_builder.get_file_path().clone(),
        });
    };

    let constant_value = fn_builder.builder.get_constant(constant_id).value.clone();

    let ty = constant_value.ty;

    Ok((constant_value, ty))
}
