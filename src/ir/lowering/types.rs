use std::sync::Arc;

use tracing::{debug, instrument};

use crate::{ast::types::TypeDescriptor, ir::lowering::Symbol};

use super::{
    IRBuilder,
    errors::LoweringError,
    ir::{self, ConstData, ConstKind, ConstValue, FloatTy, Mutability, Type, TypeIndex, ValueTree},
    structs::lower_struct,
};

#[instrument(skip_all, fields(name = ?ty.get_name()))]
pub(crate) fn lower_type(
    builder: &mut IRBuilder,
    ty: &TypeDescriptor,
) -> Result<TypeIndex, LoweringError> {
    Ok(match ty {
        TypeDescriptor::Type { name, span } => match name.name.name.as_str() {
            "i64" => builder.ir.get_i64_ty(),
            "i32" => *builder
                .ir
                .builtin_types
                .get(&Type::Int(ir::IntTy::I32))
                .unwrap(),
            "i16" => *builder
                .ir
                .builtin_types
                .get(&Type::Int(ir::IntTy::I16))
                .unwrap(),
            "i8" => *builder
                .ir
                .builtin_types
                .get(&Type::Int(ir::IntTy::I8))
                .unwrap(),
            "u64" => *builder
                .ir
                .builtin_types
                .get(&Type::Uint(ir::UintTy::U64))
                .unwrap(),
            "u32" => *builder
                .ir
                .builtin_types
                .get(&Type::Uint(ir::UintTy::U32))
                .unwrap(),
            "u16" => *builder
                .ir
                .builtin_types
                .get(&Type::Uint(ir::UintTy::U16))
                .unwrap(),
            "u8" => *builder
                .ir
                .builtin_types
                .get(&Type::Uint(ir::UintTy::U8))
                .unwrap(),
            "f32" => *builder
                .ir
                .builtin_types
                .get(&Type::Float(FloatTy::F32))
                .unwrap(),
            "f64" => *builder
                .ir
                .builtin_types
                .get(&Type::Float(FloatTy::F64))
                .unwrap(),
            "bool" => *builder.ir.builtin_types.get(&Type::Bool).unwrap(),
            "String" => *builder.ir.builtin_types.get(&Type::String).unwrap(),
            "char" => *builder.ir.builtin_types.get(&Type::Char).unwrap(),
            other => {
                // Check if the type name exists in the generic map.
                if let Some(ty) = builder.current_generics_map.get(other).copied() {
                    debug!(
                        "found in generics map: {} -> {}",
                        other,
                        builder.get_type(ty).display(&builder.ir).unwrap()
                    );
                    return Ok(ty);
                }

                let module_idx = builder.get_current_module_idx();

                let symbols = builder
                    .symbols
                    .get(&module_idx)
                    .expect("failed to get symbols struct for module");

                let mut sym = Symbol {
                    name: other.to_string(),
                    method_of: None,
                    generics: Vec::new(),
                };

                if let Some(struct_idx) = symbols.aggregates.get(&sym).copied() {
                    let struct_type_idx = *builder.struct_to_type_idx.get(&struct_idx).unwrap();
                    let body = builder.bodies.structs.get(&struct_idx).unwrap().clone();

                    if !body.generics.is_empty() {
                        let type_name_generics = name.generics.clone();
                        let mut type_name_generics = type_name_generics.iter().peekable();
                        let mut generics = Vec::new();

                        for generic_param in body.generics.clone().iter() {
                            if type_name_generics.peek().is_some() {
                                let tyname = type_name_generics.next().unwrap();
                                let ty = lower_type(builder, &tyname.clone().into())?;
                                generics.push(ty);
                            } else if let Some(ty) = builder
                                .current_generics_map
                                .get(&generic_param.name.name)
                                .copied()
                            {
                                generics.push(ty);
                            }
                        }

                        sym.generics = generics.clone();

                        // for borrowck
                        let symbols = builder.symbols.get(&module_idx).unwrap();

                        let mono_struct_idx = if let Some(mono_struct_idx) =
                            symbols.aggregates.get(&sym).copied()
                        {
                            mono_struct_idx
                        } else {
                            let mono_struct_idx = builder.ir.aggregates.insert(None);
                            let type_id = builder.ir.types.insert(Some(Type::Adt(mono_struct_idx)));
                            builder.struct_to_type_idx.insert(mono_struct_idx, type_id);
                            builder.type_to_module.insert(type_id, module_idx);
                            builder
                                .symbols
                                .get_mut(&builder.get_current_module_idx())
                                .unwrap()
                                .aggregates
                                .insert(sym.clone(), mono_struct_idx);
                            builder.bodies.structs.insert(mono_struct_idx, body.clone());

                            mono_struct_idx
                        };

                        let body = builder.bodies.structs.get(&struct_idx).unwrap().clone();
                        if builder.ir.aggregates[mono_struct_idx].is_none() {
                            let generics_mapping = builder.current_generics_map.clone();
                            for (gen_ty, gen_param) in generics.iter().zip(body.generics.iter()) {
                                builder
                                    .current_generics_map
                                    .insert(gen_param.name.name.clone(), *gen_ty);
                            }

                            let id = lower_struct(builder, &body)?;
                            assert_eq!(
                                id, mono_struct_idx,
                                "struct was already inserted so id should match"
                            );

                            builder.mono_type_to_poly.insert(
                                *builder.struct_to_type_idx.get(&mono_struct_idx).unwrap(),
                                struct_type_idx,
                            );

                            builder.current_generics_map = generics_mapping;
                        }

                        *builder
                            .struct_to_type_idx
                            .get(&mono_struct_idx)
                            .expect("should have a type idx")
                    } else {
                        struct_type_idx
                    }
                } else {
                    Err(LoweringError::UnrecognizedType {
                        span: *span,
                        name: other.to_string(),
                        path: builder.ir.modules[builder.get_current_module_idx()]
                            .file_path
                            .clone(),
                    })?
                }
            }
        },
        TypeDescriptor::Ref { of, span: _ } => {
            let tykind = Type::Ref(lower_type(builder, of)?, Mutability::Not);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::MutRef { of, span: _ } => {
            let tykind = Type::Ref(lower_type(builder, of)?, Mutability::Mut);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::ConstPtr { of, span: _ } => {
            let tykind = Type::Ptr(lower_type(builder, of)?, Mutability::Not);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::MutPtr { of, span: _ } => {
            let tykind = Type::Ptr(lower_type(builder, of)?, Mutability::Mut);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::Array { of, size, span } => {
            let tykind = Type::Array(
                lower_type(builder, of)?,
                Arc::new(ConstData {
                    ty: builder.ir.get_u64_ty(),
                    span: *span,
                    data: ConstKind::Value(ValueTree::Leaf(ConstValue::U64(*size))),
                }),
            );
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::SelfType { is_ref, is_mut, .. } => {
            let ty = builder.self_ty.expect("should have self type");

            if *is_ref {
                let tykind = Type::Ref(
                    ty,
                    if *is_mut {
                        Mutability::Mut
                    } else {
                        Mutability::Not
                    },
                );
                builder.ir.types.insert(Some(tykind))
            } else {
                ty
            }
        }
    })
}
