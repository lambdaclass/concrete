use std::sync::Arc;

use crate::{ast::types::TypeDescriptor, ir::lowering::Symbol};

use super::{
    errors::LoweringError,
    ir::{
        self, ConstData, ConstKind, ConstValue, FloatTy, Mutability, TyKind, TypeIndex, ValueTree,
    },
    IRBuilder,
};

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
                .get(&TyKind::Int(ir::IntTy::I32))
                .unwrap(),
            "i16" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Int(ir::IntTy::I16))
                .unwrap(),
            "i8" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Int(ir::IntTy::I8))
                .unwrap(),
            "u64" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Uint(ir::UintTy::U64))
                .unwrap(),
            "u32" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Uint(ir::UintTy::U32))
                .unwrap(),
            "u16" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Uint(ir::UintTy::U16))
                .unwrap(),
            "u8" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Uint(ir::UintTy::U8))
                .unwrap(),
            "f32" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Float(FloatTy::F32))
                .unwrap(),
            "f64" => *builder
                .ir
                .builtin_types
                .get(&TyKind::Float(FloatTy::F64))
                .unwrap(),
            "bool" => *builder.ir.builtin_types.get(&TyKind::Bool).unwrap(),
            "string" => *builder.ir.builtin_types.get(&TyKind::String).unwrap(),
            "char" => *builder.ir.builtin_types.get(&TyKind::Char).unwrap(),
            other => {
                // Check if the type name exists in the generic map.
                if let Some(d) = builder.current_generics_map.get(other) {
                    return lower_type(
                        builder,
                        &TypeDescriptor::Type {
                            name: d.clone(),
                            span: d.span,
                        },
                    );
                }

                let module_idx = builder.local_module.unwrap();

                let symbols = builder
                    .symbols
                    .get(&module_idx)
                    .expect("failed to get symbols struct for module");

                let mut sym = Symbol {
                    name: other.to_string(),
                    method_of: None,
                    generics: Vec::new(),
                };

                if let Some(struct_idx) = symbols.structs.get(&sym) {
                    let struct_type_idx = *builder.struct_to_type_idx.get(struct_idx).unwrap();
                    let body = &builder.bodies.structs.get(struct_idx).unwrap();

                    if !body.generics.is_empty() {
                        let mut generics = Vec::new();

                        for generic_param in body.generics.clone().iter() {
                            if let Some(g) = builder
                                .current_generics_map
                                .get(&generic_param.name.name)
                                .cloned()
                            {
                                let ty = lower_type(
                                    builder,
                                    &TypeDescriptor::Type {
                                        name: g.clone(),
                                        span: generic_param.span,
                                    },
                                )?;
                                generics.push(ty);
                            }
                        }

                        sym.generics = generics;

                        // for borrowck
                        let symbols = builder.symbols.get(&module_idx).unwrap();
                        if let Some(mono_struct_idx) = symbols.structs.get(&sym) {
                            *builder
                                .struct_to_type_idx
                                .get(mono_struct_idx)
                                .expect("should have a type idx")
                        } else {
                            Err(LoweringError::UnrecognizedType {
                                span: *span,
                                name: other.to_string(),
                                path: builder.ir.modules[builder.local_module.unwrap()]
                                    .file_path
                                    .clone(),
                            })?
                        }
                    } else {
                        struct_type_idx
                    }
                } else {
                    Err(LoweringError::UnrecognizedType {
                        span: *span,
                        name: other.to_string(),
                        path: builder.ir.modules[builder.local_module.unwrap()]
                            .file_path
                            .clone(),
                    })?
                }
            }
        },
        TypeDescriptor::Ref { of, span: _ } => {
            let tykind = TyKind::Ref(lower_type(builder, of)?, Mutability::Not);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::MutRef { of, span: _ } => {
            let tykind = TyKind::Ref(lower_type(builder, of)?, Mutability::Mut);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::ConstPtr { of, span: _ } => {
            let tykind = TyKind::Ptr(lower_type(builder, of)?, Mutability::Not);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::MutPtr { of, span: _ } => {
            let tykind = TyKind::Ptr(lower_type(builder, of)?, Mutability::Mut);
            builder.ir.types.insert(Some(tykind))
        }
        TypeDescriptor::Array { of, size, span } => {
            let tykind = TyKind::Array(
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
                let tykind = TyKind::Ref(
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
