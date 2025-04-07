use std::collections::HashMap;

use tracing::instrument;

use crate::{
    ast::{common::GenericParam, enums::EnumDecl, structs::StructDecl},
    ir::{AdtKind, FieldDef, Type, TypeIndex},
};

use super::{
    IRBuilder,
    errors::LoweringError,
    ir::{AdtBody, AdtIndex, VariantDef},
    symbols::AdtSymbol,
    types::lower_type,
};

/// Lowers a struct.
///
/// If the struct is generic, `builder.context.generics_mapping` should contain the generic specific types already.
#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_struct(
    builder: &mut IRBuilder,
    info: &StructDecl,
) -> Result<AdtIndex, LoweringError> {
    if !info.generics.is_empty() {
        return lower_generic_struct(builder, info);
    }

    let sym = AdtSymbol {
        name: info.name.name.clone(),
    };

    let module_idx = builder.get_current_module_idx();

    let idx = *builder.symbols[&module_idx]
        .aggregates
        .get(&sym)
        .expect("adt should have a symbol from lower symbols");

    // Check if its already lowered.
    if builder.ir.aggregates[idx].is_some() {
        return Ok(idx);
    }

    let body = lower_struct_body(builder, info, Default::default())?;

    builder.ir.aggregates[idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(idx);

    Ok(idx)
}

/// Assumes current module id is the module where struct is defined.
#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_generic_struct(
    builder: &mut IRBuilder,
    info: &StructDecl,
) -> Result<AdtIndex, LoweringError> {
    let (mono_idx, generics_used) =
        get_or_create_adt_mono_idx(builder, &info.name.name, &info.generics)?;

    // Check if its already lowered
    if builder.ir.aggregates[mono_idx].is_some() {
        return Ok(mono_idx);
    }

    let body = lower_struct_body(builder, info, generics_used)?;

    let module_idx = builder.get_current_module_idx();
    builder.ir.aggregates[mono_idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(mono_idx);

    Ok(mono_idx)
}

pub(crate) fn lower_struct_body(
    builder: &mut IRBuilder,
    info: &StructDecl,
    generics_used: HashMap<String, TypeIndex>,
) -> Result<AdtBody, LoweringError> {
    let mut body = AdtBody {
        is_pub: info.is_pub,
        name: info.name.name.clone(),
        variants: Vec::new(),
        variant_names: HashMap::new(),
        kind: AdtKind::Struct,
        span: info.span,
        generics_used,
    };

    let mut struct_variant = VariantDef {
        name: info.name.name.clone(),
        field_names: HashMap::default(),
        fields: Vec::new(),
        discriminant: crate::ir::VariantDiscr::Relative(0),
        span: info.span,
    };

    for field in info.fields.iter() {
        let variant = FieldDef {
            name: field.name.name.clone(),
            is_pub: field.is_pub,
            ty: lower_type(builder, &field.r#type)?,
        };
        struct_variant.fields.push(variant);
        struct_variant
            .field_names
            .insert(field.name.name.clone(), struct_variant.fields.len() - 1);
    }

    body.variants.push(struct_variant);
    body.variant_names.insert(String::new(), 0);

    Ok(body)
}

/// Lowers a enum.
///
/// If the enum is generic, `builder.context.generics_mapping` should contain the generic specific types already.
#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_enum(
    builder: &mut IRBuilder,
    info: &EnumDecl,
) -> Result<AdtIndex, LoweringError> {
    if !info.generics.is_empty() {
        return lower_generic_enum(builder, info);
    }

    let sym = AdtSymbol {
        name: info.name.name.clone(),
    };

    let module_idx = builder.get_current_module_idx();

    let idx = *builder.symbols[&module_idx]
        .aggregates
        .get(&sym)
        .expect("adt should have a symbol from lower symbols");

    // Check if its already lowered.
    if builder.ir.aggregates[idx].is_some() {
        return Ok(idx);
    }

    let body = lower_enum_body(builder, info, Default::default())?;

    builder.ir.aggregates[idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(idx);

    Ok(idx)
}

#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_generic_enum(
    builder: &mut IRBuilder,
    info: &EnumDecl,
) -> Result<AdtIndex, LoweringError> {
    let (mono_idx, generics_used) =
        get_or_create_adt_mono_idx(builder, &info.name.name, &info.generics)?;

    // Check if its already lowered
    if builder.ir.aggregates[mono_idx].is_some() {
        return Ok(mono_idx);
    }

    let body = lower_enum_body(builder, info, generics_used)?;

    let module_idx = builder.get_current_module_idx();
    builder.ir.aggregates[mono_idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(mono_idx);

    Ok(mono_idx)
}

pub(crate) fn lower_enum_body(
    builder: &mut IRBuilder,
    info: &EnumDecl,
    generics_used: HashMap<String, TypeIndex>,
) -> Result<AdtBody, LoweringError> {
    let mut body = AdtBody {
        is_pub: info.is_pub,
        name: info.name.name.clone(),
        variants: Vec::new(),
        variant_names: HashMap::new(),
        kind: AdtKind::Enum,
        span: info.span,
        generics_used,
    };

    for (i, variant) in info.variants.iter().enumerate() {
        let mut vardef = VariantDef {
            name: variant.name.name.clone(),
            field_names: HashMap::default(),
            fields: Vec::new(),
            discriminant: crate::ir::VariantDiscr::Relative(i as u32),
            span: variant.span,
        };

        for (field_idx, field) in variant.fields.iter().enumerate() {
            vardef
                .field_names
                .insert(field.name.name.clone(), field_idx);
            vardef.fields.push(FieldDef {
                name: field.name.name.clone(),
                is_pub: field.is_pub,
                ty: lower_type(builder, &field.r#type)?,
            });
        }
        body.variant_names.insert(variant.name.name.clone(), i);
        body.variants.push(vardef);
    }

    Ok(body)
}

/// Gets or creates a monomorphized adt id for the given adt.
pub(crate) fn get_or_create_adt_mono_idx(
    builder: &mut IRBuilder,
    name: &str,
    generics: &[GenericParam],
) -> Result<(AdtIndex, HashMap<String, TypeIndex>), LoweringError> {
    let sym = AdtSymbol {
        name: name.to_string(),
    };

    let mut generic_types = Vec::new();
    let mut generics_used = HashMap::new();

    for generic_param in generics {
        let ty = builder.get_generic_param_ty(generic_param)?;
        generic_types.push(ty);
        generics_used.insert(generic_param.name.name.clone(), ty);
    }

    let mono_sym = sym.monomorphize(&generic_types);

    let module_idx = builder.get_current_module_idx();

    // Check if it's already lowered.
    if let Some(id) = builder
        .get_current_symbols()
        .monomorphized_aggregates
        .get(&mono_sym)
    {
        return Ok((*id, generics_used));
    }

    let polymorphic_adt_idx = *builder.symbols[&module_idx]
        .aggregates
        .get(&sym)
        .expect("adt should have a symbol from lower symbols");
    let polymorphic_type_idx = *builder.adt_to_type_idx.get(&polymorphic_adt_idx).unwrap();

    let mono_idx = builder.ir.aggregates.insert(None);
    builder
        .get_current_symbols_mut()
        .monomorphized_aggregates
        .insert(mono_sym.clone(), mono_idx);

    let adt_type_idx = builder.ir.types.insert(Some(Type::Adt(mono_idx)));
    builder.adt_to_type_idx.insert(mono_idx, adt_type_idx);
    builder
        .mono_type_to_poly
        .insert(adt_type_idx, polymorphic_type_idx);
    Ok((mono_idx, generics_used))
}
