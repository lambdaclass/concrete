use std::collections::HashMap;

use tracing::instrument;

use crate::{
    ast::{enums::EnumDecl, structs::StructDecl},
    ir::{AdtKind, FieldDef, Type},
};

use super::{
    IRBuilder, Symbol,
    errors::LoweringError,
    ir::{AdtBody, AdtIndex, VariantDef},
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
    // Sym initially is the polymorphic symbol of the struct (only matters, if the struct is generic)
    let mut sym = Symbol {
        name: info.name.name.clone(),
        method_of: None,
        generics: Vec::new(),
    };

    let module_idx = builder.get_current_module_idx();

    let polymorphic_idx = *builder.symbols[&module_idx].aggregates.get(&sym).unwrap();

    let mut generic_types = Vec::new();
    let mut generics_used = HashMap::new();

    for generic_param in &info.generics {
        let ty = builder.get_generic_param_ty(generic_param)?;
        generic_types.push(ty);
        generics_used.insert(generic_param.name.name.clone(), ty);
    }

    // If struct is generic, get or create the id for this monomorphized struct.
    let mono_idx = if !info.generics.is_empty() {
        sym.generics = generic_types.clone();

        if let Some(id) = builder
            .symbols
            .get(&module_idx)
            .unwrap()
            .aggregates
            .get(&sym)
            .copied()
        {
            Some(id)
        } else {
            let id = builder.ir.aggregates.insert(None);
            builder
                .symbols
                .get_mut(&module_idx)
                .unwrap()
                .aggregates
                .insert(sym.clone(), id);
            let struct_type_idx = builder.ir.types.insert(Some(Type::Adt(id)));
            builder.adt_to_type_idx.insert(id, struct_type_idx);
            Some(id)
        }
    } else {
        None
    };

    // If struct is generic, use the monomorphized id, else its not generic.
    let idx = mono_idx.unwrap_or(polymorphic_idx);

    // Check if its already lowered.
    if builder.ir.aggregates[idx].is_some() {
        return Ok(idx);
    }

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

    builder.ir.aggregates[idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(idx);

    Ok(idx)
}

/// Lowers a enum.
///
/// If the enum is generic, `builder.context.generics_mapping` should contain the generic specific types already.
#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_enum(
    builder: &mut IRBuilder,
    info: &EnumDecl,
) -> Result<AdtIndex, LoweringError> {
    // Sym initially is the polymorphic symbol of the enum (only matters, if the enum is generic)
    let mut sym = Symbol {
        name: info.name.name.clone(),
        method_of: None,
        generics: Vec::new(),
    };

    let module_idx = builder.get_current_module_idx();

    let poly_idx = *builder.symbols[&module_idx].aggregates.get(&sym).unwrap();

    let mut generic_types = Vec::new();
    let mut generics_used = HashMap::new();

    for generic_param in &info.generics {
        let ty = *builder
            .context
            .generics_mapping
            .get(&generic_param.name.name)
            .unwrap_or_else(|| panic!("Missing generic type mapping {:?}", generic_param));
        generic_types.push(ty);
        generics_used.insert(generic_param.name.name.clone(), ty);
    }

    // If struct is generic, get or create the id for this monomorphized struct.
    let mono_idx = if !info.generics.is_empty() {
        sym.generics = generic_types.clone();

        if let Some(id) = builder
            .symbols
            .get(&module_idx)
            .unwrap()
            .aggregates
            .get(&sym)
            .copied()
        {
            Some(id)
        } else {
            let id = builder.ir.aggregates.insert(None);
            builder
                .symbols
                .get_mut(&module_idx)
                .unwrap()
                .aggregates
                .insert(sym.clone(), id);
            let enum_type_idx = builder.ir.types.insert(Some(Type::Adt(id)));
            builder.adt_to_type_idx.insert(id, enum_type_idx);
            Some(id)
        }
    } else {
        None
    };

    // If enum is generic, use the monomorphized id, else its not generic.
    let idx = mono_idx.unwrap_or(poly_idx);

    // Check if its already lowered.
    if builder.ir.aggregates[idx].is_some() {
        return Ok(idx);
    }

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

    builder.ir.aggregates[idx] = Some(body);
    builder.ir.modules[module_idx].aggregates.insert(idx);

    Ok(idx)
}
