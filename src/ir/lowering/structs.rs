use std::collections::HashMap;

use tracing::instrument;

use crate::{ast::structs::StructDecl, ir::TyKind};

use super::{
    errors::LoweringError,
    ir::{AdtBody, StructIndex, VariantDef},
    types::lower_type,
    IRBuilder, Symbol,
};

/// Lowers a struct.
///
/// If the struct is generic, `builder.current_generics_map` should contain the generic specific types already.
#[instrument(level = "debug", skip_all, fields(name = ?info.name.name))]
pub(crate) fn lower_struct(
    builder: &mut IRBuilder,
    info: &StructDecl,
) -> Result<StructIndex, LoweringError> {
    // Sym initially is the polymorphic symbol of the struct (only matters, if the struct is generic)
    let mut sym = Symbol {
        name: info.name.name.clone(),
        method_of: None,
        generics: Vec::new(),
    };

    let module_idx = builder.local_module.unwrap();

    let poly_idx = *builder.symbols[&module_idx].structs.get(&sym).unwrap();

    let mut generic_types = Vec::new();

    for generic_param in &info.generics {
        let ty = *builder
            .current_generics_map
            .get(&generic_param.name.name)
            .unwrap_or_else(|| panic!("Missing generic type mapping {:?}", generic_param));
        generic_types.push(ty);
    }

    // If struct is generic, get or create the id for this monomorphized struct.
    let mono_idx = if !info.generics.is_empty() {
        sym.generics = generic_types.clone();

        if let Some(id) = builder
            .symbols
            .get(&module_idx)
            .unwrap()
            .structs
            .get(&sym)
            .copied()
        {
            Some(id)
        } else {
            let id = builder.ir.structs.insert(None);
            builder
                .symbols
                .get_mut(&module_idx)
                .unwrap()
                .structs
                .insert(sym.clone(), id);
            let struct_type_idx = builder.ir.types.insert(Some(TyKind::Struct(id)));
            builder.struct_to_type_idx.insert(id, struct_type_idx);
            Some(id)
        }
    } else {
        None
    };

    // If struct is generic, use the monomorphized id, else its not generic.
    let idx = mono_idx.unwrap_or(poly_idx);

    // Check if its already lowered.
    if builder.ir.structs[idx].is_some() {
        return Ok(idx);
    }

    let mut body = AdtBody {
        is_pub: true, // todo: pub
        name: info.name.name.clone(),
        variants: Vec::new(),
        name_to_variant_idx: HashMap::new(),
        span: info.span,
    };

    for (i, field) in info.fields.iter().enumerate() {
        let variant = VariantDef {
            name: field.name.name.clone(),
            ty: lower_type(builder, &field.r#type)?,
            discriminant: i,
        };
        body.variants.push(variant);
        body.name_to_variant_idx
            .insert(field.name.name.clone(), body.variants.len() - 1);
    }

    builder.ir.structs[idx] = Some(body);
    builder.ir.modules[builder.local_module.unwrap()]
        .structs
        .insert(idx);

    Ok(idx)
}
