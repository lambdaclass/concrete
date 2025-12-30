//! Type translation from Concrete IR types to C types.

use crate::ir::{
    AdtBody, AdtKind, ConstKind, ConstValue, FloatTy, IntTy, Type, TypeIndex, UintTy, ValueTree, IR,
};

/// Converts an IR type to its C representation.
pub fn type_to_c(ty: &Type, ir: &IR) -> String {
    match ty {
        Type::Unit => "void".to_string(),
        Type::Bool => "_Bool".to_string(),
        Type::Char => "char".to_string(),
        Type::Int(int_ty) => match int_ty {
            IntTy::I8 => "int8_t".to_string(),
            IntTy::I16 => "int16_t".to_string(),
            IntTy::I32 => "int32_t".to_string(),
            IntTy::I64 => "int64_t".to_string(),
            IntTy::I128 => "__int128".to_string(),
        },
        Type::Uint(uint_ty) => match uint_ty {
            UintTy::U8 => "uint8_t".to_string(),
            UintTy::U16 => "uint16_t".to_string(),
            UintTy::U32 => "uint32_t".to_string(),
            UintTy::U64 => "uint64_t".to_string(),
            UintTy::U128 => "unsigned __int128".to_string(),
        },
        Type::Float(float_ty) => match float_ty {
            FloatTy::F32 => "float".to_string(),
            FloatTy::F64 => "double".to_string(),
        },
        Type::String => "char*".to_string(),
        Type::Array(inner_idx, _size) => {
            // For array types, we return just the element type
            // The size is handled separately in declarations
            let inner_ty = ir.types[*inner_idx].as_ref().unwrap();
            type_to_c(inner_ty, ir)
        }
        Type::Ref(inner_idx, _mutability) | Type::Ptr(inner_idx, _mutability) => {
            let inner_ty = ir.types[*inner_idx].as_ref().unwrap();
            format!("{}*", type_to_c(inner_ty, ir))
        }
        Type::Adt(adt_idx) => {
            let adt = ir.aggregates[*adt_idx].as_ref().unwrap();
            mangle_type_name(&adt.name)
        }
    }
}

/// Converts an IR type to its C declaration form.
/// This handles arrays properly by returning (base_type, suffix).
/// For example, `int32_t[10]` returns ("int32_t", "[10]").
pub fn type_to_c_decl(ty: &Type, ir: &IR) -> (String, String) {
    match ty {
        Type::Array(inner_idx, size_data) => {
            let inner_ty = ir.types[*inner_idx].as_ref().unwrap();
            let (base, inner_suffix) = type_to_c_decl(inner_ty, ir);
            let size = extract_array_size(size_data);
            (base, format!("[{}]{}", size, inner_suffix))
        }
        _ => (type_to_c(ty, ir), String::new()),
    }
}

/// Extracts the array size from ConstData.
pub fn extract_array_size(size_data: &std::sync::Arc<crate::ir::ConstData>) -> u64 {
    if let ConstKind::Value(ValueTree::Leaf(ConstValue::U64(size))) = &size_data.data {
        *size
    } else {
        panic!("Array size must be a u64 constant")
    }
}

/// Mangles a type name for C compatibility.
/// Replaces `::` with `_` and handles special characters.
pub fn mangle_type_name(name: &str) -> String {
    name.replace("::", "_").replace('<', "_").replace('>', "_").replace(", ", "_")
}

/// Mangles a function name for C compatibility.
/// Special case: `main` is not mangled.
pub fn mangle_fn_name(name: &str) -> String {
    if name == "main" {
        return "main".to_string();
    }
    name.replace("::", "_").replace('<', "_").replace('>', "_").replace(", ", "_")
}

/// Generates a C struct definition for an ADT.
pub fn generate_struct_def(adt: &AdtBody, ir: &IR) -> String {
    let mut output = String::new();
    let type_name = mangle_type_name(&adt.name);

    match adt.kind {
        AdtKind::Struct => {
            let variant = adt.variants.first().unwrap();
            // Use named struct to match the forward declaration
            output.push_str(&format!("struct {} {{\n", type_name));

            for field in &variant.fields {
                let field_ty = ir.types[field.ty].as_ref().unwrap();
                let (base_type, suffix) = type_to_c_decl(field_ty, ir);
                output.push_str(&format!("    {} {}{};\n", base_type, field.name, suffix));
            }

            output.push_str("};\n");
        }
        AdtKind::Enum => {
            // Generate the tag enum
            output.push_str(&format!("typedef enum {{\n"));
            for (i, variant) in adt.variants.iter().enumerate() {
                output.push_str(&format!(
                    "    {}_{} = {},\n",
                    type_name,
                    variant.name,
                    i
                ));
            }
            output.push_str(&format!("}} {}_Tag;\n\n", type_name));

            // Generate variant structs if they have fields
            for variant in &adt.variants {
                if !variant.fields.is_empty() {
                    output.push_str(&format!("struct {}_{} {{\n", type_name, variant.name));
                    for field in &variant.fields {
                        let field_ty = ir.types[field.ty].as_ref().unwrap();
                        let (base_type, suffix) = type_to_c_decl(field_ty, ir);
                        output.push_str(&format!("    {} {}{};\n", base_type, field.name, suffix));
                    }
                    output.push_str("};\n\n");
                }
            }

            // Generate the main tagged union
            output.push_str(&format!("struct {} {{\n", type_name));
            output.push_str(&format!("    {}_Tag tag;\n", type_name));
            output.push_str("    union {\n");
            for variant in &adt.variants {
                if !variant.fields.is_empty() {
                    output.push_str(&format!(
                        "        struct {}_{} {};\n",
                        type_name,
                        variant.name,
                        variant.name.to_lowercase()
                    ));
                }
            }
            output.push_str("    } data;\n");
            output.push_str("};\n");
        }
        AdtKind::Union => {
            output.push_str(&format!("union {} {{\n", type_name));

            for variant in &adt.variants {
                for field in &variant.fields {
                    let field_ty = ir.types[field.ty].as_ref().unwrap();
                    let (base_type, suffix) = type_to_c_decl(field_ty, ir);
                    output.push_str(&format!("    {} {}{};\n", base_type, field.name, suffix));
                }
            }

            output.push_str("};\n");
        }
    }

    output
}

/// Returns the C type index for a given TypeIndex.
pub fn type_index_to_c(type_idx: TypeIndex, ir: &IR) -> String {
    let ty = ir.types[type_idx].as_ref().unwrap();
    type_to_c(ty, ir)
}

/// Returns the C declaration parts for a given TypeIndex.
pub fn type_index_to_c_decl(type_idx: TypeIndex, ir: &IR) -> (String, String) {
    let ty = ir.types[type_idx].as_ref().unwrap();
    type_to_c_decl(ty, ir)
}
