use std::{collections::HashMap, error::Error};

use concrete_ast::{functions::FunctionDef, structs::StructDecl, types::TypeSpec};
use concrete_check::ast_helper::ModuleInfo;
use melior::{
    dialect::llvm,
    ir::{
        r#type::{IntegerType, MemRefType},
        Type,
    },
    Context as MeliorContext,
};

use crate::codegen::LocalVar;

#[derive(Debug, Clone)]
pub struct ScopeContext<'ctx, 'parent: 'ctx> {
    pub locals: HashMap<String, LocalVar<'ctx, 'parent>>,
    pub function: Option<FunctionDef>,
    pub imports: HashMap<String, &'parent ModuleInfo<'parent>>,
    pub module_info: &'parent ModuleInfo<'parent>,
}

impl<'ctx, 'parent> ScopeContext<'ctx, 'parent> {
    /// Returns the symbol name from a local name.
    pub fn get_symbol_name(&self, local_name: &str) -> String {
        if local_name == "main" {
            return local_name.to_string();
        }

        if let Some(module) = self.imports.get(local_name) {
            // a import
            module.get_symbol_name(local_name)
        } else {
            let mut result = self.module_info.name.clone();

            result.push_str("::");
            result.push_str(local_name);

            result
        }
    }

    pub fn get_function(&self, local_name: &str) -> Option<&FunctionDef> {
        if let Some(module) = self.imports.get(local_name) {
            // a import
            module.functions.get(local_name).copied()
        } else {
            self.module_info.functions.get(local_name).copied()
        }
    }

    /// Returns the size in bytes for a type.
    fn get_type_size(&self, type_info: &TypeSpec) -> Result<u32, Box<dyn Error>> {
        Ok(match type_info {
            TypeSpec::Simple { name, .. } => match name.name.as_str() {
                "u128" | "i128" => 16,
                "u64" | "i64" => 8,
                "u32" | "i32" => 4,
                "u16" | "i16" => 2,
                "u8" | "i8" => 1,
                "f64" => 8,
                "f32" => 4,
                "bool" => 1,
                name => {
                    if let Some(x) = self.module_info.structs.get(name) {
                        let mut size = 0u32;

                        for field in &x.fields {
                            let ty_size = self.get_type_size(&field.r#type)?;
                            let ty_align = self.get_align_for_size(ty_size);

                            // Calculate padding needed.
                            let size_rounded_up = size.wrapping_add(ty_align).wrapping_sub(1)
                                & !ty_align.wrapping_sub(1u32);
                            let pad = size_rounded_up.wrapping_sub(size);

                            size += pad;
                            size += ty_size;
                        }

                        let struct_align = self.get_align_for_size(size);
                        let size_rounded_up = size.wrapping_add(struct_align).wrapping_sub(1)
                            & !struct_align.wrapping_sub(1u32);
                        let pad = size_rounded_up.wrapping_sub(size);

                        size + pad
                    } else if let Some(module) = self.imports.get(name) {
                        // a import
                        self.get_type_size(
                            &module.types.get(name).expect("failed to find type").value,
                        )?
                    } else {
                        self.get_type_size(
                            &self
                                .module_info
                                .types
                                .get(name)
                                .expect("failed to find type")
                                .value,
                        )?
                    }
                }
            },
            TypeSpec::Generic { .. } => todo!(),
            TypeSpec::Array { of_type, size, .. } => {
                self.get_type_size(of_type)? * size.unwrap_or(1u32)
            }
        })
    }

    fn get_align_for_size(&self, size: u32) -> u32 {
        if size <= 1 {
            1
        } else if size <= 2 {
            2
        } else if size <= 4 {
            4
        } else {
            8
        }
    }

    /// Returns the struct type along with the field indexes.
    pub fn get_struct_type(
        &self,
        context: &'ctx MeliorContext,
        strct: &StructDecl,
    ) -> Result<(Type<'ctx>, HashMap<String, usize>), Box<dyn Error>> {
        let mut fields = Vec::with_capacity(strct.fields.len());

        let mut field_indexes = HashMap::new();
        let mut size: u32 = 0;

        for field in &strct.fields {
            let ty = self.resolve_type_spec(context, &field.r#type)?;
            let ty_size = self.get_type_size(&field.r#type)?;
            let ty_align = self.get_align_for_size(ty_size);

            // Calculate padding needed.
            let size_rounded_up =
                size.wrapping_add(ty_align).wrapping_sub(1) & !ty_align.wrapping_sub(1u32);
            let pad = size_rounded_up.wrapping_sub(size);

            if pad > 0 {
                fields.push(llvm::r#type::array(
                    IntegerType::new(context, 8).into(),
                    pad,
                ));
            }

            size += pad;
            size += ty_size;
            field_indexes.insert(field.name.name.clone(), fields.len());
            fields.push(ty);
        }

        let struct_align = self.get_align_for_size(size);

        // Calculate padding needed for whole struct.
        let size_rounded_up =
            size.wrapping_add(struct_align).wrapping_sub(1) & !struct_align.wrapping_sub(1u32);
        let pad = size_rounded_up.wrapping_sub(size);

        if pad > 0 {
            fields.push(llvm::r#type::array(
                IntegerType::new(context, 8).into(),
                pad,
            ));
        }

        Ok((
            llvm::r#type::r#struct(context, &fields, false),
            field_indexes,
        ))
    }

    pub fn resolve_type(
        &self,
        context: &'ctx MeliorContext,
        name: &str,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        Ok(match name {
            "u64" | "i64" => IntegerType::new(context, 64).into(),
            "u32" | "i32" => IntegerType::new(context, 32).into(),
            "u16" | "i16" => IntegerType::new(context, 16).into(),
            "u8" | "i8" => IntegerType::new(context, 8).into(),
            "f32" => Type::float32(context),
            "f64" => Type::float64(context),
            "bool" => IntegerType::new(context, 1).into(),
            name => {
                if let Some(strct) = self.module_info.structs.get(name) {
                    self.get_struct_type(context, strct)?.0
                } else if let Some(module) = self.imports.get(name) {
                    // a import
                    self.resolve_type_spec(
                        context,
                        &module.types.get(name).expect("failed to find type").value,
                    )?
                } else {
                    self.resolve_type_spec(
                        context,
                        &self
                            .module_info
                            .types
                            .get(name)
                            .expect("failed to find type")
                            .value,
                    )?
                }
            }
        })
    }

    pub fn resolve_type_spec(
        &self,
        context: &'ctx MeliorContext,
        spec: &TypeSpec,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        match spec.is_ref() {
            Some(_) => {
                Ok(
                    MemRefType::new(self.resolve_type_spec_ref(context, spec)?, &[], None, None)
                        .into(),
                )
            }
            None => self.resolve_type_spec_ref(context, spec),
        }
    }

    /// Resolves the type this ref points to.
    pub fn resolve_type_spec_ref(
        &self,
        context: &'ctx MeliorContext,
        spec: &TypeSpec,
    ) -> Result<Type<'ctx>, Box<dyn Error>> {
        Ok(match spec {
            TypeSpec::Simple { name, .. } => self.resolve_type(context, &name.name)?,
            TypeSpec::Generic { name, .. } => self.resolve_type(context, &name.name)?,
            TypeSpec::Array { .. } => {
                todo!("implement arrays")
            }
        })
    }

    pub fn is_type_signed(&self, type_info: &TypeSpec) -> bool {
        let signed = ["i8", "i16", "i32", "i64", "i128"];
        match type_info {
            TypeSpec::Simple { name, .. } => signed.contains(&name.name.as_str()),
            TypeSpec::Generic { name, .. } => signed.contains(&name.name.as_str()),
            TypeSpec::Array { .. } => unreachable!(),
        }
    }

    pub fn is_float(&self, type_info: &TypeSpec) -> bool {
        let signed = ["f32", "f64"];
        match type_info {
            TypeSpec::Simple { name, .. } => signed.contains(&name.name.as_str()),
            TypeSpec::Generic { name, .. } => signed.contains(&name.name.as_str()),
            TypeSpec::Array { .. } => unreachable!(),
        }
    }
}
