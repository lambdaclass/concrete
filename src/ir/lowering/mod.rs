use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use functions::lower_func;
use structs::lower_struct;

use crate::ir;
use crate::ir::{
    AdtBody, ConstBody, ConstIndex, FnBody, FnIndex, Local, LocalIndex, ModuleBody, ModuleIndex,
    ProgramBody, Statement, StructIndex, TyKind, TypeIndex,
};
use types::lower_type;

use crate::ast::{
    common::TypeName,
    constants::ConstantDef,
    expressions::{FnCallOp, StructInitExpr},
    functions::{FunctionDecl, FunctionDef},
    structs::StructDecl,
    types::{TypeDecl, TypeDescriptor},
};

mod constants;
mod errors;
mod expressions;
mod functions;
mod lower;
mod statements;
mod structs;
mod types;

pub use errors::LoweringError;
pub use lower::lower_programs;

/// Monomorphized symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
    pub method_of: Option<TypeIndex>,
    // This vec contains the specific types of the generics used.
    pub generics: Vec<TypeIndex>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub modules: HashMap<String, ModuleIndex>,
    pub functions: HashMap<Symbol, FnIndex>,
    pub constants: HashMap<String, ConstIndex>,
    pub structs: HashMap<Symbol, StructIndex>,
    pub types: HashMap<String, TypeIndex>,
}

#[derive(Debug, Clone, Default)]
pub struct Bodies {
    pub structs: HashMap<StructIndex, StructDecl>,
    pub functions: HashMap<FnIndex, FunctionDef>,
    pub functions_decls: HashMap<FnIndex, FunctionDecl>,
    pub types: HashMap<TypeIndex, TypeDecl>,
    pub constants: HashMap<ConstIndex, ConstantDef>,
}

/// Context to help build the IR.
#[derive(Debug, Clone)]
pub struct IRBuilder {
    pub ir: ProgramBody,
    pub symbols: HashMap<ModuleIndex, SymbolTable>,
    pub top_level_modules_names: HashMap<String, ModuleIndex>,
    pub current_generics_map: HashMap<String, TypeName>,
    pub self_ty: Option<TypeIndex>,
    pub bodies: Bodies,
    pub local_module: Option<ModuleIndex>,
    // Needed to not duplicate TypeIndexes for structs.
    pub struct_to_type_idx: HashMap<StructIndex, TypeIndex>,
}

#[derive(Debug)]
pub struct FnIrBuilder<'b> {
    pub body: FnBody,
    pub name_to_local: HashMap<String, LocalIndex>,
    pub statements: Vec<Statement>,
    pub ret_local: LocalIndex,
    pub builder: &'b mut IRBuilder,
    // To check when a variable is used before its declared/init
    pub local_exists: HashSet<LocalIndex>,
}

impl IRBuilder {
    pub fn get_type(&self, idx: TypeIndex) -> &TyKind {
        self.ir.types[idx].as_ref().unwrap()
    }

    pub fn get_function(&self, idx: FnIndex) -> &FnBody {
        self.ir.functions[idx].as_ref().unwrap()
    }

    pub fn get_struct(&self, idx: StructIndex) -> &AdtBody {
        self.ir.structs[idx].as_ref().unwrap()
    }

    pub fn get_constant(&self, idx: ConstIndex) -> &ConstBody {
        self.ir.constants[idx].as_ref().unwrap()
    }

    /// Gets the mangled name for the given function name.
    pub fn get_mangled_name(
        &self,
        module_idx: ModuleIndex,
        fn_name: &str,
        fn_idx: FnIndex,
    ) -> Option<String> {
        let mut name_path: Vec<&str> = Vec::new();

        let cur_module = &self.ir.modules[module_idx];

        for parent_id in &cur_module.parents {
            let module = &self.ir.modules[*parent_id];
            name_path.push(module.name.as_ref());
        }
        name_path.push(cur_module.name.as_ref());

        name_path.push(fn_name);

        Some(format!("{}@{}", name_path.join("::"), fn_idx.to_idx()))
    }

    /// Gets the struct index for the given StructInitExpr (+ using the current generics map in builder).
    /// If the given struct isn't lowered yet its gets lowered (generics).
    pub fn get_or_lower_for_struct_init(
        &mut self,
        info: &StructInitExpr,
    ) -> Result<StructIndex, LoweringError> {
        let sym = Symbol {
            name: info.name.name.name.clone(),
            method_of: None,
            generics: Vec::new(),
        };

        let module_idx = self.local_module.unwrap();

        let poly_idx = *self.symbols[&module_idx].structs.get(&sym).unwrap();
        let struct_decl = self.bodies.structs.get(&poly_idx).unwrap().clone();

        let old_generic_params = self.current_generics_map.clone();

        for (gen_ty, gen_param) in info.name.generics.iter().zip(struct_decl.generics.iter()) {
            self.current_generics_map
                .insert(gen_param.name.name.clone(), gen_ty.clone());
        }

        let id = lower_struct(self, &struct_decl)?;

        self.current_generics_map = old_generic_params;

        Ok(id)
    }
}

impl FnIrBuilder<'_> {
    /// Gets the module where this fn is located.
    pub fn get_module_body(&self) -> &ModuleBody {
        &self.builder.ir.modules[self.builder.local_module.unwrap()]
    }

    pub fn get_module_idx(&self) -> ModuleIndex {
        self.builder.local_module.unwrap()
    }

    /// Gets the local module sym table.
    pub fn get_symbols_table(&self) -> &SymbolTable {
        &self.builder.symbols[&self.builder.local_module.unwrap()]
    }

    pub fn add_local(&mut self, local: Local) -> LocalIndex {
        let id = self.body.locals.len();
        self.body.locals.push(local);
        id
    }

    pub fn add_temp_local(&mut self, ty: TypeIndex) -> LocalIndex {
        let id = self.body.locals.len();
        self.body.locals.push(Local::temp(ty));
        id
    }

    pub fn get_local(&self, name: &str) -> Option<&Local> {
        self.body.locals.get(*(self.name_to_local.get(name)?))
    }

    pub fn get_file_path(&self) -> &PathBuf {
        &self.get_module_body().file_path
    }

    /// Returns the polymorphic id and optionally the monomorphized id.
    ///
    /// If the function/method is generic and hasn't been lowered, it gets lowered.
    pub fn get_id_for_fn_call(
        &mut self,
        info: &FnCallOp,
        method_ty_idx: Option<TypeIndex>,
    ) -> Result<(FnIndex, Option<FnIndex>), LoweringError> {
        let fn_id = {
            let symbols = self.get_symbols_table();

            let mut generic_types = Vec::new();

            let poly_symbol = Symbol {
                name: info.target.name.clone(),
                method_of: method_ty_idx,
                generics: Vec::new(),
            };

            if let Some(poly_id) = symbols.functions.get(&poly_symbol).copied() {
                if !info.generics.is_empty() {
                    let old_generics = self.builder.current_generics_map.clone();
                    let fn_decl = self.builder.bodies.functions.get(&poly_id).unwrap().clone();

                    // TODO: Generic param type inference should be added around here.

                    for (gen_ty, gen_param) in
                        info.generics.iter().zip(fn_decl.decl.generic_params.iter())
                    {
                        let ty = lower_type(
                            self.builder,
                            &TypeDescriptor::Type {
                                name: gen_ty.clone(),
                                span: gen_ty.span,
                            },
                        )?;
                        generic_types.push(ty);
                        self.builder
                            .current_generics_map
                            .insert(gen_param.name.name.clone(), gen_ty.clone());
                    }

                    assert_eq!(
                        generic_types.len(),
                        fn_decl.decl.generic_params.len(),
                        "generic param mismatch"
                    );

                    let mono_symbol = Symbol {
                        name: info.target.name.clone(),
                        method_of: method_ty_idx,
                        generics: generic_types,
                    };

                    let symbols = self.get_symbols_table(); // needed for borrowck
                    let id = {
                        if let Some(id) = symbols.functions.get(&mono_symbol).copied() {
                            id
                        } else {
                            let fn_decl =
                                self.builder.bodies.functions.get(&poly_id).unwrap().clone();

                            lower_func(self.builder, &fn_decl, method_ty_idx)?
                        }
                    }; // todo error

                    self.builder.current_generics_map = old_generics;
                    (poly_id, Some(id))
                } else {
                    (poly_id, None)
                }
            } else {
                panic!("fn not found")
            }
        };

        Ok(fn_id)
    }
}
