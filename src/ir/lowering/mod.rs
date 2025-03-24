use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::Arc,
};

use adts::{lower_enum, lower_struct};
use functions::{lower_func, lower_func_decl};
use itertools::Itertools;

use crate::{
    ast::enums::EnumDecl,
    ir::{self, ConstKind, ConstValue, Mutability, ValueTree},
};
use crate::{
    ast::expressions::EnumInitExpr,
    ir::{
        AdtBody, AdtIndex, ConstBody, ConstIndex, FnIndex, Function, IR, Local, LocalIndex, Module,
        ModuleIndex, Statement, Type, TypeIndex,
    },
};
use types::lower_type;

use crate::ast::{
    constants::ConstantDef,
    expressions::{FnCallOp, StructInitExpr},
    functions::{FunctionDecl, FunctionDef},
    structs::StructDecl,
    types::{TypeDecl, TypeDescriptor},
};

mod adts;
mod constants;
mod errors;
mod expressions;
mod functions;
mod lower;
mod statements;
mod types;

pub use errors::LoweringError;
pub use lower::lower_compile_units;

/// A symbol (currently either a struct/adt or function).
///
/// Symbols are used when lowering, to find the correct IR if it's already lowered
/// or to find the polymorphic AST body, which can be done by using the name without specifying any generic types
/// within this struct.
///
/// This is used mostly with the SymbolTable struct, which itself is used to find the AST body.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
    /// Whether this symbol is a method of the given type.
    pub method_of: Option<TypeIndex>,
    /// This vec contains the specific types of the generics used.
    pub generics: Vec<TypeIndex>,
}

/// A symbol table to map names to indexes.
#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub modules: HashMap<String, ModuleIndex>,
    pub functions: HashMap<Symbol, (FnIndex, ModuleIndex)>,
    pub constants: HashMap<String, ConstIndex>,
    pub aggregates: HashMap<Symbol, AdtIndex>,
    pub types: HashMap<String, TypeIndex>,
}

/// A Struct holding the AST bodies of the given structures.
/// Needed to make lowering overall easier and for generics.
#[derive(Debug, Clone, Default)]
pub struct Bodies {
    pub structs: HashMap<AdtIndex, Arc<StructDecl>>,
    pub enums: HashMap<AdtIndex, Arc<EnumDecl>>,
    pub functions: HashMap<FnIndex, Arc<FunctionDef>>,
    pub functions_decls: HashMap<FnIndex, Arc<FunctionDecl>>,
    pub types: HashMap<TypeIndex, Arc<TypeDecl>>,
    pub constants: HashMap<ConstIndex, Arc<ConstantDef>>,
}

/// Context to help build the IR.
#[derive(Debug, Clone)]
pub struct IRBuilder {
    /// The IR the builder is building.
    pub ir: IR,
    /// A map from module id to symbol table for the given module.
    pub symbols: HashMap<ModuleIndex, SymbolTable>,
    /// The top level module names, used in imports.
    pub top_level_modules_names: HashMap<String, ModuleIndex>,
    pub bodies: Bodies,
    /// Needed to not duplicate TypeIndexes for structs.
    pub adt_to_type_idx: HashMap<AdtIndex, TypeIndex>,
    /// Type to module id where it resides, needed to find methods for the given types.
    pub type_to_module: HashMap<TypeIndex, ModuleIndex>,
    /// A map to find the polymorphic type id of the given monomorphized type.
    pub mono_type_to_poly: HashMap<TypeIndex, TypeIndex>,
    /// A helper context to resolve context dependent information.
    /// Like the current generic mappings or the current module id.
    pub context: IRBuilderContext,
}

#[derive(Debug, Clone)]
pub struct IRBuilderContext {
    /// The type used to resolve "self".
    pub self_ty: Option<TypeIndex>,
    /// The current name to type index mapping to resolve generic type names in the current context.
    pub generics_mapping: HashMap<String, TypeIndex>,
    /// A stack for the current module id.
    ///
    /// Sometimes we need to temporarely enter the "context" of another module, e.g when handling
    /// a generic function from a import that needs to be lowered at the time of a function call
    /// from another module.
    pub module_stack: Vec<ModuleIndex>,
}

#[derive(Debug)]
pub struct FnIrBuilder<'b> {
    pub body: Function,
    pub name_to_local: HashMap<String, LocalIndex>,
    pub statements: Vec<Statement>,
    pub ret_local: LocalIndex,
    pub builder: &'b mut IRBuilder,
    pub fn_id: FnIndex,
    // To check when a variable is used before its declared/init
    pub local_exists: HashSet<LocalIndex>,
}

impl IRBuilder {
    /// Gets the module in the current context.
    pub fn get_current_module(&self) -> &Module {
        &self.ir.modules[self.get_current_module_idx()]
    }

    /// Gets the module id in the current context.
    pub fn get_current_module_idx(&self) -> ModuleIndex {
        *self
            .context
            .module_stack
            .last()
            .expect("There should always be a module in context")
    }

    pub fn enter_module_context(&mut self, module_id: ModuleIndex) {
        self.context.module_stack.push(module_id);
    }

    pub fn leave_module_context(&mut self) {
        self.context.module_stack.pop();
    }

    pub fn get_type(&self, idx: TypeIndex) -> &Type {
        self.ir.types[idx].as_ref().unwrap()
    }

    pub fn get_function(&self, idx: FnIndex) -> &Function {
        self.ir.functions[idx].as_ref().unwrap()
    }

    pub fn get_adt(&self, idx: AdtIndex) -> &AdtBody {
        self.ir.aggregates[idx].as_ref().unwrap()
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

        Some(format!("{}_{}", name_path.join("_"), fn_idx.to_idx()))
    }

    /// Gets the mangled name for the given function name.
    pub fn get_debug_name(&self, module_idx: ModuleIndex, fn_name: &str) -> Option<String> {
        let mut name_path: Vec<&str> = Vec::new();

        let cur_module = &self.ir.modules[module_idx];

        for parent_id in &cur_module.parents {
            let module = &self.ir.modules[*parent_id];
            name_path.push(module.name.as_ref());
        }
        name_path.push(cur_module.name.as_ref());

        name_path.push(fn_name);

        Some(name_path.join("::"))
    }

    /// Gets the struct index for the given StructInitExpr (+ using the current generics map in builder).
    /// If the given struct isn't lowered yet its gets lowered (generics).
    pub fn get_or_lower_for_struct_init(
        &mut self,
        info: &StructInitExpr,
    ) -> Result<AdtIndex, LoweringError> {
        let sym = Symbol {
            name: info.name.name.name.clone(),
            method_of: None,
            generics: Vec::new(),
        };

        let module_idx = self.get_current_module_idx();

        let poly_idx = *self.symbols[&module_idx].aggregates.get(&sym).unwrap();
        let struct_decl = self.bodies.structs.get(&poly_idx).unwrap().clone();

        let old_generic_params = self.context.generics_mapping.clone();

        for (gen_ty, gen_param) in info.name.generics.iter().zip(struct_decl.generics.iter()) {
            let ty = lower_type(
                self,
                &TypeDescriptor::Type {
                    name: gen_ty.clone(),
                    span: gen_ty.span,
                },
            )?;
            self.context
                .generics_mapping
                .insert(gen_param.name.name.clone(), ty);
        }

        let id = lower_struct(self, &struct_decl)?;

        self.context.generics_mapping = old_generic_params;

        Ok(id)
    }

    /// Gets the struct index for the given EnumInitExpr (+ using the current generics map in builder).
    /// If the given enum isn't lowered yet its gets lowered (generics).
    pub fn get_or_lower_for_enum_init(
        &mut self,
        info: &EnumInitExpr,
    ) -> Result<AdtIndex, LoweringError> {
        let sym = Symbol {
            name: info.name.name.name.clone(),
            method_of: None,
            generics: Vec::new(),
        };

        let module_idx = self.get_current_module_idx();

        let poly_idx = *self.symbols[&module_idx].aggregates.get(&sym).unwrap();
        let enum_decl = self.bodies.enums.get(&poly_idx).unwrap().clone();

        let old_generic_params = self.context.generics_mapping.clone();

        for (gen_ty, gen_param) in info.name.generics.iter().zip(enum_decl.generics.iter()) {
            let ty = lower_type(
                self,
                &TypeDescriptor::Type {
                    name: gen_ty.clone(),
                    span: gen_ty.span,
                },
            )?;
            self.context
                .generics_mapping
                .insert(gen_param.name.name.clone(), ty);
        }

        let id = lower_enum(self, &enum_decl)?;

        self.context.generics_mapping = old_generic_params;

        Ok(id)
    }

    /// Format a type for displaying as a type name.
    pub fn display_typename(&self, id: TypeIndex) -> String {
        let ty = self.get_type(id);

        match ty {
            Type::Array(index, const_data) => {
                let value = if let ConstKind::Value(ValueTree::Leaf(ConstValue::U64(x))) =
                    &const_data.data
                {
                    *x
                } else {
                    unreachable!("const data for array sizes should always be u64")
                };
                format!("[{}; {}]", self.display_typename(*index), value)
            }
            Type::Ref(index, mutability) => {
                let word = if let Mutability::Mut = mutability {
                    "mut"
                } else {
                    "const"
                };

                format!("&{word} {}", self.display_typename(*index))
            }
            Type::Ptr(index, mutability) => {
                let word = if let Mutability::Mut = mutability {
                    "mut"
                } else {
                    "const"
                };

                format!("*{word} {}", self.display_typename(*index))
            }
            Type::Adt(index) => {
                if let Some(Some(adt_body)) = self.ir.aggregates.get(*index) {
                    let generics = match adt_body.kind {
                        ir::AdtKind::Struct => self
                            .bodies
                            .structs
                            .get(index)
                            .unwrap()
                            .generics
                            .iter()
                            .map(|x| x.name.name.clone())
                            .collect_vec(),
                        ir::AdtKind::Enum => self
                            .bodies
                            .enums
                            .get(index)
                            .unwrap()
                            .generics
                            .iter()
                            .map(|x| x.name.name.clone())
                            .collect_vec(),
                        ir::AdtKind::Union => todo!(),
                    };

                    let mut result = String::new();
                    result.push_str(&adt_body.name);

                    if !generics.is_empty() {
                        result.push('<');

                        for (i, g) in generics.iter().enumerate() {
                            let ty = adt_body.generics_used.get(g).unwrap();
                            result.push_str(&self.display_typename(*ty));

                            if i != generics.len() - 1 {
                                result.push_str(", ");
                            }
                        }
                        result.push('>');
                    }

                    result
                } else {
                    "Unknown yet".to_string()
                }
            }
            _ => ty.display(&self.ir).unwrap(),
        }
    }
}

impl FnIrBuilder<'_> {
    /// Gets the module where this fn is located.
    pub fn get_current_module(&self) -> &Module {
        self.builder.get_current_module()
    }

    pub fn get_current_module_idx(&self) -> ModuleIndex {
        self.builder.get_current_module_idx()
    }

    pub fn enter_module_context(&mut self, module_id: ModuleIndex) {
        self.builder.context.module_stack.push(module_id);
    }

    pub fn leave_module_context(&mut self) {
        self.builder.context.module_stack.pop();
    }

    /// Gets the local module sym table.
    pub fn get_symbols_table(&self) -> &SymbolTable {
        &self.builder.symbols[&self.get_current_module_idx()]
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
        &self.get_current_module().file_path
    }

    /// Returns the polymorphic id and optionally the monomorphized id.
    ///
    /// If the function/method is generic and hasn't been lowered, it gets lowered.
    pub fn get_id_for_fn_call(
        &mut self,
        info: &FnCallOp,
        method_of_type_idx: Option<TypeIndex>,
    ) -> Result<(FnIndex, Option<FnIndex>), LoweringError> {
        // If the function call is a method of the given type,
        // we need to handle the case were the given type is polymorphic.
        // The passed id here will be the monomorphic version in that case, so
        // we need to get the polymorphic type id of this type
        // to be able to get the function.
        let mut polymorphic_method_of_type_idx = method_of_type_idx;

        if let Some(method_ty_idx) = method_of_type_idx {
            if let Some(ty) = self.builder.mono_type_to_poly.get(&method_ty_idx) {
                polymorphic_method_of_type_idx = Some(*ty);
            }
        }

        let module_id = if let Some(id) = polymorphic_method_of_type_idx {
            self.builder.type_to_module.get(&id).copied().unwrap()
        } else {
            self.builder.get_current_module_idx()
        };

        let poly_symbol = Symbol {
            name: info.target.name.clone(),
            method_of: polymorphic_method_of_type_idx,
            generics: Vec::new(),
        };

        let fn_id = {
            let symbols = self.builder.symbols.get(&module_id).unwrap();

            let mut generic_types = Vec::new();

            if let Some((poly_id, fn_module_id)) = symbols.functions.get(&poly_symbol).copied() {
                // If function is generic or the self type is generic, monomorphize here.
                if !info.generics.is_empty() || polymorphic_method_of_type_idx != method_of_type_idx
                {
                    let old_generics = self.builder.context.generics_mapping.clone();
                    let fn_decl = self
                        .builder
                        .bodies
                        .functions
                        .get(&poly_id)
                        .map(|x| x.decl.clone())
                        .or_else(|| self.builder.bodies.functions_decls.get(&poly_id).cloned())
                        .clone()
                        .unwrap();

                    // TODO: Generic param type inference should be added around here.

                    for (gen_ty, gen_param) in
                        info.generics.iter().zip(fn_decl.generic_params.iter())
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
                            .context
                            .generics_mapping
                            .insert(gen_param.name.name.clone(), ty);
                    }

                    assert_eq!(
                        generic_types.len(),
                        fn_decl.generic_params.len(),
                        "generic param mismatch"
                    );

                    let mono_symbol = Symbol {
                        name: info.target.name.clone(),
                        method_of: method_of_type_idx,
                        generics: generic_types,
                    };

                    let symbols = self.builder.symbols.get(&module_id).unwrap(); // needed for borrowck
                    let id = {
                        if let Some(id) = symbols.functions.get(&mono_symbol).copied() {
                            id.0
                        } else {
                            // Add the id from here to avoid infinite recursion on recursive functions.
                            let id = self.builder.ir.functions.insert(None);
                            self.builder
                                .symbols
                                .get_mut(&module_id)
                                .unwrap()
                                .functions
                                .insert(mono_symbol.clone(), (id, fn_module_id));

                            self.builder
                                .symbols
                                .get_mut(&fn_module_id)
                                .unwrap()
                                .functions
                                .insert(mono_symbol, (id, fn_module_id));

                            if let Some(fn_def) =
                                self.builder.bodies.functions.get(&poly_id).cloned()
                            {
                                // Temporarly set the local module to the module where the function is defined to lower it in the correct context.
                                self.enter_module_context(fn_module_id);

                                let lowered_id =
                                    lower_func(self.builder, &fn_def, method_of_type_idx)?;

                                self.builder.leave_module_context();

                                assert_eq!(id, lowered_id);
                            } else if let Some(fn_decl) =
                                self.builder.bodies.functions_decls.get(&poly_id).cloned()
                            {
                                // Temporarly set the local module to the module where the function is defined to lower it in the correct context.
                                self.enter_module_context(fn_module_id);

                                let lowered_id = lower_func_decl(self.builder, &fn_decl)?;

                                self.leave_module_context();

                                assert_eq!(id, lowered_id);
                            }
                            id
                        }
                    };

                    self.builder.context.generics_mapping = old_generics;
                    (poly_id, Some(id))
                } else {
                    (poly_id, None)
                }
            } else {
                return Err(LoweringError::FunctionNotFound {
                    span: info.span,
                    function: info.target.name.clone(),
                    path: self.get_file_path().clone(),
                });
            }
        };

        Ok(fn_id)
    }
}
