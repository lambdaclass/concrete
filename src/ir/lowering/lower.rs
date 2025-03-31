use std::collections::{HashMap, HashSet};

use tracing::debug;

use crate::{
    ast::{self, modules::ModuleDefItem},
    ir::{
        Adts, Constants, Functions, Module, Modules, Types,
        lowering::{
            Bodies, IRBuilder,
            errors::{LoweringError, MissingTraitType, UnexpectedTraitType},
        },
    },
};

use super::{
    IRBuilderContext, Symbol,
    adts::{lower_enum, lower_struct},
    constants::lower_constant,
    errors::{MissingTraitFunction, UnexpectedTraitFunction},
    functions::{lower_func, lower_func_decl},
    ir::{IR, ModuleIndex, Type},
    traits::{TraitDatabase, TraitGeneric, TraitImpl},
    types::{lower_type, lower_type_decl},
};

/// Lowers the ast compile units, the last should be the "main" unit whose unit tests are saved.
pub fn lower_compile_units(compile_units: &[ast::CompilationUnit]) -> Result<IR, LoweringError> {
    let mut builder = IRBuilder {
        ir: IR {
            types: Types::new(),
            functions: Functions::new(),
            aggregates: Adts::new(),
            constants: Constants::new(),
            modules: Modules::new(),
            top_level_modules: Vec::new(),
            builtin_types: Default::default(),
            tests: Vec::new(),
        },
        symbols: Default::default(),
        top_level_modules_names: Default::default(),
        adt_to_type_idx: Default::default(),
        type_to_module: Default::default(),
        bodies: Bodies::default(),
        mono_type_to_poly: Default::default(),
        trait_db: TraitDatabase::new(),
        context: IRBuilderContext {
            add_tests: false,
            self_ty: None,
            generics_mapping: Default::default(),
            module_stack: Vec::with_capacity(8),
        },
    };

    // Prepass to fill some symbols.
    let last_i = compile_units.len() - 1;
    for (i, compile_unit) in compile_units.iter().enumerate() {
        builder.context.add_tests = i == last_i;
        for module in &compile_unit.modules {
            debug!("Lowering symbols for module {:?}", module.name.name);
            lower_module_symbols(&mut builder, module, &[])?;
        }
    }

    // Handle imports so they are transparent afterwards.
    for (i, compile_unit) in compile_units.iter().enumerate() {
        builder.context.add_tests = i == last_i;
        for module in &compile_unit.modules {
            debug!("lowering imports for module: {:?}", &module.name.name);
            let module_idx = *builder
                .top_level_modules_names
                .get(&module.name.name)
                .expect("should exist");

            lower_imports(&mut builder, module, module_idx, &[])?;
        }
    }

    for (i, compile_unit) in compile_units.iter().enumerate() {
        builder.context.add_tests = i == last_i;
        for module in &compile_unit.modules {
            let module_idx = *builder
                .top_level_modules_names
                .get(&module.name.name)
                .unwrap();
            lower_module(&mut builder, module, module_idx)?;
        }
    }

    Ok(builder.ir)
}

fn lower_module_symbols(
    builder: &mut IRBuilder,
    module: &ast::modules::Module,
    parents: &[ModuleIndex],
) -> Result<ModuleIndex, LoweringError> {
    add_builtins(builder);

    let module_body = Module {
        name: module.name.name.clone(),
        parents: parents.to_vec(),
        functions: HashSet::new(),
        aggregates: HashSet::new(),
        types: HashSet::new(),
        constants: HashSet::new(),
        modules: HashMap::new(),
        span: module.span,
        file_path: module.file_path.clone(),
    };

    // Add the empty module body to the arena.
    let module_idx = builder.ir.modules.insert(module_body);
    builder.enter_module_context(module_idx);

    // If its a top level module, save it as such.
    if parents.is_empty() {
        debug!("Adding module {:?} to top level modules", &module.name.name);
        builder
            .top_level_modules_names
            .insert(module.name.name.clone(), module_idx);
        builder.ir.top_level_modules.push(module_idx);
    } else {
        debug!(
            "Adding module {:?} to parent module {:?}",
            &module.name.name,
            &builder.ir.modules[*parents.last().unwrap()].name
        );
        builder
            .symbols
            .get_mut(parents.last().unwrap())
            .unwrap()
            .modules
            .insert(module.name.name.clone(), module_idx);
        builder.ir.modules[*parents.last().unwrap()]
            .modules
            .insert(module.name.name.clone(), module_idx);
    }

    // Populate the symbol table.
    builder.symbols.entry(module_idx).or_default();

    for item in &module.contents {
        match item {
            ast::modules::ModuleDefItem::Trait(trait_decl) => {
                builder.trait_db.add_trait(trait_decl.clone(), module_idx);
                // Here we don't have any function, we do this on the impl trait block.
            }
            ast::modules::ModuleDefItem::ImplTrait(_impl_trait) => { /* Done in second pass */ }
            ast::modules::ModuleDefItem::Constant(constant_def) => {
                let idx = builder.ir.constants.insert(None);
                builder.bodies.constants.insert(idx, constant_def.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .constants
                    .insert(constant_def.decl.name.name.clone(), idx);
            }
            ast::modules::ModuleDefItem::Function(function_def) => {
                let idx = builder.ir.functions.insert(None);
                builder.bodies.functions.insert(idx, function_def.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .functions
                    .insert(
                        Symbol {
                            name: function_def.decl.name.name.clone(),
                            method_of: None,
                            generics: Vec::new(),
                        },
                        (idx, module_idx),
                    );
                builder.ir.modules[module_idx].functions.insert(idx);
                debug!(
                    "Adding function symbol {:?} to module {:?} ({})",
                    function_def.decl.name.name,
                    module.name.name,
                    module_idx.to_idx()
                );
            }
            ast::modules::ModuleDefItem::FunctionDecl(function_decl) => {
                let idx = builder.ir.functions.insert(None);
                builder
                    .bodies
                    .functions_decls
                    .insert(idx, function_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .functions
                    .insert(
                        Symbol {
                            name: function_decl.name.name.clone(),
                            method_of: None,
                            generics: Vec::new(),
                        },
                        (idx, module_idx),
                    );
                builder.ir.modules[module_idx].functions.insert(idx);
                debug!(
                    "Adding function decl symbol {:?} to module {:?} ({})",
                    function_decl.name.name,
                    module.name.name,
                    module_idx.to_idx(),
                );
            }
            ast::modules::ModuleDefItem::Impl(_) => {}
            ast::modules::ModuleDefItem::Struct(struct_decl) => {
                let idx = builder.ir.aggregates.insert(None);
                builder.bodies.structs.insert(idx, struct_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .aggregates
                    .insert(
                        Symbol {
                            name: struct_decl.name.name.clone(),
                            method_of: None,
                            generics: Vec::new(),
                        },
                        idx,
                    );
                builder.ir.modules[module_idx].aggregates.insert(idx);
                let type_idx = builder.ir.types.insert(Some(Type::Adt(idx)));
                builder.ir.modules[module_idx].types.insert(type_idx);
                builder.adt_to_type_idx.insert(idx, type_idx);
                builder.type_to_module.insert(type_idx, module_idx);

                for attr in &struct_decl.attributes {
                    if attr.name == "langitem" {
                        let langitem = attr.value.as_ref().unwrap();

                        match langitem.as_str() {
                            "String" => {
                                builder.ir.builtin_types.insert(Type::String, type_idx);
                            }
                            _ => {
                                return Err(LoweringError::UnknownLangItem {
                                    span: attr.span,
                                    item: attr.name.clone(),
                                    path: builder.get_current_module().file_path.clone(),
                                });
                            }
                        }
                    }
                }

                debug!(
                    "Adding struct symbol {:?} to module {:?}",
                    struct_decl.name.name, module.name.name
                );
            }
            ast::modules::ModuleDefItem::Union(_union_decl) => todo!(),
            ast::modules::ModuleDefItem::Enum(enum_decl) => {
                let idx = builder.ir.aggregates.insert(None);
                builder.bodies.enums.insert(idx, enum_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .aggregates
                    .insert(
                        Symbol {
                            name: enum_decl.name.name.clone(),
                            method_of: None,
                            generics: Vec::new(),
                        },
                        idx,
                    );
                builder.ir.modules[module_idx].aggregates.insert(idx);
                let type_idx = builder.ir.types.insert(Some(Type::Adt(idx)));
                builder.ir.modules[module_idx].types.insert(type_idx);
                builder.adt_to_type_idx.insert(idx, type_idx);
                builder.type_to_module.insert(type_idx, module_idx);

                debug!(
                    "Adding enum symbol {:?} to module {:?}",
                    enum_decl.name.name, module.name.name
                );
            }
            ast::modules::ModuleDefItem::Type(type_decl) => {
                let idx = builder.ir.types.insert(None);
                builder.bodies.types.insert(idx, type_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .types
                    .insert(type_decl.name.name.clone(), idx);
                builder.type_to_module.insert(idx, module_idx);
            }
            ast::modules::ModuleDefItem::Module(submodule) => {
                let mut parents = parents.to_vec();
                parents.push(module_idx);

                lower_module_symbols(builder, submodule, &parents)?;
            }
            ast::modules::ModuleDefItem::ExternalModule(_) => {}
            ast::modules::ModuleDefItem::Import(_) => {}
        }
    }

    // Second pass
    for item in &module.contents {
        match item {
            ast::modules::ModuleDefItem::Impl(impl_block) => {
                let ty = if !impl_block.generic_params.is_empty() {
                    let adt_symbol = Symbol {
                        name: impl_block.target.get_name().unwrap(),
                        method_of: None,
                        generics: Vec::new(),
                    };

                    let id = *builder
                        .symbols
                        .get(&builder.get_current_module_idx())
                        .unwrap()
                        .aggregates
                        .get(&adt_symbol)
                        .unwrap();

                    let type_id = *builder.adt_to_type_idx.get(&id).unwrap();
                    type_id
                } else {
                    lower_type(builder, &impl_block.target)?
                };

                for function_def in &impl_block.methods {
                    debug!(
                        "Adding method {}::{} to module {:?}",
                        impl_block.target, function_def.decl.name.name, module.name.name
                    );
                    let idx = builder.ir.functions.insert(None);
                    builder.bodies.functions.insert(idx, function_def.clone());
                    let sym = Symbol {
                        name: function_def.decl.name.name.clone(),
                        method_of: Some(ty),
                        generics: Vec::new(),
                    };
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .functions
                        .insert(sym.clone(), (idx, module_idx));
                    builder.ir.modules[module_idx].functions.insert(idx);
                }
            }
            ast::modules::ModuleDefItem::ImplTrait(impl_trait) => {
                // Get trait id and verify it exists.
                let _trait_id = builder
                    .trait_db
                    .get_trait_by_name(&impl_trait.target_trait.name.name, module_idx)
                    .ok_or_else(|| LoweringError::TraitNotFound {
                        span: impl_trait.target_trait.span,
                        name: impl_trait.target_trait.name.name.clone(),
                        path: builder.get_current_module().file_path.clone(),
                    })?;

                let ty = if !impl_trait.generic_params.is_empty() {
                    let adt_symbol = Symbol {
                        name: impl_trait.target.get_name().unwrap(),
                        method_of: None,
                        generics: Vec::new(),
                    };

                    let id = *builder
                        .symbols
                        .get(&builder.get_current_module_idx())
                        .unwrap()
                        .aggregates
                        .get(&adt_symbol)
                        .unwrap();

                    let type_id = *builder.adt_to_type_idx.get(&id).unwrap();
                    type_id
                } else {
                    lower_type(builder, &impl_trait.target)?
                };

                // TODO: verify signature and types.

                #[allow(clippy::never_loop)] // remove once implemented
                for assoc_type in &impl_trait.associated_types {
                    Err(LoweringError::Unimplemented {
                        span: assoc_type.span,
                        reason: "Associated types not yet implemented".to_string(),
                        path: builder.get_current_module().file_path.clone(),
                    })?;
                }

                for function_def in &impl_trait.methods {
                    debug!(
                        "Adding trait method impl {}::{} to module {:?}",
                        impl_trait.target, function_def.decl.name.name, module.name.name
                    );
                    let idx = builder.ir.functions.insert(None);
                    builder.bodies.functions.insert(idx, function_def.clone());
                    let sym = Symbol {
                        name: function_def.decl.name.name.clone(),
                        method_of: Some(ty),
                        generics: Vec::new(),
                    };
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .functions
                        .insert(sym.clone(), (idx, module_idx));
                    builder.ir.modules[module_idx].functions.insert(idx);
                }
            }
            _ => {}
        }
    }

    builder.leave_module_context();

    Ok(module_idx)
}

fn lower_imports(
    builder: &mut IRBuilder,
    module: &ast::modules::Module,
    module_idx: ModuleIndex,
    parents: &[ModuleIndex],
) -> Result<(), LoweringError> {
    builder.enter_module_context(module_idx);
    let import_from_path = builder.ir.modules[module_idx].file_path.clone();

    for stmt in &module.contents {
        if let ModuleDefItem::Import(import) = stmt {
            let mut target_module = module_idx;

            let mut root_requested = false;

            for (i, m) in import.module.iter().enumerate() {
                if i == 0 || root_requested {
                    if m.name == "super" && !root_requested {
                        if let Some(parent) = parents.last().copied() {
                            target_module = parent;
                        } else {
                            panic!("no parent found for super, todo turn into error")
                        }
                    } else if m.name == "root" && !root_requested {
                        root_requested = true;
                    } else {
                        let id = if root_requested {
                            *builder
                                .top_level_modules_names
                                .get(&m.name)
                                .ok_or_else(|| LoweringError::ModuleNotFound {
                                    span: m.span,
                                    module: m.name.clone(),
                                    path: import_from_path.to_path_buf(),
                                })?
                        } else {
                            *builder
                                .symbols
                                .get(&target_module)
                                .unwrap()
                                .modules
                                .get(&m.name)
                                .or_else(|| builder.top_level_modules_names.get(&m.name))
                                .ok_or_else(|| LoweringError::ModuleNotFound {
                                    span: m.span,
                                    module: m.name.clone(),
                                    path: import_from_path.to_path_buf(),
                                })?
                        };

                        target_module = id;
                        root_requested = false;
                    }
                    continue;
                }

                let info = &builder.ir.modules[target_module];
                target_module =
                    *info
                        .modules
                        .get(&m.name)
                        .ok_or_else(|| LoweringError::ModuleNotFound {
                            span: m.span,
                            module: m.name.clone(),
                            path: import_from_path.clone(),
                        })?;
            }

            let target_module = target_module;

            for sym in &import.symbols {
                let target_symbols = builder.symbols.get(&target_module).unwrap();

                let symbol = Symbol {
                    name: sym.name.clone(),
                    method_of: None,
                    generics: Vec::new(),
                };
                if let Some((id, mod_id)) = target_symbols.functions.get(&symbol).cloned() {
                    debug!(
                        "Imported function symbol {:?} ({}) to module {} ({})",
                        symbol.name,
                        mod_id.to_idx(),
                        builder.ir.modules[module_idx].name,
                        module_idx.to_idx()
                    );
                    builder.ir.modules[module_idx].functions.insert(id);
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .functions
                        .insert(symbol.clone(), (id, mod_id));
                    continue;
                }

                let target_symbols = builder.symbols.get(&target_module).unwrap();
                if let Some(adt_idx) = target_symbols.aggregates.get(&symbol).cloned() {
                    debug!(
                        "Imported adt symbol {:?} to module {}",
                        symbol, builder.ir.modules[module_idx].name
                    );
                    builder.ir.modules[module_idx].aggregates.insert(adt_idx);
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .aggregates
                        .insert(symbol.clone(), adt_idx);

                    continue;
                }

                let target_symbols = builder.symbols.get(&target_module).unwrap();
                if let Some(id) = target_symbols.types.get(&sym.name).cloned() {
                    debug!(
                        "Imported type symbol {:?} to module {}",
                        symbol, builder.ir.modules[module_idx].name
                    );
                    builder.ir.modules[module_idx].types.insert(id);
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .types
                        .insert(sym.name.clone(), id);
                    continue;
                }

                let target_symbols = builder.symbols.get(&target_module).unwrap();
                if let Some(id) = target_symbols.constants.get(&sym.name).cloned() {
                    debug!(
                        "Imported constant symbol {:?} to module {}",
                        symbol, builder.ir.modules[module_idx].name
                    );
                    builder.ir.modules[module_idx].constants.insert(id);
                    builder
                        .symbols
                        .get_mut(&module_idx)
                        .unwrap()
                        .constants
                        .insert(sym.name.clone(), id);
                    continue;
                }

                Err(LoweringError::ImportNotFound {
                    module_span: module.span,
                    import_span: import.span,
                    symbol: sym.clone(),
                    path: import_from_path.to_path_buf(),
                })?;
            }
        }

        for m in &module.contents {
            if let ast::modules::ModuleDefItem::Module(submodule) = m {
                let mut parents = parents.to_vec();
                parents.push(module_idx);
                let new_module_idx = *builder.ir.modules[module_idx]
                    .modules
                    .get(&submodule.name.name)
                    .expect("failed to find submodule id");
                lower_imports(builder, submodule, new_module_idx, &parents)?;
            }
        }
    }

    builder.leave_module_context();

    Ok(())
}

fn add_builtins(builder: &mut IRBuilder) {
    for kind in [
        (Type::Unit),
        (Type::Bool),
        (Type::Char),
        (Type::Int(super::ir::IntTy::I8)),
        (Type::Int(super::ir::IntTy::I16)),
        (Type::Int(super::ir::IntTy::I32)),
        (Type::Int(super::ir::IntTy::I64)),
        (Type::Int(super::ir::IntTy::I128)),
        (Type::Uint(super::ir::UintTy::U8)),
        (Type::Uint(super::ir::UintTy::U16)),
        (Type::Uint(super::ir::UintTy::U32)),
        (Type::Uint(super::ir::UintTy::U64)),
        (Type::Uint(super::ir::UintTy::U128)),
        (Type::Float(super::ir::FloatTy::F32)),
        (Type::Float(super::ir::FloatTy::F64)),
    ] {
        builder
            .ir
            .builtin_types
            .insert(kind.clone(), builder.ir.types.insert(Some(kind)));
    }
}

pub fn lower_module(
    builder: &mut IRBuilder,
    module: &ast::modules::Module,
    module_idx: ModuleIndex,
) -> Result<(), LoweringError> {
    builder.enter_module_context(module_idx);

    for item in &module.contents {
        match item {
            ast::modules::ModuleDefItem::Trait(_) => {}
            ast::modules::ModuleDefItem::ImplTrait(impl_trait) => {
                let trait_id = builder
                    .trait_db
                    .get_trait_by_name(&impl_trait.target_trait.name.name, module_idx)
                    .ok_or_else(|| LoweringError::TraitNotFound {
                        span: impl_trait.target_trait.span,
                        name: impl_trait.target_trait.name.name.clone(),
                        path: builder.get_current_module().file_path.clone(),
                    })?;

                let mut trait_generics = Vec::new();

                for g in &impl_trait.generic_params {
                    if let Some(ty) = builder.context.generics_mapping.get(&g.name.name) {
                        trait_generics.push(TraitGeneric::Type(*ty));
                    } else {
                        trait_generics.push(TraitGeneric::Generic);
                    }
                }

                let target_ty = if !impl_trait.generic_params.is_empty() {
                    let adt_symbol = Symbol {
                        name: impl_trait.target.get_name().unwrap(),
                        method_of: None,
                        generics: Vec::new(),
                    };

                    let id = *builder
                        .symbols
                        .get(&builder.get_current_module_idx())
                        .unwrap()
                        .aggregates
                        .get(&adt_symbol)
                        .unwrap();

                    let type_id = *builder.adt_to_type_idx.get(&id).unwrap();
                    type_id
                } else {
                    lower_type(builder, &impl_trait.target)?
                };

                {
                    let tr = &builder.trait_db.traits[trait_id];

                    let assoc_types: HashMap<_, _> = tr
                        .associated_types
                        .iter()
                        .map(|x| (x.name.name.clone(), x.clone()))
                        .collect();
                    let assoc_types_found: HashMap<_, _> = impl_trait
                        .associated_types
                        .iter()
                        .map(|x| (x.name.name.clone(), x.clone()))
                        .collect();

                    let trait_module_id = builder.trait_db.get_trait_module_idx(trait_id);
                    for trait_assoc_ty in &tr.associated_types {
                        if !assoc_types_found.contains_key(&trait_assoc_ty.name.name) {
                            return Err(LoweringError::MissingTraitType(Box::new(
                                MissingTraitType {
                                    trait_name: impl_trait.target_trait.name.name.clone(),
                                    trait_span: tr.span,
                                    type_name: impl_trait.target.get_name().unwrap(),
                                    type_name_span: impl_trait.target.get_span(),
                                    assoc_type_name: trait_assoc_ty.name.name.clone(),
                                    assoc_type_name_span_def: trait_assoc_ty.span,
                                    impl_trait_span: impl_trait.span,
                                    path: builder.get_current_module().file_path.clone(),
                                    trait_path: builder.ir.modules[trait_module_id]
                                        .file_path
                                        .clone(),
                                },
                            )));
                        }
                    }

                    for assoc_ty in &impl_trait.associated_types {
                        if !assoc_types.contains_key(&assoc_ty.name.name) {
                            return Err(LoweringError::UnexpectedTraitType(Box::new(
                                UnexpectedTraitType {
                                    trait_name: impl_trait.target_trait.name.name.clone(),
                                    trait_span: tr.span,
                                    type_name: impl_trait.target.get_name().unwrap(),
                                    type_name_span: impl_trait.target.get_span(),
                                    assoc_type_name: assoc_ty.name.name.clone(),
                                    assoc_type_name_span_def: assoc_ty.span,
                                    impl_trait_span: impl_trait.span,
                                    path: builder.get_current_module().file_path.clone(),
                                    trait_path: builder.ir.modules[trait_module_id]
                                        .file_path
                                        .clone(),
                                },
                            )));
                        }
                    }

                    let trait_functions: HashMap<_, _> = tr
                        .methods
                        .iter()
                        .map(|x| (x.name.name.clone(), x.clone()))
                        .collect();
                    let functions_found: HashMap<_, _> = impl_trait
                        .methods
                        .iter()
                        .map(|x| (x.decl.name.name.clone(), x.clone()))
                        .collect();

                    for trait_method in &tr.methods {
                        if !functions_found.contains_key(&trait_method.name.name) {
                            return Err(LoweringError::MissingTraitFunction(Box::new(
                                MissingTraitFunction {
                                    trait_name: impl_trait.target_trait.name.name.clone(),
                                    trait_span: tr.span,
                                    type_name: impl_trait.target.get_name().unwrap(),
                                    type_name_span: impl_trait.target.get_span(),
                                    func_name: trait_method.name.name.clone(),
                                    func_name_span_def: trait_method.span,
                                    impl_trait_span: impl_trait.span,
                                    path: builder.get_current_module().file_path.clone(),
                                    trait_path: builder.ir.modules[trait_module_id]
                                        .file_path
                                        .clone(),
                                },
                            )));
                        }
                    }

                    for method in &impl_trait.methods {
                        if !trait_functions.contains_key(&method.decl.name.name) {
                            return Err(LoweringError::UnexpectedTraitFunction(Box::new(
                                UnexpectedTraitFunction {
                                    trait_name: impl_trait.target_trait.name.name.clone(),
                                    trait_span: tr.span,
                                    type_name: impl_trait.target.get_name().unwrap(),
                                    type_name_span: impl_trait.target.get_span(),
                                    func_name: method.decl.name.name.clone(),
                                    func_name_span_def: method.span,
                                    impl_trait_span: impl_trait.span,
                                    path: builder.get_current_module().file_path.clone(),
                                    trait_path: builder.ir.modules[trait_module_id]
                                        .file_path
                                        .clone(),
                                },
                            )));
                        }
                    }
                }

                builder.trait_db.add_trait_impl(
                    trait_id,
                    TraitImpl {
                        implementor: target_ty,
                        generics: trait_generics,
                    },
                );

                if impl_trait.generic_params.is_empty() {
                    builder.context.self_ty = Some(target_ty);
                    for method in &impl_trait.methods {
                        if method.decl.generic_params.is_empty() {
                            lower_func(builder, method, Some(target_ty))?;
                        }
                    }
                    builder.context.self_ty = None;
                }
            }
            ast::modules::ModuleDefItem::Constant(constant_def) => {
                lower_constant(builder, constant_def)?;
            }
            ast::modules::ModuleDefItem::Struct(info) => {
                if info.generics.is_empty() {
                    lower_struct(builder, info)?;
                }
            }
            ast::modules::ModuleDefItem::Type(info) => {
                lower_type_decl(builder, info)?;
            }
            ast::modules::ModuleDefItem::Function(function_def) => {
                if function_def.decl.generic_params.is_empty() {
                    lower_func(builder, function_def, None)?;
                }
            }
            ast::modules::ModuleDefItem::FunctionDecl(function_decl) => {
                if function_decl.generic_params.is_empty() {
                    lower_func_decl(builder, function_decl)?;
                }
            }
            ast::modules::ModuleDefItem::Impl(impl_block) => {
                if impl_block.generic_params.is_empty() {
                    let target_ty = lower_type(builder, &impl_block.target)?;
                    builder.context.self_ty = Some(target_ty);
                    for method in &impl_block.methods {
                        if method.decl.generic_params.is_empty() {
                            lower_func(builder, method, Some(target_ty))?;
                        }
                    }
                    builder.context.self_ty = None;
                }
            }
            ast::modules::ModuleDefItem::Union(_union_decl) => todo!(),
            ast::modules::ModuleDefItem::Enum(info) => {
                if info.generics.is_empty() {
                    lower_enum(builder, info)?;
                }
            }
            ast::modules::ModuleDefItem::Module(module) => {
                let new_module_idx = *builder
                    .symbols
                    .get(&module_idx)
                    .unwrap()
                    .modules
                    .get(&module.name.name)
                    .unwrap();
                lower_module(builder, module, new_module_idx)?;
            }
            ast::modules::ModuleDefItem::ExternalModule(_) => {}
            ast::modules::ModuleDefItem::Import(_) => {}
        }
    }

    builder.leave_module_context();

    Ok(())
}
