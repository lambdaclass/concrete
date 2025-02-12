use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use tracing::debug;

use crate::{
    ast,
    ir::lowering::{errors::LoweringError, Bodies, IRBuilder},
    ir::{Constants, Functions, Module, Modules, Structs, Types},
};

use super::{
    constants::lower_constant,
    functions::{lower_func, lower_func_decl},
    ir::{ModuleIndex, Type, IR},
    structs::lower_struct,
    types::lower_type,
    Symbol,
};

pub fn lower_compile_units(compile_units: &[ast::CompileUnit]) -> Result<IR, LoweringError> {
    let mut builder = IRBuilder {
        ir: IR {
            types: Types::new(),
            functions: Functions::new(),
            structs: Structs::new(),
            constants: Constants::new(),
            modules: Modules::new(),
            top_level_modules: Vec::new(),
            builtin_types: Default::default(),
        },
        symbols: Default::default(),
        top_level_modules_names: Default::default(),
        current_generics_map: Default::default(),
        struct_to_type_idx: Default::default(),
        type_module_idx: Default::default(),
        bodies: Bodies::default(),
        self_ty: None,
        local_module: None,
    };

    // Prepass to fill some symbols.
    for compile_unit in compile_units {
        let file_path = compile_unit
            .file_path
            .as_ref()
            .ok_or_else(|| LoweringError::InternalError("Missing program file path".to_string()))?;

        for module in &compile_unit.modules {
            lower_module_symbols(&mut builder, module, &[], file_path)?;
        }
    }

    // Handle imports so they are transparent afterwards.
    for compile_unit in compile_units {
        let file_path = compile_unit
            .file_path
            .as_ref()
            .ok_or_else(|| LoweringError::InternalError("Missing program file path".to_string()))?;

        for module in &compile_unit.modules {
            debug!("lowering imports for module: {:?}", &module.name.name);
            let module_idx = *builder
                .top_level_modules_names
                .get(&module.name.name)
                .expect("should exist");
            builder.local_module = Some(module_idx);
            lower_imports(&mut builder, module, module_idx, &[], file_path)?;
        }
    }

    for compile_unit in compile_units {
        for module in &compile_unit.modules {
            let module_idx = *builder
                .top_level_modules_names
                .get(&module.name.name)
                .unwrap();
            builder.local_module = Some(module_idx);
            lower_module(&mut builder, module, module_idx)?;
        }
    }

    Ok(builder.ir)
}

fn lower_module_symbols(
    builder: &mut IRBuilder,
    module: &ast::modules::Module,
    parents: &[ModuleIndex],
    file_path: &Path,
) -> Result<ModuleIndex, LoweringError> {
    add_builtins(builder);

    let module_body = Module {
        name: module.name.name.clone(),
        parents: parents.to_vec(),
        functions: HashSet::new(),
        structs: HashSet::new(),
        types: HashSet::new(),
        constants: HashSet::new(),
        modules: HashMap::new(),
        span: module.span,
        file_path: file_path.to_path_buf(),
    };

    // Add the empty module body to the arena.
    let module_idx = builder.ir.modules.insert(module_body);
    builder.local_module = Some(module_idx);

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
                        idx,
                    );
                builder.ir.modules[module_idx].functions.insert(idx);
                debug!(
                    "Adding function symbol {:?} to module {:?}",
                    function_def.decl.name.name, module.name.name
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
                        idx,
                    );
                builder.ir.modules[module_idx].functions.insert(idx);
                debug!(
                    "Adding function symbol {:?} to module {:?}",
                    function_decl.name.name, module.name.name
                );
            }
            ast::modules::ModuleDefItem::Impl(_) => {}
            ast::modules::ModuleDefItem::Struct(struct_decl) => {
                let idx = builder.ir.structs.insert(None);
                builder.bodies.structs.insert(idx, struct_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .structs
                    .insert(
                        Symbol {
                            name: struct_decl.name.name.clone(),
                            method_of: None,
                            generics: Vec::new(),
                        },
                        idx,
                    );
                builder.ir.modules[module_idx].structs.insert(idx);
                let type_idx = builder.ir.types.insert(Some(Type::Struct(idx)));
                builder.ir.modules[module_idx].types.insert(type_idx);
                builder.struct_to_type_idx.insert(idx, type_idx);
                builder.type_module_idx.insert(type_idx, module_idx);
                debug!(
                    "Adding struct symbol {:?} to module {:?}",
                    struct_decl.name.name, module.name.name
                );
            }
            ast::modules::ModuleDefItem::Union(_union_decl) => todo!(),
            ast::modules::ModuleDefItem::Enum(_enum_decl) => todo!(),
            ast::modules::ModuleDefItem::Type(type_decl) => {
                let idx = builder.ir.types.insert(None);
                builder.bodies.types.insert(idx, type_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .types
                    .insert(type_decl.name.name.clone(), idx);
                builder.type_module_idx.insert(idx, module_idx);
            }
            ast::modules::ModuleDefItem::Module(submodule) => {
                let mut parents = parents.to_vec();
                parents.push(module_idx);

                lower_module_symbols(builder, submodule, &parents, file_path)?;
            }
        }
    }

    // Second pass
    for item in &module.contents {
        if let ast::modules::ModuleDefItem::Impl(impl_block) = item {
            let ty = lower_type(builder, &impl_block.target)?;

            // TODO: when implementing impl generics, deal with it here.
            // e.g impl Array<u32> would be different than impl<T> Array<T>
            // the first is a specific type the second is a generic type

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
                    .insert(sym.clone(), idx);
                builder.ir.modules[module_idx].functions.insert(idx);
            }
        }
    }

    Ok(module_idx)
}

fn lower_imports(
    builder: &mut IRBuilder,
    module: &ast::modules::Module,
    module_idx: ModuleIndex,
    parents: &[ModuleIndex],
    path: &Path,
) -> Result<(), LoweringError> {
    builder.local_module = Some(module_idx);

    for import in &module.imports {
        let mut target_module = None;

        let mut root_requested = false;

        for (i, m) in import.module.iter().enumerate() {
            if i == 0 || root_requested {
                if m.name == "super" && !root_requested {
                    if let Some(parent) = parents.last().copied() {
                        target_module = Some(parent);
                    } else {
                        panic!("no parent found for super, todo turn into error")
                    }
                } else if m.name == "root" && !root_requested {
                    root_requested = true;
                } else {
                    let id = *builder
                        .top_level_modules_names
                        .get(&m.name)
                        .ok_or_else(|| LoweringError::ModuleNotFound {
                            span: m.span,
                            module: m.name.clone(),
                            path: path.to_path_buf(),
                        })?;
                    target_module = Some(id);
                    root_requested = false;
                }
                continue;
            }

            let info = &builder.ir.modules[target_module.unwrap()];
            target_module =
                Some(
                    *info
                        .modules
                        .get(&m.name)
                        .ok_or_else(|| LoweringError::ModuleNotFound {
                            span: m.span,
                            module: m.name.clone(),
                            path: path.to_path_buf(),
                        })?,
                );
        }

        let target_module = target_module.unwrap();
        for sym in &import.symbols {
            let target_symbols = builder.symbols.get(&target_module).unwrap();

            let symbol = Symbol {
                name: sym.name.clone(),
                method_of: None,
                generics: Vec::new(),
            };
            if let Some(id) = target_symbols.functions.get(&symbol).cloned() {
                debug!(
                    "Imported function symbol {:?} to module {}",
                    symbol, builder.ir.modules[module_idx].name
                );
                builder.ir.modules[module_idx].functions.insert(id);
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .functions
                    .insert(symbol.clone(), id);
                continue;
            }

            let target_symbols = builder.symbols.get(&target_module).unwrap();
            if let Some(struct_idx) = target_symbols.structs.get(&symbol).cloned() {
                debug!(
                    "Imported struct symbol {:?} to module {}",
                    symbol, builder.ir.modules[module_idx].name
                );
                builder.ir.modules[module_idx].structs.insert(struct_idx);
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .structs
                    .insert(symbol.clone(), struct_idx);

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
                path: path.to_path_buf(),
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
            lower_imports(builder, submodule, new_module_idx, &parents, path)?;
        }
    }

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
    builder.local_module = Some(module_idx);

    for item in &module.contents {
        match item {
            ast::modules::ModuleDefItem::Constant(constant_def) => {
                lower_constant(builder, module_idx, constant_def)?;
            }
            ast::modules::ModuleDefItem::Struct(info) => {
                if info.generics.is_empty() {
                    lower_struct(builder, info)?;
                }
            }
            ast::modules::ModuleDefItem::Type(_) => todo!(),
            ast::modules::ModuleDefItem::Function(function_def) => {
                if function_def.decl.generic_params.is_empty() {
                    lower_func(builder, function_def, None)?;
                }
            }
            ast::modules::ModuleDefItem::FunctionDecl(function_decl) => {
                lower_func_decl(builder, function_decl)?;
            }
            ast::modules::ModuleDefItem::Impl(impl_block) => {
                let target_ty = lower_type(builder, &impl_block.target)?;
                builder.self_ty = Some(target_ty);
                for method in &impl_block.methods {
                    lower_func(builder, method, Some(target_ty))?;
                }
                builder.self_ty = None;
            }
            ast::modules::ModuleDefItem::Union(_union_decl) => todo!(),
            ast::modules::ModuleDefItem::Enum(_enum_decl) => todo!(),
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
        }
    }

    Ok(())
}
