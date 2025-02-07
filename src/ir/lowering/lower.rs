use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use tracing::debug;

use crate::{
    ast::{
        modules::{Module, ModuleDefItem},
        Program,
    },
    ir::lowering::{errors::LoweringError, Bodies, IRBuilder},
    ir::{Constants, Functions, ModuleBody, Modules, Structs, Types},
};

use super::{
    constants::lower_constant,
    functions::{lower_func, lower_func_decl},
    ir::{FnIndex, ModuleIndex, ProgramBody, TyKind},
    structs::lower_struct,
    types::lower_type,
    Symbol,
};

pub fn lower_programs(programs: &[Program]) -> Result<ProgramBody, LoweringError> {
    let mut builder = IRBuilder {
        ir: ProgramBody {
            types: Types::new(),
            functions: Functions::new(),
            structs: Structs::new(),
            constants: Constants::new(),
            modules: Modules::new(),
            top_level_modules: Vec::new(),
            builtin_types: Default::default(),
        },
        methods: Default::default(),
        symbols: Default::default(),
        top_level_modules_names: Default::default(),
        current_generics_map: Default::default(),
        bodies: Bodies::default(),
        self_ty: None,
        local_module: None,
    };

    // Prepass to fill some symbols.
    for program in programs {
        let file_path = program
            .file_path
            .as_ref()
            .ok_or_else(|| LoweringError::InternalError("Missing program file path".to_string()))?;

        for module in &program.modules {
            prepass_module(&mut builder, module, &[], file_path)?;
        }
    }

    // Handle imports so they are transparent afterwards.
    for program in programs {
        let file_path = program
            .file_path
            .as_ref()
            .ok_or_else(|| LoweringError::InternalError("Missing program file path".to_string()))?;

        for module in &program.modules {
            debug!("lowering imports for module: {:?}", &module.name.name);
            let module_idx = *builder
                .top_level_modules_names
                .get(&module.name.name)
                .expect("should exist");
            builder.local_module = Some(module_idx);
            lower_imports(&mut builder, module, module_idx, &[], file_path)?;
        }
    }

    for program in programs {
        for module in &program.modules {
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

fn prepass_module(
    builder: &mut IRBuilder,
    module: &Module,
    parents: &[ModuleIndex],
    file_path: &Path,
) -> Result<ModuleIndex, LoweringError> {
    add_builtins(builder);

    let module_body = ModuleBody {
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
            ModuleDefItem::Constant(constant_def) => {
                let idx = builder.ir.constants.insert(None);
                builder.bodies.constants.insert(idx, constant_def.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .constants
                    .insert(constant_def.decl.name.name.clone(), idx);
            }
            ModuleDefItem::Function(function_def) => {
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
            ModuleDefItem::FunctionDecl(function_decl) => {
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
            ModuleDefItem::Impl(_) => {}
            ModuleDefItem::Struct(struct_decl) => {
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
                            generics: Vec::new(),
                        },
                        idx,
                    );
                builder.ir.modules[module_idx].structs.insert(idx);
                debug!(
                    "Adding struct symbol {:?} to module {:?}",
                    struct_decl.name.name, module.name.name
                );
            }
            ModuleDefItem::Union(_union_decl) => todo!(),
            ModuleDefItem::Enum(_enum_decl) => todo!(),
            ModuleDefItem::Type(type_decl) => {
                let idx = builder.ir.types.insert(None);
                builder.bodies.types.insert(idx, type_decl.clone());
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .types
                    .insert(type_decl.name.name.clone(), idx);
            }
            ModuleDefItem::Module(submodule) => {
                let mut parents = parents.to_vec();
                parents.push(module_idx);

                prepass_module(builder, submodule, &parents, file_path)?;
            }
        }
    }

    // Second pass
    for item in &module.contents {
        if let ModuleDefItem::Impl(impl_block) = item {
            let ty = lower_type(builder, &impl_block.target)?;

            let mut methods: HashMap<Symbol, FnIndex> = HashMap::new();

            // TODO: when implementing impl generics, deal with it here.
            // e.g impl Array<u32> would be different than impl<T> Array<T>
            // the first is a specific type the second is a generic type

            for function_def in &impl_block.methods {
                let idx = builder.ir.functions.insert(None);
                builder.bodies.functions.insert(idx, function_def.clone());
                let sym = Symbol {
                    name: function_def.decl.name.name.clone(),
                    generics: Vec::new(),
                };
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .methods
                    .insert((ty, sym.clone()), idx);
                methods.insert(sym.clone(), idx);
                builder.ir.modules[module_idx].functions.insert(idx);
            }

            builder.methods.insert(ty, methods);
        }
    }

    Ok(module_idx)
}

fn lower_imports(
    builder: &mut IRBuilder,
    module: &Module,
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
                generics: Vec::new(),
            };
            if let Some(id) = target_symbols.functions.get(&symbol).cloned() {
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
            if let Some(id) = target_symbols.structs.get(&symbol).cloned() {
                dbg!(&symbol);
                dbg!(&id);
                builder.ir.modules[module_idx].structs.insert(id);
                builder
                    .symbols
                    .get_mut(&module_idx)
                    .unwrap()
                    .structs
                    .insert(symbol.clone(), id);
                continue;
            }

            let target_symbols = builder.symbols.get(&target_module).unwrap();
            if let Some(id) = target_symbols.types.get(&sym.name).cloned() {
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
        if let ModuleDefItem::Module(submodule) = m {
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
        (TyKind::Unit),
        (TyKind::Bool),
        (TyKind::Char),
        (TyKind::Int(super::ir::IntTy::I8)),
        (TyKind::Int(super::ir::IntTy::I16)),
        (TyKind::Int(super::ir::IntTy::I32)),
        (TyKind::Int(super::ir::IntTy::I64)),
        (TyKind::Int(super::ir::IntTy::I128)),
        (TyKind::Uint(super::ir::UintTy::U8)),
        (TyKind::Uint(super::ir::UintTy::U16)),
        (TyKind::Uint(super::ir::UintTy::U32)),
        (TyKind::Uint(super::ir::UintTy::U64)),
        (TyKind::Uint(super::ir::UintTy::U128)),
        (TyKind::Float(super::ir::FloatTy::F32)),
        (TyKind::Float(super::ir::FloatTy::F64)),
    ] {
        builder
            .ir
            .builtin_types
            .insert(kind.clone(), builder.ir.types.insert(Some(kind)));
    }
}

pub fn lower_module(
    builder: &mut IRBuilder,
    module: &Module,
    module_idx: ModuleIndex,
) -> Result<(), LoweringError> {
    builder.local_module = Some(module_idx);

    for item in &module.contents {
        match item {
            ModuleDefItem::Constant(constant_def) => {
                lower_constant(builder, module_idx, constant_def)?;
            }
            ModuleDefItem::Struct(info) => {
                if !info.generics.is_empty() {
                    lower_struct(builder, info)?;
                }
            }
            ModuleDefItem::Type(_) => todo!(),
            ModuleDefItem::Function(function_def) => {
                lower_func(builder, function_def)?;
            }
            ModuleDefItem::FunctionDecl(function_decl) => {
                lower_func_decl(builder, function_decl)?;
            }
            ModuleDefItem::Impl(_impl_block) => todo!(),
            ModuleDefItem::Union(_union_decl) => todo!(),
            ModuleDefItem::Enum(_enum_decl) => todo!(),
            ModuleDefItem::Module(module) => {
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
