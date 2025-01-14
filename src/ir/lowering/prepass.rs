use std::collections::HashMap;
use std::io::Read;

use crate::ast::common::TypeName;
use crate::ast::types::TypeDescriptor;
use crate::ir::{DefId, ModuleBody};

use crate::ast;
use crate::ir::lowering::{common::BuildCtx, errors::LoweringError};

pub fn prepass_module(
    mut ctx: BuildCtx,
    mod_def: &ast::modules::Module,
) -> Result<BuildCtx, LoweringError> {
    let module_id = ctx.gen.next_defid();
    tracing::debug!("running ir prepass on module {:?}", module_id);

    ctx.body
        .top_level_module_names
        .insert(mod_def.name.name.clone(), module_id);

    ctx.body.top_level_modules.push(module_id);

    ctx.body.modules.insert(
        module_id,
        ModuleBody {
            id: module_id,
            parent_ids: vec![],
            symbols: Default::default(),
            modules: Default::default(),
            functions: Default::default(),
            structs: Default::default(),
            types: Default::default(),
            constants: Default::default(),
            imports: Default::default(),
        },
    );

    {
        let mut gen = ctx.gen;
        let current_module = ctx
            .body
            .modules
            .get_mut(&module_id)
            .expect("module should exist");

        for ct in &mod_def.contents {
            match ct {
                ast::modules::ModuleDefItem::Constant(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .constants
                        .insert(info.decl.name.name.clone(), next_id);
                    current_module.constants.insert(next_id);
                }
                ast::modules::ModuleDefItem::Function(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .functions
                        .insert(info.decl.name.name.clone(), next_id);
                    current_module.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.decl
                                .params
                                .iter()
                                .map(|x| &x.r#type)
                                .cloned()
                                .collect(),
                            info.decl.ret_type.clone(),
                        ),
                    );
                }
                ast::modules::ModuleDefItem::Struct(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    current_module.structs.insert(next_id);
                }
                ast::modules::ModuleDefItem::Type(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    current_module.types.insert(next_id);
                }
                ast::modules::ModuleDefItem::Module(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .modules
                        .insert(info.name.name.clone(), next_id);
                    current_module.modules.insert(next_id);
                }
                ast::modules::ModuleDefItem::Union(_) => todo!(),
                ast::modules::ModuleDefItem::Enum(_) => todo!(),
                ast::modules::ModuleDefItem::FunctionDecl(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .functions
                        .insert(info.name.name.clone(), next_id);
                    current_module.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.params.iter().map(|x| &x.r#type).cloned().collect(),
                            info.ret_type.clone(),
                        ),
                    );
                }
                ast::modules::ModuleDefItem::Impl(impl_block) => {
                    // TODO: traverse target path and deal with generics
                    let struct_id = current_module
                        .symbols
                        .structs
                        .get(&impl_block.target.name.name)
                        .expect("target struct not found");
                    for info in &impl_block.methods {
                        let next_id = gen.next_defid();
                        current_module
                            .symbols
                            .methods
                            .insert((*struct_id, info.decl.name.name.clone()), next_id);
                        current_module.functions.insert(next_id);
                        ctx.unresolved_function_signatures.insert(
                            next_id,
                            (
                                [TypeDescriptor::Type {
                                    name: impl_block.target.clone(),
                                    span: impl_block.target.span,
                                }]
                                .into_iter()
                                .chain(info.decl.params.iter().map(|x| &x.r#type).cloned())
                                .collect(),
                                info.decl.ret_type.clone(),
                            ),
                        );
                    }
                }
            }
        }

        ctx.gen = gen;
    }

    for ct in &mod_def.contents {
        if let ast::modules::ModuleDefItem::Module(info) = ct {
            let current_module = ctx
                .body
                .modules
                .get_mut(&module_id)
                .expect("module should exist");

            let next_id = *current_module.symbols.modules.get(&info.name.name).unwrap();
            ctx = prepass_sub_module(ctx, &[module_id], next_id, info)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_sub_module(
    mut ctx: BuildCtx,
    parent_ids: &[DefId],
    id: DefId,
    mod_def: &ast::modules::Module,
) -> Result<BuildCtx, LoweringError> {
    tracing::debug!("running ir prepass on submodule {:?}", id);
    let mut submodule_parents_ids = parent_ids.to_vec();
    submodule_parents_ids.push(id);

    {
        let mut gen = ctx.gen;
        let mut submodule = ModuleBody {
            id,
            parent_ids: parent_ids.to_vec(),
            imports: Default::default(),
            symbols: Default::default(),
            modules: Default::default(),
            functions: Default::default(),
            structs: Default::default(),
            types: Default::default(),
            constants: Default::default(),
        };

        for ct in &mod_def.contents {
            match ct {
                ast::modules::ModuleDefItem::Constant(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .constants
                        .insert(info.decl.name.name.clone(), next_id);
                    submodule.constants.insert(next_id);
                }
                ast::modules::ModuleDefItem::Function(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .functions
                        .insert(info.decl.name.name.clone(), next_id);
                    submodule.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.decl
                                .params
                                .iter()
                                .map(|x| &x.r#type)
                                .cloned()
                                .collect(),
                            info.decl.ret_type.clone(),
                        ),
                    );
                }
                ast::modules::ModuleDefItem::Struct(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    submodule.structs.insert(next_id);
                }
                ast::modules::ModuleDefItem::Type(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    submodule.types.insert(next_id);
                }
                ast::modules::ModuleDefItem::Module(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .modules
                        .insert(info.name.name.clone(), next_id);
                    submodule.modules.insert(next_id);
                }
                ast::modules::ModuleDefItem::Union(_) => todo!(),
                ast::modules::ModuleDefItem::Enum(_) => todo!(),
                ast::modules::ModuleDefItem::FunctionDecl(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .functions
                        .insert(info.name.name.clone(), next_id);
                    submodule.functions.insert(next_id);
                    ctx.unresolved_function_signatures.insert(
                        next_id,
                        (
                            info.params.iter().map(|x| &x.r#type).cloned().collect(),
                            info.ret_type.clone(),
                        ),
                    );
                }
                ast::modules::ModuleDefItem::Impl(_impl_block) => todo!(),
            }
        }

        ctx.gen = gen;

        ctx.body.modules.insert(id, submodule);
    }

    for ct in &mod_def.contents {
        if let ast::modules::ModuleDefItem::Module(info) = ct {
            let next_id = ctx.gen.next_defid();
            ctx = prepass_sub_module(ctx, &submodule_parents_ids, next_id, info)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_imports(
    mut ctx: BuildCtx,
    mod_def: &ast::modules::Module,
) -> Result<BuildCtx, LoweringError> {
    let mod_id = *ctx
        .body
        .top_level_module_names
        .get(&mod_def.name.name)
        .unwrap();

    for import in &mod_def.imports {
        let imported_module_id = ctx
            .body
            .top_level_module_names
            .get(&import.module[0].name)
            .ok_or(LoweringError::ModuleNotFound {
                span: import.module[0].span,
                module: import.module[0].name.clone(),
                program_id: mod_id.program_id,
            })?;
        let mut imported_module =
            ctx.body
                .modules
                .get(imported_module_id)
                .ok_or(LoweringError::IdNotFound {
                    span: mod_def.span,
                    id: *imported_module_id,
                    program_id: mod_id.program_id,
                })?;

        for x in import.module.iter().skip(1) {
            let imported_module_id =
                imported_module
                    .symbols
                    .modules
                    .get(&x.name)
                    .ok_or_else(|| LoweringError::ModuleNotFound {
                        span: x.span,
                        module: x.name.clone(),
                        program_id: mod_id.program_id,
                    })?;
            imported_module = ctx.body.modules.get(imported_module_id).ok_or({
                LoweringError::IdNotFound {
                    span: x.span,
                    id: *imported_module_id,
                    program_id: mod_id.program_id,
                }
            })?;
        }

        let mut imports = HashMap::new();

        for sym in &import.symbols {
            if let Some(id) = imported_module.symbols.functions.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.structs.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.types.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.constants.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else {
                Err(LoweringError::ImportNotFound {
                    module_span: mod_def.span,
                    import_span: import.span,
                    symbol: sym.clone(),
                    program_id: mod_id.program_id,
                })?;
            }
        }

        ctx.body
            .modules
            .get_mut(&mod_id)
            .unwrap()
            .imports
            .extend(imports);
    }

    for c in &mod_def.contents {
        if let ast::modules::ModuleDefItem::Module(info) = c {
            ctx = prepass_imports_submodule(ctx, info, mod_id)?;
        }
    }

    Ok(ctx)
}

pub fn prepass_imports_submodule(
    mut ctx: BuildCtx,
    mod_def: &ast::modules::Module,
    parent_id: DefId,
) -> Result<BuildCtx, LoweringError> {
    let mod_id = *ctx
        .body
        .modules
        .get(&parent_id)
        .ok_or(LoweringError::IdNotFound {
            span: mod_def.span,
            id: parent_id,
            program_id: parent_id.program_id,
        })?
        .symbols
        .modules
        .get(&mod_def.name.name)
        .ok_or_else(|| LoweringError::ModuleNotFound {
            span: mod_def.span,
            module: mod_def.name.name.clone(),
            program_id: parent_id.program_id,
        })?;

    for import in &mod_def.imports {
        let imported_module_id = ctx
            .body
            .top_level_module_names
            .get(&import.module[0].name)
            .ok_or_else(|| LoweringError::ModuleNotFound {
                span: import.module[0].span,
                module: import.module[0].name.clone(),
                program_id: parent_id.program_id,
            })?;
        let mut imported_module =
            ctx.body
                .modules
                .get(imported_module_id)
                .ok_or(LoweringError::IdNotFound {
                    span: import.span,
                    id: *imported_module_id,
                    program_id: parent_id.program_id,
                })?;

        for module_path in import.module.iter().skip(1) {
            let imported_module_id = imported_module
                .symbols
                .modules
                .get(&module_path.name)
                .ok_or_else(|| LoweringError::ModuleNotFound {
                    span: module_path.span,
                    module: module_path.name.clone(),
                    program_id: parent_id.program_id,
                })?;
            imported_module = ctx.body.modules.get(imported_module_id).ok_or({
                LoweringError::IdNotFound {
                    span: import.span,
                    id: *imported_module_id,
                    program_id: parent_id.program_id,
                }
            })?;
        }

        let mut imports = HashMap::new();

        for sym in &import.symbols {
            if let Some(id) = imported_module.symbols.functions.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.structs.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.types.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else if let Some(id) = imported_module.symbols.constants.get(&sym.name) {
                imports.insert(sym.name.clone(), *id);
            } else {
                Err(LoweringError::ImportNotFound {
                    module_span: mod_def.span,
                    import_span: import.span,
                    symbol: sym.clone(),
                    program_id: parent_id.program_id,
                })?;
            }
        }

        ctx.body
            .modules
            .get_mut(&mod_id)
            .ok_or(LoweringError::IdNotFound {
                span: mod_def.span,
                id: mod_id,
                program_id: mod_id.program_id,
            })?
            .imports
            .extend(imports);
    }

    Ok(ctx)
}
