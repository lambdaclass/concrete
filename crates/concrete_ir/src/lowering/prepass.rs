use crate::{DefId, ModuleBody};

use super::common::BuildCtx;
use concrete_ast as ast;

pub fn prepass_module(mut ctx: BuildCtx, mod_def: &ast::modules::Module) -> BuildCtx {
    let module_id = ctx.gen.next_defid();
    tracing::debug!("running ir prepass on module {:?}", module_id);

    ctx.body
        .module_names
        .insert(mod_def.name.name.clone(), module_id);

    ctx.body.modules.insert(
        module_id,
        ModuleBody {
            id: module_id,
            parent_ids: vec![],
            symbols: Default::default(),
            functions: Default::default(),
            modules: Default::default(),
            function_signatures: Default::default(),
        },
    );
    ctx.body.id_module_tree.insert(module_id, vec![]);

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
                    ctx.body.id_module_tree.insert(next_id, vec![module_id]);
                }
                ast::modules::ModuleDefItem::Function(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .functions
                        .insert(info.decl.name.name.clone(), next_id);
                    ctx.body.id_module_tree.insert(next_id, vec![module_id]);
                }
                ast::modules::ModuleDefItem::Struct(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    ctx.body.id_module_tree.insert(next_id, vec![module_id]);
                }
                ast::modules::ModuleDefItem::Type(info) => {
                    let next_id = gen.next_defid();
                    current_module
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    ctx.body.id_module_tree.insert(next_id, vec![module_id]);
                }
                ast::modules::ModuleDefItem::Module(_) => {}
            }
        }

        ctx.gen = gen;
    }

    for ct in &mod_def.contents {
        if let ast::modules::ModuleDefItem::Module(info) = ct {
            let next_id = ctx.gen.next_defid();
            let current_module = ctx
                .body
                .modules
                .get_mut(&module_id)
                .expect("module should exist");
            ctx.body.id_module_tree.insert(next_id, vec![module_id]);

            current_module
                .symbols
                .modules
                .insert(info.name.name.clone(), next_id);
            ctx = prepass_sub_module(ctx, &[module_id], next_id, info);
        }
    }

    ctx
}

pub fn prepass_sub_module(
    mut ctx: BuildCtx,
    parent_ids: &[DefId],
    id: DefId,
    mod_def: &ast::modules::Module,
) -> BuildCtx {
    tracing::debug!("running ir prepass on submodule {:?}", id);
    let mut submodule_parents_ids = parent_ids.to_vec();
    submodule_parents_ids.push(id);
    {
        let mut gen = ctx.gen;
        let mut submodule = ModuleBody {
            id,
            parent_ids: parent_ids.to_vec(),
            symbols: Default::default(),
            functions: Default::default(),
            modules: Default::default(),
            function_signatures: Default::default(),
        };

        for ct in &mod_def.contents {
            match ct {
                ast::modules::ModuleDefItem::Constant(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .constants
                        .insert(info.decl.name.name.clone(), next_id);
                    ctx.body
                        .id_module_tree
                        .insert(next_id, submodule_parents_ids.clone());
                }
                ast::modules::ModuleDefItem::Function(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .functions
                        .insert(info.decl.name.name.clone(), next_id);
                    ctx.body
                        .id_module_tree
                        .insert(next_id, submodule_parents_ids.clone());
                }
                ast::modules::ModuleDefItem::Struct(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                    ctx.body
                        .id_module_tree
                        .insert(next_id, submodule_parents_ids.clone());
                }
                ast::modules::ModuleDefItem::Type(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                    ctx.body
                        .id_module_tree
                        .insert(next_id, submodule_parents_ids.clone());
                }
                ast::modules::ModuleDefItem::Module(_) => {}
            }
        }

        ctx.gen = gen;

        let parent = ctx.get_module_mut(parent_ids).unwrap();
        parent.modules.insert(id, submodule);
    }

    for ct in &mod_def.contents {
        if let ast::modules::ModuleDefItem::Module(info) = ct {
            let next_id = ctx.gen.next_defid();
            let parent = ctx.get_module_mut(parent_ids).unwrap();
            let submodule = parent.modules.get_mut(&id).unwrap();

            submodule
                .symbols
                .modules
                .insert(info.name.name.clone(), next_id);

            ctx.body.id_module_tree.insert(next_id, submodule_parents_ids.clone());
            ctx = prepass_sub_module(ctx, &submodule_parents_ids, next_id, info);
        }
    }

    ctx
}

pub fn prepass_imports(mut ctx: BuildCtx, mod_def: &ast::modules::Module) -> BuildCtx {
    let mod_id = *ctx.body.module_names.get(&mod_def.name.name).unwrap();
    let current_module = ctx.body.modules.get(&mod_id);

    for import in &mod_def.imports {
        let imported_module_id = ctx
            .body
            .module_names
            .get(&import.module[0].name)
            .expect("import module not found");
        let mut imported_module = ctx.body.modules.get(imported_module_id).unwrap();

        for x in import.module.iter().skip(1) {
            let imported_module_id = imported_module.symbols.modules.get(&x.name).unwrap();
            imported_module = imported_module.modules.get(imported_module_id).unwrap();
        }

        for sym in &import.symbols {
            if let Some(id) = imported_module.symbols.functions.get(&sym.name) {

            }
        }
    }

    ctx
}
