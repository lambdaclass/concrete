use crate::{DefId, ModuleBody};

use super::common::BuildCtx;
use concrete_ast as ast;

pub fn prepass_module(mut ctx: BuildCtx, mod_def: &ast::modules::Module) -> BuildCtx {
    let module_id = ctx.gen.next_defid();

    ctx.body
        .module_names
        .insert(mod_def.name.name.clone(), module_id);

    ctx.body.modules.insert(
        module_id,
        ModuleBody {
            id: module_id,
            parent_id: None,
            symbols: Default::default(),
            functions: Default::default(),
            modules: Default::default(),
            function_signatures: Default::default(),
        },
    );

    for ct in &mod_def.contents {
        match ct {
            ast::modules::ModuleDefItem::Constant(info) => {
                let m = ctx
                    .body
                    .modules
                    .get_mut(&module_id)
                    .expect("module should exist");
                let next_id = ctx.gen.next_defid();
                m.symbols
                    .constants
                    .insert(info.decl.name.name.clone(), next_id);
            }
            ast::modules::ModuleDefItem::Function(info) => {
                let m = ctx
                    .body
                    .modules
                    .get_mut(&module_id)
                    .expect("module should exist");
                let next_id = ctx.gen.next_defid();
                m.symbols
                    .functions
                    .insert(info.decl.name.name.clone(), next_id);
            }
            ast::modules::ModuleDefItem::Struct(info) => {
                let m = ctx
                    .body
                    .modules
                    .get_mut(&module_id)
                    .expect("module should exist");
                let next_id = ctx.gen.next_defid();
                m.symbols.structs.insert(info.name.name.clone(), next_id);
            }
            ast::modules::ModuleDefItem::Type(info) => {
                let m = ctx
                    .body
                    .modules
                    .get_mut(&module_id)
                    .expect("module should exist");
                let next_id = ctx.gen.next_defid();
                m.symbols.types.insert(info.name.name.clone(), next_id);
            }
            ast::modules::ModuleDefItem::Module(info) => {
                let m = ctx
                    .body
                    .modules
                    .get_mut(&module_id)
                    .expect("module should exist");
                let next_id = ctx.gen.next_defid();
                m.symbols.modules.insert(info.name.name.clone(), next_id);
                m.modules.insert(
                    next_id,
                    ModuleBody {
                        id: next_id,
                        parent_id: Some(module_id),
                        symbols: Default::default(),
                        functions: Default::default(),
                        modules: Default::default(),
                        function_signatures: Default::default(),
                    },
                );
                ctx = prepass_sub_module(ctx, &[module_id], next_id, info);
            }
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
    {
        let mut gen = ctx.gen;
        let parent = ctx.get_module_mut(parent_ids).unwrap();
        let submodule = parent.modules.get_mut(&id).unwrap();

        for ct in &mod_def.contents {
            match ct {
                ast::modules::ModuleDefItem::Constant(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .constants
                        .insert(info.decl.name.name.clone(), next_id);
                }
                ast::modules::ModuleDefItem::Function(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .functions
                        .insert(info.decl.name.name.clone(), next_id);
                }
                ast::modules::ModuleDefItem::Struct(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .structs
                        .insert(info.name.name.clone(), next_id);
                }
                ast::modules::ModuleDefItem::Type(info) => {
                    let next_id = gen.next_defid();
                    submodule
                        .symbols
                        .types
                        .insert(info.name.name.clone(), next_id);
                }
                ast::modules::ModuleDefItem::Module(_) => {}
            }
        }

        ctx.gen = gen;
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

            let mut parent_ids = parent_ids.to_vec();
            parent_ids.push(id);
            ctx = prepass_sub_module(ctx, &parent_ids, next_id, mod_def);
        }
    }

    ctx
}
