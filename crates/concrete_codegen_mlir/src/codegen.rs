use std::{collections::HashMap, error::Error};

use bumpalo::Bump;
use concrete_check::ast_helper::{AstHelper, ModuleInfo};
use concrete_ir::{DefId, ModuleBody, ProgramBody};
use concrete_session::Session;
use melior::{
    dialect::{
        arith::{self, CmpiPredicate},
        cf, func, memref,
    },
    ir::{
        attribute::{
            FlatSymbolRefAttribute, FloatAttribute, IntegerAttribute, StringAttribute,
            TypeAttribute,
        },
        r#type::{FunctionType, IntegerType, MemRefType},
        Block, BlockRef, Location, Module as MeliorModule, Operation, Region, Type, Value,
        ValueLike,
    },
    Context as MeliorContext,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct CodegenCtx<'a> {
    pub mlir_context: &'a MeliorContext,
    pub mlir_module: &'a MeliorModule<'a>,
    pub session: &'a Session,
    pub program: &'a ProgramBody,
}

#[derive(Debug, Clone, Copy)]
struct ModuleCodegenCtx<'a> {
    pub ctx: CodegenCtx<'a>,
    pub module_id: DefId,
}

impl<'a> ModuleCodegenCtx<'a> {
    pub fn get_module_body(&self) -> &ModuleBody {
        self.ctx
            .program
            .modules
            .get(&self.module_id)
            .expect("module should exist")
    }
}

pub fn compile_program(ctx: CodegenCtx) -> Result<(), Box<dyn Error>> {
    for module_id in &ctx.program.top_level_modules {
        let ctx = ModuleCodegenCtx {
            ctx,
            module_id: *module_id,
        };
        compile_module(ctx)?;
    }
    Ok(())
}

pub fn compile_module(ctx: ModuleCodegenCtx) -> Result<(), Box<dyn Error>> {
    let body = ctx.get_module_body();

    for fn_id in body.functions.iter() {
        let ctx = FunctionCodegenCtx {
            ctx,
            function_id: *fn_id,
        };
        compile_function(ctx);
    }

    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct FunctionCodegenCtx<'a> {
    pub ctx: ModuleCodegenCtx<'a>,
    pub function_id: DefId,
}

pub fn compile_function(ctx: FunctionCodegenCtx) {}
