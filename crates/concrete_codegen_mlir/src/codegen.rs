use super::error::CompilerError;
use concrete_ast::{modules::Module, Program};
use melior::{
    dialect::func,
    ir::{
        attribute::{StringAttribute, TypeAttribute},
        r#type::FunctionType,
        Block, Location, Module as MeliorModule, Region,
    },
    Context as MeliorContext,
};

/// Setup _start and call into main.
pub fn compile_start(ctx: &MeliorContext, mlir_module: &MeliorModule, _entry_point_symbol: &str) {
    let location = Location::unknown(ctx);
    let region = Region::new();
    let block = region.append_block(Block::new(&[]));

    // TODO: implement
    // Note: _start should return nothing.

    block.append_operation(func::r#return(&[], location));

    mlir_module.body().append_operation(func::func(
        ctx,
        StringAttribute::new(ctx, "_start"),
        TypeAttribute::new(FunctionType::new(ctx, &[], &[]).into()),
        region,
        &[],
        location,
    ));
}

pub fn compile_program(
    ctx: &MeliorContext,
    mlir_module: &MeliorModule,
    program: &Program,
) -> Result<(), CompilerError> {
    for module in &program.modules {
        compile_module(ctx, mlir_module, module)?;
    }
    Ok(())
}

fn compile_module(
    _ctx: &MeliorContext,
    _mlir_module: &MeliorModule,
    _module: &Module,
) -> Result<(), CompilerError> {
    // todo!()
    Ok(())
}
