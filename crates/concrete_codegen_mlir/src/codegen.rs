use super::error::CompilerError;
use concrete_ast::{modules::Module, Program};
use melior::{ir::Module as MeliorModule, Context as MeliorContext};

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
