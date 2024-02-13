use std::error::Error;

use concrete_ir::ProgramBody;
use concrete_session::Session;
use melior::{
    dialect::DialectRegistry,
    ir::{Location, Module as MeliorModule},
    utility::{register_all_dialects, register_all_llvm_translations, register_all_passes},
    Context as MeliorContext,
};

use crate::codegen::CodegenCtx;

use super::{module::MLIRModule, pass_manager::run_pass_manager};

#[derive(Debug, Eq, PartialEq)]
pub struct Context {
    melior_context: MeliorContext,
}

unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        let melior_context = initialize_mlir();
        Self { melior_context }
    }

    pub fn compile(
        &self,
        session: &Session,
        program: &ProgramBody,
    ) -> Result<MLIRModule, Box<dyn Error>> {
        let file_path = session.file_path.display().to_string();
        let location = Location::new(&self.melior_context, &file_path, 0, 0);

        let mut melior_module = MeliorModule::new(location);

        let codegen_ctx = CodegenCtx {
            mlir_context: &self.melior_context,
            session,
            mlir_module: &melior_module,
            program,
        };

        super::codegen::compile_program(codegen_ctx)?;

        if session.output_mlir || session.output_all {
            std::fs::write(
                session.output_file.with_extension("before-pass.mlir"),
                melior_module.as_operation().to_string(),
            )?;
        }

        assert!(melior_module.as_operation().verify());

        // TODO: Add proper error handling.
        run_pass_manager(&self.melior_context, &mut melior_module).unwrap();

        if session.output_mlir || session.output_all {
            std::fs::write(
                session.output_file.with_extension("after-pass.mlir"),
                melior_module.as_operation().to_string(),
            )?;
        }

        Ok(MLIRModule::new(melior_module))
    }
}

/// Initialize an MLIR context.
pub fn initialize_mlir() -> MeliorContext {
    let context = MeliorContext::new();
    context.append_dialect_registry(&{
        let registry = DialectRegistry::new();
        register_all_dialects(&registry);
        registry
    });
    context.load_all_available_dialects();
    register_all_passes();
    register_all_llvm_translations(&context);
    context
}
