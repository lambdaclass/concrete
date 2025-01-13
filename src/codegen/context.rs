use crate::ir::ProgramBody;
use crate::session::Session;
use melior::{
    dialect::DialectRegistry,
    ir::{
        attribute::StringAttribute, operation::OperationBuilder, Block, Identifier, Location,
        Module as MeliorModule, Region,
    },
    utility::{register_all_dialects, register_all_llvm_translations, register_all_passes},
    Context as MeliorContext,
};

use super::{compiler::CodegenCtx, errors::CodegenError, get_data_layout_rep, get_target_triple};

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
    ) -> Result<MLIRModule, CodegenError> {
        let location = Location::unknown(&self.melior_context);
        let target_triple = get_target_triple(session);

        let module_region = Region::new();
        module_region.append_block(Block::new(&[]));

        let data_layout_ret = &get_data_layout_rep(session)?;

        let op = OperationBuilder::new("builtin.module", location)
            .add_attributes(&[
                (
                    Identifier::new(&self.melior_context, "llvm.target_triple"),
                    StringAttribute::new(&self.melior_context, &target_triple).into(),
                ),
                (
                    Identifier::new(&self.melior_context, "llvm.data_layout"),
                    StringAttribute::new(&self.melior_context, data_layout_ret).into(),
                ),
            ])
            .add_regions([module_region])
            .build()?;
        assert!(op.verify(), "module operation is not valid");

        let mut melior_module = MeliorModule::from_operation(op).expect("module failed to create");

        let codegen_ctx = CodegenCtx {
            mlir_context: &self.melior_context,
            session,
            mlir_module: &melior_module,
            program,
        };

        super::compiler::compile_program(codegen_ctx)?;

        if session.output_mlir {
            std::fs::write(
                session.output_file.with_extension("before-pass.mlir"),
                melior_module.as_operation().to_string(),
            )?;
        }

        assert!(melior_module.as_operation().verify());

        // TODO: Add proper error handling.
        run_pass_manager(&self.melior_context, &mut melior_module).unwrap();

        // The func to llvm pass has a bug where it sets the data layout string to ""
        // This works around it by setting it again.
        {
            let mut op = melior_module.as_operation_mut();
            op.set_attribute(
                "llvm.data_layout",
                StringAttribute::new(&self.melior_context, data_layout_ret).into(),
            );
        }

        if session.output_mlir {
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
