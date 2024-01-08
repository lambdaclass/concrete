use melior::ir::Module as MeliorModule;
use std::fmt::Debug;

/// A MLIR module in the context of the Concrete compiler.
pub struct MLIRModule<'m> {
    pub(crate) melior_module: MeliorModule<'m>,
}

impl<'m> MLIRModule<'m> {
    pub fn new(module: MeliorModule<'m>) -> Self {
        Self {
            melior_module: module,
        }
    }

    pub fn module(&self) -> &MeliorModule {
        &self.melior_module
    }

    // TODO: uncomment this when we have a proper Program (we should get it from the DB).
    // pub fn program_registry(&self) -> &ProgramRegistry<CoreType, CoreLibfunc> {
    //     &self.registry
    // }
}

impl Debug for MLIRModule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.melior_module.as_operation().to_string())
    }
}
