use modules::Module;

pub mod common;
pub mod constants;
pub mod enums;
pub mod expressions;
pub mod functions;
pub mod imports;
pub mod modules;
pub mod statements;
pub mod structs;
pub mod traits;
pub mod types;

/// A compile represents a whole package, made up of various modules.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompilationUnit {
    pub modules: Vec<Module>,
}
