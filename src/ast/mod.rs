use std::path::PathBuf;

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
pub mod types;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompileUnit {
    pub file_path: PathBuf,
    pub modules: Vec<Module>,
}
