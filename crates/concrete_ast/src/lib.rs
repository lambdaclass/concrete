use modules::Module;

pub mod common;
pub mod constants;
pub mod expressions;
pub mod functions;
pub mod imports;
pub mod modules;
pub mod operations;
pub mod statements;
pub mod structs;
pub mod types;

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub modules: Vec<Module>,
}
