use std::error::Error;

// TODO: Add implementation.
#[derive(Debug, Clone, Copy)]
pub struct CompilerError;

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("compiler error")
    }
}

impl Error for CompilerError {}
