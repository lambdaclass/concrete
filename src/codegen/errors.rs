use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("error linking: {0}")]
    LinkError(#[from] std::io::Error),
    #[error("llvm compile error: {0}")]
    LLVMCompileError(String),
    #[error("melior error: {0}")]
    MeliorError(#[from] melior::Error),
    #[error("not yet implemented: {0}")]
    NotImplemented(String),
    #[error("C compiler error: {0}")]
    CCompilerError(String),
    #[error("IO error: {0}")]
    IOError(String),
}
