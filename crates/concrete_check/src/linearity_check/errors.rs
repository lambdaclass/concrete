use concrete_ir::Span;

use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LinearityError {
    #[error("Variable {variable} not consumed at module {module:?}")]
    LinearNotConsumed {
        span: Span,
        module: String,
        program_id: usize,
        variable: String
    },
}