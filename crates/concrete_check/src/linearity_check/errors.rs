//use concrete_ir::Span;

use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LinearityError {
    #[error("Variable {variable} not consumed")]
    NotConsumed { variable: String },
    #[error("Borrowed mutably and used for Variable {variable}")]
    BorrowedMutUsed { variable: String },
    #[error("Variable {variable} borrowed mutably more than once")]
    BorrowedMutMoreThanOnce { variable: String },
    #[error("Variable {variable} consumed once and then used again")]
    ConsumedAndUsed { variable: String },
    #[error("Variable {variable} consumed more than once")]
    ConsumedMoreThanOnce { variable: String },
    #[error("Variable {variable} read borrowed and used in other ways")]
    ReadBorrowedAndUsed { variable: String },
    #[error("Variable {variable} write borrowed and used")]
    WriteBorrowedAndUsed { variable: String },
    #[error("Variable {variable} already consumed and used again")]
    AlreadyConsumedAndUsed { variable: String },
    #[error("Unhandled state or appearance count for Variable {variable}")]
    UnhandledStateOrCount { variable: String },
    #[error("Linearity error. Variable {variable} generated {message}")]
    Unspecified { variable: String, message: String },
    #[error("Variable {variable} not found")]
    VariableNotFound { variable: String },
    #[error("Inconsistent state {message}")]
    StateInconsistency { message: String },
    #[error("Not implemented: {message}")]
    NotImplemented{message: String},
    #[error("Unhandled statement type {r#type}")]
    UnhandledStatementType { r#type: String },
}
