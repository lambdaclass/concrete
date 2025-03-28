use std::path::PathBuf;

use crate::ast::common::{Ident, Span};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LoweringError {
    #[error("module {module:?} not found")]
    ModuleNotFound {
        span: Span,
        module: String,
        path: PathBuf,
    },
    #[error("function {function:?} not found")]
    FunctionNotFound {
        span: Span,
        function: String,
        path: PathBuf,
    },
    #[error("field {name:?} not found")]
    FieldNotFound {
        span: Span,
        name: String,
        path: PathBuf,
    },
    #[error("symbol {:?} not found", symbol.name)]
    ImportNotFound {
        module_span: Span,
        import_span: Span,
        symbol: Ident,
        path: PathBuf,
    },
    #[error("use of underclared variable {name:?}")]
    UseOfUndeclaredVariable {
        span: Span,
        name: String,
        path: PathBuf,
    },
    #[error("trying to mutate a non-mutable reference")]
    BorrowNotMutable {
        span: Span,
        type_span: Option<Span>,
        name: String,
        path: PathBuf,
    },
    #[error("can't mutate this value because it's not declared mutable")]
    NotMutable {
        span: Span,
        declare_span: Option<Span>,
        path: PathBuf,
    },
    #[error("can't take a mutable borrow to this value because it's not declared mutable")]
    CantTakeMutableBorrow {
        span: Span,
        declare_span: Option<Span>,
        path: PathBuf,
    },
    #[error("unrecognized type {name}")]
    UnrecognizedType {
        span: Span,
        name: String,
        path: PathBuf,
    },
    #[error("feature not yet implemented: {message}")]
    NotYetImplemented {
        span: Span,
        message: &'static str,
        path: PathBuf,
    },
    #[error("unexpected type")]
    UnexpectedType {
        found_span: Span,
        found: String,
        expected: String,
        expected_span: Option<Span>,
        path: PathBuf,
    },
    #[error("invalid unary op on given type")]
    InvalidUnaryOp {
        found_span: Span,
        found: String,
        path: PathBuf,
    },
    #[error("extern function {name:?} has a body")]
    ExternFnWithBody {
        span: Span,
        name: String,
        path: PathBuf,
    },
    #[error("function call parameter count mismatch, found {found}, needs {needs}")]
    CallParamCountMismatch {
        span: Span,
        found: usize,
        needs: usize,
        path: PathBuf,
    },
    #[error("function generic parameter count mismatch, found {found}, needs {needs}")]
    GenericCountMismatch {
        span: Span,
        found: usize,
        needs: usize,
        path: PathBuf,
    },
    #[error("unknown lang item {item}")]
    UnknownLangItem {
        span: Span,
        item: String,
        path: PathBuf,
    },
    #[error("invalid match, reason: {reason}")]
    InvalidMatch {
        span: Span,
        reason: String,
        path: PathBuf,
    },
    #[error("unimplemented: {reason}")]
    Unimplemented {
        span: Span,
        reason: String,
        path: PathBuf,
    },
    #[error("missing variant")]
    MissingVariant(Box<MissingVariantError>),
}

#[derive(Debug, Clone)]
pub struct MissingVariantError {
    pub match_span: Span,
    pub variant_span: Span,
    pub variant_name: String,
    pub type_name: String,
    pub type_span: Span,
    pub type_path: PathBuf,
    pub path: PathBuf,
}
