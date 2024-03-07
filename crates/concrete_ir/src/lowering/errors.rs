use concrete_ast::common::{Ident, Span};
use thiserror::Error;

use crate::{DefId, Ty, TyKind};

#[derive(Debug, Error, Clone)]
pub enum LoweringError {
    #[error("module {module:?} not found")]
    ModuleNotFound { span: Span, module: String },
    #[error("function {function:?} not found")]
    FunctionNotFound { span: Span, function: String },
    #[error("struct field {name:?} not found")]
    StructFieldNotFound { span: Span, name: String },
    #[error("symbol {:?} not found", symbol.name)]
    ImportNotFound {
        module_span: Span,
        import_span: Span,
        symbol: Ident,
    },
    #[error("use of underclared variable {name:?}")]
    UseOfUndeclaredVariable { span: Span, name: String },
    #[error("trying to mutate a non-mutable reference")]
    BorrowNotMutable {
        span: Span,
        type_span: Option<Span>,
        name: String,
    },
    #[error("unrecognized type {name}")]
    UnrecognizedType { span: Span, name: String },
    #[error("id not found")]
    IdNotFound { span: Span, id: DefId },
    #[error("feature not yet implemented: {message}")]
    NotYetImplemented { span: Span, message: &'static str },
    #[error("unexpected type")]
    UnexpectedType {
        span: Span,
        found: TyKind,
        expected: Ty,
    },
    #[error("extern function {name:?} has a body")]
    ExternFnWithBody { span: Span, name: String },
}
