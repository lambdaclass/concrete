use concrete_ast::common::{Ident, Span};
use thiserror::Error;

use crate::{DefId, Ty};

#[derive(Debug, Error, Clone)]
pub enum LoweringError {
    #[error("module {module:?} not found")]
    ModuleNotFound {
        span: Span,
        module: String,
        program_id: usize,
    },
    #[error("function {function:?} not found")]
    FunctionNotFound {
        span: Span,
        function: String,
        program_id: usize,
    },
    #[error("struct field {name:?} not found")]
    StructFieldNotFound {
        span: Span,
        name: String,
        program_id: usize,
    },
    #[error("symbol {:?} not found", symbol.name)]
    ImportNotFound {
        module_span: Span,
        import_span: Span,
        symbol: Ident,
        program_id: usize,
    },
    #[error("use of underclared variable {name:?}")]
    UseOfUndeclaredVariable {
        span: Span,
        name: String,
        program_id: usize,
    },
    #[error("trying to mutate a non-mutable reference")]
    BorrowNotMutable {
        span: Span,
        type_span: Option<Span>,
        name: String,
        program_id: usize,
    },
    #[error("can't mutate this value because it's not declared mutable")]
    NotMutable {
        span: Span,
        declare_span: Option<Span>,
        program_id: usize,
    },
    #[error("can't take a mutable borrow to this value because it's not declared mutable")]
    CantTakeMutableBorrow {
        span: Span,
        declare_span: Option<Span>,
        program_id: usize,
    },
    #[error("unrecognized type {name}")]
    UnrecognizedType {
        span: Span,
        name: String,
        program_id: usize,
    },
    #[error("id not found")]
    IdNotFound {
        span: Span,
        id: DefId,
        program_id: usize,
    },
    #[error("feature not yet implemented: {message}")]
    NotYetImplemented {
        span: Span,
        message: &'static str,
        program_id: usize,
    },
    #[error("unexpected type")]
    UnexpectedType {
        span: Span,
        found: Ty,
        expected: Ty,
        program_id: usize,
    },
    #[error("extern function {name:?} has a body")]
    ExternFnWithBody {
        span: Span,
        name: String,
        program_id: usize,
    },
    #[error("internal error: {0}")]
    InternalError(String, usize),
    #[error("function call parameter count mismatch, found {found}, needs {needs}")]
    CallParamCountMismatch {
        span: Span,
        found: usize,
        needs: usize,
        program_id: usize,
    },
}
