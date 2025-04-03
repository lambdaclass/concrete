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
    #[error("trait {name:?} not found")]
    TraitNotFound {
        span: Span,
        name: String,
        path: PathBuf,
    },
    #[error("missing trait associated type in trait impl")]
    MissingTraitType(Box<MissingTraitType>),
    #[error("missing trait associated type in trait impl")]
    UnexpectedTraitType(Box<UnexpectedTraitType>),
    #[error("missing trait function in trait impl")]
    MissingTraitFunction(Box<MissingTraitFunction>),
    #[error("unexpected trait function in trait impl")]
    UnexpectedTraitFunction(Box<UnexpectedTraitFunction>),
    #[error("can't infer type")]
    CantInferType(Box<CantInferType>),
    #[error("trait bound not met")]
    TraitBoundNotMet(Box<TraitBoundNotMet>),
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

#[derive(Debug, Clone)]
pub struct MissingTraitType {
    pub trait_name: String,
    pub trait_span: Span,
    pub type_name: String,
    pub type_name_span: Span,
    pub assoc_type_name: String,
    pub assoc_type_name_span_def: Span,
    pub impl_trait_span: Span,
    pub path: PathBuf,
    pub trait_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct UnexpectedTraitType {
    pub trait_name: String,
    pub trait_span: Span,
    pub type_name: String,
    pub type_name_span: Span,
    pub assoc_type_name: String,
    pub assoc_type_name_span_def: Span,
    pub impl_trait_span: Span,
    pub path: PathBuf,
    pub trait_path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct MissingTraitFunction {
    pub trait_name: String,
    pub trait_span: Span,
    pub type_name: String,
    pub type_name_span: Span,
    pub func_name: String,
    pub func_name_span_def: Span,
    pub impl_trait_span: Span,
    pub path: PathBuf,
    pub trait_path: PathBuf,
}

pub type UnexpectedTraitFunction = MissingTraitFunction;

#[derive(Debug, Clone)]
pub struct CantInferType {
    pub message: String,
    pub span: Span,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct TraitBoundNotMet {
    pub trait_name: String,
    pub trait_span: Span,
    pub func_name: String,
    pub func_name_span: Span,
    pub param_name: String,
    pub param_span: Span,
    pub path: PathBuf,
}
