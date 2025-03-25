use std::sync::Arc;

use crate::ir::Span;

use super::{
    common::{DocString, GenericParam, Ident},
    functions::FunctionDecl,
    types::TypeDecl,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TraitDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub generic_params: Vec<GenericParam>,
    pub is_pub: bool,
    pub span: Span,
    pub associated_types: Vec<Arc<TypeDecl>>,
    pub methods: Vec<Arc<FunctionDecl>>,
}
