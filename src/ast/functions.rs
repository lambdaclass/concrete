use std::sync::Arc;

use super::{
    common::{Attribute, DocString, GenericParam, Ident, Span},
    statements::Statement,
    types::TypeDescriptor,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDecl {
    pub doc_string: Option<DocString>,
    pub generic_params: Vec<GenericParam>,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeDescriptor>,
    pub is_extern: bool,
    pub is_pub: bool,
    pub attributes: Vec<Attribute>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDef {
    pub decl: Arc<FunctionDecl>,
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub r#type: TypeDescriptor,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ImplBlock {
    pub target: TypeDescriptor,
    pub methods: Vec<Arc<FunctionDef>>,
    pub span: Span,
}
