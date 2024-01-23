use crate::common::{DocString, Ident, Span};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Copy)]
pub enum RefType {
    Borrow,
    MutBorrow,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeSpec {
    Simple {
        name: Ident,
        is_ref: Option<RefType>,
        span: Span,
    },
    Generic {
        name: Ident,
        is_ref: Option<RefType>,
        type_params: Vec<TypeSpec>,
        span: Span,
    },
    Array {
        of_type: Box<Self>,
        size: Option<u32>,
        is_ref: Option<RefType>,
        span: Span,
    },
}

impl TypeSpec {
    pub fn is_ref(&self) -> Option<RefType> {
        match self {
            TypeSpec::Simple { is_ref, .. } => *is_ref,
            TypeSpec::Generic { is_ref, .. } => *is_ref,
            TypeSpec::Array { is_ref, .. } => *is_ref,
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            TypeSpec::Simple { name, .. } => name.name.clone(),
            TypeSpec::Generic { name, .. } => name.name.clone(),
            TypeSpec::Array { of_type, .. } => format!("[{}]", of_type.get_name()),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeSpec,
}
