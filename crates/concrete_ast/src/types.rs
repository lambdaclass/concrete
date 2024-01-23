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

    pub fn is_mut_ref(&self) -> bool {
        matches!(self.is_ref(), Some(RefType::MutBorrow))
    }

    pub fn get_name(&self) -> String {
        match self {
            TypeSpec::Simple { name, .. } => name.name.clone(),
            TypeSpec::Generic { name, .. } => name.name.clone(),
            TypeSpec::Array { of_type, .. } => format!("[{}]", of_type.get_name()),
        }
    }

    pub fn to_nonref_type(&self) -> TypeSpec {
        match self {
            TypeSpec::Simple {
                name,
                is_ref: _,
                span,
            } => TypeSpec::Simple {
                name: name.clone(),
                is_ref: None,
                span: *span,
            },
            TypeSpec::Generic {
                name,
                is_ref: _,
                type_params,
                span,
            } => TypeSpec::Generic {
                name: name.clone(),
                is_ref: None,
                type_params: type_params.clone(),
                span: *span,
            },
            TypeSpec::Array {
                of_type,
                size,
                is_ref: _,
                span,
            } => TypeSpec::Array {
                of_type: of_type.clone(),
                size: *size,
                is_ref: None,
                span: *span,
            },
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeSpec,
}
