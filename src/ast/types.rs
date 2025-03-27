use std::fmt;

use super::common::{DocString, Ident, Span, TypeName};
use educe::Educe;

#[derive(Clone, Debug, Educe)]
#[educe(PartialEq, Eq, Hash)]
pub enum TypeDescriptor {
    Type {
        name: TypeName,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    Ref {
        of: Box<Self>,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    MutRef {
        of: Box<Self>,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    ConstPtr {
        of: Box<Self>,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    MutPtr {
        of: Box<Self>,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    Array {
        of: Box<Self>,
        size: u64,
        #[educe(PartialEq(ignore), Hash(ignore))]
        span: Span,
    },
    // Used in impl blocks.
    SelfType {
        is_ref: bool,
        is_mut: bool,
        span: Span,
    },
}

impl TypeDescriptor {
    pub fn get_name(&self) -> Option<String> {
        match self {
            TypeDescriptor::Type { name, .. } => Some(name.name.name.clone()),
            TypeDescriptor::Ref { of, .. } => of.get_name(),
            TypeDescriptor::MutRef { of, .. } => of.get_name(),
            TypeDescriptor::ConstPtr { of, .. } => of.get_name(),
            TypeDescriptor::MutPtr { of, .. } => of.get_name(),
            TypeDescriptor::Array { .. } => None,
            TypeDescriptor::SelfType { .. } => None,
        }
    }

    pub fn get_generics(&self) -> Option<Vec<TypeName>> {
        match self {
            TypeDescriptor::Type { name, .. } => Some(name.generics.clone()),
            _ => None,
        }
    }

    pub fn get_span(&self) -> Span {
        match self {
            TypeDescriptor::Type { span, .. } => *span,
            TypeDescriptor::Ref { span, .. } => *span,
            TypeDescriptor::MutRef { span, .. } => *span,
            TypeDescriptor::ConstPtr { span, .. } => *span,
            TypeDescriptor::MutPtr { span, .. } => *span,
            TypeDescriptor::Array { span, .. } => *span,
            TypeDescriptor::SelfType { span, .. } => *span,
        }
    }
}

impl From<TypeName> for TypeDescriptor {
    fn from(value: TypeName) -> Self {
        Self::Type {
            span: value.span,
            name: value,
        }
    }
}

impl fmt::Display for TypeDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDescriptor::Type { name, .. } => {
                write!(f, "{}", name)
            }
            TypeDescriptor::Ref { of, span: _ } => write!(f, "&{of}"),
            TypeDescriptor::MutRef { of, .. } => write!(f, "&mut {of}"),
            TypeDescriptor::ConstPtr { of, .. } => write!(f, "*const {of}"),
            TypeDescriptor::MutPtr { of, .. } => write!(f, "*mut {of}"),
            TypeDescriptor::Array { of, .. } => write!(f, "&{of}"),
            TypeDescriptor::SelfType { is_ref, is_mut, .. } => match (*is_ref, *is_mut) {
                (true, true) => write!(f, "&mut self"),
                (true, false) => write!(f, "&self"),
                (false, true) => write!(f, "mut self"),
                (false, false) => write!(f, "self"),
            },
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeDescriptor,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AssociatedType {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: Option<TypeDescriptor>,
    pub span: Span,
}
