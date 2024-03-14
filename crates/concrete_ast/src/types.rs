use crate::common::{DocString, Ident, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeQualifier {
    Ref,    // &
    RefMut, // &mut
    Ptr,    // *const
    PtrMut, // *mut
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeSpec {
    Simple {
        name: Ident,
        qualifiers: Vec<TypeQualifier>,
        span: Span,
    },
    Generic {
        name: Ident,
        qualifiers: Vec<TypeQualifier>,
        type_params: Vec<TypeSpec>,
        span: Span,
    },
    Array {
        of_type: Box<Self>,
        size: u64,
        qualifiers: Vec<TypeQualifier>,
        span: Span,
    },
}

impl TypeSpec {
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
