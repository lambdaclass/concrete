use super::common::{DocString, Ident, Span};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeDescriptor {
    Type {
        name: Ident,
        generics: Vec<Self>,
        span: Span,
    },
    Ref {
        of: Box<Self>,
        span: Span,
    },
    MutRef {
        of: Box<Self>,
        span: Span,
    },
    ConstPtr {
        of: Box<Self>,
        span: Span,
    },
    MutPtr {
        of: Box<Self>,
        span: Span,
    },
    Array {
        of: Box<Self>,
        size: u64,
        span: Span,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeDescriptor,
}
