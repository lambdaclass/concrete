use super::common::{DocString, Ident, Span, TypeName};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeDescriptor {
    Type {
        name: TypeName,
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
    // Used in impl blocks.
    SelfType {
        is_ref: bool,
        is_mut: bool,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeDescriptor,
}
