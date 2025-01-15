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
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeDescriptor,
}
