use crate::common::{DocString, Ident, Span};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeSpec {
    Simple {
        name: Ident,
    },
    Generic {
        name: Ident,
        type_params: Vec<TypeSpec>,
        span: Span,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub value: TypeSpec,
}
