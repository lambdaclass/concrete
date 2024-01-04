use crate::common::{DocString, Ident};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TypeSpec {
    Simple {
        name: Ident,
    },
    Generic {
        name: Ident,
        type_params: Vec<TypeSpec>,
    },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
}
