use crate::{
    common::{DocString, GenericParam, Ident},
    types::TypeSpec,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub type_params: Vec<GenericParam>,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Field {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub r#type: TypeSpec,
}
