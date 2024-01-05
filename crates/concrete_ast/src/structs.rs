use crate::{
    common::{DocString, GenericParam, Ident},
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl {
    doc_string: Option<DocString>,
    name: Ident,
    type_params: Vec<GenericParam>,
    fields: Vec<Field>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Field {
    doc_string: Option<DocString>,
    name: Ident,
    r#type: TypeSpec,
}
