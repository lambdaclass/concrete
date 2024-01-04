use crate::{
    common::{DocString, Ident, TypeParam},
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl {
    doc_string: Option<DocString>,
    name: Ident,
    type_params: Vec<TypeParam>,
    fields: Vec<Field>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Field {
    doc_string: Option<DocString>,
    name: Ident,
    r#type: TypeSpec,
}
