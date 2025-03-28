use super::{
    common::{DocString, Ident},
    expressions::Expression,
    types::TypeDescriptor,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstantDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub is_pub: bool,
    pub r#type: TypeDescriptor,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConstantDef {
    pub decl: ConstantDecl,
    pub value: Expression,
}
