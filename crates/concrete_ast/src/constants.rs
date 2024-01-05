use crate::{
    common::{DocString, Ident},
    types::TypeSpec, expressions::Expression,
};

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantDef {
    pub decl: ConstantDecl,
    pub value: Expression,
}
