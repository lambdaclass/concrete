use crate::{
    common::{DocString, Ident},
    expressions::Expression,
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantDecl {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub r#type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantDef {
    decl: ConstantDecl,
    pub value: Expression,
}
