use crate::{
    common::{GenericParam, Ident, Span},
    types::TypeSpec,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructDecl {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Field {
    pub name: Ident,
    pub r#type: TypeSpec,
    pub span: Span,
}
