use super::{
    common::{GenericParam, Ident, Span},
    types::TypeDescriptor,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructDecl {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub fields: Vec<Field>,
    pub is_pub: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Field {
    pub name: Ident,
    pub r#type: TypeDescriptor,
    pub is_pub: bool,
    pub span: Span,
}
