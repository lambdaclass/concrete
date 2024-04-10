use crate::{
    common::{GenericParam, Ident, Span},
    structs::Field,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnionDecl {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<Field>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumDecl {
    pub name: Ident,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub fields: Vec<Field>,
    pub span: Span,
}
