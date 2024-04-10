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
