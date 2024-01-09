use crate::{
    common::{DocString, GenericParam, Ident},
    statements::Statement,
    types::TypeSpec,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDecl {
    pub doc_string: Option<DocString>,
    pub generic_params: Vec<GenericParam>,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeSpec>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDef {
    pub decl: FunctionDecl,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub r#type: TypeSpec,
}
