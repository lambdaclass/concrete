use crate::{
    common::{DocString, Ident, TypeParam},
    statements::Statement,
    types::TypeSpec,
};

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDecl {
    pub doc_string: Option<DocString>,
    pub type_params: Vec<TypeParam>,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_type: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDef {
    pub decl: FunctionDecl,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct MethodDecl {
    pub doc_string: Option<DocString>,
    pub type_params: Vec<TypeParam>,
    pub name: Ident,
    pub params: Vec<Param>,
    pub ret_ty: TypeSpec,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodDef {
    pub decl: MethodDecl,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Param {
    pub name: Ident,
    pub r#type: TypeSpec,
}
