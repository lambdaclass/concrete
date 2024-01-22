use crate::{
    common::{DocString, Ident},
    constants::ConstantDef,
    functions::FunctionDef,
    imports::ImportStmt,
    structs::StructDecl,
    types::TypeDecl,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module {
    pub doc_string: Option<DocString>,
    pub imports: Vec<ImportStmt>,
    pub name: Ident,
    pub contents: Vec<ModuleDefItem>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleDefItem {
    Constant(ConstantDef),
    Function(FunctionDef),
    Struct(StructDecl),
    Type(TypeDecl),
    Module(Module),
}
