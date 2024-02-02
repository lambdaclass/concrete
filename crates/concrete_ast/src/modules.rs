use crate::{
    common::{DocString, Ident, Span},
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
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleDefItem {
    Constant(ConstantDef),
    Function(FunctionDef),
    Struct(StructDecl),
    Type(TypeDecl),
    Module(Module),
}
