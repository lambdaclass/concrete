use super::{
    common::{DocString, Ident, Span},
    constants::ConstantDef,
    enums::{EnumDecl, UnionDecl},
    functions::{FunctionDecl, FunctionDef},
    imports::ImportStmt,
    structs::StructDecl,
    types::TypeDecl,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module {
    pub doc_string: Option<DocString>,
    pub external_modules: Vec<Ident>,
    pub imports: Vec<ImportStmt>,
    pub name: Ident,
    pub contents: Vec<ModuleDefItem>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleDefItem {
    Constant(ConstantDef),
    Function(FunctionDef),
    FunctionDecl(FunctionDecl),
    Struct(StructDecl),
    Union(UnionDecl),
    Enum(EnumDecl),
    Type(TypeDecl),
    Module(Module),
}
