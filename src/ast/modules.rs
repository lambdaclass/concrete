use std::{path::PathBuf, sync::Arc};

use super::{
    common::{DocString, Ident, Span},
    constants::ConstantDef,
    enums::{EnumDecl, UnionDecl},
    functions::{FunctionDecl, FunctionDef, ImplBlock},
    imports::ImportStmt,
    structs::StructDecl,
    types::TypeDecl,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Module {
    pub doc_string: Option<DocString>,
    pub name: Ident,
    pub contents: Vec<ModuleDefItem>,
    pub file_path: PathBuf,
    pub span: Span,
}

/// Top level module items, behind Arcs so they are cheaply clonable during lowering.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ModuleDefItem {
    Constant(Arc<ConstantDef>),
    Function(Arc<FunctionDef>),
    FunctionDecl(Arc<FunctionDecl>),
    Impl(Arc<ImplBlock>),
    Struct(Arc<StructDecl>),
    Union(Arc<UnionDecl>),
    Enum(Arc<EnumDecl>),
    Type(Arc<TypeDecl>),
    Module(Arc<Module>),
    ExternalModule(Ident),
    Import(Arc<ImportStmt>),
}
