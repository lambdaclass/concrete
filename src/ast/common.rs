use core::fmt;
use std::ops::Range;

use itertools::Itertools;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

impl Span {
    pub fn new(from: usize, to: usize) -> Self {
        Self { from, to }
    }
}

impl From<Span> for Range<usize> {
    fn from(val: Span) -> Self {
        val.from..val.to
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct DocString {
    pub contents: Vec<String>,
    pub span: Span,
}

/// Identifiers, without a path or generics.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

/// A type name, supporting fully qualified paths and generics:
/// ```text
/// std::module::X<T>
/// ```
/// Used only when specifying types.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct TypeName {
    pub path: Vec<Ident>,
    pub name: Ident,
    pub generics: Vec<TypeName>,
    pub span: Span,
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut generics = self.generics.iter().map(|x| x.to_string()).join(", ");
        if !generics.is_empty() {
            generics = format!("<{}>", generics);
        }
        let path = self.path.iter().map(|x| &x.name).join(".");
        write!(f, "{}{}{}", path, self.name.name, generics)
    }
}

/// Used as a generic parameter definition in function / adt declarations.
///
/// Note: When initializing a type with generics, we use a TypeName, not a GenericParam.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    pub bounds: Vec<TypeName>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Attribute {
    pub name: String,
    pub value: Option<String>,
    pub span: Span,
}
