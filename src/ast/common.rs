use std::ops::Range;

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
/// ```
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

/// Used as a generic param in function and struct declarations.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct Attribute {
    pub name: String,
    pub value: Option<String>,
    pub span: Span,
}
