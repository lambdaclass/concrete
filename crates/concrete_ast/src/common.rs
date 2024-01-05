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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DocString {
    contents: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct GenericParam {
    pub name: Ident,
    pub params: Vec<Ident>,
}
