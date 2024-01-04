#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub from: usize,
    pub to: usize,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DocString {
    contents: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Ident {
    pub name: String,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeParam {
    pub name: Ident,
    pub params: Vec<Ident>,
}
