use crate::common::{Ident, Span};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ImportStmt {
    pub module: Vec<Ident>,
    pub symbols: Vec<Ident>,
    pub span: Span,
}
