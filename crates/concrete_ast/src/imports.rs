use crate::common::Ident;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ImportStmt {
    pub module: Vec<Ident>,
    pub symbols: Vec<Ident>,
}
