use crate::common::Ident;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ImportStmt {
    module: Vec<Ident>,
    symbols: Vec<ImportedSymbol>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ImportedSymbol {
    import_name: Ident,
    rename_into: Option<Ident>,
}
