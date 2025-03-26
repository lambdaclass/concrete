use crate::lexer::TokenKind;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};
use thiserror::Error;

// Error:
//   - TokenStream offset
//   - Actual token.
//   - Expected items:
//     - BTreeMap<NodeName, BTreeSet<TokenKind>>
//     - BTreeSet<TokenKind>

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub struct Error {
    /// The `TokenStream` offset at the error.
    pub offset: usize,

    /// The expected starting symbols of non-terminals.
    pub expected_nodes: BTreeMap<String, BTreeSet<TokenKind>>,
    /// The expected symbols of the current non-terminal.
    pub altern_symbols: BTreeSet<TokenKind>,

    /// Whether the end-of-stream is also a valid symbol or not.
    pub at_end: bool,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        todo!()
    }
}
