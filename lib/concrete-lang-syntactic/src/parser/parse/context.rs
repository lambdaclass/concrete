use super::ParseNode;
use crate::{
    lexer::{Token, TokenKind},
    parser::{
        error::{Error, Result},
        storage::TreeNodeParse,
    },
};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug)]
pub struct ParseContext<'stream, 'storage> {
    buffer: &'stream [Token],
    storage: TreeNodeParse<'storage>,
    // TODO: Add an error field that gets updated or cleared automatically when parsing.
}

impl<'stream, 'storage> ParseContext<'stream, 'storage> {
    #[doc(hidden)]
    pub(crate) fn new(buffer: &'stream [Token], storage: TreeNodeParse<'storage>) -> Self {
        Self { buffer, storage }
    }

    pub fn peek(&self) -> Option<TokenKind> {
        self.buffer.first().map(Token::kind)
    }

    pub fn next_if(&mut self, kind: TokenKind) -> bool {
        match self.buffer.split_first() {
            Some((token, rest)) if token.kind() == kind => {
                self.buffer = rest;
                self.storage.push_token();
                true
            }
            _ => false,
        }
    }

    #[track_caller]
    pub fn next_of(&mut self, kind: TokenKind) -> Result<()> {
        if !self.next_if(kind) {
            return Err(Error {
                offset: self.storage.range().end,
                expected_nodes: BTreeMap::new(),
                altern_symbols: BTreeSet::from([kind]),
                at_end: false,
            });
        }

        Ok(())
    }

    pub fn is_end(&self) -> bool {
        self.buffer.is_empty()
    }

    pub fn parse<T>(&mut self) -> Result<usize>
    where
        T: ParseNode,
    {
        let mut context = ParseContext {
            buffer: self.buffer,
            storage: self.storage.push_child::<T>(),
        };
        let result = T::parse(&mut context);
        if let Ok(extra) = result {
            context.storage.set_extra(extra);
        }

        self.buffer = context.buffer;

        let range = context.storage.range();
        let len = context.storage.len();
        self.storage.finish_child(range, len);

        result
    }
}
