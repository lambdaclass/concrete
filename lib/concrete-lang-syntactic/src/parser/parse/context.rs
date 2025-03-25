use super::ParseNode;
use crate::{
    lexer::{Token, TokenKind},
    parser::storage::TreeNodeParse,
};

pub struct ParseContext<'stream, 'storage> {
    buffer: &'stream [Token],
    storage: TreeNodeParse<'storage>,
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

    pub fn next_of(&mut self, kind: TokenKind) {
        assert!(self.next_if(kind));
    }

    pub fn parse<T>(&mut self) -> usize
    where
        T: ParseNode,
    {
        let mut context = ParseContext {
            buffer: self.buffer,
            storage: self.storage.push_child::<T>(),
        };
        let extra = T::parse(&mut context);
        context.storage.set_extra(extra);

        self.buffer = context.buffer;

        let range = context.storage.range();
        let len = context.storage.len();
        self.storage.finish_child(range, len);

        extra
    }
}
