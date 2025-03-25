pub use self::tokens::{Token, TokenKind};
use crate::{UpdateResult, buffer::InputBuffer};
use logos::Logos;
use std::{
    cmp::Ordering,
    ops::{Deref, Range},
};

pub mod error;
mod tokens;

/// A token stream.
#[derive(Debug, Default)]
pub struct TokenStream {
    tokens: Vec<Token>,
    ranges: Vec<Range<usize>>,
}

impl TokenStream {
    /// Update a slice of the token stream.
    pub fn update(
        &mut self,
        range: Range<usize>,
        length: usize,
        buffer: &InputBuffer,
    ) -> UpdateResult {
        // Find token range to update.
        let token_range = find_update_token_range(&self.ranges, range.clone());

        // Update the span of all remaining tokens.
        let offset_delta = length as isize - range.len() as isize;
        for span in &mut self.ranges[token_range.end..] {
            span.start = span.start.wrapping_add_signed(offset_delta);
            span.end = span.end.wrapping_add_signed(offset_delta);
        }

        // Splice and reparse affected tokens.
        let splice_range = Range {
            start: self
                .ranges
                .get(token_range.start)
                .map(|range| range.start)
                .unwrap_or(0),
            end: self
                .ranges
                .get(token_range.end)
                .map(|range| range.end)
                .unwrap_or(buffer.len()),
        };
        let num_tokens = self.tokens.len() - token_range.len();

        let (new_tokens, new_ranges): (Vec<_>, Vec<_>) = Token::lexer(&buffer[splice_range])
            .spanned()
            .map(|(token, range)| (token.unwrap_or_else(Token::Error), range))
            .unzip();
        self.tokens.splice(token_range.clone(), new_tokens);
        self.ranges.splice(token_range.clone(), new_ranges);

        UpdateResult {
            range: token_range,
            length: self.tokens.len() - num_tokens,
        }
    }
}

impl Deref for TokenStream {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

fn find_update_token_range(items: &[Range<usize>], range: Range<usize>) -> Range<usize> {
    let lhs_search_fn = |span: &Range<usize>| {
        let value = range.start;
        if value <= span.start {
            Ordering::Greater
        } else if value <= span.end {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    };
    let rhs_search_fn = |span: &Range<usize>| {
        let value = range.end;
        if value < span.start {
            Ordering::Greater
        } else if value < span.end {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    };

    let lhs_offset = match items.binary_search_by(lhs_search_fn) {
        Ok(x) | Err(x) => x,
    };
    let rhs_offset = match items.binary_search_by(rhs_search_fn) {
        Ok(x) => x + 1,
        Err(x) => x,
    };

    lhs_offset..rhs_offset
}
