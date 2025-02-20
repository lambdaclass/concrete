use self::error::Result;
pub use self::tokens::Token;
use crate::{UpdateResult, buffer::InputBuffer};
use logos::Logos;
use std::{cmp::Ordering, ops::Range, path::PathBuf};

pub mod error;
mod tokens;

/// A token stream.
#[derive(Debug)]
pub struct TokenStream {
    /// Path to the source, relative to the project root.
    path: PathBuf,

    /// The stream's tokens.
    tokens: Vec<(Result<Token>, Range<usize>)>,
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
        let token_range = find_update_token_range(self.tokens.as_slice(), range.clone());

        // Update the span of all remaining tokens.
        let offset_delta = length as isize - range.len() as isize;
        for (_, span) in &mut self.tokens[token_range.end..] {
            span.start = span.start.wrapping_add_signed(offset_delta);
            span.end = span.end.wrapping_add_signed(offset_delta);
        }

        // Splice and reparse affected tokens.
        let splice_range = Range {
            start: self.tokens[token_range.start].1.start,
            end: self.tokens[token_range.end].1.end,
        };
        let num_tokens = self.tokens.len() - token_range.len();
        self.tokens.splice(
            token_range.clone(),
            Token::lexer(&buffer[splice_range]).spanned(),
        );

        UpdateResult {
            range: token_range,
            length: self.tokens.len() - num_tokens,
        }
    }
}

fn find_update_token_range(
    items: &[(Result<Token>, Range<usize>)],
    range: Range<usize>,
) -> Range<usize> {
    let lhs_search_fn = |(_, span): &(_, Range<usize>)| {
        let value = range.start;
        if value <= span.start {
            Ordering::Greater
        } else if value <= span.end {
            Ordering::Equal
        } else {
            Ordering::Less
        }
    };
    let rhs_search_fn = |(_, span): &(_, Range<usize>)| {
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
