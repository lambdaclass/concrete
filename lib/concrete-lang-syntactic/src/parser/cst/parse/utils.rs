use crate::{
    lexer::TokenKind,
    parser::{
        error::Result,
        parse::{CheckResult, ParseContext, ParseNode},
    },
};
use std::marker::PhantomData;

/// Parser for items that may have ABI specifications, like functions or FFI blocks.
pub struct WithAbi<T>(PhantomData<T>);

impl<T> ParseNode for WithAbi<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::KwExtern) => CheckResult::Always(0),
            _ => T::check(kind),
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_if(TokenKind::KwExtern);
        context.next_if(TokenKind::LitString);
        context.parse::<T>()?;

        Ok(0)
    }
}

/// Parser for items that may have documentation.
pub struct WithDoc<T>(PhantomData<T>);

impl<T> ParseNode for WithDoc<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::DocString) => CheckResult::Always(0),
            _ => T::check(kind),
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_if(TokenKind::DocString);
        context.parse::<T>()?;

        Ok(0)
    }
}

/// Parser for items that may have an associated visibility modifier.
pub struct WithVis<T>(PhantomData<T>);

impl<T> ParseNode for WithVis<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match kind {
            Some(TokenKind::KwPub) => CheckResult::Always(0),
            _ => T::check(kind),
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        let is_pub = context.next_if(TokenKind::KwPub);
        context.parse::<T>()?;

        Ok(is_pub as usize)
    }
}

/// Parser for items wrapped between `<` and `>`.
pub struct Angles<T>(PhantomData<T>);

impl<T> ParseNode for Angles<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::SymCmpLt))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::SymCmpLt)?;
        context.parse::<T>()?;
        context.next_of(TokenKind::SymCmpGt)?;

        Ok(0)
    }
}

/// Parser for items wrapped between `{` and `}`.
pub struct Braces<T>(PhantomData<T>);

impl<T> ParseNode for Braces<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::LBrace))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::LBrace)?;
        context.parse::<T>()?;
        context.next_of(TokenKind::RBrace)?;

        Ok(0)
    }
}

/// Parser for items wrapped between `[` and `]`.
pub struct Brackets<T>(PhantomData<T>);

impl<T> ParseNode for Brackets<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::LBracket))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::LBracket)?;
        context.parse::<T>()?;
        context.next_of(TokenKind::RBracket)?;

        Ok(0)
    }
}

/// Parser for items wrapped between `(` and `)`.
pub struct Parens<T>(PhantomData<T>);

impl<T> ParseNode for Parens<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        (kind == Some(TokenKind::LParen))
            .then_some(CheckResult::Always(0))
            .unwrap_or_default()
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        context.next_of(TokenKind::LParen)?;
        context.parse::<T>()?;
        context.next_of(TokenKind::RParen)?;

        Ok(0)
    }
}

/// Parser for contiguous sequences of items, with optional minimum and maximum lengths.
pub struct Seq<T, const MIN: usize = { usize::MIN }, const MAX: usize = { usize::MAX }>(
    PhantomData<T>,
);

impl<T, const MIN: usize, const MAX: usize> ParseNode for Seq<T, MIN, MAX>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match T::check(kind) {
            CheckResult::Always(_) => CheckResult::Always(0),
            CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => CheckResult::Empty(0),
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        const {
            assert!(MAX >= MIN);
        }

        for _ in 0..MIN {
            context.parse::<T>()?;
        }

        for _ in MIN..MAX {
            match T::check(context.peek()) {
                CheckResult::Always(_) => context.parse::<T>()?,
                CheckResult::Empty(_) => unreachable!(),
                CheckResult::Never => break,
            };
        }

        Ok(0)
    }
}

/// Parser for comma-separated sequences of items, with optional trailing separator, minimum and
/// maximum lengths.
// TODO: Abstract `CommaSep` into `Sep` with generic separator.
pub struct CommaSep<
    T,
    const MIN: usize = { usize::MIN },
    const MAX: usize = { usize::MAX },
    const ALLOW_TRAILING: bool = true,
>(PhantomData<T>);

impl<T, const MIN: usize, const MAX: usize, const ALLOW_TRAILING: bool> ParseNode
    for CommaSep<T, MIN, MAX, ALLOW_TRAILING>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        Seq::<T, MIN, MAX>::check(kind)
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        // TODO: Support MIN and MAX bounds.

        while T::check(context.peek()).is_always() {
            context.parse::<T>()?;

            if !context.next_if(TokenKind::SymComma) || !ALLOW_TRAILING {
                break;
            }
        }

        Ok(0)
    }
}

/// Utility to check if an enum is valid.
///
/// Will detect ambiguities in the following cases:
///   - Two `CheckResult::Empty(_)` are detected before a `CheckResult::Always` is seen.
///   - Two `CheckResult::Always(_)` are detected.
pub fn check_enum<const N: usize>(variants: [CheckResult; N]) -> CheckResult {
    let mut result = CheckResult::Never;
    for (index, variant) in variants.into_iter().enumerate() {
        use CheckResult::*;
        result = match (result, variant) {
            (Always(_), Always(_)) | (Empty(_), Empty(_)) => unreachable!(),

            (Always(x), _) => Always(x),
            (_, Always(_)) => Always(index),

            (Empty(x), _) => Empty(x),
            (_, Empty(_)) => Empty(index),

            (Never, Never) => Never,
        };
    }

    result
}
