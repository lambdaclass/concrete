pub use self::context::ParseContext;
use super::error::Result;
use crate::lexer::TokenKind;

mod context;

pub trait ParseNode {
    fn check(kind: Option<TokenKind>) -> CheckResult;
    fn parse(context: &mut ParseContext) -> Result<usize>;
}

impl<T> ParseNode for Option<T>
where
    T: ParseNode,
{
    fn check(kind: Option<TokenKind>) -> CheckResult {
        match T::check(kind) {
            CheckResult::Always(_) => CheckResult::Always(1),
            CheckResult::Empty(_) => unreachable!(),
            CheckResult::Never => CheckResult::Empty(0),
        }
    }

    fn parse(context: &mut ParseContext) -> Result<usize> {
        Ok(match Self::check(context.peek()).value() {
            Some(_) => {
                context.parse::<T>()?;
                1
            }
            None => 0,
        })
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum CheckResult {
    Always(usize),
    Empty(usize),
    #[default]
    Never,
}

impl CheckResult {
    pub fn is_always(self) -> bool {
        matches!(self, Self::Always(_))
    }

    pub fn value(self) -> Option<usize> {
        match self {
            CheckResult::Always(value) | CheckResult::Empty(value) => Some(value),
            CheckResult::Never => None,
        }
    }

    pub fn map(self, value: usize) -> Self {
        match self {
            CheckResult::Always(_) => CheckResult::Always(value),
            CheckResult::Empty(_) => CheckResult::Empty(value),
            CheckResult::Never => CheckResult::Never,
        }
    }

    pub fn followed_by(self, f: impl FnOnce() -> Self) -> Self {
        match self {
            CheckResult::Always(_) => CheckResult::Always(0),
            CheckResult::Empty(_) => f().map(0),
            CheckResult::Never => CheckResult::Never,
        }
    }
}
