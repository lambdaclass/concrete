use logos::Logos;
use std::{convert::Infallible, fmt}; // to implement the Display trait

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
    NumberParseError,
    #[default]
    Other,
}

impl From<std::num::ParseIntError> for LexingError {
    fn from(_: std::num::ParseIntError) -> Self {
        LexingError::NumberParseError
    }
}

impl From<Infallible> for LexingError {
    fn from(_: Infallible) -> Self {
        LexingError::Other
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexingError, skip r"[ \t\n\f]+", skip r"//.*\n?", skip r"/\*(?:[^*]|\*[^/])*\*/")]
pub enum Token {
    #[regex(r"[a-zA-Z][a-zA-Z\d]*", |lex| lex.slice().parse())]
    Identifier(String),
}
