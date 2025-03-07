use std::{fmt, ops::Range, path::PathBuf};

pub mod buffer;
pub mod cst;
pub mod lexer;

#[derive(Clone, Debug)]
pub struct Span {
    pub path: PathBuf,
    pub span: Range<usize>,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.path.display(),
            self.span.start,
            self.span.end,
        )
    }
}

#[derive(Clone, Debug)]
pub struct UpdateResult {
    pub range: Range<usize>,
    pub length: usize,
}
