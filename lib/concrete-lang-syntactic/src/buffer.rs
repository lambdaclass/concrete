use crate::UpdateResult;
use std::{
    ops::{Bound, Deref, Range, RangeBounds},
    path::PathBuf,
};

/// An input buffer.
#[derive(Debug)]
pub struct InputBuffer {
    /// Path to the source, relative to the project root.
    path: PathBuf,

    /// Buffer contents.
    data: String,
}

impl InputBuffer {
    /// Update part of the buffer.
    pub fn update(&mut self, range: impl RangeBounds<usize>, value: &str) -> UpdateResult {
        self.data
            .replace_range((range.start_bound(), range.end_bound()), value);

        UpdateResult {
            range: Range {
                start: match range.start_bound() {
                    Bound::Included(&x) => x,
                    Bound::Excluded(&x) => x + 1,
                    Bound::Unbounded => 0,
                },
                end: match range.end_bound() {
                    Bound::Included(&x) => x + 1,
                    Bound::Excluded(&x) => x,
                    Bound::Unbounded => self.data.len(),
                },
            },
            length: value.len(),
        }
    }
}

impl Deref for InputBuffer {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
