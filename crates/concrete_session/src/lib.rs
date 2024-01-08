use std::path::PathBuf;

use config::{DebugInfo, OptLevel};

pub mod config;

#[derive(Debug, Clone)]
pub struct Session {
    pub file_path: PathBuf,
    pub debug_info: DebugInfo,
    pub optlevel: OptLevel,
    pub source: String, // for debugging locations
    /// True if it should be compiled as a library false for binary.
    pub library: bool,
    /// The directory where to store artifacts and intermediate files such as object files.
    pub target_dir: PathBuf,
    pub output_file: PathBuf,
    // todo: include target, host, etc
}

impl Session {
    pub fn get_line_and_column(&self, offset: usize) -> (usize, usize) {
        let sl = &self.source[0..offset];
        let line_count = sl.lines().count();
        let column = sl.rfind('\n').unwrap_or(0);
        (line_count, column)
    }
}
