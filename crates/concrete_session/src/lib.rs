use ariadne::Source;
use std::path::PathBuf;

use config::{DebugInfo, OptLevel};

pub mod config;

#[derive(Debug, Clone)]
pub struct Session {
    pub file_path: PathBuf,
    pub debug_info: DebugInfo,
    pub optlevel: OptLevel,
    pub source: Source<String>, // for debugging locations
    /// True if it should be compiled as a library false for binary.
    pub library: bool,
    /// The directory where to store artifacts and intermediate files such as object files.
    pub target_dir: PathBuf,
    pub output_file: PathBuf,
    pub output_mlir: bool,
    pub output_ll: bool,
    pub output_asm: bool,
    pub output_all: bool,
    // todo: include target, host, etc
}

impl Session {
    pub fn get_platform_library_ext() -> &'static str {
        if cfg!(target_os = "macos") {
            "dylib"
        } else if cfg!(target_os = "windows") {
            "dll"
        } else {
            "so"
        }
    }
}
