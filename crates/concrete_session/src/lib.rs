use ariadne::Source;
use std::path::PathBuf;

use config::{DebugInfo, OptLevel};

pub mod config;

#[derive(Debug, Clone)]
pub struct Session {
    pub debug_info: DebugInfo,
    pub optlevel: OptLevel,
    pub sources: Vec<Source>, // for debugging locations
    /// True if it should be compiled as a library false for binary.
    pub library: bool,
    pub output_file: PathBuf,
    pub output_mlir: bool,
    pub output_ll: bool,
    pub output_asm: bool,
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
