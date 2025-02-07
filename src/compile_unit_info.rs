use ariadne::Source;
use std::path::PathBuf;

/// This struct holds the information needed to compile this compilation unit,
/// like whether to generate debug info, optimization levels, target, host, etc.
#[derive(Debug, Clone)]
pub struct CompileUnitInfo {
    /// The file paths of the included sources from the initial compile unit.
    pub file_paths: Vec<PathBuf>,
    /// Whether to output debug info.
    pub debug_info: DebugInfo,
    /// The optimization level to use with this compilation unit.
    pub optlevel: OptLevel,
    /// Sources for debugging locations.
    pub sources: Vec<Source<String>>,
    /// True if it should be compiled as a library false for binary.
    pub library: bool,
    /// The file where to put the compilation result.
    /// The file name will be used for all the other options, if it's
    /// a library, the platform extension will be added.
    pub output_file: PathBuf,
    /// Whether to output the generated MLIR file for this compile unit.
    pub output_mlir: bool,
    /// Whether to output the generated LLVM IR file for this compile unit.
    pub output_ll: bool,
    /// Whether to output the generated assembly file for this compile unit.
    pub output_asm: bool,
    // todo: include target, host, etc
}

impl CompileUnitInfo {
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

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum OptLevel {
    None,       // -O0
    Less,       // -O1
    Default,    // -O2
    Aggressive, // -O3
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum DebugInfo {
    None,
    Full,
}
