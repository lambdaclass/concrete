//! C code generation backend for the Concrete compiler.
//!
//! This module provides an alternative backend that generates C code instead of MLIR/LLVM IR.
//! The generated C code is then compiled using a system C compiler (cc, gcc, clang, etc.).

use std::path::PathBuf;
use std::process::Command;

use crate::compile_unit_info::{CompileUnitInfo, DebugInfo, OptLevel};
use crate::ir::IR;

use crate::codegen::errors::CodegenError;

mod codegen;
mod types;

/// Compiles the given IR to C code and then to an object file.
///
/// Returns the path to the generated object file.
pub fn compile(session: &CompileUnitInfo, program: &IR) -> Result<PathBuf, CodegenError> {
    let c_file = session.output_file.with_extension("c");
    let object_file = session.output_file.with_extension("o");

    // Generate C code
    let c_code = codegen::generate_c_code(program)?;

    // Write C file
    std::fs::write(&c_file, &c_code).map_err(|e| CodegenError::IOError(e.to_string()))?;

    tracing::debug!("Generated C code written to {:?}", c_file);

    // Optionally keep the C file for debugging
    if !session.output_c {
        // We'll delete it after compilation, but for now keep it during development
    }

    // Invoke C compiler
    invoke_c_compiler(&c_file, &object_file, session)?;

    // Clean up C file unless output_c is set
    if !session.output_c {
        let _ = std::fs::remove_file(&c_file);
    }

    Ok(object_file)
}

/// Invokes the system C compiler to compile the generated C code to an object file.
fn invoke_c_compiler(
    c_file: &PathBuf,
    object_file: &PathBuf,
    session: &CompileUnitInfo,
) -> Result<(), CodegenError> {
    // Use CC environment variable or default to "cc"
    let compiler = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());

    let mut cmd = Command::new(&compiler);

    // Compile to object file
    cmd.arg("-c").arg(c_file).arg("-o").arg(object_file);

    // Set C standard
    cmd.arg("-std=c11");

    // Optimization level
    match session.optlevel {
        OptLevel::None => {
            cmd.arg("-O0");
        }
        OptLevel::Less => {
            cmd.arg("-O1");
        }
        OptLevel::Default => {
            cmd.arg("-O2");
        }
        OptLevel::Aggressive => {
            cmd.arg("-O3");
        }
    }

    // Debug info
    if matches!(session.debug_info, DebugInfo::Full) {
        cmd.arg("-g");
    }

    // Warnings
    cmd.arg("-Wall");
    cmd.arg("-Wno-unused-label"); // We generate many labels that may be unused

    tracing::debug!("Running C compiler: {:?}", cmd);

    let output = cmd.output().map_err(|e| {
        CodegenError::CCompilerError(format!("Failed to execute C compiler '{}': {}", compiler, e))
    })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        return Err(CodegenError::CCompilerError(format!(
            "C compiler failed:\nstdout: {}\nstderr: {}",
            stdout, stderr
        )));
    }

    tracing::debug!("C compilation successful: {:?}", object_file);

    Ok(())
}
