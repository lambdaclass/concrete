use std::path::Path;

use tracing::instrument;

use crate::errors::CodegenError;

// TODO: Implement a proper linker driver, passing only the arguments needed dynamically based on the requirements.

#[instrument(level = "debug")]
pub fn link_shared_lib(input_path: &Path, output_filename: &Path) -> Result<(), CodegenError> {
    let args: &[&str] = {
        #[cfg(target_os = "macos")]
        {
            &[
                "-demangle",
                "-no_deduplicate",
                "-dynamic",
                "-dylib",
                "-L/usr/local/lib",
                "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
                &input_path.display().to_string(),
                "-o",
                &output_filename.display().to_string(),
                "-lSystem",
            ]
        }
        #[cfg(target_os = "linux")]
        {
            &[
                "--hash-style=gnu",
                "--eh-frame-hdr",
                "-shared",
                "-o",
                &output_filename.display().to_string(),
                "-L/lib/../lib64",
                "-L/usr/lib/../lib64",
                "-lc",
                "-O1",
                &input_path.display().to_string(),
            ]
        }
        #[cfg(target_os = "windows")]
        {
            unimplemented!()
        }
    };

    let mut linker = std::process::Command::new("ld");
    let proc = linker.args(args.iter()).spawn()?;
    proc.wait_with_output()?;
    Ok(())
}

#[instrument(level = "debug")]
pub fn link_binary(input_path: &Path, output_filename: &Path) -> Result<(), CodegenError> {
    let args: &[&str] = {
        #[cfg(target_os = "macos")]
        {
            &[
                "-L/usr/local/lib",
                "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
                &input_path.display().to_string(),
                "-o",
                &output_filename.display().to_string(),
                "-lSystem",
            ]
        }
        #[cfg(target_os = "linux")]
        {
            let (scrt1, crti, crtn) = {
                if file_exists("/usr/lib64/Scrt1.o") {
                    (
                        "/usr/lib64/Scrt1.o",
                        "/usr/lib64/crti.o",
                        "/usr/lib64/crtn.o",
                    )
                } else {
                    (
                        "/lib/x86_64-linux-gnu/Scrt1.o",
                        "/lib/x86_64-linux-gnu/crti.o",
                        "/lib/x86_64-linux-gnu/crtn.o",
                    )
                }
            };

            &[
                "-pie",
                "--hash-style=gnu",
                "--eh-frame-hdr",
                "--dynamic-linker",
                "/lib64/ld-linux-x86-64.so.2",
                "-m",
                "elf_x86_64",
                scrt1,
                crti,
                "-o",
                &output_filename.display().to_string(),
                "-L/lib64",
                "-L/usr/lib64",
                "-L/lib/x86_64-linux-gnu",
                "-zrelro",
                "--no-as-needed",
                "-lc",
                "-O1",
                crtn,
                &input_path.display().to_string(),
            ]
        }
        #[cfg(target_os = "windows")]
        {
            unimplemented!()
        }
    };

    let mut linker = std::process::Command::new("ld");
    let proc = linker.args(args.iter()).spawn()?;
    proc.wait_with_output()?;
    Ok(())
}

#[cfg(target_os = "linux")]
fn file_exists(path: &str) -> bool {
    Path::new(path).exists()
}
