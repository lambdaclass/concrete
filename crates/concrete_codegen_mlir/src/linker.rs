use std::path::Path;

use tracing::instrument;

// TODO: Implement a proper linker driver, passing only the arguments needed dynamically based on the requirements.

#[instrument(level = "debug")]
pub fn link_shared_lib(input_path: &Path, output_filename: &Path) -> Result<(), std::io::Error> {
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
pub fn link_binary(input_path: &Path, output_filename: &Path) -> Result<(), std::io::Error> {
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
            &[
                "-pie",
                "--hash-style=gnu",
                "--eh-frame-hdr",
                "--dynamic-linker",
                "/lib64/ld-linux-x86-64.so.2",
                "-m",
                "elf_x86_64",
                "/usr/lib64/Scrt1.o",
                "/usr/lib64/crti.o",
                "-o",
                &output_filename.display().to_string(),
                "-L/lib64",
                "-L/usr/lib64",
                "-zrelro",
                "--no-as-needed",
                "-lc",
                "/usr/lib64/crtn.o",
                &input_path.display().to_string(),
            ]
        }
        #[cfg(target_os = "windows")]
        {
            unimplemented!()
        }
    };

    let mut linker = std::process::Command::new("ld.bfd");
    let proc = linker.args(args.iter()).spawn()?;
    proc.wait_with_output()?;
    Ok(())
}
