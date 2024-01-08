use std::path::Path;

use tracing::instrument;

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
                "--hash-style=gnu",
                "--eh-frame-hdr",
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
