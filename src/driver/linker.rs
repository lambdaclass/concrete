use std::path::{Path, PathBuf};

use crate::compile_unit_info::CompileUnitInfo;
use tracing::instrument;

#[instrument(level = "debug")]
pub fn link_shared_lib(objects: &[PathBuf], output_filename: &Path) -> std::io::Result<()> {
    let mut output_filename = output_filename.to_path_buf();
    let objects: Vec<_> = objects.iter().map(|x| x.display().to_string()).collect();

    if output_filename.extension().is_none() {
        output_filename =
            output_filename.with_extension(CompileUnitInfo::get_platform_library_ext());
    }
    let output_filename = output_filename.to_string_lossy().to_string();

    let args: Vec<_> = {
        #[cfg(target_os = "macos")]
        {
            let mut args = vec![
                "-dynamic",
                "-dylib",
                "-L/usr/local/lib",
                "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
            ];

            args.extend(objects.iter().map(|x| x.as_str()));
            args.extend(&["-o", &output_filename, "-lSystem"]);

            args
        }
        #[cfg(target_os = "linux")]
        {
            let mut args = vec!["--hash-style=gnu", "--eh-frame-hdr", "-shared"];

            args.extend(&["-o", &output_filename]);

            args.extend(&["-L/lib/../lib64", "-L/usr/lib/../lib64", "-lc", "-O1"]);

            args.extend(objects.iter().map(|x| x.as_str()));

            args
        }
        #[cfg(target_os = "windows")]
        {
            unimplemented!()
        }
    };

    let mut linker = std::process::Command::new("ld");
    let proc = linker.args(args.iter()).spawn()?;
    let output = proc.wait_with_output()?;
    tracing::debug!("Linker result ok: {}", output.status.success());

    if !output.status.success() {
        tracing::error!(
            "Linker error:\n{}",
            String::from_utf8_lossy(&output.stderr).to_string()
        );
    }
    Ok(())
}

#[instrument(level = "debug")]
pub fn link_binary(objects: &[PathBuf], output_filename: &Path) -> std::io::Result<()> {
    let objects: Vec<_> = objects.iter().map(|x| x.display().to_string()).collect();
    let output_filename = output_filename.to_string_lossy().to_string();

    let args: Vec<_> = {
        #[cfg(target_os = "macos")]
        {
            let mut args = vec![
                "-L/usr/local/lib",
                "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
            ];

            args.extend(objects.iter().map(|x| x.as_str()));

            args.extend(&["-o", &output_filename, "-lSystem"]);

            args
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

            let mut args = vec![
                "-pie",
                "--hash-style=gnu",
                "--eh-frame-hdr",
                "--dynamic-linker",
                "/lib64/ld-linux-x86-64.so.2",
                "-m",
                "elf_x86_64",
                scrt1,
                crti,
            ];

            args.extend(&["-o", &output_filename]);

            args.extend(&[
                "-L/lib64",
                "-L/usr/lib64",
                "-L/lib/x86_64-linux-gnu",
                "-zrelro",
                "--no-as-needed",
                "-lc",
                "-O1",
                crtn,
            ]);

            args.extend(objects.iter().map(|x| x.as_str()));

            args
        }
        #[cfg(target_os = "windows")]
        {
            unimplemented!()
        }
    };

    let mut linker = std::process::Command::new("ld");
    let proc = linker.args(args.iter()).spawn()?;
    let output = proc.wait_with_output()?;
    tracing::debug!("Linker result ok: {}", output.status.success());

    if !output.status.success() {
        tracing::error!(
            "Linker error:\n{}",
            String::from_utf8_lossy(&output.stderr).to_string()
        );
    }
    Ok(())
}

#[cfg(target_os = "linux")]
fn file_exists(path: &str) -> bool {
    Path::new(path).exists()
}
