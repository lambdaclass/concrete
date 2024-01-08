use std::{error::Error, path::PathBuf, time::Instant};

use clap::Parser;
use concrete_codegen_mlir::linker::{link_binary, link_shared_lib};
use concrete_session::{
    config::{DebugInfo, OptLevel},
    Session,
};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct CompilerArgs {
    /// The input file.
    input: PathBuf,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    release: bool,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    library: bool,
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let start_time = Instant::now();

    tracing_subscriber::fmt::init();

    let args = CompilerArgs::parse();

    let source = std::fs::read_to_string(args.input.clone())?;
    tracing::debug!("source code:\n{}", source);
    let program = concrete_parser::parse_ast(&source);

    let cwd = std::env::current_dir()?;
    // todo: find a better name, "target" would clash with rust if running in the source tree.
    let target_dir = cwd.join("build_artifacts/");
    let output_file = target_dir.join(PathBuf::from(args.input.file_name().unwrap()));
    tracing::debug!("Output file: {:?}", output_file);

    let session = Session {
        file_path: args.input,
        debug_info: if args.release {
            DebugInfo::None
        } else {
            DebugInfo::Full
        },
        optlevel: if args.release {
            OptLevel::Aggressive
        } else {
            OptLevel::None
        },
        source,
        library: args.library,
        target_dir,
        output_file,
    };
    tracing::debug!("Compiling with session: {:#?}", session);

    let object_path = concrete_codegen_mlir::compile(&session, &program)?;

    if session.library {
        link_shared_lib(&object_path, &session.output_file.with_extension("so"))?;
    } else {
        link_binary(&object_path, &session.output_file.with_extension(""))?;
    }

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(())
}
