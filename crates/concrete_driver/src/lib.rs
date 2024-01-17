use ariadne::Source;
use clap::Parser;
use concrete_codegen_mlir::linker::{link_binary, link_shared_lib};
use concrete_parser::{error::Diagnostics, ProgramSource};
use concrete_session::{
    config::{DebugInfo, OptLevel},
    Session,
};
use std::{error::Error, path::PathBuf, time::Instant};

pub mod db;

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

    /// Prints the ast.
    #[arg(long, default_value_t = false)]
    print_ast: bool,
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let start_time = Instant::now();

    tracing_subscriber::fmt::init();

    let args = CompilerArgs::parse();

    let db = crate::db::Database::default();
    let source = ProgramSource::new(
        &db,
        std::fs::read_to_string(&args.input)?,
        args.input.display().to_string(),
    );
    tracing::debug!("source code:\n{}", source.input(&db));
    let program = match concrete_parser::parse_ast(&db, source) {
        Some(x) => x,
        None => {
            Diagnostics::dump(
                &db,
                source,
                &concrete_parser::parse_ast::accumulated::<concrete_parser::error::Diagnostics>(
                    &db, source,
                ),
            );
            std::process::exit(1);
        }
    };

    if args.print_ast {
        println!("{:#?}", program);
    }

    let cwd = std::env::current_dir()?;
    // todo: find a better name, "target" would clash with rust if running in the source tree.
    let target_dir = cwd.join("build_artifacts/");
    let output_file = target_dir.join(PathBuf::from(args.input.file_name().unwrap()));
    let output_file = if args.library {
        output_file.with_extension("so")
    } else {
        output_file.with_extension("")
    };

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
        source: Source::from(source.input(&db).to_string()),
        library: args.library,
        target_dir,
        output_file,
    };
    tracing::debug!("Compiling with session: {:#?}", session);

    let object_path = concrete_codegen_mlir::compile(&session, &program)?;

    if session.library {
        link_shared_lib(
            &object_path,
            &session
                .output_file
                .with_extension(Session::get_platform_library_ext()),
        )?;
    } else {
        link_binary(&object_path, &session.output_file.with_extension(""))?;
    }

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(())
}
