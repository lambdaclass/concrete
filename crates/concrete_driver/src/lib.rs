use ariadne::Source;
use clap::Parser;
use concrete_codegen_mlir::linker::{link_binary, link_shared_lib};
use concrete_ir::lowering::lower_program;
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

    /// Set the optimization level, 0,1,2,3
    #[arg(short = 'O', long)]
    optlevel: Option<u8>,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    library: bool,

    /// Prints the ast.
    #[arg(long, default_value_t = false)]
    print_ast: bool,

    /// Prints the middle ir.
    #[arg(long, default_value_t = false)]
    print_ir: bool,

    /// Also output the llvm ir file.
    #[arg(long, default_value_t = false)]
    output_ll: bool,

    /// Also output the mlir file
    #[arg(long, default_value_t = false)]
    output_mlir: bool,

    /// Also output the asm file.
    #[arg(long, default_value_t = false)]
    output_asm: bool,

    /// Print all file formats.
    #[arg(long, default_value_t = false)]
    output_all: bool,
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
    let parse_ast_time = Instant::now();
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
    let parse_ast_time = parse_ast_time.elapsed();

    if args.print_ast {
        println!("{:#?}", program);
    }

    let cwd = std::env::current_dir()?;
    // todo: find a better name, "target" would clash with rust if running in the source tree.
    let target_dir = cwd.join("build_artifacts/");
    if !target_dir.exists() {
        std::fs::create_dir_all(&target_dir)?;
    }
    let output_file = target_dir.join(PathBuf::from(args.input.file_name().unwrap()));
    let output_file = if args.library {
        output_file.with_extension(Session::get_platform_library_ext())
    } else if cfg!(target_os = "windows") {
        output_file.with_extension("exe")
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
        optlevel: if let Some(optlevel) = args.optlevel {
            match optlevel {
                0 => OptLevel::None,
                1 => OptLevel::Less,
                2 => OptLevel::Default,
                _ => OptLevel::Aggressive,
            }
        } else if args.release {
            OptLevel::Aggressive
        } else {
            OptLevel::None
        },
        source: Source::from(source.input(&db).to_string()),
        library: args.library,
        target_dir,
        output_file,
        output_mlir: args.output_mlir,
        output_ll: args.output_ll,
        output_asm: args.output_asm,
        output_all: args.output_all,
    };

    tracing::debug!("Compiling with session: {:#?}", session);

    let lower_time = Instant::now();
    let program_ir = match lower_program(&program) {
        Ok(ir) => ir,
        Err(error) => {
            let report = concrete_check::lowering_error_to_report(error, &session);
            let path = session.file_path.display().to_string();
            report.eprint((path, session.source.clone()))?;
            std::process::exit(1);
        }
    };
    let lower_time = lower_time.elapsed();

    if args.print_ir {
        println!("{:#?}", program_ir);
    }

    let compile_time = Instant::now();
    let object_path = concrete_codegen_mlir::compile(&session, &program_ir)?;
    let compile_time = compile_time.elapsed();

    let link_time = Instant::now();
    if session.library {
        link_shared_lib(&object_path, &session.output_file)?;
    } else {
        link_binary(&object_path, &session.output_file)?;
    }
    let link_time = link_time.elapsed();

    let elapsed = start_time.elapsed();
    tracing::debug!("Parse time {:?}", parse_ast_time);
    tracing::debug!("Lower time {:?}", lower_time);
    tracing::debug!("Combined compile time {:?}", compile_time);
    tracing::debug!("Link time {:?}", link_time);
    tracing::debug!("Done in {:?}", elapsed);

    Ok(())
}
