use std::{
    borrow::Cow,
    fmt,
    path::{Path, PathBuf},
    process::Output,
};

use ariadne::Source;
use concrete_codegen_mlir::linker::{link_binary, link_shared_lib};
use concrete_ir::lowering::lower_program;
use concrete_parser::{error::Diagnostics, ProgramSource};
use concrete_session::{
    config::{DebugInfo, OptLevel},
    Session,
};
use tempfile::TempDir;

#[derive(Debug, Clone)]
struct TestError(Cow<'static, str>);

impl fmt::Display for TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for TestError {}

#[derive(Debug)]
pub struct CompileResult {
    pub folder: TempDir,
    pub object_file: PathBuf,
    pub binary_file: PathBuf,
}

pub fn compile_program(
    source: &str,
    name: &str,
    library: bool,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let db = concrete_driver::db::Database::default();
    let source = ProgramSource::new(&db, source.to_string(), name.to_string());
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
            return Err(Box::new(TestError("error compiling".into())));
        }
    };

    let test_dir = tempfile::tempdir()?;
    let test_dir_path = test_dir.path();
    // todo: find a better name, "target" would clash with rust if running in the source tree.
    let target_dir = test_dir_path.join("build_artifacts/");
    if !target_dir.exists() {
        std::fs::create_dir_all(&target_dir)?;
    }
    let output_file = target_dir.join(PathBuf::from(name));
    let output_file = if library {
        output_file.with_extension(Session::get_platform_library_ext())
    } else if cfg!(target_os = "windows") {
        output_file.with_extension("exe")
    } else {
        output_file.with_extension("")
    };

    let session = Session {
        file_path: PathBuf::from(name),
        debug_info: DebugInfo::Full,
        optlevel: OptLevel::None,
        source: Source::from(source.input(&db).to_string()),
        library,
        target_dir,
        output_file,
        output_mlir: false,
        output_ll: false,
        output_asm: false,
        output_all: false,
    };

    let program_ir = lower_program(&program);

    let object_path = concrete_codegen_mlir::compile(&session, &program_ir)?;

    if library {
        link_shared_lib(
            &object_path,
            &session
                .output_file
                .with_extension(Session::get_platform_library_ext()),
        )?;
    } else {
        link_binary(&object_path, &session.output_file.with_extension(""))?;
    }

    Ok(CompileResult {
        folder: test_dir,
        object_file: object_path,
        binary_file: session.output_file,
    })
}

pub fn run_program(program: &Path) -> Result<Output, std::io::Error> {
    std::process::Command::new(program)
        .spawn()?
        .wait_with_output()
}
