use std::{
    borrow::Cow,
    fmt,
    path::{Path, PathBuf},
    process::{Output, Stdio},
};

use ariadne::Source;
use concrete::driver::linker::{link_binary, link_shared_lib};
use concrete::ir::lowering::lower_programs;
use concrete::parser::ProgramSource;
use concrete::session::{DebugInfo, OptLevel, Session};
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
    #[allow(unused)]
    pub folder: TempDir,
    #[allow(unused)]
    pub object_file: PathBuf,
    pub binary_file: PathBuf,
}

pub fn compile_program(
    source: &str,
    name: &str,
    library: bool,
    optlevel: OptLevel,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let db = concrete::driver::db::DatabaseImpl::default();
    let source = ProgramSource::new(&db, source.to_string(), name.to_string());
    tracing::debug!("source code:\n{}", source.input(&db));
    let mut program = match concrete::parser::parse_ast(&db, source) {
        Some(x) => x,
        None => {
            concrete::parser::parse_ast::accumulated::<concrete::parser::error::Diagnostics>(
                &db, source,
            );
            return Err(Box::new(TestError("error compiling".into())));
        }
    };

    let test_dir = tempfile::tempdir()?;
    let test_dir_path = test_dir.path();

    let input_file = test_dir_path.join(name).with_extension(".con");
    std::fs::write(&input_file, source.input(&db))?;
    program.file_path = Some(input_file.clone());

    let output_file = test_dir_path.join(name);
    let output_file = if library {
        output_file.with_extension(Session::get_platform_library_ext())
    } else if cfg!(target_os = "windows") {
        output_file.with_extension("exe")
    } else {
        output_file.with_extension("")
    };

    let session = Session {
        debug_info: DebugInfo::Full,
        optlevel,
        sources: vec![Source::from(source.input(&db).to_string())],
        library,
        output_file,
        output_mlir: false,
        output_ll: false,
        output_asm: false,
        file_paths: vec![input_file],
    };

    let program_ir = lower_programs(&[program])?;

    let object_path = concrete::codegen::compile(&session, &program_ir)?;

    if library {
        link_shared_lib(
            &[object_path.clone()],
            &session
                .output_file
                .with_extension(Session::get_platform_library_ext()),
        )?;
    } else {
        link_binary(
            &[object_path.clone()],
            &session.output_file.with_extension(""),
        )?;
    }

    Ok(CompileResult {
        folder: test_dir,
        object_file: object_path,
        binary_file: session.output_file,
    })
}

pub fn run_program(program: &Path) -> Result<Output, std::io::Error> {
    std::process::Command::new(program)
        .stdout(Stdio::piped())
        .spawn()?
        .wait_with_output()
}

#[track_caller]
pub fn compile_and_run(source: &str, name: &str, library: bool, optlevel: OptLevel) -> i32 {
    let result = compile_program(source, name, library, optlevel).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");

    output.status.code().unwrap()
}

#[allow(unused)] // false positive
#[track_caller]
pub fn compile_and_run_output(
    source: &str,
    name: &str,
    library: bool,
    optlevel: OptLevel,
) -> String {
    let result = compile_program(source, name, library, optlevel).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");

    std::str::from_utf8(&output.stdout).unwrap().to_string()
}
