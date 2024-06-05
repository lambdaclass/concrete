use std::{
    borrow::Cow,
    fmt,
    path::{Path, PathBuf},
    process::{Output, Stdio},
};

use ariadne::Source;
use concrete_ast::Program;
use concrete_driver::linker::{link_binary, link_shared_lib};
use concrete_driver::CompilerArgs;
use concrete_ir::lowering::lower_programs;
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
    optlevel: OptLevel,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let mut input_path = std::env::current_dir()?;
    input_path = input_path.join(source);
    let build_dir = std::env::current_dir()?;
    let output = build_dir.join(source);

    let compile_args = CompilerArgs {
        input: input_path.clone(),
        output: output.clone(),
        release: false,
        ast: false,
        optlevel: None,
        debug_info: None,
        library,
        ir: false,
        llvm: false,
        mlir: false,
        asm: false,
        object: false,
        check: false,
    };
    compile_program_with_args(source, name, library, optlevel, &compile_args)
}

pub fn compile_program_with_args(
    source: &str,
    name: &str,
    library: bool,
    optlevel: OptLevel,
    args: &CompilerArgs,
) -> Result<CompileResult, Box<dyn std::error::Error>> {
    let mut programs_for_check: Vec<(PathBuf, String, Program)> = Vec::new();

    let db = concrete_driver::db::Database::default();
    // Real source for programs_for_check
    let real_source = source.to_string();
    let source = ProgramSource::new(&db, source.to_string(), name.to_string());
    tracing::debug!("source code:\n{}", source.input(&db));
    let mut program = match concrete_parser::parse_ast(&db, source) {
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

    let input_file = test_dir_path.join(name).with_extension("con");

    //Build Vec for programs_for_check before moving program
    if args.check {
        programs_for_check.push((input_file.clone(), real_source, program.clone()));
    }

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

    // By now only used check for being able to run tests with linearity checking
    let mut linearity_errors = Vec::new();
    //#[allow(unused_variables)]
    if args.check {
        match concrete_check::linearity_check::linearity_check_program(
            &programs_for_check,
            &session,
        ) {
            Ok(ir) => ir,
            Err(error) => {
                //TODO improve reporting
                println!("Linearity check failed: {:#?}", error);
                linearity_errors.push(error);
            }
        };
    }

    let program_ir = lower_programs(&[program])?;

    let object_path = concrete_codegen_mlir::compile(&session, &program_ir)?;

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
    if !linearity_errors.is_empty() {
        let error = build_test_linearity_error(&linearity_errors[0]);
        Err(error)
    } else {
        Ok(CompileResult {
            folder: test_dir,
            object_file: object_path,
            binary_file: session.output_file,
        })
    }
}

pub fn build_test_linearity_error(
    linearity_error: &concrete_check::linearity_check::errors::LinearityError,
) -> Box<dyn std::error::Error> {
    let mut ret = "Linearity check failed<".to_string();
    ret.push_str(&linearity_error.to_string());
    ret.push('>');
    Box::new(TestError(ret.into()))
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
//#[cfg(test)] //TODO It should solve the warning but it doesn't
#[track_caller]
pub fn compile_and_run_with_args(
    source: &str,
    name: &str,
    library: bool,
    optlevel: OptLevel,
    args: &CompilerArgs,
) -> Result<Output, Box<dyn std::error::Error>> {
    let compile_result = compile_program_with_args(source, name, library, optlevel, args);
    match compile_result {
        //Err(e) => Err(std::error::Error::new(std::io::ErrorKind::Other, e.to_string())),
        Err(e) => Err(e),
        Ok(result) => {
            let run_output = run_program(&result.binary_file)?;
            Ok(run_output)
        }
    }
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
