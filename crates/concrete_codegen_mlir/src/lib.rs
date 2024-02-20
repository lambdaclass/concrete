#![allow(clippy::too_many_arguments)]

use std::{
    ffi::{CStr, CString},
    mem::MaybeUninit,
    path::PathBuf,
    ptr::{addr_of_mut, null_mut},
    sync::OnceLock,
    time::Instant,
};

use concrete_ir::ProgramBody;
use concrete_session::{config::OptLevel, Session};
use context::Context;
use errors::CodegenError;
use llvm_sys::{
    core::{
        LLVMContextCreate, LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule,
        LLVMPrintModuleToFile,
    },
    error::LLVMGetErrorMessage,
    prelude::{LLVMContextRef, LLVMModuleRef},
    target::{
        LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs,
        LLVM_InitializeAllTargets,
    },
    target_machine::{
        LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
        LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple, LLVMGetHostCPUFeatures,
        LLVMGetHostCPUName, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile,
        LLVMTargetRef,
    },
    transforms::pass_builder::{
        LLVMCreatePassBuilderOptions, LLVMDisposePassBuilderOptions, LLVMRunPasses,
    },
};
use module::MLIRModule;

mod codegen;
mod context;
mod error;
pub mod errors;
pub mod linker;
mod module;
mod pass_manager;

/// Compiles the given program and returns the object file path.
pub fn compile(session: &Session, program: &ProgramBody) -> Result<PathBuf, CodegenError> {
    static INITIALIZED: OnceLock<()> = OnceLock::new();
    INITIALIZED.get_or_init(|| unsafe {
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmPrinters();
        tracing::debug!("initialized llvm targets");
    });

    let context = Context::new();
    let compile_codegen_time = Instant::now();
    let mlir_module = context.compile(session, program)?;
    let compile_codegen_time = compile_codegen_time.elapsed();
    assert!(mlir_module.melior_module.as_operation().verify());

    let compile_llvm_time = Instant::now();
    let object_path = compile_to_object(session, &mlir_module)?;
    let compile_llvm_time = compile_llvm_time.elapsed();
    tracing::debug!("Codegen time {:?}", compile_codegen_time);
    tracing::debug!("Compile llvm time {:?}", compile_llvm_time);

    Ok(object_path)
}

extern "C" {
    /// Translate operation that satisfies LLVM dialect module requirements into an LLVM IR module living in the given context.
    /// This translates operations from any dilalect that has a registered implementation of LLVMTranslationDialectInterface.
    fn mlirTranslateModuleToLLVMIR(
        module_operation_ptr: mlir_sys::MlirOperation,
        llvm_context: LLVMContextRef,
    ) -> LLVMModuleRef;
}

pub fn get_target_triple(_session: &Session) -> String {
    // TODO: use session to get the specified target triple
    let target_triple = unsafe {
        let value = LLVMGetDefaultTargetTriple();
        CStr::from_ptr(value).to_string_lossy().into_owned()
    };
    target_triple
}

pub fn get_data_layout_rep(session: &Session) -> Result<String, CodegenError> {
    unsafe {
        let mut null = null_mut();
        let error_buffer = addr_of_mut!(null);

        let target_triple = LLVMGetDefaultTargetTriple();
        tracing::debug!("Target triple: {:?}", CStr::from_ptr(target_triple));

        let target_cpu = LLVMGetHostCPUName();
        tracing::debug!("Target CPU: {:?}", CStr::from_ptr(target_cpu));

        let target_cpu_features = LLVMGetHostCPUFeatures();
        tracing::debug!(
            "Target CPU Features: {:?}",
            CStr::from_ptr(target_cpu_features)
        );

        let mut target: MaybeUninit<LLVMTargetRef> = MaybeUninit::uninit();

        if LLVMGetTargetFromTriple(target_triple, target.as_mut_ptr(), error_buffer) != 0 {
            let error = CStr::from_ptr(*error_buffer);
            let err = error.to_string_lossy().to_string();
            tracing::error!("error getting target triple: {}", err);
            LLVMDisposeMessage(*error_buffer);
            Err(CodegenError::LLVMCompileError(err))?;
        }
        if !(*error_buffer).is_null() {
            LLVMDisposeMessage(*error_buffer);
        }

        let target = target.assume_init();

        let machine = LLVMCreateTargetMachine(
            target,
            target_triple.cast(),
            target_cpu.cast(),
            target_cpu_features.cast(),
            match session.optlevel {
                OptLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                OptLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
                OptLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                OptLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
            },
            if session.library {
                LLVMRelocMode::LLVMRelocDynamicNoPic
            } else {
                LLVMRelocMode::LLVMRelocDefault
            },
            LLVMCodeModel::LLVMCodeModelDefault,
        );

        let data_layout = llvm_sys::target_machine::LLVMCreateTargetDataLayout(machine);
        let data_layout_str =
            CStr::from_ptr(llvm_sys::target::LLVMCopyStringRepOfTargetData(data_layout));
        Ok(data_layout_str.to_string_lossy().into_owned())
    }
}

/// Converts a module to an object.
/// The object will be written to the specified target path.
/// TODO: error handling
///
/// Returns the path to the object.
pub fn compile_to_object(
    session: &Session,
    module: &MLIRModule<'_>,
) -> Result<PathBuf, CodegenError> {
    tracing::debug!("Compiling to object file");
    if !session.target_dir.exists() {
        std::fs::create_dir_all(&session.target_dir)?;
    }

    let target_file = session
        .file_path
        .clone()
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_string();
    let target_file = PathBuf::from(target_file).with_extension("o");
    tracing::debug!("Target file: {:?}", target_file);

    let target_path = session.target_dir.join(target_file);

    // TODO: Rework so you can specify target and host features, etc.
    // Right now it compiles for the native cpu feature set and arch

    unsafe {
        let llvm_context = LLVMContextCreate();

        let op = module.melior_module.as_operation().to_raw();

        let llvm_module = mlirTranslateModuleToLLVMIR(op, llvm_context);

        let mut null = null_mut();
        let mut error_buffer = addr_of_mut!(null);

        let target_triple = LLVMGetDefaultTargetTriple();
        tracing::debug!("Target triple: {:?}", CStr::from_ptr(target_triple));

        let target_cpu = LLVMGetHostCPUName();
        tracing::debug!("Target CPU: {:?}", CStr::from_ptr(target_cpu));

        let target_cpu_features = LLVMGetHostCPUFeatures();
        tracing::debug!(
            "Target CPU Features: {:?}",
            CStr::from_ptr(target_cpu_features)
        );

        let mut target: MaybeUninit<LLVMTargetRef> = MaybeUninit::uninit();

        if LLVMGetTargetFromTriple(target_triple, target.as_mut_ptr(), error_buffer) != 0 {
            let error = CStr::from_ptr(*error_buffer);
            let err = error.to_string_lossy().to_string();
            tracing::error!("error getting target triple: {}", err);
            LLVMDisposeMessage(*error_buffer);
            Err(CodegenError::LLVMCompileError(err))?;
        } else if !(*error_buffer).is_null() {
            LLVMDisposeMessage(*error_buffer);
            error_buffer = addr_of_mut!(null);
        }

        let target = target.assume_init();

        let machine = LLVMCreateTargetMachine(
            target,
            target_triple.cast(),
            target_cpu.cast(),
            target_cpu_features.cast(),
            match session.optlevel {
                OptLevel::None => LLVMCodeGenOptLevel::LLVMCodeGenLevelNone,
                OptLevel::Less => LLVMCodeGenOptLevel::LLVMCodeGenLevelLess,
                OptLevel::Default => LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
                OptLevel::Aggressive => LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
            },
            if session.library {
                LLVMRelocMode::LLVMRelocDynamicNoPic
            } else {
                LLVMRelocMode::LLVMRelocDefault
            },
            LLVMCodeModel::LLVMCodeModelDefault,
        );

        let opts = LLVMCreatePassBuilderOptions();
        let opt = match session.optlevel {
            OptLevel::None => 0,
            OptLevel::Less => 1,
            OptLevel::Default => 2,
            OptLevel::Aggressive => 3,
        };
        let passes = CString::new(format!("default<O{opt}>")).unwrap();
        let error = LLVMRunPasses(llvm_module, passes.as_ptr(), machine, opts);
        if !error.is_null() {
            let msg = LLVMGetErrorMessage(error);
            let msg = CStr::from_ptr(msg);
            Err(CodegenError::LLVMCompileError(
                msg.to_string_lossy().into_owned(),
            ))?;
        }

        LLVMDisposePassBuilderOptions(opts);

        if session.output_ll || session.output_all {
            let filename = CString::new(
                target_path
                    .with_extension("ll")
                    .as_os_str()
                    .to_string_lossy()
                    .as_bytes(),
            )
            .unwrap();
            if LLVMPrintModuleToFile(llvm_module, filename.as_ptr(), error_buffer) != 0 {
                let error = CStr::from_ptr(*error_buffer);
                let err = error.to_string_lossy().to_string();
                tracing::error!("error outputing ll file: {}", err);
                LLVMDisposeMessage(*error_buffer);
                Err(CodegenError::LLVMCompileError(err))?;
            } else if !(*error_buffer).is_null() {
                LLVMDisposeMessage(*error_buffer);
                error_buffer = addr_of_mut!(null);
            }
        }

        let filename = CString::new(target_path.as_os_str().to_string_lossy().as_bytes()).unwrap();
        tracing::debug!("filename to llvm: {:?}", filename);
        let ok = LLVMTargetMachineEmitToFile(
            machine,
            llvm_module,
            filename.as_ptr().cast_mut(),
            LLVMCodeGenFileType::LLVMObjectFile, // object (binary) or assembly (textual)
            error_buffer,
        );

        if ok != 0 {
            let error = CStr::from_ptr(*error_buffer);
            let err = error.to_string_lossy().to_string();
            tracing::error!("error emitting to file: {:?}", err);
            LLVMDisposeMessage(*error_buffer);
            Err(CodegenError::LLVMCompileError(err))?;
        } else if !(*error_buffer).is_null() {
            LLVMDisposeMessage(*error_buffer);
        }

        if session.output_asm || session.output_all {
            let filename = CString::new(
                target_path
                    .with_extension("asm")
                    .as_os_str()
                    .to_string_lossy()
                    .as_bytes(),
            )
            .unwrap();
            let ok = LLVMTargetMachineEmitToFile(
                machine,
                llvm_module,
                filename.as_ptr().cast_mut(),
                LLVMCodeGenFileType::LLVMAssemblyFile, // object (binary) or assembly (textual)
                error_buffer,
            );

            if ok != 0 {
                let error = CStr::from_ptr(*error_buffer);
                let err = error.to_string_lossy().to_string();
                tracing::error!("error emitting asm to file: {:?}", err);
                LLVMDisposeMessage(*error_buffer);
                Err(CodegenError::LLVMCompileError(err))?;
            } else if !(*error_buffer).is_null() {
                LLVMDisposeMessage(*error_buffer);
            }
        }

        LLVMDisposeTargetMachine(machine);
        LLVMDisposeModule(llvm_module);
        LLVMContextDispose(llvm_context);

        Ok(target_path)
    }
}
