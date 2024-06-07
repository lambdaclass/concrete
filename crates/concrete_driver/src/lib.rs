use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use ariadne::Source;
use clap::Args;
use clap::{Parser, Subcommand};
use concrete_ast::Program;
use concrete_ir::lowering::lower_programs;
use concrete_parser::{error::Diagnostics, ProgramSource};
use concrete_session::{
    config::{DebugInfo, OptLevel},
    Session,
};
use config::{Package, Profile};
use db::Database;
use git2::{IndexAddOption, Repository};
use owo_colors::OwoColorize;
use std::io::Read;
use std::os::unix::process::CommandExt;
use std::{collections::HashMap, fs::File, path::PathBuf, time::Instant};

use crate::{
    config::Config,
    linker::{link_binary, link_shared_lib},
};

pub mod config;
pub mod db;
pub mod linker;

#[derive(Parser, Debug)]
#[command(author, version, about = "The Concrete Programming Language", long_about = None, bin_name = "concrete")]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Initialize a project
    New {
        path: PathBuf,

        /// The name of the project, defaults to the directory name
        #[arg(long)]
        name: Option<String>,

        /// Use a binary (application) template [default]
        #[arg(long, group = "binary", default_value_t = true)]
        bin: bool,

        /// Use a library template
        #[arg(long, group = "binary")]
        lib: bool,
    },
    /// Build a project or file
    Build(BuildArgs),
    /// Run a project or file
    Run(BuildArgs),
}

#[derive(Args, Debug)]
pub struct BuildArgs {
    /// Build specific file
    #[arg(required = false)]
    path: Option<PathBuf>,

    /// Build the specific file as a library, only used when compiling a single file.
    #[arg(short, long, required = false, default_value_t = false)]
    lib: bool,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    release: bool,

    /// Override the profile to use.
    #[arg(short, long)]
    profile: Option<String>,

    /// Also output the ast.
    #[arg(long, default_value_t = false)]
    ast: bool,

    /// Also output the ir.
    #[arg(long, default_value_t = false)]
    ir: bool,

    /// Also output the llvm ir file.
    #[arg(long, default_value_t = false)]
    llvm: bool,

    /// Also output the mlir file
    #[arg(long, default_value_t = false)]
    mlir: bool,

    /// Also output the asm file.
    #[arg(long, default_value_t = false)]
    asm: bool,

    /// Also output the object file.
    #[arg(long, default_value_t = false)]
    object: bool,

    /// This option is for checking the program for linearity.
    #[arg(long, default_value_t = false)]
    check: bool,
}

#[derive(Parser, Debug)]
#[command(author, version, about = "concrete compiler", long_about = None)]
pub struct CompilerArgs {
    /// The input file.
    input: PathBuf,

    /// The output file.
    pub output: PathBuf,

    /// Build for release with all optimizations.
    #[arg(short, long, default_value_t = false)]
    release: bool,

    /// Set the optimization level, 0,1,2,3
    #[arg(short = 'O', long)]
    optlevel: Option<u8>,

    /// Always add debug info
    #[arg(long)]
    pub debug_info: Option<bool>,

    /// Build as a library.
    #[arg(short, long, default_value_t = false)]
    library: bool,

    /// Also output the ast.
    #[arg(long, default_value_t = false)]
    ast: bool,

    /// Also output the ir.
    #[arg(long, default_value_t = false)]
    ir: bool,

    /// Also output the llvm ir file.
    #[arg(long, default_value_t = false)]
    llvm: bool,

    /// Also output the mlir file
    #[arg(long, default_value_t = false)]
    mlir: bool,

    /// Also output the asm file.
    #[arg(long, default_value_t = false)]
    asm: bool,

    /// Also output the object file.
    #[arg(long, default_value_t = false)]
    object: bool,

    /// This option is for checking the program for linearity.
    #[arg(long, default_value_t = false)]
    check: bool,
}

pub fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::New {
            path,
            name,
            bin,
            lib,
        } => {
            let name = name.unwrap_or_else(|| {
                path.file_name()
                    .context("failed to get project name")
                    .unwrap()
                    .to_string_lossy()
                    .to_string()
            });

            if !path.exists() {
                std::fs::create_dir_all(&path).context("failed to create the project directory")?;
                std::fs::create_dir_all(path.join("src")).context("failed to create src/")?;
            }

            let config_path = path.join("Concrete.toml");

            let mut profiles = HashMap::new();

            profiles.insert(
                "release".to_string(),
                Profile {
                    release: true,
                    opt_level: 3,
                    debug_info: false,
                },
            );

            profiles.insert(
                "dev".to_string(),
                Profile {
                    release: false,
                    opt_level: 0,
                    debug_info: true,
                },
            );

            let config = Config {
                package: Package {
                    name: name.clone(),
                    version: "0.1.0".to_string(),
                    license: "MIT".to_string(),
                },
                profile: profiles,
            };

            std::fs::write(config_path, toml::to_string_pretty(&config)?)
                .context("failed to write Concrete.toml")?;
            std::fs::write(path.join(".gitignore"), "/build\n")
                .context("failed to write .gitignore")?;
            std::fs::write(
                path.join(".gitattributes"),
                "*.con linguist-language=Rust\n",
            )
            .context("failed to write .gitattributes")?;

            if bin {
                std::fs::write(
                    path.join("src").join("main.con"),
                    format!(
                        r#"
mod {} {{
    pub fn main() -> i32 {{
        return 0;
    }}
}}"#,
                        name
                    ),
                )?;
            }

            if lib {
                std::fs::write(
                    path.join("src").join("lib.con"),
                    format!(
                        r#"
mod {} {{
    pub fn hello_world() -> i32 {{
        return 0;
    }}
}}"#,
                        name
                    ),
                )?;
            }

            {
                let repo = Repository::init(&path).context("failed to create repository")?;
                let sig = repo.signature()?;
                let tree_id = {
                    let mut index = repo.index()?;

                    index.add_all(["."].iter(), IndexAddOption::DEFAULT, None)?;
                    index.write()?;
                    index.write_tree()?
                };

                let tree = repo.find_tree(tree_id).context("failed to find git tree")?;
                repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
                    .context("failed to create initial commit")?;
            }

            if bin {
                println!(
                    "  {} binary (application) `{}` package",
                    "Created".green().bold(),
                    name
                );
            } else {
                println!("  {} library `{}` package", "Created".green(), name);
            }
        }
        Commands::Build(args) => {
            handle_build(args)?;
        }
        Commands::Run(args) => {
            let output = handle_build(args)?;
            Err(std::process::Command::new(output).exec())?;
        }
    }

    Ok(())
}

fn handle_build(
    BuildArgs {
        path,
        release,
        profile,
        ast,
        ir,
        llvm,
        mlir,
        asm,
        object,
        lib,
        check,
    }: BuildArgs,
) -> Result<PathBuf> {
    match path {
        // Single file compilation
        Some(input) => {
            let input_stem = input
                .file_stem()
                .context("could not get file stem")?
                .to_str()
                .context("could not convert file stem to string")?;

            let build_dir = std::env::current_dir()?;
            let output = build_dir.join(input_stem);

            let compile_args = CompilerArgs {
                input: input.clone(),
                output: output.clone(),
                release,
                optlevel: None,
                debug_info: None,
                library: lib,
                ast,
                ir,
                llvm,
                asm,
                object,
                mlir,
                check,
            };

            println!(
                "   {} {} ({})",
                "Compiling".green().bold(),
                input_stem,
                input.display()
            );

            let start = Instant::now();
            let object = compile(&compile_args)?;

            if lib {
                link_shared_lib(&[object.clone()], &output)?;
            } else {
                link_binary(&[object.clone()], &output)?;
            }

            if !compile_args.object {
                std::fs::remove_file(object)?;
            }

            let elapsed = start.elapsed();

            println!(
                "   {} {} in {elapsed:?}",
                "Finished".green().bold(),
                if release { "release" } else { "dev" },
            );

            Ok(output)
        }
        // Project compilation.
        None => {
            let mut current_dir = std::env::current_dir()?;
            let mut config_path = None;
            for _ in 0..3 {
                if !current_dir.join("Concrete.toml").exists() {
                    current_dir = if let Some(parent) = current_dir.parent() {
                        parent.to_path_buf()
                    } else {
                        bail!("couldn't find Concrete.toml");
                    };
                } else {
                    config_path = Some(current_dir.join("Concrete.toml"));
                    break;
                }
            }
            let config_path = match config_path {
                Some(x) => x,
                None => bail!("couldn't find Concrete.toml"),
            };
            let base_dir = config_path
                .parent()
                .context("couldn't get config parent dir")?;
            let mut config = File::open(&config_path).context("failed to open Concrete.toml")?;
            let mut buf = String::new();
            config.read_to_string(&mut buf)?;
            let config: Config = toml::from_str(&buf).context("failed to parse Concrete.toml")?;
            println!(
                "   {} {} v{} ({})",
                "Compiling".green().bold(),
                config.package.name,
                config.package.version,
                base_dir.display()
            );
            let src_dir = base_dir.join("src");
            let target_dir = base_dir.join("build");
            if !target_dir.exists() {
                std::fs::create_dir_all(&target_dir)?;
            }
            let output = target_dir.join(config.package.name);
            let (profile, profile_name) = if let Some(profile) = profile {
                (
                    config
                        .profile
                        .get(&profile)
                        .context("couldn't get requested profile")?,
                    profile,
                )
            } else if release {
                (
                    config
                        .profile
                        .get("release")
                        .context("couldn't get profile: release")?,
                    "release".to_string(),
                )
            } else {
                (
                    config
                        .profile
                        .get("dev")
                        .context("couldn't get profile: dev")?,
                    "dev".to_string(),
                )
            };

            let lib_ed = src_dir.join("lib.con");
            let main_ed = src_dir.join("main.con");

            let start = Instant::now();

            for file in [main_ed, lib_ed] {
                if file.exists() {
                    let is_lib = file.file_stem().unwrap() == "lib";

                    let compile_args = CompilerArgs {
                        input: file,
                        output: if is_lib {
                            let name = output.file_stem().unwrap().to_string_lossy().to_string();
                            let name = format!("lib{name}");
                            output
                                .with_file_name(name)
                                .with_extension(Session::get_platform_library_ext())
                        } else {
                            output.clone()
                        },
                        release,
                        optlevel: Some(profile.opt_level),
                        debug_info: Some(profile.debug_info),
                        library: is_lib,
                        ast,
                        ir,
                        llvm,
                        asm,
                        object,
                        mlir,
                        check,
                    };
                    let object = compile(&compile_args)?;

                    if compile_args.library {
                        link_shared_lib(&[object], &compile_args.output)?;
                    } else {
                        link_binary(&[object], &compile_args.output)?;
                    }
                }
            }
            let elapsed = start.elapsed();
            println!(
                "   {} {} [{}{}] in {elapsed:?}",
                "Finished".green().bold(),
                profile_name,
                if profile.opt_level > 0 {
                    "optimized"
                } else {
                    "unoptimized"
                },
                if profile.debug_info {
                    " + debuginfo"
                } else {
                    ""
                }
            );

            Ok(output)
        }
    }
}

pub fn parse_file(
    modules: &mut Vec<(PathBuf, String, Program)>,
    mut path: PathBuf,
    db: &Database,
) -> Result<()> {
    if path.is_dir() {
        path = path.join("mod.ed");
    }

    let real_source = std::fs::read_to_string(&path)?;
    let source = ProgramSource::new(db, real_source.clone(), path.display().to_string());

    let mut program = match concrete_parser::parse_ast(db, source) {
        Some(x) => x,
        None => {
            Diagnostics::dump(
                db,
                source,
                &concrete_parser::parse_ast::accumulated::<concrete_parser::error::Diagnostics>(
                    db, source,
                ),
            );
            std::process::exit(1);
        }
    };
    program.file_path = Some(path.clone());

    for ident in program.modules.iter().flat_map(|x| &x.external_modules) {
        let module_path = path
            .parent()
            .unwrap()
            .join(&ident.name)
            .with_extension("con");
        parse_file(modules, module_path, db)?;
    }

    modules.push((path, real_source, program));

    Ok(())
}

pub fn compile(args: &CompilerArgs) -> Result<PathBuf> {
    let start_time = Instant::now();

    let mut programs = Vec::new();
    let db = crate::db::Database::default();
    parse_file(&mut programs, args.input.clone(), &db)?;

    let session = Session {
        file_paths: programs.iter().map(|x| x.0.clone()).collect(),
        debug_info: if let Some(debug_info) = args.debug_info {
            if debug_info {
                DebugInfo::Full
            } else {
                DebugInfo::None
            }
        } else if args.release {
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
        sources: programs.iter().map(|x| Source::from(x.1.clone())).collect(),
        library: args.library,
        output_file: args.output.with_extension("o"),
        output_asm: args.asm,
        output_ll: args.llvm,
        output_mlir: args.mlir,
    };
    tracing::debug!("Output file: {:#?}", session.output_file);
    tracing::debug!("Is library: {:#?}", session.library);
    tracing::debug!("Optlevel: {:#?}", session.optlevel);
    tracing::debug!("Debug Info: {:#?}", session.debug_info);

    let path_cache: Vec<_> = programs
        .iter()
        .map(|x| (x.0.display().to_string(), x.1.clone()))
        .collect();

    if args.ast {
        std::fs::write(
            session.output_file.with_extension("ast"),
            format!("{:#?}", programs),
        )?;
    }

    let modules: Vec<_> = programs.iter().map(|x| x.2.clone()).collect();
    let program_ir = match lower_programs(&modules) {
        Ok(ir) => ir,
        Err(error) => {
            let report = concrete_check::lowering_error_to_report(error, &session);
            report.eprint(ariadne::sources(path_cache))?;
            std::process::exit(1);
        }
    };

    #[allow(unused_variables)]
    if args.check {
        let linearity_result =
            match concrete_check::linearity_check::linearity_check_program(&programs, &session) {
                Ok(ir) => ir,
                Err(error) => {
                    //TODO improve reporting
                    println!("Linearity check failed: {:#?}", error);
                    std::process::exit(1);
                }
            };
    }

    if args.ir {
        std::fs::write(
            session.output_file.with_extension("ir"),
            format!("{:#?}", program_ir),
        )?;
    }

    let object_path = concrete_codegen_mlir::compile(&session, &program_ir).unwrap();

    let elapsed = start_time.elapsed();
    tracing::debug!("Done in {:?}", elapsed);

    Ok(object_path)
}
