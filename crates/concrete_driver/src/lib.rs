use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use clap::{Parser, Subcommand};
use concrete_ir::lowering::lower_programs;
use concrete_parser::{error::Diagnostics, ProgramSource};
use concrete_session::{
    config::{DebugInfo, OptLevel},
    Session,
};
use config::{Package, Profile};
use git2::{IndexAddOption, Repository};
use owo_colors::OwoColorize;
use std::io::Read;
use std::os::unix::process::CommandExt;
use std::{collections::HashMap, fs::File, path::PathBuf, time::Instant};
use walkdir::WalkDir;

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
    /// Build a project
    Build {
        /// Build for release with all optimizations.
        #[arg(short, long, default_value_t = false)]
        release: bool,

        /// Override the profile to use.
        #[arg(short, long)]
        profile: Option<String>,
    },
    /// Run a concrete file
    Run {
        #[arg(required = false)]
        path: Option<PathBuf>,

        /// Build for release with all optimizations.
        #[arg(short, long, default_value_t = false)]
        release: bool,

        /// Override the profile to use.
        #[arg(short, long)]
        profile: Option<String>,
    },
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
                    .context("Failed to get project name")
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
        Commands::Build { release, profile } => {
            build_command(profile, release)?;
        }
        Commands::Run {
            path,
            release,
            profile,
        } => {
            let output = match path {
                Some(input) => {
                    let output_stem = input
                        .file_stem()
                        .context("could not get file stem")?
                        .to_str()
                        .context("could not convert file stem to string")?;

                    let build_dir = std::env::current_dir()?.join("build");
                    if !build_dir.exists() {
                        std::fs::create_dir_all(&build_dir)?;
                    }
                    let output = build_dir.join(output_stem);

                    let compile_args = CompilerArgs {
                        input,
                        output: output.clone(),
                        release,
                        optlevel: None,
                        debug_info: None,
                        library: false,
                        ast: false,
                        ir: false,
                        llvm: true,
                        asm: false,
                        object: true,
                        mlir: true,
                    };
                    let object = compile(&compile_args)?;

                    link_binary(&[object], &output)?;

                    output
                }
                None => build_command(profile, release)?,
            };

            Err(std::process::Command::new(output).exec())?;
        }
    }

    Ok(())
}

fn build_command(profile: Option<String>, release: bool) -> Result<PathBuf> {
    let mut current_dir = std::env::current_dir()?;
    let mut config_path = None;
    for _ in 0..3 {
        if !current_dir.join("Concrete.toml").exists() {
            current_dir = if let Some(parent) = current_dir.parent() {
                parent.to_path_buf()
            } else {
                bail!("Couldn't find Concrete.toml");
            };
        } else {
            config_path = Some(current_dir.join("Concrete.toml"));
            break;
        }
    }
    let config_path = match config_path {
        Some(x) => x,
        None => bail!("Couldn't find Concrete.toml"),
    };
    let base_dir = config_path
        .parent()
        .context("couldn't get config parent dir")?;
    let mut config = File::open(&config_path).context("Failed to open Concrete.toml")?;
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
    let has_main = src_dir.join("main.con").exists();
    let output = target_dir.join(config.package.name);
    let (profile, profile_name) = if let Some(profile) = profile {
        (
            config
                .profile
                .get(&profile)
                .context("Couldn't get requested profile")?,
            profile,
        )
    } else if release {
        (
            config
                .profile
                .get("release")
                .context("Couldn't get profile: release")?,
            "release".to_string(),
        )
    } else {
        (
            config
                .profile
                .get("dev")
                .context("Couldn't get profile: dev")?,
            "dev".to_string(),
        )
    };
    let compile_args = CompilerArgs {
        input: src_dir,
        output: output.clone(),
        release,
        optlevel: Some(profile.opt_level),
        debug_info: Some(profile.debug_info),
        library: !has_main,
        ast: false,
        ir: false,
        llvm: true,
        asm: false,
        object: true,
        mlir: true,
    };
    let start = Instant::now();
    let object = compile(&compile_args)?;
    if !has_main {
        link_shared_lib(&[object], &output)?;
    } else {
        link_binary(&[object], &output)?;
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

pub fn compile(args: &CompilerArgs) -> Result<PathBuf> {
    let mut files = Vec::new();
    for entry in WalkDir::new(&args.input) {
        let entry = entry?;
        if let Some(ext) = entry.path().extension() {
            if ext.eq_ignore_ascii_case("con") {
                files.push(entry.path().to_path_buf());
            }
        }
    }

    if files.is_empty() {
        panic!("files is empty");
    }

    let start_time = Instant::now();

    let mut programs = Vec::new();

    let db = crate::db::Database::default();

    let mut sources = Vec::new();
    let mut sources_str = Vec::new();

    for path in files {
        let source = std::fs::read_to_string(&path)?;
        let source = ProgramSource::new(&db, source, args.input.display().to_string());

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
                std::process::exit(1);
            }
        };
        program.file_path = Some(path);
        sources.push(ariadne::Source::from(source.input(&db).clone()));
        sources_str.push(source.input(&db).clone());
        programs.push(program);
    }

    let session = Session {
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
        sources,
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
        .zip(&sources_str)
        .map(|(program, source)| {
            (
                program
                    .file_path
                    .clone()
                    .expect("path should always exist here")
                    .to_string_lossy()
                    .to_string(),
                source.clone(),
            )
        })
        .collect();

    if args.ast {
        std::fs::write(
            session.output_file.with_extension("ast"),
            format!("{:#?}", programs),
        )?;
    }

    let program_ir = match lower_programs(&programs) {
        Ok(ir) => ir,
        Err(error) => {
            let file_paths: Vec<_> = programs
                .iter()
                .map(|x| x.file_path.clone().unwrap())
                .collect();
            let report = concrete_check::lowering_error_to_report(error, &file_paths);
            report.eprint(ariadne::sources(path_cache))?;
            std::process::exit(1);
        }
    };

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
