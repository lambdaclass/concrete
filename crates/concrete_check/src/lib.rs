use std::{collections::HashMap, ops::Range};

use ariadne::{ColorGenerator, Label, Report, ReportKind};
use ast_helper::AstHelper;
use concrete_ast::{common::Ident, imports::ImportStmt, modules::Module, Program};
use concrete_session::Session;
use itertools::Itertools;
use thiserror::Error;

pub mod ast_helper;

#[derive(Error, Debug, Clone)]
pub enum CheckError<'p> {
    #[error("import not found {:?} in module {}", import, module.name.name)]
    ImportModuleMissing {
        module: &'p Module,
        import: &'p ImportStmt,
    },
    #[error("import symbol {:?} not found in module {}", symbol, module.name.name)]
    ImportSymbolMissing {
        module: &'p Module,
        import: &'p ImportStmt,
        symbol: &'p Ident,
    },
}

impl<'p> CheckError<'p> {
    pub fn to_report<'s>(&self, session: &'s Session) -> Report<'s, (String, Range<usize>)> {
        let path = session.file_path.display().to_string();
        let mut colors = ColorGenerator::new();
        colors.next();

        match self {
            CheckError::ImportModuleMissing { module, import } => {
                let contex_module_span = module.span;
                let span = {
                    let mut span = import
                        .module
                        .first()
                        .expect("module path can't be empty")
                        .span;
                    span.to = import
                        .module
                        .last()
                        .expect("module path can't be empty")
                        .span
                        .to;
                    span
                };
                let offset = import.module[0].span.from;
                Report::build(ReportKind::Error, path.clone(), offset)
                    .with_code("E1")
                    .with_label(
                        Label::new((path.clone(), contex_module_span.into()))
                            .with_message(format!("In module {:?}.", module.name.name)),
                    )
                    .with_label(
                        Label::new((path, span.into()))
                            .with_message(format!(
                                "Module {:?} not found.",
                                import.module.iter().map(|x| &x.name).join(".")
                            ))
                            .with_color(colors.next()),
                    )
                    .with_message("Failed to find import.")
                    .finish()
            }
            CheckError::ImportSymbolMissing {
                module,
                import,
                symbol,
            } => {
                let contex_module_span = module.span;
                let import_span = import.span;
                let offset = symbol.span.from;
                Report::build(ReportKind::Error, path.clone(), offset)
                    .with_code("E1")
                    .with_label(
                        Label::new((path.clone(), contex_module_span.into()))
                            .with_message(format!("In module {:?}.", module.name.name)),
                    )
                    .with_label(
                        Label::new((path.clone(), import_span.into()))
                            .with_message("In this import statement"),
                    )
                    .with_label(
                        Label::new((path, symbol.span.into()))
                            .with_message(format!("Failed to find symbol {:?}", symbol.name))
                            .with_color(colors.next()),
                    )
                    .with_message("Failed to find import.")
                    .finish()
            }
        }
    }
}

pub fn check_program<'p>(
    program: &'p Program,
    _session: &Session,
) -> Result<(), Vec<CheckError<'p>>> {
    let helper = AstHelper::new(program);

    let mut errors = Vec::new();

    for module in &program.modules {
        // let info = helper.modules.get(&module.name.name).unwrap();

        let mut imports = HashMap::new();

        // check modules
        for import in &module.imports {
            let target_module = helper.get_module_from_import(&import.module);

            if target_module.is_none() {
                errors.push(CheckError::ImportModuleMissing { module, import });
                continue;
            }

            let target_module = target_module.unwrap();

            // check if symbol exists
            for symbol in &import.symbols {
                let name = &symbol.name;
                let exists = target_module.functions.get(name).is_some()
                    || target_module.constants.get(name).is_some()
                    || target_module.structs.get(name).is_some()
                    || target_module.types.get(name).is_some();

                if !exists {
                    errors.push(CheckError::ImportSymbolMissing {
                        module,
                        import,
                        symbol,
                    });
                    continue;
                }

                imports.insert(symbol.name.clone(), target_module);
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
