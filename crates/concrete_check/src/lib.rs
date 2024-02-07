use std::{collections::HashMap, ops::Range};

use ariadne::{ColorGenerator, Label, Report, ReportKind};
use ast_helper::{AstHelper, ModuleInfo};
use concrete_ast::{
    common::Ident,
    constants::ConstantDef,
    functions::FunctionDef,
    imports::ImportStmt,
    modules::{Module, ModuleDefItem},
    statements::Statement,
    types::TypeSpec,
    Program,
};
use concrete_session::Session;
use itertools::Itertools;
use thiserror::Error;

pub mod ast_helper;

#[derive(Error, Debug, Clone)]
pub enum CheckError {
    #[error("import not found {:?} in module {}", import, module.name.name)]
    ImportModuleMissing { module: Module, import: ImportStmt },
    #[error("import symbol {:?} not found in module {}", symbol, module.name.name)]
    ImportSymbolMissing {
        module: Module,
        import: ImportStmt,
        symbol: Ident,
    },
    #[error("type not found {:?}", type_spec)]
    TypeNotFound { type_spec: TypeSpec },
}

impl CheckError {
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
                    .with_message("Unresolved import.")
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
                    .with_code("E2")
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
                    .with_message("Unresolved import.")
                    .finish()
            }
            CheckError::TypeNotFound { type_spec } => {
                let span = match type_spec {
                    TypeSpec::Simple { span, .. } => span,
                    TypeSpec::Generic { span, .. } => span,
                    TypeSpec::Array { span, .. } => span,
                };
                Report::build(ReportKind::Error, path.clone(), span.from)
                    .with_code("E3")
                    .with_label(
                        Label::new((path, (*span).into()))
                            .with_message(format!("Failed to find type {:?}", type_spec.get_name()))
                            .with_color(colors.next()),
                    )
                    .with_message(format!("Unresolved type {:?}.", type_spec.get_name()))
                    .finish()
            }
        }
    }
}

#[allow(unused)]
#[derive(Debug, Clone)]
struct ScopeContext<'parent> {
    pub locals: HashMap<String, LocalVar>,
    pub function: Option<FunctionDef>,
    pub imports: HashMap<String, &'parent ModuleInfo<'parent>>,
    pub module_info: &'parent ModuleInfo<'parent>,
}

#[allow(unused)]
#[derive(Debug, Clone)]
struct LocalVar {
    pub type_spec: TypeSpec,
}

#[allow(unused)]
impl<'parent> ScopeContext<'parent> {
    pub fn get_function(&self, local_name: &str) -> Option<&FunctionDef> {
        if let Some(module) = self.imports.get(local_name) {
            // a import
            module.functions.get(local_name).copied()
        } else {
            self.module_info.functions.get(local_name).copied()
        }
    }

    fn resolve_type(&self, name: &str) -> Option<String> {
        Some(match name {
            "u64" | "i64" => name.to_string(),
            "u32" | "i32" => name.to_string(),
            "u16" | "i16" => name.to_string(),
            "u8" | "i8" => name.to_string(),
            "f32" => name.to_string(),
            "f64" => name.to_string(),
            "bool" => name.to_string(),
            "char" => name.to_string(),
            "string" => name.to_string(),
            name => {
                if let Some(module) = self.imports.get(name) {
                    // a import
                    self.resolve_type_spec(&module.types.get(name)?.value)?
                } else {
                    self.resolve_type_spec(&self.module_info.types.get(name)?.value)?
                }
            }
        })
    }

    fn resolve_type_spec(&self, spec: &TypeSpec) -> Option<String> {
        match spec.is_ref() {
            Some(_) => Some(format!("&{}", self.resolve_type_spec_ref(spec)?)),
            None => self.resolve_type_spec_ref(spec),
        }
    }

    /// Resolves the type this ref points to.
    fn resolve_type_spec_ref(&self, spec: &TypeSpec) -> Option<String> {
        match spec {
            TypeSpec::Simple { name, .. } => self.resolve_type(&name.name),
            TypeSpec::Generic { name, .. } => self.resolve_type(&name.name),
            TypeSpec::Array { .. } => {
                todo!("implement arrays")
            }
        }
    }
}

pub fn check_program(program: &Program) -> Result<(), Vec<CheckError>> {
    let helper = AstHelper::new(program);

    let mut errors = Vec::new();

    for module in &program.modules {
        let module_info = helper.modules.get(&module.name.name).unwrap();
        if let Err(e) = check_module(module, &helper, module_info) {
            errors.extend(e);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn check_module<'p, 'x: 'p>(
    module: &Module,
    helper: &AstHelper<'p>,
    module_info: &ModuleInfo<'p>,
) -> Result<(), Vec<CheckError>> {
    let mut errors = Vec::new();

    let mut scope_ctx = ScopeContext {
        locals: HashMap::new(),
        function: None,
        imports: HashMap::new(),
        module_info,
    };

    // check modules
    for import in &module.imports {
        let target_module = helper.get_module_from_import(&import.module);

        if target_module.is_none() {
            errors.push(CheckError::ImportModuleMissing {
                module: module.clone(),
                import: import.clone(),
            });
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
                    module: module.clone(),
                    import: import.clone(),
                    symbol: symbol.clone(),
                });
                continue;
            }

            scope_ctx.imports.insert(symbol.name.clone(), target_module);
        }
    }

    for stmt in &module.contents {
        match stmt {
            ModuleDefItem::Constant(info) => {
                if let Err(e) = check_constant(info, &scope_ctx, helper, module_info) {
                    errors.extend(e);
                }
            }
            ModuleDefItem::Function(info) => {
                scope_ctx.function = Some(info.clone());
                if let Err(e) = check_function(info, &scope_ctx, helper, module_info) {
                    errors.extend(e);
                }
            }
            ModuleDefItem::Struct(_) => {}
            ModuleDefItem::Type(_) => {}
            ModuleDefItem::Module(info) => {
                let module_info = module_info.modules.get(&info.name.name).unwrap();
                if let Err(e) = check_module(info, helper, module_info) {
                    errors.extend(e);
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_function<'p>(
    info: &FunctionDef,
    scope_ctx: &ScopeContext<'p>,
    _helper: &AstHelper<'p>,
    _module_info: &ModuleInfo<'p>,
) -> Result<(), Vec<CheckError>> {
    let mut errors = Vec::new();

    for param in &info.decl.params {
        if scope_ctx.resolve_type_spec(&param.r#type).is_none() {
            errors.push(CheckError::TypeNotFound {
                type_spec: param.r#type.clone(),
            })
        }
    }

    if let Some(ret_type) = info.decl.ret_type.as_ref() {
        if scope_ctx.resolve_type_spec(ret_type).is_none() {
            errors.push(CheckError::TypeNotFound {
                type_spec: ret_type.clone(),
            })
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_constant<'p>(
    _info: &ConstantDef,
    _scope_ctx: &ScopeContext<'p>,
    _helper: &AstHelper<'p>,
    _module_info: &ModuleInfo<'p>,
) -> Result<(), Vec<CheckError>> {
    let errors = Vec::new();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn _check_statement<'p>(
    _info: &Statement,
    _scope_ctx: &mut ScopeContext<'p>,
    _helper: &AstHelper<'p>,
    _module_info: &ModuleInfo<'p>,
) -> Result<(), Vec<CheckError>> {
    let errors = Vec::new();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
