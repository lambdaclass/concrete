use crate::ir::lowering::LoweringError;
use ariadne::{ColorGenerator, Label, Report, ReportKind};
use std::ops::Range;

// pub mod linearity_check;

#[derive(Debug, Clone)]
pub struct FileSpan {
    pub span: Range<usize>,
    pub path: String,
}

impl FileSpan {
    pub fn new(path: String, span: Range<usize>) -> Self {
        Self { path, span }
    }
}

impl ariadne::Span for FileSpan {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.path
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

/// Creates a report from a lowering error.
pub fn lowering_error_to_report(error: LoweringError) -> Report<'static, FileSpan> {
    let mut colors = ColorGenerator::new();
    colors.next();
    match error {
        LoweringError::TraitNotFound { span, name, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("TraitNotFound")
                .with_label(
                    Label::new(filespan)
                        .with_message(format!("Trait {name:?} not found."))
                        .with_color(colors.next()),
                )
                .with_message("Unresolved trait.")
                .finish()
        }
        LoweringError::ModuleNotFound { span, module, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("ModuleNotFound")
                .with_label(
                    Label::new(filespan)
                        .with_message(format!("Module {module:?} not found."))
                        .with_color(colors.next()),
                )
                .with_message("Unresolved import.")
                .finish()
        }
        LoweringError::FunctionNotFound {
            span,
            function,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("FunctionNotFound")
                .with_label(
                    Label::new(filespan)
                        .with_message(format!("Function {function:?} not found."))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::FieldNotFound { span, name, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("FieldNotFound")
                .with_label(
                    Label::new(filespan)
                        .with_message(format!("field {name:?} not found."))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::ImportNotFound {
            import_span,
            module_span,
            symbol,
            path,
        } => {
            let path = path.display().to_string();
            let span = symbol.span;
            let filespan = FileSpan::new(path.clone(), span.into());
            let module_span = FileSpan::new(path.clone(), module_span.into());
            let import_span = FileSpan::new(path.clone(), import_span.into());
            let symbol_span = FileSpan::new(path, symbol.span.into());
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("ImportNotFound")
                .with_label(Label::new(module_span.clone()).with_message("In module this module."))
                .with_label(Label::new(import_span).with_message("In this import statement"))
                .with_label(
                    Label::new(symbol_span)
                        .with_message(format!("Failed to find symbol {:?}", symbol.name))
                        .with_color(colors.next()),
                )
                .with_message("Unresolved import.")
                .finish()
        }
        LoweringError::BorrowNotMutable {
            span,
            name,
            type_span,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.into());
            let mut labels = vec![
                Label::new(filespan.clone())
                    .with_message(format!(
                        "Can't mutate {name:?} because it's behind a immutable borrow"
                    ))
                    .with_color(colors.next()),
            ];

            if let Some(type_span) = type_span {
                labels.push(
                    Label::new(FileSpan::new(path.clone(), type_span.into()))
                        .with_message(format!("Variable {name:?} has this type"))
                        .with_color(colors.next()),
                );
            }

            Report::build(ReportKind::Error, filespan)
                .with_code("BorrowNotMutable")
                .with_labels(labels)
                .finish()
        }
        LoweringError::UnrecognizedType { span, name, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("UnrecognizedType")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("Failed to find type {:?}", name))
                        .with_color(colors.next()),
                )
                .with_message(format!("Unresolved type {:?}.", name))
                .finish()
        }
        LoweringError::NotYetImplemented {
            span,
            message,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("NotYetImplemented")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(message)
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::UnexpectedType {
            found_span: span,
            found,
            expected,
            expected_span,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let mut labels = vec![
                Label::new(filespan.clone())
                    .with_message(format!(
                        "Unexpected type '{}', expected '{}'",
                        found, expected
                    ))
                    .with_color(colors.next()),
            ];

            if let Some(span) = expected_span {
                labels.push(
                    Label::new(FileSpan::new(path.clone(), span.into()))
                        .with_message(format!("expected '{}' due to this expression", expected))
                        .with_color(colors.next()),
                );
            }

            Report::build(ReportKind::Error, filespan.clone())
                .with_code("UnexpectedType")
                .with_labels(labels)
                .with_message(format!("expected type {}", expected))
                .finish()
        }
        LoweringError::InvalidUnaryOp {
            found_span: span,
            found,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let labels = vec![
                Label::new(filespan.clone())
                    .with_message(format!("Invalid binary operation type '{}'", found))
                    .with_color(colors.next()),
            ];

            Report::build(ReportKind::Error, filespan.clone())
                .with_code("InvalidUnaryOp")
                .with_labels(labels)
                .with_message(format!("invalid binary operation type {}", found))
                .finish()
        }
        LoweringError::UseOfUndeclaredVariable { span, name, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("UseOfUndeclaredVariable")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("Use of undeclared variable {:?}", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::ExternFnWithBody { span, name, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("ExternFnWithBody")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("extern function {:?} declared with body", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::CallParamCountMismatch {
            span,
            found,
            needs,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("CallParamCountMismatch")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!(
                            "function call parameter count mismatch: found {}, needs {}.",
                            found, needs
                        ))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::GenericCountMismatch {
            span,
            found,
            needs,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path, span.from..span.to);
            Report::build(ReportKind::Error, filespan.clone())
                .with_code("GenericCountMismatch")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!(
                            "function call generic parameter count mismatch: found {}, needs {}.",
                            found, needs
                        ))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::NotMutable {
            span,
            declare_span,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let mut report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("NotMutable")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message("can't mutate this variable because it's not mutable")
                        .with_color(colors.next()),
                );

            if let Some(declare_span) = declare_span {
                let declare_span = FileSpan::new(path, declare_span.into());
                report = report.with_label(
                    Label::new(declare_span)
                        .with_message("variable declared here")
                        .with_color(colors.next()),
                );
            }
            report.with_help("Add the 'mut' keyword to the variable declaration to make the variable mutable.").finish()
        }
        LoweringError::CantTakeMutableBorrow {
            span,
            declare_span,
            path,
        } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let mut report = Report::build(ReportKind::Error, filespan.clone())
                        .with_code("CantTakeMutableBorrow")
                        .with_label(
                            Label::new(filespan.clone())
                                .with_message("can't take a mutate borrow to this variable because it's not declared mutable")
                                .with_color(colors.next()),
                        );

            if let Some(declare_span) = declare_span {
                let declare_span = FileSpan::new(path, declare_span.into());
                report = report.with_label(
                    Label::new(declare_span)
                        .with_message("variable declared here")
                        .with_color(colors.next()),
                );
            }
            report.finish()
        }
        LoweringError::UnknownLangItem { span, item, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("UnknownLangItem")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("unknown lang item '{}'", item))
                        .with_color(colors.next()),
                );
            report.finish()
        }
        LoweringError::InvalidMatch { span, reason, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("InvalidMatch")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("invalid match: '{}'", reason))
                        .with_color(colors.next()),
                );
            report.finish()
        }
        LoweringError::Unimplemented { span, reason, path } => {
            let path = path.display().to_string();
            let filespan = FileSpan::new(path.clone(), span.from..span.to);
            let report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("Unimplemented")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(format!("unimplemented: '{}'", reason))
                        .with_color(colors.next()),
                );
            report.finish()
        }
        LoweringError::MissingVariant(error) => {
            let path = error.path.display().to_string();
            let filespan = FileSpan::new(path.clone(), error.match_span.from..error.match_span.to);
            let variant_filespan = FileSpan::new(
                error.type_path.display().to_string(),
                error.variant_span.from..error.variant_span.to,
            );
            let ty_filespan = FileSpan::new(
                error.type_path.display().to_string(),
                error.type_span.from..error.type_span.to,
            );
            let report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("MissingVariant")
                .with_label(
                    Label::new(filespan.clone())
                        .with_order(0)
                        .with_message(format!(
                            "missing variant: '{}' for match with type '{}'",
                            error.variant_name, error.type_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(variant_filespan)
                        .with_order(1)
                        .with_message("With the missing variant defined here")
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(ty_filespan)
                        .with_order(2)
                        .with_message("For the type defined here")
                        .with_color(colors.next()),
                )
                .with_help("Add the missing variant to fix the issue.");
            report.finish()
        }
        LoweringError::MissingTraitType(error) => {
            let path = error.path.display().to_string();
            let trait_path = error.trait_path.display().to_string();
            let type_name_span = FileSpan::new(path.clone(), error.type_name_span.into());
            let assoc_type_name_span =
                FileSpan::new(trait_path.clone(), error.assoc_type_name_span_def.into());
            let assoc_trait_span = FileSpan::new(trait_path.clone(), error.trait_span.into());
            let impl_trait_span = FileSpan::new(path.clone(), error.impl_trait_span.into());
            let report = Report::build(ReportKind::Error, impl_trait_span.clone())
                .with_code("MissingTraitType")
                .with_label(
                    Label::new(impl_trait_span.clone())
                        .with_message(format!(
                            "in the trait implementation {:?}",
                            error.trait_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(type_name_span.clone())
                        .with_message(format!("for type {}", error.type_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_trait_span.clone())
                        .with_message(format!("for the trait {} defined here", error.trait_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_type_name_span.clone())
                        .with_message(format!(
                            "missing the following associated type: {}",
                            error.assoc_type_name
                        ))
                        .with_color(colors.next()),
                )
                .with_message(format!(
                    "missing trait type {:?} in implementation for type {:?}",
                    error.assoc_type_name, error.type_name
                ))
                .with_help("Add the associated type to the trait implementation");
            report.finish()
        }
        LoweringError::UnexpectedTraitType(error) => {
            let path = error.path.display().to_string();
            let trait_path = error.trait_path.display().to_string();
            let type_name_span = FileSpan::new(path.clone(), error.type_name_span.into());
            let assoc_type_name_span =
                FileSpan::new(trait_path.clone(), error.assoc_type_name_span_def.into());
            let assoc_trait_span = FileSpan::new(trait_path.clone(), error.trait_span.into());
            let impl_trait_span = FileSpan::new(path.clone(), error.impl_trait_span.into());
            let report = Report::build(ReportKind::Error, impl_trait_span.clone())
                .with_code("UnexpectedTraitType")
                .with_label(
                    Label::new(impl_trait_span.clone())
                        .with_message(format!(
                            "in the trait implementation {:?}",
                            error.trait_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(type_name_span.clone())
                        .with_message(format!("for type {}", error.type_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_trait_span.clone())
                        .with_message(format!("for the trait {} defined here", error.trait_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_type_name_span.clone())
                        .with_message(format!(
                            "unexpected associated type: {}",
                            error.assoc_type_name
                        ))
                        .with_color(colors.next()),
                )
                .with_message(format!(
                    "unexpected trait type {:?} in implementation for type {:?}",
                    error.assoc_type_name, error.type_name
                ))
                .with_help("Remove the unexpected associated type from the trait implementation");
            report.finish()
        }
        LoweringError::MissingTraitFunction(error) => {
            let path = error.path.display().to_string();
            let trait_path = error.trait_path.display().to_string();
            let type_name_span = FileSpan::new(path.clone(), error.type_name_span.into());
            let assoc_type_name_span =
                FileSpan::new(trait_path.clone(), error.func_name_span_def.into());
            let assoc_trait_span = FileSpan::new(trait_path.clone(), error.trait_span.into());
            let impl_trait_span = FileSpan::new(path.clone(), error.impl_trait_span.into());
            let report = Report::build(ReportKind::Error, impl_trait_span.clone())
                .with_code("MissingTraitFunction")
                .with_label(
                    Label::new(impl_trait_span.clone())
                        .with_message(format!(
                            "in the trait implementation {:?}",
                            error.trait_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(type_name_span.clone())
                        .with_message(format!("for type {}", error.type_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_trait_span.clone())
                        .with_message(format!("for the trait {} defined here", error.trait_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_type_name_span.clone())
                        .with_message(format!(
                            "missing the following function: {}",
                            error.func_name
                        ))
                        .with_color(colors.next()),
                )
                .with_message(format!(
                    "missing trait function {:?} in implementation for type {:?}",
                    error.func_name, error.type_name
                ))
                .with_help("Implement the missing function in the trait implementation");
            report.finish()
        }
        LoweringError::UnexpectedTraitFunction(error) => {
            let path = error.path.display().to_string();
            let trait_path = error.trait_path.display().to_string();
            let type_name_span = FileSpan::new(path.clone(), error.type_name_span.into());
            let assoc_type_name_span =
                FileSpan::new(trait_path.clone(), error.func_name_span_def.into());
            let assoc_trait_span = FileSpan::new(trait_path.clone(), error.trait_span.into());
            let impl_trait_span = FileSpan::new(path.clone(), error.impl_trait_span.into());
            let report = Report::build(ReportKind::Error, impl_trait_span.clone())
                .with_code("UnexpectedTraitFunction")
                .with_label(
                    Label::new(impl_trait_span.clone())
                        .with_message(format!(
                            "in the trait implementation {:?}",
                            error.trait_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(type_name_span.clone())
                        .with_message(format!("for type {}", error.type_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_trait_span.clone())
                        .with_message(format!("for the trait {} defined here", error.trait_name))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(assoc_type_name_span.clone())
                        .with_message(format!("unexpected function: {}", error.func_name))
                        .with_color(colors.next()),
                )
                .with_message(format!(
                    "unexpected function {:?} in implementation for type {:?}",
                    error.func_name, error.type_name
                ))
                .with_help("Remove the unexpected function from the trait implementation");
            report.finish()
        }
        LoweringError::CantInferType(error) => {
            let path = error.path.display().to_string();
            let filespan = FileSpan::new(path.clone(), error.span.into());
            let report = Report::build(ReportKind::Error, filespan.clone())
                .with_code("CantInferType")
                .with_label(
                    Label::new(filespan.clone())
                        .with_message(error.message)
                        .with_color(colors.next()),
                )
                .with_help("Specify the generic type at the function call.");
            report.finish()
        }
        LoweringError::TraitBoundNotMet(error) => {
            let path = error.path.display().to_string();
            let func_span = FileSpan::new(path.clone(), error.func_name_span.into());
            let param_span = FileSpan::new(path.clone(), error.param_span.into());
            let trait_span = FileSpan::new(path.clone(), error.trait_span.into());
            let report = Report::build(ReportKind::Error, func_span.clone())
                .with_code("TraitBoundNotMet")
                .with_label(
                    Label::new(func_span.clone())
                        .with_message(format!(
                            "in the following function call {:?}",
                            error.func_name
                        ))
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(param_span.clone())
                        .with_message("this parameter type doesn't implement the required trait bounds")
                        .with_color(colors.next()),
                )
                .with_label(
                    Label::new(trait_span.clone())
                        .with_message("This trait bound is not implemented for the given parameter type at the function call")
                        .with_color(colors.next()),
                )
                .with_help(format!("Implement the trait {:?} for the passed parameter type.", error.trait_name));
            report.finish()
        }
    }
}
