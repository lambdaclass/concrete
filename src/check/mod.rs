use crate::ir::lowering::LoweringError;
use ariadne::{ColorGenerator, Label, Report, ReportKind};
use std::ops::Range;

// pub mod linearity_check;

/// Creates a report from a lowering error.
pub fn lowering_error_to_report(error: LoweringError) -> Report<'static, (String, Range<usize>)> {
    let mut colors = ColorGenerator::new();
    colors.next();
    match error {
        LoweringError::ModuleNotFound { span, module, path } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("ModuleNotFound")
                .with_label(
                    Label::new((path, span.into()))
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
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("FunctionNotFound")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Function {function:?} not found."))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::StructFieldNotFound { span, name, path } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("StructFieldNotFound")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Struct field {name:?} not found."))
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
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("ImportNotFound")
                .with_label(
                    Label::new((path.clone(), module_span.into()))
                        .with_message("In module this module."),
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
        LoweringError::BorrowNotMutable {
            span,
            name,
            type_span,
            path,
        } => {
            let path = path.display().to_string();
            let mut labels = vec![Label::new((path.clone(), span.into()))
                .with_message(format!(
                    "Can't mutate {name:?} because it's behind a immutable borrow"
                ))
                .with_color(colors.next())];

            if let Some(type_span) = type_span {
                labels.push(
                    Label::new((path.clone(), type_span.into()))
                        .with_message(format!("Variable {name:?} has this type"))
                        .with_color(colors.next()),
                );
            }

            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("BorrowNotMutable")
                .with_labels(labels)
                .finish()
        }
        LoweringError::UnrecognizedType { span, name, path } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("UnrecognizedType")
                .with_label(
                    Label::new((path, span.into()))
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
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("NotYetImplemented")
                .with_label(
                    Label::new((path, span.into()))
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
            let mut labels = vec![Label::new((path.clone(), span.into()))
                .with_message(format!(
                    "Unexpected type '{}', expected '{}'",
                    found, expected
                ))
                .with_color(colors.next())];

            if let Some(span) = expected_span {
                labels.push(
                    Label::new((path.clone(), span.into()))
                        .with_message(format!("expected '{}' due to this expression", expected))
                        .with_color(colors.next()),
                );
            }

            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("UnexpectedType")
                .with_labels(labels)
                .with_message(format!("expected type {}", expected))
                .finish()
        }
        LoweringError::UseOfUndeclaredVariable { span, name, path } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("UseOfUndeclaredVariable")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Use of undeclared variable {:?}", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::ExternFnWithBody { span, name, path } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("ExternFnWithBody")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("extern function {:?} declared with body", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::InternalError(msg) => {
            Report::build(ReportKind::Error, ("".to_string(), 0..0))
                .with_code("InternalError")
                .with_message(msg)
                .finish()
        }
        LoweringError::CallParamCountMismatch {
            span,
            found,
            needs,
            path,
        } => {
            let path = path.display().to_string();
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("CallParamCountMismatch")
                .with_label(
                    Label::new((path, span.into()))
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
            Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("GenericCountMismatch")
                .with_label(
                    Label::new((path, span.into()))
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
            let mut report = Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("NotMutable")
                .with_label(
                    Label::new((path.clone(), span.into()))
                        .with_message("can't mutate this variable because it's not mutable")
                        .with_color(colors.next()),
                );

            if let Some(declare_span) = declare_span {
                report = report.with_label(
                    Label::new((path, declare_span.into()))
                        .with_message("variable declared here")
                        .with_color(colors.next()),
                );
            }
            report.finish()
        }
        LoweringError::CantTakeMutableBorrow {
            span,
            declare_span,
            path,
        } => {
            let path = path.display().to_string();
            let mut report = Report::build(ReportKind::Error, (path.clone(), span.from..span.to))
                .with_code("CantTakeMutableBorrow")
                .with_label(
                    Label::new((path.clone(), span.into()))
                        .with_message("can't take a mutate borrow to this variable because it's not declared mutable")
                        .with_color(colors.next()),
                );

            if let Some(declare_span) = declare_span {
                report = report.with_label(
                    Label::new((path, declare_span.into()))
                        .with_message("variable declared here")
                        .with_color(colors.next()),
                );
            }
            report.finish()
        }
    }
}
