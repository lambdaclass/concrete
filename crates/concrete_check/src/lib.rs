use std::ops::Range;

use ariadne::{ColorGenerator, Label, Report, ReportKind};
use concrete_ir::lowering::errors::LoweringError;
use concrete_session::Session;

/// Creates a report from a lowering error.
pub fn lowering_error_to_report(
    error: LoweringError,
    session: &Session,
) -> Report<'static, (String, Range<usize>)> {
    let mut colors = ColorGenerator::new();
    colors.next();
    match error {
        LoweringError::ModuleNotFound {
            span,
            module,
            program_id,
        } => {
            let offset = span.from;
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), offset)
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
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("FunctionNotFound")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Function {function:?} not found."))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::StructFieldNotFound {
            span,
            name,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
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
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            let offset = symbol.span.from;
            Report::build(ReportKind::Error, path.clone(), offset)
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
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
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

            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("BorrowNotMutable")
                .with_labels(labels)
                .finish()
        }
        LoweringError::UnrecognizedType {
            span,
            name,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("UnrecognizedType")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Failed to find type {:?}", name))
                        .with_color(colors.next()),
                )
                .with_message(format!("Unresolved type {:?}.", name))
                .finish()
        }
        LoweringError::IdNotFound {
            span,
            id,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("E_ID")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message("Failed to definition id")
                        .with_color(colors.next()),
                )
                .with_message(format!("Failed to find definition id {id:?}, this is most likely a compiler bug or a unimplemented lowering"))
                .finish()
        }
        LoweringError::NotYetImplemented {
            span,
            message,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("NotYetImplemented")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(message)
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::UnexpectedType {
            span,
            found,
            expected,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            let mut labels = vec![Label::new((path.clone(), span.into()))
                .with_message(format!(
                    "Unexpected type '{}', expected '{}'",
                    found.kind, expected.kind
                ))
                .with_color(colors.next())];

            if let Some(span) = expected.span {
                labels.push(
                    Label::new((path.clone(), span.into()))
                        .with_message(format!("expected '{}' due to this type", expected.kind))
                        .with_color(colors.next()),
                );
            }

            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("UnexpectedType")
                .with_labels(labels)
                .with_message(format!("expected type {}.", expected.kind))
                .finish()
        }
        LoweringError::UseOfUndeclaredVariable {
            span,
            name,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("UseOfUndeclaredVariable")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("Use of undeclared variable {:?}", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::ExternFnWithBody {
            span,
            name,
            program_id,
        } => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), span.from)
                .with_code("ExternFnWithBody")
                .with_label(
                    Label::new((path, span.into()))
                        .with_message(format!("extern function {:?} declared with body", name))
                        .with_color(colors.next()),
                )
                .finish()
        }
        LoweringError::InternalError(msg, program_id) => {
            let path = session.file_paths[program_id].to_str().unwrap().to_string();
            Report::build(ReportKind::Error, path.clone(), 0)
                .with_code("InternalError")
                .with_message(msg)
                .finish()
        }
    }
}
