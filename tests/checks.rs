use std::path::Path;

use concrete::ir::lowering::{lower_compile_units, LoweringError};
use concrete::parser::ProgramSource;

pub fn check_invalid_program(source: &str, path: &str) -> LoweringError {
    let db = concrete::driver::db::DatabaseImpl::default();
    let source = ProgramSource::new(&db, source.to_string(), Path::new(path));

    let program = match concrete::parser::parse_ast(&db, source) {
        Some(x) => x,
        None => {
            concrete::parser::parse_ast::accumulated::<concrete::parser::error::Diagnostics>(
                &db, source,
            );
            panic!("error parsing ast");
        }
    };

    lower_compile_units(&[program]).expect_err("expected error")
}

#[test]
fn module_not_found() {
    let (source, name) = (
        include_str!("invalid_programs/import1.con"),
        "invalid_programs/import1.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::ModuleNotFound { span: _, module, .. } if module == "Other"
        ),
        "{:#?}",
        error
    );
}

#[test]
fn import_not_found() {
    let (source, name) = (
        include_str!("invalid_programs/import2.con"),
        "invalid_programs/import2.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::ImportNotFound { symbol, .. } if symbol.name == "world"
        ),
        "{:#?}",
        error
    );
}

#[test]
fn invalid_borrow_mut() {
    let (source, name) = (
        include_str!("invalid_programs/invalid_borrow_mut.con"),
        "invalid_programs/invalid_borrow_mut.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::NotMutable { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn unrecorgnized_type() {
    let (source, name) = (
        include_str!("invalid_programs/type.con"),
        "invalid_programs/type.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::UnrecognizedType { name, .. } if name == "invalid_type"
        ),
        "{:#?}",
        error
    );
}

#[test]
fn undeclared_var() {
    let (source, name) = (
        include_str!("invalid_programs/undeclared_var.con"),
        "invalid_programs/undeclared_var.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::UseOfUndeclaredVariable { name, .. } if name == "b"
        ),
        "{:#?}",
        error
    );
}

#[test]
fn call_param_count_mismatch() {
    let (source, name) = (
        include_str!("invalid_programs/call_param_count_mismatch.con"),
        "invalid_programs/call_param_count_mismatch.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::CallParamCountMismatch { found, needs, .. } if *found == 2 && *needs == 1
        ),
        "{:#?}",
        error
    );
}

#[test]
fn call_param_type_mismatch() {
    let (source, name) = (
        include_str!("invalid_programs/call_param_type_mismatch.con"),
        "invalid_programs/call_param_type_mismatch.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::UnexpectedType { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn invalid_assign() {
    let (source, name) = (
        include_str!("invalid_programs/invalid_assign.con"),
        "invalid_programs/invalid_assign.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::UnexpectedType { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn immutable_mutation() {
    let (source, name) = (
        include_str!("invalid_programs/immutable_mutation.con"),
        "invalid_programs/immutable_mutation.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::NotMutable { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn mutable_nonmut_borrow() {
    let (source, name) = (
        include_str!("invalid_programs/mutable_nonmut_borrow.con"),
        "invalid_programs/mutable_nonmut_borrow.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::CantTakeMutableBorrow { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn use_undeclared_var() {
    let (source, name) = (
        include_str!("invalid_programs/use_undeclared_var.con"),
        "invalid_programs/use_undeclared_var.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::UseOfUndeclaredVariable { .. }),
        "{:#?}",
        error
    );
}

#[test]
fn undef_var_in_for() {
    let (source, name) = (
        include_str!("invalid_programs/undef_var_in_for.con"),
        "invalid_programs/undef_var_in_for.con",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::UseOfUndeclaredVariable { .. }),
        "{:#?}",
        error
    );
}
