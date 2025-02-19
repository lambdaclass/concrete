use std::path::PathBuf;

use concrete::ir::lowering::{lower_compile_units, LoweringError};
use concrete::parser::ProgramSource;

#[test]
fn module_not_found() {
    let (source, name) = (include_str!("invalid_programs/import1.con"), "import1");
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
    let (source, name) = (include_str!("invalid_programs/import2.con"), "import2");
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
        "invalid_borrow_mut",
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
    let (source, name) = (include_str!("invalid_programs/type.con"), "type");
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

pub fn check_invalid_program(source: &str, name: &str) -> LoweringError {
    let db = concrete::driver::db::DatabaseImpl::default();
    let source = ProgramSource::new(&db, source.to_string(), name.to_string());

    let mut program = match concrete::parser::parse_ast(&db, source, source.path(&db)) {
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
fn undeclared_var() {
    let (source, name) = (
        include_str!("invalid_programs/undeclared_var.con"),
        "undeclared_var",
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
        "call_param_count_mismatch",
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
        "call_param_type_mismatch",
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
        "invalid_assign",
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
        "immutable_mutation",
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
        "mutable_nonmut_borrow",
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
        "use_undeclared_var",
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
        "undef_var_in_for",
    );
    let error = check_invalid_program(source, name);

    assert!(
        matches!(&error, LoweringError::UseOfUndeclaredVariable { .. }),
        "{:#?}",
        error
    );
}
