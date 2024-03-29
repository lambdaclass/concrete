use concrete_ir::lowering::{errors::LoweringError, lower_program};
use concrete_parser::{error::Diagnostics, ProgramSource};

#[test]
fn module_not_found() {
    let (source, name) = (include_str!("invalid_programs/import1.con"), "import1");
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::ModuleNotFound { span: _, module } if module == "Other"
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
    let (source, name) = (include_str!("invalid_programs/refmut.con"), "refmut");
    let error = check_invalid_program(source, name);

    assert!(
        matches!(
            &error,
            LoweringError::BorrowNotMutable { name, .. } if name == "a"
        ),
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
    let db = concrete_driver::db::Database::default();
    let source = ProgramSource::new(&db, source.to_string(), name.to_string());

    let program = match concrete_parser::parse_ast(&db, source) {
        Some(x) => x,
        None => {
            Diagnostics::dump(
                &db,
                source,
                &concrete_parser::parse_ast::accumulated::<concrete_parser::error::Diagnostics>(
                    &db, source,
                ),
            );
            panic!("error parsing ast");
        }
    };

    lower_program(&program).expect_err("expected error")
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
