use crate::common::{compile_program, run_program};

mod common;

#[test]
fn borrow() {
    let program = compile_program(
        include_str!("../../../examples/borrow.con"),
        "borrow",
        false,
    )
    .unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 2);
}

#[test]
fn factorial_if() {
    let program = compile_program(
        include_str!("../../../examples/factorial_if.con"),
        "factorial_if",
        false,
    )
    .unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 24);
}

#[test]
fn fib_if() {
    let program = compile_program(
        include_str!("../../../examples/fib_if.con"),
        "fib_if",
        false,
    )
    .unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 55);
}

#[test]
fn import() {
    let program = compile_program(
        include_str!("../../../examples/import.con"),
        "import",
        false,
    )
    .unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 12);
}

#[test]
fn simple() {
    let program = compile_program(
        include_str!("../../../examples/simple.con"),
        "simple",
        false,
    )
    .unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 8);
}

#[test]
fn r#while() {
    let program =
        compile_program(include_str!("../../../examples/while.con"), "while", false).unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(result.status.code().unwrap(), 16);
}
