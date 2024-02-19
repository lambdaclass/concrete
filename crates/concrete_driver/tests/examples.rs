use crate::common::{compile_program, run_program};
use test_case::test_case;

mod common;

#[test_case(include_str!("../../../examples/borrow.con"), "borrow", false, 2 ; "borrow.con")]
#[test_case(include_str!("../../../examples/factorial_if.con"), "factorial_if", false, 24 ; "factorial_if.con")]
#[test_case(include_str!("../../../examples/fib_if.con"), "fib_if", false, 55 ; "fib_if.con")]
#[test_case(include_str!("../../../examples/import.con"), "import", false, 12 ; "import.con")]
#[test_case(include_str!("../../../examples/simple.con"), "simple", false, 8 ; "simple.con")]
#[test_case(include_str!("../../../examples/while.con"), "while", false, 16 ; "while.con")]
#[test_case(include_str!("../../../examples/chars.con"), "chars", false, 117 ; "chars.con")]
#[test_case(include_str!("../../../examples/floats.con"), "floats", false, 1 ; "floats.con")]
#[test_case(include_str!("../../../examples/refs.con"), "refs", false, 6 ; "refs.con")]
fn example_tests(source: &str, name: &str, is_library: bool, status_code: i32) {
    let program = compile_program(source, name, is_library).unwrap();

    let result = run_program(&program.binary_file).unwrap();
    assert_eq!(
        result.status.code().unwrap(),
        status_code,
        "Program {} returned a unexpected status code",
        name
    );
}
