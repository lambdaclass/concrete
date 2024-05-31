use crate::common::{compile_and_run, compile_and_run_output};
use concrete_session::config::OptLevel;
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
#[test_case(include_str!("../../../examples/structs.con"), "structs", false, 8 ; "structs.con")]
#[test_case(include_str!("../../../examples/casts.con"), "casts", false, 2 ; "casts.con")]
#[test_case(include_str!("../../../examples/malloc.con"), "malloc", false, 5 ; "malloc.con")]
#[test_case(include_str!("../../../examples/while_if_false.con"), "while_if_false", false, 7 ; "while_if_false.con")]
#[test_case(include_str!("../../../examples/if_if_false.con"), "if_if_false", false, 7 ; "if_if_false.con")]
#[test_case(include_str!("../../../examples/for.con"), "for", false, 10 ; "for.con")]
#[test_case(include_str!("../../../examples/for_while.con"), "for_while", false, 10 ; "for_while.con")]
#[test_case(include_str!("../../../examples/arrays.con"), "arrays", false, 5 ; "arrays.con")]
#[test_case(include_str!("../../../examples/linearExample01.con"), "linearity", false, 2 ; "linearExample01.con")]
fn example_tests(source: &str, name: &str, is_library: bool, status_code: i32) {
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::None)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Less)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Default)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Aggressive)
    );
}

#[test_case(include_str!("../../../examples/linearExample01.con"), "--check", "linearity", false, 2; "linearExample01.con")]
#[test_case(include_str!("../../../examples/linearExample02.con"), "--check", "linearity", false, 2 ; "linearExample02.con")]
#[test_case(include_str!("../../../examples/linearExample03if.con"), "--check", "linearity", false, 0 ; "linearExample03if.con")]
fn example_tests_with_options(
    source: &str,
    options: &str,
    name: &str,
    is_library: bool,
    status_code: i32,
) {
    // TODO need compile_and_run with Options for using args
    let _args = [options];
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::None)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Less)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Default)
    );
    assert_eq!(
        status_code,
        compile_and_run(source, name, is_library, OptLevel::Aggressive)
    );
}

#[test_case(include_str!("../../../examples/hello_world_hacky.con"), "hello_world_hacky", false, "Hello World\n" ; "hello_world_hacky.con")]
#[test_case(include_str!("../../../examples/hello_world_array.con"), "hello_world_array", false, "hello world!\n" ; "hello_world_array.con")]
fn example_tests_with_output(source: &str, name: &str, is_library: bool, result: &str) {
    assert_eq!(
        result,
        compile_and_run_output(source, name, is_library, OptLevel::None)
    );
    assert_eq!(
        result,
        compile_and_run_output(source, name, is_library, OptLevel::Less)
    );
    assert_eq!(
        result,
        compile_and_run_output(source, name, is_library, OptLevel::Default)
    );
    assert_eq!(
        result,
        compile_and_run_output(source, name, is_library, OptLevel::Aggressive)
    );
}
