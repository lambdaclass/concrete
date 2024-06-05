use crate::common::{compile_and_run, compile_and_run_output};
use common::{build_test_linearity_error, compile_and_run_with_args};
use concrete_check::linearity_check::errors::LinearityError;
use concrete_driver::CompilerArgs;
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
#[test_case(include_str!("../../../examples/constants.con"), "constants", false, 20 ; "constants.con")]
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


#[test_case(include_str!("../../../examples/linearExample01.con"),  "linearity", false, 2; "linearExample01.con")]
#[test_case(include_str!("../../../examples/linearExample02.con"), "linearity", false, 2 ; "linearExample02.con")]
#[test_case(include_str!("../../../examples/linearExample03if.con"),  "linearity", false, 0 ; "linearExample03if.con")]
fn example_tests_with_check(
    source: &str,
    name: &str,
    is_library: bool,
    status_code: i32,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut input_path = std::env::current_dir()?;
    input_path = input_path.join(source);
    let build_dir = std::env::current_dir()?;
    let output = build_dir.join(source);
    let compile_args = CompilerArgs {
        input: input_path.clone(),
        output: output.clone(),
        release: false,
        ast: false,
        optlevel: None,
        debug_info: None,
        library: false,
        ir: false,
        llvm: false,
        mlir: false,
        asm: false,
        object: false,
        check: true,
    };
    example_tests_with_args(source, name, is_library, status_code, compile_args)
}

fn example_tests_with_args(
    source: &str,
    name: &str,
    is_library: bool,
    expected_status_code: i32,
    compile_args: CompilerArgs
) -> Result<(), Box<dyn std::error::Error>> {
    
    let not_consumed_xy_error = build_test_linearity_error(&LinearityError::VariableNotConsumed {variable: "xy".to_string()});

    //let compile_result = crate::common::compile_program_with_args(source, name, is_library, OptLevel::None, &compile_args);
    let result_1 = compile_and_run_with_args(source, name, is_library, OptLevel::None, &compile_args);
    match result_1 {
        Ok(output) => assert_eq!(expected_status_code, output.status.code().unwrap()),
        Err(e) => assert_eq!(not_consumed_xy_error.to_string(), e.to_string()),
    }
    let result_2 = compile_and_run_with_args(source, name, is_library, OptLevel::Less, &compile_args);
    match result_2 {
        Ok(output) => assert_eq!(expected_status_code, output.status.code().unwrap()),
        Err(e) => assert_eq!(not_consumed_xy_error.to_string(), e.to_string()),
    }
    
    let result_3 = compile_and_run_with_args(source, name, is_library, OptLevel::Default, &compile_args);
    match result_3 {
        Ok(output) => assert_eq!(expected_status_code, output.status.code().unwrap()),
        Err(e) => assert_eq!(not_consumed_xy_error.to_string(), e.to_string()),
    }
    
    let result_4 = compile_and_run_with_args(source, name, is_library, OptLevel::Aggressive, &compile_args);
    match result_4 {
        Ok(output) => assert_eq!(expected_status_code, output.status.code().unwrap()),
        Err(e) => assert_eq!(not_consumed_xy_error.to_string(), e.to_string()),
    }
    Ok(())
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
