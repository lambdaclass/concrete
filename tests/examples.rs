use crate::common::{compile_and_run, compile_and_run_output};
use concrete::compile_unit_info::OptLevel;
use test_case::test_case;

mod common;

#[test_case(include_str!("../examples/type_alias.con"), "type_alias", false, 4 ; "type_alias.con")]
#[test_case(include_str!("../examples/simple_trait.con"), "simple_trait", false, 2 ; "simple_trait.con")]
#[test_case(include_str!("../examples/generic_enum_impl.con"), "generic_enum_impl", false, 1 ; "generic_enum_impl.con")]
#[test_case(include_str!("../examples/enum.con"), "enum", false, 1 ; "enum.con")]
#[test_case(include_str!("../examples/enum_match.con"), "enum_match", false, 2 ; "enum_match.con")]
#[test_case(include_str!("../examples/direct_module_use.con"), "direct_module_use", false, 6 ; "direct_module_use.con")]
#[test_case(include_str!("../examples/sizeof_intrinsic.con"), "foo", false, 5 ; "sizeof_intrinsic.con")]
#[test_case(include_str!("../examples/assoc_method.con"), "assoc", false, 2 ; "assoc_method.con")]
#[test_case(include_str!("../examples/opaque_vec.con"), "opaque_vec", false, 4 ; "opaque_vec.con")]
#[test_case(include_str!("../examples/vec.con"), "vec", false, 4 ; "vec.con")]
#[test_case(include_str!("../examples/generics_basic.con"), "generics_basic", false, 4 ; "generics_basic.con")]
#[test_case(include_str!("../examples/generic_struct.con"), "generic_struct", false, 2 ; "generic_struct.con")]
#[test_case(include_str!("../examples/borrow.con"), "borrow", false, 2 ; "borrow.con")]
#[test_case(include_str!("../examples/variable_inside_if.con"), "variable_inside_if", false, 16 ; "variable_inside_if.con")]
#[test_case(include_str!("../examples/if_with_literals.con"), "if_with_literals", false, 1 ; "if_with_literals.con")]
#[test_case(include_str!("../examples/factorial_if.con"), "factorial_if", false, 24 ; "factorial_if.con")]
#[test_case(include_str!("../examples/fib_if.con"), "fib_if", false, 55 ; "fib_if.con")]
#[test_case(include_str!("../examples/fib_generic.con"), "fib", false, 55 ; "fib_generic.con")]
#[test_case(include_str!("../examples/import.con"), "import", false, 12 ; "import.con")]
#[test_case(include_str!("../examples/import_struct.con"), "import_struct", false, 2 ; "import_struct.con")]
#[test_case(include_str!("../examples/import_struct_impl.con"), "import_struct_impl", false, 8 ; "import_struct_impl.con")]
#[test_case(include_str!("../examples/impl_block.con"), "impl_block", false, 8 ; "impl_block.con")]
#[test_case(include_str!("../examples/impl_block_mut.con"), "impl_block_mut", false, 4 ; "impl_block_mut.con")]
#[test_case(include_str!("../examples/simple.con"), "simple", false, 8 ; "simple.con")]
#[test_case(include_str!("../examples/while.con"), "while", false, 16 ; "while.con")]
#[test_case(include_str!("../examples/chars.con"), "chars", false, 117 ; "chars.con")]
#[test_case(include_str!("../examples/floats.con"), "floats", false, 1 ; "floats.con")]
#[test_case(include_str!("../examples/refs.con"), "refs", false, 6 ; "refs.con")]
#[test_case(include_str!("../examples/structs.con"), "structs", false, 8 ; "structs.con")]
#[test_case(include_str!("../examples/casts.con"), "casts", false, 2 ; "casts.con")]
#[test_case(include_str!("../examples/malloc.con"), "malloc", false, 5 ; "malloc.con")]
#[test_case(include_str!("../examples/while_if_false.con"), "while_if_false", false, 7 ; "while_if_false.con")]
#[test_case(include_str!("../examples/if_if_false.con"), "if_if_false", false, 7 ; "if_if_false.con")]
#[test_case(include_str!("../examples/for_loop.con"), "for_loop", false, 55 ; "for_loop.con")]
#[test_case(include_str!("../examples/for_loop_var.con"), "for", false, 0 ; "for_loop_var.con")]
#[test_case(include_str!("../examples/for.con"), "for", false, 10 ; "for.con")]
#[test_case(include_str!("../examples/for_while.con"), "for_while", false, 10 ; "for_while.con")]
#[test_case(include_str!("../examples/arrays.con"), "arrays", false, 5 ; "arrays.con")]
#[test_case(include_str!("../examples/constants.con"), "constants", false, 20 ; "constants.con")]
#[test_case(include_str!("../examples/linearExample01.con"), "linearity", false, 2 ; "linearExample01.con")]
#[test_case(include_str!("../examples/linearExample02.con"), "linearity", false, 2 ; "linearExample02.con")]
#[test_case(include_str!("../examples/linearExample03if.con"), "linearity", false, 0 ; "linearExample03if.con")]
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

#[test_case(include_str!("../examples/hello_world_hacky.con"), "hello_world_hacky", false, "Hello World\n" ; "hello_world_hacky.con")]
#[test_case(include_str!("../examples/hello_world_array.con"), "hello_world_array", false, "hello world!\n" ; "hello_world_array.con")]
#[test_case(include_str!("../examples/hello_world.con"), "hello_world", false, "hello \nworld!\n" ; "hello_world.con")]
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
