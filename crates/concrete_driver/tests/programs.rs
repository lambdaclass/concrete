use common::{compile_program, run_program};

mod common;

#[test]
fn test_while() {
    let source = r#"
        mod Simple {
            fn main() -> i64 {
                return my_func(4);
            }

            fn my_func(times: i64) -> i64 {
                let mut n: i64 = times;
                let mut result: i64 = 1;

                while n > 0 {
                    result = result + result;
                    n = n - 1;
                }

                return result;
            }
        }
    "#;

    let result = compile_program(source, "while", false).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");
    let code = output.status.code().unwrap();
    assert_eq!(code, 16);
}

#[test]
fn test_factorial_with_if() {
    let source = r#"
        mod Simple {
            fn main() -> i64 {
                return factorial(4);
            }

            fn factorial(n: i64) -> i64 {
                if n == 0 {
                    return 1;
                } else {
                    return n * factorial(n - 1);
                }
            }
        }
    "#;

    let result = compile_program(source, "factorial", false).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");
    let code = output.status.code().unwrap();
    assert_eq!(code, 24);
}

#[test]
fn test_fib_with_if() {
    let source = r#"
        mod Fibonacci {
            fn main() -> i64 {
                return fib(10);
            }

            pub fn fib(n: u64) -> u64 {
                if n < 2 {
                    return n;
                }

                return fib(n - 1) + fib(n - 2);
            }
        }
    "#;

    let result = compile_program(source, "fib", false).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");
    let code = output.status.code().unwrap();
    assert_eq!(code, 55);
}

#[test]
fn test_simple_add() {
    let source = r#"
        mod Simple {
            fn main() -> i32 {
                let x: i32 = 2;
                let y: i32 = 4;
                return add_plus_two(x, y);
            }

            fn add_plus_two(x: i32, y: i32) -> i32 {
                let mut z: i32 = 1;
                z = z + 1;
                return x + y + z;
            }
        }
    "#;

    let result = compile_program(source, "simple_add", false).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");
    let code = output.status.code().unwrap();
    assert_eq!(code, 8);
}

#[test]
fn test_import() {
    let source = r#"
        mod Simple {
            import Other.{hello};

            fn main() -> i64 {
                return hello(4);
            }
        }

        mod Other {
            pub fn hello(x: i64) -> i64 {
                return x * 2;
            }
        }
    "#;

    let result = compile_program(source, "import", false).expect("failed to compile");

    let output = run_program(&result.binary_file).expect("failed to run");
    let code = output.status.code().unwrap();
    assert_eq!(code, 8);
}
