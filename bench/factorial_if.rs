#[no_mangle]
pub extern "C" fn factorial_rust(n: u64) -> u64 {
    if n == 0 {
        return 1;
    } else {
        return n * factorial_rust(n - 1);
    }
}
