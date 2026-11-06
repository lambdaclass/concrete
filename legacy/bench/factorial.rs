#[no_mangle]
pub extern "C" fn rust_function(n: u64) -> u64 {
    if n == 0 {
        return 1;
    } else {
        return n * rust_function(n - 1);
    }
}
