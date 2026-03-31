#[no_mangle]
pub extern "C" fn rust_function(n: u64) -> u64 {
    if n < 2 {
        return n;
    }

    return rust_function(n - 1) + rust_function(n - 2);
}
