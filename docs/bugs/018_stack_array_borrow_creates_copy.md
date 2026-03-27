# Bug 018: Borrowing A Stack Array Can Create A Copy Instead Of A Stable Reference

Status: fixed (f8f1bf8)

## Symptom

Taking a pointer from a stack array via `&buf as *mut u8` can behave as if it points to a temporary copy rather than to the original stack storage.

Representative shape:

```con
trusted extern fn memset(ptr: *mut u8, value: i32, n: u64) -> *mut u8;

fn main() with(Unsafe) -> Int {
    let buf: [u8; 4] = [0; 4];
    memset(&buf as *mut u8, 65, 4);
    if buf[0] == 65 { return 42; }
    return 1;
}
```

Observed effect:

- writes through the pointer succeed somewhere
- later reads of the array expression can still observe the original zeroed values

This was first surfaced by the second-wave HTTP server and `std.net::TcpStream::read_all`, where `recv` wrote into `&buf` but later reads saw stale zeros.

## Why This Counts As A Bug

This is a compiler correctness bug, not an example-level mistake:

- a borrow of stack array storage should not silently materialize an unrelated copy for mutation
- low-level I/O code depends on stable backing storage when passing array memory to FFI
- the current behavior can silently corrupt program logic rather than fail loudly

## Current Effect

- stack-array-based I/O buffers are unsafe to rely on through borrowed pointer paths
- stdlib and example code are forced to move to heap buffers as a workaround

## Likely Area

The evidence points at how array values are materialized and borrowed when taking `&buf` / casting to raw pointers, rather than at `recv`/`memset` themselves.

## Minimal Repro

- [lean_tests/bug_stack_array_borrow_copy.con](../../lean_tests/bug_stack_array_borrow_copy.con)

## Current Workaround

- use heap-allocated buffers when passing writable storage to FFI calls that expect `*mut u8`

This is a workaround only, not a resolution.
