# Extern Functions

To interoperate with external libraries, you can declare external functions using the `extern` keyword.

```rust
extern fn fopen(name: *mut u8, mode: *mut u8) -> *mut u8;
extern fn fread(ptr: *mut u8, size: u64, nitems: u64, stream: *mut u8) -> u64;
```

The following snippet reads the first 4 bytes of a file:

```rust
let name: String = "filename";
let mode: String = "r";
let file: *mut u8 = fopen(name.ptr, mode.ptr);
let data: [u8; 4] = [0, 0, 0, 0];
fread(&data as *mut u8, 1, 4, file);
```
