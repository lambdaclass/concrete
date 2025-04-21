# Arrays

The syntax for declaring fixed-sized arrays is similar to Rust’s.

```rust
let array: [u8; 5] = [13, 26, 39, 52, 65];
```

Arrays are indexed with square brackets:

```rust
let first: u8 = array[0];
```

We can also declare nested arrays:

```rust
let matrix: [[u8; 2]; 2] = [[13, 26], [39, 52]];
```
