# References

Concrete supports references, which can though of as safe alternatives to pointers. Unlike Rust, the borrow checker aims to be much simpler.

On types, the symbol `&` indicates that its actually a reference to the inner type. The following function takes a reference to a `Vec2`.

```rust
fn get_first(vec: &[i32; 3]) -> i32 {
    return vec[0];
}
```

On values, the symbol `&` indicates that we are taking a reference of the given variable. The following snippet takes a reference to a `Vec2` and calls `get_x`.

```rust
let arr: [i32; 3] = [1, 2, 3];
get_first(&arr);
```

A reference can be mutable:

```rust
fn set_first(vec: &mut [i32; 3], first: i32) -> i32 {
    vec[0] = first;
}
```

Similarly, we can take mutable references to values:

```rust
let mut arr: [i32; 3] = [1, 2, 3];
set_x(&mut arr, 10);
```

A reference can be dereferenced with `*`:

```rust
let x: i32 = 10;
let x_ref: &i32 = &x;
*x_ref = 20;
```
