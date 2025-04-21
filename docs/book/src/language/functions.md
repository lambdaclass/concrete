# Functions

The `fn` keyword is used to declare functions.

- You must specify the type of the parameters, and the return value.

```rust
fn add(x: i32, y: i32) -> i32 {
    return x + y;
}
```

The function is called with the classic parenthesis syntax.

```rust
let z: i32 = add(x, y);
```

To make a function available outside of the module, use the `pub` modifier.

```rust
pub fn public(x: i32) {
    ...
}
```

## Generics

Functions can be generic:

```rust
fn generic<T>(arg: T) -> T {
    ...
}
```

We must specify the generic argument type when calling it:

```rust
let x: i32 = generic::<i32>(2);
```
