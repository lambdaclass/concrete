# Variables

The `let` keyword is used to declare and bind variables.

- You must declare _and bind_ the variable at the same time.
- There is no local type inference (ATM), so you must specify the type of the variable.

```rust
let x: u32 = 13;
```

## Mutate Variables

Variables declared with the mut modifier can be mutated.

```rust
let mut x: u32 = 11;
x = x + 2;
```
