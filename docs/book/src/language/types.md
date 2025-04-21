# Types

Concrete supports the classic builtin types:

- Integer types:
  - Unsigned types: `u8`, `u16`, `u32`, `u64`.
  - Signed types: `i8`, `i16`, `i32`, `i64`.
- Floats: `f32`, `f64`.
- Characters: `char`.
- Booleans: `bool`.
- Strings: `String`.
- Pointers: `*const T`, `*mut T`.
- References: `&T`, `&mut T`.
- Arrays: `[T; N]`

## Characters

To define a character, we use single quotes:

```rust
let a: char = 'a';
let newline: char = '\n';
```

## Strings

To define a literal string, we use double quotes:

```rust
let greeting: String = "Hello World!";
```

## Casting

Types can be casted with the `as` keyword.

```rust
let a: i32 = 10;
let b: u8 = a as u8;
```
