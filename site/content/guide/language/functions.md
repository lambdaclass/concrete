+++
title = "Functions"
+++

# Functions

Functions are where Concrete's surface philosophy becomes obvious.

They are not only about inputs and outputs. They are also where the language makes authority, ownership, and trust boundaries visible.

## A Plain Function

```rust
pub fn double(x: Int) -> Int {
    return x * 2;
}
```

That looks familiar, and that is deliberate. Concrete does not need unusual syntax just to feel different.

## Effects And Capabilities

Where functions start to look more like Concrete is at the authority boundary:

```rust
fn read_config(path: String) with(File) -> Result<String, String> {
    return read_file(path);
}
```

The important part is not only the return type. It is that the function says it needs `File`.

That means authority is part of the function boundary, not hidden ambient context.

## A More Concrete Example

```rust
struct Counter {
    value: Int,
}

impl Counter {
    fn inc(&mut self) {
        self.value = self.value + 1;
    }
}

fn count_lines(path: String) with(File) -> Result<Int, String> {
    let text: String = read_file(path)?;
    let mut counter: Counter = Counter { value: 0 };

    if string_contains(text, "\n") {
        counter.inc();
    }

    return Ok(counter.value);
}
```

This example shows several things at once:

- methods are ordinary functions attached through `impl`
- mutation goes through `&mut`
- authority stays visible with `with(File)`
- fallible control flow stays visible with `Result` and `?`

## Generics

Functions can be generic:

```rust
fn identity<T>(x: T) -> T {
    return x;
}

let x: Int = identity::<Int>(2);
```

Concrete already supports monomorphization and trait-based dispatch, but the language direction is to keep the generic surface explicit rather than magical.

## Methods And `Self`

Methods are written in `impl` blocks and can take:

- `self`
- `&self`
- `&mut self`

```rust
struct Point {
    x: Int,
    y: Int,
}

impl Point {
    fn translate(&mut self, dx: Int, dy: Int) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }
}
```

## Trust Boundaries

Long term, functions are also one of the most important trust surfaces in Concrete:

- ordinary functions
- `trusted fn`
- `extern fn`
- `trusted extern fn`

That is part of why function boundaries matter so much in the project. They are not only call boundaries. They are authority and trust boundaries too.

## What Functions Are Meant To Feel Like

Concrete functions should stay explicit about:

- argument and return shapes
- capability requirements
- trust boundaries
- ownership and borrowing behavior

The language is trying to make low-level code easier to inspect, not easier to hide inside convenience.
