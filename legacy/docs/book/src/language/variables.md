# Variables

Variables are introduced with `let`.

```rust
let x: Int = 2;
```

Mutable variables use `mut`:

```rust
let mut x: Int = 2;
x = x + 1;
```

That part is ordinary. What matters more in Concrete is that variables participate directly in the ownership model.

## Copyable vs Linear Values

For ordinary copyable values, reuse looks normal:

```rust
let x: Int = 3;
let y: Int = x + x;
```

For linear or resource-owning values, reuse rules are stricter. The language is trying to make ownership visible instead of leaving it implicit.

## Borrowing And Mutation

```rust
struct Counter {
    value: Int,
}

fn bump(c: &mut Counter) {
    c.value = c.value + 1;
}
```

Here the mutation boundary is explicit:

- the caller owns the `Counter`
- the function receives a mutable borrow
- mutation does not look like ordinary by-value assignment

That explicitness matters because Concrete wants aliasing and mutation to stay legible to both the programmer and the compiler.

## Why The Language Likes Explicit Types

Concrete still expects explicit types in many places:

```rust
let path: String = "README.md";
let mut total: Int = 0;
```

That is partly a maturity choice and partly a language-style choice. It helps with:

- compiler clarity
- diagnostics
- auditability
- avoiding inference-driven surprises while the language surface is still tightening

## The Bigger Point

Variables in Concrete are not just names bound to values.

They are one of the places where you see the language's real priorities:

- explicit mutation
- explicit ownership
- explicit borrowing
- explicit consumption rules for non-copy values
