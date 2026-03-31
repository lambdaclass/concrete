# The Language

Concrete is a low-level language built around explicit semantics, explicit authority, and explicit ownership.

The easiest way to understand the language is to read it as "systems code that wants to stay legible."

That means:

- effects stay visible
- control flow stays explicit
- ownership and mutation are not hidden
- ordinary names should stay ordinary
- the compiler should be able to explain what the program means

## A Small Example

```rust
struct Counter {
    value: Int,
}

impl Counter {
    fn inc(&mut self) {
        self.value = self.value + 1;
    }
}

fn count_if_present(path: String) with(File) -> Result<Int, String> {
    let text: String = read_file(path)?;
    let mut counter: Counter = Counter { value: 0 };

    if string_contains(text, "Concrete") {
        counter.inc();
    }

    return Ok(counter.value);
}
```

This is already most of the language's personality:

- plain structs and methods
- explicit mutation through `&mut`
- explicit capability requirements through `with(File)`
- explicit error propagation with `?`
- no hidden effectful magic in code that looks pure

## What Exists Today

The current implementation already has:

- structs, enums, functions, modules, methods
- generics and trait-based dispatch
- borrows and mutable borrows
- linear ownership tracking
- capabilities via `with(...)`
- explicit `Unsafe`
- FFI support
- `defer`, `Destroy`, and layout attributes

## What The Language Is Optimizing For

Concrete is not trying to become the densest or most magical systems language.

It is trying to optimize for:

- a small semantic surface
- explicit effects and authority
- explicit trust boundaries
- explicit ownership/resource behavior
- compiler architecture that stays inspectable and eventually provable against

So when the language chooses explicitness over shorthand, that is usually intentional.

## How To Read The Next Chapters

The next chapters are meant as a guided tour, not a formal spec.

- [Modules](./modules.md) explains how code is organized
- [Variables](./variables.md) explains binding, mutation, and ownership expectations
- [Memory Management](./memory.md) explains `Heap<T>`, `with(Alloc)`, and explicit cleanup
- [Functions](./functions.md) explains capabilities, generics, and method shape
- [Structs](./structs.md) explains data layout and mutation
- [Enums](./enums.md) explains variants and pattern matching
- [Control flow](./control_flow.md) explains `if`, `match`, and loops

For stable lower-level references after that, use the root docs in `docs/`.
