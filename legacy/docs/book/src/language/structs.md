# Structs

Structs are ordinary product types in Concrete, but they also matter to layout, mutation, and ABI work.

That makes them more central than "just a record type."

## A Simple Struct

```rust
struct Point {
    x: Int,
    y: Int,
}
```

You can construct values directly:

```rust
let p: Point = Point { x: 3, y: 4 };
```

## Methods

```rust
impl Point {
    pub fn new(x: Int, y: Int) -> Point {
        return Point { x: x, y: y };
    }

    pub fn translate(&mut self, dx: Int, dy: Int) {
        self.x = self.x + dx;
        self.y = self.y + dy;
    }
}
```

This is a good example of Concrete's style:

- plain data types
- explicit mutation
- no hidden object model

## Why Structs Matter More Here

In Concrete, structs matter in at least three ways:

1. they are normal user-facing data types
2. they carry layout information that matters to ABI and FFI work
3. they are one of the places where ownership and mutation become concrete

That is why the project has dedicated layout and ABI documentation, and why attributes such as `#[repr(C)]` matter.

## Generics

Structs can also be generic:

```rust
struct Boxed<T> {
    value: T,
}
```

Again, the goal is not surface cleverness. The goal is a type system that stays explicit enough to reason about through elaboration, monomorphization, and lowering.

## Mutation And Compiler Reality

Struct mutation is not only a frontend feature. It has been a major compiler-hardening area because mutable aggregates must lower in a stable way.

That is a good example of Concrete's larger design philosophy:

- user-facing code should stay normal and readable
- compiler lowering should still preserve explicit structure underneath

So structs sit right at the intersection of language ergonomics and backend honesty.
