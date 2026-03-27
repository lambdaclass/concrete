# Memory Management

Concrete does not use a garbage collector.

It is trying to keep memory management explicit like a systems language, while making ownership and cleanup easier for both the programmer and the compiler to reason about.

## The Short Version

- stack values are ordinary local values
- heap allocation is explicit
- ownership is tracked
- borrows are explicit
- cleanup is explicit
- allocation effects are explicit in function signatures

So the language is not "manual memory management" in the loose C sense, and it is not "automatic memory management" in the GC sense.

It is closer to an ownership-based model:

- you can see where allocation happens
- you can see where cleanup is scheduled
- the compiler checks that owned values are used correctly

## Three Common Models

At a high level, memory management usually falls into one of three buckets:

1. garbage-collected memory
2. unmanaged manual memory
3. ownership-tracked explicit memory

Concrete is aiming for the third model.

That means:

- memory management stays explicit
- the program still decides when allocation and cleanup happen
- but ownership rules are part of the language, not just a coding convention

So the goal is not "hide memory management from the user." The goal is "make resource behavior explicit enough to check."

## No Garbage Collector

Concrete is designed for low-level code without relying on a GC-managed runtime.

That matters because the project wants:

- visible allocation behavior
- visible cleanup behavior
- visible resource ordering
- code that is easier to audit and eventually prove properties about

If a function allocates, that should be visible in the source and in its signature. If a value must be cleaned up, that should also be visible in the source.

## Heap Memory Is Explicit

Heap-owned values use explicit heap types such as `Heap<T>`.

Typical heap allocation looks like this:

```concrete
let p: Heap<Point> = alloc(Point { x: 1, y: 2 }) with(Alloc = arena);
defer destroy(p);
```

This tells you several things immediately:

- `alloc(...)` is the allocation site
- `Heap<Point>` means the value is heap-owned
- `with(Alloc = arena)` makes the allocator binding explicit
- `defer destroy(p)` schedules cleanup explicitly

That is a central Concrete design choice: the allocation site, the ownership type, and the cleanup point should all be visible.

## What The Compiler Tracks

Concrete uses linear ownership for values that are not `Copy`.

In practice that means the compiler tracks whether owned values are consumed correctly. The intended result is that the language can reject:

- use-after-free
- double-free
- leaks from forgotten owned values
- dangling references

For heap values, the important idea is:

- heap ownership is explicit in the type
- destruction is explicit in the code
- correctness is enforced by the type system

So the compiler does not silently free memory for you, but it does force ownership to balance out correctly.

## `destroy` And `defer`

Concrete deliberately avoids hidden destructor behavior.

You do not rely on an invisible drop system. Instead, cleanup is written directly:

```concrete
defer destroy(p);
```

This means:

- cleanup is part of the source code
- scope-exit cleanup order is explicit
- the reader can see where resource release was scheduled

`defer` is the usual way to express "clean this up when the scope ends."

## `with(Alloc)` Means Allocation Is Part Of The API

Allocation is treated as a semantic effect, not an invisible implementation detail.

If a function may allocate, that is reflected in its signature with `with(Alloc)`.

That makes questions like these easy to answer:

- does this function allocate?
- where are the allocation-capable call paths?
- which APIs are allocation-free?

Concrete wants allocation behavior to be inspectable instead of ambient.

## Why This Model Exists

Concrete is trying to make a few things true at once:

- the code stays low-level and predictable
- allocation remains visible
- cleanup remains visible
- APIs can expose whether they allocate
- ownership mistakes are caught early

That combination is the main reason to use this model instead of either a GC runtime or looser manual memory management.

## Compared With Unmanaged Manual Memory

Languages with looser manual memory management usually leave ownership discipline mostly up to the programmer.

That tends to make certain problems much easier to write:

- forgetting to free memory
- freeing the same thing twice
- reading memory after it was freed
- keeping aliases around longer than intended
- losing track of which function owns cleanup responsibility

Concrete tries to improve this by making ownership and borrowing first-class parts of the language model.

So compared with unmanaged manual memory:

- allocation is still explicit
- cleanup is still explicit
- but ownership is more directly tracked by the compiler
- and resource behavior is easier to audit from the source

## Compared With Garbage-Collected Memory

Concrete also differs from GC-based languages.

In a GC model:

- allocation is often cheap to write and easy to hide
- cleanup timing is not usually expressed directly in the source
- APIs often do not surface allocation behavior clearly
- runtime behavior depends partly on collector behavior

Concrete chooses the opposite tradeoff:

- allocation is visible
- cleanup is visible
- ownership boundaries are visible
- resource ordering is visible

That fits the project goal of making low-level behavior inspectable and mechanically understandable.

## Compared With C And C++

Concrete shares the explicitness of C and C++, but it tries to remove more of the failure modes.

In C or C++:

- ownership is often a convention
- aliasing is easy to lose track of
- freeing the same thing twice is a common class of bug
- using memory after free is a common class of bug
- resource cleanup discipline depends heavily on programmer habits

Concrete tries to improve that by making ownership and borrowing part of the language model itself rather than just library style or team discipline.

So compared with C/C++:

- allocation is still explicit
- cleanup is still explicit
- but ownership is more directly tracked by the compiler
- and the intended safety guarantees are much stronger

## Compared With Rust

Concrete is closer to Rust than to C or C++.

Like Rust, it aims for:

- no GC
- ownership and borrowing
- compile-time checking of resource and memory behavior

But Concrete is intentionally stricter and more explicit in some places.

Examples:

- cleanup is written explicitly as `destroy(x)` or `defer destroy(x)`
- allocation is surfaced explicitly through `with(Alloc)`
- the language avoids more hidden machinery and convenience features

So a useful rough mental model is:

- Rust: ownership with more convenience and more language machinery
- Concrete: ownership with a stronger bias toward visible effects, visible cleanup, and a smaller semantic surface

## What This Means In Practice

When you read Concrete code, you should usually be able to answer:

- which values are owned?
- which values are borrowed?
- where heap allocation happens?
- where cleanup is scheduled?
- which functions may allocate?

That is the real goal of the design.

The point is not just to avoid a garbage collector. The point is to make resource behavior explicit enough that both humans and the compiler can reason about it clearly.

## Related Chapters

- [Variables](./variables.md)
- [Functions](./functions.md)
- [Structs](./structs.md)
- [Control flow](./control_flow.md)
