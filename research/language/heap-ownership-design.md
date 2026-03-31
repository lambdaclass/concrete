# Heap Ownership Design: `Heap<T>` Access Model

**Status:** Decided — Option B (explicit borrow) with `->` syntax
**Affects:** Phase 5 (Allocator system), Phase 9 (Standard library)
**Date:** 2026-03-07

## Context

The allocator returns heap-allocated memory. The question is: what type does it return, and how do you access the value inside it?

The spec blog post defines the allocator trait as:

```
trait Allocator {
    fn alloc<T>(&mut self, count: Uint) -> Result<&mut [T], AllocError>
    fn free<T>(&mut self, ptr: &mut [T])
    fn realloc<T>(&mut self, ptr: &mut [T], new_count: Uint) -> Result<&mut [T], AllocError>
}
```

This returns `&mut [T]` — a borrow-checked, typed reference. But this creates a circular dependency: `&mut [T]` requires borrow regions (Phase 6) and slice types, which don't exist when the allocator is built (Phase 5).

More fundamentally, **allocated memory is owned, not borrowed.** When an allocator gives you memory, you own it until you free it. That's linear ownership, not a borrow. Returning a reference misrepresents the ownership semantics.

## Proposal: `Heap<T>` as a Linear Owned Type

Instead of returning a reference or a raw pointer, the allocator returns `Heap<T>` — a built-in linear type representing heap-allocated ownership.

```
// Built-in linear type
Heap<T>

trait Allocator {
    fn alloc<T>(&mut self, val: T) -> Result<Heap<T>, AllocError>;
    fn free<T>(&mut self, ptr: Heap<T>) -> T;
}

impl Destroy for Heap<T> with(Alloc) {
    fn destroy(self) -> Unit {
        // frees via bound allocator
    }
}
```

Usage:

```
fn main() with(Std) {
    let arena: Arena = Arena.new();
    defer arena.deinit();

    let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
    defer destroy(p);

    // ... access p somehow ...
}
```

This gives us:
- **No raw pointers** in the allocator API
- **No borrow regions needed** — `Heap<T>` is an owned value, not a reference
- **Linearity enforces cleanup** — forget to free? Compile error
- **No circular dependency** — Phase 5 doesn't need Phase 6
- **LIFO defer ordering works** — `defer destroy(p)` runs before `defer arena.deinit()`

The open question is how you access the value inside `Heap<T>`.

---

## Option A: Transparent Access

`Heap<T>` is transparent for field access and borrowing. The compiler has a built-in rule: field access and borrowing on `Heap<T>` operate on the inner `T`.

```
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
defer destroy(p);

p.x = 3.0;                  // field access through heap pointer
let x: Float64 = p.x;       // field read through heap pointer
let r: &Point = &p;         // borrow gives &Point, not &Heap<Point>
compute(r);
```

### How it works

The compiler knows that `Heap<T>` wraps a pointer to `T`. When it sees `p.x` where `p: Heap<T>`, it generates the same code as field access on any struct: load from pointer + field offset. The LLVM IR is identical to stack struct access because **all Concrete structs are already passed by pointer** — the compiler generates `alloca` + GEP + load for every struct field access today.

`&p` where `p: Heap<T>` gives `&T` — the pointer to the heap memory. `&mut p` gives `&mut T`. This is a built-in compiler rule, not user-extensible.

### Precedent: Zig

Zig does exactly this. `allocator.create(Point)` returns `*Point`, and `ptr.x` accesses the field transparently:

```zig
var ptr = try allocator.create(Point);
ptr.x = 1.0;        // transparent field access
ptr.y = 2.0;
defer allocator.destroy(ptr);
```

### Arguments for

1. **Clean, natural syntax.** Heap values behave like stack values for field access. Less noise in code that uses heap allocation heavily (which is most real code).
2. **The codegen is literally identical.** All structs are already pointers under the hood. `Heap<T>` is just a different pointer. The machine instruction for `p.x` is the same regardless of stack vs heap.
3. **No hidden control flow.** Field access is a primitive operation (pointer + offset load), not a function call. The spec says "no implicit function calls" — this is not a function call. `a + b` is primitive addition; `p.x` is primitive field access. Same category.
4. **The type is visible.** You know `p` is heap-allocated because its type is `Heap<Point>`, not `Point`. The allocation site (`alloc(...)`) and cleanup (`defer destroy(p)`) are explicit. Nothing is hidden.
5. **Consistent with common systems-language practice.** C, Zig, and C++ all allow direct field access through heap pointers.

### Arguments against

1. **You can't tell by reading a single line whether `p.x` goes to the stack or the heap.** You need to look at where `p` was declared. For performance auditing, this matters.
2. **Adds a special compiler rule.** `Heap<T>` is "magic" — the compiler treats it differently from other structs. This is not user-extensible (no general "Deref" trait), which keeps it simple, but it's still a built-in exception.
3. **`&p` gives `&T`, not `&Heap<Point>`.** You can never take a reference to the wrapper itself, only to the inner value. This is a semantic asymmetry.

---

## Option B: Explicit Borrow

`Heap<T>` is an opaque linear struct. To access the inner value, you must explicitly borrow through a method.

```
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
defer destroy(p);

let r: &Point = p.borrow();
let x: Float64 = r.x;

let mr: &mut Point = p.borrow_mut();
mr.x = 3.0;
```

Or with Concrete's borrow syntax:

```
borrow p as pr in R {
    let x: Float64 = pr.x;
}

borrow mut p as pmr in R {
    pmr.x = 3.0;
}
```

### How it works

`Heap<T>` is a regular linear struct with built-in `borrow()` and `borrow_mut()` methods (or compatible with the `borrow ... as ... in R` syntax). The methods return `&T` / `&mut T` pointing to the heap memory. Field access then works normally through the reference.

### Precedent: Austral

Austral uses explicit `load`/`store` functions that thread the pointer through every operation:

```austral
let p: Pointer<Point> := allocate(Point(x => 1.0, y => 2.0));
let (p2, val): (Pointer<Point>, Point) := load(p);
-- p is consumed, p2 is the "new" pointer, val is a copy
let p3: Pointer<Point> := store(p2, Point(x => 3.0, y => 4.0));
let final_val: Point := deallocate(p3);
```

Austral's `load` only works for `Free` (non-linear) types — you can't copy a linear value out of a pointer. For linear values inside pointers, you must destructure or borrow. The threading pattern (every operation consumes and returns the pointer) is extremely verbose.

### Arguments for

1. **Every heap access is visible.** When you see `.borrow()` or `borrow p as pr`, you know a heap dereference is happening. You can grep for all heap accesses. Maximum auditability.
2. **No special compiler rules.** `Heap<T>` is a regular struct with methods. No magic. The compiler doesn't need to know that `Heap<T>` is special for field access.
3. **Strictest reading of "no hidden control flow."** Every interaction with heap memory has a visible marker in the source code.

### Arguments against

1. **Verbose.** Every heap field access requires a borrow step first. Real code that works with heap data becomes noisy with `.borrow()` / `.borrow_mut()` everywhere.
2. **The ceremony adds no safety.** Linearity already prevents use-after-free, double-free, and leaks. The borrow checker prevents dangling references. The `.borrow()` call doesn't add a check that Option A lacks — it's pure ceremony.
3. **Austral's experience.** Austral uses this approach and it's verbose enough that practical code leans heavily on borrowing to avoid the load/store threading pattern. You end up writing `.borrow().x` everywhere, which is just Option A with extra characters.
4. **False explicitness.** The `.borrow()` doesn't represent a meaningful operation — it returns a pointer to the same memory. The "explicitness" doesn't convey information the programmer doesn't already have from the type (`Heap<T>`).

---

## Safety Comparison

Both options have **identical safety guarantees.** The safety comes from linearity + borrow checking, not from the access pattern:

| Hazard | Prevention | Same in both? |
|--------|-----------|--------------|
| Use after free | Linearity: `destroy(p)` consumes `p`, any later use is a compile error | Yes |
| Double free | Linearity: can't consume `p` twice | Yes |
| Memory leak | Linearity: must consume `p` exactly once | Yes |
| Dangling reference | Borrow checker: `&p` can't outlive `p` | Yes |
| Data race | Borrow checker: `&mut` is exclusive | Yes |

The ONLY difference is auditability: can you identify heap accesses by reading individual lines (Option B) or only by looking at types and declarations (Option A)?

---

## Auditability Argument

Option B's main advantage is: you can `grep` for heap accesses.

But consider what's already greppable:
- `grep with(Alloc)` → every function that allocates
- `grep Heap<` → every variable that holds heap memory
- `grep alloc(` → every allocation site
- `grep destroy(` → every deallocation site

The individual field accesses (`p.x`) don't add audit value because:
1. The allocation and deallocation are already visible
2. The type tells you it's heap memory
3. The field access itself is safe (guaranteed by the type system)

Knowing that line 47 does `p.x` through a heap pointer vs. a stack pointer doesn't change your audit — the safety is already proven by the compiler.

---

## The LLM Argument

Most code will be written by LLMs. This changes the calculus:

| Factor | Human-written code | LLM-written code |
|--------|-------------------|-----------------|
| Writing verbosity | Hurts (A wins) | Irrelevant |
| Reading clarity | Slight edge to B | B wins — more context per line |
| Auditing / review | Both fine | B wins — every heap access greppable |
| Automated analysis | Both fine | B wins — simpler AST patterns |

**Option A optimizes for writing. Option B optimizes for reading.** In an LLM world, writing is free and reading is the bottleneck.

When an LLM generates code, it will write `->` without complaint. When a human reviews that code, `->` tells them "this line touches the heap" without checking declarations. When an LLM audits code, explicit heap access sites via `->` are unambiguous patterns.

The ergonomic argument against Option B ("too verbose") was always about writing cost. If writing cost is zero, only reading cost matters. And Option B is strictly better for reading.

## Recommendation Update

Previous recommendation was Option A. **Revised recommendation: Option B (explicit borrow) with `->` syntax**, based on the LLM-written-code argument and C/C++ precedent.

The `->` operator (arrow) is a well-known heap access marker from C/C++, where `.` vs `->` has distinguished stack from heap access for 50 years. We adopt the same convention: `p->x` on `Heap<T>` borrows `p` and accesses field `x`. For writes, `p->x = val` mutably borrows `p` and assigns. The `->` token is dual-use: return type in declarations (`fn foo() -> Int`) and heap access in expressions (`p->x`). The parser distinguishes by context.

```
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
defer destroy(p);

// Read — arrow operator borrows and accesses the field
let x: Float64 = p->x;

// Or with borrow block for multiple accesses
borrow p as pr in R {
    let x: Float64 = pr.x;
    let y: Float64 = pr.y;
    compute(pr);
}

// Mutate — arrow operator with mutable borrow
p->x = 3.0;
p->y = 4.0;

// Method call on inner value
let s: Float64 = p->sum();

// HeapArray indexing
let arr: HeapArray<Int> = alloc_array(10) with(Alloc = arena);
let val: Int = arr->[0];
arr->[3] = 42;
```

This is NOT Austral's threading pattern. `Heap<T>` stays in scope (it's linear, not consumed by borrowing). The `borrow` mechanism already exists in the language. The `->` operator is a built-in compiler rule that only works on `Heap<T>` and `HeapArray<T>` — it is NOT user-extensible.

Key ergonomic point: **`p->x` is the same number of characters as `p.x` plus one.** The borrow block form (`borrow p as pr in R { ... }`) is used when you need multiple accesses in a block, avoiding repeated `p->`. For function arguments, you still write `&p` explicitly. Neither is onerous, especially when LLMs write the code.

---

## For Collections: `HeapArray<T>`

For dynamically-sized heap allocations (needed by `Vec<T>`, etc.):

```
// Built-in linear type: heap-allocated array with runtime length
HeapArray<T>

trait Allocator {
    fn alloc<T>(&mut self, val: T) -> Result<Heap<T>, AllocError>;
    fn alloc_array<T>(&mut self, count: Uint) -> Result<HeapArray<T>, AllocError>;
    fn free<T>(&mut self, ptr: Heap<T>) -> T;
    fn free_array<T>(&mut self, arr: HeapArray<T>);
    fn realloc_array<T>(&mut self, arr: HeapArray<T>, new_count: Uint) -> Result<HeapArray<T>, AllocError>;
}
```

`HeapArray<T>` supports indexing via `arr->[i]`, consistent with the `->` heap access pattern. `Vec<T>` would wrap `HeapArray<T>` + length + capacity:

```
struct Vec<T> {
    buf: HeapArray<T>,
    len: Uint,
    cap: Uint,
}
```

`Vec<T>` is linear because it contains `HeapArray<T>` (linear). Implements `Destroy with(Alloc)`.

---

## Open Questions

1. **Should `Heap<T>` be the exact name?** Alternatives: `Own<T>`, `Box<T>`, `Ptr<T>`. `Heap<T>` is the most descriptive today.
2. **Should `free` return the value (`fn free<T>(&mut self, ptr: Heap<T>) -> T`) or consume it (`fn free<T>(&mut self, ptr: Heap<T>)`)?** Returning it is more flexible (you can move a value from heap to stack). Consuming it is simpler.
3. **Can you move a value out of `Heap<T>` without freeing?** E.g., `let val: T = unbox(p) with(Alloc);` — takes the value, frees the memory. This is what `free` returning `T` would do.
4. **Update the spec blog post** once this decision is finalized — the allocator trait definition should reflect `Heap<T>` instead of `&mut [T]`.
