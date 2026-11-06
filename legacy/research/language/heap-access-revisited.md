# Heap Access Revisited: From `(&p).x` to `p->x`

**Status:** Decided — `->` operator for heap access (C++ style arrow)
**Affects:** Phase 5 (Allocator system), Phase 6 (Borrow regions)
**Date:** 2026-03-07

## Context

In [heap-ownership-design.md](heap-ownership-design.md), we decided on Option B (explicit borrow) for accessing `Heap<T>` values. The original syntax was `(&p).x`:

```
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
defer destroy(p);

let x: Float64 = (&p).x;           // must borrow to access
```

Instead of transparent access (Option A):

```
let x: Float64 = p.x;              // transparent
```

The question: after researching how other languages handle this, is `(&p).x` the right surface syntax, or is there something better?

---

## How Other Languages Handle Heap Access

| Language | Syntax | Visual marker? | Verbosity |
|----------|--------|---------------|-----------|
| **Zig** `*T` | `ptr.field` | None — one level auto-deref for structs | Low |
| **Austral** `Pointer[T]` | `!(ref->field)` + borrow block | Yes — `->`, `!`, `borrow` blocks | High |
| **C++** `unique_ptr<T>` | `uptr->field` | Yes — `->` vs `.` | Moderate |
| **Concrete** `Heap<T>` | `p->field` | Yes — `->` vs `.` | Low-moderate |

### Codegen reality

All four languages generate **identical machine code** for the actual field access: load pointer, then load field at offset. The only difference is syntax.

For stack values, all four generate: load field from known stack offset (one instruction).
For heap values, all four generate: load pointer from stack, then load field from pointer + offset (two instructions).

Transparent access (as in Zig) and explicit deref (`->`, `!`) are purely compile-time — zero runtime cost difference.

---

## Revisiting the Arguments

### Why we chose Option B (explicit borrow)

From [heap-ownership-design.md](heap-ownership-design.md):

1. **LLM-written code:** Writing cost is zero, reading cost is the bottleneck. An explicit marker tells the reader "this touches the heap."
2. **Greppability:** Every heap access has a visible marker — you can grep for all heap dereferences.
3. **No special compiler rules:** `Heap<T>` is opaque, borrowing works through an explicit mechanism.

### What the research shows

1. **Zig chooses transparency** (no visual marker). This works well in practice, and the lack of a visual marker for heap access has not been a major usability problem there.

2. **Austral's extreme explicitness is genuinely painful.** `!(ref->sun->pos->x)` with borrow blocks everywhere — even Austral's creator acknowledges the verbosity.

3. **C++'s `->` is moderate** and has survived 40+ years. The `.` vs `->` distinction is the most battle-tested "heap access marker" in programming history.

4. **`(&p).x` was awkward.** The parentheses were needed because `&p.x` would parse as `&(p.x)` — borrowing the field, not the Heap wrapper. The `->` operator avoids this parsing issue entirely.

The explicitness spectrum:

```
Zig:     ptr.x           (transparent)
C++:     ptr->x          (visual marker, minimal overhead)
Concrete: p->x           (visual marker, minimal overhead, same as C++)
Austral: !(ref->field)   (most explicit)
```

### The real question

Does the LLM argument still hold? Let's be precise:

**What LLMs struggle with (that explicit borrow helps):**
- Nothing, actually. LLMs don't struggle with `p.x` vs `p->x`. They struggle with implicit Drop (solved by `defer`), lifetime elision (solved by no lifetime params), and trait dispatch (solved by no operator overloading).

**What explicit borrow DOES help with:**
- Human code review: "this line touches the heap" is visible without checking declarations
- Grep-based auditing: `grep '->'` finds heap accesses (much cleaner than grepping for `(&`)
- Static analysis: heap vs stack access is syntactically distinct

**What explicit borrow COSTS:**
- Visual noise: `p->x` repeated dozens of times in heap-heavy code (though this is minimal — one extra character vs `p.x`)
- Mental overhead: programmers must remember that `Heap<T>` uses `->`, unlike stack structs

---

## The `->` Syntax Decision

The original `(&p).x` syntax had a concrete problem: the parentheses. `&p.x` parses as `&(p.x)`, so you need `(&p).x` — which is noisy.

The `->` operator solves this cleanly:

```
// Old syntax (decided against)
let x: Float64 = (&p).x;
let y: Float64 = (&p).y;
let sum: Float64 = (&p).x + (&p).y;
(&mut p).x = 3.0;

// New syntax (chosen)
let x: Float64 = p->x;
let y: Float64 = p->y;
let sum: Float64 = p->x + p->y;
p->x = 3.0;
```

Advantages of `->` over `(&p).x`:
1. **No parentheses needed.** `p->x` is unambiguous without grouping.
2. **Familiar to C/C++ programmers.** 50 years of precedent for `.` vs `->`.
3. **Cleaner grep pattern.** `grep '->'` is more precise than `grep '(&'`.
4. **Less visual noise.** `p->x` vs `(&p).x` — the arrow is tighter.
5. **Dual-use token.** `->` already exists for return types (`fn foo() -> Int`). In expression position it means heap access. The parser distinguishes by context, and this is unambiguous in LL(1).

### Full syntax:

```
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc = arena);
defer destroy(p);

// Read
let x: Float64 = p->x;

// Write
p->x = 3.0;

// Method call through heap
let s: Float64 = p->sum();

// HeapArray indexing
let arr: HeapArray<Int> = alloc_array(10) with(Alloc = arena);
let val: Int = arr->[0];
arr->[3] = 42;

// Pass reference to function (still explicit &p)
compute(&p);

// Multiple accesses — borrow block (Phase 6)
borrow p as pr in R {
    let x: Float64 = pr.x;     // pr is &Point, normal dot
    let y: Float64 = pr.y;
    compute(pr);
}
```

---

## Analysis: What Actually Matters for Auditability

When auditing code for heap usage, what do you actually need to see?

1. **Where is memory allocated?** → `alloc(...)` — always visible
2. **Where is memory freed?** → `destroy(p)` or `defer destroy(p)` — always visible
3. **What function signatures require heap?** → `with(Alloc)` — always visible
4. **Which variables hold heap data?** → `Heap<T>` in the type — always visible
5. **Where are individual heap field accesses?** → `p->x` makes this visible

Items 1-4 are the most important audit points. Item 5 is lower-value but `->` provides it at near-zero syntactic cost — unlike the old `(&p).x` which was noticeably noisier.

Put differently: with `->`, you get the auditability of Option B at a syntactic cost nearly as low as Option A. Best of both worlds.

---

## The LLM Argument, Revisited

The original argument was: "In an LLM world, writing is free and reading is the bottleneck. Option B is better for reading."

With `->` syntax, this argument is strengthened:
1. **LLMs write `->` effortlessly** — it's a well-known token from C/C++, which dominates training data
2. **`->` is a clear visual marker** that tells readers (human or LLM) "this touches the heap" without checking declarations
3. **`grep '->'` in expression context** cleanly finds all heap accesses
4. **The cost is minimal** — `p->x` is one character longer than `p.x`

The borrow block form (`borrow p as pr in R`) remains valuable for multi-statement borrows and is still available.

---

## Final Decision

**Use `->` (C++ style arrow operator) for heap access on `Heap<T>` and `HeapArray<T>`.** This was chosen over the earlier `(&p).x` syntax.

Summary of the access model:

| Operation | Syntax |
|-----------|--------|
| Field read | `p->x` |
| Field write | `p->x = val` |
| Method call | `p->method()` |
| HeapArray index read | `arr->[i]` |
| HeapArray index write | `arr->[i] = val` |
| Pass as function arg | `compute(&p)` (explicit borrow) |
| Multi-access block | `borrow p as pr in R { pr.x; }` |

The `->` operator is:
- A built-in compiler rule, NOT user-extensible
- Only valid on `Heap<T>` and `HeapArray<T>`
- Sugar for "borrow then access" (read borrows immutably, write borrows mutably)
- Dual-use with the return type arrow in declarations (`fn foo() -> Int`), disambiguated by parser context

---

## Open Questions

1. **Borrow block naming:** Is `borrow p as pr in R { ... }` the best syntax? Could it be shorter? `let pr = &p { ... }`? This is a Phase 6 question.
