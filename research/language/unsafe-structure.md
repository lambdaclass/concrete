# Stronger `Unsafe` Structure

**Status:** Open, partially adopted

This note explores how Concrete could make `Unsafe` more useful and more auditable without turning it into a large, complex sub-language.

The strongest current refinement path is the three-way split:

- semantic effects stay in capabilities such as `with(Alloc)`
- implementation-level pointer unsafety is now contained by `trusted fn` / `trusted impl` at the language level, with the remaining work being coherent migration across builtins and stdlib
- foreign semantic boundaries (`extern fn`, conservatively `transmute`) remain under `with(Unsafe)`, with a narrow per-binding exception for `trusted extern fn` (audited pure foreign bindings like math functions)

This should apply uniformly across:

- compiler builtins
- stdlib code
- user code

Otherwise the language ends up with different unsafe truths at different layers.

The current rule is intentionally simple:

- `with(Unsafe)` gates the explicit low-level boundary
- `grep with(Unsafe)` finds that boundary quickly
- extern calls and raw-pointer-sensitive operations are visible in signatures and types

That simplicity is valuable. The question is how to improve the *structure* and *inspectability* of `Unsafe` without losing that clarity.

## Rust, Zig, and the Concrete Target

Rust and Zig are both useful reference points here, but in different ways.

Rust is generally stronger at *preventing* invalid low-level behavior:

- ownership and aliasing mistakes are rejected more aggressively
- safe APIs contain many classes of mistakes by default
- the type system carries more of the burden up front

Zig is often clearer at the *operational* level:

- low-level operations look low-level
- allocator behavior is visible
- fewer abstraction layers hide what the code is doing

Concrete should not try to become "Rust but stricter" or "Zig plus effects."

The better target is:

- **more explicit than Zig**
- **less sprawling than Rust**
- **more inspectable than both**

That means:

- keep `with(Unsafe)` simple and visible
- make the compiler much better at explaining *why* unsafe is required
- encourage wrapper-based containment so unsafe code stays narrow
- keep the trusted computing base small by shrinking compiler magic and keeping trusted boundaries explicit

The goal is not to beat Zig by adding more unsafe syntax. The goal is to make unsafe code easier to review, audit, and trust.

## Why Improve It

Today, `Unsafe` is broad by design:

- extern calls
- raw pointer dereference and assignment
- pointer-involving casts
- later, likely `transmute`

That is workable, but a stronger unsafe story could make Concrete better for:

- code review
- security auditing
- proof-driven containment
- low-level library design

The key is to improve the model without making ordinary code harder to read.

## Good Directions

### 1. Better `Unsafe` reports first

The highest-leverage improvement is not new syntax. It is better compiler output.

Useful reports would show:

- which functions require `Unsafe`
- why `Unsafe` is required
- whether it comes from:
  - FFI
  - raw pointer dereference
  - raw pointer assignment
  - pointer cast
  - later, `transmute`
- call-chain traces for transitive `Unsafe` usage

Example:

```text
fn read_header:
  unsafe reasons:
    - extern call: read
    - raw pointer dereference

why transitive Unsafe?
  parse_file -> read_header
```

This strengthens the audit story without changing the language surface.

This is also one of the clearest ways Concrete could improve on Zig: keep the low-level boundary explicit, but make the compiler much better at explaining it.

From a security point of view, this matters because the review target is not only “all unsafe code,” but “all places where the program stops being justified by ordinary safe semantics.” Concrete should make those places mechanically visible.

The reporting story should eventually distinguish:

- ordinary `with(Unsafe)` authority
- trusted implementation boundaries
- the specific unsafe reasons inside each category

### 2. Prefer containment through safe wrappers

The best long-term `Unsafe` structure may come more from API style than from effect syntax.

Good pattern:

- write a small unsafe wrapper
- expose a safe, narrow interface above it
- keep raw pointers and FFI details inside the wrapper

This should be encouraged by the stdlib and examples.

Examples:

- socket wrappers
- buffer views over foreign memory
- FFI handle wrappers
- `repr(C)` interop helpers

This is one of the main ways Concrete can stay simpler than Rust while still containing danger more clearly than Zig-style direct low-level code.

### 3. Add reason-level categorization before source-level splitting

Concrete may eventually want to distinguish categories like:

- `UnsafeFFI`
- `UnsafePtr`
- `UnsafeLayout`

But the first place to do that should probably be:

- compiler reports
- internal diagnostics

not source syntax.

That would improve inspectability without making effect signatures explode.

### 4. Encourage authority-restricted unsafe wrappers

Unsafe code often becomes easier to trust when it is paired with explicit wrapper values.

Examples:

- read-only foreign buffer
- append-only logging sink around an FFI call
- validated handle type after unsafe construction

This combines well with Concrete's ownership model and capability style.

### 5. Keep the source-language story simple for as long as possible

The source-level `Unsafe` effect should stay simple unless there is overwhelming evidence that more granularity is needed.

That means:

- keep `with(Unsafe)` as the user-facing gate
- improve reports and wrappers first
- only later consider finer-grained unsafe authority in source syntax

This is the key constraint. If Concrete tries to "improve unsafe" by turning it into a large unsafe effect language, it will lose one of its best qualities.

## Things To Avoid

Concrete should avoid making `Unsafe` "better" by making it harder to read.

Avoid:

- many separate unsafe effect keywords too early
- large unsafe type/effect algebras in ordinary code
- implicit unsafe propagation rules
- clever unsafe abstractions that hide the real boundary

The point of `Unsafe` is to make danger obvious, not to create a second advanced language inside the first one.

## Relationship To Other Work

This connects directly to:

- [trusted-boundary.md](trusted-boundary.md) — the `trusted fn` / `trusted impl` design for containing pointer-level implementation unsafety behind safe APIs, without leaking `Unsafe` to callers. The wrapper-based containment pattern described here is what `trusted` formalizes as a language-level boundary.
- [capability-sandboxing.md](capability-sandboxing.md)
- [no-std-freestanding.md](no-std-freestanding.md)
- [builtin-vs-stdlib.md](builtin-vs-stdlib.md)
- [../docs/FFI.md](../docs/FFI.md)

That trusted-boundary note also makes two constraints explicit that matter here:

- `trusted` should be available to user code, not only stdlib internals
- `extern fn` calls should remain under `with(Unsafe)` even inside `trusted` code

It also overlaps with audit-focused compiler outputs:

- `Unsafe` summaries
- `trusted` region reports (see trusted-boundary.md)
- later allocation / destruction reports
- capability call-chain explanations

## Recommended Order

If Concrete improves `Unsafe`, the best order is:

1. better `Unsafe` reports
2. stronger wrapper patterns in stdlib and examples
3. internal/report-level categorization of unsafe reasons
4. only then reconsider whether source-level `Unsafe` should become more structured

## Recommendation

Yes, Concrete should eventually have a stronger `Unsafe` story.

But the right path is:

- improve visibility first
- improve containment through wrappers second
- only then consider syntax or effect-model refinement

That keeps the language simple while making unsafe code easier to audit and trust.

## Bottom Line

Rust is stronger at prevention.

Zig is often clearer operationally.

Concrete should aim to be:

- operationally explicit like Zig
- structurally constrained like a smaller, simpler Rust
- more inspectable than either at the unsafe boundary

That is the path to a stronger `Unsafe` story without sacrificing readability.
