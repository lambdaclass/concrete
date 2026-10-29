# Concrete Style Guide

Status: guidance — ROADMAP old Phase 6 #16; the [iteration protocol](#iteration-protocol) section
is the canonical reference of ROADMAP old Phase 6 #17 (it was `ITERATION_PROTOCOL.md` until
2026-10-29).

Mechanical layout (indentation, spacing, wrapping, trailing commas) is **not a
matter of taste in Concrete**: it is owned by `concrete fmt`. Run it; do not hand-
format. This guide covers what `fmt` does *not* decide — naming, structure, and
the idioms that keep code readable and audit-friendly. It is advisory, with two
mechanically-enforced parts: `concrete fmt` (gated by `check_concrete_fmt.sh`) and the
traversal forms of the [iteration protocol](#iteration-protocol) (gated by
`check_iteration_protocol.sh`).

## Formatting is `concrete fmt`'s job

- `concrete fmt <file>` (stdout), `--write` (in place), `--check` (CI), `--stdin`.
- Formatting is **semantics-preserving**: `--report fingerprints` is byte-identical
  before and after, so `fmt` never invalidates proof links or manifests.
- Don't argue about brace placement or wrapping in review — `fmt --check` settles it.

## Naming

- **Types** (`struct`, `enum`, newtype, alias): `UpperCamelCase` — `Digest`, `TcpConn`.
- **Functions, variables, fields, modules**: `lower_snake_case` — `read_u32_be`, `block_count`.
- **Enum variants**: `UpperCamelCase` — `Result.Ok`, `State.Ready`.
- **Compile-time / target facts**: `SCREAMING_SNAKE_CASE` — `CONCRETE_OS`.
- Prefer names that state *units and intent*: `len_bytes`, `timeout_ms`,
  `off_in_block` — not bare `len`, `t`, `i` for anything non-local. (Concrete has
  no units-of-measure types; the name is the contract — see ROADMAP #15.)

## Functions

- Keep bodies flat: prefer small exhaustive matches and early `return` over deep nesting. Extraction into
  `ProvableV1` favors flat, non-nested control flow, so flat code is also more
  provable.
- One responsibility per function; push branching into small helpers.
- Make capabilities visible and minimal: a function takes `with(Console)` only if
  it truly needs it. Narrow capability sets are easier to audit and to prove.

## Modules

- One concept per module; expose the minimum (`pub`) surface.
- Order within a module: types → constants → public API → private helpers → tests.
- Use import aliases / qualified names (ROADMAP #2a) to avoid renaming on name
  clashes rather than inventing awkward local names.

## Pattern matching and error handling

- Prefer exhaustive `match` over chains of `if`; let the non-exhaustiveness error
  catch missing cases.
- Use `Result`/`Option`, exhaustive `match`, and explicit error returns for fallible paths; do not encode errors as
  sentinel integers (the checked-arithmetic model will trap on a sentinel that
  overflows — model the error explicitly).
- Keep match arms small; lift complex arm bodies into named helpers so each arm
  reads as one intention.

## Arithmetic (see ARITHMETIC_POLICY.md)

- `a + b` means *checked*: overflow is a bug. Write `wrapping_*` when you intend
  modular arithmetic and `saturating_*` when you intend clamping — the spelling is
  the documentation. A reviewer should never need the build profile to know
  whether an operation wraps.
- `--report arithmetic` shows every site's class; use it to audit a module.

## Contracts and proof-bearing code

- Put `#[requires]`/`#[ensures]` directly above the function; keep clauses small
  and total. A precondition that is impossible to violate is a smell.
- Keep proof-carrying functions in `ProvableV1` shape: flat control flow, no
  nested loops, explicit modular arithmetic where the spec is modular (e.g.
  `wrapping_add` in SHA-256), so extraction and refinement stay stable.
- Name the spec and proof attributes explicitly (`#[spec(...)]`,
  `#[proof_by(...)]`); a formatting-only change must not alter fingerprints, and
  `fmt` guarantees that.

## Examples

- Every example under `examples/` must be classified in
  `scripts/tests/example_manifest.txt` in the same commit it is added, and a
  runnable one needs `@run` rules there (fixed inputs under the example
  directory, expected stdout in its `expected/`, exit code, interpreter mode) or
  an `@norun` with a reason.
- Examples are documentation: prefer clarity over cleverness, comment the
  *why*, and keep them runnable and gated.
- The named-example inventory, the promotion levels and the no-duplicate rule are in
  [EXAMPLES.md](EXAMPLES.md).

## Iteration protocol

Concrete has **no `Iterator` trait, no closures, and no trait-object iterator
protocol**. Traversal is expressed with a small hierarchy of explicit forms,
each of which keeps authority (capabilities) and allocation visible in the
signature. This section is the official story for which traversal form to reach
for, and it is the contract Phase 7 stdlib authors build against (see
[STDLIB_HANDOFF.md](../stdlib/STDLIB_HANDOFF.md), surface `iteration`).

The forms are gated by `scripts/tests/check_iteration_protocol.sh`.

### Why no `Iterator` trait

A trait-object `Iterator` (`dyn Iterator`) hides three things a reviewer of
Concrete code must be able to see: the **allocation** behind the iterator state,
the **dynamic dispatch** of `next`, and the **capabilities** the per-element
callback may use. Concrete's thesis is that those are visible at the call site.
So instead of a universal lazy protocol, traversal is a fixed set of concrete
forms, chosen by what you are traversing.

### The hierarchy

Reach for the **first** form that fits:

#### 1. `for` / indexed loops — simple bounded traversal

For counting and walking a fixed or index-addressable range, use the language
`for` loop. No library, no callback, no allocation, fully predictable:

```concrete pseudocode
for (let mut i = 0; i < n; i = i + 1) { ... }
```

This is the predictable-profile-friendly default (the loop bound is explicit;
see [PROFILES.md](../platform/PROFILES.md)).

#### 2. Cursor structs — parsers and readers

For positioned, bounds-checked consumption of a byte stream (parsers, binary
readers), use a **cursor struct** that owns a position and returns `Result` on
each step. The canonical one is `ByteCursor` (`std/src/numeric.con`): `peek_u8`,
`read_u8`, advancing reads, all bounds-checked. The cursor is a plain `Copy`
value with no hidden allocation; the caller sees every step.

#### 3. Capability-polymorphic `for_each` / `fold` / `map` — collections

For the standard collections (`Vec`, `Map`, `Set`, …), traversal is a method
that takes an explicit function value whose capability set is polymorphic:

```concrete pseudocode
pub fn for_each<cap C>(&self, f: fn(&T) with(C)) with(C)
pub fn fold<A, cap C>(&self, init: A, f: fn(A, &T) with(C) -> A) with(C) -> A
pub fn map<U, cap C>(&self, f: fn(&T) with(C) -> U) with(C, Alloc) -> Vec<U>
```

The `with(C)` on the method means "this traversal needs exactly the capabilities
the callback needs" — authority is threaded, not ambient. **Allocation is
visible in the signature**: `for_each` and `fold` carry no `Alloc`; `map`
allocates a new `Vec`, so its signature carries `Alloc`. A reviewer reads the
type and knows whether traversal can allocate.

#### 4. Explicit context threading — stateful callbacks

Because there are no closures, a callback that needs mutable state takes that
state as an explicit `&mut Ctx` parameter. Collections provide a `_ctx` variant:

```concrete pseudocode
pub fn for_each_ctx<Ctx, cap C>(&self, ctx: &mut Ctx, f: fn(&mut Ctx, &T) with(C)) with(C)
```

The accumulator/state is a named value the reviewer can see, not a captured
environment.

### What is deliberately excluded

- **No closures.** Callbacks are top-level/`fn` values; state is threaded via
  `_ctx`. (See [CALLABLE_VALUES_AND_CAPABILITIES.md](../language/CALLABLE_VALUES_AND_CAPABILITIES.md).)
- **No `Iterator` trait / no `dyn` trait-object iterators.** There is no lazy
  universal protocol; pick the concrete form above.
- **No hidden allocation.** Any traversal that allocates says so in its
  capability set (`Alloc`).

These exclusions are verifiable: the gate fails if an `Iterator` trait, a `dyn`
trait object, or a closure form appears, or if an allocating traversal hides its
`Alloc`.
