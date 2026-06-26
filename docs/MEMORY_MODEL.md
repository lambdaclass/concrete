# Memory Model (for ordinary users)

Status: user-facing overview — ROADMAP Phase 6 #33.

This is the plain-language entry point to how Concrete manages memory: enough to
write and read everyday code without surprises. It is a narrative summary; the
precise, checker-aligned rules live in the reference docs linked throughout:

- [MEMORY_SEMANTICS.md](MEMORY_SEMANTICS.md) — the single authority for
  ownership, borrowing, lifecycle, and cleanup rules the checker enforces.
- [VALUE_MODEL.md](VALUE_MODEL.md) — value categories (Copy vs linear, refs, raw
  pointers, `Heap<T>`, function pointers).
- [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md) — what is enforced today, what is
  safe to claim, and where the boundaries are.
- [SAFETY.md](SAFETY.md) / [FFI.md](FFI.md) — capabilities, `trusted`/`Unsafe`,
  and the FFI boundary where assumptions become explicit.

## The headline invariants

Safe Concrete (no `Unsafe`, no FFI) holds these by construction:

1. **No uninitialized reads.** A binding cannot be declared without a value —
   `let x: T = e` is required; `let x: T;` is a *parse error*. There is no
   uninitialized state for a read to observe, so this needs no
   definite-assignment analysis; it is a grammar-level guarantee.
2. **No use-after-move.** Linear values are consumed when moved; using them
   afterward is a compile error.
3. **No aliasing through an exclusive borrow.** While a `&mut T` borrow is live,
   the owner is frozen; you cannot read or move it.
4. **No dangling references.** References are second-class — they are never
   returned from safe functions, directly or nested in an aggregate (see
   MEMORY_SEMANTICS "Borrow escape prevention").
5. **Defined cleanup.** Linear resources must be consumed or explicitly handed to
   cleanup (`defer`); leaks of tracked linear values are a compile error within
   the guarantee boundary.

## The mental model in one page

**Values, not references, are the default.** Primitives are `Copy` (cheap, no
tracking). Structs and enums are *linear* by default (move semantics); mark them
`Copy` only when that is sound. Passing a linear value by value **moves** it.

**Move / copy / drop.**
- *Move*: passing a linear value to a call or binding consumes it; the source can
  no longer be used.
- *Copy*: `Copy` values are duplicated freely; using one never changes its state.
- *Drop / cleanup*: a linear value must reach an end state — consumed by a call,
  returned, or scheduled with `defer`. There are no hidden destructors running
  arbitrary code; cleanup is explicit and visible.

**Borrows.** `&T` is a shared, immutable borrow (Copy). `&mut T` is an exclusive
borrow (linear) — while it is live the owner is *frozen*. Borrows are scoped and
cannot escape; you cannot stash a reference that outlives what it points at.

**Escape hatches.** `*const T`/`*mut T` raw pointers and FFI require the `Unsafe`
capability and a `trusted` boundary. There, and only there, can memory be
indeterminate or aliased outside the safe rules — and that becomes an **explicit,
audited assumption**, not silent behavior (see SAFETY.md / FFI.md).

## What is rejected (the common compile errors)

| You wrote | Why it is rejected |
|-----------|--------------------|
| `let x: T;` (no initializer) | uninitialized bindings do not exist (parse error) |
| use a linear value after passing it by value | use-after-move |
| read/move a value while it is `&mut`-borrowed | frozen-by-borrow |
| return a `&T`/`&mut T` (or an aggregate holding one) from a safe fn | references are second-class — cannot escape |
| drop a linear resource without consuming it | unconsumed-linear / would leak |
| dereference a raw pointer without `Unsafe` | missing capability |

## Where the guarantees stop

The invariants above are *safe-subset* guarantees. `trusted`/`Unsafe`/FFI code
carries its assumptions explicitly and they show up in audit. For the exact
enforced-vs-claimed boundary (including what is conservative or not yet
proof-backed), read [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
