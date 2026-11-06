# Memory Guarantees

Moved from docs/language: the public safety claim and its boundaries, beside GUARANTEE_STATEMENT; the rules behind it are in docs/spec.

The rules behind these claims are in [docs/spec/memory.md](../spec/memory.md),
[ownership.md](../spec/ownership.md) and [references.md](../spec/references.md); this document
keeps what is safe to claim publicly and where the claim stops.

Status: active reference

This document separates what the checker enforces today, what is safe to claim publicly, and what still needs closure. For the full operational semantics, see [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md).

For the value/reference categories, see [VALUE_MODEL.md](../archive/design-records/VALUE_MODEL.md).
For the broader safety model, see [SAFETY.md](SAFETY.md).
For the precise public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).

## What the Checker Enforces Today

These properties are mechanically enforced by `Check.lean` and verified by adversarial tests. Each
is a rule of the spec:

1. **No use-after-move**: [OWN-1](../spec/ownership.md#OWN-1). (Test: `error_memory_edge_use_after_move.con`)
2. **No forgotten linear values**: [OWN-2](../spec/ownership.md#OWN-2), with `defer` reservation [OWN-12](../spec/ownership.md#OWN-12).
3. **No borrow conflict**: [REF-11](../spec/references.md#REF-11). (Tests: `error_borrow_mut_conflict.con`, `error_double_mut_borrow.con`)
4. **No borrow escape**: [REF-6](../spec/references.md#REF-6), [REF-3](../spec/references.md#REF-3). (Tests: `error_borrow_escape.con`, `error_escape_return.con`, `error_escape_field.con`)
5. **No frozen-variable access**: [REF-8](../spec/references.md#REF-8). (Test: `error_memory_edge_move_while_borrowed.con`)
6. **No cross-loop consumption**: [OWN-4](../spec/ownership.md#OWN-4). (Test: `error_memory_edge_loop_consume_outer.con`)
7. **No live linear overwrite**: [OWN-9](../spec/ownership.md#OWN-9). (Negative: `error_memory_edge_linear_reassign.con`; positive: `linear_rebind_after_drop.con`)
8. **Branch agreement**: [OWN-3](../spec/ownership.md#OWN-3). (Tests: `error_memory_edge_branch_disagree.con`, `error_memory_edge_if_no_else_consume.con`, `bug_int_match_disagree.con`)
9. **No skip past linear**: `break` and `continue` are exits under [OWN-2](../spec/ownership.md#OWN-2). (Test: `error_break_linear_skip.con`)
10. **Trusted code does not relax linearity**: [TRUST-12](../spec/trusted.md#TRUST-12).
11. **`&mut T` is lent per call**: every `&mut` binding, a borrow-block binder included, is lent to each call and not consumed by it ([REF-10](../spec/references.md#REF-10)); a dereference does not consume it. The older two-kind model (borrow-block refs consumed on call) is superseded.

## Public Claim That Is Safe Today

**For safe Concrete code (no `trusted`, no `with(Unsafe)`), the checker enforces:** no use after
a move ([OWN-1](../spec/ownership.md#OWN-1)); no leak of a linear value ([OWN-2](../spec/ownership.md#OWN-2)); no conflicting borrows
([REF-11](../spec/references.md#REF-11)); no dangling safe reference ([REF-6](../spec/references.md#REF-6), [REF-8](../spec/references.md#REF-8)); no
overwrite of a live linear value ([OWN-9](../spec/ownership.md#OWN-9)); no aliasing through `&mut`, which is lent per
call and exclusive within it ([REF-10](../spec/references.md#REF-10), [REF-11](../spec/references.md#REF-11)); and LIFO cleanup by
`defer` ([STMT-11](../spec/statements.md#STMT-11)). Together these are
[TRUST-2](../spec/trusted.md#TRUST-2)'s safe-code claim for memory.

**What this rejects at compile time:**

| Bug class | Enforcement mechanism |
|-----------|----------------------|
| Use-after-free | Linear value consumed by `free`/`destroy` → subsequent use is use-after-move |
| Double free | Linear value consumed once → second free is use-after-move |
| Memory leak | Linear value forgotten → scope-exit error |
| Dangling reference | Borrow block scoping → escape analysis |
| Invalid aliasing through safe references | Mutable borrow is exclusive; owner is frozen during borrow |

## No-Codegen-Crash Rule

**Safe-subset ownership mistakes must be checker errors, never codegen crashes.** This is
[PASS-12](../spec/pipeline.md#PASS-12): a program every rule accepts is compiled, and no stage after
Check refuses or crashes on it.
Checker/codegen mismatches — where the checker permits a program but codegen crashes or produces wrong code — are compiler bugs. The discovered `&mut T` return-inside-borrow-block crash (fixed in `3b9a895`) is the template: the checker accepted the program, but lowering emitted dead code that broke SSA verification. The fix was in codegen (skip dead write-back), not in the checker.

This rule applies to:
- Ownership/linearity state disagreements between checker and lowering
- Reference type mismatches (e.g., `i64` vs `ptr` for `&mut T`)
- Control-flow interactions with borrow blocks (early return, break, continue)
- Any path where the checker's "safe" judgment does not match codegen's ability to emit valid IR

Violations of this rule are high-priority bugs. New adversarial tests should target the checker/codegen boundary, not just the checker in isolation.

## No-Leak Guarantee Boundary

The checker enforces a **strong no-leak guarantee** for safe code within the following boundary:

**Strong no-leak (enforced at compile time):**
- Every owned linear binding is consumed before every exit of its scope ([OWN-2](../spec/ownership.md#OWN-2)), or
  reserved by a `defer` ([OWN-12](../spec/ownership.md#OWN-12)); forgetting one is a compile-time error.
- This covers `Heap<T>`, `HeapArray<T>`, `String`, structs, enums, and all other linear types.

**What the strong no-leak guarantee does NOT cover:**
- **Arena/bulk-free patterns.** If an arena allocates many objects and is freed as a unit, the individual objects are not tracked. The arena itself is linear (so it must be freed), but objects obtained from it may not be.
- **Raw pointer indirection.** A linear value behind `*mut T` is invisible to the checker. If `trusted` code stores a linear value through a raw pointer and never retrieves it, the checker cannot detect the leak.
- **FFI/host-call boundaries.** If a linear value is passed to a foreign function, the checker trusts the signature. If the foreign function leaks it, the checker cannot detect this.
- **Circular ownership.** If two linear values each hold ownership of the other (possible only through `trusted` code), neither can be consumed without consuming the other. The checker does not detect cycles.

**Weaker allocation/cleanup audit reporting (future):**
- `--report alloc` will attribute every user-visible allocation to a source location and call path.
- Leak-risk classification will distinguish strong-no-leak code from code that crosses into trusted/FFI/arena territory.
- This is audit-grade reporting, not a compile-time guarantee. It helps reviewers identify where leaks *could* happen outside the strong boundary.

The distinction matters: the strong no-leak claim is a compiler-enforced property of safe code. The audit reporting is a best-effort tool for code that touches the boundaries.

## Where the Safe Claim Has Boundaries

These are honest boundaries, not bugs:

### Conservative (safe but restrictive)

- **Whole-value borrows only.** A borrow of any part freezes the whole owner, struct or array
  ([REF-11](../spec/references.md#REF-11)). This prevents some valid programs but never permits an invalid one.
- **Scoped borrows only.** No lifetime inference: a reference lives for its borrow block
  ([REF-8](../spec/references.md#REF-8), [REF-D3](../spec/references.md#REF-D3)).

### Outside the checker's reach

- **Raw pointers.** Once a value is behind `*mut T` / `*const T`, the checker does not track it. Raw pointer soundness is the responsibility of `trusted` code and is an audit concern.
- **Arena/bulk-free patterns.** If an arena is freed while references to its contents exist, the references dangle. The checker cannot see the arena-allocation relationship.
- **Cross-function reference use.** The checker trusts function signatures. If `fn foo(x: &T)` internally does something unsound with the reference (via `trusted`), the caller's checker does not detect it.
- **Concurrency.** A program has one thread ([STMT-3](../spec/statements.md#STMT-3)). Shared-memory concurrency would require `Send`/`Sync`-like constraints that do not exist yet. This is an **explicitly deferred boundary**.

### Not yet proof-backed

The checker enforces the above properties, but:

- There is no formal proof of checker soundness.
- The guarantees are validated by adversarial tests and code review, not by a mechanized proof.
- The proof/evidence pipeline (ProofCore, obligations, diagnostics) does not yet cover memory model properties.

## Stronger Claim: Direction But Not Yet Justified

The goal is to eventually state:

**For safe Concrete code, there is no use-after-free, no double free, no dangling safe reference, and no invalid aliasing through safe references.**

This requires:

- ~~one checker-matching memory/reference semantics document~~ **done** — [MEMORY_SEMANTICS.md](../archive/design-records/MEMORY_SEMANTICS.md)
- ~~closure on hard edge cases~~ **done** — edge cases documented with status in MEMORY_SEMANTICS.md §13, adversarial tests for each, integer-match consumption bug fixed
- ~~`&mut T` ownership/consumption closure~~ **done** — first a two-kind model (borrow-block vs parameter refs), now every `&mut` binding lent per call ([REF-10](../spec/references.md#REF-10)); checker enforcement, codegen alignment, 21 adversarial tests, return-in-borrow codegen fix
- ~~no-codegen-crash regression rule~~ **done** — documented above; safe-subset programs must either compile or fail with an explicit checker diagnostic
- ~~no-leak guarantee boundary~~ **done** — strong no-leak for safe code defined above; weaker audit reporting deferred
- proof-facing articulation of the memory model (future: connect to ProofCore/obligations)
- formal checker soundness argument or mechanized proof (future)

## Why This Matters

Without this document, it is too easy to make either of two mistakes:

- understate the language by talking as if the checker does not already enforce a serious ownership model
- overstate the language by claiming the fully centralized safe-memory theorem before formal proof work is done

The right current position is:

**Concrete enforces a real ownership/borrow discipline with explicit cleanup and explicit trust boundaries. The enforced properties are documented, tested, and match the checker implementation. What remains is the proof-facing articulation that turns checker behavior into a formally backed guarantee.**
