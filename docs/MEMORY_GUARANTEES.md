# Memory Guarantees

Status: active reference

This document separates what the checker enforces today, what is safe to claim publicly, and what still needs closure. For the full operational semantics, see [MEMORY_SEMANTICS.md](MEMORY_SEMANTICS.md).

For the value/reference categories, see [VALUE_MODEL.md](VALUE_MODEL.md).
For the broader safety model, see [SAFETY.md](SAFETY.md).
For the precise public guarantee statement, see [GUARANTEE_STATEMENT.md](GUARANTEE_STATEMENT.md).

## What the Checker Enforces Today

These properties are mechanically enforced by `Check.lean` and verified by adversarial tests:

1. **No use-after-move.** A linear variable in `consumed` state cannot be read, borrowed, or moved. (Test: `error_memory_edge_use_after_move.con`)

2. **No forgotten linear values.** Every linear variable must be consumed or reserved (via `defer`) by scope exit. (Test: linearity tests across the suite)

3. **No borrow conflict.** Mutable borrows are exclusive; shared borrows are incompatible with mutable borrows. (Tests: `error_borrow_mut_conflict.con`, `error_double_mut_borrow.con`)

4. **No borrow escape.** References created by borrow blocks cannot be assigned to variables that outlive the block. (Tests: `error_borrow_escape.con`, `error_escape_return.con`, `error_escape_field.con`)

5. **No frozen-variable access.** A variable frozen by an active borrow block cannot be read, written, moved, or re-borrowed. (Test: `error_memory_edge_move_while_borrowed.con`)

6. **No cross-loop consumption.** A linear variable from an outer scope cannot be consumed inside a loop body. (Test: `error_memory_edge_loop_consume_outer.con`)

7. **No linear reassignment.** Linear variables cannot be reassigned. (Test: `error_memory_edge_linear_reassign.con`)

8. **Branch agreement.** If/else branches and match arms must agree on consumption of pre-existing linear variables. (Tests: `error_memory_edge_branch_disagree.con`, `error_memory_edge_if_no_else_consume.con`, `bug_int_match_disagree.con`)

9. **No skip past linear.** Break and continue cannot skip unconsumed linear variables. (Test: `error_break_linear_skip.con`)

10. **Trusted code does not relax linearity.** `trusted` permits pointer arithmetic and raw pointer operations but does not suppress ownership, borrow, or scope-exit rules.

11. **`&mut T` consumption tracking.** Exclusive references created in borrow blocks are linear and consumed on function call or return. Function parameter `&mut T` refs are reborrowable (not consumed on call). Deref read/write does not consume. Branch agreement and loop-consumption restrictions apply. (Tests: `adversarial_mut_ref_*.con`, `error_mut_ref_*.con`) See [MUT_REF_SEMANTICS.md](MUT_REF_SEMANTICS.md) for the full two-kind model.

## Public Claim That Is Safe Today

**For safe Concrete code (no `trusted`, no `with(Unsafe)`), the checker enforces:**

- No use of a value after it has been moved.
- No leak of a linear value — every owned resource is consumed or has deferred cleanup.
- No conflicting borrows — mutable access is exclusive, shared access precludes mutation.
- No dangling safe references — references cannot escape their borrow block.
- No silent reassignment of linear resources.
- No unchecked exclusive-reference aliasing — `&mut T` borrow-block refs are consumed on function call, preventing double-use.
- Deterministic cleanup ordering via `defer` (LIFO).

**What this rejects at compile time:**

| Bug class | Enforcement mechanism |
|-----------|----------------------|
| Use-after-free | Linear value consumed by `free`/`destroy` → subsequent use is use-after-move |
| Double free | Linear value consumed once → second free is use-after-move |
| Memory leak | Linear value forgotten → scope-exit error |
| Dangling reference | Borrow block scoping → escape analysis |
| Invalid aliasing through safe references | Mutable borrow is exclusive; owner is frozen during borrow |

## No-Codegen-Crash Rule

**Safe-subset ownership mistakes must be checker errors, never codegen crashes.**

A well-typed safe-subset program (no `trusted`, no `with(Unsafe)`) must either:
1. Compile successfully and produce correct code, or
2. Fail in a validated earlier phase (Parse, Resolve, Check, Elab, CoreCheck) with an explicit diagnostic.

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
- Every linear variable must be consumed or reserved (via `defer`) by scope exit. Forgetting a linear value is a compile-time error (`linearVariableNeverConsumed`).
- This covers `Heap<T>`, `HeapArray<T>`, `String`, `&mut T` (borrow-block refs), structs, enums, and all other linear types.
- `defer destroy(x)` schedules cleanup in LIFO order. The `reserved` state ensures the value cannot be moved away before cleanup runs.

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

- **Whole-value borrows only.** Borrowing a struct freezes the entire struct. Disjoint field borrows are not supported. This prevents some valid programs but never permits an invalid one.
- **Whole-array borrows only.** No per-element borrow tracking. Same rationale.
- **Scoped borrows only.** No Rust-style NLL or lifetime inference. References are confined to their borrow block. This is more restrictive than necessary but structurally sound.

### Outside the checker's reach

- **Raw pointers.** Once a value is behind `*mut T` / `*const T`, the checker does not track it. Raw pointer soundness is the responsibility of `trusted` code and is an audit concern.
- **Arena/bulk-free patterns.** If an arena is freed while references to its contents exist, the references dangle. The checker cannot see the arena-allocation relationship.
- **Cross-function reference use.** The checker trusts function signatures. If `fn foo(x: &T)` internally does something unsound with the reference (via `trusted`), the caller's checker does not detect it.
- **Concurrency.** The model assumes single-threaded execution. Shared-memory concurrency would require `Send`/`Sync`-like constraints that do not exist yet. This is an **explicitly deferred boundary**.

### Not yet proof-backed

The checker enforces the above properties, but:

- There is no formal proof of checker soundness.
- The guarantees are validated by adversarial tests and code review, not by a mechanized proof.
- The proof/evidence pipeline (ProofCore, obligations, diagnostics) does not yet cover memory model properties.

## Stronger Claim: Direction But Not Yet Justified

The goal is to eventually state:

**For safe Concrete code, there is no use-after-free, no double free, no dangling safe reference, and no invalid aliasing through safe references.**

This requires:

- ~~one checker-matching memory/reference semantics document~~ **done** — [MEMORY_SEMANTICS.md](MEMORY_SEMANTICS.md)
- ~~closure on hard edge cases~~ **done** — edge cases documented with status in MEMORY_SEMANTICS.md §13, adversarial tests for each, integer-match consumption bug fixed
- ~~`&mut T` ownership/consumption closure~~ **done** — two-kind model (borrow-block vs parameter refs), checker enforcement, codegen alignment, 21 adversarial tests, return-in-borrow codegen fix; see [MUT_REF_SEMANTICS.md](MUT_REF_SEMANTICS.md) and [MUT_REF_CLOSURE.md](MUT_REF_CLOSURE.md)
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
