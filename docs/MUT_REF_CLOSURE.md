# `&mut T` Closure Checklist

Status: tracking document for completing `&mut T` semantics (roadmap item 9).

This checklist tracks what works, what needs fixing, what needs testing, and what remains open before the `&mut T` story can be considered closed.

---

## 1. Intended Semantics

See [MUT_REF_SEMANTICS.md](MUT_REF_SEMANTICS.md) for the full specification. Summary:

- `&mut T` is linear (non-Copy). Passing it to a function or returning it consumes it.
- Deref read (`*r`) and deref write (`*r = val`) do NOT consume.
- Control-flow branches must agree on consumption state.
- Cannot consume inside a loop if declared outside.
- At borrow block exit, the ref is dropped -- no consumption required. Write-back happens unconditionally.

---

## 2. Checker Behavior

### What works today

- `&mut T` tracked as linear via `isCopy` returning false.
- Borrow-block refs (tracked in `env.borrowRefs`) consumed on function call.
- Function parameter `&mut T` refs are reborrowable (not consumed on call).
- The distinction is enforced by checking `env.borrowRefs.contains varName` before consuming.
- Deref read/write does not trigger consumption.
- Borrow block exit correctly drops the ref without requiring consumption.
- Branch agreement enforced for `&mut T` refs.
- Loop consumption restriction applies to `&mut T` refs from outer scopes.

### Implemented fixes

- **Check.lean ~1300 (function call args):** Split `ref`/`refMut` handling. `&T` params skip consumption (Copy). `&mut T` params consume only if the argument is a borrow-block ref (in `borrowRefs`).
- **Check.lean ~1755 (method self):** `&self` skips consumption. `&mut self` consumes receiver only if the receiver is itself a `&mut T` ref AND is in `borrowRefs`. Auto-borrowed value types are not consumed.

---

## 3. Codegen Assumptions

- Borrow block creates an alloca, stores the owner's value, passes the pointer as `r`.
- Write-back at block exit loads from alloca, stores back to owner. This is unconditional.
- Functions receiving `&mut T` get a raw pointer. They read/write through it.
- Consumption is purely a checker concept. Codegen does not distinguish "consumed ref" from "unconsumed ref" -- the alloca and write-back work the same either way.

No codegen changes are expected for the current `&mut T` semantics. The checker is the only component that needs attention.

---

## 4. Adversarial Tests

Tests to write or verify exist:

- [ ] **Double call.** Pass `r: &mut T` to a function, then pass `r` again. Must error: use-after-move.
- [ ] **Call then deref.** Pass `r` to a function (consumed), then `*r`. Must error: use-after-move.
- [ ] **Deref then call.** `*r` (not consumption), then pass `r` to a function (consumption). Must succeed.
- [ ] **Deref then deref.** Multiple `*r` reads. Must succeed (no consumption).
- [ ] **Write then read.** `*r = 5`, then `let x = *r`. Must succeed.
- [ ] **Branch agreement.** If-else where one branch consumes `r` and the other does not. Must error.
- [ ] **Branch agreement (both consume).** If-else where both branches consume `r`. Must succeed.
- [ ] **Loop consumption.** `r` from outer scope, consumed inside loop body. Must error.
- [ ] **Loop deref.** `r` from outer scope, `*r` and `*r = val` inside loop body. Must succeed.
- [ ] **Method chain.** `r.method1()` where method1 takes `&mut self`. Must consume `r`.
- [ ] **Rebind.** `let r2 = r`, then use `r`. Must error: use-after-move.
- [ ] **Borrow block exit without consumption.** Only deref inside block, no function call. Must succeed.
- [ ] **Borrow block exit after consumption.** Pass `r` to a function inside block, block exits. Must succeed (ref consumed, write-back still happens).
- [ ] **Return from function.** `fn foo(r: &mut Int) -> &mut Int { return r; }`. Must consume `r`.
- [ ] **Break skipping unconsumed.** `r: &mut T` declared in loop, break without consuming. Must error.

---

## 5. Open Gaps

These features are not yet implemented. Each is a separate work item:

### Reborrowing

Creating `&mut *r` to pass a sub-borrow without consuming `r`. This would allow calling a function that takes `&mut T` while retaining ownership of the original `&mut T`. Requires:
- Syntax support for `&mut *r` or equivalent.
- Checker logic to create a temporary borrow of the pointee.
- Lifetime/scope reasoning to ensure the reborrow does not outlive `r`.

### Field access through `&mut T`

`r.field` auto-derefs for reads today. Missing:
- `r.field = val` (field write through mutable reference).
- Checker must verify mutability and type of the field.
- Codegen must emit GEP through the pointer.

### Array element access through `&mut T`

`r[i]` and `r[i] = val` through a mutable reference. Requires:
- Checker support for indexing through a reference type.
- Codegen: GEP for array element through pointer.

### `&mut T` in struct fields

Storing `&mut T` inside a struct. The struct becomes linear (it contains a linear field). The checker must track that consuming the struct consumes the `&mut T` inside it.

### Return inside borrow block (fixed)

Previously, `return` inside a `borrow mut` block caused an SSA verification error. The borrow write-back was emitted as dead code after the return, creating a duplicate block label that confused the verifier.

Fixed by:
1. Adding a `blockTerminated` flag to `LowerState`, set in `terminateBlock`, cleared in `startBlock`.
2. Skipping the borrow write-back when `blockTerminated` is true (dead code after early return).
3. Hoisting the borrow alloca to the entry block via `emitEntryAlloca` so it dominates all uses.

Tests: `adversarial_mut_ref_return_in_borrow.con`, `adversarial_mut_ref_return_deref_in_borrow.con`.

---

## 6. Public Claim Status

**MEMORY_GUARANTEES.md has been widened.** The `&mut T` closure is complete for the current scope.

Completed:
- MEMORY_GUARANTEES.md property 11 documents `&mut T` consumption tracking.
- Public claim includes exclusive-reference aliasing prevention.
- No-codegen-crash rule documented (safe-subset programs must compile or fail with checker diagnostic).
- No-leak guarantee boundary defined (strong no-leak for safe code; weaker audit for trusted/FFI/arena).
- Stronger-claim checklist updated with &mut T closure, no-codegen-crash, and no-leak boundary.

Remaining open items (section 5 gaps — future work, not blockers):
- Reborrowing (`&mut *r`) syntax and semantics.
- Field access/write through `&mut T`.
- Array element access through `&mut T`.
- `&mut T` in struct fields.
- These are documented as future work in MUT_REF_SEMANTICS.md and do not affect the current public claim.
