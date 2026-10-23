# `&mut T` Ownership and Consumption Semantics

Status: canonical reference for exclusive-reference behavior.

This document specifies exactly when `&mut T` references are consumed, when they are not, and how the checker enforces these rules. For the broader memory model, see [MEMORY_SEMANTICS.md](MEMORY_SEMANTICS.md).

---

## 1. Core Rule: `&mut T` Is Linear

`&mut T` is a non-Copy type. The checker tracks it as a linear variable subject to standard consumption machinery: `consumeVar` checks `isCopy` (false for `&mut T`), transitions the variable to `consumed`, and rejects further use.

`&T` (shared reference) is Copy. This document does not apply to `&T`.

---

## 2. One Argument Mode for Every `&mut T` Binding (decision D1, implemented 2026-10-23)

Every `&mut T` binding, whatever made it (a function parameter, a borrow-block binder
`borrow mut n as r in R { .. }`, a match arm binder), is passed the same way: **passing it where a
reference is expected is an implicit reborrow**, and the binding stays usable after the call
([TIGHTENING_DECISIONS D1](TIGHTENING_DECISIONS.md#d1-mut-arguments-one-mode-q1-g9-design-examples-b1-and-b2)).
One function in the checker decides it for every call form (`consumeCallArgIdent`: free function,
method argument, static method, function pointer), and a `&mut self` receiver given a reference
binding is the same reborrow. An exclusive reference that is never used may lapse: it owns nothing.

Before D1 a borrow-block binder was consumed by the first call it was passed to (`inc(r); inc(r);`
inside a borrow block was E0205) while a parameter was reborrowed: two modes for one type.

What still holds every exclusive use apart:

- two exclusive arguments of ONE call that reborrow the same reference overlap (`two(r, r)`,
  E0293);
- a deferred call holds the loan of a `&mut` reference it was given until it runs: after
  `defer inc(r);` no use of `r` may come before the deferred call (E0206,
  `error_d1_defer_holds_loan*.con`);
- a live exclusive reborrow (`let s = &mut *r` where allowed) still freezes `r` (E0232).

There is no explicit reborrow syntax; the reborrow is implicit in the call. Making it an explicit
Core form (R-0496 / design example B5) is not done here: it would move every function's legacy V1
fingerprint, which is frozen, so it waits for the V2-only identities.

## 3. What Consumes a `&mut T` Ref

| Operation | Consumes? | Mechanism |
|-----------|-----------|-----------|
| Return `r` from a function | refused (REF-1) | references are never returned |
| Rebind: `let r2 = r` | refused (REF-6) | a `let` may not hold a reference |
| Pass `r` to a call taking `&mut T` | **No** | implicit reborrow (D1) |
| Method call with `&mut self` receiver | **No** | implicit reborrow (D1) |

## 4. What Does NOT Consume a `&mut T` Argument (any binding)

| Operation | Consumes? | Why |
|-----------|-----------|-----|
| Pass `tasks` to a function taking `&mut T` | **No** | Every `&mut` binding is reborrowed (D1) |
| Method call with `&mut self` on `tasks` | **No** | Same — implicit reborrow |
| Multiple sequential calls with `tasks` | **No** | Each call reborrows independently |

---

## 5. What Does NOT Consume Any `&mut T`

| Operation | Consumes? | Why |
|-----------|-----------|-----|
| Deref read: `*r` | **No** | Reads through the pointer; does not transfer ownership of the reference |
| Deref write: `*r = val` | **No** | Writes through the pointer; does not transfer ownership of the reference |
| Field access through ref: `r.field` | **No** | Auto-deref, reads through the pointer |

These operations use the reference without consuming it. The reference remains in `used` state (or stays `unconsumed` to `used` on first access).

---

## 6. Control Flow

### Branches

Both branches of an if/else or all arms of a match must agree on whether `r` is consumed. If one branch consumes `r`, the other must too. Disagreement is `linearConsumedOneBranchNotOther` or `matchConsumptionDisagreement`.

### Loops

If `r: &mut T` is from an outer scope, it cannot be consumed inside a loop body. Error: `cannotConsumeLinearInLoop`. Deref read/write inside a loop is fine (not consumption).

### Break/continue

Cannot skip past an unconsumed `r: &mut T` at the current loop depth.

---

## 7. Borrow Block Exit

When a `borrow mut` block ends:

1. The reference `r` is dropped from scope.
2. `r` does **not** need to have been consumed. Deref-only use (read/write through the pointer without passing `r` to a function) is the normal and valid usage pattern.
3. The write-back to the owner variable happens unconditionally at block exit (codegen loads from the alloca back into the owner's slot).
4. The owner is unfrozen and resumes its pre-borrow state.

This means a `borrow mut` block where `r` is only used for `*r` reads and `*r = val` writes is perfectly valid. The reference is created, used through the pointer, and dropped at block end. No consumption required.

---

## 8. Codegen Alignment

The codegen implementation for `borrow mut`:

1. Allocates an alloca for the borrowed value.
2. Stores the owner's current value into the alloca.
3. Creates a pointer (`r`) to the alloca.
4. At block exit, loads from the alloca and stores back into the owner's slot.

**Consumption is a checker concept, not a codegen concept.** The write-back happens regardless of whether the checker considers `r` consumed. If `r` was passed to a function (consumed), the function received the pointer and may have written through it — the write-back correctly propagates those changes. If `r` was only used for direct deref, the write-back correctly propagates those changes too.

---

## 9. Key Invariant

The checker does not have special-case logic for `&mut T`. It relies on:

- `isCopy` returning false for `&mut T` -- triggers linear tracking.
- `consumeVar` applying the standard linear consumption rules.
- Deref operations (`*r`, `*r = val`) not routing through `consumeVar`.

This means any future change to consumption behavior must preserve the invariant that deref does not consume, while pass-by-value and return do consume.

---

## 10. What This Does NOT Cover

These are known gaps, not yet implemented:

- **Explicit reborrow syntax (`&mut *r`).** There is no surface syntax for creating a shorter-lived `&mut T` from an existing `&mut T`. This matters only for **borrow-block refs**: a borrow-block ref is linear and is consumed when passed to a function (section 3), and there is no way to pass it on while retaining it. **Function parameter refs do not need this syntax** — they are implicitly reborrowed at every call site and are never consumed by calls (section 2 and section 4; the checker consumes a `&mut T` argument only when it is in `env.borrowRefs`). Implementing explicit `&mut *r` would require syntax support, a checker rule creating a temporary borrow of the pointee, and scope reasoning so the reborrow cannot outlive `r`.
- **Field access through `&mut T`.** `r.field` auto-derefs for reads, but `r.field = val` (field write through a mutable reference) is not yet supported.
- **Array element access through `&mut T`.** `r[i]` and `r[i] = val` through a mutable reference are not yet supported.
- **`&mut T` in struct fields.** Storing a `&mut T` inside a struct and tracking its linearity through the struct is not yet addressed.

These gaps are tracked in [MUT_REF_CLOSURE.md](MUT_REF_CLOSURE.md).
