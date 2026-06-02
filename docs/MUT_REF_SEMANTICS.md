# `&mut T` Ownership and Consumption Semantics

Status: canonical reference for exclusive-reference behavior.

This document specifies exactly when `&mut T` references are consumed, when they are not, and how the checker enforces these rules. For the broader memory model, see [MEMORY_SEMANTICS.md](MEMORY_SEMANTICS.md).

---

## 1. Core Rule: `&mut T` Is Linear

`&mut T` is a non-Copy type. The checker tracks it as a linear variable subject to standard consumption machinery: `consumeVar` checks `isCopy` (false for `&mut T`), transitions the variable to `consumed`, and rejects further use.

`&T` (shared reference) is Copy. This document does not apply to `&T`.

---

## 2. Two Kinds of `&mut T` Variable

The checker distinguishes two kinds of `&mut T` variable:

**Borrow-block refs** — created by `borrow mut n as r in R { ... }`. These are scoped linear views into an alloca. They are tracked in `env.borrowRefs` and are consumed when passed to a function.

**Function parameter refs** — received as `fn foo(x: &mut T)`. These are reborrowable pointers. They can be used multiple times in function calls without consumption. Each call implicitly reborrows through the pointer.

This distinction exists because borrow-block refs are tied to a specific scope and alloca, while function parameter refs are incoming pointers with no scope to manage.

## 3. What Consumes a Borrow-Block `&mut T` Ref

| Operation | Consumes? | Mechanism |
|-----------|-----------|-----------|
| Pass `r` to a function taking `&mut T` | **Yes** | `consumeVar` via `borrowRefs` check |
| Return `r` from a function | **Yes** | Standard linear consumption |
| Rebind: `let r2 = r` | **Yes** | `r` is consumed; `r2` is the new owner |
| Method call with `&mut self` receiver (if `r` is in borrowRefs) | **Yes** | Receiver consumed |

After consumption, `r` is in `consumed` state. Any further use is a `variableUsedAfterMove` error.

## 4. What Does NOT Consume a Function Parameter `&mut T`

| Operation | Consumes? | Why |
|-----------|-----------|-----|
| Pass `tasks` to a function taking `&mut T` | **No** | Function param refs are reborrowable |
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

- **Reborrowing.** Creating a shorter-lived `&mut T` from an existing `&mut T` (e.g., passing `&mut *r` to a function that needs `&mut T` without consuming `r`). Currently not supported.
- **Field access through `&mut T`.** `r.field` auto-derefs for reads, but `r.field = val` (field write through a mutable reference) is not yet supported.
- **Array element access through `&mut T`.** `r[i]` and `r[i] = val` through a mutable reference are not yet supported.
- **`&mut T` in struct fields.** Storing a `&mut T` inside a struct and tracking its linearity through the struct is not yet addressed.

These gaps are tracked in [MUT_REF_CLOSURE.md](MUT_REF_CLOSURE.md).
