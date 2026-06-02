# Memory and Reference Semantics

Status: canonical reference — the single authority for Concrete's ownership, borrowing, and cleanup rules.

This document describes the memory and reference model that the checker (`Check.lean`) enforces for safe Concrete code. Where the checker and this document disagree, one of them must be corrected. Neither is silently authoritative over the other.

For the public guarantee boundary, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).
For value categories and layout, see [VALUE_MODEL.md](VALUE_MODEL.md).
For the broader safety model, see [SAFETY.md](SAFETY.md).

---

## 1. Variable Lifecycle

Every variable binding has a **state** that advances monotonically through a lifecycle:

```
unconsumed → used → consumed
                  ↗
unconsumed → reserved  (via defer)

unconsumed → frozen    (via borrow block, temporary)
```

| State | Meaning | Allowed operations |
|-------|---------|-------------------|
| `unconsumed` | Declared but never read or moved | Read, borrow, move, assign-through-ref |
| `used` | Read or borrowed at least once | Read, borrow, move |
| `consumed` | Moved by value to a call or binding | Nothing — use-after-move error |
| `reserved` | Scheduled for deferred cleanup | Read only — cannot move |
| `frozen` | Inside a borrow block that borrows this variable | Nothing — frozen-by-borrow error |

**Copy types** skip lifecycle tracking entirely. Using a Copy value never changes its state.

---

## 2. Copy vs Linear

### Copy types (non-linear)

These types can be used any number of times and reassigned freely:

- All integer types: `Int`, `Uint`, `i8`, `i16`, `i32`, `u8`, `u16`, `u32`
- `Bool`, `Float64`, `Float32`, `Char`, `()`
- Shared references: `&T`
- Raw pointers: `*mut T`, `*const T`
- Function pointers: `fn(...) -> T`
- Structs/enums with `Copy` marker
- Newtypes wrapping a Copy inner type
- Arrays of Copy element types
- Type parameters with a `Copy` bound

### Linear types

All other types are linear. Linear values must be consumed exactly once before scope exit:

- Structs and enums (default)
- `String`
- `&mut T` (exclusive reference — see [MUT_REF_SEMANTICS.md](MUT_REF_SEMANTICS.md) for consumption rules)
- `Heap<T>`, `HeapArray<T>`
- Newtypes wrapping a linear inner type

**Linear variables cannot be reassigned.** Attempting to assign to a linear variable is always an error, regardless of state. One binding, one resource. Use a new `let` binding instead.

---

## 3. Consumption Rules

**Consuming** a value means transferring ownership by passing it as a by-value argument to a function call, returning it, or (for `Heap<T>`) dereferencing with `*`.

### What consumes a linear value

- Passing as a by-value function argument
- Returning from a function
- Dereferencing `Heap<T>` with `*` (loads value from heap, frees memory)
- Passing to `destroy(x)` or `free(x)`
- Pattern-matching a newtype with `.0` extraction

### What does NOT consume

- Field access with `.` (reads the field, does not move the struct)
- Heap field access with `->` (reads through the heap pointer)
- Borrowing with `&x` or `&mut x`
- Reading a Copy value

### Consumption errors

| Error | Condition |
|-------|-----------|
| `variableUsedAfterMove` | Variable already in `consumed` state |
| `variableReservedByDefer` | Variable in `reserved` state (scheduled for cleanup) |
| `variableFrozenByBorrow` | Variable in `frozen` state (inside active borrow block) |
| `cannotConsumeLinearInLoop` | Linear variable declared at a lower loop depth than current |
| `cannotMoveLinearBorrowed` | Active borrow references exist for this variable |
| `linearVariableNeverConsumed` | Scope exit with linear variable not consumed or reserved |

---

## 4. Borrow Rules

Concrete uses **scoped borrow blocks** rather than Rust-style lifetime inference. A borrow block creates a named reference bound to a named region:

```concrete
borrow owner as ref in Region {
    // ref is available here; owner is frozen
}
// owner is unfrozen; ref no longer exists

borrow mut owner as ref in Region {
    // ref is &mut T; owner is frozen
}
```

### Borrow block semantics

1. **Freeze the owner.** The owner variable's state becomes `frozen` for the duration of the block. No reads, writes, moves, or borrows of the owner are permitted inside the block.

2. **Create the reference.** A new variable `ref` is added with type `&T` (shared) or `&mut T` (exclusive), depending on `is_mut`.

3. **Check the body.** The block body is checked with the owner frozen and the reference in scope.

4. **Unfreeze.** On block exit, the owner's state is restored to its pre-borrow state. The reference binding is removed. The reference is also removed from the escape-analysis list.

### Borrow conflict rules

| Attempting | Owner already has | Result |
|-----------|-------------------|--------|
| Shared borrow (`&`) | No active borrow block | Allowed |
| Shared borrow (`&`) | Owner already frozen by any borrow block | **Error**: `variableFrozenByBorrow` |
| Mutable borrow (`&mut`) | No active borrow block | Allowed (owner must be `mut`) |
| Mutable borrow (`&mut`) | Owner already frozen by any borrow block | **Error**: `variableFrozenByBorrow` |
| Any borrow | Owner is `consumed` | **Error**: `cannotBorrowMoved` |
| Mutable borrow | Owner is not `mut` | **Error**: `cannotMutBorrowImmutable` |

Because borrow blocks freeze the owner for the entire duration of the block, Concrete does not currently support overlapping borrows of the same owner, even shared-with-shared. The checker uses a whole-owner freeze model rather than a borrow-count model for nested borrow-block composition.

### Borrow escape prevention

If a reference created by a borrow block is assigned to a variable outside the block, the checker rejects it with `referenceEscapesBorrowBlock`. The escape check works by tracking all borrow-created reference names and checking assignments.

### What borrows do NOT do

- Borrows do not consume the owner (the owner is frozen, not consumed).
- The reference type `&T` is Copy — it can be passed to multiple functions within the borrow block.
- The reference type `&mut T` is linear — see [MUT_REF_SEMANTICS.md](MUT_REF_SEMANTICS.md) for the two-kind consumption model (borrow-block refs vs function parameter refs).
- Borrow blocks do not nest into the same owner (the inner borrow sees the owner as frozen).

---

## 5. Field Access

### Stack struct field access (`.`)

Field access on a struct does **not** consume the struct. The struct remains in its current state. The returned value has the field's declared type.

Auto-dereference: if the expression has type `&T` or `&mut T`, the checker resolves `.field` through the reference to the inner struct's field.

### Heap struct field access (`->`)

Arrow access on `Heap<T>`, `HeapArray<T>`, `&Heap<T>`, or `&mut Heap<T>` reads a field from the heap-allocated struct. The heap pointer is **not consumed** — only `*` (full dereference) or `free(x)` consumes it.

### Heap field assignment (`->` on the left side)

Arrow assignment writes to a field of a heap-allocated struct. The heap pointer must be in scope and not frozen. The assigned value must match the field's type.

### Newtype unwrap (`.0`)

Accessing `.0` on a newtype **consumes** the newtype (extracts the inner value).

### Current limitation: no partial borrows

The checker does not track field-level borrow granularity. When an owner is frozen by a borrow block, the **entire** owner is frozen — individual fields cannot be separately borrowed. This means:

```concrete
// Borrowing point freezes it entirely, even though the fields are disjoint:
borrow point as ref in R {
    // point is entirely frozen here
    // ref is &Point, not &point.x — cannot borrow fields individually
}
```

This is a deliberate conservative choice. The checker does not split struct ownership into per-field ownership slots. Field-granular borrows would require tracking which fields are frozen vs available, which is future work.

---

## 6. Array and Slice Semantics

### Array indexing

Array indexing (`arr[i]`) returns the element type. The checker does **not** track per-element ownership or borrows. The array as a whole follows normal ownership rules.

### Current limitation: no per-element borrows

The checker treats an array as a single linear value. There is no mechanism to borrow one element while leaving others available. Borrowing an array freezes the entire array.

This means:

```concrete
// Cannot borrow arr[0] and arr[1] separately
borrow(arr, ref, r, false) {
    // entire arr is frozen
}
```

Per-element borrow tracking is future work that would require either:
- Index-based tracking (knowing which elements are borrowed at which indices)
- Slice/subarray splitting (producing disjoint borrows by splitting the array)

---

## 7. Control-Flow Joins

### If/else branches

Both branches of an if/else must **agree** on the consumption state of every pre-existing linear variable:

- If the then-branch consumes a variable, the else-branch must also consume it.
- If neither consumes it, the merged state is the most-progressed of the two (`used` > `unconsumed`).
- Disagreement is an error: `linearConsumedOneBranchNotOther`.

### If without else

An if-without-else cannot consume any pre-existing linear variable in the then-branch, because there is no else-branch to match. Error: `linearConsumedNoBranch`.

### Match arms

All match arms must agree on consumption of pre-existing variables, following the same rule as if/else. The first arm establishes the consumption pattern; subsequent arms must match. Disagreement is an error: `matchConsumptionDisagreement`.

After the match, the final state from the first arm is applied (since all arms agree, any arm's state would be equivalent).

### While loops

- The loop body runs at an incremented loop depth.
- Linear variables declared at a lower loop depth cannot be consumed inside the loop body (`cannotConsumeLinearInLoop`).
- Linear variables declared inside the loop body follow normal scope rules within each iteration.
- Break and continue must not skip unconsumed linear variables declared at the current loop depth.

### Break and continue

Before executing a break or continue:
- All linear variables declared at the current loop depth must be consumed or reserved.
- Skipping an unconsumed linear variable is an error: `breakSkipsUnconsumedLinear` / `continueSkipsUnconsumedLinear`.
- Break and continue are forbidden inside defer bodies.

---

## 8. Heap Ownership

`Heap<T>` is a linear owned pointer to a heap-allocated value of type `T`. `HeapArray<T>` is the same for heap-allocated arrays.

### Allocation

```concrete
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc);
```

`alloc` consumes its argument (the value moves to the heap) and returns a `Heap<T>`. The `with(Alloc)` capability is required.

### Reading through a heap pointer

- `p->field` reads a field without consuming the pointer.
- `*p` (full dereference) **consumes** the `Heap<T>` — it loads the value from the heap and frees the memory. After `*p`, the heap pointer is in `consumed` state.

### Freeing

- `free(p)` consumes the `Heap<T>` and frees the memory.
- `destroy(p)` consumes the value and runs the type's `Destroy` implementation.

### Linearity prevents leaks

Because `Heap<T>` is linear, the checker enforces that every allocated heap pointer is either:
- Freed with `free(p)` or `destroy(p)`
- Consumed by `*p` (dereference-and-free)
- Passed to a function that takes ownership
- Scheduled for cleanup with `defer`

Forgetting a `Heap<T>` is a compile-time error (`linearVariableNeverConsumed`).

---

## 9. Cleanup and Defer

Concrete does **not** have implicit destructors (no RAII drop). Cleanup is explicit.

### `defer` semantics

```concrete
defer destroy(resource);
```

1. The body of `defer` must be a single function call expression.
2. Variables consumed by the deferred call are marked `reserved` instead of `consumed`.
3. A `reserved` variable can still be read but cannot be moved.
4. At scope exit, `reserved` variables are accepted (the deferred call will handle cleanup).
5. Defers execute in LIFO (reverse declaration) order at scope exit.

### Destroy

A type with `impl Destroy for T` provides a `destroy(x)` function that consumes the value and runs cleanup. The convention is `defer destroy(x)` immediately after allocation.

### `reserved` state interaction

| Operation | On `reserved` variable | Result |
|-----------|----------------------|--------|
| Read (field access, pass to `&T` borrow) | Allowed | State stays `reserved` |
| Move (pass by value) | **Error** | `variableReservedByDefer` |
| Borrow block owner | Allowed | Freezes as normal |
| Scope exit | Accepted | Deferred call runs |

---

## 10. Assignment Rules

### Copy variables

Copy variables can be reassigned freely if declared `mut`:

```concrete
let mut x: Int = 1;
x = 2;  // OK
```

### Linear variables

**Linear variables cannot be reassigned.** This is unconditional — regardless of whether the previous value has been consumed. Error: `assignOverwritesLinear`.

The rationale: reassigning a linear variable would either leak the old value (if not consumed) or create a confusing lifecycle where the same name refers to two different resources. Use a new `let` binding instead.

### Assignment guards

Even for Copy variables, assignment is blocked when:

| Guard | Error |
|-------|-------|
| Variable not declared `mut` | `assignToImmutable` |
| Variable frozen by borrow block | `assignToFrozen` |
| Active borrow references exist | `assignToBorrowed` |
| Assigning a borrow reference to an outer variable | `referenceEscapesBorrowBlock` |

---

## 11. Trusted Code and the Safe Boundary

The ownership and linearity rules apply uniformly in `trusted` code. `trusted` permits pointer arithmetic, raw pointer dereference, raw pointer assignment, and pointer-involving casts — but does **not** relax linearity, borrow checking, or scope-exit rules.

What `trusted` code can do that safe code cannot:

| Operation | Safe | Trusted |
|-----------|------|---------|
| Pointer arithmetic (`*mut T + offset`) | No | Yes |
| Raw pointer dereference (`*p` on `*mut T`) | No | Yes |
| Raw pointer assignment (`*p = value`) | No | Yes |
| Pointer-involving casts | No | Yes |
| Move a linear variable twice | No | **No** |
| Skip cleanup of a linear variable | No | **No** |
| Bypass borrow-block freeze | No | **No** |

This means the safe-memory guarantees (no use-after-move, no leak of linear values, no borrow conflict) hold even inside `trusted fn` bodies.

The boundary where the checker's guarantees stop is at **raw pointers**: once a value is behind a `*mut T` or `*const T`, the checker does not track its ownership. Raw pointers are Copy, and dereferencing them requires `trusted`. The soundness of the safe interface depends on the correctness of the trusted implementation.

---

## 12. What the Checker Does NOT Track

These are deliberate boundaries of the current model, not bugs:

### No field-granular borrows
The checker freezes entire values, not individual fields. Two simultaneous borrows of disjoint fields of the same struct are not supported.

### No per-element array borrows
The checker treats arrays as single values. There is no per-index ownership tracking.

### No lifetime inference
Borrows are scoped by explicit borrow blocks. There is no Rust-style NLL or region inference. References cannot outlive their borrow block.

### No automatic destruction
There is no implicit drop. Resources must be explicitly cleaned up via `destroy(x)` or `defer destroy(x)`.

### No interior mutability
There is no `Cell`/`RefCell`/`UnsafeCell` equivalent. Mutability flows from the binding (`let mut`) through borrows (`&mut T`).

### No closure captures
Function pointers are Copy and carry no captured environment. There are no closures and therefore no captured-variable ownership complications.

### No concurrency
The current model is single-threaded. There is no `Send`/`Sync` distinction, no atomics, no shared-memory concurrency. **Concurrency interaction is an explicit deferred boundary** — when concurrency is added, the ownership model will need to address thread-safety explicitly. The current rules are sound under the assumption of single-threaded execution.

### No cross-function borrow tracking
The checker does not track what a callee does with a borrowed reference. If a function takes `&T`, the checker trusts the signature — it does not verify that the callee respects the immutability of the reference. This is safe as long as the type system is sound (a `&T` parameter cannot be used to mutate the referent without `trusted` or `&mut`).

---

## 13. Hard Edge Cases and Their Current Status

### Field/substructure borrows

**Status: conservative — whole-value freeze.**

When a value is borrowed, the entire value is frozen. The checker does not distinguish between borrowing the whole struct and borrowing one field. This prevents all partial-borrow patterns:

```concrete
// Would be useful but is NOT supported:
struct Pair { a: String, b: String }
let mut p = Pair { a: owned_a, b: owned_b };
borrow(p.a, ref_a, r1, false) {  // ← not valid syntax
    consume(p.b);                  // ← p is entirely frozen
}
```

**Assessment:** The conservative choice is correct for now. Field-granular borrows would require per-field state tracking in `VarInfo`, syntax for field-path borrows, and rules for overlapping field paths. This is significant new complexity. The workaround is to structure code so that fields are moved into separate variables before borrowing.

### Array/slice element borrows

**Status: conservative — whole-array treatment.**

Arrays are treated as single linear values. There is no mechanism to borrow a sub-slice or individual element while keeping the rest of the array available.

**Assessment:** Per-element tracking is impractical without dependent types or compile-time index reasoning. The practical path is slice splitting (producing two disjoint borrowed slices from one array), which is future work.

### Control-flow joins

**Status: fully enforced.**

The branch-agreement rule is strict: both branches of an if/else must agree on consumption, match arms must agree, and if-without-else cannot consume. This prevents the "maybe consumed" state that would make resource tracking unsound.

**No known gaps.** The checker correctly handles:
- Nested if/else
- Match with arbitrary arm count
- While loops with break/continue
- Loop-depth consumption restriction

### Owner invalidation patterns

**Status: enforced within the safe model.**

The checker prevents all forms of owner invalidation while borrows are active:
- Move: `cannotMoveLinearBorrowed`
- Reassignment: `assignToBorrowed`
- Borrow-block freeze: `variableFrozenByBorrow`

The gap is at the `trusted` boundary: if trusted code holds a raw pointer derived from a reference, the checker cannot prevent the original owner from being invalidated after the borrow block ends but before the raw pointer is used. This is a fundamental limitation of the `trusted` escape hatch and is expected — `trusted` code carries audit responsibility.

**Arena/bulk-free patterns:** If an arena provides references to its allocations and then the arena is freed, the references become dangling. The checker cannot prevent this because the relationship between the arena and its allocations is not visible to the type system. This is currently an audit-level concern, not a checker-enforced guarantee.

### Concurrency interaction

**Status: explicitly deferred.**

The ownership model assumes single-threaded execution. When concurrency is added:

1. Linear ownership gives a strong starting point — a linear value has exactly one owner, which maps naturally to "one thread owns this."
2. Shared borrows (`&T`) are safe under single-threaded assumptions but would need `Send`/`Sync`-like constraints under concurrency.
3. `Heap<T>` ownership transfer between threads is sound as long as the transfer is linear (one thread gives up ownership, another gains it).
4. The `frozen` state during borrow blocks is thread-local — concurrent access to a frozen variable from another thread would violate the model.

The concurrency boundary is documented here as a known future interaction point, not a current gap.

---

## 14. Invariants the Checker Maintains

These invariants hold for all safe Concrete code that passes the checker:

1. **No use-after-move.** A linear variable in `consumed` state cannot be read, borrowed, or moved.
2. **No forgotten linear values.** Every linear variable is either consumed, reserved (with deferred cleanup), or an error is reported at scope exit.
3. **No borrow conflict.** A mutable borrow is exclusive; a shared borrow is incompatible with mutable borrows.
4. **No borrow escape.** References created by borrow blocks cannot be assigned to variables that outlive the block.
5. **No frozen-variable access.** A variable frozen by an active borrow block cannot be read, written, moved, or re-borrowed.
6. **No cross-loop consumption.** A linear variable from an outer scope cannot be consumed inside a loop body.
7. **No linear reassignment.** A linear variable cannot be reassigned, period.
8. **No skip-past-linear.** Break and continue cannot skip unconsumed linear variables in the current loop scope.
9. **No defer-body escape.** Break and continue are forbidden inside defer bodies.
10. **No assign-through-immutable.** Only `mut` variables can be assigned.
