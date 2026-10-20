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

### Definite initialization — no uninitialized reads by construction

There is no `unconsumed`-but-uninitialized state, because **a binding cannot be
declared without a value**. The grammar requires an initializer: `let x: T = e`
(and `let mut x: T = e`). A declaration with no initializer — `let x: T;` — is a
*parse error*, not a deferred-initialization form. Reassignment (`let mut x = e0;
x = e1;`) only ever replaces an already-valid value.

Consequently, safe Concrete has **no uninitialized reads, by construction** —
there is no uninitialized state for a read to observe, so this needs no
definite-assignment dataflow analysis; it is a grammar-level guarantee. (The
only path to indeterminate memory is through `Unsafe`/FFI, where it is an
explicit, audited assumption — see [SAFETY.md](SAFETY.md) and [FFI.md](../platform/FFI.md).)
Locked by `scripts/tests/check_memory_model.sh`.

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

**A live linear value cannot be overwritten.** A `mut` linear place may be
rebound only after its old value has been consumed; assigning while the old value
is live is E0219. This makes `acc = f(acc, x)` legal while preserving exactly one
live owner. One lexical name denotes one binding; a linear place contains at most
one live resource.

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

```concrete pseudocode
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

If a reference created by a borrow block is assigned to a variable outside the block, the checker rejects it with `referenceEscapesBorrowBlock`. The escape check works by tracking all borrow-created reference names and checking assignments. This is one case of the reference rules in §15 (REF-6), which also cover references made by `let` borrows.

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

```concrete pseudocode
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

```concrete pseudocode
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

```concrete pseudocode
let p: Heap<Point> = alloc(Point { x: 1.0, y: 2.0 }) with(Alloc);
```

`alloc` consumes its argument (the value moves to the heap) and returns a `Heap<T>`. The `with(Alloc)` capability is required.

### Reading through a heap pointer

- `p.field` on a heap shell auto-derefs and reads without consuming the pointer (spelled `p->field` pre-6D#3).
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

```concrete pseudocode
defer destroy(resource);
```

1. The body of `defer` must be a single function call expression.
2. Variables consumed by the deferred call are marked `reserved` instead of `consumed`.
3. A `reserved` variable can still be read but cannot be moved.
4. At scope exit, `reserved` variables are accepted (the deferred call will handle cleanup).
5. Defers execute in LIFO (reverse declaration) order at scope exit.

### Destroy

A type with `impl Destroy for T` provides a `destroy(x)` function that consumes the value and runs cleanup. The convention is `defer destroy(x)` immediately after allocation.

Concrete is linear, not affine: `Destroy` is never an implicit scope-end
destructor. A live non-`Copy` value that reaches scope exit without being
consumed or reserved by an explicit `defer` is an error. The compiler may
generate recursive drop glue only inside an explicit owner-consuming operation
such as `v.drop()` / `defer v.drop()`: the outer disposal point remains visible
in source, and the generated code is just the implementation of that explicit
disposal. Collection drop glue must also expose any capabilities required by
the element/key/value destructors in the drop path.

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

```concrete pseudocode
let mut x: Int = 1;
x = 2;  // OK
```

### Linear variables

**A linear variable cannot be overwritten while its current value is live.**
That case is `assignOverwritesLinear` (E0219), because replacing the value would
leak its ownership obligation. Once the old value has been consumed, a `mut`
binding may be assigned a new linear value; the rebound value becomes the
binding's new live obligation and must itself be consumed. This is the
fold/accumulate rule used by `acc = f(acc, x)`, including inside loops.

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
There is no implicit drop. Resources must be explicitly cleaned up via
`destroy(x)`, `x.drop()`, `defer destroy(x)`, or `defer x.drop()`. Generated
drop glue may recursively clean owned fields/elements only after one of those
explicit consuming operations has been written.

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

```concrete pseudocode
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
7. **No live linear overwrite.** A linear variable cannot be assigned while its
   current value is live; reassignment after consumption is a legal rebind.
8. **No skip-past-linear.** Break and continue cannot skip unconsumed linear variables in the current loop scope.
9. **No defer-body escape.** Break and continue are forbidden inside defer bodies.
10. **No assign-through-immutable.** Only `mut` variables can be assigned.

Invariant 4 is one case of the reference rules in §15, which state the whole second-class
discipline and its current enforcement.

---

## 15. Second-class references and lexical binding (normative; exit 2i)

**Status:** normative specification for ROADMAP exit 2i (owner R-0500, stage S4 of
[VERIFIED_PIPELINE.md](../compiler/VERIFIED_PIPELINE.md)), written 2026-10-21. This section is the
single normative statement of the reference and binding rules. The narrative in
[VALUE_MODEL.md](VALUE_MODEL.md) ("References are second-class"), the summary in
[MEMORY_MODEL.md](MEMORY_MODEL.md), and the decision records in
[LANGUAGE_INVARIANTS.md](LANGUAGE_INVARIANTS.md) §4 and [DECISIONS.md](../project/DECISIONS.md)
("No lexical value shadowing") explain the rules. Where they disagree with this section, this
section governs and the other text is corrected.

Every rule has a stable identifier and a version (`REF-3@1`). Its status is one of:

- **main**: enforced on `main` at `d967b4f2`;
- **land2**: enforced only on branch `land2-fix` at `74f7284a` (batch 2: the bug 091 normalized
  place rule and the bug 087 repair), which lands under exit 0b;
- **gap**: enforced on neither. §15.7 gives each gap an owner.

Statuses were read from the source at those SHAs and checked by compiling every rejected and
accepted example below with binaries built from them (2026-10-21).

Exit 2i closes over the **rejection rules** REF-1 to REF-8 and SCOPE-1 and the **semantic
guarantees** SCOPE-2 and SCOPE-3; SCOPE-4 is recorded but outside the exit. Each rejection rule
must be **main**, carry its versioned migration diagnostic (§15.5) and pass its reject and run rows
and its mutation rows (§15.6). Each semantic guarantee must be **main** and pass its run rows,
which compare the binary with `--interp` (and with the extracted model where a row says so; a row
the interpreter cannot run carries an explicit exemption with its reason, §15.6), and its
positive-control mutations (§15.6); it has no diagnostic of its own.

### 15.1 Definitions

- A **reference type** is `&T` or `&mut T`, including one spelled through a type alias. Raw
  pointers (`*const T`, `*mut T`) are not references; they are the trusted escape and are outside
  these rules.
- A type **holds a reference** when it is a reference type, or an array, `Heap`, heap array or
  generic instantiation one of whose element or argument types holds a reference (`tyStoresRef`
  in `Concrete/Check/CheckHelpers.lean`). A function-pointer type does not hold one (a
  `fn(&T) -> U` value stores no reference), and a raw pointer is not looked through. A named
  struct or enum can hold one only through its type arguments, because REF-3 forbids reference
  fields. A bare outer reference is looked through for bindings (`tyHoldsRefInAggregate`), so a
  binding of type `& &T` or `&[Int; 2]` holds no reference in an aggregate.
- A type **contains a reference** when a reference type occurs anywhere in it, including inside
  raw pointers, `Heap`, arrays, generic arguments and both the parameters and the result of a
  function type (`tyContainsRef` in `Concrete/Check/CheckHelpers.lean`). REF-1, REF-2 and REF-4
  use this wider predicate; REF-3 uses "holds".
- A **canonical place** is a binding followed by any number of field (`.f`), index (`[i]`) and
  newtype (`.0`) projections, where each dereference `*e` is of a reference or raw pointer whose
  own source is traced. Parentheses and identity dereferences normalize away: `(*r).f` and `r.f`
  are one place, and `&*r` names `r`'s source (`normPlace` / `normRef`, `land2-fix`, bug 091
  round 3). The **root** of a canonical place is its binding.
- A **reference binding** is a local (parameter, `let`, pattern binder or borrow-block binder)
  whose type is a reference type. Its **source** is the canonical place it was made from, recorded
  once per binding: root binding, projection path, and whether the path is exact. In the path
  every index projection is normalized to `[]` whatever the index (`normProjBase`, `land2-fix`),
  so all elements of one array are one place for provenance; this is a conservative conflict key
  for aliasing, and for lifetime it is exact, because every element of an array lives exactly as
  long as its root. A reference parameter's source is the caller's argument place, outside the
  callee. A reference binding may itself be borrowed (`let rr: & &Int = &r;`, accepted on both
  revisions): `rr` then denotes the local slot of `r`, a canonical place rooted at `r`.
- A binding is **visible** at a program point from its declaration to the end of the block that
  declares it, nested blocks included, whatever its state (live, used, consumed, reserved,
  frozen). A parameter is visible in the whole body; a match-arm, `if let` or `while let` binder
  in its arm or body; a `for` header binder in the header and the body; a borrow-block binder and
  region name in the block.
- A reference binding is **live** from its declaration to the end of its block, unless it is
  consumed first. There are no non-lexical lifetimes.

### 15.2 What the rules guarantee

The rules establish two invariants, which are the proof targets for S8 and V1:

- **INV-REF (references only flow down).** In safe code a reference value is held only by a local
  binding or a call argument of some activation `F`, and it denotes a canonical place whose root
  binding belongs to `F` or to an activation below `F` on the stack. Wherever the reference can be
  used, that root binding is in scope, is the binding the reference was made from, and has not
  been moved, consumed, freed or destroyed. No reference resides in an aggregate, a heap cell, a
  constant or the return channel. The only storage holding a reference is a local reference
  binding's own slot (or an argument slot during a call); a reference to such a slot
  (`& &T`) is allowed, but the inner reference can never be read out of it (REF-5) or written
  through it (REF-7), so a reference behind a reference is never used as a reference.
- **INV-SCOPE (one lexical name, one binding).** Within a function body a name denotes at most one
  visible local value binding at every program point, so a rule keyed by name is keyed by binding.

REF-1 to REF-8 imply INV-REF: a reference originates only from a canonical place (REF-5), so its
root is visible where it is made; it is never re-pointed (REF-6), so the root's scope encloses the
reference's; it never leaves its frame upward or into storage (REF-1 to REF-4, REF-7); and its
root stays the same live binding while it lives (REF-8; SCOPE-1 makes "the same binding" a
property of the name). SCOPE-1 implies INV-SCOPE. The scope argument also uses two facts the
compiler already gives: a callee cannot retain a reference argument (REF-1, REF-3, REF-7), and a
borrow-block reference cannot be copied or assigned into any other binding (E0234, REF-6). The rules marked "deliberate
restriction" are what keep INV-REF a lexical argument rather than a flow analysis (KNOWN_HOLES H28
on `land2-fix`); R-0499 may relax them as new rule versions.

### 15.3 Reference rules

Each rule gives the statement, examples, the diagnostic and its enforcing site, the fixtures that
pin it, and its status. Fixtures are under `tests/programs/`. A fixture in *italics* exists only on
`land2-fix`.

#### REF-1. No function returns a reference

No function, method or trait method, `trusted` included, has a return type that contains a
reference (§15.1: a reference anywhere, including inside a function type or behind a raw
pointer). The trusted escape is a raw pointer to the referent (`*const T` where `T` contains no
reference).

- Accepted: `fn get(v: &Vec<Int>, i: Int) -> Option<Int>` (a copy-out value); returning a raw
  pointer `*const T`.
- Rejected: `fn id(x: &i64) -> &i64`, `fn f() -> &i64 { return &local; }`,
  `fn f(x: &i64) -> Option<&i64>`, `fn f() -> fn(&i64) -> i64`, `fn f(x: *const &i64) -> *const &i64`.
- Diagnostic: front end, `checkFn` in `Concrete/Check/Check.lean` (`tyContainsRef` on the return
  type, "function '…' may not return a reference", **no code**); after monomorphization,
  `verifyNoReturnedRefs` in `Concrete/Check/Verify.lean` (E0236, `tyExposesRef`).
- Fixtures: `error_escape_return.con` (matched by message text in `run_tests.sh`);
  `scripts/tests/check_returned_ref_provenance.sh` legs `bare_ref_return`, `ref_to_local`,
  `aggregate_ref_return`.
- Status: **main** for function, method and trait-impl definitions. A trait method *declaration*
  (`trait T { fn get(&self) -> &Int; }`) is accepted on both revisions: `checkFn` visits
  definitions only and `verifyNoReturnedRefs` scans module functions (gap G13; no unsound program
  results, because every implementation is a definition and is rejected). Gap G2: the front-end
  rejection has no code, E0236 is missing from the
  `--report diagnostic-codes` ledger, and the post-mono predicate `tyExposesRef` is weaker than the
  front end's `tyContainsRef` (it does not look into function or raw-pointer types). The normative
  rule is the front end's: it is conservative and no program needs the other form. A returned
  function pointer whose parameters are references is rejected today; admitting it would be a
  REF-1@2 relaxation.

#### REF-2. No function type returns a reference

A function-pointer type whose return type contains a reference cannot be written, so no callback can
hand a borrowed element back (this keeps `with_value` sound). Parameters of reference type are
allowed.

- Accepted: `fn apply(x: &i64, f: fn(&i64) -> i64) -> i64`.
- Rejected: `fn apply(x: &i64, f: fn(&i64) -> &i64) -> i64`.
- Diagnostic: `resolveType` (`.fn_` case) in `Concrete/Check/CheckHelpers.lean`, "function type may
  not return a reference", **no code**.
- Fixture: `check_returned_ref_provenance.sh` leg `ref_returning_fn_type`.
- Status: **main**; no code (G2).

#### REF-3. No reference is stored

The only storage a reference may occupy is a local reference binding (and an argument slot for
the duration of a call). A reference may not be held in: a struct or enum field type, or a
newtype's inner type (each also through a type alias); a struct literal field, an enum payload or
an array literal element; a binding or parameter whose type holds a reference inside an array,
`Heap` or generic argument, also behind an outer `&`; or a module constant.

- Accepted: `struct H { f: fn(&Int) -> Int }`; a bare `let r: &Int = &x`; a reference parameter.
- Rejected: `struct W { t: &String }`; `Option::Some { value: &s }`; `[&a, &b]`;
  `let h: Heap<&Int> = alloc(&x)`; `fn f(v: &Vec<&Int>)`; `const A: [&Int; 1] = [&X]`;
  `const R: &Int = &X;` (G1).
- Diagnostic: E0239 (`refStoredInAggregate`). Sites: the declaration check in `checkModule`
  (fields, payloads and newtypes, through `expandAliasDeep`); the struct-literal, enum-literal and
  array-literal cases of `checkExpr`; `addVar` (`tyHoldsRefInAggregate`, every binding); `checkFn`
  (parameters), all in `Concrete/Check/`; and after monomorphization `verifyNoStoredRefs`
  (instantiated fields, function types, constants), run as `monoProgram (preTypeCheck :=
  verifyNoStoredRefs)` in `Concrete/Pipeline/Pipeline.lean`.
- Fixtures (E0239): `error_092_ref_struct_field.con`, `error_092_ref_in_option.con`,
  `error_092_ref_in_array.con`, `error_092_newtype_ref.con`, `error_092_alias_newtype_ref.con`,
  `error_092_alias_struct_field_ref.con`, `error_092_const_ref_array.con`,
  `error_092_phantom_ref_type_arg.con`, `error_escape_field.con`. Accepted:
  `regress_092_refs_flow_down.con`, `regress_092_type_param_shadows_ref_alias.con`.
- Status: **main** except G1: a constant whose type is a reference is accepted on both revisions,
  because `tyHoldsRefInAggregateV` looks through the outer reference. On `main`,
  `const X: Int = 42; const R: &Int = &X; fn get(r: &Int) -> Int { return *r; }` with
  `return get(R);` builds a binary that exits 0, while `--interp` reports `undefined variable 'R'`:
  silent wrong code in safe code. `land2-fix` refuses every use of `R` (E0238, "'R' is not a local
  binding") but still accepts the declaration.

#### REF-4. Generic instantiation preserves REF-1 and REF-3

Three separate conditions, each refusing an instantiation `T := A`:

- (a) `A` **contains** a reference and `T` occurs in the generic's return type;
- (b) after monomorphization, an instantiated field, binding, parameter, expression or constant
  type **holds** a reference (REF-3's predicate, so `Box<T>` at `A = &Int` is refused but at
  `A = fn(&Int) -> Int` is accepted: a function pointer stores no reference);
- (c) `A` **contains** a reference and `T` occurs behind a reference or pointer in one of the
  generic's parameter types, including inside a function type (`w: &W<T>`, `r: &T`,
  `p: *mut T`, `f: fn(&mut T)`; `tyParamOccursNested`), because the generic body is checked once
  at its opaque `T`. A top-level `x: T`, a by-value `W<T>` and a by-value callback parameter
  `f: fn(T) -> U` are not nested positions.

- Accepted: `fn ignore<T: Copy>(x: T, k: Int) -> Int` at `T = &Int`;
  `fn apply<T>(x: T, f: fn(T) -> Int) -> Int` at `T = &Int`; `Box<fn(&Int) -> Int>` holding a
  callback; *`trait_generic_fn_value_param.con`*.
- Rejected: `wrap::<&i64>(&x)` with `fn wrap<R>(r: R) -> R`; a `Box<T>` built at `T = &Int`;
  `forward::<&P>(w: &W<T>, ..)`; a generic passing its own opaque `T` on to a `&mut U` parameter
  (`relay` calling `update::<T>`).
- Diagnostic and site: (a) the call cases of `checkExpr` and `inferMethodParamAndRetTys`
  (`tyParamOccursIn` on the return type; "instantiated to a reference", **no code**), and E0236
  after monomorphization; (b) E0239, `verifyNoStoredRefs`; (c) E0238,
  `refuseNestedRefInstantiation` (`Concrete/Check/CheckHelpers.lean`) at every call form and
  `refuseNestedRefSpecialization` (`Concrete/IR/Mono.lean`) at every specialization.
- Fixtures: `check_returned_ref_provenance.sh` leg `generic_ref_in_return`;
  `error_092_generic_box_ref.con`, `error_092_generic_array_ref.con` (E0239);
  *`error_091_norm_generic_fn_mut_param.con`*, *`error_091_norm_generic_transitive_ref.con`* (E0238).
  *`error_091_norm_generic_nested_ref.con`* and *`error_091_norm_generic_nested_alias_ref.con`*
  were written for (c) but report E0239, because the `W<&P>` they build is refused as storage
  first; (c) has no fixture for a plain `r: &T` parameter (G7). The
  `r091_table` in *`run_tests.sh`* records the code each bug-091 fixture reports.
- Status: (a) and (b) **main**, (a) without a front-end code (G2); (c) **land2**.

#### REF-5. A reference originates only from a canonical place

A reference value is created only by: a borrow `&p` / `&mut p` of a canonical place `p`; a
reference parameter; a borrow-block binder; a copy, reborrow, projection or cast of a reference
binding whose source is traced (`let s = r`, `&*r`, `&mut *r`, `&r.f`, `r as &U`); or a pattern
binder that binds through such a reference. A reference may be used as the initializer of a `let`
of reference type, a call argument or receiver, the operand of `*`, a field or index base, or the
right-hand side of a reassignment REF-6 allows. Each of the following is refused as the source of
a reference:

- an `if` or `match` whose value is a reference (`let r = if c { &a } else { &b };`,
  `f(if c { &a } else { &b })`) — a deliberate restriction; the backend also miscompiles the form
  (bug 105);
- a borrow of a temporary, that is of any operand that is not a canonical place: a literal
  (`&42`), a call result (`&mk()`), an aggregate literal, an operator expression (`&(40 + 2)`),
  and an owned temporary as a `&mut self` receiver (`make().bump()`): bind the value to a `let`
  first. INV-REF's root argument needs every reference to have a binding root;
- a call whose result is a reference (REF-1 makes this unwritable for user functions; the rule
  covers intrinsics);
- a read of a reference out of storage: a field, element, heap cell or referenced place whose type
  is a reference (`*w.r`, `**r`, `(**r).f`).

The same normalized place rule also restricts ordinary value reads, which is a deliberate
restriction of its own (H28), not a reference rule: the base of a field access or index, and the
operand of `*`, must normalize to a canonical place or be a fresh owned temporary. So
`(if c { a } else { b }).f` is refused even when every branch yields an owned non-reference value
(E0238 on `land2-fix`, *`error_091_norm_if_projection.con`*; accepted on `main`), as are
`(**r).f` and `*(if c { &r } else { &r })`. The rewrite binds the branch result first:
`let w = if c { a } else { b }; w.f`. It belongs to REF-5's diagnostic (E0238) and version, and
R-0499 may lift it as REF-5@2.

- Accepted: `let r: &P = &w.p; f(r); f(&w.p); let s: &P = r;`.
- Diagnostic: E0238 (`untraceablePlace`). Sites (`land2-fix`, `Concrete/Check/Check.lean`):
  `checkExpr` (the single entry that refuses a reference-typed call result or storage read),
  `refuseRefValuedBranch`, `checkImplicitReceiverBorrow`, `checkRefArg` and `recordRefProvenance`,
  over `normPlace` / `normRef`.
- Fixtures (E0238): *`error_091_norm_if_projection.con`*, *`error_105_ref_valued_if_arg.con`*, *`error_105_ref_valued_if_field.con`*,
  *`error_091_norm_temporaries.con`*, *`error_091_norm_if_local_binding.con`*,
  *`error_091_norm_if_local_chain.con`*, *`error_091_norm_double_deref_read.con`*,
  *`error_091_norm_nested_ref_binder.con`*.
- Status: **land2**, except temporaries. On `main` a reference-valued `if` is accepted and its
  binary segfaults (bug 105), and every borrow of a temporary is accepted (the interpreter
  rejects `&42` and `&mk()` with "unsupported borrow target shape" while the binary runs them).
  `land2-fix` rejects an operator operand (`&(40 + 2)`) and a temporary `&mut self` receiver, but
  classifies literals, call results and aggregate literals as `.temp` (`normPlace`) and accepts
  borrowing them (`checkSharedAccess`, `recordRefProvenance` record no root): gap G12. The
  alternative to closing G12 by rejection is to specify a temporary as an anonymous local of the
  enclosing block and extend INV-REF to such roots, as REF-5@2.

#### REF-6. A reference binding is never re-pointed

A reassignment `r = e` of a reference binding is allowed only when `e`'s source has the same root,
the same normalized path (indices normalized to `[]`, §15.1) and the same exactness as `r`'s
recorded source. Any other reassignment is refused: provenance is one place per binding and is not
joined across branches. `r = &a[1]` after `let mut r = &a[0]` is therefore accepted (one array,
one lifetime), and `r = &b` is not. This is the rule
that keeps a reference out of any binding that outlives its source (design example B6, finding
G8): a reference binding declared in an outer block can only point at a place whose root was
visible at its declaration. A borrow-block reference binder is stricter: it may not be copied or
assigned into any other binding at all, inside the block or outside it, however the copy is
spelled; it may be passed as a call argument, dereferenced and projected. The E0234 checks test
for a bare identifier: `borrow x as r in R { let s: &Int = r; }` is E0234 on both revisions, but
`let s: &Int = (r);` is accepted on `main` and E0234 only on `land2-fix`, which checks each body
with parentheses removed (`unparenDeep`). A reborrow `&*r` is not an E0234 case on either
revision. On `land2-fix` it is a traced reference to the borrowed owner, so assigning it to an
outer binding is refused by the re-pointing rule unless that binding already borrowed the same
owner place, which is harmless because the owner outlives both; on `main`
`out = &*r;` inside the block is accepted whatever `out` pointed at (gap G14).

- Accepted: `let mut r: &P = &p; r = &p;`.
- Rejected: `let mut leaked: &Point = &p; if true { let q: Point = ..; leaked = &q; }` (bug 104,
  stack variant), its heap variant, `leaked = r2` where `r2` borrows a block local, and a reference
  parameter re-pointed at a local (`fn f(r: &mut Int) { let mut x = 1; r = &mut x; }`).
- Diagnostic: E0238 at the assignment, `recordRefProvenance (reassign := true)` (`land2-fix`);
  E0234 (`referenceEscapesBorrowBlock`) for a borrow-block reference, in the `let` and assignment
  cases of `checkStmt` (`main`).
- Fixtures: *`error_104_assigned_ref_heap_block.con`*, *`error_104_assigned_ref_stack_block.con`*,
  *`error_091_ref_repointed_in_branch.con`*, *`error_091_mut_receiver_reassigned_ref.con`* (E0238);
  `error_borrow_escape.con` (E0234).
- Status: **land2** for re-pointing; **main** for the borrow-block case only. On `main` both bug
  104 reproducers are accepted, and the stack variant exits 30 by reading a dead slot. On `main`
  re-pointing a `&mut` parameter is refused only as E0219 (the parameter is linear), and
  re-pointing a `&T` binding that started from a parameter at a block local is accepted and reads
  the dead slot (`fn f(r: &P) -> Int { let mut out: &P = r; if true { let q: P = ..; out = &q; }
  return out.n; }` returns `q`'s value).
  A deliberate restriction: R-0499's flow-sensitive loans may admit re-pointing at a place that
  outlives the binding, as REF-6@2.

#### REF-7. No reference is written through a reference

An assignment whose target lies behind a reference and whose value type holds a reference
(`*slot = &local` with `slot: &mut &T`) is refused: the borrow would land in storage the writer
does not own and leave the caller holding a reference to the callee's local.

- Rejected: `fn set(slot: &mut & Int) { let local: Int = 7; *slot = &local; }`.
- Diagnostic: E0238, the deref-assignment case of `checkStmt` (`land2-fix`, bug 091 integration
  review).
- Fixture: *`error_091_norm_ref_write_through_ref.con`*.
- Status: **land2**. On `main` the program above, called from `main`, compiles and segfaults.

#### REF-8. A reference's source stays the same live binding

While a reference binding is live, its source root may not be moved or consumed (E0204,
`consumeVar`), accessed while a borrow block freezes it (E0203), mutably borrowed or written while
a shared reference to it is live or used while an exclusive one is (E0231 / E0232), or shadowed by
a new binding of the same name (E0238, `guardShadowedSource`, `land2-fix`, bug 107). A reference's
own uses obey the same exclusivity through its ancestry of reborrows and copies (`refParent`,
`land2-fix`).

Borrows that last only for one call obey the same exclusivity without any binding: the borrows a
single call makes (each `&p` / `&mut p` argument, an implicit `&self` / `&mut self` receiver, and,
on `land2-fix`, reference arguments by their normalized place) may not overlap when at least one
is exclusive. Two borrows overlap when they have the same root and one projection path is a prefix
of the other (`x` and `x.f`; distinct fields are disjoint; every index is the step `[]`, so two
indexings of one array overlap). The diagnostic is E0293 (`conflictingCallBorrows`), from
`checkCallBorrowConflicts` over `borrowPathOf` / `pathsOverlap` in
`Concrete/Check/CheckHelpers.lean`, called from the call, method-call and static-method-call cases
of `checkExpr` (**main**; `land2-fix` computes the keys from the normalized place, through
`argBorrowParts` and `receiverBorrowParts`). Fixtures: the
E0293 legs of `scripts/tests/check_callable_values.sh` (`neg_double_mut_borrow`,
`neg_shared_plus_mut_borrow`, `neg_receiver_ctx_alias`), *`error_091_norm_cast_arg_overlap.con`*,
*`error_091_mut_receiver_paren_borrow_arg.con`*.

- Rejected: `let r: &Heap<B> = &h; let s: &Heap<B> = r; free(h);` (E0204, `main`);
  `let x: Int = 1; let r: &Int = &x; let x: Int = 2;` (E0238 on `land2-fix`; on `main` the binary
  returns 2 where `--interp` returns 1).
- Fixtures: `error_borrow_frozen.con` (E0203), `error_named_ref_mut_conflict.con` (E0232);
  *`error_107_shadow_under_live_ref.con`*, *`error_107_shadow_in_block_under_live_ref.con`*,
  *`error_091_norm_assign_under_shared_ref.con`*, *`error_091_norm_copied_mut_ref.con`*.
- Status: moving, freeing, freezing and let-borrow conflicts **main**; shadowing and ancestry
  **land2**. Once SCOPE-1 lands it rejects the shadowing case first (E0301), and
  `guardShadowedSource` stays as defense in depth.

### 15.4 Lexical binding rules

#### SCOPE-1. One lexical name, one binding

A binder may not reuse the spelling of a local value binding visible at its declaration (§15.1).
Binders are: function parameters (two parameters of one function included), `let` binders, `let`
destructuring binders, match-arm payload and value binders, `if let` and `while let` binders,
`for` header binders, and borrow-block reference and region names. The visible binding's state and
type do not matter: `let s = transform(s)` is rejected although the right-hand side consumed `s`,
and a Copy binding is protected like a linear one. This is the rule ratified for R-0435 on
2026-07-25 and inherited by R-0500.

- Accepted: the shapes of SCOPE-2.
- Rejected: `let x: Int = 1; let x: Int = x + 41;`; `if a > 0 { let a: Int = 7; }` with a
  parameter `a`; `for (let mut i: Int = 0; ..)` under an outer `i`;
  `let value = 1; match o { Option::Some { value } => .. }`; `let P { v } = p;` under an outer `v`;
  `fn f(a: Int, a: Int)`; `borrow x as x in R { }`.
- Diagnostic: **E0301, proposed** (`shadowsVisibleBinding`; confirm the number is unused on every
  branch when it is allocated), with the earlier binding as a related span. It runs at binder
  introduction, before E0292, in Check: the `let` and destructuring cases of `checkStmt`, the
  parameters in `checkFn`, the pattern binders of `match`, `if let` and `while let`, the `for`
  header and `.borrowIn`. Elab's `bindLocal` alpha-renaming (bug 087, `land2-fix`) stays mandatory
  as defense in depth ([PRINCIPLES.md](../project/PRINCIPLES.md) #12).
- Partial enforcement on `main`: E0292 rejects shadowing a live non-Copy binding by `let` or
  destructuring (`checkNoLiveShadow`; `error_094_destructure_shadows_live.con`, the "live shadow"
  leg of `scripts/tests/check_linear_discard.sh`); E0214 and E0215 reject a borrow-block reference
  or region name that is already bound (`.borrowIn` in `checkStmt`; `error_borrow_shadow.con`; E0215
  has no fixture). `land2-fix` adds E0238 for a name a live reference borrows (REF-8).
- Status: **gap** (G3). Every other rejected shape above is accepted on both revisions. Duplicate
  parameter names pass the checker on both and then fail in `llvm-as` ("redefinition of
  argument"), a loud failure that also breaks LANGUAGE_INVARIANTS §19.
- Prerequisite (G4): the renamed pattern binder `Variant { value: inner }`, in `match`, `if let`,
  `while let` and destructuring `let`. It does not parse today ("expected identifier, got :").
  Without it a nested `Option` / `Result` match cannot be written under SCOPE-1, because both
  payload fields are named `value`.

#### SCOPE-2. What is not a collision

- Bindings in sibling scopes: the two branches of an `if`, consecutive blocks, sibling match arms,
  consecutive loops. Neither binding is visible from the other.
- A declaration in a loop body: it is one lexical binding however many times it executes.
- Assignment: `x = e` introduces no binding. A `mut` linear place may be assigned after its value
  is consumed (`acc = f(acc, x)`); assigning over a live linear value stays E0219.
- Names in other namespaces: field labels, type names, traits and modules.

Status: **main**; all accepted. *`regress_087_sibling_promoted.con`* and
*`regress_087_unpromoted_sibling.con`* pin sibling reuse of a name with promoted storage.

#### SCOPE-3. Blocks are lexical on every consumer

A binding declared in a block is not visible after it, and every consumer (Check, Elab, the
interpreter, Lower, extraction, evidence frames) resolves a name to the innermost visible binding.
Once SCOPE-1 lands only sibling reuse reaches the renaming path; until then programs that shadow
must at least mean the lexical thing.

- Status: **land2** (the bug 087 repair: `bindLocal`, `enterBlock` / `exitBlock` in
  `Concrete/Elab/Elab.lean`; fixtures *`regress_087_let_block_shadow.con`*,
  *`regress_087_sibling_promoted.con`*, *`regress_087_unpromoted_sibling.con`*,
  *`regress_087_borrow_shadow_uaf/`*). On `main`, shadowing inside a block, a loop body or a
  borrow-block body miscompiles: `let x: Int = 1; if true { let x: Int = 100; } return x + 41;`
  exits 141, and the `for`-header shadow above exits 4 where 42 is lexical.

#### SCOPE-4. Namespace boundary (open, outside exit 2i)

Whether a local may reuse the name of a top-level function, an import alias or an intrinsic is a
separate namespace decision that SCOPE-1 does not make (DECISIONS.md). Today it may: `fn value()`
and `let value: Int` coexist. Contract and specification binders (`result`, quantifier binders)
are not local value bindings.

### 15.5 Versioned migration diagnostics

**What exists.** The repository has no language edition or language-version mechanism.
`Concrete.toml` has `[package]`, `[policy]` and `[profile]` sections and no edition key, and a
standalone file has no manifest at all. The nearest precedent is the targeted diagnostics for
removed syntax, which carry no version (E0289 for `let _ = …;`, the `?` operator, `while` as an
expression). Every check diagnostic has a stable code (`CheckError.code`); `--report
diagnostic-codes` (`Concrete/Report/Report.lean`) is the code ledger, kept complete by
`scripts/tests/check_diagnostic_codes_complete.sh`. The `Diagnostic` structure
(`Concrete/Report/Diagnostic.lean`) already has `reason`, `nextAction`, `related` and `evidence`
fields, which the human and the JSON renderers both print.

**Definition.** A diagnostic that enforces rule `R` is a *versioned migration diagnostic* when:

1. it has a stable E-code, and the ledger row for that code names every rule it enforces with its
   version (`E0238: REF-4@1, REF-5@1, REF-6@1, REF-7@1, REF-8@1`);
2. it names the rule, its version and this section through the existing `evidence` field:
   `evidence = [("rule", "REF-6@1"), ("spec", "docs/language/MEMORY_SEMANTICS.md §15")]`, which
   the human renderer prints as `  rule: REF-6@1` and `  spec: …` lines and the JSON renderer as
   the `evidence` array (`Concrete/Report/Diagnostic.lean`, `Diagnostic.render` and its JSON
   counterpart; no renderer change is needed);
3. it gives the mechanical rewrite for the rejected shape in the existing `nextAction` field
   (human `  next: …`, JSON `next_action`), and for a binding conflict the earlier binding in
   `related`;
4. each rejected shape has a fixture asserting the code, the rule tag and a non-empty rewrite,
   in both renderings, paired with an accepted fixture that is the rewritten program (a migration
   pair).

**Versioning policy.** A rule's version increments whenever its accepted set changes, by
tightening or by relaxing. The table below is the record, and each change cites the commit that
changed the compiler. There is no edition switch and no opt-out: these are soundness and identity
rules, and a mode that kept the old behavior would keep the unsound programs compilable. The
corpus that breaks is the repository's own and is migrated in the same change. A relaxation (for
example R-0499 lifting an H28 restriction) is a new version whose formerly rejected fixtures become
accepted fixtures, with the diagnostic text updated.

| rule | version | code | since |
|---|---|---|---|
| REF-1 | 1 | E0236 (to be attached at the front end, G2) | H1 closure, 2026-06-13; post-mono re-assertion, class 6 |
| REF-2 | 1 | E0236 (proposed; no code today) | H1 closure, 2026-06-13 |
| REF-3 | 1 | E0239 | bug 092 (`df0215f0`; batch 4 follow-ups `fc34504e` to `7f4ae423`); bare-reference constants once G1 is repaired |
| REF-4 | 1 | E0236 (a), E0239 (b), E0238 (c) | (a) 2026-06-13; (b) batch 4; (c) batch 2 |
| REF-5 | 1 | E0238 | batch 2, landing under exit 0b |
| REF-6 | 1 | E0238; E0234 | batch 2; E0234 earlier |
| REF-7 | 1 | E0238 | batch 2 (`a4ceab04`) |
| REF-8 | 1 | E0204, E0203, E0231, E0232, E0293; E0238 | E0238 case batch 2 |
| SCOPE-1 | 1 | E0301 (proposed) | the change that closes G3 |
| SCOPE-3 | 1 | none: a semantics rule, pinned by fixtures comparing `--interp`, the binary and the extracted model | bug 087, batch 2 |

**Diagnostic text.** For SCOPE-1:

```text
src/main.con:7:31: error[check]: (E0301) 'value' is already bound in an enclosing scope; a new binding may not reuse a visible name
  <snippet and the related span of the earlier binding, src/main.con:3:9>
  next: give the new binding its own name; for a pattern field write `Variant { value: inner }`;
        to replace the value of a `mut` binding, assign instead (`value = ...`)
  rule: SCOPE-1@1
  spec: docs/language/MEMORY_SEMANTICS.md §15
```

The rewrites for the other rules: REF-1, "return a value or an owned view, use a scoped callback
(`with_value`), or return a raw pointer from trusted code"; REF-3, "store an owned value, an index
or a `ByteView` instead, or pass the reference down as an argument"; REF-5 (branch value), "borrow
inside each branch and use the reference there, or choose the value first and borrow a named
place"; REF-5 (temporary), "bind the value to a `let` and borrow that"; REF-6, "declare a new
reference binding for the new place (`let r2 = &q;`)"; REF-7, "pass the reference down as an
argument instead of storing it through `&mut &T`". The wording is the implementer's; the rule tag,
the rewrite and the related span are required.

### 15.6 Acceptance list

The implementer adds a manifest `tests/rules/exit_2i.tsv` (new) with one row per fixture and five
columns: `rule`; `fixture`; `expected`, either `reject:<code>:<rule@version>` or
`run:<exit code>`; `pair`, an identifier shared by a reject row and the run row of its rewritten
program (`-` when none); and `modes`, a comma list from `binary`, `interp`, `model` (the extracted
model of the named function agrees) and `fmt` (formatter round-trip). A gate
`scripts/tests/check_rules_2i.sh` (new, in the full gate set) runs it. For every reject row it
checks, in both the human and the JSON rendering, the code, the `rule` evidence with the exact
rule and version, the `spec` evidence, and a non-empty `next` / `next_action`; for a SCOPE-1 row,
also a related span. These diagnostic checks are mandatory and do not depend on `modes`, which
lists only the execution modes of run rows (`binary`, `interp`, `model`, `fmt`). For each run row
it runs every listed mode and requires the expected result from each. It fails
when a rejection rule (REF-1 to REF-8, SCOPE-1) has no reject row, no run row or a reject row
without a pair, when a run row of a semantic guarantee (SCOPE-2, SCOPE-3) lacks mode `interp`
without an `interp-exempt:<reason>` entry in `modes`, when a row names a missing fixture, or when the ledger's rule and version for a
code disagree with §15.5. Existing fixtures are referenced, not copied. Every new reject row in
the table below has a `pair` whose run row is its rewritten program; existing fixtures get one
when the manifest is written.
Rows naming `error_091_*`, `error_104_*`, `error_105_*`, `error_107_*`, `regress_087_*` or
`trait_generic_fn_value_param.con` need batch 2 on `main`.

| rule | fixture | expected |
|---|---|---|
| REF-1 | `error_escape_return.con` | reject E0236 REF-1@1 (today matched by text) |
| REF-1 | `error_2i_ref1_return_bare.con`, `error_2i_ref1_return_option.con`, `error_2i_ref1_return_fnptr_ref_param.con` (new; the last returns `fn(&Int) -> Int`) | reject E0236 REF-1@1 |
| REF-1 | `escape_return.con`, `regress_092_refs_flow_down.con` | run (their current expected values) |
| REF-2 | `error_2i_ref2_fn_type_returns_ref.con` (new) | reject E0236 REF-2@1 |
| REF-2 | `regress_2i_ref2_callback_takes_ref.con` (new; `apply(&x, g)`, `g: fn(&Int) -> Int`) | run 42 |
| REF-3 | the nine E0239 fixtures listed under REF-3 | reject E0239 REF-3@1 |
| REF-3 | `error_2i_ref3_const_bare_ref.con` (new; `const R: &Int = &X;`, G1) | reject E0239 REF-3@1 |
| REF-3 | `regress_2i_ref3_fnptr_field.con` (new; `struct H { f: fn(&Int) -> Int }`) | run 42 |
| REF-4 | `error_2i_ref4_generic_return.con` (new; `wrap::<&i64>`) | reject E0236 REF-4@1 |
| REF-4 | `error_092_generic_box_ref.con`, `error_092_generic_array_ref.con` | reject E0239 REF-4@1 |
| REF-4 | `error_091_norm_generic_fn_mut_param.con`, `error_091_norm_generic_transitive_ref.con` | reject E0238 REF-4@1 |
| REF-4 | `error_2i_ref4_nested_param_instantiation.con` (new; `fn peek<T>(r: &T) -> Int` called as `peek::<&P>(&rp)` with `rp: &P`: the argument type `& &P` holds no reference in an aggregate, so (c) is reached rather than E0239) | reject E0238 REF-4@1 |
| REF-3 | `error_091_norm_generic_nested_ref.con`, `error_091_norm_generic_nested_alias_ref.con` (their `W<&P>` value is refused as storage before the instantiation check is reached) | reject E0239 REF-3@1 |
| REF-4 | `trait_generic_fn_value_param.con`; `regress_2i_ref4_top_level_ref_arg.con` (new; `ignore::<&Int>` and `apply::<&Int>` above) | run |
| REF-5 | `error_105_ref_valued_if_arg.con`, `error_105_ref_valued_if_field.con`, `error_091_norm_temporaries.con`, `error_091_norm_if_local_binding.con`, `error_091_norm_double_deref_read.con`, `error_091_norm_nested_ref_binder.con` | reject E0238 REF-5@1 |
| REF-5 | `error_2i_ref5_borrow_temporary.con` (new; `&(40 + 2)`) | reject E0238 REF-5@1 |
| REF-5 | `regress_2i_ref5_canonical_places.con` (new; `&w.p`, `&a[i]`, `let s = r`, `&*r`) | run 42 |
| REF-6 | `error_104_assigned_ref_heap_block.con`, `error_104_assigned_ref_stack_block.con`, `error_091_ref_repointed_in_branch.con` | reject E0238 REF-6@1 |
| REF-6 | `error_2i_ref6_alias_into_outer.con` (new; `leaked = r2`), `error_2i_ref6_param_repointed.con` (new; `&T` and `&mut T` parameters) | reject E0238 REF-6@1 |
| REF-6 | `error_borrow_escape.con` | reject E0234 REF-6@1 |
| REF-6 | `regress_2i_ref6_same_source.con` (new) | run 42 |
| REF-7 | `error_091_norm_ref_write_through_ref.con` | reject E0238 REF-7@1 |
| REF-7 | `regress_2i_ref7_write_value_through_mut_ref.con` (new; `*slot = 42` with `slot: &mut Int`) | run 42 |
| REF-8 | `error_borrow_frozen.con` (E0203), `error_named_ref_mut_conflict.con` (E0232), `error_091_norm_copied_mut_ref.con` (E0231), `error_091_norm_assign_under_shared_ref.con` (E0232) | reject, code as given, REF-8@1 |
| REF-8 | `error_2i_ref8_free_while_aliased.con` (new; `free(h)` under `let s = r`) | reject E0204 REF-8@1 |
| REF-8 | `regress_2i_ref8_free_after_ref_block.con` (new; the reference lives in an inner block, `free(h)` after it) | run 42 |
| REF-8 | `error_2i_ref8_call_overlap.con` (new; `mix(&x, &mut x)`), `error_091_norm_cast_arg_overlap.con` | reject E0293 REF-8@1 |
| REF-8 | `regress_2i_ref8_call_disjoint.con` (new; `mix(&x, &mut y)`, `two(&mut w.a, &mut w.b)`) | run 42 |
| REF-4 | `regress_2i_ref4_box_callback.con` (new; `Box<fn(&Int) -> Int>`) | run 42 |
| REF-5 | `error_091_norm_if_projection.con` | reject E0238 REF-5@1 |
| REF-5 | `regress_2i_ref5_bind_branch_first.con` (new; `let w = if c { a } else { b }; w.f`) | run 42 |
| REF-6 | `error_2i_ref6_borrow_block_copy.con` (new; `borrow x as r in R { let s: &Int = r; }`), `error_2i_ref6_borrow_block_paren_copy.con` (new; `= (r)`) | reject E0234 REF-6@1 |
| REF-6 | `error_2i_ref6_borrow_block_reborrow_escape.con` (new; `out = &*r` where `out` borrowed another place) | reject E0238 REF-6@1 (G14 on `main`) |
| REF-6 | `regress_2i_ref6_reborrow_same_owner.con` (new; `out` borrowed `x`, then `out = &*r` inside `borrow x as r`) | run 42 |
| REF-5 | `error_2i_ref5_borrow_literal.con`, `error_2i_ref5_borrow_call_result.con`, `error_2i_ref5_borrow_aggregate_literal.con` (new; `&42`, `&mk()`, `&P { n: 1 }`) | reject E0238 REF-5@1 (G12) |
| REF-1 | `error_2i_ref1_trait_decl_returns_ref.con` (new; `trait T { fn get(&self) -> &Int; }`) | reject E0236 REF-1@1 (G13) |
| REF-6 | `regress_2i_ref6_index_same_array.con` (new; `r = &a[1]` after `&a[0]`) | run 42 |
| REF-8 / SCOPE-1 | `error_107_shadow_under_live_ref.con`, `error_107_shadow_in_block_under_live_ref.con` | reject E0301 SCOPE-1@1 once SCOPE-1 lands; E0238 REF-8@1 before |
| SCOPE-1 | `error_2i_scope1_same_block_let.con`, `error_2i_scope1_block_let.con`, `error_2i_scope1_param_in_block.con`, `error_2i_scope1_consuming_rebind.con`, `error_2i_scope1_for_header.con`, `error_2i_scope1_arm_binder.con`, `error_2i_scope1_if_let.con`, `error_2i_scope1_while_let.con`, `error_2i_scope1_destructure.con`, `error_2i_scope1_duplicate_params.con`, `error_2i_scope1_borrow_ref.con`, `error_2i_scope1_region_name.con` (all new) | reject E0301 SCOPE-1@1 |
| SCOPE-1 | `regress_2i_scope1_<shape>_renamed.con` (new), the migration twin of each fixture in the row above | run, binary = `--interp` |
| SCOPE-1 | `error_borrow_shadow.con` | reject E0301 SCOPE-1@1 (E0214 today) |
| SCOPE-1 | `error_093_shadowing_block_local_leak.con`, `error_093_shadowing_match_binder_leak.con`, `error_094_arm_binder_shadows_live.con`, `error_094_destructure_shadows_live.con`, `error_094_value_binder_shadows_live.con`, `error_094_borrow_block_shadow_leak.con`, `error_094_mut_ref_param_shadow_leak.con` | reject E0301 SCOPE-1@1 (E0208, E0209 or E0292 today) |
| SCOPE-1 | `regress_045_match_binder_shadow.con` | migrated to `Variant { value: inner }` (needs G4); run 42 |
| SCOPE-2 | `regress_2i_scope2_sibling_branches.con`, `regress_2i_scope2_sibling_arms.con`, `regress_2i_scope2_consecutive_loops.con`, `regress_2i_scope2_loop_body_let.con`, `regress_2i_scope2_assign_rebind.con` (new); `regress_087_sibling_promoted.con`, `regress_087_unpromoted_sibling.con` | run 42, binary = `--interp` |
| SCOPE-3 | `regress_087_let_block_shadow.con` | modes `binary,interp,model`: run 42 until SCOPE-1 lands; then reject E0301 SCOPE-1@1, paired with a renamed twin that runs 42 in the same modes |
| SCOPE-3 | `regress_087_borrow_shadow_uaf/` (a project fixture; success is exit 0) | modes `binary,interp` if the interpreter runs the project, otherwise `binary,interp-exempt:<the interpreter's refusal>`: run 0 until SCOPE-1 lands; then reject E0301 SCOPE-1@1, paired with a renamed twin that runs 0 in the same modes |
| G4 | `regress_2i_renamed_pattern_binder.con` (new; `match`, `if let`, `while let`, destructuring `let`) | run 42, binary = `--interp`, formatter round-trip |

The 092 and 093 reproducers stay rejected, the 093 ones by SCOPE-1 instead of E0208. 087's
shadowing reproducers become rejected by SCOPE-1, and their lexical-semantics controls survive as
renamed positive twins. Once SCOPE-1 lands, source programs can no longer reach the identity-keyed
checks bugs 093 and 094 repaired; they stay as defense in depth, and their mutation rows move to a
leg that disables E0301 or to a Core-level test (R-0498 decides).

**Generator coverage (S1).** The prevention harness (R-0491) generates, for each rule, the rejected
shapes above with the expected outcome "rejection with this code and rule tag", and their accepted
near misses with the outcome "value", compared between the interpreter and the binary. The
existing shadow shape of `fuzz_differential.py` (`k == 11`) splits into a rejection leg and a
renamed value leg, and the live-shadow case of `fuzz_linearity.py` expects E0301. Each feature key
(`ref.return`, `ref.fn_type`, `ref.store`, `ref.generic`, `ref.origin`, `ref.repoint`,
`ref.write_through`, `ref.owner_live`, `scope.binder.<kind>` for each SCOPE-1 binder kind,
`scope.sibling`) gets a minimum completed-comparison count in exit 2e's inventory. The proposed
starting minimum is 20 completed comparisons per key per qualification run; 2e owns the number.

**Mutations.** R-0498's claim manifest records one row per mutation below, each killed by a named
manifest row:

- for each rejection rule and each of its enforcing sites, deleting the check (the `throwCheck` at
  the site, or the `verifyNoStoredRefs` / `verifyNoReturnedRefs` call) is detected by a named gate
  leg. Where the site is the only layer for some reject row, the leg is that row turning into an
  acceptance. Where a second layer would still reject it (REF-1 and REF-4(a) at the front end and
  after monomorphization; REF-3 in Check and in `verifyNoStoredRefs`), the leg checks the layer
  directly: the diagnostic's `pass` (`check` or `post-mono`) for a fixture only that layer can see
  (a generic instantiated at a reference type is visible only after monomorphization), or a run
  with the other layer disabled by the mutation harness;
- for SCOPE-1, one mutation per binder kind (parameter, duplicate parameter, `let`, destructuring
  `let`, match-arm binder, `if let`, `while let`, `for` header, borrow-block reference, region
  name), each omitting that kind from the check and killed by that kind's reject row (inherited
  from R-0435);
- positive-control mutations, killed by run rows: SCOPE-1 extended to treat assignment as binding
  introduction (killed by `regress_2i_scope2_assign_rebind.con`), SCOPE-1 extended to sibling
  scopes (killed by the SCOPE-2 sibling rows);
- for SCOPE-3, the mutations bug 087's repair carries (removing `exitBlock`'s restoration of the
  visible variables, or `bindLocal`'s renaming) are killed by `regress_087_let_block_shadow.con`
  in modes `binary,interp,model` while shadowing is accepted; once SCOPE-1 makes shadowing
  unreachable from source they move, like the 093 and 094 rows, to a leg that disables E0301.

### 15.7 Gaps and owners

| gap | what is missing | proposed owner |
|---|---|---|
| G1 | REF-3: a constant of bare reference type is accepted on both revisions; on `main` its use is silent wrong code (exit 0 for 42) and the interpreter rejects it. A candidate defect, not yet filed | row 0, exit 0b: file the bug, reject the declaration with E0239 in `checkModule` and `verifyNoStoredRefs` |
| G2 | REF-1, REF-2, REF-4(a): the front-end rejections carry no code; E0236 is absent from the diagnostic-codes ledger, because the completeness gate scans `CheckError.lean` and `CoreCheck.lean` but not `Verify.lean`; post-mono `tyExposesRef` is weaker than `tyContainsRef` | R-0500: attach E0236 at `checkFn`, `resolveType` and the instantiation checks, extend the completeness gate to `Verify.lean`, align the two predicates |
| G3 | SCOPE-1 is unenforced: same-block, block, parameter, loop-header, arm, destructuring, `if let` and `while let` shadowing and `let s = t(s)` are accepted; duplicate parameters fail in `llvm-as` | R-0500, inheriting R-0435's slices 2 and 3 (the E0301 check and corpus migration; the inventory and mutation gates) |
| G4 | the renamed pattern binder `Variant { value: inner }` does not exist; it is G3's prerequisite | R-0500, R-0435's slice 1 (grammar, parser, formatter, every pattern position) |
| G5 | the **land2** parts of REF-4 to REF-8 and SCOPE-3 hold only on `land2-fix`; `main` accepts bugs 104, 105 and 107 and the write-through-reference shape, and miscompiles block shadowing (087) | exit 0b (batch 2 landing) |
| G6 | no rule has a versioned migration diagnostic (§15.5 items 1 to 4: the ledger's rule column, the `rule` evidence, the `migrate` text, the migration pairs) | R-0500, through the existing `Diagnostic.evidence` / `nextAction` fields and a rule column in `Report.lean` |
| G7 | no manifest or gate exists for these rules (`tests/rules/exit_2i.tsv`, `scripts/tests/check_rules_2i.sh`); E0215 has no fixture; REF-4(c) has no fixture for a plain `r: &T` parameter | R-0500, with R-0513 for the inventory |
| G8 | S1 generator coverage and minima for the feature keys of §15.6 | R-0491 (generators), R-0513 and exit 2e (minima) |
| G9 | `&mut` usage modes are inconsistent and partly unspecified: a borrow-block `&mut` is consumed when passed to a call (MUT_REF_SEMANTICS §3) yet may lapse unused; a parameter `&mut` is reborrowed at every call; a `let`-bound `&mut` (`let r: &mut Int = &mut x;`) is linear with no discharging use, since writing through it, passing it to a call, reborrowing it or moving it into another `let` all end in E0208 | an R-0500 decision with R-0511 (design examples B1, B2 and G1); not a 2i restriction, but the V1 model may not model a looser rule than the compiler enforces |
| G10 | a borrow block freezes its owner against reads (`borrow x as r in R { out = x; }` is E0203), while the design model lets shared borrows and owner reads coexist (B7) | an R-0500 decision; until it is made the model must not be cited as describing the current rule |
| G12 | REF-5: borrowing a literal, call result or aggregate literal is accepted on both revisions (only `&(40 + 2)` and temporary `&mut self` receivers are refused, on `land2-fix`); the interpreter refuses them, so this is also an interpreter/binary disagreement | R-0500: reject them (E0238), or specify temporaries as anonymous block locals as REF-5@2 |
| G13 | REF-1: a trait method declaration may declare a reference return type | R-0500: check trait signatures in Check (E0236) |
| G14 | REF-6 on `main`: E0234 sees only a bare identifier, so `(r)` copies a borrow-block reference out, and `out = &*r` re-points an outer reference at the borrowed owner | exit 0b (batch 2 closes both: `unparenDeep` and the re-pointing rule) |
| G11 | ROADMAP exit 2i's deliverable reads "none stored in an aggregate"; "strictly second-class" also covers REF-6 (never assigned or aliased into a binding that outlives its source; design example G8) | the roadmap owner (wording) |
