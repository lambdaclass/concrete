# Failure Strategy

Status: reference

This document defines Concrete's failure strategy: what can fail, how the language responds, what cleanup runs, what FFI must assume, and what proof-backed code may rely on. It is the authoritative reference for panic/abort/failure decisions.

For the general execution model, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For predictable-profile runtime boundaries, see [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md).
For proof semantics gaps, see [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md).

---

## Decision: Abort-Only, No Unwinding

Concrete uses **abort-only failure**. There is no panic, no stack unwinding, no catch, no exception mechanism. This is a permanent design commitment, not a temporary omission.

**Rationale:**

- Unwinding requires hidden control flow that the predictable profile cannot reason about
- Unwinding requires runtime support (personality functions, LSDA tables) that conflicts with freestanding targets
- Unwinding makes proof reasoning harder: every call site becomes a potential exit point
- Abort-only keeps the execution model simple enough to state on one page

**What this means:**

| Situation | Response | Cleanup runs? |
|-----------|----------|---------------|
| Normal return | Function returns value | Yes — defer runs LIFO |
| Error return via `?` | `Result::Err` propagated to caller | Yes — defer runs LIFO in each unwound frame |
| `break` / `continue` | Loop control flow | Yes — defer runs for exited scopes |
| Out-of-memory | terminal abort | **No** |
| Stack overflow | OS guard page → SIGSEGV | **No** |
| Hardware signal | OS kills process | **No** |
| `abort()` call | terminal abort | **No** |

The `?` operator is **not** unwinding. It is sugar for early return with a `Result::Err` value. Defer runs normally on `?`-triggered returns because they are normal returns from the caller's perspective.

---

## Failure Taxonomy

### 1. Explicit errors (normal control flow)

Errors are values. The `Result<T, E>` type and `?` operator handle all recoverable failures.

```
fn parse(input: &[u8]) -> Result<Header, ParseError> {
    let version = input[0];
    if version != 1 { return Err(ParseError::BadVersion); }
    // ...
}
```

Explicit errors:
- Are visible in function signatures
- Propagate via normal return (`?` is sugar, not magic)
- Do not skip defer — all deferred cleanup runs
- Are the **only** error mechanism in Concrete

### 2. Abort (terminal operation)

Abort never returns. No defer, explicit destruction, or generated drop glue
runs.

- In the current hosted profile, abort terminates the process and the OS
  reclaims process-owned resources.
- A future freestanding profile must call a declared non-returning target
  handler. The language does not assume an OS exists or that resources are
  reclaimed.

**Sources of abort:**

| Source | Trigger | Who calls abort | Reachable from predictable? |
|--------|---------|----------------|---------------------------|
| OOM | `malloc`/`realloc` returns null | `__concrete_check_oom` | No (no allocation) |
| User code | `abort()` intrinsic | User | No (requires Process capability) |
| Stdlib | Precondition violations in `std.alloc` | Stdlib wrappers | No (requires Alloc) |
| Checked integer operation | overflow, div/mod zero, invalid shift, `MIN / -1` | compiler-emitted trap helper | Yes unless statically discharged |
| Checked array index | negative or OOB index | compiler-emitted bounds helper | Yes unless statically discharged |

**Abort behavior:**

- Hosted execution calls libc `abort()`, which typically raises SIGABRT; its
  exit status is OS-dependent (often 134 on POSIX)
- Freestanding execution uses the target profile's non-returning abort handler
- No deferred expressions execute
- No `Destroy` implementations or generated drop glue run — `abort()` bypasses
  all cleanup. Concrete has explicit destruction (`impl Destroy`, `destroy(x)`,
  `x.drop()`, `defer x.drop()`) but no implicit scope-exit destructors, so there
  is nothing the compiler would have inserted on this path either.
- OS reclaims all process memory, file descriptors, etc.

### 3. Hardware traps (outside language model)

These terminate the process via OS signal. They are outside the language's semantic model.

| Trap | Cause | Signal |
|------|-------|--------|
| Null pointer dereference | Load/store through null pointer (only in trusted code) | SIGSEGV |
| Stack overflow | Call depth exceeds OS stack limit | SIGSEGV |
| Illegal instruction | Should not happen from correct codegen | SIGILL |

Checked arithmetic and safe indexing are not hardware-trap/UB cases: Concrete
inserts language-defined abort paths before executing an invalid operation.

### 4. Undefined behavior outside the safe surface

These require trusted/Unsafe operations or an incorrect foreign boundary; safe
ordinary arithmetic and indexing are not in this table.

| UB source | Consequence | Mitigation |
|-----------|-------------|-----------|
| Trusted unchecked pointer/index access | Reads/writes arbitrary memory | Keep the boundary named and audited |
| Dishonest extern signature/contract | ABI or memory corruption | FFI wrapper review and assumptions |
| Invalid raw-pointer lifetime/aliasing | use-after-free or alias violation | `trusted`/`Unsafe` containment |

---

## Cleanup Guarantees

### Defer runs on all normal exits

Deferred expressions run in LIFO order (last-deferred, first-executed) on:

- Normal `return`
- Error return via `?`
- `break` and `continue` (for scopes being exited)
- Implicit return at end of function body
- End of scope block (if/else, while body, borrow block)

### Defer does NOT run on abort or signals

When the process terminates via `abort()`, SIGSEGV, or any signal, deferred expressions are skipped entirely. This is by design:

- Abort means the situation is unrecoverable
- Signal handlers cannot safely run arbitrary user code
- The OS reclaims all process resources unconditionally

### No-leak guarantee for normal control flow

For functions that return normally (including error returns via `?`):

- All deferred expressions execute
- All linear values are consumed (enforced by checker)
- All borrow blocks have write-back (for mutable borrows)
- Stack frame is reclaimed by caller

For predictable code specifically:
- No heap allocation, so no heap to leak
- All data is on the stack
- Defer is the only cleanup mechanism needed

### Leak on abort

If a function aborts, resources held by that function and all callers receive no
language-level cleanup. Concrete has explicit `Destroy` implementations and
generated drop glue, but no unwinding or implicit scope-end destruction; none of
those explicit actions runs unless it executed before the abort.

For hosted programs this is acceptable because:
- The process is terminating and the OS reclaims process-owned resources
- Predictable code cannot reach abort (no allocation, no Process capability)
- The alternative (unwinding to run cleanup) would compromise the execution model

The second bullet is limited to capability-driven abort. Checked arithmetic and
bounds traps remain reachable in predictable code unless discharged. A
freestanding target cannot rely on OS reclamation and must state its terminal
handler/resource assumptions in the profile.

---

## FFI Failure Consequences

### What Concrete promises at the FFI boundary

- Linear values passed by-value to extern functions are consumed (ownership transferred)
- Reference parameters are borrowed for the duration of the call
- Return values from extern functions follow declared type (trusted, not verified)

### What Concrete does NOT promise

- No verification that extern functions actually follow their declared contract
- No cleanup if an extern function calls `longjmp`, `exit`, or triggers a signal
- No tracking of raw pointers (`*mut T`) obtained from extern functions
- No guarantee that extern functions are deterministic, pure, or predictable

### FFI and abort interaction

If an extern function calls `abort()` or triggers a signal:
- Concrete's defer does not run
- Linear values in scope are leaked to the OS
- There is no recovery path

If an extern function calls `longjmp`:
- Behavior is undefined
- Concrete does not emit setjmp/longjmp-safe code
- Defer does not run for skipped frames

### FFI and proof interaction

Proved functions cannot call extern functions (FFI gate in proof eligibility). Therefore:
- Proved code cannot be affected by FFI failure modes
- Proved code cannot leak resources to extern functions
- The FFI boundary is entirely outside the proof model

---

## Proof-Backed Code: Failure Assumptions

### What proved code may assume

1. **No capability-driven abort or OOM**: proof-eligible functions are
   authority-free and non-allocating. Checked arithmetic/bounds traps are a
   separate semantic obligation.
2. **No trusted hardware UB from safe operations**: proof-eligible functions
   exclude raw-pointer/FFI behavior; admitted checked operations may still abort
   outside the theorem's modeled domain.
3. **Normal control flow only**: proved functions return normally or via explicit `Result` error — no hidden exit paths
4. **Defer runs if present**: deferred expressions in proved functions always execute (no abort path exists)

### What proved code may NOT assume

1. **Trap-free fixed-width execution**: ordinary arithmetic traps when the
   machine-width result is invalid, while many ProofCore theorems use unbounded
   integers. The theorem needs an explicit range/no-overflow obligation to claim
   normal return for all runtime inputs.
2. **Static array bounds**: safe indexing traps rather than causing UB, but a
   functional theorem does not by itself prove the trap unreachable.
3. **Termination beyond the modeled evaluator/variant**: selected bounded loops
   are admitted, but proof status alone is not a general language termination
   theorem.
4. **No binary correspondence**: the proof is over PExpr (source-level IR), not compiled LLVM IR. Backend transformations are not formally verified.

### Gap summary for proved code

| Property | Proof model | Runtime reality | Gap |
|----------|------------|-----------------|-----|
| Integers | Mostly unbounded, with selected width-aware operators | Fixed-width checked or explicitly wrapping/saturating | Normal-return claim needs matching range/width semantics |
| Array access | Functional get/set for admitted forms | Runtime bounds trap | Trap-unreachability is a separate obligation |
| Control flow | Selected bounded loops and functional state forms | Compiled with LLVM optimizations | Backend not verified |
| Failure | Not modeled | abort/signal/UB possible in theory | Proved functions avoid all sources in practice |

---

## Summary of Commitments

1. **Abort-only**: no panic, no unwinding, no catch. This is permanent.
2. **Defer runs on normal paths**: every return, `?`, break, continue, scope exit.
3. **Defer skipped on abort/signal**: hosted execution terminates; freestanding
   invokes its non-returning handler. Neither promises language cleanup.
4. **No leak on normal paths**: linear ownership + defer guarantees cleanup.
5. **Abort cleanup is profile-scoped**: hosted OS reclamation is an environmental
   fact, not a language guarantee; freestanding targets must name their handler
   and resource assumptions.
6. **FFI is trust-based**: extern function contracts are not mechanically verified.
7. **Proved code avoids all failure sources**: by construction (no capabilities, no raw pointers, no allocation).
8. **Integer overflow and array OOB are known gaps**: documented, not yet mitigated at runtime.
