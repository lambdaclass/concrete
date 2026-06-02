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
| Out-of-memory | `abort()` — process terminates | **No** |
| Stack overflow | OS guard page → SIGSEGV | **No** |
| Hardware signal | OS kills process | **No** |
| `abort()` call | Process terminates | **No** |

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

### 2. Abort (process termination)

Abort terminates the process immediately. No defer runs. No cleanup. The OS reclaims all resources.

**Sources of abort:**

| Source | Trigger | Who calls abort | Reachable from predictable? |
|--------|---------|----------------|---------------------------|
| OOM | `malloc`/`realloc` returns null | `__concrete_check_oom` | No (no allocation) |
| User code | `abort()` intrinsic | User | No (requires Process capability) |
| Stdlib | Precondition violations in `std.alloc` | Stdlib wrappers | No (requires Alloc) |

**Abort behavior:**

- Calls libc `abort()`, which typically raises SIGABRT
- Exit code is OS-dependent (typically 134 on POSIX)
- No deferred expressions execute
- No destructors run (there are no destructors)
- OS reclaims all process memory, file descriptors, etc.

### 3. Hardware traps (outside language model)

These terminate the process via OS signal. They are outside the language's semantic model.

| Trap | Cause | Signal |
|------|-------|--------|
| Null pointer dereference | Load/store through null pointer (only in trusted code) | SIGSEGV |
| Stack overflow | Call depth exceeds OS stack limit | SIGSEGV |
| Division by zero | Integer division by zero on x86 | SIGFPE |
| Illegal instruction | Should not happen from correct codegen | SIGILL |

### 4. Undefined behavior (semantic gap)

These produce undefined behavior. The compiler does not detect or trap them.

| UB source | Consequence | Mitigation |
|-----------|-------------|-----------|
| Array out-of-bounds | Reads/writes arbitrary memory | Future: optional bounds checking |
| Integer overflow | Silent wrap (two's complement) | Future: optional overflow checking |
| Shift by >= bit width | LLVM poison value | Rare in practice |

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

If a function calls `abort()` (directly or via OOM), resources held by that function and all callers are leaked to the OS. Since there are no destructors and no unwinding, there is no mechanism to run cleanup in the abort path.

This is acceptable because:
- The process is terminating — the OS reclaims everything
- Predictable code cannot reach abort (no allocation, no Process capability)
- The alternative (unwinding to run cleanup) would compromise the execution model

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

1. **No abort reachable**: proved functions are pure (no capabilities), so `abort()` is not callable and OOM cannot occur
2. **No hardware traps from safe operations**: proved functions use only safe operations (no raw pointers, no division in current provable subset)
3. **Normal control flow only**: proved functions return normally or via explicit `Result` error — no hidden exit paths
4. **Defer runs if present**: deferred expressions in proved functions always execute (no abort path exists)

### What proved code may NOT assume

1. **No integer overflow safety**: proofs use unbounded integers (Lean `Int`); binary uses fixed-width wrapping. A theorem about `abs(x) >= 0` holds mathematically but not for `Int.MIN` at runtime.
2. **No array bounds safety**: array indexing is not bounds-checked. A proved function accessing `arr[i]` has no runtime guarantee that `i < len`.
3. **No termination guarantee**: the provable subset currently excludes loops, but the proof itself does not prove termination — it proves a property of the PExpr representation.
4. **No binary correspondence**: the proof is over PExpr (source-level IR), not compiled LLVM IR. Backend transformations are not formally verified.

### Gap summary for proved code

| Property | Proof model | Runtime reality | Gap |
|----------|------------|-----------------|-----|
| Integers | Unbounded | Fixed-width wrap | Overflow not caught |
| Array access | Not modeled | Unchecked GEP | OOB is UB |
| Control flow | PExpr (no loops, no mutation) | Compiled with LLVM optimizations | Backend not verified |
| Failure | Not modeled | abort/signal/UB possible in theory | Proved functions avoid all sources in practice |

---

## Summary of Commitments

1. **Abort-only**: no panic, no unwinding, no catch. This is permanent.
2. **Defer runs on normal paths**: every return, `?`, break, continue, scope exit.
3. **Defer skipped on abort/signal**: the process is dying; the OS cleans up.
4. **No leak on normal paths**: linear ownership + defer guarantees cleanup.
5. **Leak on abort is acceptable**: process termination reclaims everything.
6. **FFI is trust-based**: extern function contracts are not mechanically verified.
7. **Proved code avoids all failure sources**: by construction (no capabilities, no raw pointers, no allocation).
8. **Integer overflow and array OOB are known gaps**: documented, not yet mitigated at runtime.
