# Predictable/Proved Code Boundaries

Status: reference

This document classifies the runtime boundaries of Concrete code that passes the predictable profile and/or has Lean-backed proofs. It answers: what can happen at runtime, what is ruled out, and what is assumed.

For the general execution model, see [EXECUTION_MODEL.md](EXECUTION_MODEL.md).
For profile definitions and gates, see [PROFILES.md](PROFILES.md).
For the trusted computing base, see [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md).
For memory safety guarantees, see [MEMORY_GUARANTEES.md](MEMORY_GUARANTEES.md).

---

## Scope

This classification applies to functions that pass `--check predictable` (all five gates: no recursion, bounded loops, no allocation, no FFI, no blocking I/O). Functions that additionally have Lean-backed proofs (`evidence: proved`) have a further layer described under "Proved functions" below.

The classification is **source-level**. It describes what the compiler enforces and reports about the source program. It does **not** make claims about binary-level timing, LLVM optimization effects, or hardware behavior.

---

## 1. Host Calls

### Reachable from predictable code

Predictable functions cannot allocate, call FFI, or use blocking I/O capabilities. The host calls reachable from predictable code are:

| Host call | Reachable? | Condition | Notes |
|-----------|-----------|-----------|-------|
| `malloc` | No | Blocked by no-allocation gate | Alloc capability or alloc/vec_new intrinsics |
| `free` | No | Blocked by no-allocation gate | |
| `realloc` | No | Blocked by no-allocation gate | |
| `abort` | No | `abort` requires Process capability | Blocked by no-blocking gate |
| `printf` | Only from `main` wrapper | Compiler-emitted, not user code | `main` wrapper prints return value |
| `write` | Via Console capability | Console is allowed in predictable | `print`/`println` use write(2) |
| `strlen` | No | String operations imply Alloc | |
| `memcpy` | No | Vec/String operations imply Alloc | |
| `memset` | No | Vec pop implies Alloc | |
| `memcmp` | No | String equality implies Alloc | |
| `snprintf` | No | int_to_string implies Alloc | |
| `strtol` | No | string_to_int implies Alloc | |
| File/Network/Process syscalls | No | Blocked by no-blocking gate | |

**Summary**: Predictable functions may call `write` (Console print) and pure arithmetic/logic. All heap, string, Vec, FFI, and blocking operations are excluded by the five gates.

### Compiler-emitted code in predictable functions

| Emitted construct | Present? | Notes |
|-------------------|----------|-------|
| `alloca` (stack allocation) | Yes | Local variables, temporaries, aggregates |
| `getelementptr` (field/array access) | Yes | Struct field access, array indexing |
| `call` to user functions | Yes | Bounded call depth (acyclic, no recursion) |
| `call` to `__concrete_check_oom` | No | Only emitted for malloc results |
| Loop constructs | Yes | All loops bounded by predictable gate |
| `br` / `switch` (branching) | Yes | if/else, match |

---

## 2. Cleanup Paths

### Defer behavior

Predictable functions may use `defer`. Deferred expressions execute:

- **On normal return**: LIFO order, innermost scope first
- **On scope exit**: before control leaves the scope block
- **On break/continue**: deferred calls for exited scopes, stopping at loop/function boundary

Deferred expressions do **not** execute on:

- **abort()**: not reachable from predictable code (requires Process capability)
- **Hardware signals (SIGSEGV)**: process terminates immediately; defer is not a signal handler
- **OOM**: not reachable from predictable code (no allocation)

### Resource cleanup

Predictable functions do not allocate heap memory, so there is no heap resource to leak. All data is on the stack. When the function returns, the stack frame is reclaimed by the caller.

The only resources predictable code may hold are file descriptors from Console (stdout/stderr), which are process-global and not closed by user code.

---

## 3. Determinism Sources

### What is deterministic (compiler-enforced)

| Property | Guarantee level | Mechanism |
|----------|----------------|-----------|
| No recursion | Source-level | Call graph SCC analysis |
| Bounded iteration | Source-level | Loop bound classification |
| No allocation | Source-level | Alloc capability + intrinsic detection |
| No FFI | Source-level | Extern function detection |
| No blocking I/O | Source-level | Capability gate (File, Network, Process) |
| Acyclic call graph | Source-level | SCC analysis + recursion gate |
| Bounded stack depth | Source-level | `--report stack-depth` (item 25), not gated |
| Report/IR determinism | Compiler | No HashMap in output paths, monotonic counters |

### What is NOT deterministic (outside compiler scope)

| Property | Why not | Mitigation |
|----------|---------|-----------|
| Execution timing | LLVM optimization, CPU microarchitecture, OS scheduling | Not claimed |
| Binary layout | LLVM register allocation, instruction scheduling | Not claimed |
| Stack addresses | ASLR, OS stack allocation | Not claimed |
| Cache behavior | Hardware-dependent | Not claimed |
| Console output timing | OS write(2) buffering | Not claimed |

**Key point**: "Predictable" means source-level execution shape is bounded and reviewable. It does **not** mean constant-time, timing-deterministic, or binary-reproducible.

---

## 4. Failure Paths

### Failures reachable from predictable code

| Failure mode | Reachable? | Condition | Consequence |
|-------------|-----------|-----------|-------------|
| Explicit error return | Yes | `Result<T, E>` propagation | Normal control flow, caller handles |
| OOM (malloc null) | No | No allocation in predictable code | |
| Null pointer deref | Only in trusted | Safe code has no raw pointers | SIGSEGV if triggered |
| Integer overflow | Yes | Arithmetic on i32/i64/u32/u64 | Silent wrap (LLVM default) |
| Array out-of-bounds | Possible | Safe `arr[i]` generates GEP+load | See below |
| Stack overflow | Possible | Deep (but bounded) call chains | OS guard page → SIGSEGV |
| abort() | No | Requires Process capability | |

### Array bounds checking

Safe array indexing (`arr[i]`) currently generates unchecked GEP+load. The index is not bounds-checked at runtime. This is a known gap:

- The compiler does **not** insert runtime bounds checks for `arr[i]`
- The `--report stack-depth` shows call depth is bounded, but array indices are not statically bounded
- Out-of-bounds access is undefined behavior

This is explicitly documented in CLAIMS_TODAY.md and LANGUAGE_GAPS.md as a gap. Future work may add optional bounds checking or static index analysis.

### Integer overflow

Arithmetic operations on fixed-width integers (i32, i64, u32, u64) wrap silently per LLVM's default `add`/`sub`/`mul` semantics. There is no overflow trap. This is a known semantic gap between:

- Lean proofs (unbounded integers)
- Runtime behavior (fixed-width wrapping)

The gap is documented in PROOF_SEMANTICS_BOUNDARY.md.

---

## 5. Memory and UB Boundaries

### Safe code guarantees (enforced by checker)

| Guarantee | Mechanism | Applies to predictable? |
|-----------|-----------|------------------------|
| No use-after-move | VarState tracking in Check.lean | Yes |
| No borrow conflict | Exclusive mutable borrow enforcement | Yes |
| No borrow escape | Escape analysis | Yes |
| No linear value leak | Scope-exit consumption check | Yes |
| No reassignment of consumed linear | State machine | Yes |
| Branch agreement on ownership | Both arms must agree | Yes |

### UB still possible in predictable code

| Source of UB | How | Why not prevented |
|-------------|-----|-------------------|
| Array out-of-bounds | `arr[i]` with invalid index | No runtime bounds check (see above) |
| Integer overflow | `a + b` wrapping silently | No overflow detection |
| Division by zero | `a / 0` | No runtime check (hardware trap on x86) |
| Shift overflow | `a << 64` | LLVM poison value |

### UB NOT possible in safe predictable code

| Excluded UB | Why |
|-------------|-----|
| Use-after-free | No allocation, no free |
| Double free | No allocation |
| Null pointer deref | No raw pointers in safe code |
| Dangling reference | Borrow checker prevents escape |
| Data race | Single-threaded execution model |
| Uninitialized memory | All variables initialized at declaration |
| Type confusion | Static type system, no casts in safe code (except numeric) |

### Trusted functions in predictable code

Functions marked `trusted fn` bypass the checker. A predictable program may call trusted functions — they pass the five gates (no recursion, bounded loops, no allocation, no FFI, no blocking) but their bodies are not ownership-checked.

`--report effects` classifies trusted functions as `evidence: trusted-assumption`. This means the compiler verifies the function's signature and gate properties but trusts the implementation.

---

## 6. Proved Functions — Additional Boundaries

Functions with `evidence: proved` have all predictable properties plus a Lean-verified theorem about their PExpr representation.

### What "proved" adds

| Property | Mechanism |
|----------|-----------|
| Stated theorem holds over PExpr | Lean 4 kernel checking |
| Fingerprint matches current body | Stale detection via structural fingerprint |
| Function is pure (no capabilities) | Proof eligibility gate |
| No mutation in body | Proof eligibility gate |
| No loops | Proof eligibility gate (stricter than bounded) |

### What "proved" does NOT add

| Gap | Reason |
|-----|--------|
| Binary correctness | Proof is over PExpr, not compiled LLVM IR |
| Integer semantics match | PExpr uses unbounded integers; binary uses fixed-width |
| Composition | Per-function proofs, no cross-function theorem |
| All properties covered | Only the stated theorem; other properties unchecked |
| Backend faithfulness | Extraction pipeline not formally verified |

### Proof ≠ predictable

Proof eligibility is stricter than predictable (no loops, no mutation, no capabilities at all). A proved function is necessarily predictable, but a predictable function is not necessarily provable.

---

## 7. Profile Interaction Summary

| Property | Safe | Predictable | Proved |
|----------|------|-------------|--------|
| Ownership enforcement | Yes | Yes (inherited) | Yes (inherited) |
| No recursion | No | Yes | Yes (inherited) |
| Bounded loops | No | Yes | N/A (no loops) |
| No allocation | No | Yes | Yes (no capabilities) |
| No FFI | No | Yes | Yes (inherited) |
| No blocking I/O | No | Yes | Yes (no capabilities) |
| Stack bound computable | No (may recurse) | Yes (`--report stack-depth`) | Yes (inherited) |
| Lean theorem verified | No | No | Yes |
| Integer overflow possible | Yes | Yes | Yes (gap) |
| Array OOB possible | Yes | Yes | No (no array indexing in proved) |

---

## 8. Verification Commands

| Question | Command |
|----------|---------|
| Does this function pass predictable? | `concrete file.con --check predictable` |
| What are the effects of each function? | `concrete file.con --report effects` |
| What is the stack depth? | `concrete file.con --report stack-depth` |
| Is this function proved? | `concrete file.con --report proof-status` |
| What host calls does this function make? | `concrete file.con --report alloc` (allocation), `--report caps` (capabilities) |
| Is there recursion? | `concrete file.con --report recursion` |
| What is trusted? | `concrete file.con --report effects` (trusted: yes/no column) |
| What changed since last snapshot? | `concrete diff old.json new.json` |
