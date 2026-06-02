# Predictable Failure Discipline

Status: reference

This document defines which failure forms are allowed and excluded in code that passes the predictable profile. It builds on [FAILURE_STRATEGY.md](FAILURE_STRATEGY.md) (the language-wide failure model) and narrows it to the predictable subset.

For the five predictable gates, see [PROFILES.md](PROFILES.md).
For runtime boundary details, see [PREDICTABLE_BOUNDARIES.md](PREDICTABLE_BOUNDARIES.md).

---

## Principle

Predictable code has **no hidden control flow**. Every exit from a function is visible in the source: a normal return, an explicit error return via `Result`, or a `break`/`continue`. There are no runtime surprises — no abort, no OOM, no panic, no unwinding, no exception, no longjmp.

This is what "failure-only discipline" means: the only failures that can happen are the ones the programmer wrote.

---

## Allowed Failure Forms

### 1. Explicit error return (`Result<T, E>`)

Predictable functions may return `Result<T, E>` and propagate errors with `?`.

- Errors are values — they appear in the function signature
- `?` is sugar for early return with `Err`, not an exception throw
- Callers must handle or propagate — no silent dropping
- Defer runs normally on `?`-triggered returns

This is the primary and preferred error mechanism for predictable code.

### 2. Error-code return (`i32`, `bool`, enum)

Predictable functions may return error codes as integers, booleans, or enum variants. This is a lower-level pattern than `Result` but equally explicit:

```
fn validate_version(v: i32) -> i32 {
    if v == 1 { return 0; }  // ok
    return 2;                 // error code: bad version
}
```

The caller sees the return type and must check the value. Nothing is hidden.

### 3. Sentinel values

Functions may return sentinel values (0 for not-found, -1 for error, etc.). These are explicit in the return type and documented by convention.

---

## Excluded Failure Forms

The five predictable gates exclude all hidden or unbounded failure paths:

| Excluded failure | Why excluded | Which gate |
|-----------------|-------------|-----------|
| `abort()` | Requires `Process` capability | No blocking |
| OOM (`malloc` null) | Requires allocation | No allocation |
| Stack overflow from recursion | Requires recursion | No recursion |
| Unbounded stack growth from loops | Loops must be bounded | Bounded loops |
| Blocking I/O failure | Requires File/Network/Process | No blocking |
| FFI-triggered abort/signal | Requires extern calls | No blocking (FFI gate) |
| `longjmp` across frames | Requires FFI | No blocking |
| `exit()` | Requires Process capability | No blocking |

### What about stack overflow from deep (but bounded) calls?

Predictable functions have bounded call depth (acyclic call graph, no recursion). The `--report stack-depth` command shows the worst-case stack bound. However, the predictable profile does **not** currently gate on stack size — a deeply nested chain of functions with large frames could still overflow.

This is a known gap. The stack depth is **reported** but not **enforced**. The OS guard page is the current protection.

---

## Remaining UB in Predictable Code

Two sources of undefined behavior remain reachable from predictable code:

### Integer overflow

Arithmetic on fixed-width integers wraps silently (LLVM two's complement default). This is not hidden control flow — the program continues executing — but the result may be incorrect.

- **Current status**: not detected, not gated
- **Visibility**: `--report effects` does not flag overflow risk
- **Future**: optional overflow-checking mode (not yet planned)

### Array out-of-bounds

Safe array indexing (`arr[i]`) generates unchecked GEP+load. An invalid index produces UB, not a trap.

- **Current status**: not detected, not gated
- **Visibility**: no report flags OOB risk
- **Future**: optional bounds-checking mode (not yet planned)

Neither of these is hidden control flow — they do not change which code path executes. They are semantic correctness gaps, not control flow violations.

---

## How to Verify

| Question | Command | What to look for |
|----------|---------|-----------------|
| Does this function pass predictable? | `--check predictable` | `pass` / `fail` with gate details |
| Can this function abort? | `--report effects` | `caps: (pure)` or `caps: Console` means no abort path |
| Can this function allocate? | `--report effects` | `alloc: none` |
| What is the stack depth? | `--report stack-depth` | `depth: N` and `stack: M bytes` |
| Is error handling explicit? | Read the source | Return type is `Result`, `i32`, enum, or `bool` |
| Does this function use trusted code? | `--report effects` | `trusted: no` and `evidence: enforced` |

---

## Connection to Proof Eligibility

Proved functions are a strict subset of predictable functions with additional restrictions (no loops, no mutation, no capabilities at all). All proved functions satisfy the predictable failure discipline automatically.

The reverse is not true: predictable functions may use loops, mutation, and Console capability, which makes them ineligible for proof but still predictable.

---

## Summary

Predictable failure discipline = **explicit errors only**.

- **Allowed**: `Result` return, error codes, sentinel values, `?` propagation
- **Excluded**: abort, OOM, panic, unwinding, exceptions, blocking I/O failure, FFI failure, `longjmp`
- **Remaining gaps**: integer overflow (silent wrap), array OOB (UB) — neither is hidden control flow
- **Verification**: `--check predictable` + `--report effects` + `--report stack-depth`
