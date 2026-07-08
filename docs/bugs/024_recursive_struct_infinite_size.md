# Bug 024: Recursive struct with infinite size leaks to `llvm-as`

**Status:** Fixed
**Discovered:** 2026-07-07
**Fixed:** 2026-07-07
**Discovered in:** panic-to-diagnostic edge-case probing (pipeline second-tier work)
**Regression test:** `scripts/tests/check_error_leaks.sh` (gate corpus)

## Symptom

A struct that contains itself by value — directly, mutually, or through an
array element — was not rejected by the checker. It flowed all the way to code
generation, where `llvm-as` refused the self-referential LLVM struct type:

```
LLVM IR validation failed for .../p1.con.ll:
llvm-as: .../p1.con.ll:798:1: error: identified structure type 'struct.S' is recursive
```

```concrete
struct S { x: S }                       // direct
struct A { b: B } struct B { a: A }     // mutual
struct S { xs: [S; 2] }                 // via array element
```

All three reached `llvm-as` instead of producing a compiler diagnostic — an
internal-layer error leaking to the user, and a soundness/robustness gap (an
infinite-size type has no valid layout).

## Root Cause

**File:** `Concrete/Check/CoreCheck.lean`, `ccCheckModuleDecls`.

Module-declaration validation checked repr(C), packed/align, Copy fields, etc.,
but never checked that a struct's by-value field graph is acyclic. Layout
computation happily produced a self-referential LLVM struct type, deferring the
failure to `llvm-as`.

## Fix

Added `tyReachesByValue` (a by-value reachability walk over the struct/enum type
graph — `named`/`array` edges continue, every indirection `ref`/`refMut`/
`ptrMut`/`ptrConst`/`heap`/`heapArray` breaks the cycle) and a new
`ccCheckModuleDecls` section 1b that rejects any struct that reaches itself,
with `CoreCheckError.recursiveType` (E0583):

```
error[core-check]: [m] recursive type 'S' has infinite size
  hint: a value cannot contain itself by value; store the recursive field
        behind an indirection — heap<T>, a reference, or a raw pointer
```

Indirected recursive shapes (`next: *const Node`, mutual via `*mut`) still
compile — the gate pins both the rejection and the no-false-positive cases.
