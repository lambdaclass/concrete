# Bug 250 — natively, writing an element of an aggregate local rebuilds the whole aggregate

<!-- bug-ledger
summary: natively, writing an element or field of an aggregate local carried through a loop rebuilt the whole aggregate per write (and reading a field loaded the whole struct); the first-class loads and stores of these aggregates made clang -O2 superlinear in their size (a 20000-element array written in a loop: 99 s to compile). Lower writes and reads scalar places of promoted locals at their address; EmitSSA holds a loaded aggregate by pointer (a fresh slot and a `memcpy`). Pinned by `regress_250_native_array_writes.con`. Found writing bug 248's regression. Class 17.
fixtures: regress_250_native_array_writes.con
class: 17 — a native place write rebuilt its whole aggregate base
silent: no
-->

**Status:** FIXED (branch `wip/perf-quadratics`)
**Found:** 2026-10-25, writing bug 248's regression: its program, compiled natively, did not
finish compiling in 120 s.
**Severity:** loud (performance), at compile time and at run time. In a loop, `a[i] = v` on a local
array, `s.f = v` and `s.arr[i] = v` on a local struct copied the whole aggregate per write, and
reading `s.f` or `s.arr[i]` loaded the whole struct; the first-class LLVM loads and stores of these
aggregates made clang -O2 superlinear in their size.

## Reproducer

A loop writing a local `[i64; n]` (`tests/programs/regress_250_native_array_writes.con` has the
shapes): clang -O2 alone took 1.6 s at n = 2500, 5.0 s at 5000, 26 s at 10000, 99 s at 20000, and
each write `memcpy`d the array into a temporary, stored the element and loaded and stored the
whole array back. A struct holding a 10000-element array written and read through its field in a
loop: 46 s to compile. Fixed: 0.6 s (n = 10000) and 2.0 s (n = 100000) for the array, 0.2 s for the
struct.

## Cause

`Lower.lean`, `storeToPlace`: a place whose base is a value (not a reference) was written by
rebuilding the base (`lowerExpr` of the base, a copy into a temporary, the store, a load of the
whole temporary) and storing the rebuilt value into the base's own place, recursively. For a local
carried through a loop, the place is the local's stable alloca, so the rebuild was a whole-aggregate
copy per write. Reads of `s.f` and `s.arr[i]` lowered `s` by value (a load of the whole struct)
before taking the field. In EmitSSA, a loaded aggregate was a first-class LLVM value (`load %T`),
turned back into memory by a `store` wherever a pointer was needed (a call, a field), so every such
copy was a first-class load and store of the whole type.

## Fix

- Lower writes a scalar (integer, float, `bool`, `char`) into an element or field of a promoted
  local, or of a chain of struct fields of one (each a struct or array held by value, never a
  reference, pointer or heap cell), at its address (`placeAddr`), and reads a field, or an element
  of an array field whose index cannot change memory, of such a local at its address. Other places
  keep the rebuild.
- EmitSSA loads an aggregate as a copy held by pointer (a fresh slot and a `memcpy` into it), the
  form an aggregate parameter already has, so no first-class load or store of it remains.

The bytes each statement leaves in memory are the same; a copy taken before a write (`let b = a;`)
is still a copy. One `--emit-ssa` golden moved: in `tests/golden/ssa/defer_basic`, `return r.value;`
on a `Resource` local promoted to its slot `%r.agg.3` read the field through the loaded struct value
(`%t6 = load i64, %t2`) and now reads it at the slot (`%t5 = load i64, %r.agg.3`). The slot holds the
stored struct, nothing writes it between the store and the read, and the read still comes before
the deferred `Resource_destroy` call, so the returned value is the same; one register fewer is
numbered. Pinned by `regress_250_native_array_writes.con` (`run_ok … 42`, the native compile
within 20 s under `bounded_run -r 20`, and `--interp` = 42).

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): a place write was a
whole-value recomputation of its base, and the backend had two representations of an aggregate (a
first-class value and a pointer to memory) converted by whole copies; bug 027 is the same backend's
literal side.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes); ROADMAP #5 (the Lower builder) for place
lowering.
