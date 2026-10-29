# Bug 027: EmitSSA renders SSA→LLVM text in O(n²)

<!-- bug-ledger
summary: a large array literal compiled natively in superlinear time (`[7; 80000]`: 14 s; `[7; 200000]`: over 120 s; now 0.44 s): EmitSSA searched a list of pointer registers per operand, and one store per element made a block clang -O2 schedules in superlinear time. Pointer registers are a set, a uniform literal is filled by a loop, and a run of constant element stores is one `memcpy` from a private constant. Pinned by `regress_027_large_array_literals.con` and a generated distinct-element literal. Class 17.
fixtures: regress_027_large_array_literals.con
class: 17 — EmitSSA searched a list of pointer registers per operand; one store per literal element
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/perf-quadratics`)
**Discovered:** 2026-07-08
**Partial fix:** 2026-07-09
**Discovered in:** array-repeat-count probing (bug 026) — a moderate array
literal that passes the parser cap is still slow to codegen.

## Symptom

Codegen of a function with many instructions is quadratic in the instruction
count. Bisected with a 20 000-element array literal (`[0; 20000]`):

```
--check      : fast
--emit-core  : fast          (Elab fine)
--emit-ssa   : fast          (Lower fine)
--emit-llvm  : TIMES OUT      (>25s)   <-- EmitSSA rendering
```

So Lower produces the ~20 000 stores quickly, but rendering the SSA to LLVM
text does not scale.

## Root Cause (suspected)

**File:** `Concrete/Backend/EmitSSA.lean`.

Pervasive `acc ++ x` accumulation inside folds, each copying the growing
accumulator — O(n²) over `n` instructions/blocks. Examples:

- type collection: `b.insts.foldl (fun acc inst => acc ++ collectSInstTys inst) acc`
  (lines ~1321-1322 and ~1413-1414), and `collectSValTys` using `++`;
- block/instruction text assembly built by string `++` append per instruction.

## Partial Fix (2026-07-09)

The **type-collection folds** in `emitSModule` and `scanBuiltinEnumArgs` did
`acc ++ collectSInstTys inst` — copying the growing accumulator once per
instruction, O(n²) over a function's instruction count. Flipped to
`collectSInstTys inst ++ acc` (prepend small onto big; the collected type list
is deduped downstream via `findSome?`/`filterMap`, so order is irrelevant), and
the seed `acc ++ retTys ++ paramTys` → `retTys ++ paramTys ++ acc`.

Effect: a 20 000-element array literal went from a hard timeout (>2 min) to
compiling. This removed the dominant quadratic.

## Remaining (2026-07-09; resolved below)

Codegen of very large functions is still superlinear (measured ~7s at 10k
instructions, ~35s at 20k — worse than the 2× the size implies). The remaining
cost is elsewhere on the `--emit-llvm` path (candidates: per-string
`ssaEscapeStringForLLVM` `acc ++ char` at EmitSSA.lean:455; `dedupDecls`
`acc.any` scan; or inherent output-size effects in the `EmitLLVM.printLLVM*`
text assembly, which already uses `intercalate`). Pinning it down needs actual
profiling, not eyeballing, and is a broader codegen refactor. Bug 026 caps
array-repeat literals at 2²⁰, which bounds the array-literal trigger in
practice; this remaining quadratic matters before large (20k+ LOC /
large-function) workloads and should be driven by a profiler.

## Measured and fixed (2026-10-25, branch `wip/perf-quadratics`)

Measured natively (`concrete f.con -o bin`, OVH host, main `abc6318a` against the fix), a literal
as the only statement besides one read:

| elements | `[7; n]` before | after | `[7, 7, …]` before | after |
|---|---|---|---|---|
| 1000 | 0.14 s | 0.13 s | 0.15 s | 0.14 s |
| 10000 | 0.34 s | 0.20 s | 0.53 s | 0.21 s |
| 20000 | 1.03 s | 0.26 s | 2.17 s | 0.27 s |
| 40000 | 3.75 s | 0.43 s | 10.4 s | 0.43 s |
| 80000 | 13.7 s | 0.83 s | 47.9 s | 0.82 s |
| 200000 | > 120 s | 0.44 s | — | 0.50 s |

Three superlinear costs, one per layer:

1. **EmitSSA.** The set of registers known to hold pointers (`ptrRegs`, and `fnTypeRegs` beside it)
   was a list, queried for every operand. Each element's `gep` asks about the array's base, the
   oldest pointer register of the function, at the far end of the list: `--emit-llvm` of
   `[7; 40000]` spent 3.35 s in emission, 0.06 s with a set. The partial fix of 2026-07-09 had
   removed the type-collection appends; this was the remaining quadratic.
2. **The parser** (explicit elements only): bug 249.
3. **clang -O2**, on what Lower gave it: one `store` per element, a block of n stores, which the
   machine instruction scheduler handles in superlinear time once the function does anything
   else. Two 100000-element literals and a loop over them: 28 s, with (1) fixed; two literals of
   100000 distinct values: 40 s.

## Fix

- `ptrRegs` and `fnTypeRegs` are `Std.HashSet`s.
- Lower fills a literal of at least 64 copies of one integer, `bool` or `char` constant (what
  `[v; n]` expands to) with a loop over its elements; the constant has no effects, so the bytes
  written are the same.
- EmitSSA copies a run of at least 64 integer constants stored to consecutive elements of an
  array (a literal of distinct values) with one `memcpy` from a private constant holding them.

After: `[7; 200000]` 0.44 s, two 100000-element literals and a loop 0.45 s, two literals of 100000
distinct values and a loop 2.0 s. Pinned by `regress_027_large_array_literals.con`
(`run_ok … 42`, and the native compile within 20 s under `bounded_run -r 20`, which the old
backend cannot meet) and by a generated literal of 2 x 100000 distinct elements compiled within
the same budget.

## Structural cause

Class 17 ("pass state in association lists and per-query recomputation"): the emitter's
per-function register facts were a list searched per query, and the lowering of an aggregate
literal produced output proportional to its length in one block, which a later tool processes in
superlinear time. Bug 250 is the same backend's write and read side.

## Structural owner

R-0422 (compiler throughput queue, complexity envelopes), with the backend's aggregate
representation (bug 250).
