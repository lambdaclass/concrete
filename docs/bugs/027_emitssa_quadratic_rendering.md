# Bug 027: EmitSSA renders SSA→LLVM text in O(n²)

**Status:** Partially fixed (performance)
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

## Remaining (still Open)

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
