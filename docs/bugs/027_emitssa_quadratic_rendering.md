# Bug 027: EmitSSA renders SSA→LLVM text in O(n²)

**Status:** Open (performance)
**Discovered:** 2026-07-08
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

## Proposed Fix

Assemble instruction/block output and type lists without repeated `++`: collect
into `Array` (or a list built in reverse then reversed once) and `String.join` /
`intercalate` at the end — O(n). This is a codegen perf refactor, not a
correctness bug: output is correct, just slow at scale. Bug 026 caps array-
repeat literals at 2^20, which bounds the array-literal trigger; this bug is the
general quadratic and should be fixed before large (20k+ LOC / large-function)
workloads.
