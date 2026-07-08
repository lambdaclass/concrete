# Bug 026: Huge array-repeat count hangs the compiler

**Status:** Fixed
**Discovered:** 2026-07-08
**Fixed:** 2026-07-08
**Discovered in:** panic-to-diagnostic edge-case probing (pipeline second-tier work)
**Regression test:** `scripts/tests/check_error_leaks.sh` (`huge_array` case)

## Symptom

An array repeat literal `[value; count]` with a large `count` hung the compiler
(no output, no termination) instead of compiling or diagnosing:

```concrete
let a: [i64; 100000000000] = [0; 100000000000];   // hangs / OOM
```

Even moderate counts were pathological: `[0; 20000]` timed out at 20s.

## Root Cause

**File:** `Concrete/Frontend/Parser.lean`, array-literal parsing.

`[value; count]` was expanded by materialising `count` AST nodes with an
`elems := elems ++ [first]` loop — an **O(count²)** list append (each `++`
copies the whole accumulator). So even ~20k elements were quadratic, and an
absurd count materialised billions of nodes → OOM.

## Fix

- Build the element list in O(count) with `List.replicate count first`.
- Cap `count` at 2^20 (1,048,576): a fixed-size array literal larger than that
  is never legitimate (largest real use in the tree is 4096), and materialising
  it would exhaust memory before any later stage runs. Over the cap emits a
  clean parse diagnostic:
  `error[parse]: array repeat count N is too large (maximum 1048576)`
  with a hint to use heap allocation for large buffers.

`--emit-llvm` / compile of realistic sizes (4096) is unaffected and fast.

## Related

A *separate* quadratic remains downstream — see bug 027 (EmitSSA renders
SSA→LLVM text in O(n²), so a large array literal that passes the parser cap is
still slow to codegen). That is a codegen perf refactor, not a parse-robustness
issue; this bug only covers the parse-time hang/OOM.
