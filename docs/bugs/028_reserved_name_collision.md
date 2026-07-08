# Bug 028: User function colliding with a compiler-emitted name leaks to LLVM

**Status:** Fixed
**Discovered:** 2026-07-08
**Fixed:** 2026-07-08
**Discovered in:** generated-name hygiene probing (pipeline second-tier work)
**Regression test:** `scripts/tests/check_error_leaks.sh` (`clash_*` + `extern_argc` cases)

## Symptom

A user function whose name matched a compiler-emitted LLVM symbol produced a
duplicate/conflicting `define` and failed at `llvm-as` instead of a clean
diagnostic:

```concrete
fn user_main() -> Int { return 7; }        // clashes with the entry wrapper
fn __cc_bounds_check() -> Int { return 1; } // clashes with the bounds-check helper
fn __concrete_get_argc() -> Int { return 9; } // clashes with the argc accessor
```

Each reached codegen and emitted `LLVM IR validation failed`.

## Root Cause

**File:** `Concrete/Resolve/Intrinsic.lean`, `isReservedFnName`.

The reserved-name list was only `[destroy, abort, alloc, free, ...]`. It did not
cover `user_main` (the symbol EmitSSA renames the entry point to) or the `__`
prefix used by every compiler-internal helper (`__cc_*`, `__concrete_*`,
`__destr_*`). A user *definition* with one of those names therefore compiled a
second `define` for a symbol the backend also emits.

## Fix

Extended `isReservedFnName` to also reject `user_main` and any name starting
with `__`. The reserved-name check runs over `m.functions` (definitions) in both
CoreCheck and Check, producing `error[...]: '<name>' is a reserved identifier`
(E0578). `extern` declarations of compiler-provided `__` symbols live in
`m.externFns`, not `m.functions`, so `trusted extern fn __concrete_get_argc()`
(used by std/user code to *call* the accessor) is unaffected — verified by the
`extern_argc` gate case.
