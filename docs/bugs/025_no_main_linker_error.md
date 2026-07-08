# Bug 025: No `main` in an executable build leaks a linker error

**Status:** Fixed
**Discovered:** 2026-07-07
**Fixed:** 2026-07-08
**Discovered in:** panic-to-diagnostic edge-case probing (pipeline second-tier work)
**Regression test:** `scripts/tests/check_error_leaks.sh` (`no_main`, `empty_file` cases)

## Symptom

Compiling a file that defines no `main` function to an executable produces a
raw `ld` linker error instead of a clean compiler diagnostic:

```
$ concrete empty.con -o out          # (or a file with only helper fns)
Undefined symbols for architecture arm64:
  "_main", referenced from: implicit entry/start for main executable
ld: symbol(s) not found for architecture arm64
clang: error: linker command failed with exit code 1
```

The program IS rejected (nonzero exit), so this is a UX/robustness gap, not a
soundness hole — but the error is an internal linker leak, exactly the class the
error-leak gate exists to eliminate.

## Root Cause

**File:** `Concrete/Backend/EmitSSA.lean` (main-wrapper emission) + `Main.lean`
(`compileSSA`).

When no user `main` exists, EmitSSA emits no `@main` wrapper, so `user_main` /
`@main` is simply absent from the `.ll`, and the failure is deferred to `ld`.
Nothing on the compile-to-executable path asserts "an executable build needs a
`main`".

## Fix

`Main.lean` `compileSSA`, at the `.ok ssa` branch: before codegen, check
`ssa.ssaModules.any (·.functions.any ·.isEntryPoint)` (SSA modules are flat —
submodules included — so this is complete). If no entry point exists, emit
`error[link]: no \`main\` function found; ... define \`fn main() -> Int\`` and
return 1. `--emit-llvm` is exempt (dumping IR for inspection is legitimate
without an entry point), and there is no separate library/embedded build
profile, so requiring `main` on the executable path is safe. Regression:
`check_error_leaks.sh` `no_main` and `empty_file` cases.
