# Bug 025: No `main` in an executable build leaks a linker error

**Status:** Open
**Discovered:** 2026-07-07
**Discovered in:** panic-to-diagnostic edge-case probing (pipeline second-tier work)
**Tracked by:** `scripts/tests/check_error_leaks.sh` (NOT yet in the corpus — add when fixed)

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

## Proposed Fix

On the compile-to-executable path (`Main.lean` `compileSSA`), after checking,
verify the program defines an entry `main`; if not, emit a clean diagnostic
("error: no `main` function — an executable needs an entry point") and skip
`clang`. Must respect the module/submodule main-resolution already used at
`Main.lean:421` (`some ["main"] | some ["lib"]`) and must NOT reject legitimate
library-only compilation if such a mode exists. Once fixed, add a
`clean_reject` case to `check_error_leaks.sh` and flip this to Fixed.
