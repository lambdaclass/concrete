# Bug 022: A submodule declaration breaks impl-method consumption in the parent

**Status:** Fixed
**Discovered:** 2026-07-07
**Fixed:** 2026-07-07
**Discovered in:** the #35 `conlog` log-analyzer workload (`examples/conlog`)
**Regression test:** `tests/programs/submodule_linear_consume/` (project-mode)

## Symptom

Declaring a sibling submodule (`mod helper;`) in a module made the PARENT
module's linear-consumption analysis stop recognizing by-value-`self` method
calls (e.g. `String::drop`) as consuming. A straight-line `s.drop()` on an
owned `String` was then falsely rejected with **E0208 "never consumed"**.

```concrete
mod app {
    mod helper;                 // <-- remove this line and it compiles
    pub fn main() with(Std) -> Int {
        let s: String = "abc";
        s.drop();               // E0208: 's' never consumed (!)
        return 0;
    }
}
```

- Without the submodule: builds clean.
- With `mod helper;` present (even with no `import helper.{...}`): every
  by-value-self method consume in the parent is silently a no-op, so the
  linear value is reported unconsumed.

It blocked essentially every multi-module project that owns a linear value.
Over-rejection (fail-closed), so not unsound — but a hard blocker. It went
unnoticed because `std`'s parent `lib.con` re-exports submodules but never
calls a consuming method in its own body, so the mis-resolution was never
exercised.

## Root Cause

**File:** `Concrete/Check/Check.lean`, `checkModule` signature-table construction.

`allSigs` is laid out as
`imported ++ fnSigs ++ builtin ++ extern ++ submodule ++ impl`, but the
per-group index maps were computed inconsistently:

```
let implOffset := externOffset + externSigs.length          -- WRONG
```

This omits `submoduleSigs.length`. Impl-method names (`String_drop`,
`String_append`, …) were therefore mapped to indices that land in the
**submodule-sigs** region of `allSigs`, not the impl region. So `s.drop()`
resolved to some submodule function's signature (whose first parameter is not
a by-value `self`), and the method-call consume logic — which consumes the
receiver only when `self` is by value — treated `.drop()` as non-consuming.

With no submodules, `submoduleSigs.length == 0`, so the offset was
accidentally correct — which is why single-module projects worked.

## Fix

```
let implOffset := externOffset + externSigs.length + submoduleSigs.length
```

`implOffset` now matches the actual position of the impl sigs in `allSigs`,
so `String_drop` et al. resolve to the correct by-value-`self` signature and
are recognized as consuming.

## Regression Test

`tests/programs/submodule_linear_consume/` — a project with a sibling
submodule whose parent consumes an owned `String` via `.drop()` both
straight-line and inside a nested `if`-in-`while`. Auto-built and run by the
project-level test loop in `run_tests.sh` (build must succeed, run must exit
0). Before the fix: build fails with E0208. After: exit 0.
