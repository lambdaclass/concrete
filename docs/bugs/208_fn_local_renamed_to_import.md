# Bug 208 — a fn-typed local spelled like an imported function was replaced by that function

<!-- bug-ledger
summary: Elab's import-rename pass rewrote every fn-typed identifier whose spelling an import maps, so a fn-typed parameter spelled like an imported function was replaced by that function when used as a value (silent wrong code on both backends: 141 instead of 42; bug 050's recorded remaining gap). Core references now carry binding identities (R-0492 step 3) and an identified local is never renamed. Pinned by `regress_208_fn_local_named_like_import.con` (binary and --interp)
fixtures: regress_208_fn_local_named_like_import.con
class: 1 — a fn-typed local renamed to a same-spelled imported function
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `vp-r0492-core`)
**Found:** 2026-10-23, by the Codex review of R-0492 step 3: the new `ElabVerify` pass would have
refused the program (U1), because Elab's import-rename pass gave an identified local reference a
global function's name. The defect itself is bug 050's recorded "remaining gap".
**Severity:** silent wrong code. A call through a fn-typed local reached a different function, and
the binary and `--interp` agreed on the wrong answer, so differential testing could not see it.

## Reproducer

`tests/programs/regress_208_fn_local_named_like_import.con`:

```con pseudocode
mod m {
    mod s { pub fn inc(x: i32) -> i32 { return x + 100; } }
    import s.{inc};
    fn plus1(x: i32) -> i32 { return x + 1; }
    fn apply(inc: fn(i32) -> i32, v: i32) -> i32 {
        let g: fn(i32) -> i32 = inc;
        return g(v);
    }
    pub fn main() -> i32 { return apply(plus1, 41); }
}
```

Expected 42 (`apply` calls `plus1` through its parameter). At `b422d33a` the binary exited 141 and
`--interp` printed 141. With the fix both give 42.

## Cause

After elaboration, `renameFnExpr` rewrites a submodule's imported function names to their prefixed
Core names. For an identifier of fn type it applied the import's rename map by spelling, because
Core's `.ident` carried only a name and a type and could not tell a global function used as a value
from a local of fn type. The parameter `inc` was therefore renamed to the submodule's `inc`. Bug
050 fixed the call form of this hazard (`Callee.indirect`) and recorded this one as open.

## Fix

Core references carry the binding identity the local walk minted (`CLocal.id`, R-0492 step 3). The
rename pass skips any reference that carries one. Only an unidentified reference (a global
function used as a value) is renamed.

## Structural cause

Declaration identity by spelled name (class 1): a pass decided by spelling whether an identifier
named a global or a local.

## Structural owner

R-0492. `ElabVerify` (U1) now refuses Core in which an identified reference is spelled differently
from its binding, so a pass that renames a local fails closed at elaboration rather than reaching
the backends.

## Regression

`regress_208_fn_local_named_like_import.con`, a `run_tests.sh` leg (exit 42) plus a `--interp`
comparison.
