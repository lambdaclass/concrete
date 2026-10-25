# Bug 218 — two declarations of one name in a module were accepted

<!-- bug-ledger
summary: MOD-3 was enforced between imports and functions only, so two declarations of one name in a module were accepted: two structs (the first silently won), a struct and an enum, two aliases; two functions failed in `llvm-as` and two constants at `lower`. `Check.declarationCollisions` refuses the later declaration (E0299), callables, constants and types in separate namespaces. Pinned by five `error_218_*.con` fixtures and `regress_218_type_and_value_namespaces.con`. Found by the diagnostics probes of the post-R-0492 differential hunt
fixtures: error_218_duplicate_function.con error_218_duplicate_struct.con error_218_struct_and_enum.con error_218_duplicate_constant.con error_218_duplicate_alias.con regress_218_type_and_value_namespaces.con
class: 1 — two declarations of one spelling in a module: nothing compared declarations with each other
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/diffhunt-1023`)
**Found:** 2026-10-23, by the diagnostics probes of the post-R-0492 differential hunt (does every
refusal carry a code?).
**Severity:** silent acceptance, and a backend failure. Two `struct S` declarations compiled with
the first silently winning (a literal of the second's shape was refused against the first's
fields); a struct and an enum `E` and two type aliases `A` were accepted. Two functions `f` failed in `llvm-as` ("invalid redefinition of function") while
`--interp` refused the call; two constants `X` failed at `lower` (E0602) with a message about two
modules.

## Reproducer

`tests/programs/error_218_duplicate_struct.con`:

```con
struct Copy S { a: Int }
struct Copy S { a: Int, b: Int }

fn main() -> Int {
    let s: S = S { a: 7 };
    return s.a;
}
```

Observed on main `dcc48dc1`: compiles; both backends return 7. Expected: refused by MOD-3@2 ("a
module binds each name to at most one declaration, whether by a declaration or an import").

## Cause

MOD-3's enforcing site, `Check.importBindingCollisions` (bug 082), compares a module's imports with
its own functions and with each other; nothing compared the module's declarations with each other.
Every later consumer keyed its tables by name and took the first (Check, Elab) or emitted both
(the backend).

## Fix

`Check.declarationCollisions`, run by `checkModule` beside `importBindingCollisions` for every
module and submodule: within a module, callables (functions, `extern` declarations), constants
and types (structs, enums, newtypes, aliases, traits) each bind a name once; the later declaration
is refused with E0299 (`CheckError.declarationCollision`). Two `extern` declarations of one name
stay with the link-name rule (E0118). A type and a callable may share a spelling, as may
declarations in different modules (`regress_218_type_and_value_namespaces.con`, 42 on both
backends), and so may a constant and a function: `subject_const_fn_collision.con` (the R-0004
witness) states that the language permits it, and whether MOD-3's "one declaration per name"
reaches across those two namespaces is left to the language owner. The parser now records the
spans of constants and type aliases, so the later of two is the one reported. Regression legs in
`run_tests.sh`: `error_218_duplicate_function.con`, `error_218_duplicate_struct.con`,
`error_218_struct_and_enum.con`, `error_218_duplicate_constant.con` and
`error_218_duplicate_alias.con` (`run_err ... (E0299)`). E0299's registry text names the new shape.

## Structural cause

Class 1 ("declaration identity by spelled name, not a resolved `DefId`"): a declaration is found by
its spelling in per-kind lists, so two declarations of one spelling are not an error anywhere
unless a rule compares them, and MOD-3's site compared only imports.

## Structural owner

R-0492 (S5 `DefId`: declaring a name allocates its identity once per module namespace, so a second
declaration cannot be represented), applied to the shared front end by R-0514.
