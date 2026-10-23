# Bug 200 — a generic type named without its argument list was accepted

**Status:** FIXED (branch `wip/bug200`, based on `main` `1b2dba60`)
**Found:** 2026-10-23, by the Codex review of the language specification's types chapter (TYPE-15
claims a named type takes exactly the type arguments its declaration has), then probed on both
consumers
**Severity:** a type written with a missing argument compiled. Bug 073 refused a wrong-length
argument list (`Box1<i32, bool>`) but not a missing list, so `B` for a `B<T>` reached the later
passes with `T` never bound.

## Reproducer

```con pseudocode
struct Copy Box1<T> { v: Int }
fn get(b: Box1) -> Int { return b.v; }
fn main() -> Int { return get(Box1::<Int> { v: 42 }); }
```

Expected: refused (E0113, `Box1` takes 1 type argument, 0 given). Observed on `wip/train4`
`a4a9c05d`: both consumers return 42.

## Cause

`Concrete/Resolve/Resolve.lean`, `checkTyDeep`: the `.generic` case compares the argument list
with the declaration (`checkGenericArgs`, bug 073 and R-0484), but the `.named` case, a type
written without a list, checked only for missing capability arguments (E0115), never for missing
type arguments.

## Fix

The `.named` case refuses a known type whose declaration has type parameters with E0113 (0 type
arguments given), before the capability check. Fixtures: `error_200_bare_generic_type.con`
(E0113) and `regress_200_generic_type_with_arguments.con` (run 42), in `run_tests.sh`. A
rule-impact scan over the 2,169 units of the train 4 corpus found no program newly refused.

## Structural cause

Class 13, a documented rule with no enforcing site: the arity rule was enforced where an argument
list was written and nowhere else, so the form with no list bypassed it.

## Structural owner

R-0492 (types resolved once, by identity, with their arguments), where an unapplied generic type
cannot be represented; until then `checkTyDeep` owns both forms.
