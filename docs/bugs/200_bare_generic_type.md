# Bug 200 — a generic type named without its argument list was accepted

<!-- bug-ledger
summary: a generic type named without its argument list (`Box1` for `Box1<T>`) was accepted: a value of it failed in `llvm-as` natively while `--interp` ran the program, and an uncalled signature naming it compiled and ran on both; bug 073 refused only a list of the wrong length. Refused with E0113 in `checkTyDeep` (type parameters in scope shadow declarations); pinned by `error_200_bare_generic_type.con`, `error_200_bare_generic_signature.con`, `regress_200_generic_type_with_arguments.con` and `regress_200_type_param_shadows_generic.con`. Found by the Codex review of the specification's types chapter (TYPE-15)
fixtures: error_200_bare_generic_type.con error_200_bare_generic_signature.con regress_200_generic_type_with_arguments.con regress_200_type_param_shadows_generic.con regress_200_self_receiver_of_generic_impl.con
class: 13 — a generic type named without its argument list: arity checked only where a list was written
silent: yes
exit: 0b
ruling: Violates: TYPE-15@1
-->

**Status:** FIXED (branch `wip/bug200`, based on `main` `1b2dba60`)
**Found:** 2026-10-23, by the Codex review of the language specification's types chapter (TYPE-15
claims a named type takes exactly the type arguments its declaration has), then probed on both
consumers
**Severity:** silent on the interpreter, fails closed natively. Bug 073 refused a wrong-length
argument list (`Box1<i32, bool>`) but not a missing list, so `B` for a `B<T>` reached the later
passes with `T` never bound: a value of the bare type failed in `llvm-as` natively while `--interp`
ran the program, and a signature that names it but is never called compiled and ran on both.

Confirmed real by Codex review, 2026-10-25: docs/spec/types.md TYPE-15 requires exact generic arity; Resolve checked explicit argument lists but let bare generic names through. OD5 does not decide arity.

## Reproducer

```con pseudocode
struct Copy Box1<T> { v: Int }
fn main() -> Int {
    let b: Box1 = Box1 { v: 42 };
    return b.v;
}
```

Expected: refused (E0113, `Box1` takes 1 type argument, 0 given). Observed on `main` `1b2dba60`:
natively `llvm-as` fails (`Cannot allocate unsized type`, `alloca %struct.Box1`); `--interp`
returns 42. With `fn get(b: Box1) -> Int` declared and never called, both consumers return 42.

## Cause

`Concrete/Resolve/Resolve.lean`, `checkTyDeep`: the `.generic` case compares the argument list
with the declaration (`checkGenericArgs`, bug 073 and R-0484), but the `.named` case, a type
written without a list, checked only for missing capability arguments (E0115), never for missing
type arguments.

## Fix

The `.named` case refuses a known type whose declaration has type parameters with E0113 (0 type
arguments given), before the capability check. A type parameter in scope (the function's or its
impl's) shadows a declaration of the same name, and a name declared as an alias or trait never
falls back to the builtin table (`type Vec = Box1<Int>` takes no arguments); both held for the
capability check too, and both are pinned by `regress_200_type_param_shadows_generic.con`. A method's `self`, which the
parser writes as the impl's bare type name, takes the impl target's arguments and is exempt
(`regress_200_self_receiver_of_generic_impl.con`; bug 186's impl fixtures caught the first
version of the fix refusing it). Fixtures: `error_200_bare_generic_type.con` and
`error_200_bare_generic_signature.con` (E0113), `regress_200_generic_type_with_arguments.con` and
`regress_200_type_param_shadows_generic.con` (run 42), in `run_tests.sh`. A
rule-impact scan over the 2,169 units of the train 4 corpus found no program newly refused.

## Structural cause

Class 13, a documented rule with no enforcing site: the arity rule was enforced where an argument
list was written and nowhere else, so the form with no list bypassed it.

## Structural owner

R-0492 (types resolved once, by identity, with their arguments), where an unapplied generic type
cannot be represented; until then `checkTyDeep` owns both forms.
