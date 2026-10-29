# Bug 196 — a match arm whose pattern did not fit the scrutinee was not checked

<!-- bug-ledger
summary: a match arm whose pattern did not fit the scrutinee was not checked: a variant of another enum made the checker panic (`Option.get!`) or, sharing a name, was silently taken as the scrutinee's own variant, a range over an enum failed in the SSA verifier natively while `--interp` ran it, and variant arms over integers or literals of another type were accepted. Refused with E0309; pinned by six `error_196_*.con` fixtures. Found by the language specification's patterns chapter
fixtures: error_196_other_enum_variant.con error_196_range_over_enum.con error_196_variant_over_int.con error_196_literal_type.con error_196_literal_over_type_param.con error_196_same_name_other_enum.con
class: 13 — pattern shape left to CoreCheck, after Elab crashed or lowered the arm
silent: yes
exit: 0b
ruling: Violates: PAT-3@2
-->

**Status:** FIXED (branch `wip/bug196`)
**Found:** 2026-10-23, by the language specification's patterns chapter (`docs/spec/patterns.md`,
PAT-3: a variant pattern names a variant of the scrutinee's enum; a literal or range pattern
appears only over a scrutinee of its type), on `wip/spec-single-source`
**Severity:** the compiler panicked or a malformed program reached the backend. A variant of another
enum (`F::X` over an `E` scrutinee) made the checker panic (`Option.get!`, "value is none") before
elaboration reported `unknown variant`; a range pattern over an enum failed in the SSA verifier
natively (E0715, an enum compared with an integer) while `--interp` ran the program. When the other
enum shares the variant's name (`F::A` over an `E` that also has `A`), the arm was silently taken as
`E::A`: wrong code with no diagnostic.

Confirmed real by Codex review, 2026-10-25: docs/spec/patterns.md PAT-3 requires patterns to fit the scrutinee; lookup ignored enum identity and used get!, explaining both crashes and wrong-variant matching.

## Reproducer

```con pseudocode
enum Copy E { A, B }
enum Copy F { X, Y }
fn main() -> Int {
    let e: E = E::A;
    match e {
        F::X => { return 1; },
        _ => { return 0; }
    }
}
```

and, with the same `E`, `match e { 0..=9 => { return 1; }, _ => { return 42; } }`. Expected: both
refused by the checker. Observed on `wip/train4` `a4a9c05d`: a Lean panic followed by E0411 at
`elab` (both consumers), and E0715 at `ssa-verify` natively with `--interp` returning 42.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore`, the `match` case. Over an enum scrutinee, a variant
arm looked its variant up with `(ed.variants.find? ...).get!`, never comparing the arm's enum with
the scrutinee's (a comment deferred "wrong-enum" to CoreCheck, which runs after Elab); a literal or
range arm was accepted without a look at its type. Over a non-enum scrutinee, a literal's type was
never compared with the scrutinee's, a range was accepted over any type, and a variant arm was
accepted outright.

## Fix

The checker refuses, with the new code E0309: a variant arm naming another enum, a literal or range
arm over an enum, a variant arm over a non-enum, a literal of another type, and a range over a
scrutinee that is not an integer or `char`. A literal or range pattern over a type-parameter
scrutinee is refused as well: a generic body is checked once at its opaque parameter, and no bound
says every instantiation has integer literals (before, an integer literal took the parameter as its
type and passed, and the specialization decided). An unknown variant of the right enum is E0260, and the
`get!` is gone. An unknown scrutinee type (from an earlier error) is not reported again. Fixtures:
`error_196_other_enum_variant.con`, `error_196_range_over_enum.con`,
`error_196_variant_over_int.con`, `error_196_literal_type.con` and
`error_196_literal_over_type_param.con` and `error_196_same_name_other_enum.con` (E0309, in
`run_tests.sh`). A
rule-impact scan over the 2,169 units of the train 4 corpus found no program whose outcome changed.

## Structural cause

Class 13, documented rules with no enforcing site: the shape rules of a pattern were left to
CoreCheck, which runs after Elab and after the point where a malformed arm crashes or lowers to
ill-typed SSA. Deferring a check to a later stage is only sound when no stage in between depends on
it.

## Structural owner

R-0499 (S8: one checker owns pattern well-formedness). Until then the source checker must run every
pattern check that elaboration relies on, because elaboration is not total on malformed patterns
(the `get!`); a check deferred past a stage that depends on it is the cause, not a placement.
The specification's PAT-3 reject examples run on both consumers and pin the first responsible
stage.
