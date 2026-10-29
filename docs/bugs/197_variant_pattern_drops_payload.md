# Bug 197 — a variant pattern without a field list silently dropped a non-Copy payload

<!-- bug-ledger
summary: a variant pattern written without a field list (`E::A =>`) over an owned scrutinee silently dropped a non-Copy payload (both consumers; the discard rule lived on `_` binders and CoreCheck's field count skips an empty list). Refused with E0288; pinned by `error_197_variant_pattern_drops_payload.con` and `regress_197_variant_pattern_ignores_copy.con`. Found by the Codex review of the specification's patterns chapter
fixtures: error_197_variant_pattern_drops_payload.con regress_197_variant_pattern_ignores_copy.con
class: 15 — a variant pattern with no field list ignored a non-Copy payload
silent: yes
exit: 0b
ruling: Violates: PAT-3@2, OWN-5@2
-->

**Status:** FIXED (branch `wip/bug197`, based on `wip/bug196`)
**Found:** 2026-10-23, by the Codex review of the language specification's patterns chapter
(PAT-3 claims a variant pattern binds or ignores every field; CoreCheck skips its field count when
the pattern lists no fields), then probed on both consumers
**Severity:** a linearity violation in safe code. `match e { E::A => .., E::B => .. }` over an owned
`E` whose variant `A` carries a non-Copy payload compiled, and the payload vanished without being
consumed: the leak the linear rules exist to refuse (OWN-5).

Confirmed real by Codex review, 2026-10-25: docs/spec/ownership.md OWN-5 forbids discarding linear payloads; empty binder lists bypassed disposal checks. Ignoring Copy fields is valid.

## Reproducer

```con pseudocode
struct R { v: Int }
enum E { A { r: R }, B }
fn main() -> Int {
    let e: E = E::A { r: R { v: 42 } };
    match e {
        E::A => { return 1; },
        E::B => { return 0; }
    }
}
```

Expected: refused (E0288, like `E::A { _ }`). Observed on `wip/train4` `a4a9c05d`: both consumers
return 1, and `r` is never consumed.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore`, the enum `match` case: the arm's binders are zipped
with the variant's fields, so an empty binder list visits no field and none is checked against the
"only a Copy value may be ignored" rule that `_` binders obey. CoreCheck's field-count check (E0533)
explicitly skips an empty list, because a bare variant name is how a field-less variant is written.

## Fix

A variant pattern written without a field list over an owned scrutinee refuses every non-Copy field
of the variant with E0288 (`wildcardDiscardsNonCopy`), exactly as an explicit `_` would. Through a
reference nothing is moved, so it stays allowed, and so does ignoring Copy fields. Fixtures:
`error_197_variant_pattern_drops_payload.con` (E0288) and `regress_197_variant_pattern_ignores_copy.con`
(run 42, both consumers), in `run_tests.sh`. A rule-impact scan over the 2,169 units of the train 4 corpus found
one program newly refused, `tests/programs/result_generic_propagation.con`, whose `Result::Err =>`
arm dropped a non-Copy `MyError`: the bug itself, in a test. It now binds and consumes the error.

## Structural cause

Class 15, ownership state not modeled along every path: the discard rule was attached to `_`
binders, so a pattern that ignores fields without naming any binder bypassed it. It is the same
shape as bug 090's guard and 142's exits: a construct that disposes of a value without passing
through the site that checks disposal.

## Structural owner

R-0499 (S8: one conservation judgment on Core, where an arm's pattern is explicit about every field
it binds or ignores), with R-0507's linear-conservation proof.
