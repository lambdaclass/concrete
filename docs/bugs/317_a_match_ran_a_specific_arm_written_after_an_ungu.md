# Bug 317 — A match ran a specific arm written after an unguarded catch-all arm instead of refusing it

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's answer to OD18 (PAT-9, decided 2026-10-29): an `Int` match whose unguarded catch-all arm precedes a literal arm, native against `--interp`, on main 0bf8350a.
**Severity:** silent. Both consumers accepted the program and ran an arm the decided rule never selects: the `_` arm is first in source order and matches every value, yet the later `1` arm ran and the program returned 42.

<!-- bug-ledger
compiler: legacy
summary: A match ran a specific arm written after an unguarded catch-all arm instead of refusing it
coverage: archived reproducer(s) for bug 317; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
class: 13
silent: yes
ruling: Violates: PAT-9@1
-->

## Reproducer

`tests/regressions/bug317_a_match_ran_a_specific_arm_written_after_an_ungu/repro.con`: `match x { _ => { return 1; }, 1 => { return 42; } }` with `x == 1`.

## Observed

On main 0bf8350a (built on OVH) the native binary exits 42 and `--interp` prints 42, with no
diagnostic.

## Expected

[PAT-9@1](../spec/patterns.md#PAT-9) (OD18): the arm that runs is the first written one that
matches, and an arm that cannot run is refused. Here `_` comes first and matches every value, so
the `1` arm is refused at check (E0325, `rule: PAT-9@1`) on both consumers.

## Cause

`Concrete/Elab/CoreCanonicalize.lean`, `sortMatchArms`, moved every unguarded catch-all arm after
the other arms of every `match`, so a specific arm written after `_` came first in Core and was
selected by both consumers (`Interp` takes the first matching arm of the Core list; Lower's
comparison chain follows it). Bug 213's provisional repair then skipped, in Lower, the arms after
the first catch-all in that reordered list. No checker refused an arm earlier arms already cover:
CoreCheck's duplicate check (PAT-3, E0532) covers a repeated unguarded variant only.

## Structural cause

Class 13: a documented rule with no enforcing site, here worse, a normalization pass that encoded
the opposite reading. Arm order was a property of a Core-to-Core pass rather than of the checked
source, so the checker never saw the order the user wrote. Same class as bugs 310–313 (the
class-13 audit); bug 213 is the lowering symptom of the same reordering.

## Structural owner

R-0500 (the specification as the judge of each rule, with an enforcing site and fixtures for every
clause) and R-0499 (the Core checker as the one judgment of a well-formed program). check_spec.sh
S12 requires a negative witness per rule code, which E0325 now has.

## Historical branch containment (unlanded)

The filing branch contained an unlanded repair attempt; it is archived, not part of the frozen compiler.

## Archived fix attempt

Check refuses an arm earlier unguarded arms already cover (`Concrete.unreachableMatchArm`, called
from `checkExprCore`'s `match` case, E0325, `rule: PAT-9@1`), in source order: after an unguarded
catch-all, after an unguarded arm for every variant of the enum, after `true` and `false`, a
repeated literal and a literal or range inside earlier literals and ranges, and a guarded repeat
of a variant. A guarded arm covers nothing. CoreCanonicalize no longer reorders arms. An unguarded
repeated variant stays PAT-3's (E0532) wherever it stands. The fallback `_` arm the parser adds when
it desugars `if let`, `let ... else` or `while let` is not a written arm and is not judged (it
carries the statement's own span, which no written arm has), so `if let` over a one-variant enum
stays accepted. Ranges that together span an integer type are not counted
as covering every value, since PAT-2 requires the catch-all after them. Bug 213's fixture is now a
refusal on both consumers.

Codex confirmation 2026-10-29: CONFIRMED — Main silently selects the later literal arm and accepts a program that OD18 requires rejecting, so this is a real defect to fix (reordering at CoreCanonicalize.lean:38/82, no refusal in Check or CoreCheck for scalar scrutinees, silent; no owner decision beyond OD18 needed).

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-317`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/programs/error_317_guarded_after_catch_all.con`
- `tests/programs/error_317_range_inside_ranges.con`
- `tests/programs/error_317_repeated_variant_is_pat3.con`
- `tests/programs/error_317_value_match_after_every_variant.con`
- `tests/programs/regress_317_if_let_one_variant.con`
- `tests/programs/regress_317_partial_range_coverage.con`
- `tests/regressions/bug317_a_match_ran_a_specific_arm_written_after_an_ungu/repro.con`
