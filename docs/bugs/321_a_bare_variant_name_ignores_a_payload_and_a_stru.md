# Bug 321 — A bare variant name ignores a payload and a struct let over a Copy struct may omit fields

<!-- bug-ledger
summary: A bare variant name ignores a payload and a struct let over a Copy struct may omit fields, which PAT-3 refuses as the owner decided OD25 on 2026-10-29
coverage: reproducer tests/regressions/bug321_a_bare_variant_name_ignores_a_payload_and_a_stru/repro.con, found checking main against OD25 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending PAT-3 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD25 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program PAT-3 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD25: the code site below produces it).

## Reproducer

`tests/regressions/bug321_a_bare_variant_name_ignores_a_payload_and_a_stru/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `E::A => ...` matches `A { v: Int }` with no field list, and `let P { a } = p;` binds one of two fields of a Copy struct; both consumers return 42.

## Expected

[PAT-3](../spec/patterns.md#PAT-3) (OD25): a bare variant name is legal only for a fieldless variant (the payload is ignored with `Variant { .. }` or `field: _`), and a struct `let` names every field or ignores one with `field: _`.

## Cause

`Concrete/Check/Check.lean`, `checkExprCore` (a bare variant over a Copy payload) and `checkStmt` (a Copy struct pattern may omit fields).

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and PAT-3's
pending example records main's outcome. A program written to the decided rule is accepted today too.
