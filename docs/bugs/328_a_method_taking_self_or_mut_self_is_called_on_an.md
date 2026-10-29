# Bug 328 — A method taking &self or &mut self is called on an owned place with an implicit borrow

<!-- bug-ledger
summary: A method taking &self or &mut self is called on an owned place with an implicit borrow, which OWN-15 refuses as the owner decided OD32 on 2026-10-29
coverage: reproducer tests/regressions/bug328_a_method_taking_self_or_mut_self_is_called_on_an/repro.con, found checking main against OD32 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending OWN-15 example)
class: 13
silent: yes
ruling: Violates: OWN-15@3
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD32 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program OWN-15 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD32: the code site below produces it).

## Reproducer

`tests/regressions/bug328_a_method_taking_self_or_mut_self_is_called_on_an/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `s.get()` with `fn get(&self)` on an owned `s` is accepted with an implicit borrow and both consumers return 42.

## Expected

[OWN-15](../spec/ownership.md#OWN-15) (OD32): no receiver auto-borrow: the call is written `(&s).get()` (or `(&mut s).m()`); a receiver that is already a reference is unchanged.

## Cause

`Concrete/Elab/Elab.lean`, `elabExprEv`: method elaboration inserts a borrow when the method expects a reference and the receiver is owned.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and OWN-15's
pending example records main's outcome. A program written to the decided rule is accepted today too.
