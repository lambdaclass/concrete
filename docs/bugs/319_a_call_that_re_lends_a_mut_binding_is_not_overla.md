# Bug 319 — A call that re-lends a &mut binding is not overlap-checked, so r.add(r.step) is accepted

<!-- bug-ledger
summary: A call that re-lends a &mut binding is not overlap-checked, so r.add(r.step) is accepted, which REF-11 refuses as the owner decided OD23 on 2026-10-29
coverage: reproducer tests/regressions/bug319_a_call_that_re_lends_a_mut_binding_is_not_overla/repro.con, found checking main against OD23 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending REF-11 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD23 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program REF-11 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD23: the code site below produces it).

## Reproducer

`tests/regressions/bug319_a_call_that_re_lends_a_mut_binding_is_not_overla/repro.con`.

## Observed

On main 0bf8350a (built on OVH): a call re-lends `r: &mut S` as the receiver while another argument reads `r.step` through it; native and `--interp` return 42.

## Expected

[REF-11](../spec/references.md#REF-11) (OD23): the call is refused (E0293): every argument, the receiver included and re-lent `&mut` bindings too, is overlap-checked against its root; the read goes into a `let` first.

## Cause

`Concrete/Check/CheckHelpers.lean`, `mutLoanRoot?` and `checkCallRootReads`: a re-lent reference binding makes no loan on its root (REF-11@2's exception), so a read through it in another argument is not checked.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and REF-11's
pending example records main's outcome. A program written to the decided rule is accepted today too.
