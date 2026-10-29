# Bug 322 — Trusted code compares raw pointers with ==, < and the other comparison operators

<!-- bug-ledger
summary: Trusted code compares raw pointers with ==, < and the other comparison operators, which TYPE-6 refuses as the owner decided OD26 on 2026-10-29
coverage: reproducer tests/regressions/bug322_trusted_code_compares_raw_pointers_with_and_the/repro.con, found checking main against OD26 on 2026-10-29; reproduction pinned by check_example_outputs.sh (open-bug reproducers leg, runs-exit 42) and check_spec.sh S4 (the pending TYPE-6 example)
class: 13
silent: yes
-->

**Status:** OPEN
**Found:** 2026-10-29, by checking main against the owner's tightening OD26 ("strict across the board", 2026-10-29), one program per tightening, native against `--interp`, on main 0bf8350a.
**Severity:** silent under the decided rule: a program TYPE-6 refuses is accepted, and both consumers agree on its result.

Codex confirmation 2026-10-29: CONFIRMED (all ten tightenings OD23 – OD32 were confirmed in one review of main's measured behaviour; for OD26: the code site below produces it).

## Reproducer

`tests/regressions/bug322_trusted_code_compares_raw_pointers_with_and_the/repro.con`.

## Observed

On main 0bf8350a (built on OVH): `p == q` on two `*const Int` in a `trusted fn` compiles and both consumers return 42.

## Expected

[TYPE-6](../spec/types.md#TYPE-6) (OD26): arithmetic, shifts and comparisons take numbers only; pointer offset, difference and ordering are `ptr::offset` and named functions (TRUST-1).

## Cause

`Concrete/Check/Check.lean`, `comparisonRefusal?`: raw pointers are comparable in trusted or `Unsafe` code, and arithmetic on a pointer and an integer is admitted there too.

## Structural cause

Class 13: a decided rule with no enforcing site. The rule changed on 2026-10-29 and main still
implements the earlier, looser one.

## Structural owner

Exit 2q (one migration of std, examples and tests to the decided rules) with R-0499 (the Core
checker, by stable rule ID).

## Containment while open

The reproducer is pinned by check_example_outputs.sh's open-bug reproducers leg, and TYPE-6's
pending example records main's outcome. A program written to the decided rule is accepted today too.
