# Bug 207 — check_binder_refs.sh aborted on a failed assertion, so its mutations had no verdict

<!-- bug-ledger
summary: `check_binder_refs.sh`'s Lean probe threw on its first failed check, so the gate exited before its verdict line and the mutation harness read families 55 – 57 as "no verdict"; the probe now reports each check and the gate counts every outcome. Regression vehicle: the mutation harness (families 55 – 57 KILLED)
coverage: check_binder_refs.sh aborted before its verdict on a failed assertion. Regression vehicle is the mutation harness (families 55-57 KILLED), not a tests/programs fixture (2026-10-23)
class: 11 — a gate that died on its first failed check gave the mutation harness no verdict
silent: no
ruling: Outside the spec: tooling — check_binder_refs.sh aborted before its verdict line, so mutation families 55–57 had no verdict
-->

**Status:** FIXED (branch `binder-refs-verdict`)
**Found:** 2026-10-23, running mutation families 55 – 57 for R-0492 step 2. The harness reported
each as `ERROR (scripts/tests/check_binder_refs.sh never reached the end it reaches on pristine
source — no verdict, not evidence)`; family 55 does the same on the R-0492 slice-1 base.
**Severity:** loud. CI was never misled: the gate still exited non-zero when an assertion failed.
What was lost is the mutation evidence. Three mutation families (a local reference that emits no
evidence node, eager binder frames, swapped position components) could not show that the gate
they name is load-bearing, because the gate never printed the verdict line the harness reads.

Confirmed real by Codex review, 2026-10-25: Throwing on failed assertions prevented mutation verdicts, violating ROADMAP.md R-0513’s coverage requirement; CI rejection itself remained correct.

## Reproducer

```
bash scripts/tests/test_mutation.sh --mutation 55
```

Before the fix: `ERROR (… never reached the end …)`. After it: `KILLED`.

## Cause

The gate's Lean probe asserted with `throw (IO.userError …)` on the first false check. Lean's
`#eval` failed, `lake env lean` exited non-zero, and the shell printed `FATAL: binder-ref probe did
not elaborate` and called `exit 1` before the `BINDER-REFS: PASS=… FAIL=…` line. Under any mutation
the probe was meant to catch, the gate therefore aborted instead of failing, and the harness
correctly refuses to count an abort as a kill.

## Fix

The probe prints `  FAIL <label>` for a false check and continues. The shell turns every probe
outcome into a counted verdict: a probe that does not elaborate, emits a diagnostic, or prints
fewer than its eight verdicts is a FAIL. The gate always reaches its summary line and exits
non-zero when anything failed.

## Structural cause

A gate's failure path was an early exit rather than a verdict. The mutation harness can only
count a gate that reports, so a gate that dies on the first failed check gives the same output
for a real kill and for a crash: no verdict to count. That is class 11 of
[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md) (absence read as a fact, here the absence of a
verdict).

## Structural owner

R-0513 (trustworthy gate execution). The gate-hygiene rule that every gate prints its verdict line
on every path, failure paths included, belongs to the gate-hygiene checks that R-0513 owns.

## Regression

Mutation families 55 – 57 report KILLED (`test_mutation.sh --mutation 55|56|57`), and the pristine
gate passes. There is no `tests/programs` fixture: the defect is in the gate, and its regression
vehicle is the mutation harness's verdict on the gate.
