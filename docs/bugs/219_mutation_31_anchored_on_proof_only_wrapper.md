# Bug 219: mutation 31 mutated a copy of the bug-061 arm that no gate executes

<!-- bug-ledger
summary: mutation family 31 (bug 061 restored) was anchored on the `.applyVar` arm of the proof-only `cExprToPExpr` wrapper, which no gate executes, so it survived; it now mutates the arm of `cExprToPExprImpl`, the extractor the compiler runs. Regression vehicle: the mutation harness (family 31 KILLED)
coverage: mutation 31 was anchored on the proof-only cExprToPExpr wrapper and survived. Regression vehicle is the mutation harness (family 31 KILLED), not a tests/programs fixture (2026-10-23)
class: 12 — the bug-061 extraction arm in an executed and a proof-only copy; the mutation hit the unexecuted one
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `vp-r0492-backends`, R-0492 step 4)
**Found:** 2026-10-23, Codex delta review of R-0492 step 4 (the anchor had to follow ProofCore's
explicit `binding.name`). Confirmed by running the family on main `dcc48dc1`:
`[31/100] ... SURVIVED <-- TEST GAP`.
**Severity:** loud. No compiler output was wrong. What was lost is evidence: mutation family 31,
which restores bug 061 (a parameter application extracted as a definition call), could not show
that `check_proofcore_callable_identity.sh` guards the extractor, because the code it mutated never
ran.

Confirmed real by Codex review, 2026-10-25: Mutation 31 changed an unexecuted wrapper, defeating ROADMAP.md’s demonstrated-enforcement requirement; SURVIVED did not establish a compiler regression.

## Reproducer

```
bash scripts/tests/test_mutation.sh --mutation 31
```

On main `dcc48dc1`: SURVIVED. With the anchor moved: KILLED (gate, confirmed green once restored).

## Cause

`Concrete/Proof/ProofCore.lean` has two copies of the `.call (.indirect binding)` arm. One is in
`cExprToPExprImpl`, the extractor in the mutual block that `cStmtsToPExpr` and every report run. The other is
in `cExprToPExpr`, the wrapper outside that block that exists so the ProofSoundness theorems can reduce
literal cases by `rfl`; no executable path calls it. The family's anchor
(`some (.applyVar binding pargs)` followed by `| .structLit ...`) matched exactly once, in the
wrapper, so the harness's "matches exactly once" freshness check passed while the mutant changed
nothing a gate executes.

## Fix

The anchor now names the arm of `cExprToPExprImpl` (the line followed by `| .ifExpr ...`), with a
comment saying why. Family 31 is KILLED by `check_proofcore_callable_identity.sh`.

Structural follow-up (2026-10-24, branch `vp-219-wrapper`): the copies are gone. `cExprToPExpr` and
`cStmtsToPExprK` are now bare delegations to `cExprToPExprImpl` / `cStmtsToPExprKImpl`, and the
ProofSoundness preservation theorems (R-01 – R-20) unfold the implementation's equations instead of
a wrapper's arms, so they are about the code the compiler runs. `check_proofcore_callable_identity.sh`
fails if either public name grows an arm again or a second site builds `.applyVar`, with controls
for both.

## Structural cause

One decision (a locally bound callable extracts as `.applyVar`, never as a global `.call`) is kept
in two places, and only one of them is executed. A check aimed at the unexecuted copy reads as a
check of the decision. Class 12 of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md) (one fact kept
in two places).

## Structural owner

R-0513 (trustworthy gate execution): a mutation anchor must sit on code a gate executes; "matches
exactly once" does not establish that. The duplicate arms were removed by the follow-up above, which
makes the class unrepresentable for these two extractors.

## Regression

Mutation family 31 reports KILLED. There is no `tests/programs` fixture: the defect is in the
mutation harness's target, and its regression vehicle is the harness's verdict.
