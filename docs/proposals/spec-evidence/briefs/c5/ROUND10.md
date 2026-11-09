# C5 round 10: revert the gate workarounds (agy writes; Codex reviews)

State: the branch is at a76584c6, round 9. _review/REV-C5-codex-a76584c6.md is an independent Codex review of 902ce88a..a76584c6. It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 e3ad6f0705212431dffb33f044b8776f734a1d2d878774980791ae6b7d97e75c). Read it completely, together with _review/VACUITY_CONVENTION_FINAL.md and _review/ROUND9.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

Changing proofs so that a defective gate stops reporting them is a shortcut. It is the defect this round removes. A failing N2 line that comes from a confirmed gate defect is the correct result until the gate is fixed by the gates worker. Never make it pass by reshaping the evidence.

## Tasks
1. **Keep the good structure.** The review accepts the folding of the 60 rejected helpers into their public Laws and the 4 promoted Laws. Keep them.
2. **Remove the custom decision procedures.** These are the three major findings (Grant.lean:424-500 and 1437-1447; Own.lean:3704-3830):
   - remove `decidableBAllSafe`, `decidableBExSafe`, `decidableMemSafe`, `decidableIsPrefixSafe`, `decidablePairwiseSafe`, `decidableNodupSafe` and every instance override;
   - restore the ordinary decidable premises (`by decide` and the standard instances);
   - remove the manual Eq.rec/Decidable.rec eliminators and gate-oriented motive definitions in the erasure witnesses (Own.lean:3844-3906 and 4320-4378), and restore ordinary constructor inversion and the original helper statements. `step_det_none_inv` must conclude what it concluded at 902ce88a, not an arbitrary proposition;
   - remove the five reducible conclusion predicates (`EvalArgsOkErasure`, `EvalArgsTrapErasure`, `StepNextErasure`, `StepRequestErasure`, `StepErasure`) and restore the direct conclusions with matching evidence.

   Diff each restored declaration against 902ce88a: apart from the folding, it should match what the approval review saw.
3. **Approvals.**
   - A declaration the review marks REJECT because its proof was reshaped (`step_det_next_inv`, `step_det_none_inv`, `acceptedMove_checkerAccepted_inv`, `primFrameStuck_checkerAccepted_inv`, `bindMove_checkerAccepted_inv`) goes back to `(approved: APPR-C5-codex-902ce88a)` only if its statement and proof are restored exactly to the ones that review approved. Otherwise it gets `(approved: PENDING)`.
   - Every other declaration whose statement or proof differs materially from 902ce88a also gets `(approved: PENDING)`.
   - Keep every approval the review confirms.
4. **Run N2 and report.** Run the N2 gate on Grant and Own:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Grant,ConcreteSpec.Proofs.Own'

   Report every FAIL by category. instance-false-discharge and instance-application findings caused by the restored ordinary proofs are expected, and they stay. List them separately so they can go to the gates worker.

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If something seems to need such a change, stop that item and put it in QUESTIONS.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md, ISSUES.tsv or anything under the gate directory.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Final message: for each item, done or left (with why); the counts; the N2 FAIL list by category; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.
