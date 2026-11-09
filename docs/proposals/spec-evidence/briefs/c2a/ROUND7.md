# C2 round 7: the owner's constants decision (Codex writes; agy reviews)

State: the branch is at e19a4248c, and all seven gates pass. Its latest review, by agy, is a contract-valid ACCEPT. The owner has now decided the C2 constants question. Read _review/OWNER_DECISIONS_2026-11-09.md item 2, which carries the owner's verbatim quote, together with _review/ISS-0039.md, _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings.

## Tasks
1. **Remove `tryConvert` from CExpr.** Under option (a)+(i), initializers use only operations whose surface type is scalar. OD106 types the surface `try_convert` as `Result<T, OutOfRange>`, so `tryConvert` leaves the closed initializer subset. Remove it completely. This covers:
   - the constructor;
   - `refs`, `height` and every structural function;
   - the failure-site/op types;
   - `eval`;
   - the `ConstEval` rules;
   - `ty?`;
   - the child relations;
   - every proof, Instance, Planted and Accepted/Refused example that uses it.

   Do not replace it with another conversion: the owner chose (i), not (ii). Keep the lossless `convert`. Rework every law whose evidence used `tryConvert` so that its evidence is still substantive, using a remaining fallible operation (`plain`/`plainNeg`) where a refusal witness is needed. Do not weaken any law. If a law becomes vacuous without `tryConvert`, report it instead of keeping it in a trivial form.
2. **State the boundary in the module doc of Spec/Const.lean.** It must say:
   - Constants are scalar.
   - Checked forms (`Result<T, ArithmeticError>`) and `try_convert` (`Result<T, OutOfRange>`) are excluded from initializers because their surface types are not scalar.
   - The single reopening path is aggregate/enum constants (the owner's option (c)). Its preconditions are a ConstTy separate from C4's CTy, a revisit of `ofC_copy`, the Err-as-value question, and a motivating workload.

   Cite the owner quote as the authority. Do not cite docs/decisions.md rows that do not yet record it. Keep the doc short and normative. Remove the text about `tryConvert` being an allowed fallible operation.
3. **Check checked forms are excluded.** Confirm that no checked arithmetic form or `Result`-typed operation remains reachable from CExpr. Report what you found.
4. **Close ISS-0039 in ISSUES.tsv.** Its closure condition is met by item 1. Set its status to closed, following the file's existing format for closed rows; if no closed row exists yet, read scripts/checks/issues.sh to find the accepted status values and the closure-evidence convention. Name this round's commit as the evidence. Run `bash scripts/checks/issues.sh` and gate the commit on its exit status.
5. **Re-tag and run N2.** Re-tag any changed theorem under the final convention. Every new or changed Helper or Representation lemma gets `(approved: PENDING)`. Do not touch existing approvals of unchanged declarations. Run the N2 gate on Const and Scalar:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Const,ConcreteSpec.Proofs.Scalar'

   Report only new findings. instance-false-discharge findings are a known gate defect being fixed by the gates worker. Do not reshape the evidence around them.

## Rules
- You may edit Spec/Const.lean, Proofs/Const.lean and ISSUES.tsv for the decided change.
- Do not change any other owner-decided rule, the ratified vacuity convention, or the meaning of any other normative definition. Do not touch C4's CTy. If something seems to need such a change, stop that item and put it in QUESTIONS.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md.
- Never commit _review/ or scratch files.
- Lean core only, no new axioms, no sorry.
- Keep tool output short.
- Final message: for each item, done or left (with why), counts, commit SHAs, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.
