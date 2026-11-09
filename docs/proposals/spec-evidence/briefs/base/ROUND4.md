# Base migration round 4: refine the quantified restatements (Codex writes; agy reviews)

State: the branch is at 980e80550 (round 3 plus its regenerated golden), and all seven gates pass. Three scoped agy reviews of rounds 2 and 3 (4f68b10a7..8601f966a) pass the official reviewer citation contract (checker 60934a193):
- _review/REV-base23a-agy-8601f966a.md covers Slice0, Slice1, Examples and Basics: ACCEPT (sha256 53af5317ef05b3063914acd884495ea06dbd1d8fe0b0cdb1bb519aee05752fff);
- _review/REV-base23b-agy-8601f966a.md covers Borrow, Brand and Cleanup: CHANGES (sha256 640e4332a83f7adc51aae6e7800d091be4d567eed8dcf0bf87a1a16f01984182);
- _review/REV-base23c-agy-8601f966a.md covers Authority, Checker, Rep and Observe: ACCEPT (sha256 640a520979ffe11a8943299b387570a61b2ad7f4979fc38f30740a7824120e27).
Read all three completely.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners. Shortcuts are findings.

## The refined coordinator resolution (assumed-yes AY-1, revised; pending owner confirmation)
Round 3 moved inputs, input premises and the only derivation into wrapper conclusion predicates. The Instances then closed nothing, which repeats the round-1 blocker. That is wrong. Quantify an output in the conclusion only where the Instance still supplies:
- all inputs;
- all input premises;
- a real derivation.

Use these forms instead, in this order:
1. **Refusal laws:** keep every input and premise as a theorem parameter, and state the refusal as the conclusion (`¬ Ok ...` / `¬ RangeOk ...`), as `ConcreteSpec.Borrow.view_not_moved` and `ConcreteSpec.Brand.brand_mismatch_refused` already do. A permissive inductive mutant then refutes it at the Instance's exact arguments.
2. **Laws whose conclusion is fixed by a normative function or predicate:** keep the original parameters and the real derivation, and take that function or predicate as the (additional) subject, with a premise-preserving function mutant. Examples are `ConcreteSpec.Cleanup.expected` and `ConcreteSpec.Cleanup.targets`, as review 23b finding 2 suggests.
3. **Determinism and uniqueness laws:** keep the first derivation as a parameter and quantify the second derivation in the conclusion, as round 3 did for `exec_deterministic`. That form is accepted.
4. **Otherwise:** if a Law has no premise-preserving mutant at a fixed accepted derivation, the convention says it is not a Law. Reclassify it as a Helper with a reason and `(approved: PENDING)`, while another Law of the module supplies closed derivations (as review 23b suggests for `reg_inv`, `ok_keeps_locked` and `run_inv`). List every reclassification.

## Tasks
1. Fix every finding of review 23b (blockers 1 and 2, majors 3 and 4, and any minor) by the forms above. Remove the wrapper predicates that only carry inputs. `range_open_mut_freezes_root` is stated in terms of `RangeOk` and handled as finding 4 says. Keep every law at least as strong as its round-2 statement.
2. Apply any finding of reviews 23a and 23c (both ACCEPT, so only minors or nits, if any) where it is the better long-term structure.
3. Record the approvals. For each declaration a review APPROVES, replace `(approved: PENDING)` with that review's reference (`REV-base23a-agy-8601f966a`, `REV-base23b-agy-8601f966a` or `REV-base23c-agy-8601f966a`), unless this round changes the declaration materially. Report every PENDING left.
4. Run N2 on all eleven modules as in round 3. Report new FAILs other than PENDING approvals, subject-kind, instance-false-discharge and their cascades.

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If something seems to need such a change, stop that item and put it in QUESTIONS.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md, ISSUES.tsv or anything under the gate directory.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep elaboration memory bounded.
- Write any results file inside _review/.
- Final message: for each item, done or left; each changed Law with its old and new statement and the form used; the reclassifications; counts; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
