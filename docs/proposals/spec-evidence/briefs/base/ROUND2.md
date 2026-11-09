# Base migration round 2: review findings, approvals and released inductive laws (Codex writes; agy reviews)

State: the branch is at 4f68b10a7 (round 1 plus its regenerated axioms golden), and all seven gates pass. Round 1 was reviewed by three independent, scoped agy reviews. Two of them pass the official reviewer citation contract (checker 60934a193), and both have status CHANGES:
- _review/REV-base1a-agy-01025713b.md covers Slice0, Slice1, Examples and Basics (sha256 72146c9bcdf27a43ed46711d9c96bda1962378e47cf136192bdc12e0ca948b72);
- _review/REV-base1b-agy-01025713b.md covers Borrow, Brand and Cleanup (sha256 9edc2b5fbf0ab43981963f71fdcddd81208a4773d97d4d3912aff8c50f1aaca9).
The third, covering Authority, Checker, Rep and Observe, is being redone. Its findings and approvals come in round 3, so do not change approvals in those four modules in this round.

Read both reviews completely, with _review/OWNER_DECISIONS_2026-11-09.md (item 3, which releases the inductive laws), _review/VACUITY_CONVENTION_FINAL.md, _review/ROUND1.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. Do not reshape evidence around a confirmed gate defect (instance-false-discharge on by-decide/simp premises; subject-kind for inductive subjects, which the gate change will allow).

## Tasks
1. **Fix every REAL DEFECT in the two reviews.**
   - 1a finding 1: `ConcreteSpec.corr_mark_move` becomes a Law, with an Instance and a premise-preserving Planted example.
   - 1b finding 1: split `ConcreteSpec.Brand.reset_fresh` so that every quantifier and premise is a theorem parameter, and give each part closed Instances and Planted examples.
   - 1b finding 2: `ConcreteSpec.Cleanup.defer_cleanup_releases` must not be vacuous at its instance. Give each clause its own premises and closed Instances that discharge them, including a trapping case.
   - 1b findings 3 and 4: dead hypotheses. Removing an unused premise strengthens a Law. Before removing one, check against the spec that the premise is not an intended restriction. In particular, `e ≠ .abort` against the abort semantics, since a Law named `abort_runs_no_cleanup` exists. If it is intended, express it as the reviewer suggests rather than as a dead hypothesis.
   - 1a finding 2 is a preference. Apply it if it is the better long-term structure, and say why either way.
2. **Record the approvals from these two reviews only.**
   - For every declaration a review APPROVES, replace `(approved: PENDING)` with `(approved: REV-base1a-agy-01025713b)` or `(approved: REV-base1b-agy-01025713b)`, whichever review approved it. Match by fully qualified name and tag line.
   - A declaration this round changes materially, or one the review rejects, stays or becomes `(approved: PENDING)`. Report it.
3. **The released inductive laws.** These are the 28 Laws whose subject is an inductive relation, on hold in round 1. Under owner decision 3 (Q-SUBJ (a)), give each one premise-preserving Planted evidence under the final convention. The mutant must be a different inductive relation of exactly the subject's type, for example the relation with one extra overlapping or permissive constructor. Keep the existing Instances. Do not restate these laws or change their representations, except where a review finding requires it. Their N2 results wait for the gate change; do not reshape them around the current subject-kind refusal.
4. **Run N2 on the seven modules you touched and report.**
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Slice0,ConcreteSpec.Proofs.Slice1,ConcreteSpec.Proofs.Examples,ConcreteSpec.Proofs.Basics,ConcreteSpec.Proofs.Borrow,ConcreteSpec.Proofs.Brand,ConcreteSpec.Proofs.Cleanup'

   Report new FAILs other than PENDING approvals, subject-kind and instance-false-discharge.

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If a fix seems to need one, stop that item and put it in QUESTIONS with the options you see.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md, ISSUES.tsv or anything under the gate directory.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep elaboration memory bounded: split big proofs into lemmas per case.
- Final message: for each item, done or left (with why); the counts; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.
