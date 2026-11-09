# Base migration round 3: the third review and the 19 blocked inductive cases (Codex writes; agy reviews rounds 2 and 3 together)

State: the branch is at 36f5a3c58, round 2. _review/REV-base1c-agy-01025713b.md is the third scoped agy review of round 1, covering Authority, Checker, Rep and Observe. It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 44fc88527b2057622656918eb128fcb88179160888ff284e781a21a44bc09f8e). Read it completely, with _review/ROUND2_RESULTS.txt (your QUESTIONS section), _review/VACUITY_CONVENTION_FINAL.md and _review/OWNER_DECISIONS_2026-11-09.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. Do not reshape evidence around a confirmed gate defect.

## Tasks
1. **Fix the third review's findings.**
   - Blocker 1: the ten checker correspondence theorems become Laws with subjects the checker functions, with closed Instances and same-signature Planted mutants. `ConcreteSpec.covers_iff` stays a Representation lemma.
   - Blocker 2: Rep's case tokens must be simple case names, not qualified names that do not resolve.
   - Major 3: `ConcreteSpec.Rep.decodeNat_encodeNat` and `ConcreteSpec.Rep.int_roundtrip` become Laws, with evidence including a negative signed integer. Add signed-integer and empty-struct cases for `ConcreteSpec.Rep.rep_roundtrip`, and fix the helper class of `ConcreteSpec.Rep.encodeNat_length`.
   - Preferences 4, 5 and 6: apply each one if it is the better long-term structure, and say why either way.
2. **Record the third review's approvals.** For each declaration it APPROVES, replace `(approved: PENDING)` with `(approved: REV-base1c-agy-01025713b)`. A declaration it rejects is reclassified as task 1 says, or stays PENDING and is reported.
3. **The 19 blocked cases** (your round-2 QUESTION). The coordinator resolves this within the ratified convention, as assumed-yes pending owner confirmation.
   - Many of these Laws take a run's or a judgment's outputs as theorem parameters, for example `exec_deterministic` with both results as parameters. Fixing those parameters at the Instance makes the conclusion independently true, so no relation mutant can refute it.
   - Restate each such Law in the logically equivalent form that keeps the inputs and the first derivation as parameters and quantifies the outputs it constrains in the conclusion. C1 does this for its determinism law: `ConcreteSpec.Core.Exec.rules_deterministic` concludes `InputDeterministic`, which quantifies the second derivation.
   - Then the closed Instance supplies the inputs and the real derivation, and the Planted example at the same arguments refutes the quantified conclusion with the mutant's extra or different derivation.
   - The restated Law must be logically equivalent to the old one, never weaker. Prove the old form from the new one wherever the old form is used, or replace those uses. Keep every Instance.
   - If a Law has no premise-preserving mutant even in this form, the convention says it is not a Law. Reclassify it with a reason and `(approved: PENDING)`, and list it.
   - Do not amend the convention, and do not add mutation cases at other arguments.
4. **Run N2 on all eleven modules and report** new FAILs other than PENDING approvals, subject-kind and instance-false-discharge:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Slice0,ConcreteSpec.Proofs.Slice1,ConcreteSpec.Proofs.Borrow,ConcreteSpec.Proofs.Brand,ConcreteSpec.Proofs.Cleanup,ConcreteSpec.Proofs.Basics,ConcreteSpec.Proofs.Checker,ConcreteSpec.Proofs.Authority,ConcreteSpec.Proofs.Examples,ConcreteSpec.Proofs.Rep,ConcreteSpec.Proofs.Observe'

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If a fix seems to need one, stop that item and put it in QUESTIONS with the options you see.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md, ISSUES.tsv or anything under the gate directory.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep elaboration memory bounded.
- Final message: for each item, done or left (with why); the counts; each restated Law with its old and new statement in one line; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.
