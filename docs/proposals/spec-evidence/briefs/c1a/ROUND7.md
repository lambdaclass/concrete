# C1 round 7: inductive subjects released; determinism over independent operational rules (Codex writes; agy reviews)

State: the branch is at 368bd96f5 (or at the axioms.golden commit the gates lane adds on top; use HEAD). Its latest review, by agy, is a contract-valid ACCEPT.

The owner has decided Q-SUBJ. Read these first:
- _review/OWNER_DECISIONS_2026-11-09.md item 3, which carries the owner's verbatim quote;
- finding 1 of _review/APPR-C1-codex-5531d9e34.md, the determinism-by-construction defect;
- _review/N2-c1-Exec.txt: its subject-kind lines list the laws that were on hold;
- _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. A determinism proof that compares two results of the same function, or a relation defined from the function it is meant to check, is the defect this round removes. It is not a solution.

## Tasks
1. **Reclassify the two determinism theorems.** `ConcreteSpec.Core.Exec.step_deterministic` and `ConcreteSpec.Core.Exec.step_silent_deterministic` become `Representation lemma.` entries, with a reason and `(approved: PENDING)`. They are facts about the function-backed `AnswerStep`, never soundness evidence.
2. **Determinism over independently specified operational rules.**
   - Specify C1's transition rules as an inductive relation stated directly over configurations and statement forms, one rule per construct and case. It must be stated without `step`, its continuation or any executable function, and it must keep `Operands`/`allows` as the only external interface. It belongs in ConcreteSpec/Spec/Exec.lean, because it is normative.
   - Prove its correspondence to the executable `step`/`Step`/`AnswerStep`, in both directions (soundness and completeness of the function against the rules), as Laws with subject `step`.
   - Prove determinism Laws over the rules by rule inversion: internal steps are unique, and a fixed external answer fixes the label and the next configuration. The subject is the rule relation, which is now allowed as an inductive subject.
   - Give closed Instances, and premise-preserving Planted mutants that are same-signature inductive relations, for example a rule set with an extra overlapping rule for one construct.
   - Keep every existing Law about `Step`/`Steps` true. Restate one only if it is the better long-term statement, and never weaken it.
   - The owner's decisions do not fix the granularity of the rules (for example big-step or small-step for expressions). Choose the granularity that matches the existing small-step `Step` exactly, so the correspondence is a bijection on transitions. Report any construct where it cannot be.
3. **Release the held laws.** These are the 36 laws whose N2 subject-kind finding names `Step`, `Steps` or `AnswerStep`. Give each Law that lacks Planted evidence a premise-preserving Planted example under the final convention, with a mutant that is a different inductive relation of exactly the subject's type. Keep the existing Instances.
   - Per the owner, their N2 results wait for the gate change, which the gates worker is doing. Do not reshape the evidence around the current gate's subject-kind refusal.
   - Do not restate these laws or change their representations, except as task 2 requires.
4. **Re-tag and run N2.** Re-tag every changed or new theorem under the final convention. New exemptions get `(approved: PENDING)`. Existing approvals of unchanged declarations stay. Run N2 on Exec and Scalar:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Exec,ConcreteSpec.Proofs.Scalar'

   Report only new findings other than subject-kind and instance-false-discharge. Both are gate-side issues in progress.

## Rules
- You may edit ConcreteSpec/Spec/Exec.lean only to add the rule relation and its doc. Do not change what `step` does.
- Do not change any other owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If something seems to need such a change, stop that item and put it in QUESTIONS with the options you see.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv.
- Never commit _review/ or scratch files.
- Lean core only, no new axioms, no sorry.
- Keep tool output short.
- Final message: for each item, done or left (with why); each new definition and theorem with its statement in one line; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Never write `check | tail && git commit`. Do not run reviewers, and do not write git notes.

## Restart note (coordinator, 06:15Z)
The first run of this round was killed by the out-of-memory killer at 16 GB, during a Lean build of Proofs/Exec.lean. Its uncommitted partial edits are still in the working tree (Spec/Exec.lean and Proofs/Exec.lean).
- Inspect them first with `git diff`. Keep what is right, and fix or discard the rest.
- Keep elaboration memory bounded: no large `decide` over rule relations and no exhaustive `cases` blowups across many constructors at once. Split big proofs into lemmas per construct.
- The limit is now 32 GB. A proof that needs more is a design smell to fix, not to work around.
