# Base migration round 1: older slice, vertical and Examples proofs under the final N2 convention (Codex writes)

Branch wip/spec-base-migration, started at 0dbeba29e (wip/spec-core integration round 6; seven gates pass). The coordinator assigned this migration (last in the migration order: older slice/vertical/Examples proofs).

Files owned by this branch, all under ConcreteSpec/Proofs/: Slice0.lean, Slice1.lean, Borrow.lean, Brand.lean, Cleanup.lean, Basics.lean, Checker.lean, Authority.lean, Examples.lean, Rep.lean and Observe.lean. Do not edit other proof files; they belong to other contract branches.

_review/N2-base.tsv lists the N2 gate (checker 550fab85e) FAIL lines in these files, as code, location, declaration, detail. Read it, together with _review/VACUITY_CONVENTION_FINAL.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. A law that restates a validator, a guard or a constructor list is a shortcut. Reshaping a statement only to satisfy the gate, without improving the evidence, is a shortcut.

## Tasks
1. Classify EVERY authored theorem in these files with exactly one class:
   - `Law (subject …)` names the definitions it is about;
   - `Helper (<kind>). <reason> (approved: PENDING)`;
   - `Representation lemma. <reason> (approved: PENDING)`.
   A substantive soundness or semantic invariant is a Law, not a helper.
2. Give every Law at least one closed Instance (a full application of the law with every argument closed, whose type is defeq to the statement there) and, for every declared subject and every Instance case, a Planted example. The Planted uses a same-typed, premise-preserving mutant; its type is the law's hypotheses with the subject replaced by the mutant (also inside definitions that reach the subject) at the Instance's exact arguments, ∧ ¬ the mutated conclusion. Put the evidence examples in a namespace next to the law, as the other contracts do.
3. Fix every unknown-name: every backticked name in a tagged doc comment must resolve to a declaration from that context. Otherwise write it as plain prose. Keep each comment's meaning.
4. If a law cannot name a mutable definition subject, restate it over an operation parameter. If no premise-preserving mutant makes sense, it is not a Law: reclassify it.
5. Inductive relation subjects (Step, Steps, AnswerStep, or any other inductive) are an open owner question. Do not use an inductive as a subject and do not change any inductive representation. Where a law can only be about an inductive relation, tag it `Law (subject <that inductive>)` exactly as the C1 branch does (a known subject-kind finding awaiting the owner), and list it in QUESTIONS.
6. Never write an approval: leave `(approved: PENDING)` for the independent reviewer.

## Check
Run the N2 gate on these modules through the build semaphore, and confirm that no FAIL remains except PENDING approvals, the instance-false-discharge gate false positive on `by decide`/simp premises (confirmed, being fixed by the gates worker; do not reshape around it) and its cascade:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Slice0,ConcreteSpec.Proofs.Slice1,ConcreteSpec.Proofs.Borrow,ConcreteSpec.Proofs.Brand,ConcreteSpec.Proofs.Cleanup,ConcreteSpec.Proofs.Basics,ConcreteSpec.Proofs.Checker,ConcreteSpec.Proofs.Authority,ConcreteSpec.Proofs.Examples,ConcreteSpec.Proofs.Rep,ConcreteSpec.Proofs.Observe'
Do not edit anything under that gate directory.

## Rules
Do not change any owner-decided rule. If a task seems to need one, stop and ask in QUESTIONS. Commit each file's migration after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per file, what was done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
