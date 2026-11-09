# C2 audit round 5: N2 diagnostics (Codex writes; agy wrote round 4)

State: the branch is at 9cb0ef7fe (round 4 by agy, golden, ISS-0039). All seven gates pass there. C2 acceptance stays conditional on ISS-0039 and the owner CTy answer; do not act on either here.

The N2 law-to-witness gate (checker wip/gate-vacuity 550fab85e, vacuity.lean sha256 8aabc56b) was run on this exact tree:
- _review/N2-c2-Const.txt: every FAIL line of the run on ConcreteSpec.Proofs.Const alone.
- _review/N2-c2-Scalar.tsv: this branch's checked-negation part of Scalar.lean, as code, location, declaration, detail.

Read both files completely, together with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. Reshaping a proof or a statement only to satisfy the gate, without improving the evidence, is a shortcut.

## Fix these (real spec defects)
1. **unknown-name** (Const 48, plus Scalar `Overflow`, `MIN` and the others in the tsv). Every backticked name in a tagged doc comment must resolve, in that declaration's namespace and open context, to a declaration. Locals and fields such as `ds`, `U.ofC_copy` and `isCopy`, expressions, keywords and dot-constructors all fail.
   - Write each one as plain prose, or reference a real declaration by a name that resolves there (qualify as needed).
   - Keep every doc comment's meaning.
2. **planted-proposition** at Const:1155 (evalConsts_sound/division), Const:1324 (admitted_copy/division_type) and Const:1354 (admitted_copy/division_value). The expected Planted is the law's hypotheses with the subject replaced by the mutant wherever it is reachable through definitions, ∧ ¬ the mutated conclusion. ConstsEvaluate unfolds to evalConsts and ConstsTyped unfolds to typeConsts, so those premises must be stated under the mutant.
   - Use mutants that keep the mutated premises true at the Instance arguments and falsify the mutated conclusion: premise-preserving, not vacuous.
   - Restate each Planted exactly.
3. **missing-planted**: admitted_copy (division_value needs a typeConsts Planted; division_type needs an evalConsts Planted) and semantic_failure_refuses (unused_after_prefix_admitted needs an evalConsts Planted; unused_after_prefix needs a ConstsAdmitted Planted). Each declared subject needs its own substantive Planted for every Instance case.

## Do NOT change
- **instance-false-discharge** (5) and its cascade: missing-instance 4, unpaired-planted 5, planted-proposition "no-valid-closed-instance" 5. This is a confirmed gate false positive (`by decide` proof terms such as List.decidableBAll and List.decidablePerm), and the gates worker is fixing it. Do not rewrite those Instances or premise proofs to avoid it.
- **unresolved-approval** (46 Const, 2 Scalar). Leave `(approved: PENDING)`. An independent review under the official citation contract fills these later. Never write an approval yourself.
- Pre-existing Scalar theorems outside checked negation: the C1 branch owns their classification.

## Check
After your edits, run the gate on Const through the build semaphore and confirm the targeted lines are gone and nothing new appeared:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Const,ConcreteSpec.Proofs.Scalar'
Do not edit anything under that gate directory.

## Rules
Do not change any owner-decided rule. If a task seems to need one, stop and ask in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
