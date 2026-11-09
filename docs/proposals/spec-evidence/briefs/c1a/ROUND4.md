# C1 audit round 4: N2 diagnostics (agy writes; Codex wrote round 3)

State: the branch is at ccca4abaa (round 3 by Codex plus the regenerated golden). All seven gates pass there. The N2 law-to-witness gate (checker wip/gate-vacuity 550fab85e, vacuity.lean sha256 8aabc56b) was run on this exact tree. Its FAIL lines for files this branch owns are:
- _review/N2-c1-Exec.txt: the gate run on ConcreteSpec.Proofs.Exec alone.
- _review/N2-c1-Scalar.tsv: Scalar.lean, from the full run, as code, location, declaration, detail.

Read both files completely, together with _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Shortcuts are findings. Reshaping a proof or a statement only to satisfy the gate, without improving the evidence, is a shortcut.

## Fix these (real spec defects)
1. **unknown-name** (Exec 228, Scalar 3). The final convention requires every backticked name in a tagged doc comment to resolve, in that declaration's namespace and open context, to a declaration. Backticks around expressions, locals, binders, numerals, keywords, dot-constructors (`.enter`) and bare names that do not resolve in context (for example `StuckStep` inside CoreEvidence, which lives in Refusal) all fail.
   - Fix each one: either write it as plain prose without backticks, or reference a real declaration by a name that resolves from that context (qualify it when needed).
   - Keep the meaning of every doc comment. Do not delete content to silence the gate.
   - Case labels in `case` fields are exempt.
2. **planted-proposition, Scalar.lean toBits_shl case `concrete`** (Planted near Scalar.lean:797). The gate builds the expected Planted from the law's hypotheses with the subject replaced by the mutant everywhere it is reachable through definitions. `checked … .shl` reaches `toBits` through `wrap`, so the premise must be stated under the mutant too.
   - Choose a same-typed mutant that keeps the mutated premise true at the Instance's exact arguments and falsifies the mutated conclusion. It must be premise-preserving, not vacuous.
   - Then state the Planted exactly as convention requires: mutated hypotheses ∧ ¬ mutated conclusion.
3. **missing-planted, `destroyCall_enters` case `consume_tok`, subject `ConcreteSpec.Core.Exec.DestroyProtocol.destroyCall`**. Each declared subject needs its own Planted for every Instance case. One subject's pair cannot cover its siblings. Add a substantive one.

## Do NOT change (each is decided elsewhere)
- **instance-false-discharge** (11) and its cascade: missing-instance 11, unpaired-planted 11, planted-proposition "no-valid-closed-instance" 11. This is a confirmed gate false positive (`by decide`/simp proof terms), and the gates worker is fixing it. Do not rewrite these Instances or their premise proofs to avoid it.
- **subject-kind** (36 Laws with subject Step/Steps/AnswerStep). This is an owner question. Leave these Laws, their tags and the Step/Steps/AnswerStep representations exactly as they are.
- **unresolved-approval** (84 `(approved: PENDING)`). Leave PENDING. An independent review under the official citation contract fills these in a later round. Never write an approval yourself.

## Check
After your edits, run the gate yourself on this module to confirm that the unknown-name, Scalar planted-proposition and missing-planted lines are gone and nothing new appeared. It must run through the build semaphore:
  bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Exec,ConcreteSpec.Proofs.Scalar'
Export PWD_SRC first: `export PWD_SRC="$PWD"`. Do not edit anything under that gate directory.

## Rules
Do not change any owner-decided rule. If a task seems to need one, stop and ask in QUESTIONS. Commit each piece after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md or ROADMAP.md. Never commit _review/ or scratch files. Lean core only, no new axioms, no sorry. Keep tool output short. Final message: per item, done or left (with why), commit SHAs, QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
