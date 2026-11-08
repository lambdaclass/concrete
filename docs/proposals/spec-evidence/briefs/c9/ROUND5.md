# C9 round 5 (reviewer agy: CHANGES, 3 blockers, 2 major, 1 minor). Fix on this branch.

HEAD is 4d11dabc2 (round 4 by Kimi + Codex; golden regenerated and committed; all seven gates pass on concrete-ci-2). Do not edit scripts/proofs/axioms.golden. All rules of _review/BRIEF.md apply: commit each piece after `nix develop -c timeout 1200 lake build ConcreteSpec` passes; do not push; never --no-verify. You (agy) write this round because Codex wrote round 4; read ROUND4.md and ROUND4b.md for the task and the owner decisions (items 8–10), which remain binding.

Read the review in full: `_review/rev-c9r4-agy.out.md`. Fix every REAL DEFECT with the structural fix it describes or a better long-term one: discharge by classical Prop equality (identify obligations by identity, not by propositional equality), `EncodedPremise.read_unknown` taking the decoder's result as a premise (state it over the reader as a parameter so a catch-all decoder is refuted; add the schema-version check), `OutsideFunctionEdit.persistentKeys` restating its field (make it a named Obligation `def … : Prop` and prove consequences for a concrete assignment), the `KeyConstruct` insertion laws (cover insertions anywhere, scope ordinals to the nearest named ancestor as the owner decided), the Instance/Planted witnesses that violate the FINAL convention, and `SiteTable.Valid` connected to `Span.LocationOk` and the trap line. If a fix needs a rule no decided row answers, list it under QUESTIONS with options and a recommendation.

Apply the FINAL vacuity convention (_review/VACUITY_CONVENTION_FINAL.md) to every theorem of the files you touch.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch: no law that holds by construction or restates its definition, no weakened statement, no premise that switches off an obligation. If staging is truly unavoidable, record the long-term design and its roadmap row in your final message.

## Final message
Commits (SHA + one line), per finding what changed and its Instance/Planted witnesses, every exemption left PENDING, the build/rules/trust_ban results you ran, and QUESTIONS ("None" if none).

## Coordinator rules (binding)
Best long-term design, never cut corners. Do not run reviewers (no codex/kimi/agy review invocations of your own; review is done separately). Do not write git notes.

## Build and commit gating (binding)
Run every Lean build through the machine-wide build semaphore: `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec` (at most two heavy builds on this machine). Gate every commit on the check's own exit status with `set -o pipefail`; never write `check | tail && git commit`. Do not run reviewers; do not write git notes.
