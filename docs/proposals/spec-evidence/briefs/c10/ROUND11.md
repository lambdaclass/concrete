# C10 round 11: what happens downstream when the loader refuses a project (Codex writes; agy reviews)

State: the branch is at de0e8bfd2, and all seven gates pass. Its latest review, by agy, is a contract-valid ACCEPT.

The owner has decided the source-path question. Read _review/OWNER_DECISIONS_2026-11-09.md item 1, which carries the owner's verbatim quote. C9 has implemented the decision at 115a73b2d on wip/spec-c9, fetched here; its briefs are _review/C9-ROUND12.md and _review/C9-ROUND13.md. That head has a contract-valid agy ACCEPT. It provides structured `SourcePath`, a strict parser with typed refusals, and a whole-project loader that admits the complete input or refuses it, naming the input.

## Interpretation of "C10 owns the refused-input examples" (coordinator, recorded here as binding)
The coordinator interpreted its own relay of the decision text; this is not an owner decision. The rule is one owner per fact:
- **C9 owns** the loader refusals and their Refused examples. Each refusal law and its Refused example live together in C9. C10 must not restate them.
- **C10 owns** the end-to-end consequence. A project that C9 refuses yields no launchable image, so there is no launch, no trap site and no exit-125 startup.
- **Startup refusal (125) is a different run-time event.** It must never be conflated with load refusal.
- **Scope.** Write one end-to-end law or example per refusal CLASS, not per character. State it over C9's admission predicate by reference, with a Planted mutant: a loader that admits the bad path must break it.

## Best long-term design (owner rule, binding)
Judge everything by the best long-term design. Never cut corners. Shortcuts are findings. There must be exactly one owner and one statement per fact: no duplicated refusal rules.

## Tasks
1. **Merge C9.** `git merge --no-ff 115a73b2d`. Never rebase. Resolve conflicts by ownership: C9 owns Ident and the loader, C10 owns Frame. Record in the merge message that this branch now depends on C9 at that SHA.
2. **Check C9's Refused examples.** Check that C9 has a Refused example for every refusal class of its parser and loader: malformed path, out-of-project path, duplicate file, duplicate site, site without a loaded file, control character, backslash. Do NOT add any missing ones here; list them in your final message so C9 adds them.
3. **The end-to-end connection.** Connect C9's loader to C10's launch boundary (`Launcher` / `Startup` in Spec/Frame.lean), stated over C9's admission predicate by reference:
   - define, or reuse if it exists, the step from an admitted project to a launchable artifact;
   - prove that a refused project has no launchable artifact, so no `Launcher.Run`, no `Startup` outcome and no trap report can come from it;
   - prove that this is distinct from startup refusal (125): load refusal produces no startup outcome at all, while startup refusal is a run-time `Startup` outcome of an admitted project.

   Write one Law per refusal class, or one Law parameterized by the class if that is the better long-term structure (say which and why). Each needs a closed Instance and a Planted example whose mutant is a loader that admits the bad path, which must break the law.
4. **Re-tag and run N2.** Re-tag under the final convention. New exemptions get `(approved: PENDING)`. Run N2 on Frame:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 2400 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.Frame'

   Report new FAILs other than PENDING, instance-false-discharge and their cascades.

## Rules
- Do not change any owner-decided rule, the startup statuses, the ratified vacuity convention, or the meaning of any other normative definition.
- Do not edit C9's files except as the merge requires.
- If anything needs a decision the owner has not made, stop that item and put it in QUESTIONS with the options.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep elaboration memory bounded.
- Final message: for each item, done or left; the new laws with their statements in one line; the Refused examples missing from C9; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
