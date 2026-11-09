# N2 reproducer: instance-application on a law whose conclusion is an inline match (Codex writes)

The gates worker asked for this through the coordinator. Best long-term design, never cut corners: the reproducer must show the gate defect exactly, with no workaround and nothing extra.

## Background
The vacuity gate N2 at checker 550fab85e (vacuity.lean sha256 8aabc56b...) reports `instance-application` on five Instance examples in C5 at 6efdce61. They are listed in _review/C5-instance-application-6efdce61.txt, and the source is _review/Own-6efdce61.lean; see for example line 4224, Instance of `step_next_erasure`, and the law's statement.
- Each such Law's conclusion is an inline `match`.
- Each Instance is, as far as the reviewers can tell, a genuine full closed application of the law, so the finding looks like a gate defect: the defeq/application check does not see through the matcher of an inline-match conclusion.
- An earlier round hid the matches behind reducible definitions to make N2 pass. Reviewers rejected that as a workaround; the gate must be fixed instead.
- _review/F3b-repro-example/ is the reproducer format the gates worker accepted for another N2 defect. Follow it: a README stating the run, expected and actual results, the controls and the suspected cause; a ConcreteSpec.lean root; ConcreteSpec/Repro.lean; and the recorded gate output.

## Task
1. Write a minimal, bounded, self-contained Lean 4.34.1 core-only reproducer in this clone, under `scripts/proofs/audits/n2-repro-instance-application/`. It needs:
   - its own ConcreteSpec.lean root importing ConcreteSpec.Repro;
   - ConcreteSpec/Repro.lean, one file;
   - a ROADMAP.md stub if the driver needs one;
   - README.txt;
   - gate-550fab85e-output.txt.

   Repro.lean must contain:
   - one small definition as the subject;
   - one Law, tagged under the final convention (`Law (subject ...)`), whose conclusion is an inline `match` on the subject's result, like `step_next_erasure`;
   - a closed, real Instance that is a full application of the law;
   - a premise-preserving Planted example with a same-typed mutant;
   - controls: the same law with its conclusion stated without an inline match (for example as an equation or an existential), whose Instance N2 accepts; and, if useful, a second control that narrows the trigger (match on a literal versus on a function result).

   Every tag and backticked name must resolve. No sorry, no axioms beyond the standard three, no native_decide.
2. Run the gate exactly as the F3b README does, from the pristine gate copy, and record its output in gate-550fab85e-output.txt:
  bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 1500 python3 scripts/proofs/vacuity_driver.py --source <absolute path of the reproducer directory> --scope ConcreteSpec.Repro'

   Expected: no FAIL lines. Actual: `instance-application` on the inline-match Instance, and nothing on the controls. If the gate does not reproduce the finding on your first minimal version, change the reproducer until it matches the real C5 shape more closely (implicits, a match on an Option or sum result, a match in a reducible position) until it does. Report which feature triggers it.
   - If it never reproduces, say so. If you conclude that the real C5 Instances are not full applications, say so too, and explain why: then the finding is a spec defect, not a gate defect, and the reproducer is not needed.
3. Make sure the repository's own checks are not disturbed by the new directory: `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh` and `nix develop -c timeout 600 bash scripts/spec/rules.sh` must still pass. If a check scans every .lean file in the tree, report it and keep the reproducer compliant.
4. Commit only the reproducer directory, with the message "n2-repro: instance-application on an inline-match law conclusion (gate 550fab85e)". Do not push.

## Rules
- Edit nothing outside the reproducer directory, and nothing under the gate copy.
- Do not edit docs/decisions.md or ROADMAP.md (the root one).
- Never commit _review/.
- Run Lean builds through `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh`. Gate the commit on the exit status of the check itself, with `set -o pipefail`.
- Do not run reviewers, and do not write git notes.
- Final message: the commit SHA, the triggering feature, the gate output summary, and QUESTIONS.
