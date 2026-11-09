# C9 round 15: the loader-level Refused example for control characters (Codex writes; agy reviews)

State: the branch is at 73e76ea0c, or at the axioms.golden commit the gates lane adds; use HEAD. Its latest review is a contract-valid agy ACCEPT.

C10, in its round 11, checked C9's Refused examples against every refusal class of the parser and loader. Under the coordinator's ruling, C9 owns the loader refusals together with their Refused examples. C10 reports one gap: the loader has no Refused example for a control character in a path. The parser-level examples exist.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Task
1. Add the loader-level Refused example for a control-character path (AY-3, assumed-yes pending owner confirmation): a project whose source file path contains a control character is refused by the loader, with the typed reason and the offending input named. Tag it under the final convention as a Refused example of the loader's rule. If the loader's refusal law needs an Instance/Planted pair for this class to be complete, add one, with a mutant loader that admits the control character.
2. Re-check every refusal class (malformed, out-of-project, duplicate file, duplicate site, site without a loaded file, control character, backslash). Each must have a loader-level Refused example; list them.
3. Run N2 on Ident as before, and report only new FAILs.

## Rules
Do not change any rule or normative definition. Commit after the build passes. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: what you added, the class-to-example list, the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
