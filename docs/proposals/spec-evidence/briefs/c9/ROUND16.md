# C9 round 16: fix the round-15 review findings (Codex writes; agy reviews)

State: the branch is at 8a37ceaee (round 15), or at the axioms.golden commit the gates lane adds; use HEAD. _review/REV-C9-agy-8a37ceaee.md is an independent agy review of round 15. It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 e10743966eb748b2f2187eb283685cfac05ad7151712fcafbfe77c6a49acc902), and every location and identifier it cites resolves. The coordinator confirmed Finding 1 by reading scripts/spec/rules.py and ConcreteSpec/Spec/Rules.lean.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings.

## Findings to fix
1. **Unregistered rule citations (blocker).** The seven new `Refused (rule `ConcreteSpec.Core.Observe.LoadedProject.load`)` tags cite a rule that ConcreteSpec.Core.registeredRules does not register, so rules.sh fails. Registering the loader as a rule assigns a rule ID, and rule IDs are an owner decision. The coordinator has sent that question up; do NOT edit ConcreteSpec/Spec/Rules.lean.
   The coordinator confirmed this: rule IDs are the owner's, and option (a), registering the loader, is on the owner's page as the recommended assumed-yes. Do not add any import from Rules to Observe.
   Record the pending state in ONE place in the tree: an ordinary (non-doc) `--` comment directly above the first loader Refused example in ConcreteSpec/Proofs/Observe2.lean. It must say:
   - the loader's Refused examples are intentionally unregistered, pending the owner's decision on registering ConcreteSpec.Core.Observe.LoadedProject.load as a rule;
   - the exact switch, if the owner says yes: add `⟨``ConcreteSpec.Core.Observe.LoadedProject.load, 1⟩` to ConcreteSpec.Core.registeredRules in ConcreteSpec/Spec/Rules.lean (with the import of ConcreteSpec.Spec.Observe that this needs), and change each loader `Refused:` tag to `Refused (rule `ConcreteSpec.Core.Observe.LoadedProject.load`):`.
   Do not repeat the note anywhere else. Make sure rules.py does not read the comment as a citation (it scans all comments): write the rule name without the "(rule" prefix, and check that rules.sh passes.
   For now, write each loader refusal tag in the existing unregistered form that the tree already uses, `Refused:` with no `(rule ...)` (see ConcreteSpec/Spec/Observe.lean and ConcreteSpec/Proofs/Ident.lean for precedent), and keep the class wording after the colon. Then run `nix develop -c timeout 600 bash scripts/spec/rules.sh` and gate on its exit.
2. **Helper tag on an anonymous example (blocker).** A Helper classification and an `(approved: ...)` tag belong only on a named declaration. Pick the form the final convention (VACUITY_CONVENTION_FINAL.md) supports, by the best long-term design, and say why:
   - if the control-character refusal needs a reusable computation, make it a named Helper theorem in SourceLoadDemo with `(approved: PENDING)`, and put the Refused tag on the example that shows the refusal;
   - otherwise keep a single example carrying only the Refused tag.
   The Instance and Planted pair for the class must stay complete and closed.
3. **Every loader refusal class and malformed reason needs its Refused example tagged (major).** Add Refused tags to the loader examples the review lists:
   - badLocation (invalid location);
   - dotInput, absoluteInput, driveInput and emptyComponentInput (malformed reasons).
   Check them against the constructors of ConcreteSpec.Core.Observe.SourceLoadRefusal and ConcreteSpec.Core.SourcePath.Reason, so that each constructor and reason has a loader-level Refused example. List the mapping.
4. **Doc nit.** The doc comment of load_refuses_malformed should name the control-character reason with the others. Fix it.

## Rules
Do not change any rule or normative definition, and do not edit ConcreteSpec/Spec/Rules.lean. Commit after the build, rules.sh and trust_ban.sh pass. Run N2 on the C9 modules as in round 15, and report only new FAILs. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: each finding and how you fixed it, the constructor-to-example map, the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
