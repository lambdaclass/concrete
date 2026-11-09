# C6 round 7: source paths under the owner's decision, on top of C9's source identity (Codex writes; agy reviews)

State: the branch is at 5120f5210, and all seven gates pass. Its latest review, by agy, is a contract-valid ACCEPT.

The owner has decided the source-path question, which is REVIEW4 finding 2 and its QUESTION. Read these first:
- _review/OWNER_DECISIONS_2026-11-09.md item 1, which carries the owner's verbatim quote;
- _review/REVIEW4-codex.md finding 2;
- _review/ihead-r1-questions.md: the integration merge found that C6 and C9 conflict on Span and SiteTable.Valid;
- _review/VACUITY_CONVENTION_FINAL.md, _review/BRIEF.md and _review/COMMON.md.

Under the decision, C9 owns source identity and the trap-site table, and C6 drops its two now-redundant assumptions from its trap theorem. C9's implementation is at 115a73b2d on wip/spec-c9: structured `SourcePath`, a strict parser, a whole-project loader, laws, and a consumer interface for C6. It is fetched here. It is under review, so this round depends on it; that dependency is recorded, and a later C9 fix will be merged on top.

## Best long-term design (owner rule, binding)
Judge everything by the best long-term design. Never cut corners. Shortcuts are findings. There must be exactly one way to do each thing: no two site-table validity predicates, and no C6-private path check next to C9's.

## Tasks
1. **Merge C9.** `git merge --no-ff 115a73b2d`. Never rebase. Resolve conflicts by ownership: C9 owns Ident, Span's fields and the source/site tables; C6 owns Observe's observation, report and trap theorems.
2. **Replace C6's string path validation.** Remove C6's string `PathOk` and the filtering `sourceSites`. There must be one structured path type and one admission boundary, C9's loader. The ExecAdapter, or whatever supplies sites to reporting, takes C9's loaded project (source table and site table), not a raw list.
3. **Drop the two premises from the trap theorem.** `runFrom_source_trap` drops `found` and `pathOk`. Their content now follows from the loaded project, through C9's consumer theorem, with no run-time re-check. Keep every other conjunct: path identity, line, byte column. Restate its Instance and Planted evidence under the final convention. The mutants must still reject wrong file identity and scalar columns, and they must now also show that an unloaded or unvalidated path cannot be reported. Use a project whose loader refused that path; nothing is filtered.
4. **One SiteTable.Valid.** Keep a single predicate: C9's, with source resolution and LocationOk. Adapt C6's uses. Any path-only fact that C6 needs is a consequence, proved as a Helper, not a second predicate.
5. **trapLine_injective.** C9's Span now carries byte offsets that the decided trap line does not render. Restate C6's `trapLine_injective` as injectivity on exactly the rendered fields (kind, site, path, line, column). Keep C9's counterexample showing that unrendered offsets do not affect the line. Keep the decided renderer byte-identical. This is not a change of rule: before C9 those fields were the whole Span. Keep its Instances and Planted evidence substantive.
6. **Re-tag and run N2.** Re-tag under the final convention. A changed declaration whose approval no longer covers its text gets `(approved: PENDING)`. Run N2 on the C6 modules:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 2400 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.ObserveReport,ConcreteSpec.Proofs.ObserveSource,ConcreteSpec.Proofs.ObserveEvidence,ConcreteSpec.Proofs.ObserveOutput,ConcreteSpec.Proofs.ObserveEnvironment,ConcreteSpec.Proofs.ObserveTransform,ConcreteSpec.Proofs.Observe2'

   Report new FAILs other than PENDING, instance-false-discharge and their cascades.

## Rules
- Do not change any owner-decided rule, the decided trap-line format or statuses, the ratified vacuity convention, or the meaning of any other normative definition.
- Do not edit C9's files except as the merge requires. If C9's interface is insufficient, stop that item and say exactly what is missing; do not fork it.
- If anything needs a decision the owner has not made, stop that item and put it in QUESTIONS with the options.
- Commit each piece after the build passes. Do not push.
- Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv.
- Never commit _review/.
- Lean core only, no new axioms, no sorry.
- Keep elaboration memory bounded.
- Final message: for each item, done or left; each changed Law with its old and new statement; commit SHAs; QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`. Gate every commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
