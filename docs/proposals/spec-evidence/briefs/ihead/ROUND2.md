# Integration head, merge round 2 (Codex writes; agy reviews)

Goal: ONE integration head that merges every accepted spec head, including the base migration. The coordinator will send it for an unscoped full audit.

State: this branch, wip/spec-integration-head, is at 0cbd2ba33. Round 1 merged C1, C2, C4, C5, C7, C10 (de0e8bfd2) and C9 (0551add93). It stopped before C6 because of the trap-line conflict recorded in _review/QUESTIONS.md. That conflict is now resolved at the source: C6 round 7 merged C9 itself and restated trapLine_injective on the rendered fields. Round 7 also made C9's SiteTable.Valid the single one and removed C6's string PathOk. Fetch the remotes first.

## Best long-term design (owner rule, binding)
Resolve every conflict by the best long-term design, never by the quickest edit. A merge must not weaken, drop or silently rewrite any law, definition, tag or approval of either side. Shortcuts are findings.

## Merge, in this order, one merge commit each (`git merge --no-ff <sha>`; never rebase, squash or rewrite history)
1. origin/wip/spec-c9 at 971eb025d (C9 rounds 14-16: recorded approvals, loader Refused examples for every class, pending-rule note)
2. origin/wip/spec-c10 at 871db4e19 (C10 rounds 11-12: source admission to artifact and launcher; recorded approvals)
3. origin/wip/spec-c6-audit at f2321db98 (C6 rounds 6-8: source-path decision, rendered-field injectivity, doc-name fixes, recorded approvals)
4. origin/wip/spec-base-migration at 9e48fdb06 (base migration rounds 1-5)
Use these exact SHAs, not the branch tips.

## Conflict rules
- **Ownership.** C9 owns Ident, Prim, Spec/Observe's source identity, the site table and the loader, plus the C9 part of Observe2. C6 owns the rest of Observe and its proof modules. C10 owns Frame. The base migration owns Borrow, Brand, Cleanup, Rep and the other base modules it changed.
- **C6 and C9 overlap.** Both C6 and C10 already contain C9 115a73b2d. The merges should therefore apply C9's later rounds and each side's own changes. Where C6 and C9 still touch the same declaration, keep C9's form for source identity, loader and site table, and C6's for observation and the trap report. Report every such place.
- **Approvals.** Keep every recorded approval. If merged text differs from what an approval covered, set that declaration to `(approved: PENDING)` and list it.
- **The base migration** may touch modules that later contracts changed (for example Exec or Scalar). Keep both sides' substance and report any semantic overlap. Do not invent a third design.
- **Spec.lean, ConcreteSpec.lean and README.md.** Union of the imports and sections, de-duplicated, in a sensible order.
- **scripts/proofs/axioms.golden.** Do not hand-edit it; in any conflict, keep this branch's side. Regenerating it is BLOCKED on an owner decision, and the coordinator handles it.
- **ISSUES.tsv.** Take both sides' rows; there must be no duplicate IDs.

## Checks
- After each merge commit, run the build (`bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1800 lake build ConcreteSpec`).
- If the build fails, fix it in a follow-up commit before the next merge.
- Gate commits on the exit status of the check itself, with `set -o pipefail`.
- After the last merge, run `nix develop -c timeout 600 bash scripts/spec/rules.sh` and `nix develop -c timeout 900 bash scripts/proofs/trust_ban.sh`.

## Rules
- Do not change any owner-decided rule, the ratified vacuity convention, or the meaning of any normative definition. If a conflict seems to need that, stop and put it in _review/QUESTIONS2.md with the options.
- Do not push.
- Do not edit docs/decisions.md or ROADMAP.md.
- Never commit _review/.
- Do not run reviewers, and do not write git notes.
- Final message: each merge commit SHA with its conflicts and how you resolved each; every declaration whose approval you reset; the overlaps reported; the build, rules and trust results; QUESTIONS.
