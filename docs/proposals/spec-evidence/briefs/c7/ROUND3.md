# C7 round 3: vacuity audit fix round (Codex: CHANGES; agy: CHANGES). Fix on this branch.

HEAD is 4d146992 (round 2; all seven gates passed on concrete-ci-2). C7 was accepted before the owner's vacuity rule; a full audit of the contract (base 5bc0748b..4d146992) by Codex and agy found vacuous laws. Their complete reports are `_review/codex-vacuity-audit.md` and `_review/agy-vacuity-audit.md`: read both in full, including the per-law tables. Do not edit scripts/proofs/axioms.golden. All rules of _review/BRIEF.md apply (commit each piece after `nix develop -c timeout 1200 lake build ConcreteSpec` passes; do not push).

First merge the integrated spec core: `git fetch origin wip/spec-core && git merge --no-edit origin/wip/spec-core` (C1/C2, C4, the C1 partial lifecycle, C6). Expected conflicts: ConcreteSpec.lean / ConcreteSpec/Spec.lean module lists (keep every entry of both sides) and scripts/proofs/axioms.golden (take the spec-core side, `git checkout --theirs scripts/proofs/axioms.golden`; the reviewer regenerates it). Commit the merge with its resolutions only, list the conflicted files in your final message, then make the tree build.

Scope: the C7 (Leak) laws only. The `*_refused` and `continuation_refused` laws in Proofs/Exec.lean are C1's (the partial lifecycle) and are fixed in C1's own audit round: do not change Exec.lean or Proofs/Exec.lean.

Fix, each as its own commit:
1. `established_report_sound` (Proofs/Leak.lean ~523): it reduces to `x ≤ x` by copying the actual quantities into the bounds. Prove soundness for a report built independently of the quantities it bounds (from an action sequence or an analysis result) and connect entry and exit accounts by an actual run. Example (Codex): 100 historical requested bytes, enter with 2 live, allocate 8, free 3; an independent report requested ≤ 8, net ≤ 5, local peak ≤ 10 is sound; the planted underbounds 7, 4, 9 are each unsound.
2. `unestablished_report_sound` (~540): holds only because its status premises are false. Keep it as a named representation lemma, documented as such (not soundness evidence), and add a report with mixed statuses where the established fields constrain the actual values, plus a planted understated established field that fails.
3. `reset_conservation` (Spec/Leak.lean ~236, Proofs/Leak.lean ~416): `apply` accepts a reset only when the action's `released` equals the current live bytes, and the law uses exactly that guard. Derive the released amount from the transition itself (not from an action field checked against it), or prove a refinement from C4 resets to the accounting records; plant a reset whose recorded outflow disagrees with the bytes actually released and show the conservation obligation fails. `run_conservation` must follow.
4. `WipePreservation` (Spec/Leak.lean ~324): give a related source/target pair with at least one nonzero wipe, prove preservation, and plant a target that drops, changes or reorders the wipe and show it fails.
5. Every other C7 law the two reports mark V, M or D (vacuous, missing instance or counter-example, or definition-level): add the missing non-trivial instance and planted counter-example, or, where a law only restates a definition, either replace it by a substantive law or mark it as a helper in its doc comment and do not count it as contract evidence. Go through the tables row by row and say in your final message what you did for each row.

If a fix needs a rule no decided row gives (docs/decisions.md, docs/proposals/FOUNDATION.md C7, `_review/LANGUAGE_DECISIONS_2026-11-08.md`), do not decide it: put it under QUESTIONS with options and a recommendation.

## Vacuity rule (owner, binding)
No law may hold because a guard or a definition makes it trivially true (for example a run-time re-check of the invariant inside the operation, a theorem that restates its definition, or a hypothesis that can never hold). For every law you add or change, give (a) a non-trivial instance: an example where its hypotheses hold, and (b) a planted counter-example: a mutated rule or a bad state that fails it, written as an `example` with a negation. Say in your final message, per law, which examples witness (a) and (b).

## Final message
Commits (SHA + one line), merge conflicts and resolutions, per item and per audit-table row what changed and the (a)/(b) witnesses, the build/rules/trust_ban results you ran, and QUESTIONS ("None" if none).

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch: no law that holds by construction or restates its definition, no weakened statement, no premise that switches off an obligation. If staging is truly unavoidable, say so in your final message and record the long-term design and the roadmap row it belongs to (text for the coordinator; do not edit ROADMAP.md).
