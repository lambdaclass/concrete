# C6 round 8: fix every unknown-name diagnostic in the C6 tagged doc comments (Codex writes; the review is by agy)

State: the branch is at 285cf57db (round 7); use HEAD. _review/REV-C6-agy-285cf57db.md is an independent agy review of round 7 (5120f5210..285cf57db). It passes the official reviewer citation contract (checker 60934a193, status CHANGES; sha256 7a765d8415f6414c4c9b10bc406cfdeb147eca5cad9a2469969df3626c01c0ce). Every location and identifier it cites resolves in the tree. It finds the source-path decision and round-4 findings 1, 3 and 4 resolved, and every changed Law at least as strong as before. Its one finding (Finding 1) is that the C6 proof modules carry 114 unknown-name N2 diagnostics.

## Best long-term design (owner rule, binding)
Never cut corners. Shortcuts are findings. Do not weaken, delete or reclassify any declaration to make a diagnostic go away.

## Task
_review/round7-unknown-names.txt lists all 114 unknown-name diagnostics from the round-7 N2 run: the tagged declaration, then the backticked token that does not resolve. They are backticked local parameter names (n, m, store, sp, history, ...), unprefixed constructor fragments (.infinite, none, ...), numerals, compound expressions (rest₁ ++ rest₂, Exec.Ending O, ...), short or partial names (AdapterDemo, BlockingPermitted, runFrom, ...), file names, and tag words written inside backticks (Instance, Planted).

The convention (VACUITY_CONVENTION_FINAL.md, "Carrier") says every backticked name in a tagged doc comment must resolve to a declaration of the right kind. For each diagnostic:
- if the token means a declaration of this tree, write its fully qualified name, and check it resolves;
- otherwise (a local, a numeral, an expression, a file name, a tag word), keep the prose and remove the backticks, rewording only if it no longer reads clearly.
Change doc comments only; no statement, proof or tag class may change.

## Task 2: record the C6 approvals from the same review
The review's section "Pending Helper and Representation Verdicts" APPROVES 40 declarations and rejects none. Rows 1-20, 23-25 and 33-40 are C9's declarations (Ident.lean and the loader part of Observe2.lean); C9 records them on its own branch, and C6 takes them by a later merge. Do not touch them.

Record the nine C6-owned rows: 21 `ConcreteSpec.Core.Observe.ExecAdapter.sites_valid`, 22 `ConcreteSpec.Core.Observe.decodeKind_render`, 26 `ConcreteSpec.Core.Observe.steps_trap_observation`, 27 `ConcreteSpec.Core.Observe.AdapterDemo.answeringWellFormed`, 28 `ConcreteSpec.Core.Observe.AdapterDemo.blockingWellFormed`, 29 `ConcreteSpec.Core.Observe.ReportDemo.relativePathWellFormed`, 30 `ConcreteSpec.Core.Observe.ReportDemo.sourcePermitted`, 31 `ConcreteSpec.Core.Observe.environment_oracle_wellFormed`, 32 `ConcreteSpec.Core.Observe.sourcePrefixSpan_hasByteColumn`.
For each, replace `(approved: PENDING)` with `(approved: REV-C6-agy-285cf57db)`. Match by fully qualified name, and check that the tag line is that declaration's tag.
Report every `(approved: PENDING)` left in the C6 files, with the reason (for example, a C9-owned row).

Make the doc fix and the recording two separate commits.

## Disputed part of the finding (do NOT act on it)
The review also asks that every `case` label in Instance and Planted tags become a fully qualified declaration name, or a new "proposed:" form. That is not a finding. A case value is a label, not a name reference: the N2 gate does not resolve it (none of the 114 diagnostics is a case label), and the accepted C1, C9 and C10 heads use the same free labels. Leave every case label as it is. It is recorded as disputed in the lane record.

## Check
Run the N2 vacuity gate on the seven C6 modules exactly as in round 7:
  export PWD_SRC="$PWD"; bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh bash -c 'cd /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/n2/repro/gate && timeout 2400 python3 scripts/proofs/vacuity_driver.py --source "$PWD_SRC" --scope ConcreteSpec.Proofs.ObserveReport,ConcreteSpec.Proofs.ObserveSource,ConcreteSpec.Proofs.ObserveEvidence,ConcreteSpec.Proofs.ObserveOutput,ConcreteSpec.Proofs.ObserveEnvironment,ConcreteSpec.Proofs.ObserveTransform,ConcreteSpec.Proofs.Observe2'
Report:
- the unknown-name count, which must be 0 in the C6 modules;
- confirmation that no other category changed in count from round 7.
Then run the build, rules and trust_ban checks as in round 7.

## Rules
Commit after the build and the checks pass. Do not push. Do not edit scripts/proofs/axioms.golden, docs/decisions.md, ROADMAP.md or ISSUES.tsv. Never commit _review/. Final message: the diagnostics fixed (counts by kind), the N2 before and after, the commit SHA, and QUESTIONS.

## Build and commit gating (binding)
Run every Lean build as `bash /tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/score/tools/lean-build.sh nix develop -c timeout 1200 lake build ConcreteSpec`. Gate the commit on the exit status of the check itself, with `set -o pipefail`. Do not run reviewers, and do not write git notes.
