# C6 round 7 result

Final commit: 285cf57dbed9eff7cda6d2a3cc0d634390bee786.

## Items

1. DONE. Merged 115a73b2d as the second parent of 9a41764cc; never rebased. C9 owns identity, span fields, source tables and loader. Mandatory merge migration in Spec/Prim.lean and Proofs/Prim.lean preserves the final store added by C6. The axiom inventory was resolved to the unchanged C6 parent version.
2. DONE. Removed Observe.PathOk, SourcePosition and filtering sourceSites. ExecAdapter carries LoadedProject, its ProjectSources input and successful loader admission. No reporting-time path validation remains.
3. DONE. runFrom_source_trap has neither found nor pathOk. C9 load_trap_resolves supplies resolution and location validity. Membership of the exact original (site, span) pair preserves file identity more strongly than path equality alone. Source lookup, line, byte column, bounds, history, final state and status remain explicit. The wrong-file and scalar-column mutants preserve the law's premises. Missing-file and traversal-path projects are refused as whole projects; no adapter can be constructed for either. The missing-file example additionally shows that bypassing provenance and rendering the raw table would emit the unregistered path.
4. DONE. Only C9 SiteTable.Valid remains. ExecAdapter.sites_valid is an unapproved Helper deriving that predicate from loader admission, not a second invariant or an adapter admission premise.
5. DONE. Injectivity is exactly on kind, site, path, line and column. C9's equal-lines/different-ranges counterexample is retained. Renderer bytes and statuses are unchanged. Punctuated-path evidence uses part:a:b (site 9), avoiding C9's strict drive-prefix refusal of a:b at component start.
6. DONE. Changed Helper approvals reset to PENDING. N2 run on all seven requested modules; baseline comparison run on exact pre-merge commit 5120f5210.

## Changed Laws: old -> new; evidence

Names below are in ConcreteSpec.Core.Observe except Ghost.runStmts_terminal.

- trapLine_injective: equal lines iff kind/site/full Span equality -> equal lines iff kind/site/path/line/column equality. Instances distinct_sites_punctuated_path and distinct_columns_punctuated_path; mutants Evidence.trapLineWithoutSite and Evidence.trapLineWithoutCol. The unrendered-range counterexample remains.
- runFrom_source_trap: raw source list, found and string pathOk premises -> admitted adapter and program-site membership, with exact original span membership and loaded source byte correspondence. No found/pathOk premise or filtering. Instance source_path_to_trap; mutants wrongFileReport and scalarColumnReport. Refused projects unloadedSourceInput and refusedSourceInput witness provenance enforcement independently.
- source_trap_column: string path plus pathOk and wrong-column premise imply unequal diagnostic -> structured SourcePath and an iff: the candidate diagnostic equals the source-produced diagnostic exactly when its column equals UTF-8 byte length plus one. Retains all mismatch rejection and adds exact matching. Instances utf8_tab and matching_utf8_tab; mutant scalarColumnSpan.
- sourcePrefixSpan_second_line: string/pathOk, with a second-line span lacking absolute ranges -> structured SourcePath, second-line coordinates with both offsets equal to the entire source prefix's UTF-8 byte length. Instance multiline_utf8_tab; scalarPrefixSpan preserves the absolute offsets while corrupting only column, and singleLinePrefixSpan ignores the newline.
- sourceSpan_ascii_eq_char_col: string/pathOk -> SourcePath; retains ASCII byte-column = scalar-count + 1. Instance ascii_tab; mutant expandedTabSpan.
- sourceSpan_multibyte_gt_char_col: string/pathOk -> SourcePath; retains strict byte-column excess for a non-ASCII scalar. Instance multibyte_tab; mutant scalarColumnSpan.
- output_before_trap_kept: resolved site plus separate pathOk premise -> resolved site in an admitted adapter, no redundant path premise. Retains complete report, final state, independent physical execution and ordered outputs. Instance two_outputs_then_trap_report; mutants dropTrapHistoryReport and eraseProjection.
- ConcreteSpec.Core.Ghost.runStmts_terminal (required C9 merge migration): terminal SegmentReport ending .final f -> .final f c.store. All original terminal/configuration/fuel behavior retained. Instances Vacuity.returnedStart and Vacuity.trappedStart; restartingRunStmts mutant migrated to the final-store shape.

runFrom_trapped_status_134 retains its statement and all classification/status conjuncts; its proof now obtains path validity from the loader-derived Helper rather than a WellFormed field. C9's other imported Laws are unchanged.

## Commits

- 9a41764cc77bd2d3171e9dfbf34db2bca294e6a0 — Merge C9 source identity and derive C6 trap reports from admitted projects.
- bcf4b3fb76e9e8b0fff63a055c8fc934a8400665 — Strengthen source column discrimination to an exact rendering equivalence.
- 285cf57dbed9eff7cda6d2a3cc0d634390bee786 — Exercise loader refusal of an unloaded trap source identity.

Each commit followed a successful wrapped ConcreteSpec build, checked by its actual process exit status. A subsequent wrapped build at final HEAD also passed.

## Validation at final HEAD

- Wrapped timeout-1200 lake build ConcreteSpec: PASS, exit 0.
- scripts/spec/rules.sh: PASS=32 FAIL=0, exit 0.
- scripts/proofs/trust_ban.sh: PASS=23 FAIL=0, exit 0.
- N2, exact seven requested modules: FAIL, exit 1. New FAILs other than PENDING, instance-false-discharge and their cascades: NONE, compared with 5120f5210. All 114 standalone unknown-name diagnostics are identical to the baseline. Remaining categories: 32 unresolved-approval (PENDING), 33 instance-false-discharge, 41 unpaired-planted, 41 planted-proposition (all no-valid-closed-instance), 15 missing-instance. The transient projection-law finding in the first integrated run was removed by strengthening source_trap_column to the iff; final N2 has no projection-law finding.

The three adjacent round7-*.log files retain final N2, rules and trust output. No reviewers, git notes, pushes or prohibited ledger edits. _review is not committed.

## Left / dependency

C9 commit 115a73b2d is under independent review. That dependency is recorded in the C6 README; later C9 fixes must be merged on top. Inherited N2 diagnostics and pending independent approvals remain. Source elaborator integration remains O1, as before.

QUESTIONS: none. No owner-decided rule or undecided C9 character policy was changed.
