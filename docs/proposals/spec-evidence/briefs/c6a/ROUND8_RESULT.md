# C6 round 8 result

Only doc comments changed. Statements, proofs, tag classes and case labels are unchanged.

114 unknown-name diagnostics fixed: 26 declaration references fully qualified; backticks removed from 21 locals, 52 expressions, 3 numerals, 3 namespace labels, 3 file names and 6 tag words.

N2 on the seven requested modules:

| Category | Round 7 | Doc fix | Approval recording |
|---|---:|---:|---:|
| unknown-name | 114 | 0 | 0 |
| unresolved-approval | 32 | 32 | 23 |
| instance-false-discharge | 33 | 33 | 33 |
| unpaired-planted | 41 | 41 | 41 |
| planted-proposition | 41 | 41 | 41 |
| missing-instance | 15 | 15 | 15 |

N2 exits 1 due to the inherited remaining categories. No other category changed for the doc fix; recording the nine authorized C6 approvals reduces only unresolved-approval by nine. All new declaration references resolve in N2.

Validation on each committed slice: wrapped ConcreteSpec build exit 0; rules PASS=32 FAIL=0, exit 0; trust_ban PASS=23 FAIL=0, exit 0. Checks ran with pipefail and commits followed actual successful check exits. The first doc build attempt failed with a missing ObserveEnvironment.olean output file; a subsequent build passed before the first commit. Logs are adjacent round8-*.log files. Validation source content is exactly that of the corresponding commit.

No reviewers, git notes or pushes. No prohibited files changed. Working tree clean after both commits. No theorem changed, so no new law evidence required.

## Commits

f2321db982441c68c844b5051224a3aaf2a4d48b Record nine independently reviewed C6 helper approvals
fa60a709af0e73f9fe13c546f60a88069e51b781 Resolve C6 tagged doc comment name references

## Remaining approvals

All 23 are C9-owned and are reserved for recording on C9’s branch, then a later merge. Each is listed below.

ConcreteSpec/Proofs/Observe2.lean:24 ConcreteSpec.Core.Observe.LoadedProject.parseFiles_render — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:32 ConcreteSpec.Core.Observe.LoadedProject.parseFiles_exact — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:50 ConcreteSpec.Core.Observe.LoadedProject.parseFiles_iff — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:59 ConcreteSpec.Core.Observe.LoadedProject.firstDuplicate_none — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:68 ConcreteSpec.Core.Observe.LoadedProject.siteIssue_none — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:78 ConcreteSpec.Core.Observe.LoadedProject.findSiteIssue_none — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:126 ConcreteSpec.Core.Observe.LoadedProject.lookup_iff_mem — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:176 ConcreteSpec.Core.Observe.LoadedProject.firstDuplicate_mem — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:188 ConcreteSpec.Core.Observe.LoadedProject.parseFiles_refusal_names — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:211 ConcreteSpec.Core.Observe.LoadedProject.siteIssue_names — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:225 ConcreteSpec.Core.Observe.LoadedProject.findSiteIssue_names — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:316 ConcreteSpec.Core.Observe.LoadedProject.lookup_functional — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:324 ConcreteSpec.Core.Observe.LoadedProject.lift_permutation — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:357 ConcreteSpec.Core.Observe.LoadedProject.lookup_permutation — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:378 ConcreteSpec.Core.Observe.ProjectSources.realizes_permutation — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:501 ConcreteSpec.Core.Observe.SourceLoadDemo.parent_invalid — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:511 ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate_invalid — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:519 ConcreteSpec.Core.Observe.SourceLoadDemo.missing_invalid — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:530 ConcreteSpec.Core.Observe.SourceLoadDemo.duplicateFiles_invalid — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:542 ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation_invalid — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:1413 ConcreteSpec.Core.Observe.trapLineChars_prefix — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:1420 ConcreteSpec.Core.Observe.decodeWithKind_render — C9-owned; reserved for C9 branch.
ConcreteSpec/Proofs/Observe2.lean:1429 ConcreteSpec.Core.Observe.decodeTrapLine_render — C9-owned; reserved for C9 branch.

QUESTIONS: none.
