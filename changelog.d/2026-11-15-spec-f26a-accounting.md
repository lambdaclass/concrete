# C7 memory accounting, split from the leakage slice

Extracts only accounting from `19d577d68`, preserving declaration identities under
`ConcreteSpec.Core.Leak`. `Spec/Accounting.lean` imports only `Spec/Ident` and owns
accounts, checked actions and sequences, reset outflow, independent live-byte trajectory
observation, allocator-defined optional footprint, signed net intervals and report soundness.
It also owns the finite-data adapters. No execution, diagnostics, releases or security
judgments are introduced. The existing Const and Own rules and their citations are unchanged.

Authority is docs/decisions.md#decided-2026-11-08-foundation (C7's conservation boundary)
and docs/decisions.md#decided-project-memory-2026-11-06 (adopting MEMORY §1: cumulative
requests, signed net change, peak above entry, allocator-defined footprint and separate
established/not-established statuses). The absolute internal local peak is converted to
peak above entry in `LineageReport.Sound`; a function with no allocation reports zero peak
even when caller storage is live. `SignedInterval.Sound` now has its own Authority line.

`Proofs/Accounting.lean` carries twelve Laws: `Account.alloc_zero`, `run_wf`,
`apply_alloc_zero`, `free_permitted_iff`, `free_conservation`, `reset_conservation`,
`run_conservation`, `run_peak_monotone`, `run_peak_exact`,
`run_historical_peak_exact`, `function_entry_peak`, and `report_certified`.
Each has populated Instance evidence and single-rule Planted counterexamples with inhabited
premises, substituting transitions in dependent folds and observers as well as execution.
Helpers and Representation lemmas remain explicitly separate from Law evidence, under
docs/decisions.md#decided-2026-11-13-proof-labels and
docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.

The specification registry adds exactly eight rules (`Account.WF`, `ResetConserved`,
`SignedNetSound`, `ByteBoundSound`, `ByteBoundSoundOptional`, `ReportSound`,
`FiniteActionPermitted`, `FiniteSequencePermitted`) with sixteen closed accepted/refused
examples in `Spec/Rules.lean`. The Lean exporter is unchanged. Accounting carrier controls
check concrete inputs and refuse opaque inputs of all eight shapes. The axiom golden is
regenerated as the exact union with main, without security declarations.

The authority validator uses CommonMark entry boundaries, a closed title allowlist
(text, emphasis, code), plain paragraph-leading record markers and entry-local owner
utterance fences. Unsupported titles and dispositions refuse. Open waiting links share citation path/fragment
normalization, including equivalent paths and reference links; wrong paths refuse.
Text citations and normalized Markdown link/image targets are checked independently,
including reference links. Raw anchor counts independently require unique entry,
navigation or table-quotation accounting; concealed entries and outdented fields/code
refuse. Diagnostic controls cover both reviewers’ concrete reproducers. ISS-0083
tracks the explicit owner-approved status format still needed. Main’s rules, citations
and exporter are unchanged. The foundation title and memory approval prefix are joined onto single lines
as required by the plain-token allowlists; the existing normalized word-equality gate still passes.

Held scope remains excluded, without inferring authority from inherited approval claims:

- U+FEFF scope and bare-CR comment termination: ISS-0064 and ISS-0065; unresolved scope.
- Additional source-path policy: ISS-0068; no owner answer for the queued character rules.
- SPARK character preconditions: ISS-0071; explicitly left open.
- Boolean encoding and whole-pointer bytes/representation: ISS-0069 and ISS-0070;
  scalar and target decisions do not establish these representations.
- Constant restrictions and queued partial recovery items: ISS-0080 and ISS-0065; only recorded
  dispositions apply, without adding any constant policy in this branch.
- Wiping: ISS-0072; no wiping operation, counter or theorem is carried.
- Startup/ABI: ISS-0073; no startup or calling-convention rule is carried.
- Struct layout and general whole-pointer layout: ISS-0070 (layout/padding scope);
  structural cause is mistaking an interface/target boundary for representation approval;
  structural owner is the owner-approved layout contract and its C4/C10 implementation.
  Closure requires a recorded owner disposition and corresponding representation laws and
  independent planted controls. This branch introduces no layout operation or assumption.

Security and release/facts qualification (ISS-0082 on the source branch) follows separately;
this branch neither allocates that identifier nor carries its security issue row.
