# C7 security and source-grounded release admission

Carries the security half of `19d577d68` onto the accounting base, preserving declaration
identities. `Spec/Leak.lean` owns leakage, ordered typed-site/value releases, separate reason/path
facts, public-state equivalence, C1 instrumentation, delimited release noninterference (NI) and
constant time (CT). `Spec/ReleaseCalls.lean` owns conservative source occurrences, source paths,
call/release extraction and function/site admission. `Spec/LeakData.lean` supplies closed integer
machine data adapters, not a Concrete scalar or Boolean representation policy. Accounting and the
base's validator remain unchanged; Const and Own retain their existing authority provenance.

Authority is docs/decisions.md#decided-2026-11-08-foundation (the approved C7 interface and laws)
and docs/decisions.md#decided-project-infoflow-2026-11-06 (separate NI/CT obligations, public
lengths and the baseline observer). Numeric local fixtures follow
docs/decisions.md#decided-2026-11-08-c9-identities; diagnostic spellings stay outside execution.
No inherited approval claim supplies authority.

`Proofs/Leak.lean` carries nine Laws: `instrumentation_erasure`,
`forbidsRelease_not_authorized`, `release_metadata_irrelevant`, `secure_ni`, `secure_ct`,
`timing_ni`, `timing_not_ct`, `authorized_release_not_ni`, `authorized_release_not_ct`.
`Proofs/ReleaseCalls.lean` carries seven: `extraction_complete`, `source_reaches_complete`,
`program_release_complete`, `unlisted_caller_refused`, `unlisted_site_refused`,
`forbidden_release_refused`, `unlisted_program_caller_refused`. Each has populated Instance
witnesses and single-rule, uniformly substituted Planted counterexamples with inhabited premises.
Helper and Representation lemma tags distinguish support from Law evidence, following
docs/decisions.md#decided-2026-11-13-proof-labels and
docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests.

The end-to-end program refusal Law now assumes only source reachability, source function-table
membership, a source release occurrence and caller non-membership. It composes the reachability
and release extraction Laws with graph-level refusal. Its Planted controls replace extraction
with dropped-call and dropped-release-table projections, or replace ProgramAdmitted with
program-lifted direct-owner-only and ignored-function-list predicates. None of the source premises
can disappear under substitution. A separate function-table drop control qualifies
`program_release_complete`; the release-traversal mutant is named separately from the
`droppedReleaseGraph` table-projection mutant, which preserves production call extraction.

Exactly nine security rules join the eight accounting rules: `UnitCallsAdmitted`,
`IntegerProgramAdmitted`, `StringReleasePermitted`, `UnitReleaseForbidden`, `UnitTraceAuthorized`,
`IntegerRunsAuthorized`, `IntegerLowEquivalent`, `IntegerDelimitedReleaseNI`, `IntegerConstantTime`.
Their eighteen closed accepted/refused examples live in `Proofs/LeakRegistry.lean` because they use
security Laws and execution witnesses; Spec cannot import Proofs. The exporter gains only that
registry import. All nine input shapes have closed-data and opaque-carrier controls. The axiom
golden is regenerated from the combined environment. ISS-0082 is copied verbatim from the source;
run-connected facts and separate reason/path mutants qualify its semantic projection boundary.

Held scope remains excluded; existing tracking covers every excluded family:

- U+FEFF scope and bare-CR termination: ISS-0064 / ISS-0065, unresolved source scope.
- Additional path policy: ISS-0068, missing owner dispositions for character/component rules.
- SPARK character preconditions: ISS-0071, explicitly open alternative.
- Boolean storage/conversion encoding: ISS-0069, scalar rules do not authorize representation.
- Pointer bytes, whole-pointer representation and struct layout: ISS-0070, target/interface
  decisions do not authorize physical mapping or padding.
- Constant restrictions and queued partial recovery items: ISS-0080 / ISS-0065, unrecorded or
  reopened restrictions do not become security rules.
- Wiping: ISS-0072, reopened policy; no wiping operation or dependent evidence is carried.
- Startup/ABI: ISS-0073, no source-backed startup or calling-convention policy is added.

Secret typing, speculative leakage and binary constant-time guarantees remain later-slice or
backend obligations; these proofs concern the explicit C1 source observer. No new issue number
is allocated by this branch.
