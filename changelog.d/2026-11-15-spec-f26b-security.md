# C7 security and source-grounded release admission

`Spec/Leak.lean` owns leakage, ordered typed-site/value releases, separate reason/path
facts, public-state equivalence, C1 instrumentation, delimited release noninterference (NI) and
constant time (CT). `Spec/ReleaseCalls.lean` owns conservative source occurrences, source paths,
call/release extraction and function/site admission. `Spec/LeakData.lean` supplies closed integer
machine data adapters, not a Concrete scalar or Boolean representation policy.

Authority is docs/decisions.md#decided-2026-11-08-foundation (the approved C7 interface and laws)
and docs/decisions.md#decided-project-infoflow-2026-11-06 (separate NI/CT obligations, public
lengths and the baseline observer). Numeric local fixtures follow
docs/decisions.md#decided-2026-11-08-c9-identities; diagnostic spellings stay outside execution.
No inherited approval claim supplies authority.

`Proofs/Leak.lean` carries ten original Laws: `release_sites_agree`, `instrumentation_erasure`,
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
Their twenty-four closed accepted/refused examples live in `Proofs/LeakRegistry.lean` because they use
security Laws and execution witnesses; Spec cannot import Proofs. The exporter gains only that
registry import. All nine input shapes have closed-data and opaque-carrier controls. The axiom
golden is regenerated from the combined environment. ISS-0082 tracks the release/facts boundary;
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
backend obligations; these proofs concern the explicit C1 source observer. No additional issue number beyond ISS-0082
is allocated by this branch.

Runtime release instrumentation preserves the actual source site and evaluated value. A per-step
site agreement Law connects runtime release sites to static extraction, with nonzero-site positive
and zero-site substitution controls. Registry evidence accepts site 7 under its own policy and
refuses it under a policy permitting only site 0. Information-flow authorities are recorded directly
on the registered judgments. Root modules explicitly import all security modules in dependency order.
Compound extraction controls isolate each child traversal; reachability includes a two-edge witness
and a direct-only mutant, and erasure has witnesses in both directions.

Round 4 independently reproduced all three review claims in bounded, kernel-checked scratch
proofs before editing the implementation: internal step counts made asymmetric leaks vacuously
NI/CT; identical silent secret branches passed CT with inhabited premises; and an observer
restricted to unbound, empty-stack releases satisfied the old agreement statement while dropping
other releases. The scratch statements use complete C1 execution witnesses, not simulated traces.

Deterministic instrumentation now emits no external choices. Evaluated conditionals emit their
actual source site and decision. Evaluated loop tests emit that same branch information plus their
loop identity and an iteration contribution (one when entering the body, zero when exiting).
The ordered trace retains each test, including the exit test; it does not infer a count from
unrelated internal steps. Release agreement quantifies over every destination and continuation.

Three additional Laws, `deterministic_choices`, `conditional_leakage`, and `loop_leakage`, cite
the recorded C7 matched-choice boundary and information-flow baseline observer above. Each has
an inhabited Instance and an isolated Planted substitution. Complete executions refuse asymmetric
secret output under both NI and CT, identical-arm secret conditions under CT, and a finite secret
loop under CT. Four additional registry refusal witnesses export these cases. Planted observers
change only step choices, conditional leakage, loop-test leakage, destination filtering, or stack
filtering; each is independently detected. Existing straight-line execution witnesses now have
empty external-choice traces. No security typing or termination-sensitive NI rule is introduced.

Observer-field audit: public output preserves the emitted value; final observation preserves the
C1 outcome, including trap kind/site; primitive timing preserves source sites and evaluated
operands; releases preserve typed sites, values and order; public-local projection and low-store
relations explicitly mark the toy machine's secret state; facts erase only reason/path metadata.
All continuation and destination shapes are retained. The toy operand language has no address,
allocation, container-length or external-effect operations, so it manufactures none of those
events; the generic observer retains their payloads and public-length comparison. The Unit choice,
address and effect carriers do not stand in for any existing nondeterministic or memory operation.
The accounting base's files remain byte-for-byte unchanged.
