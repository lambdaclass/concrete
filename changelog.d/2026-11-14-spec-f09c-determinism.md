Extend Exec proofs with E3: determinism by inversion of the independently written Rules,
and soundness/completeness against the sole executable dispatcher at the same external
answer. Retain function-backed determinism only as Representation lemmas. Adapt agreement
to the separate functionEnd lifecycle boundary, refusal control and partial continuations.

Add populated positive Instances, kernel-checked Planted counterexamples for overlapping
rules and removed/added dispatches, and agreement examples for silent primitive answers,
moved-local stuckness, binding refusal and continuation refusal. Regenerate the complete
axiom inventory. No execution policy or second executable driver is introduced.

Authority: docs/decisions.md#decided-2026-11-14-trap-sites-and-law-tests admits
inductive law subjects and same-shape broken rules, classifies function-tautological
determinism as Representation lemmas, and requires determinism over separate rules
with two-way executable agreement under the same external answer. It requires each
planted defect to break one specific rule, substituted everywhere the law mentions it,
with a Lean-checked concrete counterexample preserving the premises; errors and
timeouts do not count. The November 13 proof-label record supplies only tag vocabulary;
foundation Q1 supplies the interface/law boundary. Inherited unrecorded approval claims
remain discarded; the recorded November 14 confirmation is the authority above. Path policy (ISS-0068), Boolean
encoding (ISS-0069), pointer bytes (ISS-0070), SPARK char preconditions (ISS-0071), wiping
(ISS-0072), startup/ABI (ISS-0073), constant restrictions (ISS-0065), U+FEFF/bare-CR scope (ISS-0064) and
struct/whole-pointer layout (ISS-0070) remain outside this piece. No held rule is imported.
