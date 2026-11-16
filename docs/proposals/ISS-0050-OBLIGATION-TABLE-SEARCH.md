# ISS-0050: obligation search, admission and discharge

Design round 2; proposed declarations and laws below are implementation obligations,
not claims of completed Lean verification. Authority: [redesign authorization](../decisions.md#decided-2026-11-15-audit-redesigns),
[owner goals](../decisions.md#owner-goals-2026-11-15-provability-errors) and
[Philosophy rules 1, 5–7](../PHILOSOPHY.md#the-rules).
Evidence anchors use integration snapshot `aa44140a33747fa37ece59c76090972e564c65b4`
(`origin/wip/spec-integration-head`), unless explicitly marked main. Main has newer
foundation interfaces; migration uses those interfaces, never reinstalls the snapshot wholesale.

## Raw search

`ObligationTable` remains `List SafetyObligation`. Implement `lookup` by
`List.find? (fun o => o.id == requested)` using lawful equality. No independent map key.
Do not add a uniqueness premise to raw search. For every same-type executable subject `L`:

* `L table req = some found ↔ ∃ before after, table = before ++ found :: after ∧
  (∀ o ∈ before, o.id ≠ req) ∧ found.id = req`.
* `L table req = none ↔ ∀ o ∈ table, o.id ≠ req`.

Prove these by list induction for lookup, retaining the full `lookup_id` corollary;
body equality alone is a representation lemma. Closed witness `[other,a,b]` has
`other.id ≠ req`, `a.id = b.id = req`, `a.key ≠ b.key` (distinct function keys from
`Exec.Demo.mainF` and `Exec.Demo.logF`). Lookup returns `a`; reverse search returns
`b`, head-only and always-none return none. Include empty, absent and wrong-id-return
controls, with identical inputs/premises. Raw duplicate search remains defined.

## Admission owns uniqueness

Proposed `AdmittedObligationTable` contains `entries : ObligationTable` and
`unique : (entries.map SafetyObligation.id).Nodup`. No unchecked constructor is an
execution boundary: Lean construction requires the proof. Proposed executable
`admitObligations : ObligationTable → Except ObligationCollision AdmittedObligationTable`
traverses left to right with an indexed prefix; on the first repeated id it refuses.
`ObligationCollision` contains indices `i,j`, both complete records and the common id.
It is an admission failure, never a discharge failure.

Prove `∃ a, admitObligations t = .ok a ↔ (t.map SafetyObligation.id).Nodup`,
success preserves `a.entries = t`, and every success is the same data/proof up to
proof irrelevance. Refusal iff there exist `i < j < t.length` with equal ids and
`j` the least index repeating an id in its prefix; `i` is its unique prior match
(the prefix before `j` is Nodup). Refusal records equal `t[i]`, `t[j]` at those exact
indices. Earlier distinct entries are not discarded. A-B-B-A must refuse `(1,2)`,
not `(0,3)`; equal duplicate records still have distinct occurrences. Controls:
keep-first admission, reject-distinct admission, later-collision and equal-record
index substitution. Prove admitted lookup is unambiguous, and reordering unique
entries preserves the returned record. Admission and lookup have separate laws.

## One semantic discharge operation

Proposed authoritative `dischargeChecked e admitted current requested` consumes
`AdmittedObligationTable`, returning a typed success (`DischargedFact claim`) or
semantic refusal. It checks, in order: `e.Usable current`, then
`current.obligation = requested`, then raw lookup in `admitted.entries`.
For unusable context return the first unequal `EvidenceContext` field in declaration
order: obligation, target, contract, checker, spec. Prove that all these fields
agree iff the existing `Usable` relation holds. Refusal retains requested identity,
both compared values and the failing field, or requested identity and missing lookup.
No collision branch exists here. Success transports the supplied `e.proof` using
context and lookup identity equalities; it never decides arbitrary `claim` propositions.

Retain an unlocated raw compatibility entry point only as composition:
`dischargeRaw t = admitObligations t >>= dischargeChecked` (with a sum of admission
and discharge refusals). Its Option view maps every refusal to none. No separate
legacy search/check implementation survives. For ALL raw tuples the equality is
between this composition and its Option erasure. Agreement with OLD discharge is
quantified only when `t.map id` is Nodup, or when `admitObligations t = .ok a`:
`erase (dischargeChecked e a c r) = oldDischarge e t c r` with `a.entries = t`.
Duplicate-ID success in the old function is deliberately removed; an unconditional
old-function equality is false and is not a migration obligation.

Preserve mismatch, unregistered and discharge-soundness conclusions with their
explicit admitted-table premises. Prove success iff existing `Usable`, requested
identity equality and registered membership all hold, with the same authoritative
record and transported proof. Refusal precision proves the selected check fails
and all earlier checks pass. Existing raw fixtures keep their input types; unique
ones pass admission, duplicate ones test admission refusal instead of discharge.

## Located diagnostic projection

Semantic refusals carry occurrence identities, not fabricated source spans.
Use the shared low-level carriers and decoding relation specified in
[ISS-0047's located boundary](ISS-0047-SITE-TABLE-OBLIGATIONS.md#located-boundary).
For discharge the primary is the requested operation/proof-attribute occurrence
in the Concrete source, never the possibly mismatched evidence site. Proposed
`LocatedDischargeInput` contains raw arguments, verified provenance for that
request, and for each obligation registration used in collision evidence.
Its `Decoded` proof ties them to the actual elaboration derivation and byte ranges.
An input with no bytes (a raw fixture) is not coerced into this type.

`checkLocatedDischarge x` first runs the SAME `dischargeRaw` on `x.raw`, then projects
its refusal using `x.provenance`; no check is rerun to explain none. Erasing only
diagnostic decorations equals the semantic result for every `x` satisfying
`Decoded`; for every raw `r` and every verified lift `x` with `x.raw = r`, the same
equality holds. This states a relation, not an impossible assertion that every raw
input has a located lift. Admission collisions use the later registration as primary
and the earlier as related evidence; discharge uses the requested occurrence.

The compiler boundary requires the verified lift and returns exactly one approved
rule and primary location per selected refusal, plus related evidence. The raw
semantic API remains total without promising external diagnostics. Prove exact
occurrence/range identity, byte-bounded ByteLocation validity (and `Span.LocationOk`
where the document has an admitted path), correct rule/check
association, and invariance of semantic erasure under alternative valid provenance.
Controls replace a primary by another valid occurrence, swap rule/reason, erase the
site, location or entire payload, or exchange equal-valued registration indices.
Validity of a replacement location alone must not pass the identity law.
For multiple requests use ISS-0047's per-file scheduler; field priority is only a
tie-break at one request, never a project-wide error category priority.

## Authority, layering and landing

Rule registration remains an OWNER QUESTION, with concrete options and costs in
[ISS-0047](ISS-0047-SITE-TABLE-OBLIGATIONS.md#rule-registration-owner-question).
Proposed mapping here: obligation collision → proposed `ObligationIdsUnique`;
context-field failures → proposed `EvidenceContextMatches` (tag the field);
request mismatch → proposed `EvidenceRequestMatches`; absence → proposed
`ObligationRegistered`. These are candidate Lean declaration names, not allocated
RuleIds. Approval and decision-linked declarations precede diagnostic completion.
No manifest grammar is needed or proposed. Search and semantic admission do not
need path policy. Located diagnostics wait for the approved path interface.
Shared provenance belongs below Prim/Observe, alongside Ident's byte/span carriers;
Prim must not import Observe, Contract or higher observation modules. Remove its
old Authority import in favor of the landed CapabilityBase interface.

The coordinator's F03 landing plan (`scratchpad/f03/plan.md`, rows F18/F19,
2026-11-16 writer input) budgets F18 under 2.5k changed lines, large effort, after
F10, F04, CAP and accepted redesign. This redesign adds measurable work; do not
pretend it fits that extraction budget without measuring the complete closure:

1. Shared verified provenance construction/erasure and byte-location controls:
   medium, separate useful lower-module slice, after F03p/F04p; no parser invention.
2. Owner-approved rule registration and positive/refused registry examples:
   small-to-medium, independent slice; include decision citations and registry gates.
3. F18 checked operations, admission, lookup/discharge proofs and raw controls:
   large; migrate Spec/Prim and Proofs/Prim together using main numeric identities,
   shared lookup and executor (ISS-0046/ISS-0049). Keep semantic admission independently
   useful if diagnostic prerequisites require a later slice; do not call it full closure.
4. Located discharge, relation-preserving erasures and all diagnostic consumers:
   medium-to-large, after 1–3; request/proof-attribute provenance production is part
   of the cost, not an assumed oracle. Measure affected production consumers before
   splitting; no API switch lands without its compiling consumers and controls.

F19 is independently sequenced in ISS-0047. ISS-0048 state ownership first arrives
there; main ISS-0073 still holds startup/ABI for F28/F29, and does not block raw search.
Use finite recursion and constructive equality/list proofs throughout, no new axioms,
trusted search, classical choice, held assumptions or parallel semantic relation.
Diagnostics project existing Spec checks; provenance relations only certify origins.

## Anchored migration evidence

At the pinned snapshot: Spec/Prim.lean:255 (`EvidenceContext`), :266 (`SafetyEvidence`),
:272 (`SafetyObligation`), :288 (`ObligationTable`), :292 (`lookup`), :299 (`lookup_id`),
:312 (`DischargedFact`), :320 (`SafetyEvidence.discharge`); Proofs/Prim.lean:134,143,155,191,210
contain context/obligation mismatch, soundness, target mismatch and unregistered evidence.
Spec/Basics.lean:27 owns AList lookup and its header directs intrinsic-id lists to
`List.find?`; Spec/Ident.lean:90,367,836 supplies ObligationId, StableKey, RuleId.
These are snapshot anchors, not main line numbers. Proposed carriers/functions above
do not exist yet. Resolve actual uses with
`git grep -n -E 'ObligationTable|SafetyEvidence\.(discharge|Usable)|DischargedFact' -- ConcreteSpec`,
including namespace-local calls. `load_realizes` and reviewer `CheckM` are not current
spec declarations (`CheckM` occurs only in the frozen legacy checker); do not use them
as authority. Keep this bounded inventory rather than the former unfiltered search.
