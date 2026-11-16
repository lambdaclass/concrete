# ISS-0050: obligation search, admission and discharge

Design round 3; proposed declarations and laws below are implementation obligations,
not claims of completed Lean verification. Authority: redesign authorization in the writer brief (owner, 2026-11-15: “yes do all this”),
[owner goals](../decisions.md#owner-goals-2026-11-15-provability-errors) and
[Philosophy rules 1, 5–7](../PHILOSOPHY.md#the-rules).
Evidence anchors use integration snapshot `aa44140a33747fa37ece59c76090972e564c65b4`
(`origin/wip/spec-integration-head`); every snapshot file citation below names that
ref, full path and line explicitly. Main has newer
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
is the RAW projection of ONE shared executable `obligationChecks t` kernel.
The kernel computes a finite indexed fact at EVERY registration j: either its id
has no prior equal entry, or a collision retaining the least earlier matching i.
It also computes the occurrences for each id, so request uniqueness uses the SAME
facts, not a second admission algorithm. Raw admission projects the least failing j;
located admission projects per-file minima through ISS-0047's graph/selector.
Prove each kernel fact iff its exact prefix contains that id, and prove BOTH
projection laws. Never implement located admission by decorating one raw collision.
`ObligationCollision` contains indices `i,j`, both complete records and the common id.
It is an admission failure, never a discharge failure.

Prove `(∃ a, admitObligations t = .ok a) ↔ (t.map SafetyObligation.id).Nodup`,
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

Semantic refusals carry occurrences, never fabricated spans. Use
[ISS-0047's boundary](ISS-0047-SITE-TABLE-OBLIGATIONS.md#located-boundary):
`LocatedDischargeInput` in Diagnostics contains raw evidence arguments, indexed
requests, documents, origins and WellLocated covering EVERY registration, request
and checked field, not just entries selected as collision evidence. Decoded lives
only in actual producer modules and proves erasure/WellLocated/exact origins.
Malformed-table located fixtures require WellLocated, not a sound producer that
could never emit malformed tables. Raw fixtures need no bytes or coercion.

`checkLocatedDischarge x` invokes obligationChecks ONCE, adds per-request context,
requested-id and registration facts, evaluates the complete finite prerequisite
graph and selects per-document minima. It does NOT call dischargeRaw and decorate
its one global refusal. A request's checks use the semantic predicates of
`dischargeChecked` in the same order, without constructing a DischargedFact on an
unadmitted whole table. Context → requested identity → registration existence;
missing registration is a failure, even if an unrelated id collides. For a present
requested id, using its record requires exactly ONE occurrence in the ENTIRE table:
all duplicate facts for that id are prerequisites, including a later registration
which duplicates an earlier request target. An ambiguous id blocks record-dependent
checks with its collision root. Unrelated duplicates do not block this request.
These are diagnostic facts over existing Spec checks, never provisional authority.

After all required facts pass, construct AdmittedObligationTable and call the
single authoritative dischargeChecked for each request. If ANY admission check
fails, no authoritative discharge success or artifact is returned, even when a
request's diagnostic facts independently pass. Prove that on globally admitted
inputs the request facts agree exactly with dischargeChecked, and that success of
the entire located entry is equivalent to admission plus successful requests.
This separates independent error discovery from globally admitted proof authority.

Prove raw first-collision projection of the kernel equals admitObligations; prove
located selection equals the same kernel under the SUPPLIED graph/plan. Exact
decoration erasure uses that same plan. Across different WellLocated layouts,
underlying facts and admission are invariant; selected refusal collections need
not agree. No equality with dischargeRaw's single refusal is claimed on malformed
multi-document input. With a single request and successful admission, prove exact
semantic agreement with dischargeChecked, retaining occurrence/reason precision.

Required actual-entry controls: registrations A,A attributed to doc0 and B,B to
doc1; checkLocatedDischarge retains BOTH later-registration collisions (same raw
admitObligations reports only A,A). Add a failing request for unique C at offset 10
and unrelated A collision at 500 in its document: request failure wins. Move that
collision to a second document: retain both. Duplicate C instead: block its
record-dependent request checks, retaining its collision root. Use valid distinct
bounded origins for every registration/request; no impossible Decoded premise.
Mutants decorate only raw admission, require global Nodup for diagnostic requests,
ignore later same-id duplicates, or return authoritative success despite collision.

Each static selected refusal has exactly one approved rule, primary occurrence and
bounded byte location, plus related collision evidence. Request primary is its
operation/proof attribute; collision primary is the later registration. Prove exact
identity (not merely validity), byte-derived coordinates, rule/check association,
and production consumed-range identity under Decoded. Controls swap rule/reason,
replace primary with a different valid occurrence, lose payload or exchange equal
record indices. No static rule registration is implied by a runtime unmapped trap.

## Authority, layering and landing

Rule registration remains an OWNER QUESTION, with concrete options and costs in
[ISS-0047](ISS-0047-SITE-TABLE-OBLIGATIONS.md#rule-registration-owner-question).
Proposed mapping here: obligation collision → proposed `ObligationIdsUnique`;
context-field failures → proposed `EvidenceContextMatches` (tag the field);
request mismatch → proposed `EvidenceRequestMatches`; absence → proposed
`ObligationRegistered`. These are candidate Lean declaration names, not allocated
RuleIds. Approval and decision-linked declarations precede diagnostic completion.
Candidate predicates take concrete EvidenceContext/ObligationTable/ObligationId,
never SafetyEvidence's Prop-valued claim; rule examples use unapplied constants and
doc comments cite an existing governing Q*/OD*/P*/R-* row after owner approval.
No row ID is invented. No manifest grammar is needed or proposed. Search and semantic admission do not
need path policy. Located diagnostics wait for the approved path interface.
Only Var-free byte carriers belong in an Ident-only leaf; WellLocated inputs,
producer derivations and adapters combining programs and bytes belong in Diagnostics
outside Spec. Follow ISS-0047's explicit local_identity carrier/module boundary;
no Spec forwarding wrapper may reintroduce the reachable Var/raw-text combination.
Prim must not import Observe, producer modules, Diagnostics or higher observation modules. Remove its
old Authority import in favor of the landed CapabilityBase interface.

The coordinator's F03 landing plan (`scratchpad/f03/plan.md`, rows F18/F19,
2026-11-16 writer input) budgets F18 under 2.5k changed lines, large effort, after
F10, F04, CAP and accepted redesign. This redesign adds measurable work; do not
pretend it fits that extraction budget without measuring the complete closure:

1. Byte-only carriers, Diagnostics WellLocated inputs and producer-local
   Decoded construction/erasure with byte-location and trust-ban controls:
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

Historical evidence (not files to import):

| Explicit snapshot anchor | Migration subject |
|---|---|
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Prim.lean:255` | EvidenceContext, SafetyEvidence, SafetyObligation |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Prim.lean:288` | ObligationTable, lookup, lookup_id |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Prim.lean:312` | DischargedFact, SafetyEvidence.discharge |
| `origin/wip/spec-integration-head:ConcreteSpec/Proofs/Prim.lean:134` | mismatch, soundness, target and unregistered controls |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Basics.lean:27` | AList lookup; header prescribes List.find? for intrinsic ids |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Ident.lean:90` | ObligationId |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Ident.lean:367` | StableKey |
| `origin/wip/spec-integration-head:ConcreteSpec/Spec/Ident.lean:836` | RuleId |

Proposed carriers/functions above
do not exist yet. Resolve actual uses with
`git grep -n -E 'ObligationTable|SafetyEvidence\.(discharge|Usable)|DischargedFact' -- ConcreteSpec`,
including namespace-local calls. `load_realizes` and reviewer `CheckM` are not current
spec declarations (`CheckM` occurs only in the frozen legacy checker); do not use them
as authority. Keep this bounded inventory rather than the former unfiltered search.
