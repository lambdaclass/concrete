# ISS-0050: obligation declaration search

Design only. Owner authorized both redesigns on 2026-11-15 (“yes do all this”).
Goals: total, provable rules and refusals identifying one rule and one precise source location.
Follow [PHILOSOPHY.md](../PHILOSOPHY.md): one authoritative definition, checked views.
Source anchors below resolve at `origin/wip/spec-integration-head`,
`aa44140a33747fa37ece59c76090972e564c65b4`; these are design obligations, not executed proofs.

## Mechanism and admission

Retain `ObligationTable.lookup` as a domain helper whose entire implementation is
`table.find? (fun o => o.id == requested)`. `List.find?` owns traversal;
`DecidableEq ObligationId` supplies constructive, lawful equality. No custom recursion remains.
Raw-list search retains first-match semantics, including hostile duplicate inputs.
Admitted obligation tables separately require `(table.map SafetyObligation.id).Nodup`.
Admission rejects collisions with both records and their input occurrences retained;
it never silently keeps the first. Distinct obligations use the existing discriminator.
This implements the owner's requested admission invariant, not a new identity policy.

## Laws and controls

Quantify each law over a same-type search subject `L`; instantiate with lookup and mutants.
Use identical inputs and premises; a mutant must falsify the conclusion.

* First match: `L table req = some found ↔ ∃ before after,
  table = before ++ found :: after ∧ (∀ o ∈ before, o.id ≠ req) ∧ found.id = req`.
* Absence: `L table req = none ↔ ∀ o ∈ table, o.id ≠ req`.
* Preserve `ObligationTable.lookup_id` as a corollary with its full existing statement.
  The equality to the wrapper's body is only a representation lemma, not a Law.

Closed witness: `[other, a, b]`, with `other.id ≠ req`, `a.id = b.id = req`,
and `a.key ≠ b.key`. Construct distinct keys with `Exec.Demo.mainF` and `Exec.Demo.logF`.
Expected result is `some a`; reverse-order search returns `some b`, head-only and
always-none search return `none`. Add empty and absent-id cases and a wrong-id return
mutant. Duplicate fixtures test raw search without a Nodup premise. Admission controls
use duplicate-good and distinct-id tables; a keep-first admission mutant must fail.
Prove admitted lookup is unambiguous, independently of list order.

## Informative discharge

Replace the authoritative discharge operation with a checked result carrying success or a
typed refusal; any retained `Option` API is only its proved erasure. No consumer reruns checks
to explain `none`. This diagnostic work and admission uniqueness are required in this redesign.
Preserve current outer priority: unusable context, requested-identity mismatch, missing registration.
Within unusable context, select the first unequal field in declared `EvidenceContext` order
(obligation, target, contract, checker, spec), proving equivalence to existing whole-context equality.
This refines diagnostics without changing which evidence succeeds.

Each refusal carries exactly one registered `RuleId`, requested obligation/site, failure payload,
and exactly one primary location; collision payloads also retain both records as related evidence.
Share the independently validated input-occurrence provenance contract in
[ISS-0047](ISS-0047-SITE-TABLE-OBLIGATIONS.md#diagnostic-boundary).
The primary location is the requested discharge occurrence, not the evidence's potentially
mismatched site. Missing obligation/site registration still points to that occurrence in the
actual input/manifest bytes. Require located input at the compiler boundary; do not invent a
span, use an unknown-location sentinel, or trust a failing site's own coordinates.

Prove success iff the old discharge succeeds, with the same authoritative record and proof;
prove erasure equality for all raw inputs and preserve existing mismatch, unregistered and
soundness theorem statements through that erasure. For every refusal, prove its selected
branch fails, all higher-priority branches pass, its single rule denotes that check, and its
primary location is exactly the requested occurrence with byte-derived coordinates.
Controls at fixed inputs: swap reason/rule, substitute another existing valid occurrence,
lose the site, erase the location, and erase the whole payload. Each must violate the
corresponding law, even when both candidate locations are otherwise valid.

## Proof boundary and owner dependencies

All searches and admission checks terminate on finite lists; context comparisons are decidable.
Prove the laws constructively by list induction and equality transport, with no new axioms,
classical choice, held assumptions, or assumed claims. A `DischargedFact` transports the supplied
`SafetyEvidence.proof`; the design does not prove arbitrary `claim` propositions.
Rule registration is an explicit owner dependency shared with ISS-0047: present the proposed
check-to-rule mapping for approval; do not allocate rule identities or register undecided rules.
ISS-0048 startup state and ISS-0073 startup/ABI are outside this redesign. The search itself
has no ISS-0068 path-character dependency; diagnostic provenance shares ISS-0047's boundary.

## Mechanically resolved evidence and migration

At the pinned integration ref, declarations are anchored as follows (paths under `ConcreteSpec/`):

* `Spec/Basics.lean:27`: `AList.lookup`; its header prescribes `List.find?` for intrinsic ids.
  `Spec/Slice1Syntax.lean:152` (`findDecl`) and `Spec/Authority.lean:151,204`
  (`Authority.Prog.find`, `Authority.ProgOk`) are existing examples, not new authorities.
* `Spec/Ident.lean:90,367,836`: `ObligationId`, `StableKey`, `RuleId`.
  `Spec/Exec.lean:985,987`: `Exec.Demo.logF`, `Exec.Demo.mainF`.
* `Spec/Prim.lean:255–320`: `EvidenceContext`, `SafetyEvidence`, `SafetyObligation`,
  `SafetyEvidence.Usable`, `ObligationTable`, `ObligationTable.lookup`,
  `ObligationTable.lookup_id`, `DischargedFact`, `SafetyEvidence.discharge` all exist.
  `EvidenceDemo.current` exists at :341. `Usable` is context equality, not five existing checks.
* `Proofs/Prim.lean:134,143,155,191,210`: discharge context/obligation mismatch,
  soundness, target mismatch and unregistered theorems. Review placement of `lookup_id`
  in `Proofs/Prim` is stale: the declaration is in `Spec/Prim.lean:299`.

Migrate the lookup body/proof, add independent laws and controls, add unique admission and checked
refusals, then migrate consumers through the checked result or proved erasure. Check anchored uses
with `git grep -n -E 'ObligationTable|SafetyEvidence\.(discharge|Usable)|DischargedFact' <ref> -- ConcreteSpec`.
No bare-English `discharge` inventory is evidence. See ISS-0047 for unresolved reviewer names.
