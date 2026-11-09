# ISS-0050: obligation declaration search

Design only; verified against tracked files at `3b4d01d6fee3910436eb7b63e49847f1e734a3d3`. No implementation is authorized by this note. The architectural criterion is one authoritative fact with checked views (docs/PHILOSOPHY.md:68-73).

Non-vacuity convention: each proposed law is a schema quantified over the named executable/predicate subject, with that subject occurring in its conclusion. Instantiate it with the intended subject and with a mutant of exactly the same type, at identical inputs and identical premises. A negative control must falsify the conclusion, never manufacture an inconsistent premise or assume the desired output. Closed cases below are proposed proof obligations, not claims of executed Lean checks. Preservation of an existing theorem means its full quantified proposition, including refusal and hostile-input cases, survives; proof scripts may change.

## Pair and decision: VIEW

ConcreteSpec/Spec/Prim.lean:288-295 declares an intrinsic-id table but searches it with custom recursion. ConcreteSpec/Spec/Basics.lean:7-13 assigns declaration-table search to List.find?. Prim's lookup_id (:299-309) proves intrinsic identity; discharge (:320-340) relies on it. The library owns search mechanics; Prim owns the predicate selecting an obligation by typed id. List.find? is a library declaration, not a second tracked repository implementation; Basics is the tracked architectural authority for using it.

Keep the discoverable ObligationTable.lookup API as a domain-specific derived view: `obligations.find? (fun obligation => obligation.id == requested)`. No recursive clauses remain. This Boolean spelling uses the existing DecidableEq and must agree with propositional id equality. Preserve first match even on duplicate ids; do not add uniqueness or change evidence discharge policy.

## Laws and discriminators

Let L have exact type `ObligationTable → ObligationId → Option SafetyObligation`.

* `∀ table requested, L table requested = table.find? (fun o => o.id == requested)`. Subject L occurs in conclusion. Closed id `EvidenceDemo.current.obligation`, `a := ⟨id, ⟨.function Exec.Demo.mainF, ⟨[]⟩⟩⟩`, `b := ⟨id, ⟨.function Exec.Demo.logF, ⟨[]⟩⟩⟩` (both SafetyObligation): table `[a,b]` returns a. Mutant L searches the reverse list and returns b. Same input and no premises; both entries have requested id, so lookup_id alone cannot catch this error. StableKey constructors are at ConcreteSpec/Spec/Ident.lean:359-370.
* Retain exact `∀ table requested found, L table requested = some found → found.id = requested`. Closed table `[a]`, requested a.id, found a satisfies it. Same-type mutant returning a for a distinct requested id leaves success true and refutes intrinsic identity. No assumption of table uniqueness is introduced.

The single correspondence equation subsumes nil, match, nonmatch and missing-id behavior; add closed empty and missing-id cases as regressions, not extra independent semantic laws. Decision conversion from == to equality is covered for all typed ids by DecidableEq correctness.

## Existing strength and order

Keep lookup_id's statement exactly. Rewrite its proof using List.find? success facts or induction over the shared library search. Keep SafetyEvidence.discharge and every context/obligation/target-mismatch, unregistered and soundness theorem in Proofs/Prim unchanged. Keep EvidenceDemo and all hostile-input controls; discharge still returns the first authoritative obligation record with its persistent key. No unrelated local-identity lookup changes are authorized.

Order: replace lookup body in Spec/Prim; adapt lookup_id; add correspondence equation and duplicate-key discriminator in Proofs/Prim; rebuild all inventory users. No change to Basics or List is necessary.

## QUESTIONS

None. Whether duplicate obligation identities should be rejected is a separate admission-policy question; this design preserves current first-match behavior and does not decide it.

## Verified user and affected declaration inventory

Repository-wide tracked-text search at the SHA above, excluding historical legacy/, task material, issue rows and proposal notes. Includes declarations, qualified and unqualified references, proof unfolding, examples, comments and documentation; broad matches intentionally retain dependent users rather than losing namespace-local calls. No production user is excluded by a hand-picked file list. External Lean library internals are not repository users.

Search expression: `\b(ObligationTable|lookup_id|discharge|SafetyObligation)\b`. Reproduce with `git grep -n -E` over the tracked nonhistorical sources (the expression uses standard word boundaries).

Every matching use (path:line):

```text
ConcreteSpec/Proofs/ObserveReport.lean:7: the pending request, and terminal histories must discharge every request.
ConcreteSpec/Proofs/Own.lean:930: /-- Helper (computation). Normalizes discharge and conflict checks; drop_preserves and leave_no_activation_slot establish cleanup safety. (approved: APPR-C5-codex-902ce88a). -/
ConcreteSpec/Proofs/Prim.lean:135: (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
ConcreteSpec/Proofs/Prim.lean:137: e.discharge obligations current requested = none := by
ConcreteSpec/Proofs/Prim.lean:138: simp [SafetyEvidence.discharge, mismatch]
ConcreteSpec/Proofs/Prim.lean:144: (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
ConcreteSpec/Proofs/Prim.lean:146: e.discharge obligations current requested = none := by
ConcreteSpec/Proofs/Prim.lean:148: · simp [SafetyEvidence.discharge, usable, mismatch]
ConcreteSpec/Proofs/Prim.lean:149: · simp [SafetyEvidence.discharge, usable]
ConcreteSpec/Proofs/Prim.lean:151: /-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
ConcreteSpec/Proofs/Prim.lean:156: (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:157: match e.discharge obligations current requested with
ConcreteSpec/Proofs/Prim.lean:159: ObligationTable.lookup obligations requested = none
ConcreteSpec/Proofs/Prim.lean:161: ObligationTable.lookup obligations requested = some discharged.obligation ∧
ConcreteSpec/Proofs/Prim.lean:167: unfold SafetyEvidence.discharge at hnone
ConcreteSpec/Proofs/Prim.lean:174: unfold SafetyEvidence.discharge at hsome
ConcreteSpec/Proofs/Prim.lean:182: ObligationTable.lookup_id obligations requested obligation hlookup⟩
ConcreteSpec/Proofs/Prim.lean:188: /-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
ConcreteSpec/Proofs/Prim.lean:189: Evidence for one obligation identity cannot discharge a retrieved obligation with a different
ConcreteSpec/Proofs/Prim.lean:192: (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
ConcreteSpec/Proofs/Prim.lean:193: (requested : ObligationId) (obligation : SafetyObligation)
ConcreteSpec/Proofs/Prim.lean:194: (lookup : ObligationTable.lookup obligations requested = some obligation)
ConcreteSpec/Proofs/Prim.lean:196: e.discharge obligations current requested = none := by
ConcreteSpec/Proofs/Prim.lean:201: have found := ObligationTable.lookup_id obligations requested obligation lookup
ConcreteSpec/Proofs/Prim.lean:208: /-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
ConcreteSpec/Proofs/Prim.lean:211: (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
ConcreteSpec/Proofs/Prim.lean:212: (requested : ObligationId) (absent : ObligationTable.lookup obligations requested = none) :
ConcreteSpec/Proofs/Prim.lean:213: e.discharge obligations current requested = none := by
ConcreteSpec/Proofs/Prim.lean:216: · unfold SafetyEvidence.discharge
ConcreteSpec/Proofs/Prim.lean:558: def obligation : SafetyObligation := ⟨EvidenceDemo.current.obligation, obligationKey⟩
ConcreteSpec/Proofs/Prim.lean:559: def obligations : ObligationTable := [obligation]
ConcreteSpec/Proofs/Prim.lean:563: def mismatchedObligation : SafetyObligation := ⟨⟨⟨0⟩, .safety, 1⟩, otherKey⟩
ConcreteSpec/Proofs/Prim.lean:564: def mismatchedObligations : ObligationTable := [mismatchedObligation]
ConcreteSpec/Proofs/Prim.lean:566: /-- Mutant consumer that fabricates an obligation record without checking `ObligationTable.lookup`. -/
ConcreteSpec/Proofs/Prim.lean:568: (_obligations : ObligationTable) (_current : EvidenceContext) (_requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:574: (_obligations : ObligationTable) (_current : EvidenceContext) (_requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:579: (_obligations : ObligationTable) (_current : EvidenceContext) (_requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:583: example : match evidence.discharge obligations EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:586: ObligationTable.lookup obligations obligation.id = none
ConcreteSpec/Proofs/Prim.lean:589: ObligationTable.lookup obligations obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:593: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.obligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWrongKey`). -/
ConcreteSpec/Proofs/Prim.lean:597: ObligationTable.lookup obligations obligation.id = none
ConcreteSpec/Proofs/Prim.lean:600: ObligationTable.lookup obligations obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:606: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.obligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.alwaysRefuseDischarge`). -/
ConcreteSpec/Proofs/Prim.lean:610: ObligationTable.lookup obligations obligation.id = none
ConcreteSpec/Proofs/Prim.lean:613: ObligationTable.lookup obligations obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:620: (_obligations : ObligationTable) (current : EvidenceContext) (_requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:625: example : match evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id with
ConcreteSpec/Proofs/Prim.lean:628: ObligationTable.lookup mismatchedObligations mismatchedObligation.id = none
ConcreteSpec/Proofs/Prim.lean:631: ObligationTable.lookup mismatchedObligations mismatchedObligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:635: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.mismatchedObligations`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeIgnoringObligation`). -/
ConcreteSpec/Proofs/Prim.lean:640: ObligationTable.lookup mismatchedObligations mismatchedObligation.id = none
ConcreteSpec/Proofs/Prim.lean:643: ObligationTable.lookup mismatchedObligations mismatchedObligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:655: (obligations : ObligationTable) (_current : EvidenceContext) (requested : ObligationId) :
ConcreteSpec/Proofs/Prim.lean:657: match ObligationTable.lookup obligations requested with
ConcreteSpec/Proofs/Prim.lean:665: example : match evidence.discharge obligations contractChanged obligation.id with
ConcreteSpec/Proofs/Prim.lean:668: ObligationTable.lookup obligations obligation.id = none
ConcreteSpec/Proofs/Prim.lean:671: ObligationTable.lookup obligations obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:675: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.contractChanged`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeIgnoringContext`). -/
ConcreteSpec/Proofs/Prim.lean:679: ObligationTable.lookup obligations obligation.id = none
ConcreteSpec/Proofs/Prim.lean:682: ObligationTable.lookup obligations obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:690: example : match evidence.discharge [] EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:693: ObligationTable.lookup [] obligation.id = none
ConcreteSpec/Proofs/Prim.lean:696: ObligationTable.lookup [] obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:700: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.obligations`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWithoutTable`). -/
ConcreteSpec/Proofs/Prim.lean:704: ObligationTable.lookup [] obligation.id = none
ConcreteSpec/Proofs/Prim.lean:707: ObligationTable.lookup [] obligation.id = some result.obligation ∧
ConcreteSpec/Proofs/Prim.lean:714: (`2 + 2 = 4`) are true propositions, yet evidence for `obligation.id` cannot discharge
ConcreteSpec/Proofs/Prim.lean:716: example : evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id = none :=
ConcreteSpec/Proofs/Prim.lean:720: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_target_mismatch`, case `ConcreteSpec.Core.Vacuity.mismatchedObligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWithoutTable`). -/
ConcreteSpec/Proofs/Prim.lean:721: example : (ObligationTable.lookup mismatchedObligations mismatchedObligation.id =
ConcreteSpec/Proofs/Prim.lean:731: example : evidence.discharge [] EvidenceDemo.current obligation.id = none :=
ConcreteSpec/Proofs/Prim.lean:734: /-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_unregistered`, case `ConcreteSpec.Core.Vacuity.obligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWithoutTable`). -/
ConcreteSpec/Proofs/Prim.lean:735: example : ObligationTable.lookup [] obligation.id = none ∧
ConcreteSpec/Spec/Frame.lean:953: direct-syscall launcher supplied by R-0584. Toy launches do not discharge it. -/
ConcreteSpec/Spec/Ident.lean:18: and discharge tokens are keyed by site; a program's site table maps each site to its byte-ranged
ConcreteSpec/Spec/Ident.lean:88: obligations of the same kind at one site distinct. Facts and discharge evidence name this identity,
ConcreteSpec/Spec/Leak.lean:19: does not discharge either claim. No scalar secret typing rule is chosen here (slice 7).
ConcreteSpec/Spec/Own.lean:232: an assignment to an existing binding must discharge/reinitialize its previous obligation.
ConcreteSpec/Spec/Prim.lean:272: structure SafetyObligation where
ConcreteSpec/Spec/Prim.lean:288: abbrev ObligationTable := List SafetyObligation
ConcreteSpec/Spec/Prim.lean:292: def ObligationTable.lookup : ObligationTable → ObligationId → Option SafetyObligation
ConcreteSpec/Spec/Prim.lean:295: if obligation.id = requested then some obligation else ObligationTable.lookup rest requested
ConcreteSpec/Spec/Prim.lean:299: theorem ObligationTable.lookup_id (obligations : ObligationTable) (requested : ObligationId)
ConcreteSpec/Spec/Prim.lean:300: (found : SafetyObligation) (success : ObligationTable.lookup obligations requested = some found) :
ConcreteSpec/Spec/Prim.lean:303: | nil => simp [ObligationTable.lookup] at success
ConcreteSpec/Spec/Prim.lean:306: · simp [ObligationTable.lookup, same] at success
ConcreteSpec/Spec/Prim.lean:309: · exact ih (by simpa [ObligationTable.lookup, same] using success)
ConcreteSpec/Spec/Prim.lean:313: obligation : SafetyObligation
ConcreteSpec/Spec/Prim.lean:320: def SafetyEvidence.discharge {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Spec/Prim.lean:321: (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId) :
ConcreteSpec/Spec/Prim.lean:325: match hlookup : ObligationTable.lookup obligations requested with
ConcreteSpec/Spec/Prim.lean:329: ObligationTable.lookup_id obligations requested obligation hlookup
ROADMAP.md:406: | R-0585 | Vacuity checks and contract-strength labels from the first proof slice | no fact reads "proved" when its theorem holds only under an unsatisfiable precondition or premise set, from the first proof slice on; the checker reports a context as infeasible wherever its octagon is empty, and a discharge inside an infeasible context is marked vacuous, never established, unless the infeasibility itself is the claim (a dead branch), applying OD120 from B1 rather than F; each proved claim carries its strength (a point instance, one direction, an equivalence or a full contract), so a single-input proof never reads as a universal contract; planted vacuous and point-only proofs are caught; obligation sufficiency stays with R-0460 and the phase-F validation project with R-0219 | R-0575 |
docs/archive/crucial-2026-11-06/DIAGNOSTICS.md:58: discharge the lemma's side conditions.
docs/archive/no-hidden-traps-2026-11-06/PROPOSAL.md:197: 4. a Lean discharge.
docs/archive/no-hidden-traps-2026-11-06/PROPOSAL.md:267: **(f) A Lean discharge.** A binary search's midpoint `lo + (hi - lo) / 2 < len(v)` needs facts about
docs/archive/no-hidden-traps-2026-11-06/PROPOSAL.md:448: 6. **Lean discharge**: a theorem named through the evidence ledger whose statement is exactly the
docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md:209: on Core does not discharge these rows.
docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md:522: discharge them with Lean's own program-verification tools (`Std.Do`, `mvcgen`, `grind`) instead of
docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md:528: discharge their trust assumptions instead of carrying them.
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:811: | C | “Loop-bound…named discharge class”, 13365–13368 | duplicate of R-0244 | 6 | merge into R-0244 acceptance |
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:856: | C | R-0244 auto-discharge/call-graph composition, 14460–14468 | live | 6 | keep under R-0244 |
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:3185: out of the trusted base.** The per-rule discharge below (R-01..R-21), the
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:166: R-0102	register	2, 3, 4	split	E	Record and discharge LCB-adapter semantic gaps for arithmetic, floats, memory, traps and conversions using explicit checks or unsupported classifications.	-	The old SSA-to-QBE ledger retires, while the enumerated equivalence obligations remain.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:220: R-0171	register	1, 4	split	F	Every new proof-automation route must preserve trust classifications and pass route-specific attempts to upgrade weaker evidence into proof.	-	The universal firewall and attack controls survive the old discharge implementation.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:272: R-0465	register	1	split	F	Every discharge route, including multi-kernel routes, must obey one verdict authority that preserves assumption debt.	-	The universal assumption-axis firewall survives while the enumerated legacy adapter wiring retires.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:468: R-0459	backlog	4, 6; after-1.0/research	split	E	Support non-arithmetic obligations with replayable kernel discharge, freshness-bound hand proofs, and separately scoped relational proofs without granting proof status to uncertified solver results.	-	The body mixes old arithmetic and typed-term machinery with exhaustiveness, relational-proof and hand-proof requirements.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:476: R-0256	backlog	4, 6	backlog	E	-	New-compiler verification users demonstrate that annotations for supported arithmetic obligations block practical use.	The body gates inference on measured annotation burden and requires independent discharge of every inferred fact.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:477: R-0257	backlog	4, 6	carried	E	-	-	The objective connects runtime-safety proofs to invariants and frame facts with traceable discharge and actionable diagnostics.
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:479: R-0452	backlog	6; after-1.0/research	backlog	E	-	The new compiler discharges window-index obligations using frame facts and checked newtype hypotheses sufficiently to measure a zlib inflate port.	The body defines a density workload with bit-exact corpus tests, adversarial streams and measured discharge ratios.
docs/archive/roadmap-2026-11/task_disposition.tsv:103: R-0102	Write a QBE semantic-gap ledger before broad coverage	transferred-with-obligations	A	R-0506	Record and discharge LCB-adapter semantic gaps for arithmetic, floats, memory, traps and conversions using explicit checks or unsupported classifications.		superseded — the operation gap ledger becomes typed pass obligations (R-0506) and the adapter's obligations (R-0517).
docs/archive/roadmap-2026-11/task_disposition.tsv:170: R-0169	Add simple auto-discharge for structural obligations that do not need human proof search	deferred-with-trigger			Add simple auto-discharge for structural obligations that do not need human proof search. V1 shapes: reflexive field projection, tuple/struct constructor-destructor round trips, enum tag preservation, fixed-array literal length, direct call wrapper, and source-contract metadata erasure.	A new-compiler external trial measures structural obligations as a dominant proof-authoring cost.	pull-gated, no committed plan; trigger: A new-compiler external trial measures structural obligations as a dominant proof-authoring cost.
docs/archive/roadmap-2026-11/task_disposition.tsv:171: R-0170	Add operational VC auto-discharge as the next automation tier after structural auto-discharge	transferred-with-obligations	E	R-0177	Operational proof automation must use exact new-Core functional models and Lean verification tools, preserve fixed-width trap semantics and report unsupported leaves without upgrading evidence.		Operational proof automation must use exact new-Core functional models and Lean verification tools, preserve fixed-width trap semantics and report unsupported leaves without …
docs/archive/roadmap-2026-11/task_disposition.tsv:258: R-0257	Connect runtime-safety obligations to loop, frame, and dependency facts	transferred-with-obligations	E	R-0259	Connect runtime-safety obligations to loop, frame, and dependency facts.		The objective connects runtime-safety proofs to invariants and frame facts with traceable discharge and actionable diagnostics.
docs/archive/roadmap-2026-11/task_disposition.tsv:262: R-0261	Discharge the soundness-bridge area semantic obligations through R-0496's normative Core semantics	transferred-with-obligations	A	R-0496	Discharge the soundness-bridge area semantic obligations through R-0496's normative Core semantics; do not introduce a separate subset interpreter. (Superseded objective, 2026-10-18: Build source semantics for the provable subset as needed by rule discharge, not as a speculative full-language …		duplicate of R-0496/R-0460: merge; already explicitly reconciled.
docs/archive/roadmap-2026-11/task_disposition.tsv:459: R-0459	Add obligation families that are not arithmetic, so the ledger can demand	transferred-with-obligations	E	R-0495	Support non-arithmetic obligations with replayable kernel discharge, freshness-bound hand proofs, and separately scoped relational proofs without granting proof status to uncertified solver results.		Support non-arithmetic obligations with replayable kernel discharge, freshness-bound hand proofs, and separately scoped relational proofs without granting proof status to …
docs/archive/roadmap-2026-11/task_disposition.tsv:465: R-0465	Extend the discharge-adapter firewall to the assumption axis, and bring the	done					done for its enumerated wiring; the remaining universal-verdict requirement is carried by R-0494.
docs/decisions.md:518: certificates are strengthened to discharge compiler-execution trust where they cover it, any gap they
docs/decisions.md:678: obligation discharges it, and every discharge edge is recorded, each obligation identity with
docs/decisions.md:1332: 5. **`trusted` and `with(Unsafe)` never discharge an operational capability** (`Fs`, `Network`, `Env`,
```

Enclosing declarations, including every directly affected existing theorem and example (names are original source headers; continuation lines remain in the source):

```text
ConcreteSpec/Proofs/ObserveReport.lean:1: (module documentation or namespace context)
ConcreteSpec/Proofs/Own.lean:896: theorem enter_progress (types : Var → Option Descriptor) (s : State) (params : List Var)
ConcreteSpec/Proofs/Prim.lean:134: theorem SafetyEvidence.discharge_context_mismatch {claim : ObligationId → Prop}
ConcreteSpec/Proofs/Prim.lean:143: theorem SafetyEvidence.discharge_obligation_mismatch {claim : ObligationId → Prop}
ConcreteSpec/Proofs/Prim.lean:155: theorem SafetyEvidence.discharge_sound {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:191: theorem SafetyEvidence.discharge_target_mismatch {claim : ObligationId → Prop}
ConcreteSpec/Proofs/Prim.lean:210: theorem SafetyEvidence.discharge_unregistered {claim : ObligationId → Prop}
ConcreteSpec/Proofs/Prim.lean:558: def obligation : SafetyObligation := ⟨EvidenceDemo.current.obligation, obligationKey⟩
ConcreteSpec/Proofs/Prim.lean:559: def obligations : ObligationTable := [obligation]
ConcreteSpec/Proofs/Prim.lean:563: def mismatchedObligation : SafetyObligation := ⟨⟨⟨0⟩, .safety, 1⟩, otherKey⟩
ConcreteSpec/Proofs/Prim.lean:564: def mismatchedObligations : ObligationTable := [mismatchedObligation]
ConcreteSpec/Proofs/Prim.lean:567: def dischargeWithoutTable {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:573: def dischargeWrongKey {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:578: def alwaysRefuseDischarge {claim : ObligationId → Prop} (_e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:583: example : match evidence.discharge obligations EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:594: example : ¬ (match dischargeWrongKey evidence obligations EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:607: example : ¬ (match alwaysRefuseDischarge evidence obligations EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:619: def dischargeIgnoringObligation {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:625: example : match evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id with
ConcreteSpec/Proofs/Prim.lean:636: example : ¬ (match dischargeIgnoringObligation evidence mismatchedObligations EvidenceDemo.current
ConcreteSpec/Proofs/Prim.lean:654: def dischargeIgnoringContext {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ConcreteSpec/Proofs/Prim.lean:665: example : match evidence.discharge obligations contractChanged obligation.id with
ConcreteSpec/Proofs/Prim.lean:676: example : ¬ (match dischargeIgnoringContext evidence obligations contractChanged obligation.id with
ConcreteSpec/Proofs/Prim.lean:690: example : match evidence.discharge [] EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:701: example : ¬ (match dischargeWithoutTable evidence [] EvidenceDemo.current obligation.id with
ConcreteSpec/Proofs/Prim.lean:716: example : evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id = none :=
ConcreteSpec/Proofs/Prim.lean:721: example : (ObligationTable.lookup mismatchedObligations mismatchedObligation.id =
ConcreteSpec/Proofs/Prim.lean:731: example : evidence.discharge [] EvidenceDemo.current obligation.id = none :=
ConcreteSpec/Proofs/Prim.lean:735: example : ObligationTable.lookup [] obligation.id = none ∧
ConcreteSpec/Spec/Frame.lean:942: def ProjectTrap {Artifact : Type} (build : Observe.LoadedProject → Artifact)
ConcreteSpec/Spec/Ident.lean:1: (module documentation or namespace context)
ConcreteSpec/Spec/Ident.lean:73: inductive ObligationKind where
ConcreteSpec/Spec/Leak.lean:1: (module documentation or namespace context)
ConcreteSpec/Spec/Own.lean:228: def accessLoan (s : State) (i : LoanId) (m : Mode) : Option State :=
ConcreteSpec/Spec/Prim.lean:272: structure SafetyObligation where
ConcreteSpec/Spec/Prim.lean:288: abbrev ObligationTable := List SafetyObligation
ConcreteSpec/Spec/Prim.lean:292: def ObligationTable.lookup : ObligationTable → ObligationId → Option SafetyObligation
ConcreteSpec/Spec/Prim.lean:299: theorem ObligationTable.lookup_id (obligations : ObligationTable) (requested : ObligationId)
ConcreteSpec/Spec/Prim.lean:312: structure DischargedFact (claim : ObligationId → Prop) where
ConcreteSpec/Spec/Prim.lean:320: def SafetyEvidence.discharge {claim : ObligationId → Prop} (e : SafetyEvidence claim)
ROADMAP.md:1: (module documentation or namespace context)
docs/archive/crucial-2026-11-06/DIAGNOSTICS.md:1: (module documentation or namespace context)
docs/archive/no-hidden-traps-2026-11-06/PROPOSAL.md:155: theorem whose statement is exactly the conjunction of those obligations (spelling: owner
docs/archive/roadmap-2026-11/ROADMAP_DECISIONS.md:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/ROADMAP_RECONCILIATION.md:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/ROADMAP_TASKS.md:3045: example cases and `scripts/tests/check_arithmetic_site_evidence.sh`. Acceptance: the same expression
docs/archive/roadmap-2026-11/roadmap_restructure_2026-11-02.tsv:1: (module documentation or namespace context)
docs/archive/roadmap-2026-11/task_disposition.tsv:1: (module documentation or namespace context)
docs/decisions.md:1: (module documentation or namespace context)
```
