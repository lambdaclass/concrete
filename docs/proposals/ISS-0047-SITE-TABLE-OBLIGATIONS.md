# ISS-0047: site table obligations

Design only; verified against tracked files at `3b4d01d6fee3910436eb7b63e49847f1e734a3d3`. No implementation is authorized by this note. The architectural criterion is one authoritative fact with checked views (docs/PHILOSOPHY.md:68-73).

Non-vacuity convention: each proposed law is a schema quantified over the named executable/predicate subject, with that subject occurring in its conclusion. Instantiate it with the intended subject and with a mutant of exactly the same type, at identical inputs and identical premises. A negative control must falsify the conclusion, never manufacture an inconsistent premise or assume the desired output. Closed cases below are proposed proof obligations, not claims of executed Lean checks. Preservation of an existing theorem means its full quantified proposition, including refusal and hostile-input cases, survives; proof scripts may change.

## Pair and decision: VIEW

ConcreteSpec/Spec/Observe.lean:167-169 checks only lookup-visible entries in SiteTable.Valid; :251-256 checks every supplied entry in ProjectSources.Realizes and separately requires exact files/sites and unique keys. WellFormed at :576-589 concerns events and requests, while adapter.loaded (:461) already establishes source admission. Thus the two source predicates differ under shadowing; event correctness is not source validity.

Single validity predicate, owned by Spec/Observe: `SiteTable.EntriesValid sources sites := ∀ entry ∈ sites, ∃ bytes, AList.lookup sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes`. It checks all entries. Key uniqueness does NOT belong in it: byte correspondence and key identity are independent requirements, and Realizes already owns uniqueness. Preserve duplicate refusal rather than using correspondence as its substitute. Source-key uniqueness also remains a separate Realizes requirement; EntriesValid deliberately uses the established source lookup policy.

Realizes references EntriesValid in place of its final quantified clause, retaining all four other conjuncts verbatim. Existing SiteTable.Valid remains a useful lookup-visible view: define it using EntriesValid on the list filtered to entries whose span equals lookup at their key, and retain PathOk in the exposed theorem (structured paths establish it). This avoids another coordinate predicate. Prove equivalence to the old lookup-visible statement, and equivalence to EntriesValid under site-key Nodup.

ExecAdapter.WellFormed references the same predicate through a derived theorem `source_entries_valid A` obtained from A.loaded and load_realizes. Add no independent source proof field: that would repeat admission and burden all constructors. WellFormed's event/request fields remain exactly unchanged; a theorem `WellFormed.source_entries_valid` may expose the inherited guarantee without using those fields as an extra premise.

## Laws and discriminators

Let E have exact type `SourceTable → SiteTable → Prop`.

* `∀ sources sites, E sources sites ↔ ∀ entry ∈ sites, ∃ b, lookup sources entry.2.path = some b ∧ entry.2.LocationOk b`. Closed sources `[(mainSourcePath,[])]`, good point span at offset 0 line/col 1, bad span same fields except line 2, sites `[(⟨0⟩,good),(⟨0⟩,bad)]`. Lookup-only E mutant accepts this table, while the conclusion requires False. Premises are unchanged; no Nodup premise belongs to this law. A coordinate-ignoring mutant fails the singleton bad table. A mutant requiring Nodup fails the duplicate-good table, demonstrating deliberate separation of concerns.
* For V : SourceTable → SiteTable → Prop, `∀ sources sites, V sources sites ↔ ∀ site sp, lookup sites site = some sp → sp.PathOk ∧ ∃ b, lookup sources sp.path = some b ∧ sp.LocationOk b`. Closed good-first/bad-shadowed table above is valid; replacing V by E fails here. This preserves the old strength exactly.
* `Nodup (sites.map Prod.fst) → (V sources sites ↔ E sources sites)`. Subjects V and E both occur in the conclusion. Closed singleton bad table satisfies Nodup; lookup-coordinate-ignoring V mutant accepts it and refutes equivalence. Closed singleton good refutes always-False E. Do not use the shadowed table under this premise.
* R : ProjectSources → LoadedProject → Prop obeys the complete existing Realizes equation with only its final clause factored through E. Closed input one canonical file and the good singleton, matching project, makes R true; R mutant discarding sites fails exact preservation. Duplicate-good input/project makes R false despite E true; a mutant dropping site Nodup fails here. `load input = admitted project ↔ R input project` keeps its existing statement and rejects a keep-first loader on that duplicate case. Loader mutants have exact type `ProjectSources → SourceLoadResult` (use the existing load return type at implementation, not an invented outcome representation).
* `∀ A, E A.sourceProject.sources A.sites` subject `source_entries_valid`'s actual adapter projection occurs in conclusion. Closed admitted demo adapter; a same-type adapter-construction mutant cannot fabricate an invalid project while retaining loaded. Therefore do not pretend a field mutation is a premise-preserving control for this theorem. Test its executable subject instead: `∀ input project, load input = admitted project → E project.sources project.sites`; substitute keep-first-and-ignore-location loader of load's exact type, choosing bad singleton input and its returned project. Its admission premise stays true while E is false. This is the required admission discriminator; the adapter theorem is a corollary, not separately counted evidence.

## Existing strength and order

Preserve load_realizes biconditional, rejection iff no realization, exact input preservation, file/site uniqueness, reorder laws, all diagnostic naming and trap-resolution theorems. Preserve the current load-valid and adapter-valid conclusions with lookup-visible Valid (including PathOk); add EntriesValid consequences. Never strengthen old Valid itself and thereby invalidate shadowed-entry theorems. Every theorem/example in the inventory is either definitionally unchanged by factoring Realizes or reproved through the equivalences above; no antecedent gains uniqueness unless it already had it. Frame launch/build/refusal theorems keep their exact statements.

Order: introduce EntriesValid and equivalences in Spec/Observe and Proofs/Observe2; factor Realizes; reprove loader soundness/completeness; derive adapter source theorem; migrate source obligations in Frame/ObserveReport and examples. Keep WellFormed constructors untouched. No change to accepted projects or refusal ordering.

## QUESTIONS

None. If a later API wants WellFormed to accept adapters without loaded, or wants duplicate source/site keys admitted, that changes the boundary and needs the owner; it is outside this proposal.

## Verified user and affected declaration inventory

Repository-wide tracked-text search at the SHA above, excluding historical legacy/, task material, issue rows and proposal notes. Includes declarations, qualified and unqualified references, proof unfolding, examples, comments and documentation; broad matches intentionally retain dependent users rather than losing namespace-local calls. No production user is excluded by a hand-picked file list. External Lean library internals are not repository users.

Search expression: `\b(SiteTable\.Valid|EntriesValid|Realizes|WellFormed|load_realizes|loaded)\b`. Reproduce with `git grep -n -E` over the tracked nonhistorical sources (the expression uses standard word boundaries).

Every matching use (path:line):

```text
ConcreteSpec/Proofs/Adequacy.lean:1363: (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀) (n : Nat) :
ConcreteSpec/Proofs/Adequacy.lean:1399: (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀) (n : Nat) :
ConcreteSpec/Proofs/Adequacy.lean:1411: (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀)
ConcreteSpec/Proofs/Adequacy.lean:1472: (wellFormed : Observe.ExecAdapter.WellFormed A C input ρ₀)
ConcreteSpec/Proofs/Adequacy.lean:1878: loaded := rfl
ConcreteSpec/Proofs/Adequacy.lean:1888: loaded := rfl
ConcreteSpec/Proofs/Adequacy.lean:1908: theorem erasingWellFormed : Observe.ExecAdapter.WellFormed erasingAdapter Observe.Demo.contract () observationEnv := by
ConcreteSpec/Proofs/Adequacy.lean:1919: theorem keepingWellFormed : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv := by
ConcreteSpec/Proofs/Adequacy.lean:2575: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Adequacy.lean:2617: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Adequacy.lean:2656: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Authority.lean:62: theorem mem_of_no_vars {e : CapExpr} (he : e.WellFormed 0) {ρ : Nat → List Cap} {c : Cap}
ConcreteSpec/Proofs/Frame.lean:98: theorem store_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt) (m' : Memory)
ConcreteSpec/Proofs/Frame.lean:148: private theorem metadataStoreWellFormed : metadataStoreBefore.memory.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Frame.lean:243: theorem alloc_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:301: theorem write_materialized_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:324: theorem rawWrite_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:356: (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:378: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:603: (wellFormed : before.memory.WellFormed tgt) :
ConcreteSpec/Proofs/Frame.lean:693: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:712: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:728: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:786: /-- Helper (inversion). Every C4 effect derivation preserves `ConcreteSpec.Core.Mem.Memory.WellFormed`.
ConcreteSpec/Proofs/Frame.lean:789: (effects : c4Execution tgt before f after) (hw : before.memory.WellFormed tgt) :
ConcreteSpec/Proofs/Frame.lean:790: after.memory.WellFormed tgt := by
ConcreteSpec/Proofs/Frame.lean:904: (wellFormed : (project middle.store).memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:907: (project exit.store).memory.WellFormed tgt := by
ConcreteSpec/Proofs/Frame.lean:925: have hWfNext : (project next.store).memory.WellFormed tgt :=
ConcreteSpec/Proofs/Frame.lean:938: (wellFormed : (project entry.store).memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:960: (wellFormed : (project entry.store).memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:1074: (id storingEntry.store).memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:1186: theorem toySignature_wellFormed : toySignature.WellFormed := by
ConcreteSpec/Proofs/Frame.lean:1369: toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1376: example : toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1501: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1511: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1521: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1531: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1554: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1564: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1574: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1750: (h : pair.WellFormed submitParam submitResult completeParam completeResult) :
ConcreteSpec/Proofs/Frame.lean:1849: pair.submit = submit ∧ pair.complete = complete ∧ pair.WellFormed sp sr cp cr := by
ConcreteSpec/Proofs/Frame.lean:1870: ∀ h, ¬ (⟨submit, complete, h⟩ : KeptPair F H).WellFormed sp sr cp cr := by
ConcreteSpec/Proofs/Frame.lean:2178: | .entered ((module, loaded), input, returned, observation) =>
ConcreteSpec/Proofs/Frame.lean:2179: .entered ((module, (root + 1) :: loaded), input, returned, observation)
ConcreteSpec/Proofs/Frame.lean:2221: let loaded := retainChain root
ConcreteSpec/Proofs/Frame.lean:2222: .entered ((root, loaded), loaded.length, result, encodeToyExit root result)
ConcreteSpec/Proofs/Frame.lean:2237: (source : Observe.ProjectSources) (invalid : ¬ ∃ project, source.Realizes project) :
ConcreteSpec/Proofs/Frame.lean:2239: cases loaded : Observe.LoadedProject.load source with
ConcreteSpec/Proofs/Frame.lean:2240: | refused issue => simp [assembleProject, loaded]
ConcreteSpec/Proofs/Frame.lean:2243: (Observe.LoadedProject.load_admitted source project).mp loaded⟩)
ConcreteSpec/Proofs/Frame.lean:2249: (invalid : ¬ ∃ project, source.Realizes project)
ConcreteSpec/Proofs/Frame.lean:2262: (invalid : ¬ ∃ project, source.Realizes project)
ConcreteSpec/Proofs/Frame.lean:2280: (source : Observe.ProjectSources) (invalid : ¬ ∃ project, source.Realizes project)
ConcreteSpec/Proofs/Frame.lean:2290: (built : assembleProject build source = some (project, artifact)) : source.Realizes project := by
ConcreteSpec/Proofs/Frame.lean:2291: cases loaded : Observe.LoadedProject.load source with
ConcreteSpec/Proofs/Frame.lean:2292: | refused issue => simp [assembleProject, loaded] at built
ConcreteSpec/Proofs/Frame.lean:2295: simpa [assembleProject, loaded] using congrArg (Option.map (fun pair => pair.1)) built
ConcreteSpec/Proofs/Frame.lean:2297: exact (Observe.LoadedProject.load_admitted source project).mp loaded
ConcreteSpec/Proofs/Frame.lean:2306: ∃ project artifact, source.Realizes project ∧ launcher.rootOf artifact = root ∧
ConcreteSpec/Proofs/Frame.lean:2328: (refused : LoadedProject.load source = .refused issue) : ¬ ∃ project, source.Realizes project := by
ConcreteSpec/Proofs/Frame.lean:2340: theorem empty_invalid : ¬ ∃ project, emptyInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2345: theorem backslash_invalid : ¬ ∃ project, backslashInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2350: theorem control_invalid : ¬ ∃ project, controlInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2362: example : (¬ ∃ project, emptyInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2377: example : (¬ ∃ project, parentPath.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2392: example : (¬ ∃ project, duplicateFiles.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2407: example : (¬ ∃ project, duplicate.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2422: example : (¬ ∃ project, missing.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2437: example : (¬ ∃ project, controlInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2452: example : (¬ ∃ project, backslashInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2467: example : (¬ ∃ project, badLocation.Realizes project) ∧
ConcreteSpec/Proofs/Grant.lean:28: preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:29: theorem fresh_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:30: ResultSatisfies WellFormed (fresh s scope) := by
ConcreteSpec/Proofs/Grant.lean:31: change WellFormed { s with
ConcreteSpec/Proofs/Grant.lean:62: /-- Law (subject `ConcreteSpec.Core.Generative.depend`). Registering a dependency preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:63: theorem depend_preserves (s : State) (d : Dependency) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:64: ResultSatisfies WellFormed (depend s d) := by
ConcreteSpec/Proofs/Grant.lean:86: /-- Law (subject `ConcreteSpec.Core.Generative.consume`). Consuming a dependent place preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:87: theorem consume_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:88: ResultSatisfies WellFormed (consume s p) := by
ConcreteSpec/Proofs/Grant.lean:101: /-- Law (subject `ConcreteSpec.Core.Generative.release`). Releasing an allocator preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:102: theorem release_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:103: ResultSatisfies WellFormed (release s i) := by
ConcreteSpec/Proofs/Grant.lean:139: generation preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:140: theorem reset_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:141: ResultSatisfies WellFormed (reset s i) := by
ConcreteSpec/Proofs/Grant.lean:149: have hw : WellFormed u := by
ConcreteSpec/Proofs/Grant.lean:183: `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:184: theorem endScope_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:185: ResultSatisfies WellFormed (endScope s scope) := by
ConcreteSpec/Proofs/Grant.lean:292: theorem nextBrand_fresh (s : State) (hs : WellFormed s) (b : Binder) (hb : b ∈ s.binders) :
ConcreteSpec/Proofs/Grant.lean:359: `WellFormed`. (approved: APPR-C5-codex-902ce88a). -/
ConcreteSpec/Proofs/Grant.lean:360: theorem allocations_have_live_allocator_of_wf (s : State) (hs : WellFormed s) (d : Dependency)
ConcreteSpec/Proofs/Grant.lean:367: theorem allocations_have_live_allocator (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:376: have hw : WellFormed t := by
ConcreteSpec/Proofs/Grant.lean:499: example : ResultSatisfies WellFormed (fresh emptyState ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:503: example : WellFormed emptyState ∧
ConcreteSpec/Proofs/Grant.lean:504: ¬ ResultSatisfies WellFormed (freshNoAdvance emptyState ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:507: example : ResultSatisfies WellFormed (depend phantomOnly heap) :=
ConcreteSpec/Proofs/Grant.lean:511: example : WellFormed phantomOnly ∧
ConcreteSpec/Proofs/Grant.lean:512: ¬ ResultSatisfies WellFormed (dependDuplicatePlace phantomOnly heap) := by decide
ConcreteSpec/Proofs/Grant.lean:515: example : ResultSatisfies WellFormed (consume dependent ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:519: example : WellFormed dependent ∧
ConcreteSpec/Proofs/Grant.lean:520: ¬ ResultSatisfies WellFormed (consumeDropBinders dependent ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:523: example : ResultSatisfies WellFormed (release releaseInput ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:527: example : WellFormed releaseInput ∧
ConcreteSpec/Proofs/Grant.lean:528: ¬ ResultSatisfies WellFormed (releaseAll releaseInput ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:531: example : ResultSatisfies WellFormed (reset initial ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:535: example : WellFormed initial ∧
ConcreteSpec/Proofs/Grant.lean:536: ¬ ResultSatisfies WellFormed (resetNoAdvance initial ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:539: example : ResultSatisfies WellFormed (endScope departing ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:543: example : WellFormed departing ∧
ConcreteSpec/Proofs/Grant.lean:544: ¬ ResultSatisfies WellFormed (endScopeDropAllBinders departing ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:650: example : WellFormed releaseInput ∧
ConcreteSpec/Proofs/Grant.lean:803: theorem fresh_nominal (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:860: example : WellFormed releasedWithPhantom ∧
ConcreteSpec/Proofs/Grant.lean:895: preserves `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:896: theorem lifetime_wf (s : State) (ops : List LifetimeOp) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:897: ResultSatisfies WellFormed (runLifetime s ops) := by
ConcreteSpec/Proofs/Grant.lean:932: (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:943: have huw : WellFormed u := by
ConcreteSpec/Proofs/Grant.lean:946: have htw : WellFormed t := by
ConcreteSpec/Proofs/Grant.lean:973: /-- Mutant: executing a lifetime sequence duplicates a binder brand, breaking `WellFormed`. -/
ConcreteSpec/Proofs/Grant.lean:979: example : ResultSatisfies WellFormed (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:983: example : WellFormed departed ∧
ConcreteSpec/Proofs/Grant.lean:984: ¬ ResultSatisfies WellFormed (runLifetimeDuplicateBrand departed operations) := by decide
ConcreteSpec/Proofs/Grant.lean:993: example : WellFormed departing ∧
ConcreteSpec/Proofs/Hybrid.lean:66: theorem write_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:103: theorem typed_store_frame (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:200: (hw : st.WellFormed tgt) (hm : m.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:202: ({ st with memory := m } : State).WellFormed tgt := by
ConcreteSpec/Proofs/Hybrid.lean:209: (hw : st.WellFormed tgt) (hc : c.Projected st.memory) :
ConcreteSpec/Proofs/Hybrid.lean:210: (st.set p c).WellFormed tgt := by
ConcreteSpec/Proofs/Hybrid.lean:219: theorem write_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:222: | some st' => st'.WellFormed tgt
ConcreteSpec/Proofs/Hybrid.lean:253: theorem rawWrite_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:256: | some st' => st'.WellFormed tgt
ConcreteSpec/Proofs/Hybrid.lean:312: theorem alloc_projected (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:328: theorem materialize_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:331: | some (_, st') => st'.WellFormed tgt
ConcreteSpec/Proofs/Hybrid.lean:342: have halloc : ({ st with memory := m } : State).WellFormed tgt := by
ConcreteSpec/Proofs/Hybrid.lean:359: theorem expose_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:362: | some (_, st') => st'.WellFormed tgt
ConcreteSpec/Proofs/Hybrid.lean:423: theorem rangesDisjoint_separated (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:452: theorem write_frame_disjoint (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:467: theorem materialize_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:500: theorem expose_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:527: theorem materialize_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:608: private theorem not_badState_wf : ¬ badState.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:614: private theorem before_wf : before.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:626: private theorem structState_wf : structState.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:690: example : before.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:705: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:752: | some st' => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:757: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:759: | some st' => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:766: | some st' => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:771: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:773: | some st' => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:780: | some (_, st') => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:785: example : structState.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:787: | some (_, st') => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:794: | some (_, st') => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:799: example : structState.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:801: | some (_, st') => st'.WellFormed linuxX8664
ConcreteSpec/Proofs/Hybrid.lean:822: private theorem observedStart_wf : observedStart.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:830: private theorem observed_wf : observed.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:833: private theorem structState_wf : structState.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:836: private theorem before_wf : before.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:890: example : before.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:903: example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:916: example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:931: example : structState.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Ident.lean:1217: /-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.HasCollision`, `ConcreteSpec.Core.LoadedModules.WellFormed`).
ConcreteSpec/Proofs/Ident.lean:1223: | .admitted admitted => admitted = modules ∧ WellFormed modules := by
ConcreteSpec/Proofs/Ident.lean:1233: /-- Law (subject `ConcreteSpec.Core.LoadedModules.load`, `ConcreteSpec.Core.LoadedModules.WellFormed`).
ConcreteSpec/Proofs/Ident.lean:1237: load modules = .admitted admitted ↔ admitted = modules ∧ WellFormed modules := by
ConcreteSpec/Proofs/Ident.lean:2183: /-- Mutant loader that fabricates a collision naming a module not in the loaded list. -/
ConcreteSpec/Proofs/Ident.lean:2191: LoadedModules.WellFormed [firstModule, collidingModule] :=
ConcreteSpec/Proofs/Ident.lean:2198: LoadedModules.WellFormed [firstModule, collidingModule]) := by
ConcreteSpec/Proofs/Ident.lean:2210: LoadedModules.WellFormed [firstModule, collidingModule]) := by
ConcreteSpec/Proofs/Ident.lean:2230: LoadedModules.WellFormed [firstModule, secondModule] :=
ConcreteSpec/Proofs/Ident.lean:2240: LoadedModules.WellFormed [firstModule, secondModule]) := by
ConcreteSpec/Proofs/Ident.lean:2250: LoadedModules.WellFormed [firstModule, collidingModule] :=
ConcreteSpec/Proofs/Ident.lean:2257: LoadedModules.WellFormed [firstModule, collidingModule]) := by
ConcreteSpec/Proofs/Memory.lean:141: theorem decodePtr_encodePtr (tgt : Target) (ht : tgt.WellFormed) (p : Ptr) (hpv : p.Valid tgt) :
ConcreteSpec/Proofs/Memory.lean:171: theorem empty_wellFormed (tgt : Target) : Memory.empty.WellFormed tgt := by
ConcreteSpec/Proofs/Memory.lean:201: theorem modify_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:207: (m.modify b f).WellFormed tgt := by
ConcreteSpec/Proofs/Memory.lean:262: theorem alloc_fresh (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:364: theorem alloc_ne_existing (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:384: theorem alloc_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:387: | .ok (_, m') => m'.WellFormed tgt
ConcreteSpec/Proofs/Memory.lean:457: theorem candidate_contains_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:471: theorem fromExposed_sound_inv (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:502: theorem fromExposed_sound (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:536: theorem fromExposed_contains (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:549: theorem candidate_onePast_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:562: theorem fromExposed_onePast (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:576: theorem fromExposed_none (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:614: theorem access_valid (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:668: theorem decodePtr_some (tgt : Target) (ht : tgt.WellFormed) (bs : List MByte) (p : Ptr) :
ConcreteSpec/Proofs/Memory.lean:715: theorem store_load (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:754: theorem store_same_block_frame (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:784: theorem store_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:787: | some m' => m'.WellFormed tgt
ConcreteSpec/Proofs/Memory.lean:814: theorem free_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:817: | some m' => m'.WellFormed tgt
ConcreteSpec/Proofs/Memory.lean:865: theorem expose_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:868: | some m' => m'.WellFormed tgt
ConcreteSpec/Proofs/Memory.lean:1036: (hw : m.WellFormed tgt) (b c : MemBlockId) (x : Block)
ConcreteSpec/Proofs/Memory.lean:1170: private theorem not_badEmpty_wf : ¬ badEmpty.WellFormed target := by
ConcreteSpec/Proofs/Memory.lean:1176: private theorem memory_wf : memory.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1180: private theorem exposedMemory_wf : exposedMemory.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1240: example : target.WellFormed ∧ ptr.Valid target ∧
ConcreteSpec/Proofs/Memory.lean:1244: example : target.WellFormed ∧ ptr.Valid target ∧
ConcreteSpec/Proofs/Memory.lean:1253: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1258: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1268: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1287: example : Memory.empty.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1291: example : ¬ badEmpty.WellFormed target := not_badEmpty_wf
ConcreteSpec/Proofs/Memory.lean:1301: example : Memory.empty.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1463: example : memory.WellFormed target ∧ memory.block ⟨0⟩ = some initBlock ∧
ConcreteSpec/Proofs/Memory.lean:1472: | .ok (_, m') => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1477: example : Memory.empty.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1479: | .ok (_, m') => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1493: example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
ConcreteSpec/Proofs/Memory.lean:1506: example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
ConcreteSpec/Proofs/Memory.lean:1519: example : exposedMemory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1541: example : memory.WellFormed target ∧ (2 : Nat) ≠ 0 ∧
ConcreteSpec/Proofs/Memory.lean:1569: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1596: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1622: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1627: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1629: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1636: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1641: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1643: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1661: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1666: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1668: | some m' => m'.WellFormed target
ConcreteSpec/Proofs/Memory.lean:1714: example : memory.WellFormed target ∧ memory.free ⟨0⟩ = some freedMemory ∧
ConcreteSpec/Proofs/Memory.lean:1796: example : exposedMemory.WellFormed target ∧
ConcreteSpec/Proofs/Observe2.lean:96: LoadedProject.load input = .admitted project ↔ input.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:150: cases loaded : LoadedProject.load input with
ConcreteSpec/Proofs/Observe2.lean:153: have h := (LoadedProject.load_admitted input project).mp loaded
ConcreteSpec/Proofs/Observe2.lean:159: to a loaded file and to its actual byte-derived range, line and column. -/
ConcreteSpec/Proofs/Observe2.lean:163: | .admitted project => SiteTable.Valid project.sources project.sites := by
ConcreteSpec/Proofs/Observe2.lean:164: cases loaded : LoadedProject.load input with
ConcreteSpec/Proofs/Observe2.lean:167: have h := (LoadedProject.load_admitted input project).mp loaded
ConcreteSpec/Proofs/Observe2.lean:171: simpa only [loaded] using LoadedProject.load_complete input
ConcreteSpec/Proofs/Observe2.lean:277: (invalid : ¬ ∃ project, input.Realizes project) :
ConcreteSpec/Proofs/Observe2.lean:279: cases loaded : LoadedProject.load input with
ConcreteSpec/Proofs/Observe2.lean:282: exact False.elim (invalid ⟨project, (LoadedProject.load_admitted input project).mp loaded⟩)
ConcreteSpec/Proofs/Observe2.lean:292: have invalid : ¬ ∃ project, input.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:311: cases loaded : LoadedProject.load input with
ConcreteSpec/Proofs/Observe2.lean:314: have h := (LoadedProject.load_admitted input project).mp loaded
ConcreteSpec/Proofs/Observe2.lean:384: (project : LoadedProject) (realizes : input.Realizes project) :
ConcreteSpec/Proofs/Observe2.lean:385: ∃ reordered, other.Realizes reordered := by
ConcreteSpec/Proofs/Observe2.lean:426: cases loaded : LoadedProject.load input with
ConcreteSpec/Proofs/Observe2.lean:436: rw [loaded] at complete
ConcreteSpec/Proofs/Observe2.lean:439: rw [loaded] at valid
ConcreteSpec/Proofs/Observe2.lean:504: theorem parent_invalid : ¬ ∃ project, parentPath.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:514: theorem duplicate_invalid : ¬ ∃ project, duplicate.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:522: theorem missing_invalid : ¬ ∃ project, missing.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:533: theorem duplicateFiles_invalid : ¬ ∃ project, duplicateFiles.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:545: theorem badLocation_invalid : ¬ ∃ project, badLocation.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:558: example : LoadedProject.load good = .admitted admitted ↔ good.Realizes admitted :=
ConcreteSpec/Proofs/Observe2.lean:562: example : ¬ (loseSites good = .admitted admitted ↔ good.Realizes admitted) := by
ConcreteSpec/Proofs/Observe2.lean:564: have realizes : good.Realizes admitted := by
ConcreteSpec/Proofs/Observe2.lean:575: example : LoadedProject.load duplicate = .admitted admitted ↔ duplicate.Realizes admitted :=
ConcreteSpec/Proofs/Observe2.lean:579: example : ¬ (keepFirst duplicate = .admitted admitted ↔ duplicate.Realizes admitted) := by
ConcreteSpec/Proofs/Observe2.lean:585: parentPath.Realizes ⟨admitted.sources, parentPath.sites⟩ :=
ConcreteSpec/Proofs/Observe2.lean:590: parentPath.Realizes ⟨admitted.sources, parentPath.sites⟩) := by
ConcreteSpec/Proofs/Observe2.lean:629: | .admitted project => SiteTable.Valid project.sources project.sites :=
ConcreteSpec/Proofs/Observe2.lean:635: | .admitted project => SiteTable.Valid project.sources project.sites) := by
ConcreteSpec/Proofs/Observe2.lean:662: example : (¬ ∃ project, parentPath.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:736: example : (¬ ∃ project, duplicate.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:763: example : (¬ ∃ project, duplicateFiles.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:785: Refused: site without a loaded file. -/
ConcreteSpec/Proofs/Observe2.lean:790: example : (¬ ∃ project, missing.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:817: example : (¬ ∃ project, badLocation.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:1079: SiteTable.Valid A.sourceProject.sources A.sites := by
ConcreteSpec/Proofs/Observe2.lean:1081: rw [A.loaded] at valid
ConcreteSpec/Proofs/Observe2.lean:1512: (valid : SiteTable.Valid sources sites) (kind : TrapKind) (site : SiteId)
ConcreteSpec/Proofs/Observe2.lean:1530: /-- Concrete source and site tables connecting `SiteTable.Valid`, `tabSource`/`tabSpan`, and
ConcreteSpec/Proofs/Observe2.lean:1564: (valid : SiteTable.Valid sources sites) (kind : TrapKind) (site : SiteId) :
ConcreteSpec/Proofs/Observe2.lean:1578: theorem validSites_valid : SiteTable.Valid validSources validSites := by
ConcreteSpec/Proofs/Observe2.lean:1593: example : SiteTable.Valid validSources validSites ∧
ConcreteSpec/Proofs/Observe2.lean:1609: example : SiteTable.Valid validSources validSites ∧
ConcreteSpec/Proofs/Observe2.lean:1668: (input : C.Input) (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ) :
ConcreteSpec/Proofs/Observe2.lean:1734: (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/Observe2.lean:1758: (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/Observe2.lean:1848: loaded := rfl
ConcreteSpec/Proofs/Observe2.lean:1861: ExecAdapter.WellFormed adapter (makeContract blocking) () answeringOracle := by
ConcreteSpec/Proofs/Observe2.lean:1876: theorem blockingWellFormed : ExecAdapter.WellFormed adapter permittingContract () blockingOracle := by
ConcreteSpec/Proofs/ObserveEnvironment.lean:44: ExecAdapter.WellFormed (Environment.adapter S) C input (E.oracle input) := by
ConcreteSpec/Proofs/ObserveOutput.lean:211: loaded := by decide
ConcreteSpec/Proofs/ObserveReport.lean:46: (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/ObserveReport.lean:78: (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/ObserveReport.lean:176: (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/ObserveReport.lean:253: (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ)
ConcreteSpec/Proofs/ObserveReport.lean:310: rw [A.loaded] at consumer
ConcreteSpec/Proofs/ObserveReport.lean:314: rw [A.loaded] at complete
ConcreteSpec/Proofs/ObserveReport.lean:379: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:408: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:426: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:473: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:505: loaded := by decide }
ConcreteSpec/Proofs/ObserveReport.lean:510: ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle := by
ConcreteSpec/Proofs/ObserveReport.lean:519: example : ¬ ExecAdapter.WellFormed adapter forbiddingContract () blockingOracle := by
ConcreteSpec/Proofs/ObserveReport.lean:537: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:564: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:584: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:670: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:700: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:732: loaded := by decide }
ConcreteSpec/Proofs/ObserveReport.lean:816: /-- A syntactically valid identity whose source file was never loaded. This is precisely the
ConcreteSpec/Proofs/ObserveReport.lean:831: have loaded := A.loaded
ConcreteSpec/Proofs/ObserveReport.lean:832: rw [inputEqual] at loaded
ConcreteSpec/Proofs/ObserveReport.lean:835: rw [refused] at loaded
ConcreteSpec/Proofs/ObserveReport.lean:836: cases loaded
ConcreteSpec/Proofs/ObserveReport.lean:841: /-- Refused: loader refusal cannot be bypassed by supplying a fabricated loaded table. -/
ConcreteSpec/Proofs/ObserveReport.lean:844: have loaded := A.loaded
ConcreteSpec/Proofs/ObserveReport.lean:845: rw [inputEqual] at loaded
ConcreteSpec/Proofs/ObserveReport.lean:848: rw [refused] at loaded
ConcreteSpec/Proofs/ObserveReport.lean:849: cases loaded
ConcreteSpec/Proofs/Own.lean:22: | some s => WellFormed s
ConcreteSpec/Proofs/Own.lean:26: | some s => WellFormed s.own
ConcreteSpec/Proofs/Own.lean:30: | some s => CallState.WellFormed s
ConcreteSpec/Proofs/Own.lean:49: (hf : ∀ s a t, WellFormed s → f s a = some t → WellFormed t) (xs : List α)
ConcreteSpec/Proofs/Own.lean:50: (s t : State) (hs : WellFormed s) (h : xs.foldlM f s = some t) : WellFormed t := by
ConcreteSpec/Proofs/Own.lean:62: theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
ConcreteSpec/Proofs/Own.lean:111: theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
ConcreteSpec/Proofs/Own.lean:185: (m : Mode) (hs : WellFormed s) : StateResult (borrow s p path m) := by
ConcreteSpec/Proofs/Own.lean:241: (m : Mode) (activation : Nat) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:319: (m : Mode) (hs : WellFormed s) : StateResult (reborrow s i path m) := by
ConcreteSpec/Proofs/Own.lean:330: theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
ConcreteSpec/Proofs/Own.lean:362: (hs : WellFormed s) (h : accessOwner s p m = some t) : WellFormed t := by
ConcreteSpec/Proofs/Own.lean:370: (hs : WellFormed s) (h : accessLoan s i m = some t) : WellFormed t := by
ConcreteSpec/Proofs/Own.lean:425: example : WellFormed initial ∧ (move initial ⟨0⟩).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:426: WellFormed ((move initial ⟨0⟩).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:427: example : WellFormed shared ∧ (borrow shared ⟨0⟩ [] .shared).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:428: WellFormed ((borrow shared ⟨0⟩ [] .shared).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:429: example : WellFormed exclusive ∧ (reborrowTo exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:430: WellFormed child := by decide
ConcreteSpec/Proofs/Own.lean:431: example : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
ConcreteSpec/Proofs/Own.lean:432: example : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
ConcreteSpec/Proofs/Own.lean:434: example : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
ConcreteSpec/Proofs/Own.lean:436: example : ¬ WellFormed { child with slots := [] } := by decide
ConcreteSpec/Proofs/Own.lean:438: example : ¬ WellFormed { shared with nextLoan := 0 } := by decide
ConcreteSpec/Proofs/Own.lean:439: example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
ConcreteSpec/Proofs/Own.lean:452: (hs : WellFormed s) : StateResult (bind types x s) := by
ConcreteSpec/Proofs/Own.lean:540: theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
ConcreteSpec/Proofs/Own.lean:614: (hs : WellFormed s) : StateResult (enter types xs s) := by
ConcreteSpec/Proofs/Own.lean:618: have hw : WellFormed { s with
ConcreteSpec/Proofs/Own.lean:654: theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
ConcreteSpec/Proofs/Own.lean:701: (hs : WellFormed s) (hf : s.frames.head? = some f) :
ConcreteSpec/Proofs/Own.lean:710: have hu_wf : WellFormed u := by
ConcreteSpec/Proofs/Own.lean:738: theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
ConcreteSpec/Proofs/Own.lean:754: have hu_wf : WellFormed u := by
ConcreteSpec/Proofs/Own.lean:757: have hw : WellFormed v := by
ConcreteSpec/Proofs/Own.lean:794: /-- Helper (congruence). Updating only the view-binding lookup table of the current frame preserves `WellFormed`. (approved: APPR-C5-codex-902ce88a). -/
ConcreteSpec/Proofs/Own.lean:796: (hs : WellFormed s) (hf : s.frames.head? = some f) :
ConcreteSpec/Proofs/Own.lean:797: WellFormed { s with frames := { f with views := views } :: s.frames.tail } := by
ConcreteSpec/Proofs/Own.lean:822: theorem enterDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:829: have hu_wf : WellFormed u := by
ConcreteSpec/Proofs/Own.lean:838: have hw : WellFormed v := by
ConcreteSpec/Proofs/Own.lean:853: (hs : WellFormed s) : StateResult (life types e s) := by
ConcreteSpec/Proofs/Own.lean:967: example : WellFormed moved ∧ reinitialize moved ⟨0⟩ = some initial := by decide
ConcreteSpec/Proofs/Own.lean:968: example : WellFormed child ∧ (endLoan child ⟨1⟩).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:969: WellFormed ((endLoan child ⟨1⟩).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:970: example : WellFormed shared ∧ (bind types y shared).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:971: WellFormed ((bind types y shared).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:972: example : WellFormed moved ∧ (drop [x] moved).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:973: WellFormed ((drop [x] moved).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:974: example : WellFormed entered ∧ (enter types [y] shared).isSome = true := by decide
ConcreteSpec/Proofs/Own.lean:975: example : WellFormed called ∧ (endLocals 1 called).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:976: WellFormed ((endLocals 1 called).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:977: example : (leave called).isSome = true ∧ WellFormed ((leave called).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:980: example : ¬ WellFormed { shared with slots := [] } := by decide
ConcreteSpec/Proofs/Own.lean:982: example : ¬ WellFormed { initial with slots := initial.slots ++ initial.slots } := by decide
ConcreteSpec/Proofs/Own.lean:984: example : ¬ WellFormed { shared with loans := shared.loans.map fun l =>
ConcreteSpec/Proofs/Own.lean:987: example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
ConcreteSpec/Proofs/Own.lean:996: theorem loan_has_owned_root_of_wf (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
ConcreteSpec/Proofs/Own.lean:1001: theorem moved_no_loan_of_wf (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
ConcreteSpec/Proofs/Own.lean:1033: private theorem map_metadata_preserves (s : State) (hs : WellFormed s)
ConcreteSpec/Proofs/Own.lean:1037: WellFormed { s with loans := s.loans.map fun l =>
ConcreteSpec/Proofs/Own.lean:1061: (hs : WellFormed s) : StateResult (adoptDown s args) := by
ConcreteSpec/Proofs/Own.lean:1089: (hs : WellFormed s.own) : CallOwnResult (passDown s i parameter) := by
ConcreteSpec/Proofs/Own.lean:1099: have hw : WellFormed u := by
ConcreteSpec/Proofs/Own.lean:1137: (hs : CallState.WellFormed s) : CallResult (passDown s i parameter) := by
ConcreteSpec/Proofs/Own.lean:1141: have hw : WellFormed t.own := by
ConcreteSpec/Proofs/Own.lean:1205: (s : CallState) (hs : WellFormed s.own) : CallOwnResult (callLife types e s) := by
ConcreteSpec/Proofs/Own.lean:1216: have hu_wf : WellFormed u := by
ConcreteSpec/Proofs/Own.lean:1342: (s : CallState) (hs : CallState.WellFormed s) : CallResult (callLife types e s) := by
ConcreteSpec/Proofs/Own.lean:1346: have hw : WellFormed t.own := by
ConcreteSpec/Proofs/Own.lean:1393: example : CallState.WellFormed ⟨exclusive, []⟩ ∧ CallState.WellFormed waiting := by decide
ConcreteSpec/Proofs/Own.lean:1397: example : CallState.WellFormed ((callLife types (.enter [y]) waiting).getD waiting) := by decide
ConcreteSpec/Proofs/Own.lean:1399: example : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
ConcreteSpec/Proofs/Own.lean:1400: example : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
ConcreteSpec/Proofs/Own.lean:1402: example : ¬ CallState.WellFormed { waiting with own := initial } := by decide
ConcreteSpec/Proofs/Own.lean:1416: (hp : ∀ s t, WellFormed s → permit s = some t → WellFormed t) (operation : σ → Option (α × σ))
ConcreteSpec/Proofs/Own.lean:1417: (s : Store σ) (a : α) (t : Store σ) (hs : WellFormed s.own)
ConcreteSpec/Proofs/Own.lean:1419: WellFormed t.own := by
ConcreteSpec/Proofs/Own.lean:1482: theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
ConcreteSpec/Proofs/Own.lean:1528: (hf : s.frames.head? = some f) (hs : WellFormed s) (he : entry ∈ f.locals) :
ConcreteSpec/Proofs/Own.lean:1534: have ht : WellFormed t := by
ConcreteSpec/Proofs/Own.lean:1585: (m : Mode) (x : Var) (hs : WellFormed s.own) : CallOwnResult (passOwner s p path m x) := by
ConcreteSpec/Proofs/Own.lean:1644: (path : List Projection) (m : Mode) (x : Var) (hs : CallState.WellFormed s) :
ConcreteSpec/Proofs/Own.lean:1649: have hw : WellFormed t.own := by
ConcreteSpec/Proofs/Own.lean:1767: (hs : WellFormed s) (hf : s.frames.head? = some f) (hl : l ∈ s.loans)
ConcreteSpec/Proofs/Own.lean:1875: example : WellFormed localLoan ∧ leave localLoan = some exited ∧
ConcreteSpec/Proofs/Own.lean:1926: (hs : WellFormed s) (hf : s.frames.head? = some f)
ConcreteSpec/Proofs/Own.lean:1999: example : CallState.WellFormed caller ∧ passOwner caller ⟨1⟩ [.index 3] .mutable z = some waiting ∧
ConcreteSpec/Proofs/Own.lean:2000: CallState.WellFormed waiting := by decide
ConcreteSpec/Proofs/Own.lean:2002: example : ¬ WellFormed { waiting.own with nextLoan := 1 } := by decide
ConcreteSpec/Proofs/Own.lean:2024: example : WellFormed called.own ∧
ConcreteSpec/Proofs/Own.lean:4408: theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
ConcreteSpec/Proofs/Own.lean:4414: theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
ConcreteSpec/Proofs/Own.lean:4467: breaking the frame-local completeness clause of `WellFormed`. -/
ConcreteSpec/Proofs/Own.lean:4496: breaking the frame-local completeness clause of `WellFormed`. -/
ConcreteSpec/Proofs/Own.lean:4516: `s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
ConcreteSpec/Proofs/Own.lean:4529: `s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
ConcreteSpec/Proofs/Own.lean:4542: it to `s.pending`, breaking `CallState.WellFormed`'s pending-flag synchronization. -/
ConcreteSpec/Proofs/Own.lean:4570: example : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4577: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4584: example : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4591: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4598: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4605: example : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4612: example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4619: example : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4626: example : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4633: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide
ConcreteSpec/Proofs/Own.lean:4640: example : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4647: example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCallerInstead (1) (LifecycleWitnesses.called)) := by decide
ConcreteSpec/Proofs/Own.lean:4654: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveKeepLocalLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4661: example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide
ConcreteSpec/Proofs/Own.lean:4668: example : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4675: example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4682: example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4689: example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4696: example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4703: example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4710: example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4717: example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4750: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4759: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4777: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ (RoundThreeWitnesses.localLoan).loans) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)).activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) ∧ ¬ ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leaveAllLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4816: the frame-local completeness clause of `WellFormed`. -/
ConcreteSpec/Proofs/Own.lean:4822: example : ¬ WellFormed unindexedCopy := by decide
ConcreteSpec/Proofs/Own.lean:4823: example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide
ConcreteSpec/Proofs/Own.lean:4826: example : ¬ WellFormed { initial with frames := [⟨1, [], []⟩], nextActivation := 2 } := by decide
ConcreteSpec/Proofs/Own.lean:4829: example : ¬ WellFormed { initial with
ConcreteSpec/Proofs/Own.lean:4837: WellFormed s ∧ (drop [x] s).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:4838: WellFormed ((drop [x] s).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:4881: example : WellFormed DirectArgumentWitnesses.caller.own ∧
ConcreteSpec/Proofs/Own.lean:4948: (m : Mode) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:4961: theorem moved_no_loan (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:5001: (hp : ∀ s t, WellFormed s → permit s = some t → WellFormed t)
ConcreteSpec/Proofs/Own.lean:5002: (operation : σ → Option (α × σ)) (s : Store σ) (hs : WellFormed s.own) :
ConcreteSpec/Proofs/Own.lean:5003: ResultSatisfies (fun r : α × Store σ => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5419: example : WellFormed initial ∧
ConcreteSpec/Proofs/Own.lean:5437: example : WellFormed moveWithOtherLoan ∧
ConcreteSpec/Proofs/Own.lean:5469: example : ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5476: example : (∀ s t, WellFormed s → sharedPermit s = some t → WellFormed t) ∧
ConcreteSpec/Proofs/Own.lean:5477: WellFormed intStore0.own ∧
ConcreteSpec/Proofs/Own.lean:5478: ¬ ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5676: (f : Activation) (caller : Nat) (a : Loan) (hs : WellFormed s)
ConcreteSpec/Proofs/Own.lean:5737: example : WellFormed LifecycleWitnesses.called ∧
ConcreteSpec/Proofs/Own.lean:5818: (parameter : Var) (f : Activation) (hs : WellFormed s)
ConcreteSpec/Proofs/Own.lean:5885: example : WellFormed heldSource ∧
ConcreteSpec/Proofs/Own.lean:5904: example : WellFormed heldSourceForLeak ∧
ConcreteSpec/Proofs/Own.lean:5926: (hs : WellFormed s) (found : slot s p = some x) (linear : x.desc.copy = false)
ConcreteSpec/Proofs/Own.lean:5986: example : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
ConcreteSpec/Spec/Authority.lean:60: def CapExpr.WellFormed (n : Nat) (e : CapExpr) : Prop :=
ConcreteSpec/Spec/Authority.lean:63: instance (n : Nat) (e : CapExpr) : Decidable (e.WellFormed n) := by
ConcreteSpec/Spec/Authority.lean:64: unfold CapExpr.WellFormed; infer_instance
ConcreteSpec/Spec/Authority.lean:167: caps.length = callee.capParams ∧ (∀ e ∈ caps, e.WellFormed f.capParams) ∧
ConcreteSpec/Spec/Authority.lean:198: def FnOk (P : Prog) (f : Fn) : Prop := f.perms.WellFormed f.capParams ∧ ∀ s ∈ f.body, StmtOk P f s
ConcreteSpec/Spec/Frame.lean:273: st.memory.WellFormed tgt → st.memory.access tgt p bs.length align = access →
ConcreteSpec/Spec/Frame.lean:283: st.memory.WellFormed tgt → st.memory.alloc tgt space base size align = .ok (b, m') →
ConcreteSpec/Spec/Frame.lean:290: st.memory.WellFormed tgt → st.places p = some c → c.storage = .materialized r →
ConcreteSpec/Spec/Frame.lean:296: st.memory.WellFormed tgt → st.memory.access tgt p bs.length align = access →
ConcreteSpec/Spec/Frame.lean:316: (project before.store).memory.WellFormed tgt →
ConcreteSpec/Spec/Frame.lean:435: def Signature.WellFormed {Ty Val Reg Word : Type} (sig : Signature Ty Val Reg Word) : Prop :=
ConcreteSpec/Spec/Frame.lean:708: receipt accepted by the paired completion is checked separately by `KeptPair.WellFormed`. -/
ConcreteSpec/Spec/Frame.lean:743: def KeptPair.WellFormed {F H : Type} (pair : KeptPair F H)
ConcreteSpec/Spec/Frame.lean:842: satisfies `KeptPair.WellFormed` for the given endpoints and descriptors, while acceptance preserves
ConcreteSpec/Spec/Frame.lean:843: the endpoint identities, establishes `WellFormed`, validates the endpoint result types, and rejects
ConcreteSpec/Spec/Frame.lean:848: | none => ∀ h, ¬ (⟨submit, complete, h⟩ : KeptPair F H).WellFormed sp sr cp cr
ConcreteSpec/Spec/Frame.lean:850: pair.submit = submit ∧ pair.complete = complete ∧ pair.WellFormed sp sr cp cr ∧
ConcreteSpec/Spec/Frame.lean:905: loaded tables. This is an interface to artifact construction, not a native compiler implementation;
ConcreteSpec/Spec/Frame.lean:972: number of loaded modules and exit encodes the result with the root's observation prefix.
ConcreteSpec/Spec/Frame.lean:987: let loaded := retainChain root
ConcreteSpec/Spec/Frame.lean:988: .entered ((root, loaded), loaded.length, result, encodeToyExit root result)
ConcreteSpec/Spec/Frame.lean:1027: example : (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1033: example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1035: ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1044: example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1051: example : ¬ (⟨0, 0, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Grant.lean:45: def WellFormed (s : State) : Prop :=
ConcreteSpec/Spec/Grant.lean:51: instance (s : State) : Decidable (WellFormed s) := by unfold WellFormed; infer_instance
ConcreteSpec/Spec/Grant.lean:170: example : WellFormed { initial with dependencies := [heap, phantom] } := by decide
ConcreteSpec/Spec/Grant.lean:171: example : ¬ WellFormed { initial with dependencies := [heap, heap] } := by decide
ConcreteSpec/Spec/Hybrid.lean:122: structure State.WellFormed (tgt : Target) (st : State) : Prop where
ConcreteSpec/Spec/Hybrid.lean:123: memory : st.memory.WellFormed tgt
ConcreteSpec/Spec/Ident.lean:108: /-- Where a loaded package came from. Origin is deliberately outside `PackageId`: equal name and
ConcreteSpec/Spec/Ident.lean:125: /-- One loaded module and the origin of its package. -/
ConcreteSpec/Spec/Ident.lean:131: /-- A load-time refusal, retaining both complete loaded-module records for the diagnostic. OD44
ConcreteSpec/Spec/Ident.lean:142: /-- The first loaded module named by a collision diagnostic. -/
ConcreteSpec/Spec/Ident.lean:146: /-- The second loaded module named by a collision diagnostic. -/
ConcreteSpec/Spec/Ident.lean:150: /-- Both sides of a collision occur in the loaded input. -/
ConcreteSpec/Spec/Ident.lean:154: /-- A loaded set is well formed exactly when it contains no collision. -/
ConcreteSpec/Spec/Ident.lean:155: def LoadedModules.WellFormed (modules : List LoadedModule) : Prop :=
ConcreteSpec/Spec/Ident.lean:174: /-- Search every candidate against the complete loaded set. Ordered pairs make completeness
ConcreteSpec/Spec/Ident.lean:182: /-- Find the first collision in a loaded set. -/
ConcreteSpec/Spec/Ident.lean:186: /-- Either admit exactly the loaded input or refuse it with a two-sided collision diagnostic. -/
ConcreteSpec/Spec/Memory.lean:120: structure Memory.WellFormed (tgt : Target) (m : Memory) : Prop where
ConcreteSpec/Spec/Memory.lean:315: example : tinyTarget.WellFormed := by decide
ConcreteSpec/Spec/Observe.lean:167: def SiteTable.Valid (sources : SourceTable) (sites : SiteTable) : Prop :=
ConcreteSpec/Spec/Observe.lean:251: def ProjectSources.Realizes (input : ProjectSources) (project : LoadedProject) : Prop :=
ConcreteSpec/Spec/Observe.lean:461: loaded : LoadedProject.load sourceInput = .admitted sourceProject
ConcreteSpec/Spec/Observe.lean:576: structure ExecAdapter.WellFormed (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
ConcreteSpec/Spec/Observe.lean:695: loaded := rfl
ConcreteSpec/Spec/Observe.lean:779: example : SiteTable.Valid locationSources [(site, tabSpan)] := by
ConcreteSpec/Spec/Observe.lean:786: example : ¬ SiteTable.Valid locationSources [(site, badLocationSpan)] := by
ConcreteSpec/Spec/Observe.lean:837: loaded := rfl
ConcreteSpec/Spec/Own.lean:107: def WellFormed (s : State) : Prop :=
ConcreteSpec/Spec/Own.lean:122: instance (s : State) : Decidable (WellFormed s) := by
ConcreteSpec/Spec/Own.lean:123: unfold WellFormed
ConcreteSpec/Spec/Own.lean:273: `WellFormed`, every slot belonging to the departing activation is indexed in `f.locals`, so
ConcreteSpec/Spec/Own.lean:325: def CallState.WellFormed (s : CallState) : Prop :=
ConcreteSpec/Spec/Own.lean:326: Own.WellFormed s.own ∧
ConcreteSpec/Spec/Own.lean:330: instance (s : CallState) : Decidable (CallState.WellFormed s) := by
ConcreteSpec/Spec/Own.lean:331: unfold CallState.WellFormed
ConcreteSpec/Spec/Own.lean:699: ∀ tr c, Exec.Steps P c₀ tr c → WellFormed c.store.own ∧ StepPermitted O types permit P c
ConcreteSpec/Spec/Own.lean:963: /-- Refused: an incomplete locals table that omits an active frame's slot violates `WellFormed`. -/
ConcreteSpec/Spec/Own.lean:964: example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide
ConcreteSpec/Spec/Target.lean:24: unchanged. Another target is another profile (OD126), and must satisfy `Target.WellFormed`.
ConcreteSpec/Spec/Target.lean:81: def Target.WellFormed (t : Target) : Prop :=
ConcreteSpec/Spec/Target.lean:87: instance (t : Target) : Decidable t.WellFormed := by unfold Target.WellFormed; infer_instance
ConcreteSpec/Spec/Target.lean:106: example : linuxX8664.WellFormed := by decide
ConcreteSpec/Spec/Target.lean:114: maxObject := 2 ^ 16 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:117: example : ¬ ({ linuxX8664 with ptrAlign := 6 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:120: example : ¬ ({ linuxX8664 with ptrAlign := 16 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:123: example : ¬ ({ linuxX8664 with maxObject := 4 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:126: example : ¬ ({ linuxX8664 with maxObject := 2 ^ 64 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:135: maxObject := 2 ^ 15 } : Target).WellFormed := by decide
changelog.d/2026-11-08-c4-memory.md:1: C4: prove byte encodings and memory laws. Require canonical block entries in Memory.WellFormed,
docs/LANDING.md:478: - **What dominates.** The remote full set is about 98% of the cost: 87 minutes on a loaded host.
docs/archive/bugs-2026-11/bug_disposition.tsv:142: 146	`*h` on an owned `Heap<T>` loaded the value but never freed the cell, although the checker consumes `h` and MEMORY_SEMANTICS says it frees (a leak; found by Codex reviewing bug 144's migration). Elab now lowers it to the `free` intrinsic. `regress_146_heap_deref_frees.con` plus an emitted-SSA leg. Joins exit 0b. ((`safety-eval2`))	fixed	rule	MEM-5@1	accept: *h on owned Heap<T> moves out and frees the cell (no leak)	tests/regressions/bug146_heap_deref_does_not_free tests/programs/regress_146_heap_deref_frees.con tests/programs/regress_146_heap_deref_cast.con tests/programs/error_146_deref_heap_place_write.con tests/programs/error_146_deref_heap_field_read.con	yes	front-end judgment or semantic choice the cited rule decides	carry: keep ruling + negative/positive regression, add conformance example	8	-	none yet (no new-compiler slice landed)	-
docs/archive/bugs-2026-11/bug_disposition.tsv:224: 250	natively, writing an element or field of an aggregate local carried through a loop rebuilt the whole aggregate per write (and reading a field loaded the whole struct); the first-class loads and stores of these aggregates made clang -O2 superlinear in their size (a 20000-element array written in a loop: 99 s to compile). Lower writes and reads scalar places of promoted locals at their address; EmitSSA holds a loaded aggregate by pointer (a fresh slot and a `memcpy`). Pinned by `regress_250_native_array_writes.con`. Found writing bug 248's regression. Class 17.	fixed	legacy-impl	outside (performance)	-	tests/programs/regress_250_native_array_writes.con	yes	old pass data structure superlinear; program useful as scaling input	retired with implementation (B3); not fixed	17	-	none yet (no new-compiler slice landed)	-
docs/archive/bugs-2026-11/bug_disposition.tsv:259: 285	the walk that tells Lower's branch and loop sites which locals have their address taken (`cexprAddrTaken`) had no case for an enum literal and fell to a wildcard, so `&x` inside an enum payload was invisible: a pre-branch local was promoted lazily inside the branch and the other path loaded a never-stored slot, and a loop variable was phi-carried and promoted mid-body (the loop never ended)	fixed	legacy-impl	MEM-3@1	accept, interp==native (as fixture)	tests/programs/regress_285_enum_payload_addr_taken.con	yes	old lowering/codegen miscompile or invalid IR; rule unambiguous	retired with implementation (B3); not fixed	8	-	none yet (no new-compiler slice landed)	-
docs/archive/bugs-2026-11/bug_disposition.tsv:276: 302	a loop promotes to a slot every scalar local its body or condition takes the address of, and at its exit Lower loaded the slot into a register and dropped the promotion, so later reads and writes of the local used the register while a raw pointer taken in the loop (`&mut x as *mut i64`) still pointed at the slot: a write on either side was not seen on the other (silent wrong code)	fixed	legacy-impl	PASS-13@1	accept, interp==native (as fixture)	tests/programs/regress_302_loop_keeps_addr_taken_slot.con	yes	old lowering/codegen miscompile or invalid IR; rule unambiguous	retired with implementation (B3); not fixed	8	-	none yet (no new-compiler slice landed)	-
docs/archive/bugs-2026-11/bug_disposition.tsv:279: 305	Lower pointed a borrow block's reference at a copy of the owner in a fresh slot and loaded the copy back into the owner only when the block fell through its end; a `break`, `continue` (labelled or not) or `return` leaving a `borrow mut` block early skipped that write-back, so the writes made through the reference were lost on that exit: the loop exit or continue edge carried the owner's old value, and an enclosing deferred call reading the owner read it too (silent wrong code)	fixed	legacy-impl	REF-13@1, STMT-11@1	accept, interp==native (as fixture)	tests/programs/regress_305_borrow_block_early_exit.con tests/programs/regress_305_borrow_block_owners_and_exits.con	yes	old lowering/codegen miscompile or invalid IR; rule unambiguous	retired with implementation (B3); not fixed	8	-	none yet (no new-compiler slice landed)	-
docs/decisions.md:1257: | OD142 | <a id="od142"></a>`X.con` beside `X/mod.con` (added 2026-10-30, raised by the spec quality audit of 2026-10-30). Question: Which file `mod X;` loads when both `X.con` and `X/mod.con` exist. Bears on MOD-4. The current compiler: `X.con` is loaded and `X/mod.con` is ignored without a diagnostic (with `X.con` returning 7 and `X/mod.con` 9, the program exits 7 on both consumers). MOD-4 says one module is loaded from either file. Options: (a) refuse the pair; (b) `X.con` takes precedence, as the current compiler does; (c) `X/mod.con` takes precedence. Austral: not checked. Pick: none given (raised by the spec quality audit, 2026-10-30). | see OPEN-142 | see OPEN-142 | decided 2026-11-01 by the owner (final set): One module layout: `X.con`, children in directory `X/`; no `mod.con` |
scripts/checks/lib/treestate.sh:80: # loaded: a file truncated immediately after this function would pass while ts_tracked,
scripts/checks/lib/treestate.sh:89: echo "FATAL: treestate.sh loaded but '$fn' is missing — the library is truncated." >&2
scripts/proofs/axioms.golden:1509: ConcreteSpec.Core.Hybrid.State.WellFormed.memory	propext
scripts/proofs/axioms.golden:1510: ConcreteSpec.Core.Hybrid.State.WellFormed.projected	propext
scripts/proofs/axioms.golden:1637: ConcreteSpec.Core.Mem.Memory.WellFormed.aligned	propext
scripts/proofs/axioms.golden:1638: ConcreteSpec.Core.Mem.Memory.WellFormed.canonical	propext
scripts/proofs/axioms.golden:1639: ConcreteSpec.Core.Mem.Memory.WellFormed.disjoint	propext
scripts/proofs/axioms.golden:1640: ConcreteSpec.Core.Mem.Memory.WellFormed.fits	propext
scripts/proofs/axioms.golden:1641: ConcreteSpec.Core.Mem.Memory.WellFormed.fresh	propext
scripts/proofs/axioms.golden:1642: ConcreteSpec.Core.Mem.Memory.WellFormed.sized	propext
scripts/proofs/axioms.golden:1643: ConcreteSpec.Core.Mem.Memory.WellFormed.spaces	propext
scripts/proofs/axioms.golden:1644: ConcreteSpec.Core.Mem.Memory.WellFormed.unique	propext
scripts/proofs/axioms.golden:1826: ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.answered
scripts/proofs/axioms.golden:1827: ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.blocked
scripts/proofs/axioms.golden:1828: ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.internal
```

Enclosing declarations, including every directly affected existing theorem and example (names are original source headers; continuation lines remain in the source):

```text
ConcreteSpec/Proofs/Adequacy.lean:1360: theorem InfiniteExecution.projectedTrace_valid {S : Observe.EventSignature}
ConcreteSpec/Proofs/Adequacy.lean:1396: theorem InfiniteExecution.projected_completeTrace {S : Observe.EventSignature}
ConcreteSpec/Proofs/Adequacy.lean:1408: theorem finite_projected_observation {S : Observe.EventSignature}
ConcreteSpec/Proofs/Adequacy.lean:1469: theorem infinite_projected_observation {S : Observe.EventSignature}
ConcreteSpec/Proofs/Adequacy.lean:1871: def erasingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
ConcreteSpec/Proofs/Adequacy.lean:1881: def keepingAdapter : Observe.ExecAdapter ops Observe.Demo.signature where
ConcreteSpec/Proofs/Adequacy.lean:1908: theorem erasingWellFormed : Observe.ExecAdapter.WellFormed erasingAdapter Observe.Demo.contract () observationEnv := by
ConcreteSpec/Proofs/Adequacy.lean:1919: theorem keepingWellFormed : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv := by
ConcreteSpec/Proofs/Adequacy.lean:2575: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Adequacy.lean:2617: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Adequacy.lean:2656: example : Observe.ExecAdapter.WellFormed keepingAdapter Observe.Demo.contract () observationEnv ∧
ConcreteSpec/Proofs/Authority.lean:62: theorem mem_of_no_vars {e : CapExpr} (he : e.WellFormed 0) {ρ : Nat → List Cap} {c : Cap}
ConcreteSpec/Proofs/Frame.lean:98: theorem store_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt) (m' : Memory)
ConcreteSpec/Proofs/Frame.lean:148: private theorem metadataStoreWellFormed : metadataStoreBefore.memory.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Frame.lean:243: theorem alloc_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:301: theorem write_materialized_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:324: theorem rawWrite_frame (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Frame.lean:353: theorem alloc_fresh_disjoint (tgt : Target) (st : State)
ConcreteSpec/Proofs/Frame.lean:378: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:602: theorem runEffect_frame (tgt : Target) (before : State) (command : EffectCommand)
ConcreteSpec/Proofs/Frame.lean:693: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:712: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:728: example : metadataStoreBefore.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Frame.lean:766: theorem c4Execution_frame (tgt : Target) (before : State) (f : Footprint) (after : State)
ConcreteSpec/Proofs/Frame.lean:788: theorem c4Execution_wellFormed (tgt : Target) (before : State) (f : Footprint) (after : State)
ConcreteSpec/Proofs/Frame.lean:899: theorem c1_steps_frame {O : Exec.Operands} {P : Exec.Program O} {project : O.Store → State}
ConcreteSpec/Proofs/Frame.lean:935: theorem c1_call_frame_of_implementation {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
ConcreteSpec/Proofs/Frame.lean:957: theorem c1_call_frame (tgt : Target) (O : Exec.Operands) (f : FnId) (P : Exec.Program O)
ConcreteSpec/Proofs/Frame.lean:1073: example :
ConcreteSpec/Proofs/Frame.lean:1186: theorem toySignature_wellFormed : toySignature.WellFormed := by
ConcreteSpec/Proofs/Frame.lean:1368: theorem toyCall_conforms (base : Machine Nat Nat) (arg result : Nat) :
ConcreteSpec/Proofs/Frame.lean:1376: example : toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1501: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1511: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1521: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1531: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1554: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1564: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1574: example : ¬ (toySignature.WellFormed ∧
ConcreteSpec/Proofs/Frame.lean:1748: theorem keptPair_results_valid {F H : Type} (pair : KeptPair F H)
ConcreteSpec/Proofs/Frame.lean:1846: theorem checkKeptPair_result {F H : Type} [DecidableEq F] [DecidableEq H]
ConcreteSpec/Proofs/Frame.lean:1867: theorem checkKeptPair_complete {F H : Type} [DecidableEq F] [DecidableEq H]
ConcreteSpec/Proofs/Frame.lean:2174: def overRetainingToyLaunch (root reserved result : Nat) :
ConcreteSpec/Proofs/Frame.lean:2219: def unboundedToyLaunch (root _reserved result : Nat) :
ConcreteSpec/Proofs/Frame.lean:2236: theorem inadmissible_no_artifact {Artifact : Type} (build : Observe.LoadedProject → Artifact)
ConcreteSpec/Proofs/Frame.lean:2247: theorem inadmissible_no_run {Root Artifact Input Result Observation : Type}
ConcreteSpec/Proofs/Frame.lean:2260: theorem inadmissible_no_startup {Root Artifact Input Result Observation : Type}
ConcreteSpec/Proofs/Frame.lean:2279: theorem inadmissible_no_trap {Artifact : Type} (build : Observe.LoadedProject → Artifact)
ConcreteSpec/Proofs/Frame.lean:2288: theorem assembled_admitted {Artifact : Type} (build : Observe.LoadedProject → Artifact)
ConcreteSpec/Proofs/Frame.lean:2301: theorem startup125_admitted {Root Artifact Input Result Observation : Type}
ConcreteSpec/Proofs/Frame.lean:2327: theorem refused_invalid (source : ProjectSources) (issue : SourceLoadRefusal)
ConcreteSpec/Proofs/Frame.lean:2340: theorem empty_invalid : ¬ ∃ project, emptyInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2345: theorem backslash_invalid : ¬ ∃ project, backslashInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2350: theorem control_invalid : ¬ ∃ project, controlInput.Realizes project :=
ConcreteSpec/Proofs/Frame.lean:2362: example : (¬ ∃ project, emptyInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2377: example : (¬ ∃ project, parentPath.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2392: example : (¬ ∃ project, duplicateFiles.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2407: example : (¬ ∃ project, duplicate.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2422: example : (¬ ∃ project, missing.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2437: example : (¬ ∃ project, controlInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2452: example : (¬ ∃ project, backslashInput.Realizes project) ∧
ConcreteSpec/Proofs/Frame.lean:2467: example : (¬ ∃ project, badLocation.Realizes project) ∧
ConcreteSpec/Proofs/Grant.lean:21: instance {α : Type} (P : α → Prop) (x : Option α) [∀ a, Decidable (P a)] :
ConcreteSpec/Proofs/Grant.lean:29: theorem fresh_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:59: theorem fresh_progress (s : State) (scope : BlockId) :
ConcreteSpec/Proofs/Grant.lean:63: theorem depend_preserves (s : State) (d : Dependency) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:87: theorem consume_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:102: theorem release_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:140: theorem reset_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:184: theorem endScope_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:292: theorem nextBrand_fresh (s : State) (hs : WellFormed s) (b : Binder) (hb : b ∈ s.binders) :
ConcreteSpec/Proofs/Grant.lean:348: theorem endScope_non_escape (s : State) (scope : BlockId) :
ConcreteSpec/Proofs/Grant.lean:360: theorem allocations_have_live_allocator_of_wf (s : State) (hs : WellFormed s) (d : Dependency)
ConcreteSpec/Proofs/Grant.lean:367: theorem allocations_have_live_allocator (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:499: example : ResultSatisfies WellFormed (fresh emptyState ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:503: example : WellFormed emptyState ∧
ConcreteSpec/Proofs/Grant.lean:507: example : ResultSatisfies WellFormed (depend phantomOnly heap) :=
ConcreteSpec/Proofs/Grant.lean:511: example : WellFormed phantomOnly ∧
ConcreteSpec/Proofs/Grant.lean:515: example : ResultSatisfies WellFormed (consume dependent ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:519: example : WellFormed dependent ∧
ConcreteSpec/Proofs/Grant.lean:523: example : ResultSatisfies WellFormed (release releaseInput ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:527: example : WellFormed releaseInput ∧
ConcreteSpec/Proofs/Grant.lean:531: example : ResultSatisfies WellFormed (reset initial ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:535: example : WellFormed initial ∧
ConcreteSpec/Proofs/Grant.lean:539: example : ResultSatisfies WellFormed (endScope departing ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:543: example : WellFormed departing ∧
ConcreteSpec/Proofs/Grant.lean:650: example : WellFormed releaseInput ∧
ConcreteSpec/Proofs/Grant.lean:803: theorem fresh_nominal (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:860: example : WellFormed releasedWithPhantom ∧
ConcreteSpec/Proofs/Grant.lean:879: private theorem binder_unique (xs : List Binder) (hn : (xs.map Binder.brand).Nodup)
ConcreteSpec/Proofs/Grant.lean:896: theorem lifetime_wf (s : State) (ops : List LifetimeOp) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:931: theorem scope_exit_lifetime (s : State) (scope : BlockId) (ops : List LifetimeOp)
ConcreteSpec/Proofs/Grant.lean:979: example : ResultSatisfies WellFormed (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:983: example : WellFormed departed ∧
ConcreteSpec/Proofs/Grant.lean:993: example : WellFormed departing ∧
ConcreteSpec/Proofs/Hybrid.lean:66: theorem write_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:103: theorem typed_store_frame (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:199: theorem memory_wellFormed (tgt : Target) (st : State) (m : Memory)
ConcreteSpec/Proofs/Hybrid.lean:208: theorem set_wellFormed (tgt : Target) (st : State) (p : Place) (c : Cell)
ConcreteSpec/Proofs/Hybrid.lean:219: theorem write_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:253: theorem rawWrite_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:312: theorem alloc_projected (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:328: theorem materialize_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:359: theorem expose_wellFormed (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:423: theorem rangesDisjoint_separated (tgt : Target) (st : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:452: theorem write_frame_disjoint (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:467: theorem materialize_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:500: theorem expose_frame (tgt : Target) (st st' : State) (hw : st.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:527: theorem materialize_read (tgt : Target) (st st' : State) (hw : st.memory.WellFormed tgt)
ConcreteSpec/Proofs/Hybrid.lean:608: private theorem not_badState_wf : ¬ badState.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:614: private theorem before_wf : before.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:626: private theorem structState_wf : structState.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:690: example : before.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:705: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:751: example : match before.write linuxX8664 ⟨0⟩ (.bool true) with
ConcreteSpec/Proofs/Hybrid.lean:757: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:765: example : match before.rawWrite linuxX8664 range.ptr [.init 1 none] 1 with
ConcreteSpec/Proofs/Hybrid.lean:771: example : before.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:779: example : match structState.materialize linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
ConcreteSpec/Proofs/Hybrid.lean:785: example : structState.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:793: example : match structState.expose linuxX8664 ⟨0⟩ ⟨0⟩ 40 1 [] with
ConcreteSpec/Proofs/Hybrid.lean:799: example : structState.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Hybrid.lean:822: private theorem observedStart_wf : observedStart.WellFormed linuxX8664 := by
ConcreteSpec/Proofs/Hybrid.lean:830: private theorem observed_wf : observed.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:833: private theorem structState_wf : structState.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:836: private theorem before_wf : before.WellFormed linuxX8664 :=
ConcreteSpec/Proofs/Hybrid.lean:890: example : before.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:903: example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:916: example : structState.WellFormed linuxX8664 ∧ (⟨1⟩ : Place) ≠ ⟨0⟩ ∧
ConcreteSpec/Proofs/Hybrid.lean:931: example : structState.memory.WellFormed linuxX8664 ∧
ConcreteSpec/Proofs/Ident.lean:1192: theorem LoadedModules.load_refusal_complete (modules : List LoadedModule) (collision : LoadCollision)
ConcreteSpec/Proofs/Ident.lean:1220: theorem LoadedModules.load_refusal_sound (modules : List LoadedModule) :
ConcreteSpec/Proofs/Ident.lean:1236: theorem LoadedModules.load_admitted (modules admitted : List LoadedModule) :
ConcreteSpec/Proofs/Ident.lean:2181: def collidingOrigin : LoadCollision := .packageOrigin firstModule samePackageOtherOrigin (by decide) (by decide)
ConcreteSpec/Proofs/Ident.lean:2188: example : match LoadedModules.load [firstModule, collidingModule] with
ConcreteSpec/Proofs/Ident.lean:2195: example : ¬ (match fabricatedRefusalLoad [firstModule, collidingModule] with
ConcreteSpec/Proofs/Ident.lean:2207: example : ¬ (match admitAll [firstModule, collidingModule] with
ConcreteSpec/Proofs/Ident.lean:2227: example : LoadedModules.load [firstModule, secondModule] =
ConcreteSpec/Proofs/Ident.lean:2237: example : ¬ (discardLoad [firstModule, secondModule] =
ConcreteSpec/Proofs/Ident.lean:2247: example : LoadedModules.load [firstModule, collidingModule] =
ConcreteSpec/Proofs/Ident.lean:2254: example : ¬ (admitAll [firstModule, collidingModule] =
ConcreteSpec/Proofs/Memory.lean:141: theorem decodePtr_encodePtr (tgt : Target) (ht : tgt.WellFormed) (p : Ptr) (hpv : p.Valid tgt) :
ConcreteSpec/Proofs/Memory.lean:171: theorem empty_wellFormed (tgt : Target) : Memory.empty.WellFormed tgt := by
ConcreteSpec/Proofs/Memory.lean:201: theorem modify_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:262: theorem alloc_fresh (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:364: theorem alloc_ne_existing (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:384: theorem alloc_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:457: theorem candidate_contains_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:471: theorem fromExposed_sound_inv (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:502: theorem fromExposed_sound (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:536: theorem fromExposed_contains (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:549: theorem candidate_onePast_unique (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:562: theorem fromExposed_onePast (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:576: theorem fromExposed_none (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:614: theorem access_valid (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:668: theorem decodePtr_some (tgt : Target) (ht : tgt.WellFormed) (bs : List MByte) (p : Ptr) :
ConcreteSpec/Proofs/Memory.lean:715: theorem store_load (tgt : Target) (m m' : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:754: theorem store_same_block_frame (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:784: theorem store_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:814: theorem free_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:865: theorem expose_wellFormed (tgt : Target) (m : Memory) (hw : m.WellFormed tgt)
ConcreteSpec/Proofs/Memory.lean:1035: theorem free_alloc_ne_existing (tgt : Target) (m freed : Memory)
ConcreteSpec/Proofs/Memory.lean:1170: private theorem not_badEmpty_wf : ¬ badEmpty.WellFormed target := by
ConcreteSpec/Proofs/Memory.lean:1176: private theorem memory_wf : memory.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1180: private theorem exposedMemory_wf : exposedMemory.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1240: example : target.WellFormed ∧ ptr.Valid target ∧
ConcreteSpec/Proofs/Memory.lean:1244: example : target.WellFormed ∧ ptr.Valid target ∧
ConcreteSpec/Proofs/Memory.lean:1253: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1258: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1268: example : target.WellFormed ∧
ConcreteSpec/Proofs/Memory.lean:1287: example : Memory.empty.WellFormed target :=
ConcreteSpec/Proofs/Memory.lean:1291: example : ¬ badEmpty.WellFormed target := not_badEmpty_wf
ConcreteSpec/Proofs/Memory.lean:1301: example : Memory.empty.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1463: example : memory.WellFormed target ∧ memory.block ⟨0⟩ = some initBlock ∧
ConcreteSpec/Proofs/Memory.lean:1471: example : match Memory.empty.alloc target ⟨0⟩ 40 8 1 with
ConcreteSpec/Proofs/Memory.lean:1477: example : Memory.empty.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1493: example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
ConcreteSpec/Proofs/Memory.lean:1506: example : exposedMemory.WellFormed target ∧ exposedMemory.block ⟨0⟩ = some exposedBlock ∧
ConcreteSpec/Proofs/Memory.lean:1519: example : exposedMemory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1541: example : memory.WellFormed target ∧ (2 : Nat) ≠ 0 ∧
ConcreteSpec/Proofs/Memory.lean:1569: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1596: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1621: example : match memory.store target ptr [.init 7 none] 1 with
ConcreteSpec/Proofs/Memory.lean:1627: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1635: example : match memory.free ⟨0⟩ with
ConcreteSpec/Proofs/Memory.lean:1641: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1660: example : match memory.expose ptr with
ConcreteSpec/Proofs/Memory.lean:1666: example : memory.WellFormed target ∧
ConcreteSpec/Proofs/Memory.lean:1714: example : memory.WellFormed target ∧ memory.free ⟨0⟩ = some freedMemory ∧
ConcreteSpec/Proofs/Memory.lean:1796: example : exposedMemory.WellFormed target ∧
ConcreteSpec/Proofs/Observe2.lean:95: theorem LoadedProject.load_admitted (input : ProjectSources) (project : LoadedProject) :
ConcreteSpec/Proofs/Observe2.lean:145: theorem LoadedProject.load_complete (input : ProjectSources) :
ConcreteSpec/Proofs/Observe2.lean:160: theorem LoadedProject.load_locations (input : ProjectSources) :
ConcreteSpec/Proofs/Observe2.lean:276: theorem LoadedProject.load_refuses (input : ProjectSources)
ConcreteSpec/Proofs/Observe2.lean:288: theorem LoadedProject.load_refuses_malformed (input : ProjectSources) (file : String × SourceText)
ConcreteSpec/Proofs/Observe2.lean:307: theorem LoadedProject.load_unique (input : ProjectSources) :
ConcreteSpec/Proofs/Observe2.lean:382: theorem ProjectSources.realizes_permutation (input other : ProjectSources)
ConcreteSpec/Proofs/Observe2.lean:418: theorem LoadedProject.load_trap_resolves (input : ProjectSources) (kind : TrapKind) :
ConcreteSpec/Proofs/Observe2.lean:504: theorem parent_invalid : ¬ ∃ project, parentPath.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:514: theorem duplicate_invalid : ¬ ∃ project, duplicate.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:522: theorem missing_invalid : ¬ ∃ project, missing.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:533: theorem duplicateFiles_invalid : ¬ ∃ project, duplicateFiles.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:545: theorem badLocation_invalid : ¬ ∃ project, badLocation.Realizes project := by
ConcreteSpec/Proofs/Observe2.lean:558: example : LoadedProject.load good = .admitted admitted ↔ good.Realizes admitted :=
ConcreteSpec/Proofs/Observe2.lean:562: example : ¬ (loseSites good = .admitted admitted ↔ good.Realizes admitted) := by
ConcreteSpec/Proofs/Observe2.lean:575: example : LoadedProject.load duplicate = .admitted admitted ↔ duplicate.Realizes admitted :=
ConcreteSpec/Proofs/Observe2.lean:579: example : ¬ (keepFirst duplicate = .admitted admitted ↔ duplicate.Realizes admitted) := by
ConcreteSpec/Proofs/Observe2.lean:584: example : LoadedProject.load parentPath = .admitted ⟨admitted.sources, parentPath.sites⟩ ↔
ConcreteSpec/Proofs/Observe2.lean:589: example : ¬ (acceptParent parentPath = .admitted ⟨admitted.sources, parentPath.sites⟩ ↔
ConcreteSpec/Proofs/Observe2.lean:627: example : match LoadedProject.load badLocation with
ConcreteSpec/Proofs/Observe2.lean:633: example : ¬ (match ignoreLocation badLocation with
ConcreteSpec/Proofs/Observe2.lean:662: example : (¬ ∃ project, parentPath.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:736: example : (¬ ∃ project, duplicate.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:763: example : (¬ ∃ project, duplicateFiles.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:778: example : ¬ (match fabricatedRefusal duplicateFiles with
ConcreteSpec/Proofs/Observe2.lean:790: example : (¬ ∃ project, missing.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:817: example : (¬ ∃ project, badLocation.Realizes project) ∧
ConcreteSpec/Proofs/Observe2.lean:1078: theorem ExecAdapter.sites_valid {O : Exec.Operands} {S : EventSignature} (A : ExecAdapter O S) :
ConcreteSpec/Proofs/Observe2.lean:1511: theorem trapTerminal_valid_location_inv (sources : SourceTable) (sites : SiteTable)
ConcreteSpec/Proofs/Observe2.lean:1563: theorem trapTerminal_valid_location (sources : SourceTable) (sites : SiteTable)
ConcreteSpec/Proofs/Observe2.lean:1578: theorem validSites_valid : SiteTable.Valid validSources validSites := by
ConcreteSpec/Proofs/Observe2.lean:1593: example : SiteTable.Valid validSources validSites ∧
ConcreteSpec/Proofs/Observe2.lean:1609: example : SiteTable.Valid validSources validSites ∧
ConcreteSpec/Proofs/Observe2.lean:1667: theorem runFrom_validTrace (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S)
ConcreteSpec/Proofs/Observe2.lean:1732: theorem runFrom_noAnswer_pending_valid (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Proofs/Observe2.lean:1756: theorem runFrom_blockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
ConcreteSpec/Proofs/Observe2.lean:1841: def adapter : ExecAdapter ops signature where
ConcreteSpec/Proofs/Observe2.lean:1860: theorem answeringWellFormed (blocking : Unit → List (Event signature) → Request signature → Prop) :
ConcreteSpec/Proofs/Observe2.lean:1876: theorem blockingWellFormed : ExecAdapter.WellFormed adapter permittingContract () blockingOracle := by
ConcreteSpec/Proofs/ObserveEnvironment.lean:42: theorem environment_oracle_wellFormed {S : EventSignature} (C : OperationContract S)
ConcreteSpec/Proofs/ObserveOutput.lean:200: def adapter : ExecAdapter Exec.Demo.ops Demo.signature where
ConcreteSpec/Proofs/ObserveReport.lean:44: theorem runFrom_report_trace {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:76: theorem runFrom_blocked_report {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:174: theorem runFrom_outOfFuel_report_none {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:251: theorem runFrom_trapped_status_134 {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:293: theorem runFrom_source_trap {O : Exec.Operands} {S : EventSignature}
ConcreteSpec/Proofs/ObserveReport.lean:379: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:408: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:426: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:473: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:501: def relativePathAdapter : ExecAdapter ops signature :=
ConcreteSpec/Proofs/ObserveReport.lean:509: theorem relativePathWellFormed :
ConcreteSpec/Proofs/ObserveReport.lean:519: example : ¬ ExecAdapter.WellFormed adapter forbiddingContract () blockingOracle := by
ConcreteSpec/Proofs/ObserveReport.lean:537: example : ExecAdapter.WellFormed adapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:564: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:584: example : ExecAdapter.WellFormed adapter permittingContract () answeringOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:670: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:700: example : ExecAdapter.WellFormed relativePathAdapter permittingContract () blockingOracle ∧
ConcreteSpec/Proofs/ObserveReport.lean:729: def sourceAdapter : ExecAdapter ops signature :=
ConcreteSpec/Proofs/ObserveReport.lean:788: example : ([Demo.site] = List.map Prod.fst sourceAdapter.sourceInput.sites) ∧
ConcreteSpec/Proofs/ObserveReport.lean:824: example : LoadedProject.load unloadedSourceInput =
ConcreteSpec/Proofs/ObserveReport.lean:839: def refusedSourceInput : ProjectSources := { sourceInput with files := [("../main.con", sourceBytes)] }
ConcreteSpec/Proofs/ObserveReport.lean:842: example : ¬ ∃ A : ExecAdapter ops signature, A.sourceInput = refusedSourceInput := by
ConcreteSpec/Proofs/Own.lean:20: def StateResult : Option State → Prop
ConcreteSpec/Proofs/Own.lean:24: def CallOwnResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:28: def CallResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:48: private theorem fold_preserves {α : Type} (f : State → α → Option State)
ConcreteSpec/Proofs/Own.lean:62: theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
ConcreteSpec/Proofs/Own.lean:111: theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
ConcreteSpec/Proofs/Own.lean:184: theorem borrow_preserves (s : State) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:240: theorem reborrowTo_preserves (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:318: theorem reborrow_preserves (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:330: theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
ConcreteSpec/Proofs/Own.lean:361: theorem accessOwner_preserves_result (s t : State) (p : Hybrid.Place) (m : Mode)
ConcreteSpec/Proofs/Own.lean:369: theorem accessLoan_preserves_result (s t : State) (i : LoanId) (m : Mode)
ConcreteSpec/Proofs/Own.lean:425: example : WellFormed initial ∧ (move initial ⟨0⟩).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:427: example : WellFormed shared ∧ (borrow shared ⟨0⟩ [] .shared).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:429: example : WellFormed exclusive ∧ (reborrowTo exclusive ⟨0⟩ [] .mutable 0).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:431: example : accessOwner shared ⟨0⟩ .shared = some shared ∧ WellFormed shared := by decide
ConcreteSpec/Proofs/Own.lean:432: example : accessLoan child ⟨1⟩ .mutable = some child ∧ WellFormed child := by decide
ConcreteSpec/Proofs/Own.lean:434: example : ¬ WellFormed { shared with slots := shared.slots.map fun x =>
ConcreteSpec/Proofs/Own.lean:436: example : ¬ WellFormed { child with slots := [] } := by decide
ConcreteSpec/Proofs/Own.lean:438: example : ¬ WellFormed { shared with nextLoan := 0 } := by decide
ConcreteSpec/Proofs/Own.lean:439: example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
ConcreteSpec/Proofs/Own.lean:451: theorem bind_preserves (types : Var → Option Descriptor) (s : State) (x : Var)
ConcreteSpec/Proofs/Own.lean:540: theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
ConcreteSpec/Proofs/Own.lean:613: theorem enter_preserves (types : Var → Option Descriptor) (s : State) (xs : List Var)
ConcreteSpec/Proofs/Own.lean:654: theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
ConcreteSpec/Proofs/Own.lean:700: theorem leave_no_activation_slot (s : State) (f : Activation)
ConcreteSpec/Proofs/Own.lean:738: theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
ConcreteSpec/Proofs/Own.lean:795: private theorem set_views_preserves (s : State) (f : Activation) (views : AList Var LoanId)
ConcreteSpec/Proofs/Own.lean:822: theorem enterDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:852: theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : State)
ConcreteSpec/Proofs/Own.lean:967: example : WellFormed moved ∧ reinitialize moved ⟨0⟩ = some initial := by decide
ConcreteSpec/Proofs/Own.lean:968: example : WellFormed child ∧ (endLoan child ⟨1⟩).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:970: example : WellFormed shared ∧ (bind types y shared).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:972: example : WellFormed moved ∧ (drop [x] moved).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:974: example : WellFormed entered ∧ (enter types [y] shared).isSome = true := by decide
ConcreteSpec/Proofs/Own.lean:975: example : WellFormed called ∧ (endLocals 1 called).isSome = true ∧
ConcreteSpec/Proofs/Own.lean:977: example : (leave called).isSome = true ∧ WellFormed ((leave called).getD initial) := by decide
ConcreteSpec/Proofs/Own.lean:980: example : ¬ WellFormed { shared with slots := [] } := by decide
ConcreteSpec/Proofs/Own.lean:982: example : ¬ WellFormed { initial with slots := initial.slots ++ initial.slots } := by decide
ConcreteSpec/Proofs/Own.lean:984: example : ¬ WellFormed { shared with loans := shared.loans.map fun l =>
ConcreteSpec/Proofs/Own.lean:987: example : ¬ WellFormed { child with loans := child.loans.filter (·.id != ⟨0⟩) } := by decide
ConcreteSpec/Proofs/Own.lean:996: theorem loan_has_owned_root_of_wf (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
ConcreteSpec/Proofs/Own.lean:1001: theorem moved_no_loan_of_wf (s : State) (hs : WellFormed s) (x : Slot) (hx : x ∈ s.slots)
ConcreteSpec/Proofs/Own.lean:1033: private theorem map_metadata_preserves (s : State) (hs : WellFormed s)
ConcreteSpec/Proofs/Own.lean:1060: theorem adoptDown_preserves (s : State) (args : List (Var × LoanId))
ConcreteSpec/Proofs/Own.lean:1088: theorem passDown_preserves (s : CallState) (i : LoanId) (parameter : Var)
ConcreteSpec/Proofs/Own.lean:1136: theorem passDown_call_preserves (s : CallState) (i : LoanId) (parameter : Var)
ConcreteSpec/Proofs/Own.lean:1204: theorem callLife_preserves (types : Var → Option Descriptor) (e : Exec.Life)
ConcreteSpec/Proofs/Own.lean:1341: theorem callLife_call_preserves (types : Var → Option Descriptor) (e : Exec.Life)
ConcreteSpec/Proofs/Own.lean:1393: example : CallState.WellFormed ⟨exclusive, []⟩ ∧ CallState.WellFormed waiting := by decide
ConcreteSpec/Proofs/Own.lean:1397: example : CallState.WellFormed ((callLife types (.enter [y]) waiting).getD waiting) := by decide
ConcreteSpec/Proofs/Own.lean:1399: example : ¬ CallState.WellFormed { waiting with pending := waiting.pending ++ waiting.pending } := by decide
ConcreteSpec/Proofs/Own.lean:1400: example : ¬ CallState.WellFormed { waiting with own := exclusive } := by decide
ConcreteSpec/Proofs/Own.lean:1402: example : ¬ CallState.WellFormed { waiting with own := initial } := by decide
ConcreteSpec/Proofs/Own.lean:1415: theorem instrument_preserves_result {σ α : Type} (permit : State → Option State)
ConcreteSpec/Proofs/Own.lean:1482: theorem nextLoan_fresh (s : State) (hs : WellFormed s) (l : Loan) (hl : l ∈ s.loans) :
ConcreteSpec/Proofs/Own.lean:1527: theorem leave_no_local_loan (s : State) (f : Activation) (entry : Var × Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:1584: theorem passOwner_preserves (s : CallState) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:1643: theorem passOwner_call_preserves (s : CallState) (p : Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:1766: theorem leave_retains_caller (s : State) (f : Activation) (l : Loan)
ConcreteSpec/Proofs/Own.lean:1875: example : WellFormed localLoan ∧ leave localLoan = some exited ∧
ConcreteSpec/Proofs/Own.lean:1924: theorem directOwner_roundtrip (s : State) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:1999: example : CallState.WellFormed caller ∧ passOwner caller ⟨1⟩ [.index 3] .mutable z = some waiting ∧
ConcreteSpec/Proofs/Own.lean:2002: example : ¬ WellFormed { waiting.own with nextLoan := 1 } := by decide
ConcreteSpec/Proofs/Own.lean:2024: example : WellFormed called.own ∧
ConcreteSpec/Proofs/Own.lean:4408: theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
ConcreteSpec/Proofs/Own.lean:4414: theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
ConcreteSpec/Proofs/Own.lean:4462: def bindNoAdvancePlace (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
ConcreteSpec/Proofs/Own.lean:4491: def enterDownReuseParentId (s : State) (args : List (Var × LoanId)) : Option State := do
ConcreteSpec/Proofs/Own.lean:4510: def passDownClearParent (s : CallState) (i : LoanId) (x : Var) : Option CallState := do
ConcreteSpec/Proofs/Own.lean:4523: def callLifeUnissuedActivation (types : Var → Option Descriptor) (e : Exec.Life) (s : CallState) : Option CallState := do
ConcreteSpec/Proofs/Own.lean:4536: def passOwnerClearCallRoot (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode) (x : Var) : Option CallState := do
ConcreteSpec/Proofs/Own.lean:4570: example : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4577: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4584: example : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4591: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4598: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4605: example : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4612: example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4619: example : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4626: example : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4633: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide
ConcreteSpec/Proofs/Own.lean:4640: example : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4647: example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCallerInstead (1) (LifecycleWitnesses.called)) := by decide
ConcreteSpec/Proofs/Own.lean:4654: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveKeepLocalLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4661: example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide
ConcreteSpec/Proofs/Own.lean:4668: example : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4675: example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4682: example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4689: example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4696: example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4703: example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4710: example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4717: example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4750: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4759: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4777: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ (RoundThreeWitnesses.localLoan).loans) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)).activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) ∧ ¬ ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leaveAllLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4813: example : (LiveDestinations (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (leaveStranded (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4822: example : ¬ WellFormed unindexedCopy := by decide
ConcreteSpec/Proofs/Own.lean:4823: example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide
ConcreteSpec/Proofs/Own.lean:4826: example : ¬ WellFormed { initial with frames := [⟨1, [], []⟩], nextActivation := 2 } := by decide
ConcreteSpec/Proofs/Own.lean:4829: example : ¬ WellFormed { initial with
ConcreteSpec/Proofs/Own.lean:4835: example : let s : State := { LifecycleWitnesses.moved with
ConcreteSpec/Proofs/Own.lean:4881: example : WellFormed DirectArgumentWitnesses.caller.own ∧
ConcreteSpec/Proofs/Own.lean:4947: theorem loan_has_owned_root (s : State) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:4961: theorem moved_no_loan (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:5000: theorem instrument_preserves {σ α : Type} (permit : State → Option State)
ConcreteSpec/Proofs/Own.lean:5419: example : WellFormed initial ∧
ConcreteSpec/Proofs/Own.lean:5437: example : WellFormed moveWithOtherLoan ∧
ConcreteSpec/Proofs/Own.lean:5469: example : ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5476: example : (∀ s t, WellFormed s → sharedPermit s = some t → WellFormed t) ∧
ConcreteSpec/Proofs/Own.lean:5675: theorem downward_reborrow_protocol (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:5737: example : WellFormed LifecycleWitnesses.called ∧
ConcreteSpec/Proofs/Own.lean:5817: theorem deferred_call_lifetime (s : State) (root : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:5885: example : WellFormed heldSource ∧
ConcreteSpec/Proofs/Own.lean:5904: example : WellFormed heldSourceForLeak ∧
ConcreteSpec/Proofs/Own.lean:5925: theorem move_reinitialize_protocol (s : State) (p : Hybrid.Place) (x : Slot)
ConcreteSpec/Proofs/Own.lean:5986: example : WellFormed source ∧ slot source ⟨1⟩ = some selected ∧ selected.desc.copy = false ∧
ConcreteSpec/Spec/Authority.lean:60: def CapExpr.WellFormed (n : Nat) (e : CapExpr) : Prop :=
ConcreteSpec/Spec/Authority.lean:63: instance (n : Nat) (e : CapExpr) : Decidable (e.WellFormed n) := by
ConcreteSpec/Spec/Authority.lean:166: def ArgsOk (f callee : Fn) (caps : List CapExpr) (gs : List Nat) : Prop :=
ConcreteSpec/Spec/Authority.lean:198: def FnOk (P : Prog) (f : Fn) : Prop := f.perms.WellFormed f.capParams ∧ ∀ s ∈ f.body, StmtOk P f s
ConcreteSpec/Spec/Frame.lean:267: inductive C4Effects (tgt : Target) : State → Footprint → State → Prop where
ConcreteSpec/Spec/Frame.lean:312: def C1EffectImplementation (O : Exec.Operands) (P : Exec.Program O) (project : O.Store → State)
ConcreteSpec/Spec/Frame.lean:435: def Signature.WellFormed {Ty Val Reg Word : Type} (sig : Signature Ty Val Reg Word) : Prop :=
ConcreteSpec/Spec/Frame.lean:702: structure ExternParam (H : Type) where
ConcreteSpec/Spec/Frame.lean:743: def KeptPair.WellFormed {F H : Type} (pair : KeptPair F H)
ConcreteSpec/Spec/Frame.lean:837: def RetentionConforms {F H : Type} (pair : KeptPair F H)
ConcreteSpec/Spec/Frame.lean:845: def CheckedPairCorrect {F H : Type} (submit complete : F)
ConcreteSpec/Spec/Frame.lean:894: def Launcher.Outcome {Root Artifact Input Result Observation : Type}
ConcreteSpec/Spec/Frame.lean:967: def encodeToyExit : Nat → Nat → Nat
ConcreteSpec/Spec/Frame.lean:983: def toyLaunch (root reserved result : Nat) : Startup ((Nat × List Nat) × Nat × Nat × Nat) :=
ConcreteSpec/Spec/Frame.lean:1027: example : (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1033: example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1044: example : ¬ (⟨0, 1, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Frame.lean:1051: example : ¬ (⟨0, 0, 7⟩ : KeptPair Nat Nat).WellFormed
ConcreteSpec/Spec/Grant.lean:45: def WellFormed (s : State) : Prop :=
ConcreteSpec/Spec/Grant.lean:51: instance (s : State) : Decidable (WellFormed s) := by unfold WellFormed; infer_instance
ConcreteSpec/Spec/Grant.lean:170: example : WellFormed { initial with dependencies := [heap, phantom] } := by decide
ConcreteSpec/Spec/Grant.lean:171: example : ¬ WellFormed { initial with dependencies := [heap, heap] } := by decide
ConcreteSpec/Spec/Hybrid.lean:122: structure State.WellFormed (tgt : Target) (st : State) : Prop where
ConcreteSpec/Spec/Ident.lean:103: structure PackageId where
ConcreteSpec/Spec/Ident.lean:123: def ModuleId.topLevel (m : ModuleId) : String := m.path.head m.path_nonempty
ConcreteSpec/Spec/Ident.lean:126: structure LoadedModule where
ConcreteSpec/Spec/Ident.lean:134: inductive LoadCollision where
ConcreteSpec/Spec/Ident.lean:143: def LoadCollision.first : LoadCollision → LoadedModule
ConcreteSpec/Spec/Ident.lean:147: def LoadCollision.second : LoadCollision → LoadedModule
ConcreteSpec/Spec/Ident.lean:151: def LoadedModules.HasCollision (modules : List LoadedModule) (collision : LoadCollision) : Prop :=
ConcreteSpec/Spec/Ident.lean:155: def LoadedModules.WellFormed (modules : List LoadedModule) : Prop :=
ConcreteSpec/Spec/Ident.lean:168: def LoadCollision.withModule (first : LoadedModule) : List LoadedModule → Option LoadCollision
ConcreteSpec/Spec/Ident.lean:176: def LoadedModules.findCollisionFrom : List LoadedModule → List LoadedModule → Option LoadCollision
ConcreteSpec/Spec/Ident.lean:183: def LoadedModules.findCollision (modules : List LoadedModule) : Option LoadCollision :=
ConcreteSpec/Spec/Memory.lean:120: structure Memory.WellFormed (tgt : Target) (m : Memory) : Prop where
ConcreteSpec/Spec/Memory.lean:315: example : tinyTarget.WellFormed := by decide
ConcreteSpec/Spec/Observe.lean:167: def SiteTable.Valid (sources : SourceTable) (sites : SiteTable) : Prop :=
ConcreteSpec/Spec/Observe.lean:251: def ProjectSources.Realizes (input : ProjectSources) (project : LoadedProject) : Prop :=
ConcreteSpec/Spec/Observe.lean:454: structure ExecAdapter (O : Exec.Operands) (S : EventSignature) where
ConcreteSpec/Spec/Observe.lean:576: structure ExecAdapter.WellFormed (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
ConcreteSpec/Spec/Observe.lean:688: def Environment.adapter (S : EventSignature) : ExecAdapter (Environment.operands S) S where
ConcreteSpec/Spec/Observe.lean:779: example : SiteTable.Valid locationSources [(site, tabSpan)] := by
ConcreteSpec/Spec/Observe.lean:786: example : ¬ SiteTable.Valid locationSources [(site, badLocationSpan)] := by
ConcreteSpec/Spec/Observe.lean:830: def refusingAdapter : ExecAdapter refusingBindOps signature where
ConcreteSpec/Spec/Own.lean:107: def WellFormed (s : State) : Prop :=
ConcreteSpec/Spec/Own.lean:122: instance (s : State) : Decidable (WellFormed s) := by
ConcreteSpec/Spec/Own.lean:268: def endLocals (activation : Nat) (s : State) : Option State :=
ConcreteSpec/Spec/Own.lean:325: def CallState.WellFormed (s : CallState) : Prop :=
ConcreteSpec/Spec/Own.lean:330: instance (s : CallState) : Decidable (CallState.WellFormed s) := by
ConcreteSpec/Spec/Own.lean:695: def CheckerAccepted (O : Exec.Operands) (types : Var → Option Descriptor)
ConcreteSpec/Spec/Own.lean:960: example : (do
ConcreteSpec/Spec/Own.lean:964: example : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide
ConcreteSpec/Spec/Target.lean:1: (module documentation or namespace context)
ConcreteSpec/Spec/Target.lean:81: def Target.WellFormed (t : Target) : Prop :=
ConcreteSpec/Spec/Target.lean:87: instance (t : Target) : Decidable t.WellFormed := by unfold Target.WellFormed; infer_instance
ConcreteSpec/Spec/Target.lean:106: example : linuxX8664.WellFormed := by decide
ConcreteSpec/Spec/Target.lean:110: example : ¬ ({ linuxX8664 with
ConcreteSpec/Spec/Target.lean:117: example : ¬ ({ linuxX8664 with ptrAlign := 6 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:120: example : ¬ ({ linuxX8664 with ptrAlign := 16 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:123: example : ¬ ({ linuxX8664 with maxObject := 4 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:126: example : ¬ ({ linuxX8664 with maxObject := 2 ^ 64 } : Target).WellFormed := by decide
ConcreteSpec/Spec/Target.lean:130: example : ({ linuxX8664 with
changelog.d/2026-11-08-c4-memory.md:1: (module documentation or namespace context)
docs/LANDING.md:1: (module documentation or namespace context)
docs/archive/bugs-2026-11/bug_disposition.tsv:1: (module documentation or namespace context)
docs/decisions.md:1: (module documentation or namespace context)
scripts/checks/lib/treestate.sh:1: (module documentation or namespace context)
scripts/proofs/axioms.golden:1: (module documentation or namespace context)
```
