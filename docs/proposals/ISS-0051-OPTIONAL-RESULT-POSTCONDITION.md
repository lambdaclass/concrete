# ISS-0051: optional result postconditions

Design only; verified against tracked files at `3b4d01d6fee3910436eb7b63e49847f1e734a3d3`. No implementation is authorized by this note. The architectural criterion is one authoritative fact with checked views (docs/PHILOSOPHY.md:68-73).

Non-vacuity convention: each proposed law is a schema quantified over the named executable/predicate subject, with that subject occurring in its conclusion. Instantiate it with the intended subject and with a mutant of exactly the same type, at identical inputs and identical premises. A negative control must falsify the conclusion, never manufacture an inconsistent premise or assume the desired output. Closed cases below are proposed proof obligations, not claims of executed Lean checks. Preservation of an existing theorem means its full quantified proposition, including refusal and hostile-input cases, survives; proof scripts may change.

## Pair and decision: RETIRE generic copies; VIEW specialized postconditions

ConcreteSpec/Proofs/Own.lean:40-45, ConcreteSpec/Proofs/Grant.lean:17-25 and :1009-1017 each define ResultSatisfies and a decision instance: none satisfies True, some x satisfies P x. Own:20-37 additionally copies the branching and decision policy in StateResult, CallOwnResult and CallResult. The generic copies lack any distinct purpose. The specialized wrappers do have distinct purposes: ownership WellFormed, projected own WellFormed, and full call-state WellFormed respectively.

Spec/Basics owns `OptionalResult.Satisfies {α : Type} (P : α → Prop) : Option α → Prop` with the existing cases, plus exactly one Decidable instance with pointwise decidability. Proofs/Basics owns its laws. Delete all three namespace-generic ResultSatisfies declarations and instances, migrate every user to the qualified common name. Keep specialized names as abbreviations applying the common predicate to their EXACT current postconditions; delete their custom decision instances and use the common instance by reduction. Do not replace CallResult by CallOwnResult or equate their postconditions.

## Laws and discriminators

S has exact type `{α : Type} → (α → Prop) → Option α → Prop` (implicit universe here matches existing Type; universe generalization is unnecessary).

* `∀ {α} (P : α → Prop) r, S P r ↔ ∀ x, r = some x → P x`. Subject S appears in conclusion. Closed α=Nat, P=(· = 0), r=none is True; same-type mutant returning False on none fails. At r=some 1 the result is False; always-True/ignore-success mutant fails. At r=some 0 the result is True; always-False mutant fails. No successful-result premise hides an erroneous none case.
* Decision law for d of exact type `∀ {α} (P : α → Prop) [∀ x, Decidable (P x)] (r : Option α), Decidable (S P r)`: `∀ P r, @decide (S P r) (d P r) = decide (match r with | none => True | some x => P x)`. Subject d occurs in conclusion. Same closed cases apply. There is no well-typed incorrect Decidable mutant for fixed S in consistent Lean: the proof fields prevent one. Use the joint exact-type subject `Σ S : ({α : Type} → (α → Prop) → Option α → Prop), ∀ {α} P, [∀ x, Decidable (P x)] → ∀ r, Decidable (S P r)` to mutate policy and its decision procedure together; reject-none and ignore-success pairs are well typed and fail the same decision law. Do not report an impossible inhabitant as a negative control.
* Specialized view equations: `∀ r, StateResult r ↔ S Own.WellFormed r`; `∀ r, CallOwnResult r ↔ S (fun s => Own.WellFormed s.own) r`; `∀ r, CallResult r ↔ S Own.CallState.WellFormed r`. Each subject has its original `Option State → Prop` or `Option CallState → Prop` type. Closed none refutes reject-none mutants. For closed successful controls use `Own.Examples.initial` and `⟨Own.Examples.exclusive, []⟩ : Own.CallState`. For StateResult use the closed ill-formed state `{ Own.Examples.shared with slots := [] }`, already refuted at ConcreteSpec/Proofs/Own.lean:980. For CallOwnResult wrap that state as `⟨bad, []⟩`. For CallResult use `{ Own.CallWitnesses.waiting with pending := Own.CallWitnesses.waiting.pending ++ Own.CallWitnesses.waiting.pending }`, refuted at ConcreteSpec/Proofs/Own.lean:1399. Its own projection is the unchanged well-formed waiting.own, so replacing CallResult with CallOwnResult is rejected too. Always-True successful branches fail on these closed some results; always-False successful branches fail on the closed well-formed results. None cases need no state fixture.

## Existing strength and order

All operation preservation, frame/cleanup, generativity, attenuation, consumption and protocol theorems using these names keep their exact predicates on successful results and continue to allow refusal. Replacing a generic name by the qualified common owner is definitional equality, with no additional antecedents and no weakened well-formedness requirement. Every instance and theorem in the inventory below is in the migration set; update unfolding/simp lists to the new definition rather than adding compatibility copies. Specialized abbreviations can retain their names in existing theorem statements. No runtime operation, ownership rule, allocator behavior or accepted input changes.

Order: common definition/instance in Spec/Basics and laws/discriminators in Proofs/Basics; add/import common owner as needed (Spec/Own and Spec/Grant already reach shared foundation dependencies, verify no cycle); replace specialized bodies by abbreviations; migrate generic uses in Proofs/Own and Proofs/Grant and all further inventory users; remove duplicated instances. Lean build must confirm instance synthesis as well as propositions. No independent namespace decision procedure survives.

## QUESTIONS

None about semantics. A well-typed false Decidable value cannot be constructed; the joint policy/decision subject above is the honest mutation boundary. The closed specialized fixtures above preserve each distinct postcondition.

## Verified user and affected declaration inventory

Repository-wide tracked-text search at the SHA above, excluding historical legacy/, task material, issue rows and proposal notes. Includes declarations, qualified and unqualified references, proof unfolding, examples, comments and documentation; broad matches intentionally retain dependent users rather than losing namespace-local calls. No production user is excluded by a hand-picked file list. External Lean library internals are not repository users.

Search expression: `\b(ResultSatisfies|StateResult|CallOwnResult|CallResult)\b`. Reproduce with `git grep -n -E` over the tracked nonhistorical sources (the expression uses standard word boundaries).

Every matching use (path:line):

```text
ConcreteSpec/Proofs/Grant.lean:17: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Grant.lean:22: Decidable (ResultSatisfies P x) :=
ConcreteSpec/Proofs/Grant.lean:30: ResultSatisfies WellFormed (fresh s scope) := by
ConcreteSpec/Proofs/Grant.lean:64: ResultSatisfies WellFormed (depend s d) := by
ConcreteSpec/Proofs/Grant.lean:88: ResultSatisfies WellFormed (consume s p) := by
ConcreteSpec/Proofs/Grant.lean:103: ResultSatisfies WellFormed (release s i) := by
ConcreteSpec/Proofs/Grant.lean:141: ResultSatisfies WellFormed (reset s i) := by
ConcreteSpec/Proofs/Grant.lean:185: ResultSatisfies WellFormed (endScope s scope) := by
ConcreteSpec/Proofs/Grant.lean:307: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:325: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:349: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:368: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:396: ResultSatisfies (fun r : α × (σ × State) => operation s.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:405: ResultSatisfies (fun r : List α × (σ × State) => runErased cs s.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:422: simp [ResultSatisfies, runErased, hc, ht]
ConcreteSpec/Proofs/Grant.lean:499: example : ResultSatisfies WellFormed (fresh emptyState ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:504: ¬ ResultSatisfies WellFormed (freshNoAdvance emptyState ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:507: example : ResultSatisfies WellFormed (depend phantomOnly heap) :=
ConcreteSpec/Proofs/Grant.lean:512: ¬ ResultSatisfies WellFormed (dependDuplicatePlace phantomOnly heap) := by decide
ConcreteSpec/Proofs/Grant.lean:515: example : ResultSatisfies WellFormed (consume dependent ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:520: ¬ ResultSatisfies WellFormed (consumeDropBinders dependent ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:523: example : ResultSatisfies WellFormed (release releaseInput ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:528: ¬ ResultSatisfies WellFormed (releaseAll releaseInput ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:531: example : ResultSatisfies WellFormed (reset initial ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:536: ¬ ResultSatisfies WellFormed (resetNoAdvance initial ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:539: example : ResultSatisfies WellFormed (endScope departing ⟨0⟩) :=
ConcreteSpec/Proofs/Grant.lean:544: ¬ ResultSatisfies WellFormed (endScopeDropAllBinders departing ⟨0⟩) := by decide
ConcreteSpec/Proofs/Grant.lean:610: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:616: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:621: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:627: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:632: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:638: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:643: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:651: ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:685: example : ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:690: example : ¬ ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:694: example : ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:699: example : ¬ ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:723: (hr : Retired s i) : ResultSatisfies (fun t => Retired t i) (lifetimeStep s op) := by
ConcreteSpec/Proofs/Grant.lean:785: (hr : Retired s i) : ResultSatisfies (fun t => Retired t i) (runLifetime s ops) := by
ConcreteSpec/Proofs/Grant.lean:838: example : ResultSatisfies (fun t => Retired t ⟨0⟩) (lifetimeStep departed (.fresh ⟨0⟩)) :=
ConcreteSpec/Proofs/Grant.lean:842: example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:846: example : ResultSatisfies (fun t => Retired t ⟨0⟩) (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:850: example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:897: ResultSatisfies WellFormed (runLifetime s ops) := by
ConcreteSpec/Proofs/Grant.lean:933: ResultSatisfies (fun t => ∀ b ∈ s.binders, b.scope = scope →
ConcreteSpec/Proofs/Grant.lean:940: | none => simp only [Option.bind_some, hx, ResultSatisfies]
ConcreteSpec/Proofs/Grant.lean:942: simp only [Option.bind_some, hx, ResultSatisfies]
ConcreteSpec/Proofs/Grant.lean:964: simpa only [hx, ResultSatisfies] using h
ConcreteSpec/Proofs/Grant.lean:979: example : ResultSatisfies WellFormed (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:984: ¬ ResultSatisfies WellFormed (runLifetimeDuplicateBrand departed operations) := by decide
ConcreteSpec/Proofs/Grant.lean:987: example : ResultSatisfies (fun t => ∀ b ∈ departing.binders, b.scope = ⟨0⟩ →
ConcreteSpec/Proofs/Grant.lean:994: ¬ ResultSatisfies (fun t => ∀ b ∈ departing.binders, b.scope = ⟨0⟩ →
ConcreteSpec/Proofs/Grant.lean:1009: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Grant.lean:1014: Decidable (ResultSatisfies P x) :=
ConcreteSpec/Proofs/Grant.lean:1081: ResultSatisfies (Bounded budget) (narrow s i requested) := by
ConcreteSpec/Proofs/Grant.lean:1097: ResultSatisfies (Bounded budget) (acquire s i requested selected permissions allowed handle) := by
ConcreteSpec/Proofs/Grant.lean:1114: ResultSatisfies (fun r : CloseResult × State H => Bounded budget r.2)
ConcreteSpec/Proofs/Grant.lean:1156: ResultSatisfies (Bounded budget) (runOps s ops) := by
ConcreteSpec/Proofs/Grant.lean:1177: ResultSatisfies Fresh (narrow s i requested) := by
ConcreteSpec/Proofs/Grant.lean:1189: ResultSatisfies Fresh (acquire s i requested selected permissions allowed handle) := by
ConcreteSpec/Proofs/Grant.lean:1201: ResultSatisfies (fun r : CloseResult × State H => Fresh r.2) (close s i result) := by
ConcreteSpec/Proofs/Grant.lean:1255: ResultSatisfies (fun r : CloseResult × State H => r.1 = result ∧ Dead r.2 i)
ConcreteSpec/Proofs/Grant.lean:1268: ResultSatisfies (fun t : State H => Dead t i) (narrow s i requested) := by
ConcreteSpec/Proofs/Grant.lean:1282: ResultSatisfies (fun t : State H => Dead t i ∧ Fresh t) (runOps s ops) := by
ConcreteSpec/Proofs/Grant.lean:1335: ResultSatisfies (fun t : State H =>
ConcreteSpec/Proofs/Grant.lean:1347: ResultSatisfies (fun t : State H => usable t i = usable s i)
ConcreteSpec/Proofs/Grant.lean:1359: simp [ResultSatisfies, usable, issue, AList.lookup, hi]
ConcreteSpec/Proofs/Grant.lean:1470: example : ResultSatisfies (Bounded launchBudget) (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1475: ¬ ResultSatisfies (Bounded launchBudget) (narrowTakeRequested initial ⟨0⟩ [1, 2]) := by decide
ConcreteSpec/Proofs/Grant.lean:1478: example : ResultSatisfies (Bounded launchBudget) (acquire initial ⟨0⟩ [1, 2] [1] [.fs] true 99) :=
ConcreteSpec/Proofs/Grant.lean:1483: ¬ ResultSatisfies (Bounded launchBudget)
ConcreteSpec/Proofs/Grant.lean:1487: example : ResultSatisfies (fun r : CloseResult × State Nat => Bounded launchBudget r.2)
ConcreteSpec/Proofs/Grant.lean:1493: ¬ ResultSatisfies (fun r : CloseResult × State Nat => Bounded launchBudget r.2)
ConcreteSpec/Proofs/Grant.lean:1497: example : ResultSatisfies (Bounded launchBudget) (runOps initial validOps) :=
ConcreteSpec/Proofs/Grant.lean:1502: ¬ ResultSatisfies (Bounded launchBudget) (runOpsMintRight initial validOps) := by decide
ConcreteSpec/Proofs/Grant.lean:1505: example : ResultSatisfies Fresh (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1510: ¬ ResultSatisfies Fresh (narrowNoAdvance initial ⟨0⟩ [1, 2]) := by decide
ConcreteSpec/Proofs/Grant.lean:1513: example : ResultSatisfies Fresh (acquire initial ⟨0⟩ [1] [1] [.fs] true 99) :=
ConcreteSpec/Proofs/Grant.lean:1518: ¬ ResultSatisfies Fresh (acquireNoAdvance initial ⟨0⟩ [1] [1] [.fs] true 99) := by decide
ConcreteSpec/Proofs/Grant.lean:1521: example : ResultSatisfies (fun r : CloseResult × State Nat => Fresh r.2)
ConcreteSpec/Proofs/Grant.lean:1527: ¬ ResultSatisfies (fun r : CloseResult × State Nat => Fresh r.2)
ConcreteSpec/Proofs/Grant.lean:1539: example : ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1544: example : ¬ ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1548: example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩) (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1553: ¬ ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1557: example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩ ∧ Fresh t)
ConcreteSpec/Proofs/Grant.lean:1564: ¬ ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩ ∧ Fresh t)
ConcreteSpec/Proofs/Grant.lean:1568: example : ResultSatisfies (fun t : State Nat =>
ConcreteSpec/Proofs/Grant.lean:1575: example : ¬ ResultSatisfies (fun t : State Nat =>
ConcreteSpec/Proofs/Grant.lean:1586: example : ResultSatisfies (fun t : State Nat => usable t ⟨0⟩ = usable initial ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1592: ¬ ResultSatisfies (fun t : State Nat => usable t ⟨0⟩ = usable initial ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1601: ResultSatisfies (fun t => ∀ right,
ConcreteSpec/Proofs/Grant.lean:1619: example : ResultSatisfies (fun t => ∀ right,
ConcreteSpec/Proofs/Grant.lean:1625: example : ¬ ResultSatisfies (fun t => ∀ right,
ConcreteSpec/Proofs/Own.lean:20: def StateResult : Option State → Prop
ConcreteSpec/Proofs/Own.lean:24: def CallOwnResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:28: def CallResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:32: instance (r : Option State) : Decidable (StateResult r) := by
ConcreteSpec/Proofs/Own.lean:33: cases r <;> unfold StateResult <;> infer_instance
ConcreteSpec/Proofs/Own.lean:34: instance (r : Option CallState) : Decidable (CallOwnResult r) := by
ConcreteSpec/Proofs/Own.lean:35: cases r <;> unfold CallOwnResult <;> infer_instance
ConcreteSpec/Proofs/Own.lean:36: instance (r : Option CallState) : Decidable (CallResult r) := by
ConcreteSpec/Proofs/Own.lean:37: cases r <;> unfold CallResult <;> infer_instance
ConcreteSpec/Proofs/Own.lean:40: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Own.lean:44: Decidable (ResultSatisfies P r) := by
ConcreteSpec/Proofs/Own.lean:45: cases r <;> unfold ResultSatisfies <;> infer_instance
ConcreteSpec/Proofs/Own.lean:62: theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
ConcreteSpec/Proofs/Own.lean:111: theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
ConcreteSpec/Proofs/Own.lean:185: (m : Mode) (hs : WellFormed s) : StateResult (borrow s p path m) := by
ConcreteSpec/Proofs/Own.lean:242: StateResult (reborrowTo s i path m activation) := by
ConcreteSpec/Proofs/Own.lean:319: (m : Mode) (hs : WellFormed s) : StateResult (reborrow s i path m) := by
ConcreteSpec/Proofs/Own.lean:330: theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
ConcreteSpec/Proofs/Own.lean:452: (hs : WellFormed s) : StateResult (bind types x s) := by
ConcreteSpec/Proofs/Own.lean:540: theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
ConcreteSpec/Proofs/Own.lean:614: (hs : WellFormed s) : StateResult (enter types xs s) := by
ConcreteSpec/Proofs/Own.lean:654: theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
ConcreteSpec/Proofs/Own.lean:702: ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ f.id) (leave s) := by
ConcreteSpec/Proofs/Own.lean:738: theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
ConcreteSpec/Proofs/Own.lean:823: StateResult (enterDown s args) := by
ConcreteSpec/Proofs/Own.lean:853: (hs : WellFormed s) : StateResult (life types e s) := by
ConcreteSpec/Proofs/Own.lean:1061: (hs : WellFormed s) : StateResult (adoptDown s args) := by
ConcreteSpec/Proofs/Own.lean:1089: (hs : WellFormed s.own) : CallOwnResult (passDown s i parameter) := by
ConcreteSpec/Proofs/Own.lean:1137: (hs : CallState.WellFormed s) : CallResult (passDown s i parameter) := by
ConcreteSpec/Proofs/Own.lean:1205: (s : CallState) (hs : WellFormed s.own) : CallOwnResult (callLife types e s) := by
ConcreteSpec/Proofs/Own.lean:1342: (s : CallState) (hs : CallState.WellFormed s) : CallResult (callLife types e s) := by
ConcreteSpec/Proofs/Own.lean:1529: ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ entry.2) (leave s) := by
ConcreteSpec/Proofs/Own.lean:1585: (m : Mode) (x : Var) (hs : WellFormed s.own) : CallOwnResult (passOwner s p path m x) := by
ConcreteSpec/Proofs/Own.lean:1645: CallResult (passOwner s p path m x) := by
ConcreteSpec/Proofs/Own.lean:1701: ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:1714: cases m <;> simp [ResultSatisfies, ownerAccess, show (Mode.mutable == Mode.shared) = false from rfl]
ConcreteSpec/Proofs/Own.lean:1769: ResultSatisfies (fun t : State => l ∈ t.loans) (leave s) := by
ConcreteSpec/Proofs/Own.lean:1816: ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ f.id) (leave s) := by
ConcreteSpec/Proofs/Own.lean:2064: ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo s i path m a) := by
ConcreteSpec/Proofs/Own.lean:2085: ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown s args) := by
ConcreteSpec/Proofs/Own.lean:2128: ResultSatisfies (fun t : State => LiveDestinations t) (leave s) := by
ConcreteSpec/Proofs/Own.lean:4408: theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
ConcreteSpec/Proofs/Own.lean:4414: theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
ConcreteSpec/Proofs/Own.lean:4566: example : StateResult (move (initial) (⟨0⟩)) :=
ConcreteSpec/Proofs/Own.lean:4570: example : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4573: example : StateResult (reinitialize (LifecycleWitnesses.moved) (⟨0⟩)) :=
ConcreteSpec/Proofs/Own.lean:4577: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4580: example : StateResult (borrow (shared) (⟨0⟩) ([.index 1]) (.shared)) :=
ConcreteSpec/Proofs/Own.lean:4584: example : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4587: example : StateResult (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) :=
ConcreteSpec/Proofs/Own.lean:4591: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4594: example : StateResult (reborrow (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) :=
ConcreteSpec/Proofs/Own.lean:4598: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4601: example : StateResult (endLoan (child) (⟨1⟩)) :=
ConcreteSpec/Proofs/Own.lean:4605: example : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4608: example : StateResult (accessOwner (shared) (⟨0⟩) (.shared)) :=
ConcreteSpec/Proofs/Own.lean:4612: example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4615: example : StateResult (accessLoan (child) (⟨1⟩) (.mutable)) :=
ConcreteSpec/Proofs/Own.lean:4619: example : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4622: example : StateResult (bind (types) (y) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4626: example : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4629: example : StateResult (drop ([x]) (LifecycleWitnesses.moved)) :=
ConcreteSpec/Proofs/Own.lean:4633: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide
ConcreteSpec/Proofs/Own.lean:4636: example : StateResult (enter (types) ([y]) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4640: example : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4643: example : StateResult (endLocals (1) (LifecycleWitnesses.called)) :=
ConcreteSpec/Proofs/Own.lean:4647: example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCallerInstead (1) (LifecycleWitnesses.called)) := by decide
ConcreteSpec/Proofs/Own.lean:4650: example : StateResult (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4654: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveKeepLocalLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4657: example : StateResult (enterDown (exclusive) ([(y, ⟨0⟩)])) :=
ConcreteSpec/Proofs/Own.lean:4661: example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide
ConcreteSpec/Proofs/Own.lean:4664: example : StateResult (life (types) ((.bind y)) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4668: example : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4671: example : StateResult (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
ConcreteSpec/Proofs/Own.lean:4675: example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4678: example : CallOwnResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
ConcreteSpec/Proofs/Own.lean:4682: example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4685: example : CallResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
ConcreteSpec/Proofs/Own.lean:4689: example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4692: example : CallOwnResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
ConcreteSpec/Proofs/Own.lean:4696: example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4699: example : CallResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
ConcreteSpec/Proofs/Own.lean:4703: example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4706: example : CallOwnResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4710: example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4713: example : CallResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4717: example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4746: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4750: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4755: example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4759: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4764: example : ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4768: example : ¬ ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwnerNoFreeze (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4773: example : ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4777: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ (RoundThreeWitnesses.localLoan).loans) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)).activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) ∧ ¬ ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leaveAllLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4782: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4786: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4791: example : ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) :=
ConcreteSpec/Proofs/Own.lean:4795: example : (LiveDestinations (exclusive)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (reborrowToDead (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4800: example : ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
ConcreteSpec/Proofs/Own.lean:4804: example : (LiveDestinations (adoptionInput)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (adoptDownDead (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4809: example : ResultSatisfies (fun t : State => LiveDestinations t) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4813: example : (LiveDestinations (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (leaveStranded (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4949: ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
ConcreteSpec/Proofs/Own.lean:4955: have hwf : StateResult (borrow s p path m) := borrow_preserves s p path m hs
ConcreteSpec/Proofs/Own.lean:4962: ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
ConcreteSpec/Proofs/Own.lean:4968: have hwf : StateResult (move s p) := move_preserves s p hs
ConcreteSpec/Proofs/Own.lean:4975: ResultSatisfies (fun r : α × Store σ => operation (erase s) = some (r.1, erase r.2))
ConcreteSpec/Proofs/Own.lean:4984: ResultSatisfies (fun t : Store σ => erase t = erase s ∧ life types e s.own = some t.own)
ConcreteSpec/Proofs/Own.lean:4993: ResultSatisfies (fun t : σ × CallState => t.1 = s.1 ∧ callLife types e s.2 = some t.2)
ConcreteSpec/Proofs/Own.lean:5003: ResultSatisfies (fun r : α × Store σ => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5011: ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5021: ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5030: ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5041: ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5052: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5063: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5074: ResultSatisfies (fun t : State => t.nextLoan = s.nextLoan)
ConcreteSpec/Proofs/Own.lean:5082: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5115: ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5133: ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:5414: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
ConcreteSpec/Proofs/Own.lean:5420: ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
ConcreteSpec/Proofs/Own.lean:5432: example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
ConcreteSpec/Proofs/Own.lean:5438: ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
ConcreteSpec/Proofs/Own.lean:5442: example : ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
ConcreteSpec/Proofs/Own.lean:5447: example : ¬ ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
ConcreteSpec/Proofs/Own.lean:5451: example : ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
ConcreteSpec/Proofs/Own.lean:5456: example : ¬ ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
ConcreteSpec/Proofs/Own.lean:5460: example : ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
ConcreteSpec/Proofs/Own.lean:5465: example : ¬ ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
ConcreteSpec/Proofs/Own.lean:5469: example : ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5478: ¬ ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5483: example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5489: example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5497: example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5503: example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5512: example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5520: example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5530: example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5537: example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5550: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5557: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5563: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5571: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5583: example : ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
ConcreteSpec/Proofs/Own.lean:5588: example : ¬ ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
ConcreteSpec/Proofs/Own.lean:5592: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5600: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5607: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5614: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5620: example : ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:5629: example : ¬ ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:6001: ResultSatisfies (fun t => RetiredLoan t i) (loanAction s op) := by
ConcreteSpec/Proofs/Own.lean:6051: ResultSatisfies (fun t => RetiredLoan t i) (runLoanSequence s ops) := by
ConcreteSpec/Proofs/Own.lean:6084: example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
ConcreteSpec/Proofs/Own.lean:6090: ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
ConcreteSpec/Proofs/Own.lean:6098: example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (runLoanSequence endedChild actions) :=
ConcreteSpec/Proofs/Own.lean:6103: ¬ ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (recycle endedChild actions) := by decide
```

Enclosing declarations, including every directly affected existing theorem and example (names are original source headers; continuation lines remain in the source):

```text
ConcreteSpec/Proofs/Grant.lean:17: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Grant.lean:21: instance {α : Type} (P : α → Prop) (x : Option α) [∀ a, Decidable (P a)] :
ConcreteSpec/Proofs/Grant.lean:29: theorem fresh_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:63: theorem depend_preserves (s : State) (d : Dependency) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:87: theorem consume_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:102: theorem release_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:140: theorem reset_preserves (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:184: theorem endScope_preserves (s : State) (scope : BlockId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:306: theorem fresh_generation (s : State) (scope : BlockId) :
ConcreteSpec/Proofs/Grant.lean:324: theorem reset_generation (s : State) (i : BrandId) :
ConcreteSpec/Proofs/Grant.lean:348: theorem endScope_non_escape (s : State) (scope : BlockId) :
ConcreteSpec/Proofs/Grant.lean:367: theorem allocations_have_live_allocator (s : State) (i : BrandId) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:394: theorem instrument_erasure {σ α : Type} (transition : State → Option State)
ConcreteSpec/Proofs/Grant.lean:404: theorem run_erasure {σ α : Type} (cs : List (Command σ α)) (s : σ × State) :
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
ConcreteSpec/Proofs/Grant.lean:610: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:616: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:621: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:627: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:632: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:638: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:643: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Grant.lean:650: example : WellFormed releaseInput ∧
ConcreteSpec/Proofs/Grant.lean:685: example : ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:690: example : ¬ ResultSatisfies (fun r : Nat × (Nat × State) => incNat runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:694: example : ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:699: example : ¬ ResultSatisfies (fun r : List Nat × (Nat × State) => runErased commands runStart.1 = some (r.1, r.2.1))
ConcreteSpec/Proofs/Grant.lean:722: theorem lifetime_retired_step (s : State) (op : LifetimeOp) (i : BrandId)
ConcreteSpec/Proofs/Grant.lean:784: theorem retired_never_revives (s : State) (i : BrandId) (ops : List LifetimeOp)
ConcreteSpec/Proofs/Grant.lean:838: example : ResultSatisfies (fun t => Retired t ⟨0⟩) (lifetimeStep departed (.fresh ⟨0⟩)) :=
ConcreteSpec/Proofs/Grant.lean:842: example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:846: example : ResultSatisfies (fun t => Retired t ⟨0⟩) (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:850: example : Retired departed ⟨0⟩ ∧ ¬ ResultSatisfies (fun t => Retired t ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:896: theorem lifetime_wf (s : State) (ops : List LifetimeOp) (hs : WellFormed s) :
ConcreteSpec/Proofs/Grant.lean:931: theorem scope_exit_lifetime (s : State) (scope : BlockId) (ops : List LifetimeOp)
ConcreteSpec/Proofs/Grant.lean:979: example : ResultSatisfies WellFormed (runLifetime departed operations) :=
ConcreteSpec/Proofs/Grant.lean:983: example : WellFormed departed ∧
ConcreteSpec/Proofs/Grant.lean:987: example : ResultSatisfies (fun t => ∀ b ∈ departing.binders, b.scope = ⟨0⟩ →
ConcreteSpec/Proofs/Grant.lean:993: example : WellFormed departing ∧
ConcreteSpec/Proofs/Grant.lean:1009: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Grant.lean:1013: instance {α : Type} (P : α → Prop) (x : Option α) [∀ a, Decidable (P a)] :
ConcreteSpec/Proofs/Grant.lean:1079: theorem narrow_bounded (budget : List Nat) (s : State H) (i : GrantId) (requested : List Nat)
ConcreteSpec/Proofs/Grant.lean:1094: theorem acquire_bounded (budget : List Nat) (s : State H) (i : GrantId)
ConcreteSpec/Proofs/Grant.lean:1112: theorem close_bounded (budget : List Nat) (s : State H) (i : GrantId) (result : CloseResult)
ConcreteSpec/Proofs/Grant.lean:1154: theorem rights_never_grow (budget : List Nat) (s : State H) (ops : List (Op H))
ConcreteSpec/Proofs/Grant.lean:1176: theorem narrow_fresh (s : State H) (i : GrantId) (requested : List Nat) (hs : Fresh s) :
ConcreteSpec/Proofs/Grant.lean:1187: theorem acquire_fresh (s : State H) (i : GrantId) (requested selected : List Nat)
ConcreteSpec/Proofs/Grant.lean:1200: theorem close_fresh (s : State H) (i : GrantId) (result : CloseResult) (hs : Fresh s) :
ConcreteSpec/Proofs/Grant.lean:1254: theorem close_consumes (s : State H) (i : GrantId) (result : CloseResult) :
ConcreteSpec/Proofs/Grant.lean:1267: theorem narrow_consumes (s : State H) (i : GrantId) (requested : List Nat) (hs : Fresh s) :
ConcreteSpec/Proofs/Grant.lean:1280: theorem consumed_never_revives (s : State H) (i : GrantId) (ops : List (Op H))
ConcreteSpec/Proofs/Grant.lean:1334: theorem narrow_intersection (s : State H) (i : GrantId) (requested : List Nat) :
ConcreteSpec/Proofs/Grant.lean:1345: theorem acquire_keeps_parent (s : State H) (i : GrantId) (requested selected : List Nat)
ConcreteSpec/Proofs/Grant.lean:1470: example : ResultSatisfies (Bounded launchBudget) (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1474: example : Bounded launchBudget initial ∧
ConcreteSpec/Proofs/Grant.lean:1478: example : ResultSatisfies (Bounded launchBudget) (acquire initial ⟨0⟩ [1, 2] [1] [.fs] true 99) :=
ConcreteSpec/Proofs/Grant.lean:1482: example : Bounded launchBudget initial ∧
ConcreteSpec/Proofs/Grant.lean:1487: example : ResultSatisfies (fun r : CloseResult × State Nat => Bounded launchBudget r.2)
ConcreteSpec/Proofs/Grant.lean:1492: example : Bounded launchBudget initial ∧
ConcreteSpec/Proofs/Grant.lean:1497: example : ResultSatisfies (Bounded launchBudget) (runOps initial validOps) :=
ConcreteSpec/Proofs/Grant.lean:1501: example : Bounded launchBudget initial ∧
ConcreteSpec/Proofs/Grant.lean:1505: example : ResultSatisfies Fresh (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1509: example : Fresh initial ∧
ConcreteSpec/Proofs/Grant.lean:1513: example : ResultSatisfies Fresh (acquire initial ⟨0⟩ [1] [1] [.fs] true 99) :=
ConcreteSpec/Proofs/Grant.lean:1517: example : Fresh initial ∧
ConcreteSpec/Proofs/Grant.lean:1521: example : ResultSatisfies (fun r : CloseResult × State Nat => Fresh r.2)
ConcreteSpec/Proofs/Grant.lean:1526: example : Fresh initial ∧
ConcreteSpec/Proofs/Grant.lean:1539: example : ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1544: example : ¬ ResultSatisfies (fun r : CloseResult × State Nat => r.1 = .error 5 ∧ Dead r.2 ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1548: example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩) (narrow initial ⟨0⟩ [1, 2]) :=
ConcreteSpec/Proofs/Grant.lean:1552: example : Fresh initial ∧
ConcreteSpec/Proofs/Grant.lean:1557: example : ResultSatisfies (fun t : State Nat => Dead t ⟨0⟩ ∧ Fresh t)
ConcreteSpec/Proofs/Grant.lean:1562: example : Fresh afterClose0 ∧
ConcreteSpec/Proofs/Grant.lean:1568: example : ResultSatisfies (fun t : State Nat =>
ConcreteSpec/Proofs/Grant.lean:1575: example : ¬ ResultSatisfies (fun t : State Nat =>
ConcreteSpec/Proofs/Grant.lean:1586: example : ResultSatisfies (fun t : State Nat => usable t ⟨0⟩ = usable initial ⟨0⟩)
ConcreteSpec/Proofs/Grant.lean:1591: example : Fresh initial ∧
ConcreteSpec/Proofs/Grant.lean:1600: theorem narrow_permissions (s : State H) (i : GrantId) (requested : List Nat) :
ConcreteSpec/Proofs/Grant.lean:1619: example : ResultSatisfies (fun t => ∀ right,
ConcreteSpec/Proofs/Grant.lean:1625: example : ¬ ResultSatisfies (fun t => ∀ right,
ConcreteSpec/Proofs/Own.lean:20: def StateResult : Option State → Prop
ConcreteSpec/Proofs/Own.lean:24: def CallOwnResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:28: def CallResult : Option CallState → Prop
ConcreteSpec/Proofs/Own.lean:32: instance (r : Option State) : Decidable (StateResult r) := by
ConcreteSpec/Proofs/Own.lean:34: instance (r : Option CallState) : Decidable (CallOwnResult r) := by
ConcreteSpec/Proofs/Own.lean:36: instance (r : Option CallState) : Decidable (CallResult r) := by
ConcreteSpec/Proofs/Own.lean:40: def ResultSatisfies {α : Type} (P : α → Prop) : Option α → Prop
ConcreteSpec/Proofs/Own.lean:43: instance {α : Type} (P : α → Prop) [∀ t, Decidable (P t)] (r : Option α) :
ConcreteSpec/Proofs/Own.lean:62: theorem move_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (move s p) := by
ConcreteSpec/Proofs/Own.lean:111: theorem reinitialize_preserves (s : State) (p : Hybrid.Place) (hs : WellFormed s) : StateResult (reinitialize s p) := by
ConcreteSpec/Proofs/Own.lean:184: theorem borrow_preserves (s : State) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:240: theorem reborrowTo_preserves (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:318: theorem reborrow_preserves (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:330: theorem endLoan_preserves (s : State) (i : LoanId) (hs : WellFormed s) : StateResult (endLoan s i) := by
ConcreteSpec/Proofs/Own.lean:451: theorem bind_preserves (types : Var → Option Descriptor) (s : State) (x : Var)
ConcreteSpec/Proofs/Own.lean:540: theorem drop_preserves (s : State) (xs : List Var) (hs : WellFormed s) : StateResult (drop xs s) := by
ConcreteSpec/Proofs/Own.lean:613: theorem enter_preserves (types : Var → Option Descriptor) (s : State) (xs : List Var)
ConcreteSpec/Proofs/Own.lean:654: theorem endLocals_preserves (s : State) (a : Nat) (hs : WellFormed s) : StateResult (endLocals a s) := by
ConcreteSpec/Proofs/Own.lean:700: theorem leave_no_activation_slot (s : State) (f : Activation)
ConcreteSpec/Proofs/Own.lean:738: theorem leave_preserves (s : State) (hs : WellFormed s) : StateResult (leave s) := by
ConcreteSpec/Proofs/Own.lean:822: theorem enterDown_preserves (s : State) (args : List (Var × LoanId)) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:852: theorem life_preserves (types : Var → Option Descriptor) (e : Exec.Life) (s : State)
ConcreteSpec/Proofs/Own.lean:1060: theorem adoptDown_preserves (s : State) (args : List (Var × LoanId))
ConcreteSpec/Proofs/Own.lean:1088: theorem passDown_preserves (s : CallState) (i : LoanId) (parameter : Var)
ConcreteSpec/Proofs/Own.lean:1136: theorem passDown_call_preserves (s : CallState) (i : LoanId) (parameter : Var)
ConcreteSpec/Proofs/Own.lean:1204: theorem callLife_preserves (types : Var → Option Descriptor) (e : Exec.Life)
ConcreteSpec/Proofs/Own.lean:1341: theorem callLife_call_preserves (types : Var → Option Descriptor) (e : Exec.Life)
ConcreteSpec/Proofs/Own.lean:1527: theorem leave_no_local_loan (s : State) (f : Activation) (entry : Var × Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:1584: theorem passOwner_preserves (s : CallState) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:1643: theorem passOwner_call_preserves (s : CallState) (p : Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:1699: theorem passOwner_freezes (s : CallState) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:1766: theorem leave_retains_caller (s : State) (f : Activation) (l : Loan)
ConcreteSpec/Proofs/Own.lean:1814: theorem leave_no_activation_loan (s : State) (f : Activation)
ConcreteSpec/Proofs/Own.lean:2062: theorem reborrowTo_live (s : State) (i : LoanId) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:2083: theorem adoptDown_live (s : State) (args : List (Var × LoanId))
ConcreteSpec/Proofs/Own.lean:2126: theorem leave_live (s : State) (f : Activation) (hs : LiveDestinations s)
ConcreteSpec/Proofs/Own.lean:4408: theorem accessOwner_preserves (s : State) (p : Hybrid.Place) (m : Mode) (hs : WellFormed s) : StateResult (accessOwner s p m) := by
ConcreteSpec/Proofs/Own.lean:4414: theorem accessLoan_preserves (s : State) (i : LoanId) (m : Mode) (hs : WellFormed s) : StateResult (accessLoan s i m) := by
ConcreteSpec/Proofs/Own.lean:4566: example : StateResult (move (initial) (⟨0⟩)) :=
ConcreteSpec/Proofs/Own.lean:4570: example : (WellFormed (initial)) ∧ ¬ StateResult (moveDuplicateSlot (initial) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4573: example : StateResult (reinitialize (LifecycleWitnesses.moved) (⟨0⟩)) :=
ConcreteSpec/Proofs/Own.lean:4577: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (reinitializeUnissuedPlace (LifecycleWitnesses.moved) (⟨0⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4580: example : StateResult (borrow (shared) (⟨0⟩) ([.index 1]) (.shared)) :=
ConcreteSpec/Proofs/Own.lean:4584: example : (WellFormed (shared)) ∧ ¬ StateResult (borrowNoAdvanceSupply (shared) (⟨0⟩) ([.index 1]) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4587: example : StateResult (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) :=
ConcreteSpec/Proofs/Own.lean:4591: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowToDropParent (exclusive) (⟨0⟩) ([.index 1]) (.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4594: example : StateResult (reborrow (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) :=
ConcreteSpec/Proofs/Own.lean:4598: example : (WellFormed (exclusive)) ∧ ¬ StateResult (reborrowDropAncestors (exclusive) (⟨0⟩) ([.index 1]) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4601: example : StateResult (endLoan (child) (⟨1⟩)) :=
ConcreteSpec/Proofs/Own.lean:4605: example : (WellFormed (child)) ∧ ¬ StateResult (endLoanRemoveParent (child) (⟨1⟩)) := by decide
ConcreteSpec/Proofs/Own.lean:4608: example : StateResult (accessOwner (shared) (⟨0⟩) (.shared)) :=
ConcreteSpec/Proofs/Own.lean:4612: example : (WellFormed (shared)) ∧ ¬ StateResult (accessOwnerConsumeRoot (shared) (⟨0⟩) (.shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4615: example : StateResult (accessLoan (child) (⟨1⟩) (.mutable)) :=
ConcreteSpec/Proofs/Own.lean:4619: example : (WellFormed (child)) ∧ ¬ StateResult (accessLoanDropParent (child) (⟨1⟩) (.mutable)) := by decide
ConcreteSpec/Proofs/Own.lean:4622: example : StateResult (bind (types) (y) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4626: example : (WellFormed (shared)) ∧ ¬ StateResult (bindNoAdvancePlace (types) (y) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4629: example : StateResult (drop ([x]) (LifecycleWitnesses.moved)) :=
ConcreteSpec/Proofs/Own.lean:4633: example : (WellFormed (LifecycleWitnesses.moved)) ∧ ¬ StateResult (dropLocalOnly ([x]) (LifecycleWitnesses.moved)) := by decide
ConcreteSpec/Proofs/Own.lean:4636: example : StateResult (enter (types) ([y]) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4640: example : (WellFormed (shared)) ∧ ¬ StateResult (enterDuplicateFrameId (types) ([y]) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4643: example : StateResult (endLocals (1) (LifecycleWitnesses.called)) :=
ConcreteSpec/Proofs/Own.lean:4647: example : (WellFormed (LifecycleWitnesses.called)) ∧ ¬ StateResult (endLocalsCallerInstead (1) (LifecycleWitnesses.called)) := by decide
ConcreteSpec/Proofs/Own.lean:4650: example : StateResult (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4654: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ¬ StateResult (leaveKeepLocalLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4657: example : StateResult (enterDown (exclusive) ([(y, ⟨0⟩)])) :=
ConcreteSpec/Proofs/Own.lean:4661: example : (WellFormed (exclusive)) ∧ ¬ StateResult (enterDownReuseParentId (exclusive) ([(y, ⟨0⟩)])) := by decide
ConcreteSpec/Proofs/Own.lean:4664: example : StateResult (life (types) ((.bind y)) (shared)) :=
ConcreteSpec/Proofs/Own.lean:4668: example : (WellFormed (shared)) ∧ ¬ StateResult (lifeBindUnindexed (types) ((.bind y)) (shared)) := by decide
ConcreteSpec/Proofs/Own.lean:4671: example : StateResult (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
ConcreteSpec/Proofs/Own.lean:4675: example : (WellFormed (adoptionInput)) ∧ ¬ StateResult (adoptDownUnissuedActivation (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4678: example : CallOwnResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
ConcreteSpec/Proofs/Own.lean:4682: example : (WellFormed (⟨exclusive, []⟩ : CallState).own) ∧ ¬ CallOwnResult (passDownClearParent (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4685: example : CallResult (passDown (⟨exclusive, []⟩) (⟨0⟩) (y)) :=
ConcreteSpec/Proofs/Own.lean:4689: example : (CallState.WellFormed (⟨exclusive, []⟩)) ∧ ¬ CallResult (passDownOmitPending (⟨exclusive, []⟩) (⟨0⟩) (y)) := by decide
ConcreteSpec/Proofs/Own.lean:4692: example : CallOwnResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
ConcreteSpec/Proofs/Own.lean:4696: example : (WellFormed (CallWitnesses.waiting).own) ∧ ¬ CallOwnResult (callLifeUnissuedActivation (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4699: example : CallResult (callLife (types) ((.enter [y])) (CallWitnesses.waiting)) :=
ConcreteSpec/Proofs/Own.lean:4703: example : (CallState.WellFormed (CallWitnesses.waiting)) ∧ ¬ CallResult (callLifeKeepPending (types) ((.enter [y])) (CallWitnesses.waiting)) := by decide
ConcreteSpec/Proofs/Own.lean:4706: example : CallOwnResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4710: example : (WellFormed (DirectArgumentWitnesses.caller).own) ∧ ¬ CallOwnResult (passOwnerClearCallRoot (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4713: example : CallResult (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4717: example : (CallState.WellFormed (DirectArgumentWitnesses.caller)) ∧ ¬ CallResult (passOwnerOmitPending (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4746: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4750: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (WellFormed (RoundThreeWitnesses.localLoan)) ∧ (((y, Hybrid.Place.mk 1)) ∈ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).locals) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.root ≠ ((y, Hybrid.Place.mk 1)).2) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4755: example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4759: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4764: example : ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwner (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) :=
ConcreteSpec/Proofs/Own.lean:4768: example : ¬ ResultSatisfies (fun t : CallState => ownerAccess t.own (⟨1⟩) .mutable = false ∧ ((Mode.mutable) = .mutable → ownerAccess t.own (⟨1⟩) .shared = false)) (passOwnerNoFreeze (DirectArgumentWitnesses.caller) (⟨1⟩) ([.index 3]) (Mode.mutable) (DirectArgumentWitnesses.z)) := by decide
ConcreteSpec/Proofs/Own.lean:4773: example : ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4777: example : (WellFormed (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ (RoundThreeWitnesses.localLoan).loans) ∧ (((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)).activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) ∧ ¬ ResultSatisfies (fun t : State => ((Examples.shared.loans.headD ⟨⟨99⟩, ⟨99⟩, [], .shared, none, [], 99, false, false⟩)) ∈ t.loans) (leaveAllLoans (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4782: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4786: example : ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => ∀ l ∈ t.loans, l.activation ≠ (⟨1, [(y, ⟨1⟩)], []⟩ : Activation).id) (leaveIdentity (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4791: example : ResultSatisfies (fun t : State => LiveDestinations t) (reborrowTo (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) :=
ConcreteSpec/Proofs/Own.lean:4795: example : (LiveDestinations (exclusive)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (reborrowToDead (exclusive) (⟨0⟩) ([.index 1]) (Mode.mutable) (0)) := by decide
ConcreteSpec/Proofs/Own.lean:4800: example : ResultSatisfies (fun t : State => LiveDestinations t) (adoptDown (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) :=
ConcreteSpec/Proofs/Own.lean:4804: example : (LiveDestinations (adoptionInput)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (adoptDownDead (adoptionInput) (DirectArgumentWitnesses.waiting.pending)) := by decide
ConcreteSpec/Proofs/Own.lean:4809: example : ResultSatisfies (fun t : State => LiveDestinations t) (leave (RoundThreeWitnesses.localLoan)) :=
ConcreteSpec/Proofs/Own.lean:4813: example : (LiveDestinations (RoundThreeWitnesses.localLoan)) ∧ ((RoundThreeWitnesses.localLoan).frames.head? = some (⟨1, [(y, ⟨1⟩)], []⟩ : Activation)) ∧ ¬ ResultSatisfies (fun t : State => LiveDestinations t) (leaveStranded (RoundThreeWitnesses.localLoan)) := by decide
ConcreteSpec/Proofs/Own.lean:4947: theorem loan_has_owned_root (s : State) (p : Hybrid.Place) (path : List Projection)
ConcreteSpec/Proofs/Own.lean:4961: theorem moved_no_loan (s : State) (p : Hybrid.Place) (hs : WellFormed s) :
ConcreteSpec/Proofs/Own.lean:4973: theorem instrument_erasure {σ α : Type} (permit : State → Option State)
ConcreteSpec/Proofs/Own.lean:4982: theorem life_erasure {σ : Type} (types : Var → Option Descriptor)
ConcreteSpec/Proofs/Own.lean:4991: theorem callLife_erasure {σ : Type} (types : Var → Option Descriptor)
ConcreteSpec/Proofs/Own.lean:5000: theorem instrument_preserves {σ α : Type} (permit : State → Option State)
ConcreteSpec/Proofs/Own.lean:5010: theorem readOwner_erasure (tgt : Target) (p : Hybrid.Place) (s : Store Hybrid.State) :
ConcreteSpec/Proofs/Own.lean:5019: theorem writeOwner_erasure (tgt : Target) (p : Hybrid.Place) (v : Hybrid.Value)
ConcreteSpec/Proofs/Own.lean:5029: theorem readLoan_erasure (tgt : Target) (i : LoanId) (s : Store Hybrid.State) :
ConcreteSpec/Proofs/Own.lean:5039: theorem writeLoan_erasure (tgt : Target) (i : LoanId) (v : Hybrid.Value)
ConcreteSpec/Proofs/Own.lean:5050: theorem borrow_birth (s : State) (p : Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:5061: theorem reborrow_birth (s : State) (i : LoanId)
ConcreteSpec/Proofs/Own.lean:5073: theorem endLoan_supply (s : State) (i : LoanId) :
ConcreteSpec/Proofs/Own.lean:5081: theorem adoptDown_only_arguments (s : State) (args : List (Var × LoanId)) :
ConcreteSpec/Proofs/Own.lean:5113: theorem reborrowTo_current (s : State) (i : LoanId) (suffix : List Projection)
ConcreteSpec/Proofs/Own.lean:5131: theorem passOwner_birth (s : CallState) (p : Hybrid.Place)
ConcreteSpec/Proofs/Own.lean:5414: example : ResultSatisfies (fun t : State => ∀ l ∈ t.loans, ∃ x ∈ t.slots, x.place = l.root ∧ x.status = .owned)
ConcreteSpec/Proofs/Own.lean:5419: example : WellFormed initial ∧
ConcreteSpec/Proofs/Own.lean:5432: example : ResultSatisfies (fun t : State => ∀ x ∈ t.slots, x.status = .moved → ∀ l ∈ t.loans, x.place ≠ l.root)
ConcreteSpec/Proofs/Own.lean:5437: example : WellFormed moveWithOtherLoan ∧
ConcreteSpec/Proofs/Own.lean:5442: example : ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
ConcreteSpec/Proofs/Own.lean:5447: example : ¬ ResultSatisfies (fun r : Int × Store Int => incIntOp (erase intStore0) = some (r.1, erase r.2))
ConcreteSpec/Proofs/Own.lean:5451: example : ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
ConcreteSpec/Proofs/Own.lean:5456: example : ¬ ResultSatisfies (fun t : Store Int => erase t = erase intStore0 ∧ life types (.bind y) intStore0.own = some t.own)
ConcreteSpec/Proofs/Own.lean:5460: example : ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
ConcreteSpec/Proofs/Own.lean:5465: example : ¬ ResultSatisfies (fun t : Int × CallState => t.1 = intCallStore0.1 ∧ callLife types (.enter [y]) intCallStore0.2 = some t.2)
ConcreteSpec/Proofs/Own.lean:5469: example : ResultSatisfies (fun r : Int × Store Int => WellFormed r.2.own)
ConcreteSpec/Proofs/Own.lean:5476: example : (∀ s t, WellFormed s → sharedPermit s = some t → WellFormed t) ∧
ConcreteSpec/Proofs/Own.lean:5483: example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5489: example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5497: example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5503: example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5512: example : ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5520: example : ¬ ResultSatisfies (fun r : Hybrid.Value × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5530: example : ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5537: example : ¬ ResultSatisfies (fun r : Unit × Store Hybrid.State =>
ConcreteSpec/Proofs/Own.lean:5550: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5557: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5563: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5571: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5583: example : ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
ConcreteSpec/Proofs/Own.lean:5588: example : ¬ ResultSatisfies (fun t : State => t.nextLoan = shared.nextLoan)
ConcreteSpec/Proofs/Own.lean:5592: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5600: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5607: example : ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5614: example : ¬ ResultSatisfies (fun t : State =>
ConcreteSpec/Proofs/Own.lean:5620: example : ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:5629: example : ¬ ResultSatisfies (fun t : CallState =>
ConcreteSpec/Proofs/Own.lean:5999: theorem loanSequence_retired_step (s : State) (op : LoanAction) (i : LoanId)
ConcreteSpec/Proofs/Own.lean:6049: theorem ended_loans_never_revive (s : State) (ops : List LoanAction) (i : LoanId)
ConcreteSpec/Proofs/Own.lean:6084: example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩)
ConcreteSpec/Proofs/Own.lean:6089: example : RetiredLoan endedChild ⟨1⟩ ∧
ConcreteSpec/Proofs/Own.lean:6098: example : ResultSatisfies (fun t => RetiredLoan t ⟨1⟩) (runLoanSequence endedChild actions) :=
ConcreteSpec/Proofs/Own.lean:6102: example : RetiredLoan endedChild ⟨1⟩ ∧
```
