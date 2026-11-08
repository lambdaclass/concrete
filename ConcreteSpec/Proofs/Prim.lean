import ConcreteSpec.Spec.Prim
import ConcreteSpec.Proofs.Exec

/-!
# Laws of identities, primitive contracts, evidence and ghost erasure

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Core
namespace Prim

/-- Lookup after renaming an environment and its numeric key is the original lookup. -/
theorem lookupId_rename (r : Renaming) : ∀ (L : AList Var Int) (n : Nat),
    lookupId (renameLocals r L) (r.toFun n) = lookupId L n
  | [], _ => rfl
  | (x, v) :: L, n => by
    by_cases h : x.id = n
    · subst n
      simp [lookupId, renameLocals, renameVar]
    · have hr : r.toFun x.id ≠ r.toFun n := by
        intro he
        apply h
        calc
          x.id = r.inv (r.toFun x.id) := (r.inv_toFun _).symm
          _ = r.inv (r.toFun n) := congrArg r.inv he
          _ = n := r.inv_toFun _
      simp [lookupId, renameLocals, renameVar, h, hr]
      exact lookupId_rename r L n

/-- **Renaming preserves evaluation.** A bijective, binder-aware renaming of a term and its locals
changes neither a value, a trap, nor unsafe stuckness. -/
theorem eval_rename (r : Renaming) (L : AList Var Int) (e : Term) :
    Term.eval (renameLocals r L) (renameTerm r e) = Term.eval L e := by
  induction e generalizing L with
  | lit n => rfl
  | var x =>
    simp only [renameTerm, renameVar, Term.eval]
    rw [lookupId_rename]
  | copy x =>
    simp only [renameTerm, renameVar, Term.eval]
    rw [lookupId_rename]
  | move x =>
    simp only [renameTerm, renameVar, Term.eval]
    rw [lookupId_rename]
  | add a b iha ihb => simp [renameTerm, Term.eval, iha, ihb]
  | bind x value body ihv ihb =>
    simp only [renameTerm, Term.eval]
    rw [ihv]
    cases Term.eval L value with
    | ok v => simpa [Res.bind, renameLocals] using ihb ((x, v) :: L)
    | trap k => rfl
    | stuck => rfl

/-- The same law through C1's `Operands.eval`. -/
theorem eval_operands_rename (r : Renaming) (L : AList Var Int) (st : Unit) (e : Term) :
    operands.eval (renameLocals r L) st (renameTerm r e) = operands.eval L st e := by
  simp [operands, eval_rename, Res.map]

/-- **A bijective renaming is invertible.** Applying its inverse recovers the original term. -/
theorem rename_inverse (r : Renaming) (e : Term) : renameTerm r.symm (renameTerm r e) = e := by
  induction e with
  | lit n => rfl
  | var x | copy x | move x => simp [renameTerm, renameVar, Renaming.symm, r.inv_toFun]
  | add a b iha ihb => simp [renameTerm, iha, ihb]
  | bind x value body ihv ihb =>
    have hx : renameVar r.symm (renameVar r x) = x := by
      simp [renameVar, Renaming.symm, r.inv_toFun]
    simp only [renameTerm, hx, ihv, ihb]

/-- **Identity renaming is the identity.** -/
theorem rename_id (e : Term) : renameTerm .id e = e := by
  induction e with
  | lit n => rfl
  | var x | copy x | move x => rfl
  | add a b iha ihb => simp [renameTerm, iha, ihb]
  | bind x value body ihv ihb =>
    have hx : renameVar .id x = x := rfl
    simp only [renameTerm, hx, ihv, ihb]

/-- Lookup is invariant under changing every hint consistently: only the numeric key is read. -/
theorem lookupId_rehint (h : Var → String) : ∀ (L : AList Var Int) (n : Nat),
    lookupId (rehintLocals h L) n = lookupId L n
  | [], _ => rfl
  | (x, v) :: L, n => by
    simp only [rehintLocals, List.map_cons, lookupId, rehintVar]
    by_cases hx : x.id = n
    · simp [hx]
    · simp only [hx, ↓reduceIte]
      exact lookupId_rehint h L n

/-- **Names are presentation only.** Changing every `Var.hint`, in a term and its locals, without
changing any numeric identity changes no evaluation result. -/
theorem eval_rehint (h : Var → String) (L : AList Var Int) (e : Term) :
    Term.eval (rehintLocals h L) (rehintTerm h e) = Term.eval L e := by
  induction e generalizing L with
  | lit n => rfl
  | var x =>
    simp only [rehintTerm, rehintVar, Term.eval]
    rw [lookupId_rehint]
  | copy x =>
    simp only [rehintTerm, rehintVar, Term.eval]
    rw [lookupId_rehint]
  | move x =>
    simp only [rehintTerm, rehintVar, Term.eval]
    rw [lookupId_rehint]
  | add a b iha ihb => simp [rehintTerm, Term.eval, iha, ihb]
  | bind x value body ihv ihb =>
    simp only [rehintTerm, Term.eval]
    rw [ihv]
    cases Term.eval L value with
    | ok v => simpa [Res.bind, rehintLocals] using ihb ((x, v) :: L)
    | trap k => rfl
    | stuck => rfl

/-- Rehinting also preserves C1's operand evaluation. -/
theorem eval_operands_rehint (h : Var → String) (L : AList Var Int) (st : Unit) (e : Term) :
    operands.eval (rehintLocals h L) st (rehintTerm h e) = operands.eval L st e := by
  simp [operands, eval_rehint, Res.map]

end Prim

/-- Evidence is usable in its own exact context. -/
theorem SafetyEvidence.usable_current (e : SafetyEvidence) : e.Usable e.context := rfl

/-- Changing the typed obligation identity invalidates evidence. -/
theorem SafetyEvidence.obligation_mismatch (e : SafetyEvidence) (current : EvidenceContext)
    (h : e.context.obligation ≠ current.obligation) : ¬ e.Usable current := by
  intro hu
  exact h (congrArg EvidenceContext.obligation hu)

/-- Changing the target invalidates evidence. -/
theorem SafetyEvidence.target_mismatch (e : SafetyEvidence) (current : EvidenceContext)
    (h : e.context.target ≠ current.target) : ¬ e.Usable current := by
  intro hu
  exact h (congrArg EvidenceContext.target hu)

/-- Changing the primitive contract or its revision invalidates evidence. -/
theorem SafetyEvidence.contract_mismatch (e : SafetyEvidence) (current : EvidenceContext)
    (h : e.context.contract ≠ current.contract) : ¬ e.Usable current := by
  intro hu
  exact h (congrArg EvidenceContext.contract hu)

/-- Changing the checker version invalidates evidence. -/
theorem SafetyEvidence.checker_mismatch (e : SafetyEvidence) (current : EvidenceContext)
    (h : e.context.checker ≠ current.checker) : ¬ e.Usable current := by
  intro hu
  exact h (congrArg EvidenceContext.checker hu)

/-- Changing the specification version invalidates evidence. -/
theorem SafetyEvidence.spec_mismatch (e : SafetyEvidence) (current : EvidenceContext)
    (h : e.context.spec ≠ current.spec) : ¬ e.Usable current := by
  intro hu
  exact h (congrArg EvidenceContext.spec hu)

/-- An unregistered primitive has no allowed answer. -/
theorem PrimRegistry.allows_unknown {Val Ty Store Event : Type} (R : PrimRegistry Val Ty Store Event)
    (p : PrimId) (hnone : AList.lookup R.entries p = none) (args : List Val) (st : Store)
    (a : Exec.Answer Val Event Store) : R.allows p args st a = false := by
  simp [PrimRegistry.allows, hnone]

/-- **Unknown primitives are stuck.** A C1 request for a primitive absent from the registry has no
small-step transition for any answer: the machine never invents a catch-all contract. -/
theorem PrimRegistry.unknown_primitive_stuck {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (P : Exec.Program (R.operands O))
    {c : Exec.Config (R.operands O)} {p : PrimId} {vs : List O.Val} {site : SiteId} {st : O.Store}
    {k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config (R.operands O))}
    (hstep : Exec.step P c = some (.request p vs site st k))
    (hnone : AList.lookup R.entries p = none) (l : Exec.Label (R.operands O))
    (c' : Exec.Config (R.operands O)) : ¬ Exec.Step P c l c' := by
  intro hs
  cases hs with
  | silent h => rw [hstep] at h; cases h
  | prim h ha hk =>
    rw [hstep] at h
    cases h
    simp [PrimRegistry.operands, PrimRegistry.allows, hnone] at ha

/-- Every answer accepted through a registered primitive satisfies that primitive's complete
contract, via C1's `allows` check. -/
theorem PrimRegistry.allows_allowed {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (p : PrimId)
    (c : PrimContract O.Val O.Ty O.Store O.Event) (hc : AList.lookup R.entries p = some c)
    (args : List O.Val) (st : O.Store) (a : Exec.Answer O.Val O.Event O.Store)
    (ha : (R.operands O).allows p args st a = true) : c.Allowed args st a := by
  have ha' : c.answerOk args st a = true := by
    simpa [PrimRegistry.operands, PrimRegistry.allows, hc] using ha
  exact (R.answerOk_iff p c hc args st a).1 ha'

/-- **Accepted answers satisfy the postcondition.** The predicate observes arguments, the result
and the surviving store; no entry-state snapshot is available. -/
theorem PrimRegistry.allows_postcondition {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (p : PrimId)
    (c : PrimContract O.Val O.Ty O.Store O.Event) (hc : AList.lookup R.entries p = some c)
    (args : List O.Val) (st : O.Store) (a : Exec.Answer O.Val O.Event O.Store)
    (ha : (R.operands O).allows p args st a = true) :
    match a.result with
    | .ok v => c.ensures args v a.store
    | .error _ => False := by
  have h := R.allows_allowed p c hc args st a ha
  have hm := h.2.2.2.2
  cases hr : a.result with
  | error kd => simp [hr] at hm
  | ok v =>
    rw [hr] at hm
    exact hm.1

/-- An accepted answer is always an ordinary result value, never an implicit trap. -/
theorem PrimRegistry.allows_no_implicit_trap {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (p : PrimId)
    (c : PrimContract O.Val O.Ty O.Store O.Event) (hc : AList.lookup R.entries p = some c)
    (args : List O.Val) (st : O.Store) (a : Exec.Answer O.Val O.Event O.Store)
    (ha : (R.operands O).allows p args st a = true) : ∃ v, a.result = .ok v := by
  have hp := R.allows_postcondition p c hc args st a ha
  cases hr : a.result with
  | ok v => exact ⟨v, rfl⟩
  | error kd => simp [hr] at hp

namespace Ghost
variable {O : Exec.Operands}

/-- **Concrete-step soundness.** Every run of `drive` has a C1 `Steps` derivation from its initial
to its retained final configuration, emitting exactly the events added to the history. -/
theorem drive_steps (P : Exec.Program O) (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) (r : SegmentReport O),
      drive P ρ fuel hist c = r → ∃ tr, r.events = hist ++ tr ∧ Exec.Steps P c tr r.config := by
  intro fuel
  induction fuel with
  | zero =>
    intro hist c r h
    cases hf : c.final? with
    | some f =>
      simp [drive, hf] at h
      cases h
      exact ⟨[], by simp, .refl _⟩
    | none =>
      simp [drive, hf] at h
      cases h
      exact ⟨[], by simp, .refl _⟩
  | succ n ih =>
    intro hist c r h
    cases hf : c.final? with
    | some f =>
      simp [drive, hf] at h
      cases h
      exact ⟨[], by simp, .refl _⟩
    | none =>
      cases hs : Exec.step P c with
      | none =>
        simp [drive, hf, hs] at h
        cases h
        exact ⟨[], by simp, .refl _⟩
      | some out =>
        cases out with
        | next c' =>
          simp [drive, hf, hs] at h
          obtain ⟨tr, he, hc⟩ := ih hist c' r h
          exact ⟨tr, he, Exec.steps_next hs hc⟩
        | request p vs site st k =>
          cases ha : ρ hist p vs site st with
          | none =>
            simp [drive, hf, hs, ha] at h
            cases h
            exact ⟨[], by simp, .refl _⟩
          | some a =>
            by_cases hallow : O.allows p vs st a = true
            · cases hk : k a with
              | none =>
                simp [drive, hf, hs, ha, hallow, hk] at h
                cases h
                exact ⟨[], by simp, .refl _⟩
              | some c' =>
                simp [drive, hf, hs, ha, hallow, hk] at h
                obtain ⟨tr, he, hc⟩ := ih (hist ++ a.event.toList) c' r h
                refine ⟨a.label.events ++ tr, ?_, Exec.Steps.step (.prim hs hallow hk) hc⟩
                cases hev : a.event with
                | none =>
                  simpa [Exec.Answer.label, Exec.Label.events, hev, List.append_assoc] using he
                | some ev =>
                  simpa [Exec.Answer.label, Exec.Label.events, hev, List.append_assoc] using he
            · simp [drive, hf, hs, ha, hallow] at h
              cases h
              exact ⟨[], by simp, .refl _⟩

/-- Once a configuration is not ordinarily completed, no later statement restarts it. -/
theorem runStmts_of_not_normal (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ss : List (Exec.Stmt O)) (c : Exec.Config O)
    (hf : c.final? ≠ some .normal) : runStmts P ρ fuel hist ss c = drive P ρ fuel hist c := by
  cases ss with
  | nil => rfl
  | cons s ss =>
    cases h : c.final? with
    | none => simp [runStmts, h]
    | some f => cases f <;> simp [runStmts, h] at hf ⊢

/-- **Ghost erasure preserves concrete execution.** Removing every ghost statement leaves the same
complete report: identical events, C1 `Ending`, final configuration and remaining fuel. Ghost
computations may update ghost locals and read the concrete configuration, but cannot change it. -/
theorem ghost_erasure_run (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O) :
    ∀ (fuel : Nat) (hist : List O.Event) (ps : List (Piece G)) (Γ : AList Var G.Val)
      (c : Exec.Config O), runPieces G P ρ fuel hist ps Γ c =
        runStmts P ρ fuel hist (erase ps) c := by
  intro fuel hist ps
  induction ps generalizing fuel hist with
  | nil => intro Γ c; rfl
  | cons piece ps ih =>
    intro Γ c
    cases piece with
    | ghost g =>
      by_cases hn : c.final? = some .normal
      · have h := ih fuel hist (evalStmt G c.locals c.store g Γ) c
        simpa [runPieces, erase, erasePiece, List.filterMap, hn] using h
      · simp [runPieces, erase, erasePiece, List.filterMap, hn, runStmts_of_not_normal]
    | concrete s =>
      by_cases hn : c.final? = some .normal
      · cases hr : drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩ with
        | mk events ending config remaining =>
          cases ending with
          | final f =>
            cases f with
            | normal =>
              have h := ih remaining events Γ config
              simpa [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr] using h
            | returned v => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
            | trapped k site => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
          | stuck c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
          | noAnswer c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
          | badAnswer c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
          | outOfFuel c' => simp [runPieces, runStmts, erase, erasePiece, List.filterMap, hn, hr]
      · simp [runPieces, erase, erasePiece, List.filterMap, hn, runStmts_of_not_normal]

/-- In particular, ghost erasure preserves public events and the exact C1 ending classification. -/
theorem ghost_erasure_ending (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ps : List (Piece G)) (Γ : AList Var G.Val)
    (c : Exec.Config O) :
    (runPieces G P ρ fuel hist ps Γ c).events = (runStmts P ρ fuel hist (erase ps) c).events ∧
    (runPieces G P ρ fuel hist ps Γ c).ending = (runStmts P ρ fuel hist (erase ps) c).ending := by
  rw [ghost_erasure_run]
  exact ⟨rfl, rfl⟩

end Ghost
end ConcreteSpec.Core
