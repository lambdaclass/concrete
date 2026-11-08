import ConcreteSpec.Spec.Prim

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

end ConcreteSpec.Core
