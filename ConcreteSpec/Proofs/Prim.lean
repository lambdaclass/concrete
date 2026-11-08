import ConcreteSpec.Spec.Prim
import ConcreteSpec.Proofs.Exec

/-!
# Laws of identities, primitive contracts, evidence and ghost erasure

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Core
namespace Prim

/-- Law (subject `ConcreteSpec.Core.Prim.lookupId`, `ConcreteSpec.Core.Prim.renameLocals`).
Lookup after renaming an environment and its numeric key is the original lookup. -/
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

/-- Law (subject `ConcreteSpec.Core.Prim.Term.eval`, `ConcreteSpec.Core.Prim.renameTerm`, `ConcreteSpec.Core.Prim.renameLocals`).
**Renaming preserves evaluation.** A bijective, binder-aware renaming of a term and its locals
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

/-- Law (subject `ConcreteSpec.Core.Prim.operands`, `ConcreteSpec.Core.Prim.renameTerm`, `ConcreteSpec.Core.Prim.renameLocals`).
The same renaming law through C1's `Exec.Operands.eval`. -/
theorem eval_operands_rename (r : Renaming) (L : AList Var Int) (st : Unit) (e : Term) :
    operands.eval (renameLocals r L) st (renameTerm r e) = operands.eval L st e := by
  simp [operands, eval_rename, Res.map]

/-- Law (subject `ConcreteSpec.Core.Prim.renameTerm`, `ConcreteSpec.Core.Prim.Renaming.symm`).
**A bijective renaming is invertible.** Applying its inverse recovers the original term. -/
theorem rename_inverse (r : Renaming) (e : Term) : renameTerm r.symm (renameTerm r e) = e := by
  induction e with
  | lit n => rfl
  | var x | copy x | move x => simp [renameTerm, renameVar, Renaming.symm, r.inv_toFun]
  | add a b iha ihb => simp [renameTerm, iha, ihb]
  | bind x value body ihv ihb =>
    have hx : renameVar r.symm (renameVar r x) = x := by
      simp [renameVar, Renaming.symm, r.inv_toFun]
    simp only [renameTerm, hx, ihv, ihb]

/-- Law (subject `ConcreteSpec.Core.Prim.renameTerm`, `ConcreteSpec.Core.Prim.Renaming.id`).
**Identity renaming is the identity.** -/
theorem rename_id (e : Term) : renameTerm .id e = e := by
  induction e with
  | lit n => rfl
  | var x | copy x | move x => rfl
  | add a b iha ihb => simp [renameTerm, iha, ihb]
  | bind x value body ihv ihb =>
    have hx : renameVar .id x = x := rfl
    simp only [renameTerm, hx, ihv, ihb]

/-- Law (subject `ConcreteSpec.Core.Prim.lookupId`, `ConcreteSpec.Core.Prim.rehintLocals`).
Lookup is invariant under changing every hint consistently: only the numeric key is read. -/
theorem lookupId_rehint (h : Var → String) : ∀ (L : AList Var Int) (n : Nat),
    lookupId (rehintLocals h L) n = lookupId L n
  | [], _ => rfl
  | (x, v) :: L, n => by
    simp only [rehintLocals, List.map_cons, lookupId, rehintVar]
    by_cases hx : x.id = n
    · simp [hx]
    · simp only [hx, ↓reduceIte]
      exact lookupId_rehint h L n

/-- Law (subject `ConcreteSpec.Core.Prim.Term.eval`, `ConcreteSpec.Core.Prim.rehintTerm`, `ConcreteSpec.Core.Prim.rehintLocals`).
**Names are presentation only.** Changing every `Var.hint`, in a term and its locals, without
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

/-- Law (subject `ConcreteSpec.Core.Prim.operands`, `ConcreteSpec.Core.Prim.rehintTerm`, `ConcreteSpec.Core.Prim.rehintLocals`).
Rehinting also preserves C1's operand evaluation. -/
theorem eval_operands_rehint (h : Var → String) (L : AList Var Int) (st : Unit) (e : Term) :
    operands.eval (rehintLocals h L) st (rehintTerm h e) = operands.eval L st e := by
  simp [operands, eval_rehint, Res.map]

end Prim

/-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
The output of evidence consumption names the requested registered obligation in the current context. -/
theorem SafetyEvidence.discharge_sound {claim : ObligationId → Prop} (e : SafetyEvidence claim)
    (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId) :
    match e.discharge obligations current requested with
    | none => True
    | some discharged => e.Usable current ∧ current.obligation = requested ∧
        ObligationTable.lookup obligations requested = some discharged.obligation ∧
        discharged.obligation.id = requested := by
  have sound : ∀ discharged : DischargedFact claim,
      e.discharge obligations current requested = some discharged →
      e.Usable current ∧ current.obligation = requested ∧
        ObligationTable.lookup obligations requested = some discharged.obligation ∧
        discharged.obligation.id = requested := by
    intro discharged success
    by_cases usable : e.Usable current
    · by_cases identity : current.obligation = requested
      · unfold SafetyEvidence.discharge at success
        simp only [usable, identity, ↓reduceDIte] at success
        split at success
        · cases success
        · rename_i obligation found
          simp only [Option.some.injEq] at success
          subst discharged
          exact ⟨usable, identity, found,
            ObligationTable.lookup_id obligations requested obligation found⟩
      · simp [SafetyEvidence.discharge, usable, identity] at success
    · simp [SafetyEvidence.discharge, usable] at success
  cases result : e.discharge obligations current requested with
  | none => trivial
  | some discharged => exact sound discharged result

/-- Helper (computation). Computes the context-guard refusal branch. The public output contract carries soundness evidence.
(approved: PENDING). -/
theorem SafetyEvidence.discharge_context_mismatch {claim : ObligationId → Prop}
    (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
    (requested : ObligationId) (mismatch : ¬ e.Usable current) :
    e.discharge obligations current requested = none := by
  simp [SafetyEvidence.discharge, mismatch]

/-- Helper (computation). Computes the requested-identity guard branch; this guard equation is not independent soundness evidence.
(approved: PENDING). -/
theorem SafetyEvidence.discharge_obligation_mismatch {claim : ObligationId → Prop}
    (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
    (requested : ObligationId) (mismatch : current.obligation ≠ requested) :
    e.discharge obligations current requested = none := by
  by_cases usable : e.Usable current
  · simp [SafetyEvidence.discharge, usable, mismatch]
  · simp [SafetyEvidence.discharge, usable]

/-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
Evidence for one obligation identity cannot discharge a retrieved obligation with a different
identity, even when both obligations' propositions hold. -/
theorem SafetyEvidence.discharge_target_mismatch {claim : ObligationId → Prop}
    (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
    (requested : ObligationId) (obligation : SafetyObligation)
    (lookup : ObligationTable.lookup obligations requested = some obligation)
    (mismatch : e.context.obligation ≠ obligation.id) :
    e.discharge obligations current requested = none := by
  by_cases usable : e.Usable current
  · by_cases identity : current.obligation = requested
    · exfalso
      apply mismatch
      have found := ObligationTable.lookup_id obligations requested obligation lookup
      have ctxObl : e.context.obligation = current.obligation :=
        congrArg EvidenceContext.obligation usable
      exact (ctxObl.trans identity).trans found.symm
    · simp [SafetyEvidence.discharge, usable, identity]
  · simp [SafetyEvidence.discharge, usable]

/-- Law (subject `ConcreteSpec.Core.SafetyEvidence.discharge`).
An obligation absent from the authoritative table cannot be discharged. -/
theorem SafetyEvidence.discharge_unregistered {claim : ObligationId → Prop}
    (e : SafetyEvidence claim) (obligations : ObligationTable) (current : EvidenceContext)
    (requested : ObligationId) (absent : ObligationTable.lookup obligations requested = none) :
    e.discharge obligations current requested = none := by
  by_cases usable : e.Usable current
  · by_cases identity : current.obligation = requested
    · unfold SafetyEvidence.discharge
      simp only [usable, identity, ↓reduceDIte]
      split
      · rfl
      · rename_i obligation hlookup
        rw [absent] at hlookup
        cases hlookup
    · simp [SafetyEvidence.discharge, usable, identity]
  · simp [SafetyEvidence.discharge, usable]

/-- Law (subject `ConcreteSpec.Core.PrimRegistry.allows`).
An unregistered primitive has no allowed answer. -/
theorem PrimRegistry.allows_unknown {Val Ty Store Event : Type} (R : PrimRegistry Val Ty Store Event)
    (p : PrimId) (hnone : AList.lookup R.entries p = none) (args : List Val) (st : Store)
    (a : Exec.Answer Val Event Store) : R.allows p args st a = false := by
  simp [PrimRegistry.allows, hnone]

/-- Law (subject `ConcreteSpec.Core.PrimRegistry.operands`, `ConcreteSpec.Core.Exec.Step`).
**Unknown primitives are stuck.** A C1 request for a primitive absent from the registry has no
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

/-- Helper (projection). Unpacks the registry's `answerOk_iff` completeness field through
`PrimRegistry.operands` and `PrimRegistry.allows`. (approved: REVIEW5-codex). -/
theorem PrimRegistry.allows_allowed {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (p : PrimId)
    (c : PrimContract O.Val O.Ty O.Store O.Event) (hc : AList.lookup R.entries p = some c)
    (args : List O.Val) (st : O.Store) (a : Exec.Answer O.Val O.Event O.Store)
    (ha : (R.operands O).allows p args st a = true) : c.Allowed args st a := by
  have ha' : c.answerOk args st a = true := by
    simpa [PrimRegistry.operands, PrimRegistry.allows, hc] using ha
  exact (R.answerOk_iff p c hc args st a).1 ha'

/-- Helper (projection). Projects the postcondition or declared-failure disjunction from
`PrimRegistry.allows_allowed`. (approved: REVIEW5-codex). -/
theorem PrimRegistry.allows_postcondition {O : Exec.Operands}
    (R : PrimRegistry O.Val O.Ty O.Store O.Event) (p : PrimId)
    (c : PrimContract O.Val O.Ty O.Store O.Event) (hc : AList.lookup R.entries p = some c)
    (args : List O.Val) (st : O.Store) (a : Exec.Answer O.Val O.Event O.Store)
    (ha : (R.operands O).allows p args st a = true) :
    match a.result with
    | .ok v => c.ensures v a.store ∨ ∃ f ∈ c.failures, f.errorTy = c.result ∧ f.holds v
    | .error _ => False := by
  have h := R.allows_allowed p c hc args st a ha
  have hm := h.2.2.2.2
  cases hr : a.result with
  | error kd => simp [hr] at hm
  | ok v =>
    rw [hr] at hm
    exact hm.1

/-- Helper (projection). Projects the absence of implicit traps from
`PrimRegistry.allows_postcondition`. (approved: REVIEW5-codex). -/
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

/-- Law (subject `ConcreteSpec.Core.Ghost.drive`).
**Concrete-step soundness.** The retained configuration is reached by C1 steps, with exactly
 the events appended to the incoming history. The induction establishes the contract here. -/
theorem drive_steps_result (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    ∃ tr, (drive P ρ fuel hist c).events = hist ++ tr ∧
      Exec.Steps P c tr (drive P ρ fuel hist c).config := by
  have sound : ∀ (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) (r : SegmentReport O),
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
  exact sound fuel hist c _ rfl

/-- Helper (computation). Exposes the non-normal guard branch for the erasure induction. The terminal report and drive-step laws carry execution evidence.
(approved: PENDING). -/
theorem runStmts_of_not_normal (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ss : List (Exec.Stmt O)) (c : Exec.Config O)
    (hf : c.final? ≠ some .normal) : runStmts P ρ fuel hist ss c = drive P ρ fuel hist c := by
  cases ss with
  | nil => rfl
  | cons s ss =>
    cases h : c.final? with
    | none => simp [runStmts, h]
    | some f => cases f <;> simp [runStmts, h] at hf ⊢

/-- Law (subject `ConcreteSpec.Core.Ghost.runStmts`).
**A non-normal configuration has no later transition.** Running trailing statements from a
returned or trapped configuration emits no event and preserves the exact ending, configuration and
fuel; no statement body is entered. -/
theorem runStmts_terminal (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ss : List (Exec.Stmt O)) (c : Exec.Config O)
    (f : Exec.Final O.Val) (hf : c.final? = some f) (hn : f ≠ .normal) :
    runStmts P ρ fuel hist ss c = ⟨hist, .final f, c, fuel⟩ := by
  cases ss with
  | nil => simp [runStmts, drive, hf]
  | cons s ss => cases f <;> simp [runStmts, drive, hf] at hn ⊢

/-- Law (subject `ConcreteSpec.Core.Ghost.runPieces`, `ConcreteSpec.Core.Ghost.erase`).
**Ghost erasure preserves concrete execution.** Removing every ghost statement leaves the same
complete report: identical events, C1 `Exec.Ending`, final configuration and remaining fuel. Ghost
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

/-- Law (subject `ConcreteSpec.Core.Ghost.runPieces`, `ConcreteSpec.Core.Ghost.erase`).
In particular, ghost erasure preserves public events and the exact C1 ending classification. -/
theorem ghost_erasure_ending (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ps : List (Piece G)) (Γ : AList Var G.Val)
    (c : Exec.Config O) :
    (runPieces G P ρ fuel hist ps Γ c).events = (runStmts P ρ fuel hist (erase ps) c).events ∧
    (runPieces G P ρ fuel hist ps Γ c).ending = (runStmts P ρ fuel hist (erase ps) c).ending := by
  rw [ghost_erasure_run]
  exact ⟨rfl, rfl⟩

end Ghost

/-! ## Non-vacuity witnesses

Each law above has a closed instance that applies the theorem directly, paired with a planted
counter-example that substitutes a mutant of the same type for the law's subject and falsifies the
conclusion under the law's hypotheses. -/

namespace Vacuity
open Prim

def x0 : Var := ⟨"x", 0⟩
def x1 : Var := ⟨"x", 1⟩

/-- Swap two numeric local identities and leave every other identity unchanged. -/
def swapNat : Nat → Nat
  | 0 => 1
  | 1 => 0
  | n => n

def swap : Renaming where
  toFun := swapNat
  inv := swapNat
  toFun_inv := fun
    | 0 | 1 | _ + 2 => rfl
  inv_toFun := fun
    | 0 | 1 | _ + 2 => rfl

def locals : AList Var Int := [(x0, 10), (x1, 3)]
def source : Term := .bind x0 (.lit 2) (.add (.var x0) (.copy x1))
def hints (_ : Var) : String := "renamed"

/-- Mutant environment renamer that leaves local keys unrenamed. -/
def unrenamedLocals (_ : Renaming) (L : AList Var Int) : AList Var Int := L

/-- Mutant inverse that returns the identity renaming instead of `r.symm`. -/
def idSymm (_ : Renaming) : Renaming := .id

/-- Mutant local rehinting that also shifts numeric identities. -/
def corruptIdRehintLocals (h : Var → String) (L : AList Var Int) : AList Var Int :=
  List.map (fun e => (⟨h e.1, e.1.id + 1⟩, e.2)) L

/-- Mutant term rehinting that also swaps numeric binder identities. -/
def corruptIdRehintTerm (h : Var → String) (e : Term) : Term :=
  renameTerm swap (rehintTerm h e)

/-- Instance (law `ConcreteSpec.Core.Prim.lookupId_rename`, case `ConcreteSpec.Core.Vacuity.locals`). -/
example : lookupId (renameLocals swap locals) (swap.toFun 1) = lookupId locals 1 :=
  lookupId_rename swap locals 1

/-- Planted (law `ConcreteSpec.Core.Prim.lookupId_rename`, case `ConcreteSpec.Core.Vacuity.locals`, subject `ConcreteSpec.Core.Prim.renameLocals`, mutant `ConcreteSpec.Core.Vacuity.unrenamedLocals`). -/
example : ¬ (lookupId (unrenamedLocals swap locals) (swap.toFun 1) = lookupId locals 1) := by decide

/-- Instance (law `ConcreteSpec.Core.Prim.eval_rename`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : Term.eval (renameLocals swap locals) (renameTerm swap source) = Term.eval locals source :=
  eval_rename swap locals source

/-- Planted (law `ConcreteSpec.Core.Prim.eval_rename`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.renameLocals`, mutant `ConcreteSpec.Core.Vacuity.unrenamedLocals`). -/
example : ¬ (Term.eval (unrenamedLocals swap locals) (renameTerm swap source) = Term.eval locals source) := by
  decide

/-- Instance (law `ConcreteSpec.Core.Prim.eval_operands_rename`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : operands.eval (renameLocals swap locals) () (renameTerm swap source) =
    operands.eval locals () source :=
  eval_operands_rename swap locals () source

/-- Planted (law `ConcreteSpec.Core.Prim.eval_operands_rename`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.renameLocals`, mutant `ConcreteSpec.Core.Vacuity.unrenamedLocals`). -/
example : ¬ ((operands.eval (unrenamedLocals swap locals) () (renameTerm swap source) : Res (Int × Unit)) =
    operands.eval locals () source) := by
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Prim.rename_inverse`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : renameTerm swap.symm (renameTerm swap source) = source :=
  rename_inverse swap source

/-- Planted (law `ConcreteSpec.Core.Prim.rename_inverse`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.Renaming.symm`, mutant `ConcreteSpec.Core.Vacuity.idSymm`). -/
example : ¬ (renameTerm (idSymm swap) (renameTerm swap source) = source) := by decide

/-- Instance (law `ConcreteSpec.Core.Prim.rename_id`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : renameTerm .id source = source :=
  rename_id source

/-- Planted (law `ConcreteSpec.Core.Prim.rename_id`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.Renaming.id`, mutant `ConcreteSpec.Core.Vacuity.swap`). -/
example : ¬ (renameTerm swap source = source) := by decide

/-- Instance (law `ConcreteSpec.Core.Prim.lookupId_rehint`, case `ConcreteSpec.Core.Vacuity.locals`). -/
example : lookupId (rehintLocals hints locals) 1 = lookupId locals 1 :=
  lookupId_rehint hints locals 1

/-- Planted (law `ConcreteSpec.Core.Prim.lookupId_rehint`, case `ConcreteSpec.Core.Vacuity.locals`, subject `ConcreteSpec.Core.Prim.rehintLocals`, mutant `ConcreteSpec.Core.Vacuity.corruptIdRehintLocals`). -/
example : ¬ (lookupId (corruptIdRehintLocals hints locals) 1 = lookupId locals 1) := by decide

/-- Instance (law `ConcreteSpec.Core.Prim.eval_rehint`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : Term.eval (rehintLocals hints locals) (rehintTerm hints source) = Term.eval locals source :=
  eval_rehint hints locals source

/-- Planted (law `ConcreteSpec.Core.Prim.eval_rehint`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.rehintTerm`, mutant `ConcreteSpec.Core.Vacuity.corruptIdRehintTerm`). -/
example : ¬ (Term.eval (rehintLocals hints locals) (corruptIdRehintTerm hints source) =
    Term.eval locals source) := by decide

/-- Instance (law `ConcreteSpec.Core.Prim.eval_operands_rehint`, case `ConcreteSpec.Core.Vacuity.source`). -/
example : operands.eval (rehintLocals hints locals) () (rehintTerm hints source) =
    operands.eval locals () source :=
  eval_operands_rehint hints locals () source

/-- Planted (law `ConcreteSpec.Core.Prim.eval_operands_rehint`, case `ConcreteSpec.Core.Vacuity.source`, subject `ConcreteSpec.Core.Prim.rehintTerm`, mutant `ConcreteSpec.Core.Vacuity.corruptIdRehintTerm`). -/
example : ¬ ((operands.eval (rehintLocals hints locals) () (corruptIdRehintTerm hints source) : Res (Int × Unit)) =
    operands.eval locals () source) := by
  intro h
  cases h

/-- The persistent key and obligation used by the evidence-consumption witnesses. -/
def demoOwner : StableOwner :=
  .function ⟨⟨⟨"demo", ⟨"1"⟩⟩, ["demo"], by decide⟩, "f"⟩
def obligationKey : StableKey := ⟨demoOwner, ⟨[⟨.primitive, .ordinal 0⟩]⟩⟩
def otherKey : StableKey := ⟨demoOwner, ⟨[⟨.primitive, .ordinal 1⟩]⟩⟩

/-- Every obligation in `EvidenceDemo.claim` has a true arithmetic proposition (`1 + 1 = 2` for the
current obligation and `2 + 2 = 4` for every other obligation), so distinguishing obligations by
identity is tested on genuinely true propositions rather than a false proposition. -/
def evidence : SafetyEvidence EvidenceDemo.claim := EvidenceDemo.evidence
def obligation : SafetyObligation := ⟨EvidenceDemo.current.obligation, obligationKey⟩
def obligations : ObligationTable := [obligation]
def discharged : DischargedFact EvidenceDemo.claim := ⟨obligation, by decide⟩

/-- A second obligation with a distinct `ObligationId` whose proposition `2 + 2 = 4` is also true. -/
def mismatchedObligation : SafetyObligation := ⟨⟨⟨0⟩, .safety, 1⟩, otherKey⟩
def mismatchedObligations : ObligationTable := [mismatchedObligation]

/-- Mutant consumer that fabricates an obligation record without checking `ObligationTable.lookup`. -/
def dischargeWithoutTable {claim : ObligationId → Prop} (e : SafetyEvidence claim)
    (_obligations : ObligationTable) (_current : EvidenceContext) (_requested : ObligationId) :
    Option (DischargedFact claim) :=
  some ⟨⟨e.context.obligation, obligationKey⟩, e.proof⟩

/-- Mutant consumer that keeps the proof but fabricates the persistent key in its output. -/
def dischargeWrongKey {claim : ObligationId → Prop} (e : SafetyEvidence claim)
    (_obligations : ObligationTable) (_current : EvidenceContext) (_requested : ObligationId) :
    Option (DischargedFact claim) := some ⟨⟨e.context.obligation, otherKey⟩, e.proof⟩

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.obligation`). -/
example : match evidence.discharge obligations EvidenceDemo.current obligation.id with
    | none => True
    | some result => evidence.Usable EvidenceDemo.current ∧
        EvidenceDemo.current.obligation = obligation.id ∧
        ObligationTable.lookup obligations obligation.id = some result.obligation ∧
        result.obligation.id = obligation.id :=
  SafetyEvidence.discharge_sound evidence obligations EvidenceDemo.current obligation.id

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`, case `ConcreteSpec.Core.Vacuity.obligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWrongKey`). -/
example : ¬ (match dischargeWrongKey evidence obligations EvidenceDemo.current obligation.id with
    | none => True
    | some result => evidence.Usable EvidenceDemo.current ∧
        EvidenceDemo.current.obligation = obligation.id ∧
        ObligationTable.lookup obligations obligation.id = some result.obligation ∧
        result.obligation.id = obligation.id) := by
  intro h
  have equal := congrArg (fun result => result.map (fun ob => ob.key)) h.2.2.1
  contradiction

/-- Regression example (not independent soundness evidence). -/
example : evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id = none :=
  SafetyEvidence.discharge_obligation_mismatch evidence mismatchedObligations
    EvidenceDemo.current mismatchedObligation.id (by decide)

/-- Mutant consumer that ignores the requested `ObligationId` and discharges `e.context.obligation`. -/
def dischargeIgnoringObligation {claim : ObligationId → Prop} (e : SafetyEvidence claim)
    (_obligations : ObligationTable) (current : EvidenceContext) (_requested : ObligationId) :
    Option (DischargedFact claim) :=
  if e.Usable current then some ⟨⟨e.context.obligation, obligationKey⟩, e.proof⟩ else none

/-- Regression example (not independent soundness evidence). -/
example : EvidenceDemo.current.obligation ≠ mismatchedObligation.id ∧
    ¬ (dischargeIgnoringObligation evidence mismatchedObligations EvidenceDemo.current
      mismatchedObligation.id = none) := by
  refine ⟨by decide, ?_⟩
  intro h
  simp [dischargeIgnoringObligation, SafetyEvidence.Usable, evidence, EvidenceDemo.evidence] at h

/-- Only the contract revision changes in this stale context. -/
def contractChanged : EvidenceContext := { EvidenceDemo.current with contract := ⟨⟨0⟩, 2⟩ }

/-- Regression example (not independent soundness evidence). -/
example : evidence.discharge obligations contractChanged obligation.id = none :=
  SafetyEvidence.discharge_context_mismatch evidence obligations contractChanged obligation.id (by decide)

/-- Mutant consumer that omits the `Usable` context check. -/
def dischargeIgnoringContext {claim : ObligationId → Prop} (e : SafetyEvidence claim)
    (obligations : ObligationTable) (_current : EvidenceContext) (requested : ObligationId) :
    Option (DischargedFact claim) :=
  match ObligationTable.lookup obligations requested with
  | none => none
  | some found =>
    if same : e.context.obligation = found.id then
      some ⟨found, same ▸ e.proof⟩
    else none

/-- Regression example (not independent soundness evidence). -/
example : ¬ evidence.Usable contractChanged ∧
    ¬ (dischargeIgnoringContext evidence obligations contractChanged obligation.id = none) := by
  refine ⟨by decide, ?_⟩
  intro h
  simp [dischargeIgnoringContext, obligations, obligation, ObligationTable.lookup, evidence,
    EvidenceDemo.evidence] at h

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_target_mismatch`, case `ConcreteSpec.Core.Vacuity.mismatchedObligation`).
Both `EvidenceDemo.claim obligation.id` (`1 + 1 = 2`) and `EvidenceDemo.claim mismatchedObligation.id`
(`2 + 2 = 4`) are true propositions, yet evidence for `obligation.id` cannot discharge
`mismatchedObligation`. -/
example : evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id = none :=
  SafetyEvidence.discharge_target_mismatch evidence mismatchedObligations EvidenceDemo.current
    mismatchedObligation.id mismatchedObligation rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_target_mismatch`, case `ConcreteSpec.Core.Vacuity.mismatchedObligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWithoutTable`). -/
example : (ObligationTable.lookup mismatchedObligations mismatchedObligation.id =
      some mismatchedObligation ∧
    evidence.context.obligation ≠ mismatchedObligation.id) ∧
    ¬ (dischargeWithoutTable evidence mismatchedObligations EvidenceDemo.current
      mismatchedObligation.id = none) := by
  refine ⟨⟨rfl, by decide⟩, ?_⟩
  intro h
  simp [dischargeWithoutTable] at h

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_unregistered`, case `ConcreteSpec.Core.Vacuity.obligation`). -/
example : evidence.discharge [] EvidenceDemo.current obligation.id = none :=
  SafetyEvidence.discharge_unregistered evidence [] EvidenceDemo.current obligation.id rfl

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_unregistered`, case `ConcreteSpec.Core.Vacuity.obligation`, subject `ConcreteSpec.Core.SafetyEvidence.discharge`, mutant `ConcreteSpec.Core.Vacuity.dischargeWithoutTable`). -/
example : ObligationTable.lookup [] obligation.id = none ∧
    ¬ (dischargeWithoutTable evidence [] EvidenceDemo.current obligation.id = none) := by
  refine ⟨rfl, ?_⟩
  intro h
  simp [dischargeWithoutTable] at h

def successAnswer : Exec.Answer Int Int Int := ⟨none, .ok 0, 7⟩
def failureAnswer : Exec.Answer Int Int Int := ⟨none, .ok 1, 7⟩
def undeclaredAnswer : Exec.Answer Int Int Int := ⟨none, .ok 2, 7⟩
def trapAnswer : Exec.Answer Int Int Int := ⟨none, .error .abort, 7⟩

/-- Mutant registry checker with a catch-all branch that accepts unregistered primitives. -/
def catchAllAllows {Val Ty Store Event : Type} (R : PrimRegistry Val Ty Store Event)
    (p : PrimId) (args : List Val) (st : Store) (a : Exec.Answer Val Event Store) : Bool :=
  match AList.lookup R.entries p with
  | none => true
  | some c => c.answerOk args st a

/-- Mutant small-step relation that omits the `O.allows` check on primitive requests. -/
def uncheckedPrimStep {O : Exec.Operands} (P : Exec.Program O) (c : Exec.Config O)
    (l : Exec.Label O) (c' : Exec.Config O) : Prop :=
  ∃ p vs site st k a,
    Exec.step P c = some (.request p vs site st k) ∧
    l = a.label ∧ k a = some c'

/-- Instance (law `ConcreteSpec.Core.PrimRegistry.allows_unknown`, case `ConcreteSpec.Core.Vacuity.successAnswer`). -/
example : PrimDemo.registry.allows ⟨1⟩ [] 7 successAnswer = false :=
  PrimRegistry.allows_unknown PrimDemo.registry ⟨1⟩ rfl [] 7 successAnswer

/-- Planted (law `ConcreteSpec.Core.PrimRegistry.allows_unknown`, case `ConcreteSpec.Core.Vacuity.successAnswer`, subject `ConcreteSpec.Core.PrimRegistry.allows`, mutant `ConcreteSpec.Core.Vacuity.catchAllAllows`). -/
example : AList.lookup PrimDemo.registry.entries ⟨1⟩ = none ∧
    ¬ (catchAllAllows PrimDemo.registry ⟨1⟩ [] 7 successAnswer = false) :=
  ⟨rfl, by decide⟩

abbrev requestProgram : Exec.Program PrimDemo.machine := ⟨[]⟩
abbrev unknownStart : Exec.Config PrimDemo.machine :=
  ⟨.exec (.prim none ⟨1⟩ [] ⟨0⟩), [], (7 : Int), []⟩
abbrev registeredStart : Exec.Config PrimDemo.machine :=
  ⟨.exec (.prim none ⟨0⟩ [] ⟨0⟩), [], (7 : Int), []⟩
abbrev finishedConfig : Exec.Config PrimDemo.machine :=
  ⟨.done, [], (7 : Int), []⟩

abbrev unknownCont :
    Exec.Answer PrimDemo.machine.Val PrimDemo.machine.Event PrimDemo.machine.Store →
      Option (Exec.Config PrimDemo.machine) :=
  fun a =>
    match a.result with
    | .ok _ => some ⟨.done, [], a.store, []⟩
    | .error kd => some ⟨.trapped kd ⟨0⟩, [], a.store, []⟩

/-- Instance (law `ConcreteSpec.Core.PrimRegistry.unknown_primitive_stuck`, case `ConcreteSpec.Core.Vacuity.unknownStart`). -/
example : ¬ Exec.Step requestProgram unknownStart .silent finishedConfig :=
  PrimRegistry.unknown_primitive_stuck (O := PrimDemo.base)
    PrimDemo.registry requestProgram rfl rfl .silent finishedConfig

/-- Planted (law `ConcreteSpec.Core.PrimRegistry.unknown_primitive_stuck`, case `ConcreteSpec.Core.Vacuity.unknownStart`, subject `ConcreteSpec.Core.Exec.Step`, mutant `ConcreteSpec.Core.Vacuity.uncheckedPrimStep`). -/
example : (Exec.step requestProgram unknownStart =
      some (.request ⟨1⟩ [] ⟨0⟩ unknownStart.store unknownCont) ∧
    AList.lookup PrimDemo.registry.entries ⟨1⟩ = none) ∧
    ¬ ¬ uncheckedPrimStep requestProgram unknownStart .silent finishedConfig := by
  have hstep : Exec.step requestProgram unknownStart =
      some (.request ⟨1⟩ [] ⟨0⟩ unknownStart.store unknownCont) := by
    unfold Exec.step requestProgram unknownStart Exec.onRes Exec.evalArgs unknownCont
    dsimp
    congr 1
    congr 1
    funext a
    cases a.result <;> rfl
  refine ⟨⟨hstep, rfl⟩, ?_⟩
  intro hnot
  apply hnot
  exact ⟨⟨1⟩, [], ⟨0⟩, unknownStart.store, unknownCont, successAnswer, hstep, rfl, rfl⟩

abbrev answerOracle : Exec.Oracle PrimDemo.machine := fun _ _ _ _ _ => some successAnswer
abbrev initialHist : List PrimDemo.machine.Event := [(99 : Int)]

/-- Mutant driver that drops the incoming event history from the returned report. -/
def dropHistoryDrive {O : Exec.Operands} (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) : Ghost.SegmentReport O :=
  { Ghost.drive P ρ fuel hist c with events := [] }

/-- Instance (law `ConcreteSpec.Core.Ghost.drive_steps_result`, case `ConcreteSpec.Core.Vacuity.registeredStart`). -/
example : ∃ tr,
    (Ghost.drive requestProgram answerOracle 2 initialHist registeredStart).events =
      initialHist ++ tr ∧
    Exec.Steps requestProgram registeredStart tr
      (Ghost.drive requestProgram answerOracle 2 initialHist registeredStart).config :=
  Ghost.drive_steps_result requestProgram answerOracle 2 initialHist registeredStart

/-- Planted (law `ConcreteSpec.Core.Ghost.drive_steps_result`, case `ConcreteSpec.Core.Vacuity.registeredStart`, subject `ConcreteSpec.Core.Ghost.drive`, mutant `ConcreteSpec.Core.Vacuity.dropHistoryDrive`). -/
example : ¬ ∃ tr,
    (dropHistoryDrive requestProgram answerOracle 2 initialHist registeredStart).events =
      initialHist ++ tr ∧
    Exec.Steps requestProgram registeredStart tr
      (dropHistoryDrive requestProgram answerOracle 2 initialHist registeredStart).config := by
  rintro ⟨tr, he, _⟩
  cases he

abbrev returnedStart : Exec.Config PrimDemo.base := ⟨.exiting (.ret 1), [], 5, []⟩
abbrev trappedStart : Exec.Config PrimDemo.base := ⟨.trapped .abort ⟨3⟩, [], 5, []⟩

/-- A planted mutant that resumes a trailing statement even from a returned or trapped state. -/
def restartingRunStmts {O : Exec.Operands} (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) : List (Exec.Stmt O) → Exec.Config O → Ghost.SegmentReport O
  | [], c => Ghost.drive P ρ fuel hist c
  | s :: ss, c =>
    let r := Ghost.drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
    match r.ending with
    | .final .normal => restartingRunStmts P ρ r.remaining r.events ss r.config
    | _ => r

/-- Regression example (not independent soundness evidence). -/
example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
    Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] returnedStart :=
  Ghost.runStmts_of_not_normal GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩]
    returnedStart (by decide)

/-- Regression example (not independent soundness evidence). -/
example : returnedStart.final? ≠ some .normal ∧
    ¬ (restartingRunStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
      Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] returnedStart) := by
  refine ⟨by decide, ?_⟩
  intro h
  cases h

/-- Instance (law `ConcreteSpec.Core.Ghost.runStmts_terminal`, case `ConcreteSpec.Core.Vacuity.returnedStart`). -/
example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
    ⟨[], .final (.returned 1), returnedStart, 20⟩ :=
  Ghost.runStmts_terminal GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart
    (.returned 1) rfl (by decide)

/-- Instance (law `ConcreteSpec.Core.Ghost.runStmts_terminal`, case `ConcreteSpec.Core.Vacuity.trappedStart`). -/
example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart =
    ⟨[], .final (.trapped .abort ⟨3⟩), trappedStart, 20⟩ :=
  Ghost.runStmts_terminal GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart
    (.trapped .abort ⟨3⟩) rfl (by decide)

/-- Planted (law `ConcreteSpec.Core.Ghost.runStmts_terminal`, case `ConcreteSpec.Core.Vacuity.returnedStart`, subject `ConcreteSpec.Core.Ghost.runStmts`, mutant `ConcreteSpec.Core.Vacuity.restartingRunStmts`). -/
example : (returnedStart.final? = some (.returned 1) ∧ Exec.Final.returned (1 : Int) ≠ .normal) ∧
    ¬ (restartingRunStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
      ⟨[], .final (.returned 1), returnedStart, 20⟩) := by
  refine ⟨⟨rfl, by decide⟩, ?_⟩
  intro h
  cases h

/-- Mutant eraser that drops concrete statements as well as ghost statements. -/
def eraseAllPieces {O : Exec.Operands} {G : Ghost.Language O} (_ : List (Ghost.Piece G)) :
    List (Exec.Stmt O) := []

def abortPieces : List (Ghost.Piece GhostDemo.language) :=
  [.ghost (.assert ⟨⟨0⟩, .assertion, 0⟩ True), .concrete (.abort ⟨3⟩)]

/-- Instance (law `ConcreteSpec.Core.Ghost.ghost_erasure_run`, case `ConcreteSpec.Core.GhostDemo.pieces`). -/
example : Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start =
    Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase GhostDemo.pieces)
      GhostDemo.start :=
  Ghost.ghost_erasure_run GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start

/-- Planted (law `ConcreteSpec.Core.Ghost.ghost_erasure_run`, case `ConcreteSpec.Core.GhostDemo.pieces`, subject `ConcreteSpec.Core.Ghost.erase`, mutant `ConcreteSpec.Core.Vacuity.eraseAllPieces`). -/
example : ¬ (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start =
    Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces GhostDemo.pieces)
      GhostDemo.start) := by
  intro h
  have hrem := congrArg Ghost.SegmentReport.remaining h
  revert hrem
  decide

/-- Instance (law `ConcreteSpec.Core.Ghost.ghost_erasure_ending`, case `ConcreteSpec.Core.Vacuity.abortPieces`). -/
example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    abortPieces [] GhostDemo.start).events =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase abortPieces)
      GhostDemo.start).events ∧
    (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
      abortPieces [] GhostDemo.start).ending =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (Ghost.erase abortPieces)
      GhostDemo.start).ending :=
  Ghost.ghost_erasure_ending GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    abortPieces [] GhostDemo.start

/-- Planted (law `ConcreteSpec.Core.Ghost.ghost_erasure_ending`, case `ConcreteSpec.Core.Vacuity.abortPieces`, subject `ConcreteSpec.Core.Ghost.erase`, mutant `ConcreteSpec.Core.Vacuity.eraseAllPieces`). -/
example : ¬ ((Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    abortPieces [] GhostDemo.start).events =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces abortPieces)
      GhostDemo.start).events ∧
    (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
      abortPieces [] GhostDemo.start).ending =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] (eraseAllPieces abortPieces)
      GhostDemo.start).ending) := by
  rintro ⟨_, hend⟩
  cases hend

end Vacuity
end ConcreteSpec.Core
