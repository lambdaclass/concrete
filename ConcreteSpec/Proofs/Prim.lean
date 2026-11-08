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

/-- Helper (projection). Lookup returns an obligation with the requested intrinsic identity. -/
theorem ObligationTable.lookup_id (obligations : ObligationTable) (requested : ObligationId)
    (found : SafetyObligation) (success : ObligationTable.lookup obligations requested = some found) :
    found.id = requested := by
  induction obligations with
  | nil => simp [ObligationTable.lookup] at success
  | cons obligation rest ih =>
    by_cases same : obligation.id = requested
    · simp [ObligationTable.lookup, same] at success
      cases success
      exact same
    · exact ih (by simpa [ObligationTable.lookup, same] using success)

/-- **Evidence consumption is sound.** Success consumes the evidence's exact context, discharges
the obligation identity it names, and retrieves that obligation from the authoritative table. The
returned dependent record contains a proof of the retrieved proposition. -/
theorem SafetyEvidence.discharge_sound (e : SafetyEvidence) (obligations : ObligationTable)
    (current : EvidenceContext) (requested : ObligationId) (discharged : DischargedFact)
    (success : e.discharge obligations current requested = some discharged) :
    e.context = current ∧ current.obligation = requested ∧
      ObligationTable.lookup obligations requested = some discharged.obligation ∧
      discharged.obligation.id = requested := by
  by_cases context : e.context = current
  · by_cases identity : current.obligation = requested
    · cases lookup : ObligationTable.lookup obligations requested with
      | none => simp [SafetyEvidence.discharge, context, identity, lookup] at success
      | some obligation =>
        by_cases fact : e.fact = obligation.fact
        · simp [SafetyEvidence.discharge, context, identity, lookup, fact] at success
          cases success
          exact ⟨context, identity, rfl, ObligationTable.lookup_id obligations requested obligation lookup⟩
        · simp [SafetyEvidence.discharge, context, identity, lookup, fact] at success
    · simp [SafetyEvidence.discharge, context, identity] at success
  · simp [SafetyEvidence.discharge, context] at success

/-- A stale context cannot produce a successful discharge. -/
theorem SafetyEvidence.discharge_context_mismatch (e : SafetyEvidence)
    (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId)
    (mismatch : e.context ≠ current) : e.discharge obligations current requested = none := by
  simp [SafetyEvidence.discharge, mismatch]

/-- Evidence cannot discharge an obligation identity different from the one it names. -/
theorem SafetyEvidence.discharge_obligation_mismatch (e : SafetyEvidence)
    (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId)
    (mismatch : current.obligation ≠ requested) : e.discharge obligations current requested = none := by
  by_cases context : e.context = current
  · simp [SafetyEvidence.discharge, context, mismatch]
  · simp [SafetyEvidence.discharge, context]

/-- A proof of a different proposition cannot discharge the retrieved obligation. -/
theorem SafetyEvidence.discharge_proposition_mismatch (e : SafetyEvidence)
    (obligations : ObligationTable) (current : EvidenceContext) (requested : ObligationId)
    (obligation : SafetyObligation) (lookup : ObligationTable.lookup obligations requested = some obligation)
    (mismatch : e.fact ≠ obligation.fact) : e.discharge obligations current requested = none := by
  by_cases context : e.context = current
  · by_cases identity : current.obligation = requested
    · simp [SafetyEvidence.discharge, context, identity, lookup, mismatch]
    · simp [SafetyEvidence.discharge, context, identity]
  · simp [SafetyEvidence.discharge, context]

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

/-- **Accepted answers satisfy the postcondition or a declared failure.** Both predicates observe
only the result and surviving state; neither entry arguments nor an entry-state snapshot is
available to `ensures`. A failure alternative must have the contract's result type. -/
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

/-- The concrete-step soundness law for the report returned by `drive` itself. -/
theorem drive_steps_result (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (c : Exec.Config O) :
    ∃ tr, (drive P ρ fuel hist c).events = hist ++ tr ∧
      Exec.Steps P c tr (drive P ρ fuel hist c).config :=
  drive_steps P ρ fuel hist c _ rfl

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

/-- **A non-normal configuration has no later transition.** Running trailing statements from a
returned or trapped configuration emits no event and preserves the exact ending, configuration and
fuel; no statement body is entered. -/
theorem runStmts_terminal (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) (ss : List (Exec.Stmt O)) (c : Exec.Config O)
    (f : Exec.Final O.Val) (hf : c.final? = some f) (hn : f ≠ .normal) :
    runStmts P ρ fuel hist ss c = ⟨hist, .final f, c, fuel⟩ := by
  cases ss with
  | nil => simp [runStmts, drive, hf]
  | cons s ss => cases f <;> simp [runStmts, drive, hf] at hn ⊢

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

/-! ## Non-vacuity witnesses

Each law above has a concrete instance whose interesting premise or result holds, followed by a
planted counter-example. The counter-examples mutate the transformation, checker, evidence match or
execution state; they do not merely negate a definition. -/

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

/-- Non-trivial instance for `lookupId_rename`: lookup finds the second renamed entry. -/
example : lookupId (renameLocals swap locals) (swap.toFun 1) = lookupId locals 1 ∧
    lookupId locals 1 = some 3 := by decide

/-- Planted counter-example: renaming the environment but not the numeric key loses the entry. -/
example : lookupId (renameLocals swap [(x0, 3)]) 0 ≠ lookupId [(x0, 3)] 0 := by decide

/-- Non-trivial instance for `eval_rename`: both evaluations return the same non-stuck value. -/
example : Term.eval (renameLocals swap locals) (renameTerm swap source) = Term.eval locals source ∧
    Term.eval locals source = .ok 5 := by decide

/-- Planted counter-example: renaming only the term makes its read unbound. -/
example : Term.eval locals (renameTerm swap (.var x0)) ≠ Term.eval locals (.var x0) := by decide

/-- Non-trivial instance for `eval_operands_rename`: the C1 adapter also returns `5`. -/
example : operands.eval (renameLocals swap locals) () (renameTerm swap source) =
    operands.eval locals () source ∧
    (operands.eval locals () source : Res (Int × Unit)) = .ok ((5 : Int), ()) := by
  constructor <;> rfl

/-- Planted counter-example: the adapter cannot repair an inconsistently renamed occurrence. -/
example : (operands.eval locals () (renameTerm swap (.var x0)) : Res (Int × Unit)) ≠
    operands.eval locals () (.var x0) := by
  intro h
  cases h

/-- Non-trivial instance for `rename_inverse`: the source changes once and is then recovered. -/
example : renameTerm swap.symm (renameTerm swap source) = source ∧
    renameTerm swap source ≠ source := by decide

/-- Planted counter-example: substituting the identity for the inverse does not recover the term. -/
example : renameTerm .id (renameTerm swap (.var x0)) ≠ .var x0 := by decide

/-- Non-trivial instance for `rename_id`: a compound binder term is unchanged. -/
example : renameTerm .id source = source := by decide

/-- Planted counter-example: a non-identity renaming is not the identity law. -/
example : renameTerm swap (.var x0) ≠ .var x0 := by decide

/-- Non-trivial instance for `lookupId_rehint`: numeric lookup still finds the value. -/
example : lookupId (rehintLocals hints locals) 1 = lookupId locals 1 ∧
    lookupId locals 1 = some 3 := by decide

/-- Planted counter-example: C1's whole-`Var` lookup fails if only the environment is rehinted. -/
example : AList.lookup (rehintLocals hints [(x0, 3)]) x0 ≠ AList.lookup [(x0, 3)] x0 := by decide

/-- Non-trivial instance for `eval_rehint`: consistent rehinting preserves a successful result. -/
example : Term.eval (rehintLocals hints locals) (rehintTerm hints source) =
    Term.eval locals source ∧ Term.eval locals source = .ok 5 := by decide

/-- Planted counter-example: a mutated rehinting that also changes the numeric identity breaks evaluation. -/
example : Term.eval [(x0, 10)] (.var ⟨"renamed", 1⟩) ≠ Term.eval [(x0, 10)] (.var x0) := by decide

/-- Non-trivial instance for `eval_operands_rehint`: the adapter preserves the value `5`. -/
example : operands.eval (rehintLocals hints locals) () (rehintTerm hints source) =
    operands.eval locals () source ∧
    (operands.eval locals () source : Res (Int × Unit)) = .ok ((5 : Int), ()) := by
  constructor <;> rfl

/-- Planted counter-example: the adapter cannot excuse changing an occurrence's numeric identity. -/
example : (operands.eval [(x0, (10 : Int))] () (.var ⟨"renamed", 1⟩) : Res (Int × Unit)) ≠
    operands.eval [(x0, (10 : Int))] () (.var x0) := by
  intro h
  cases h

/-- Helper (projection). A non-trivial proved fact and its exact evidence context. -/
def evidence : SafetyEvidence := ⟨1 + 1 = 2, EvidenceDemo.current, by decide⟩

/-- Helper (projection). The arithmetic obligation named by the evidence context. -/
def obligation : SafetyObligation := ⟨EvidenceDemo.current.obligation, 1 + 1 = 2⟩
def obligations : ObligationTable := [obligation]
def discharged : DischargedFact := ⟨obligation, by simp [obligation]⟩

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`). Real consumption returns the
authoritative arithmetic obligation under the exact context and identity. -/
example : evidence.discharge obligations EvidenceDemo.current obligation.id = some discharged ∧
    ObligationTable.lookup obligations obligation.id = some discharged.obligation := by
  simp [SafetyEvidence.discharge, obligations, discharged, obligation, evidence, ObligationTable.lookup]

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_sound`). A fabricated result for an
obligation absent from the table is not a successful consumption. -/
example : ¬ (evidence.discharge [] EvidenceDemo.current obligation.id = some discharged) := by
  simp [SafetyEvidence.discharge, ObligationTable.lookup]

/-- Helper (projection). The same proposition under a different typed obligation identity. -/
def mismatchedObligation : SafetyObligation :=
  { obligation with id := ⟨⟨0⟩, .safety, 1⟩ }
def mismatchedObligations : ObligationTable := [mismatchedObligation]

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_obligation_mismatch`). Even though the
proposition and the rest of the context match, the differently identified request is refused. -/
example : evidence.discharge mismatchedObligations EvidenceDemo.current mismatchedObligation.id = none := by
  apply SafetyEvidence.discharge_obligation_mismatch
  decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
noncomputable def dischargeIgnoringObligation (e : SafetyEvidence) (obligations : ObligationTable)
    (current : EvidenceContext) (requested : ObligationId) : Option SafetyObligation := by
  classical
  exact
    if e.context = current then
      match ObligationTable.lookup obligations requested with
      | some found => if e.fact = found.fact then some found else none
      | none => none
    else none

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_obligation_mismatch`). A consumer that
omits the evidence's obligation identity accepts that exact mismatch. -/
example : evidence.context.obligation ≠ mismatchedObligation.id ∧
    dischargeIgnoringObligation evidence mismatchedObligations EvidenceDemo.current
      mismatchedObligation.id = some mismatchedObligation := by
  refine ⟨by decide, ?_⟩
  simp [dischargeIgnoringObligation, evidence, mismatchedObligations, mismatchedObligation,
    obligation, ObligationTable.lookup]

/-- Helper (projection). Only the contract revision changes in this stale context. -/
def contractChanged : EvidenceContext := { EvidenceDemo.current with contract := ⟨⟨0⟩, 2⟩ }

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_context_mismatch`). A changed contract
revision prevents consumption even for the original obligation and proposition. -/
example : evidence.discharge obligations contractChanged obligation.id = none := by
  apply SafetyEvidence.discharge_context_mismatch
  decide

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_context_mismatch`). Target, checker,
specification and obligation identity changes are each refused by the actual consumer as well. -/
example :
    evidence.discharge obligations { EvidenceDemo.current with target := { linuxX8664 with byteOrder := .big } }
      obligation.id = none ∧
    evidence.discharge obligations { EvidenceDemo.current with checker := ⟨0, 2, 0⟩ }
      obligation.id = none ∧
    evidence.discharge obligations { EvidenceDemo.current with spec := ⟨0, 2⟩ }
      obligation.id = none ∧
    evidence.discharge obligations
      { EvidenceDemo.current with obligation := mismatchedObligation.id } obligation.id = none := by
  refine ⟨?_, ?_, ?_, ?_⟩ <;>
    apply SafetyEvidence.discharge_context_mismatch <;> decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
noncomputable def dischargeIgnoringContext (e : SafetyEvidence) (obligations : ObligationTable)
    (requested : ObligationId) : Option SafetyObligation := by
  classical
  exact
    match ObligationTable.lookup obligations requested with
    | some found => if e.context.obligation = requested ∧ e.fact = found.fact then some found else none
    | none => none

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_context_mismatch`). A consumer omitting
the context check accepts stale contract evidence. -/
example : evidence.context ≠ contractChanged ∧
    dischargeIgnoringContext evidence obligations obligation.id = some obligation := by
  refine ⟨by decide, ?_⟩
  simp [dischargeIgnoringContext, evidence, obligations, obligation, ObligationTable.lookup]

/-- Helper (projection). A different proposition under the evidence's obligation identity. -/
def differentObligation : SafetyObligation := ⟨obligation.id, 1 + 1 = 3⟩
def differentObligations : ObligationTable := [differentObligation]

/-- Instance (law `ConcreteSpec.Core.SafetyEvidence.discharge_proposition_mismatch`). Context and
identity match, but the arithmetic proof is for a different proposition and cannot be consumed. -/
example : evidence.discharge differentObligations EvidenceDemo.current obligation.id = none := by
  apply SafetyEvidence.discharge_proposition_mismatch (obligation := differentObligation)
  · rfl
  · intro equal
    change (1 + 1 = 2) = (1 + 1 = 3) at equal
    have valid : 1 + 1 = 2 := by decide
    have invalid : 1 + 1 = 3 := equal ▸ valid
    omega

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def lookupIgnoringEvidence (obligations : ObligationTable) (requested : ObligationId) :
    Option SafetyObligation := ObligationTable.lookup obligations requested

/-- Planted (law `ConcreteSpec.Core.SafetyEvidence.discharge_proposition_mismatch`). Looking up the
obligation while ignoring the evidence proposition returns the mismatched obligation successfully. -/
example : evidence.fact ≠ differentObligation.fact ∧
    lookupIgnoringEvidence differentObligations obligation.id = some differentObligation := by
  refine ⟨?_, rfl⟩
  intro equal
  change (1 + 1 = 2) = (1 + 1 = 3) at equal
  have valid : 1 + 1 = 2 := by decide
  have invalid : 1 + 1 = 3 := equal ▸ valid
  omega

def successAnswer : Exec.Answer Int Int Int := ⟨none, .ok 0, 7⟩
def failureAnswer : Exec.Answer Int Int Int := ⟨none, .ok 1, 7⟩
def undeclaredAnswer : Exec.Answer Int Int Int := ⟨none, .ok 2, 7⟩
def trapAnswer : Exec.Answer Int Int Int := ⟨none, .error .abort, 7⟩

/-- A two-type contract: `true` is its result type, while its only failure alternative is wrongly
labeled with the distinct type `false`. -/
def twoTypeContract : PrimContract Int Bool Int Int where
  id := ⟨⟨0⟩, 1⟩
  parameters := []
  result := true
  authority := []
  ownership := fun _ before after => after = before
  dependencies := fun _ _ _ => True
  requires := fun _ _ => True
  ensures := fun v _ => v = 0
  footprint := fun _ _ => []
  initialized := fun _ => True
  valid := fun _ => True
  failures := [⟨false, fun v => v = 1⟩]
  events := fun ev => ev = none
  eventTags := fun _ => []
  leakageTags := fun _ => []
  resources := fun _ before after => after = before
  termination := fun _ _ => True
  answerOk := fun args st a => match a.result with
    | .ok v => decide (args = [] ∧ a.event = none ∧ v = 0 ∧ a.store = st)
    | .error _ => false

/-- Non-trivial two-type instance: the ordinary success remains accepted. -/
example : twoTypeContract.Allowed [] 7 successAnswer := by
  simp [twoTypeContract, PrimContract.Allowed, successAnswer]

/-- Refused: the failure value `1` has the undeclared error type `false`, not result type `true`. -/
example : ¬ twoTypeContract.Allowed [] 7 failureAnswer := by
  simp [twoTypeContract, PrimContract.Allowed, failureAnswer]

/-- Planted counter-example: a matcher that removes `f.errorTy = c.result` accepts that same failure. -/
example : ∃ f ∈ twoTypeContract.failures, f.holds 1 := by
  exact ⟨⟨false, fun v => v = 1⟩, by simp [twoTypeContract], rfl⟩

/-- Non-trivial instance for `PrimRegistry.answerOk_iff`: the declared failure passes both sides. -/
example : PrimDemo.unitContract.answerOk [] 7 failureAnswer = true ↔
    PrimDemo.unitContract.Allowed [] 7 failureAnswer := by
  simp [PrimDemo.unitContract, PrimContract.Allowed, failureAnswer]

/-- Planted counter-example: a checker that omits the declared failure violates the equivalence. -/
example : ¬ (false = true ↔ PrimDemo.unitContract.Allowed [] 7 failureAnswer) := by
  simp [PrimDemo.unitContract, PrimContract.Allowed, failureAnswer]

/-- Non-trivial instance for `PrimRegistry.allows_unknown`: primitive one has no registry entry. -/
example : PrimDemo.registry.allows ⟨1⟩ [] 7 successAnswer = false := by decide

/-- Planted counter-example: treating the registered primitive zero as unknown rejects a valid answer. -/
example : ¬ PrimDemo.registry.allows ⟨0⟩ [] 7 successAnswer = false := by decide

abbrev requestProgram : Exec.Program PrimDemo.machine := ⟨[]⟩
abbrev unknownStart : Exec.Config PrimDemo.machine :=
  ⟨.exec (.prim none ⟨1⟩ [] ⟨0⟩), [], (7 : Int), []⟩
abbrev registeredStart : Exec.Config PrimDemo.machine :=
  ⟨.exec (.prim none ⟨0⟩ [] ⟨0⟩), [], (7 : Int), []⟩

/-- Non-trivial instance for `PrimRegistry.unknown_primitive_stuck`: an actual request has no step. -/
example (l : Exec.Label PrimDemo.machine) (c' : Exec.Config PrimDemo.machine) :
    ¬ Exec.Step requestProgram unknownStart l c' := by
  apply PrimRegistry.unknown_primitive_stuck (O := PrimDemo.base)
    PrimDemo.registry requestProgram
  · rfl
  · rfl

/-- Planted counter-example: the same request to the registered primitive does have a step. -/
example : ¬ (∀ (l : Exec.Label PrimDemo.machine) (c' : Exec.Config PrimDemo.machine),
    ¬ Exec.Step requestProgram registeredStart l c') := by
  intro h
  apply h .silent ⟨.done, [], (7 : Int), []⟩
  exact Exec.Step.prim (P := requestProgram) (a := successAnswer) rfl (by decide) rfl

/-- Non-trivial instance for `PrimRegistry.allows_allowed`: a declared failure satisfies `Allowed`. -/
example : PrimDemo.unitContract.Allowed [] 7 failureAnswer :=
  PrimRegistry.allows_allowed (O := PrimDemo.base) PrimDemo.registry ⟨0⟩ PrimDemo.unitContract rfl []
    7 failureAnswer (by decide)

/-- Planted counter-example: the undeclared ordinary result `2` is not allowed. -/
example : ¬ PrimDemo.unitContract.Allowed [] 7 undeclaredAnswer := by
  simp [PrimDemo.unitContract, PrimContract.Allowed, undeclaredAnswer]

/-- Non-trivial instance for `PrimRegistry.allows_postcondition`: the declared failure matches its
alternative even though it does not satisfy the success predicate. -/
example : PrimDemo.unitContract.ensures 1 failureAnswer.store ∨
    ∃ f ∈ PrimDemo.unitContract.failures,
      f.errorTy = PrimDemo.unitContract.result ∧ f.holds 1 := by
  simp [PrimDemo.unitContract, failureAnswer]

/-- Planted counter-example: the old ensures-only claim rejects this accepted declared failure. -/
example : ¬ (PrimDemo.registry.allows ⟨0⟩ [] 7 failureAnswer = true →
    PrimDemo.unitContract.ensures 1 failureAnswer.store) := by
  simp [PrimDemo.registry, PrimRegistry.allows, AList.lookup, PrimDemo.unitContract, failureAnswer]

/-- Non-trivial instance for `PrimRegistry.allows_no_implicit_trap`: the accepted failure is ordinary. -/
example : ∃ v, failureAnswer.result = .ok v :=
  PrimRegistry.allows_no_implicit_trap (O := PrimDemo.base) PrimDemo.registry ⟨0⟩
    PrimDemo.unitContract rfl [] 7 failureAnswer (by decide)

/-- Planted counter-example: an implicit trap is not an ordinary result value. -/
example : ¬ ∃ v, trapAnswer.result = .ok v := by
  rintro ⟨v, hv⟩
  simp [trapAnswer] at hv

abbrev answerOracle : Exec.Oracle PrimDemo.machine := fun _ _ _ _ _ => some successAnswer

/-- Non-trivial instance for `Ghost.drive_steps`: a real primitive request has a `Steps` derivation. -/
example : ∃ tr, (Ghost.drive requestProgram answerOracle 2 [] registeredStart).events = [] ++ tr ∧
    Exec.Steps requestProgram registeredStart tr
      (Ghost.drive requestProgram answerOracle 2 [] registeredStart).config :=
  Ghost.drive_steps requestProgram answerOracle 2 [] registeredStart _ rfl

/-- A real completed report and two planted fabrications of its observations and retained state. -/
def completedReport : Ghost.SegmentReport PrimDemo.base :=
  Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] GhostDemo.start

def fabricatedEventReport : Ghost.SegmentReport PrimDemo.base :=
  { completedReport with events := [99] }

def fabricatedStoreReport : Ghost.SegmentReport PrimDemo.base :=
  { completedReport with config := ⟨.done, [], 8, []⟩ }

/-- Planted counter-example for `Ghost.drive_steps`: no trace has the fabricated event and reaches
the report's configuration, even though the report itself was obtained by mutating a real run. -/
example : ¬ ∃ tr, fabricatedEventReport.events = [] ++ tr ∧
    Exec.Steps GhostDemo.program GhostDemo.start tr fabricatedEventReport.config := by
  have inv : ∀ {tr : List Int} {c' : Exec.Config PrimDemo.base},
      Exec.Steps GhostDemo.program GhostDemo.start tr c' → tr = [] ∧ c' = GhostDemo.start := by
    intro tr c' hs
    cases hs with
    | refl => exact ⟨rfl, rfl⟩
    | step h hs =>
      cases h with
      | silent hstep => simp [Exec.step] at hstep
      | prim hstep ha hk => simp [Exec.step] at hstep
  rintro ⟨tr, he, hs⟩
  obtain ⟨ht, hc⟩ := inv hs
  subst tr
  simp [fabricatedEventReport] at he

/-- Non-trivial instance for `Ghost.drive_steps_result`, with no auxiliary report argument. -/
example : ∃ tr, (Ghost.drive requestProgram answerOracle 2 [] registeredStart).events = [] ++ tr ∧
    Exec.Steps requestProgram registeredStart tr
      (Ghost.drive requestProgram answerOracle 2 [] registeredStart).config :=
  Ghost.drive_steps_result requestProgram answerOracle 2 [] registeredStart

/-- Planted counter-example for `Ghost.drive_steps_result`: no trace reaches the fabricated store,
so changing only `config` is enough to falsify the full soundness property. -/
example : ¬ ∃ tr, fabricatedStoreReport.events = [] ++ tr ∧
    Exec.Steps GhostDemo.program GhostDemo.start tr fabricatedStoreReport.config := by
  have inv : ∀ {tr : List Int} {c' : Exec.Config PrimDemo.base},
      Exec.Steps GhostDemo.program GhostDemo.start tr c' → tr = [] ∧ c' = GhostDemo.start := by
    intro tr c' hs
    cases hs with
    | refl => exact ⟨rfl, rfl⟩
    | step h hs =>
      cases h with
      | silent hstep => simp [Exec.step] at hstep
      | prim hstep ha hk => simp [Exec.step] at hstep
  rintro ⟨tr, he, hs⟩
  obtain ⟨ht, hc⟩ := inv hs
  have hstore := congrArg Exec.Config.store hc
  simp [fabricatedStoreReport] at hstore

abbrev returnedStart : Exec.Config PrimDemo.base := ⟨.exiting (.ret 1), [], 5, []⟩
abbrev trappedStart : Exec.Config PrimDemo.base := ⟨.trapped .abort ⟨3⟩, [], 5, []⟩

/-- Non-trivial instance for `Ghost.runStmts_of_not_normal`: a returned configuration is not restarted. -/
example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.skip] returnedStart =
    Ghost.drive GhostDemo.program GhostDemo.oracle 20 [] returnedStart := by
  apply Ghost.runStmts_of_not_normal
  decide

/-- Non-trivial instances for `Ghost.runStmts_terminal`: returned and trapped runs preserve the
complete report rather than entering the trailing `abort`. -/
example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart =
    ⟨[], .final (.returned 1), returnedStart, 20⟩ :=
  Ghost.runStmts_terminal GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] returnedStart
    (.returned 1) rfl (by decide)

example : Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart =
    ⟨[], .final (.trapped .abort ⟨3⟩), trappedStart, 20⟩ :=
  Ghost.runStmts_terminal GhostDemo.program GhostDemo.oracle 20 [] [.abort ⟨4⟩] trappedStart
    (.trapped .abort ⟨3⟩) rfl (by decide)

/-- A planted mutant that resumes a trailing statement even from a returned or trapped state. -/
def restartingRunStmts {O : Exec.Operands} (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) : List (Exec.Stmt O) → Exec.Config O → Ghost.SegmentReport O
  | [], c => Ghost.drive P ρ fuel hist c
  | s :: ss, c =>
    let r := Ghost.drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
    match r.ending with
    | .final .normal => restartingRunStmts P ρ r.remaining r.events ss r.config
    | _ => r

/-- A returned configuration with a live local that the trailing assignment can change. -/
abbrev returnedWithLocal : Exec.Config PrimDemo.base := ⟨.exiting (.ret 1), [(x0, 1)], 5, []⟩

/-- Planted counter-example for `Ghost.runStmts_terminal`: the restarting runner executes the
assignment, changing both the local state and the ending after the return. -/
example : (restartingRunStmts GhostDemo.program GhostDemo.oracle 20 []
    [.assign x0 () ⟨4⟩] returnedWithLocal).config.locals ≠
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 []
      [.assign x0 () ⟨4⟩] returnedWithLocal).config.locals := by decide

example : (restartingRunStmts GhostDemo.program GhostDemo.oracle 20 []
    [.assign x0 () ⟨4⟩] returnedWithLocal).ending ≠ .final (.returned 1) := by
  intro h
  cases h

/-- Non-trivial instance for `Ghost.ghost_erasure_run`: ghost reads and writes surround concrete `skip`. -/
example : Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start =
    Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.skip] GhostDemo.start :=
  Ghost.ghost_erasure_run GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start

/-- Planted counter-example: erasing concrete `skip` as well changes the retained fuel. -/
example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start).remaining ≠
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [] GhostDemo.start).remaining := by decide

/-- Non-trivial instance for `Ghost.ghost_erasure_ending`: both observations are preserved. -/
example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start).events =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.skip] GhostDemo.start).events ∧
    (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
      GhostDemo.pieces [] GhostDemo.start).ending =
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [.skip] GhostDemo.start).ending :=
  Ghost.ghost_erasure_ending GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    GhostDemo.pieces [] GhostDemo.start

/-- Planted counter-example: erasing a concrete `abort` changes the ending, so only ghost code is
removable. -/
example : (Ghost.runPieces GhostDemo.language GhostDemo.program GhostDemo.oracle 20 []
    [.ghost (.assert ⟨⟨0⟩, .assertion, 0⟩ True), .concrete (.abort ⟨3⟩)] [] GhostDemo.start).ending ≠
    (Ghost.runStmts GhostDemo.program GhostDemo.oracle 20 [] [] GhostDemo.start).ending := by
  intro h
  cases h

end Vacuity
end ConcreteSpec.Core
