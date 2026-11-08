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
end ConcreteSpec.Core
