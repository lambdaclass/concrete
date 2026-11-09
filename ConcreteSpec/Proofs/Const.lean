import ConcreteSpec.Spec.Const
import ConcreteSpec.Proofs.Scalar

/-!
# The laws of constants (`Spec/Const.lean`, foundation contract C2)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

- `eval_agrees`: exact agreement with independent primitive-grounded, fuel-indexed semantics.
- `semantic_failure_refuses`: a semantic failure after any successful prefix refuses the whole
  list with the failing identity and prevents admission, regardless of usage.
- `eval_wellTyped_progress`: with sufficient fuel, a well-typed initializer in the full closed
  grammar (including fallible operations) either evaluates to a valid value of its static type or
  reports a located failure of a fallible sub-expression (`ConstFailure.InExpr`), never `.invalid`
  or `.outOfFuel`.
- `eval_total`: totality for the fallible-free fragment, derived from `eval_wellTyped_progress`.
- `evalConsts_sound`: every declaration in a statically typed list that evaluates to `env` has a
  valid value in `env` of its declared type.
- `typed_evalConsts_refusal`: with sufficient fuel for every initializer (used or unused), a
  statically typed list either evaluates to `.ok env` or refuses with `.error (.operation d.id f)`
  for a declared constant `d ∈ ds` and a located failure `f.InExpr d.init`.
- `admitted_copy`: every declaration in an admitted list has a `Copy` declared type, static scalar
  type, and evaluated value type, derived from `U.ofC T = d.ty` and `U.ofC_copy` without a runtime
  `isCopy` guard.
- `typeConsts_refuses_nonCopy`: any declaration list containing a non-`Copy` declared type is
  statically refused by `typeConsts`.
- `cycle_excludes_dependencyOrder`: a real reference cycle (`CycleChain`) precludes every
  `DependencyOrder`.
- `DependencySortingComplete`: every name-resolved acyclic declaration list has a `DependencyOrder`,
  constructed by `sortConsts`.
- `ConstOrderIndependent`: any two dependency orders of a source list agree on static typing,
  evaluation success, admission, and the computed value of every constant identity.
-/

namespace ConcreteSpec.Core

/-- The earlier constants' values agree with their types, and are valid. -/
def Agree (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  ∀ c T, AList.lookup Γ c = some T → ∃ v, AList.lookup env c = some v ∧ v.ty = T ∧ v.Valid

/-- Helper (computation). Empty environments trivially agree. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem agree_nil : Agree [] [] := by
  intro _ _ h
  cases h

/-- Helper (computation). Every evaluator result has an independent syntax-directed derivation,
proved by induction on fuel and case analysis on the expression and primitive outcomes.
(approved: REV-C2-agy-59d4c501d) -/
private theorem eval_derivation_inv (env : AList ConstId CVal) :
    ∀ (fuel : Nat) (e : CExpr), ConstEval env fuel e (e.eval env fuel)
  | 0, e => .exhausted e
  | fuel + 1, .int t n => by
    simp only [CExpr.eval]
    split
    · rename_i h; exact .int_ok h
    · rename_i h; exact .int_outOfRange h
  | fuel + 1, .bool b => .bool fuel b
  | fuel + 1, .unit => .unit fuel
  | fuel + 1, .ref c => by
    simp only [CExpr.eval]
    split
    · rename_i v h; exact .ref_ok h
    · rename_i h; exact .ref_missing h
  | fuel + 1, .plainNeg a => by
    have iha := eval_derivation_inv env fuel a
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases va with
      | int t x =>
        cases hop : Core.plainNeg t x with
        | ok n =>
          simp only [CExpr.eval, ha, CEvalResult.bind, hop]
          exact .plainNeg_ok iha hop
        | trap k =>
          simp only [CExpr.eval, ha, CEvalResult.bind, hop]
          exact .plainNeg_failure iha hop
        | stuck => exact (plainNeg_not_stuck t x hop).elim
      | bool _ | unit =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .intUnary_invalid (.plainNeg a) iha (by intro _ _ h; cases h)
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .unary_nonValue (.plainNeg a) iha trivial
  | fuel + 1, .bitNot a => by
    have iha := eval_derivation_inv env fuel a
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases va with
      | int t x =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .bitNot_ok iha
      | bool _ | unit =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .intUnary_invalid (.bitNot a) iha (by intro _ _ h; cases h)
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .unary_nonValue (.bitNot a) iha trivial
  | fuel + 1, .not a => by
    have iha := eval_derivation_inv env fuel a
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases va with
      | bool x =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .not_ok iha
      | int _ _ | unit =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .not_invalid iha (by intro _ h; cases h)
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .unary_nonValue (.not a) iha trivial
  | fuel + 1, .convert t a => by
    have iha := eval_derivation_inv env fuel a
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases va with
      | int s x =>
        by_cases hloss : Lossless s t
        · simp only [CExpr.eval, ha, CEvalResult.bind, hloss, ↓reduceIte]
          exact .convert_int_ok iha hloss
        · simp only [CExpr.eval, ha, CEvalResult.bind, hloss, ↓reduceIte]
          exact .convert_int_invalid iha hloss
      | bool b =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .convert_bool_ok iha
      | unit =>
        simp only [CExpr.eval, ha, CEvalResult.bind]
        exact .convert_unit_invalid iha
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .unary_nonValue (.convert t a) iha trivial
  | fuel + 1, .wrapping op a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | int t x =>
          cases vb with
          | int t' y =>
            by_cases htt : t = t'
            · subst htt
              simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte]
              exact .wrapping_ok iha ihb
            · simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, htt, ↓reduceIte]
              exact .intBinary_invalid (.wrapping op a b) iha ihb (by
                intro t₀ x₀ y₀
                by_cases h1 : t = t₀
                · right; intro h2; cases h2; exact htt h1
                · left; intro h2; cases h2; exact h1 rfl)
          | bool _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
            exact .intBinary_invalid (.wrapping op a b) iha ihb (by intro _ _ _; right; intro h; cases h)
        | bool _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
          exact .intBinary_invalid (.wrapping op a b) iha ihb (by intro _ _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.wrapping op a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.wrapping op a b) iha trivial
  | fuel + 1, .saturating op a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | int t x =>
          cases vb with
          | int t' y =>
            by_cases htt : t = t'
            · subst htt
              simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte]
              exact .saturating_ok iha ihb
            · simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, htt, ↓reduceIte]
              exact .intBinary_invalid (.saturating op a b) iha ihb (by
                intro t₀ x₀ y₀
                by_cases h1 : t = t₀
                · right; intro h2; cases h2; exact htt h1
                · left; intro h2; cases h2; exact h1 rfl)
          | bool _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
            exact .intBinary_invalid (.saturating op a b) iha ihb (by intro _ _ _; right; intro h; cases h)
        | bool _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
          exact .intBinary_invalid (.saturating op a b) iha ihb (by intro _ _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.saturating op a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.saturating op a b) iha trivial
  | fuel + 1, .plain op a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | int t x =>
          cases vb with
          | int t' y =>
            by_cases htt : t = t'
            · subst htt
              cases hop : Core.plain t op x y with
              | ok n =>
                simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte, hop]
                exact .plain_ok iha ihb hop
              | trap k =>
                simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte, hop]
                exact .plain_failure iha ihb hop
              | stuck => exact (plain_not_stuck t op x y hop).elim
            · simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, htt, ↓reduceIte]
              exact .intBinary_invalid (.plain op a b) iha ihb (by
                intro t₀ x₀ y₀
                by_cases h1 : t = t₀
                · right; intro h2; cases h2; exact htt h1
                · left; intro h2; cases h2; exact h1 rfl)
          | bool _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
            exact .intBinary_invalid (.plain op a b) iha ihb (by intro _ _ _; right; intro h; cases h)
        | bool _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
          exact .intBinary_invalid (.plain op a b) iha ihb (by intro _ _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.plain op a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.plain op a b) iha trivial
  | fuel + 1, .bit op a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | int t x =>
          cases vb with
          | int t' y =>
            by_cases htt : t = t'
            · subst htt
              simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte]
              exact .bit_ok iha ihb
            · simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, htt, ↓reduceIte]
              exact .intBinary_invalid (.bit op a b) iha ihb (by
                intro t₀ x₀ y₀
                by_cases h1 : t = t₀
                · right; intro h2; cases h2; exact htt h1
                · left; intro h2; cases h2; exact h1 rfl)
          | bool _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
            exact .intBinary_invalid (.bit op a b) iha ihb (by intro _ _ _; right; intro h; cases h)
        | bool _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
          exact .intBinary_invalid (.bit op a b) iha ihb (by intro _ _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.bit op a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.bit op a b) iha trivial
  | fuel + 1, .cmp op a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | int t x =>
          cases vb with
          | int t' y =>
            by_cases htt : t = t'
            · subst htt
              simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, ↓reduceIte]
              exact .cmp_ok iha ihb
            · simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt, htt, ↓reduceIte]
              exact .intBinary_invalid (.cmp op a b) iha ihb (by
                intro t₀ x₀ y₀
                by_cases h1 : t = t₀
                · right; intro h2; cases h2; exact htt h1
                · left; intro h2; cases h2; exact h1 rfl)
          | bool _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
            exact .intBinary_invalid (.cmp op a b) iha ihb (by intro _ _ _; right; intro h; cases h)
        | bool _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, sameInt]
          exact .intBinary_invalid (.cmp op a b) iha ihb (by intro _ _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.cmp op a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.cmp op a b) iha trivial
  | fuel + 1, .and a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | bool x =>
          cases vb with
          | bool y =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
            exact .and_ok iha ihb
          | int _ _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
            exact .boolBinary_invalid (.and a b) iha ihb (by intro _ _; right; intro h; cases h)
        | int _ _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
          exact .boolBinary_invalid (.and a b) iha ihb (by intro _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.and a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.and a b) iha trivial
  | fuel + 1, .or a b => by
    have iha := eval_derivation_inv env fuel a
    have ihb := eval_derivation_inv env fuel b
    cases ha : a.eval env fuel with
    | value va =>
      rw [ha] at iha
      cases hb : b.eval env fuel with
      | value vb =>
        rw [hb] at ihb
        cases va with
        | bool x =>
          cases vb with
          | bool y =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
            exact .or_ok iha ihb
          | int _ _ | unit =>
            simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
            exact .boolBinary_invalid (.or a b) iha ihb (by intro _ _; right; intro h; cases h)
        | int _ _ | unit =>
          simp only [CExpr.eval, ha, hb, CEvalResult.bind, twoBools]
          exact .boolBinary_invalid (.or a b) iha ihb (by intro _ _; left; intro h; cases h)
      | failure _ | outOfFuel | invalid _ =>
        rw [hb] at ihb
        simp only [CExpr.eval, ha, hb, CEvalResult.bind]
        exact .binary_nonValue_right (.or a b) iha ihb trivial
    | failure _ | outOfFuel | invalid _ =>
      rw [ha] at iha
      simp only [CExpr.eval, ha, CEvalResult.bind]
      exact .binary_nonValue_left (.or a b) iha trivial

/-- Helper (inversion). Independent syntax-directed derivations determine the evaluator result.
(approved: REV-C2-agy-59d4c501d) -/
private theorem eval_derivation_unique_inv {env : AList ConstId CVal} {fuel : Nat}
    {e : CExpr} {r : CEvalResult} (h : ConstEval env fuel e r) : e.eval env fuel = r := by
  induction h with
  | exhausted e => rfl
  | int_ok h => simp [CExpr.eval, h]
  | int_outOfRange h => simp [CExpr.eval, h]
  | bool fuel b => rfl
  | unit fuel => rfl
  | ref_ok h => simp [CExpr.eval, h]
  | ref_missing h => simp [CExpr.eval, h]
  | @unary_nonValue fuel e a r hchild ha hr iha =>
    cases hchild <;> cases r <;> simp_all [CExpr.eval, CEvalResult.bind, CEvalResult.NonValue]
  | @binary_nonValue_left fuel e a b r hchild ha hr iha =>
    cases hchild <;> cases r <;> simp_all [CExpr.eval, CEvalResult.bind, CEvalResult.NonValue]
  | @binary_nonValue_right fuel e a b va r hchild ha hb hr iha ihb =>
    cases hchild <;> cases r <;> simp_all [CExpr.eval, CEvalResult.bind, CEvalResult.NonValue]
  | wrapping_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb]
  | saturating_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb]
  | plain_ok _ _ hop iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb, hop]
  | plain_failure _ _ hop iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb, hop]
  | plainNeg_ok _ hop iha => simp [CExpr.eval, CEvalResult.bind, iha, hop]
  | plainNeg_failure _ hop iha => simp [CExpr.eval, CEvalResult.bind, iha, hop]
  | bit_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb]
  | bitNot_ok _ iha => simp [CExpr.eval, CEvalResult.bind, iha]
  | cmp_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, sameInt, iha, ihb]
  | not_ok _ iha => simp [CExpr.eval, CEvalResult.bind, iha]
  | and_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, twoBools, iha, ihb]
  | or_ok _ _ iha ihb => simp [CExpr.eval, CEvalResult.bind, twoBools, iha, ihb]
  | convert_int_ok _ hloss iha => simp [CExpr.eval, CEvalResult.bind, iha, hloss]
  | convert_int_invalid _ hloss iha => simp [CExpr.eval, CEvalResult.bind, iha, hloss]
  | convert_bool_ok _ iha => simp [CExpr.eval, CEvalResult.bind, iha]
  | convert_unit_invalid _ iha => simp [CExpr.eval, CEvalResult.bind, iha]
  | @intUnary_invalid fuel e a va hchild ha hnot iha =>
    cases hchild <;> cases va with
    | int t x => exact (hnot t x rfl).elim
    | bool _ | unit => simp [CExpr.eval, CEvalResult.bind, iha]
  | @not_invalid fuel a va ha hnot iha =>
    cases va with
    | bool b => exact (hnot b rfl).elim
    | int _ _ | unit => simp [CExpr.eval, CEvalResult.bind, iha]
  | @intBinary_invalid fuel e a b va vb hchild ha hb hnot iha ihb =>
    have hnone : sameInt (some va) (some vb) = none := by
      cases va with
      | int t x =>
        cases vb with
        | int t' y =>
          by_cases htt : t = t'
          · subst htt; rcases hnot t x y with h | h <;> exact (h rfl).elim
          · simp [sameInt, htt]
        | bool _ | unit => rfl
      | bool _ | unit => rfl
    cases hchild <;> simp [CExpr.eval, CEvalResult.bind, iha, ihb, hnone]
  | @boolBinary_invalid fuel e a b va vb hchild ha hb hnot iha ihb =>
    have hnone : twoBools (some va) (some vb) = none := by
      cases va with
      | bool x =>
        cases vb with
        | bool y => rcases hnot x y with h | h <;> exact (h rfl).elim
        | int _ _ | unit => rfl
      | int _ _ | unit => rfl
    cases hchild <;> simp [CExpr.eval, CEvalResult.bind, iha, ihb, hnone]

/-- Law (subject `ConcreteSpec.Core.CExpr.eval`). The evaluator agrees exactly with independent
primitive-grounded semantics, including failures, invented failures, and fuel exhaustion. -/
theorem eval_agrees (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) :
    ConstEval env fuel e (e.eval env fuel) ∧
    ∀ r, ConstEval env fuel e r → e.eval env fuel = r := by
  exact ⟨eval_derivation_inv env fuel e, fun _ h => eval_derivation_unique_inv h⟩

/-- Helper (inversion). Successful binding exposes both successful computations.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem bind_value_inv (r : CEvalResult) (f : CVal → CEvalResult) (v : CVal) :
    r.bind f = .value v ↔ ∃ a, r = .value a ∧ f a = .value v := by
  cases r <;> simp [CEvalResult.bind]

/-- Helper (inversion). Integer operand extraction. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem sameInt_some_inv {x y : CVal} {t : IntTy} {a b : Int}
    (h : sameInt (some x) (some y) = some (t, a, b)) :
    x = .int t a ∧ y = .int t b := by
  cases x <;> cases y <;> simp [sameInt] at h
  obtain ⟨rfl, rfl, rfl, rfl⟩ := h
  exact ⟨rfl, rfl⟩

/-- Helper (inversion). Boolean operand extraction. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem twoBools_some_inv {x y : CVal} {a b : Bool}
    (h : twoBools (some x) (some y) = some (a, b)) :
    x = .bool a ∧ y = .bool b := by
  cases x <;> cases y <;> simp [twoBools] at h
  obtain ⟨rfl, rfl⟩ := h
  exact ⟨rfl, rfl⟩

/-- Helper (computation). Structural preservation for successful initializer evaluation;
used by declaration soundness rather than counted as independent evidence. (approved: REV-C2-agy-59d4c501d) -/
private theorem eval_sound_inv (env : AList ConstId CVal) (Γ : AList ConstId CTy)
    (hm : ∀ c v, AList.lookup env c = some v → AList.lookup Γ c = some v.ty ∧ v.Valid) :
    ∀ (e : CExpr) (fuel : Nat) (v : CVal), e.eval env fuel = .value v →
      e.ty? Γ = some v.ty ∧ v.Valid := by
  intro e
  induction e with
  | int t n =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval] at h
      split at h
      · rename_i hr; cases h; exact ⟨by simp [CExpr.ty?, hr, CVal.ty], hr⟩
      · cases h
  | bool b =>
    intro fuel v h; cases fuel <;> cases h
    exact ⟨rfl, trivial⟩
  | unit =>
    intro fuel v h; cases fuel <;> cases h
    exact ⟨rfl, trivial⟩
  | ref c =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval] at h
      split at h
      · rename_i w hw; cases h; exact hm c _ hw
      · cases h
  | wrapping op a b iha ihb | saturating op a b iha ihb | bit op a b iha ihb
  | cmp op a b iha ihb | plain op a b iha ihb =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, vb, hb, h⟩ := h
      split at h
      · rename_i t x y hs
        obtain ⟨rfl, rfl⟩ := sameInt_some_inv hs
        obtain ⟨hta, hva⟩ := iha fuel _ ha
        obtain ⟨htb, _⟩ := ihb fuel _ hb
        first
        | cases h; exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, sameIntTy], wrap_inRange _ _⟩
        | cases h; exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, sameIntTy], saturate_inRange _ _⟩
        | cases h; exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, sameIntTy], bitwise_inRange _ _ _ _⟩
        | cases h; exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, sameIntTy], trivial⟩
        | split at h
          · rename_i r hr; cases h
            exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, sameIntTy],
              checked_inRange t op x y r hva ((plain_ok_iff t op x y r).1 hr)⟩
          · cases h
          · cases h
      · cases h
  | and a b iha ihb | or a b iha ihb =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, vb, hb, h⟩ := h
      split at h
      · rename_i x y hs
        obtain ⟨rfl, rfl⟩ := twoBools_some_inv hs
        obtain ⟨hta, _⟩ := iha fuel _ ha
        obtain ⟨htb, _⟩ := ihb fuel _ hb
        cases h
        exact ⟨by simp [CExpr.ty?, hta, htb, CVal.ty, bothBool], trivial⟩
      · cases h
  | bitNot a ih =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, h⟩ := h
      obtain ⟨hta, hva⟩ := ih fuel va ha
      cases va with
      | int src x =>
        cases h
        exact ⟨by simp [CExpr.ty?, hta, CVal.ty], bitNot_inRange _ _⟩
      | bool b =>
        cases h
      | unit => cases h
  | not a ih =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, h⟩ := h
      obtain ⟨hta, hva⟩ := ih fuel va ha
      cases va with
      | int src x =>
        cases h
      | bool b =>
        cases h
        exact ⟨by simp [CExpr.ty?, hta, CVal.ty], trivial⟩
      | unit => cases h
  | plainNeg a ih =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, h⟩ := h
      obtain ⟨hta, hva⟩ := ih fuel va ha
      cases va with
      | int src x =>
        simp only at h
        cases hr : plainNeg src x with
        | ok r =>
          simp only [hr] at h; cases h
          exact ⟨by simp [CExpr.ty?, hta, CVal.ty],
            inRangeOr_inRange ((plainNeg_ok_iff _ _ _).1 hr)⟩
        | trap k => simp [hr] at h
        | stuck => simp [hr] at h
      | bool b =>
        cases h
      | unit => cases h
  | convert t a ih =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, h⟩ := h
      obtain ⟨hta, hva⟩ := ih fuel va ha
      cases va with
      | int src x =>
        simp only at h
        split at h
        · rename_i hr; cases h
          exact ⟨by simp [CExpr.ty?, hta, CVal.ty, hr], (lossless_iff _ _).1 hr _ hva⟩
        · cases h
      | bool b =>
        cases h
        exact ⟨by simp [CExpr.ty?, hta, CVal.ty], boolToInt_inRange _ _⟩
      | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem CVal.int_of_ty {v : CVal} {t : IntTy} (h : v.ty = .int t) : ∃ n, v = .int t n := by
  cases v with
  | int t' n => cases h; exact ⟨n, rfl⟩
  | bool _ => cases h
  | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem CVal.bool_of_ty {v : CVal} (h : v.ty = .bool) : ∃ b, v = .bool b := by
  cases v with
  | int _ _ => cases h
  | bool b => exact ⟨b, rfl⟩
  | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem sameIntTy_some {x y : Option CTy} {t : IntTy} (h : sameIntTy x y = some t) :
    x = some (.int t) ∧ y = some (.int t) := by
  match x, y, h with
  | some (.int a), some (.int b), h =>
    simp only [sameIntTy] at h
    split at h
    · cases h; rename_i hab; subst hab; exact ⟨rfl, rfl⟩
    · cases h

/-- Helper (inversion). Operand typing inversion. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem bothBool_true {x y : Option CTy} (h : bothBool x y = true) :
    x = some .bool ∧ y = some .bool := by
  match x, y, h with
  | some .bool, some .bool, _ => exact ⟨rfl, rfl⟩

/-- Helper (inversion). A fallible-free expression contains no located operation failure.
(approved: REV-C2-agy-59d4c501d) -/
private theorem not_inExpr_of_fallibleFree (f : ConstFailure) :
    ∀ e : CExpr, e.fallibleFree = true → ¬ f.InExpr e := by
  intro e
  induction e with
  | int _ _ | bool _ | unit | ref _ =>
    intro _ hf; nomatch hf
  | wrapping _ a b iha ihb | saturating _ a b iha ihb | bit _ a b iha ihb
  | cmp _ a b iha ihb | and a b iha ihb | or a b iha ihb =>
    intro hff hf
    simp only [CExpr.fallibleFree, Bool.and_eq_true] at hff
    cases hf with
    | inl h => exact iha hff.1 h
    | inr h => exact ihb hff.2 h
  | bitNot a iha | not a iha | convert _ a iha =>
    intro hff hf
    exact iha hff hf
  | plain _ _ _ | plainNeg _ =>
    intro hff
    simp [CExpr.fallibleFree] at hff

/-- Law (subject `ConcreteSpec.Core.CExpr.eval`). With sufficient fuel, a well-typed initializer in
the full closed grammar (including fallible operations) never runs out of fuel and never fails
static validation (.invalid): it either produces a valid value of its static type or reports a
located failure of a fallible sub-expression. -/
theorem eval_wellTyped_progress (env : AList ConstId CVal) (Γ : AList ConstId CTy)
    (hag : Agree env Γ) :
    ∀ (e : CExpr) (T : CTy), e.ty? Γ = some T →
      ∀ fuel, e.height ≤ fuel →
        (∃ v, e.eval env fuel = .value v ∧ v.ty = T ∧ v.Valid) ∨
        (∃ f, e.eval env fuel = .failure f ∧ f.InExpr e)
  | .int t n, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      simp only [CExpr.ty?] at h
      split at h
      · cases h
        exact Or.inl ⟨.int t n, by simp [CExpr.eval, *], rfl, by assumption⟩
      · cases h
  | .bool b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      cases h; exact Or.inl ⟨.bool b, rfl, rfl, trivial⟩
  | .unit, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      cases h; exact Or.inl ⟨.unit, rfl, rfl, trivial⟩
  | .ref c, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      obtain ⟨v, hv, hty, hvalid⟩ := hag c T h
      exact Or.inl ⟨v, by simp [CExpr.eval, hv], hty, hvalid⟩
  | .wrapping op a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
      · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
          exact Or.inl ⟨.int t (Core.wrapping t op x y),
            by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl, wrap_inRange t _⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
      · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
  | .saturating op a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
      · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
          exact Or.inl ⟨.int t (Core.saturating t op x y),
            by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl, saturate_inRange t _⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
      · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
  | .plain op a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, hva⟩ | ⟨f, hea, hfa⟩
      · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
          cases hr : Core.plain t op x y with
          | ok n =>
            exact Or.inl ⟨.int t n,
              by simp [CExpr.eval, hea, heb, sameInt, hr, CEvalResult.bind], rfl,
              checked_inRange t op x y n hva ((plain_ok_iff t op x y n).1 hr)⟩
          | trap k =>
            exact Or.inr ⟨⟨.plain op, .plain op a b, .arithmetic k⟩,
              by simp [CExpr.eval, hea, heb, sameInt, hr, CEvalResult.bind],
              Or.inl ⟨k, rfl⟩⟩
          | stuck =>
            exact (plain_not_stuck t op x y hr).elim
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr (Or.inr hfb)⟩
      · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inr (Or.inl hfa)⟩
  | .plainNeg a, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i t ha
        cases h
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hna with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          cases hr : Core.plainNeg t x with
          | ok n =>
            exact Or.inl ⟨.int t n,
              by simp [CExpr.eval, hea, hr, CEvalResult.bind], rfl,
              inRangeOr_inRange ((plainNeg_ok_iff t x n).1 hr)⟩
          | trap k =>
            exact Or.inr ⟨⟨.plainNeg, .plainNeg a, .arithmetic k⟩,
              by simp [CExpr.eval, hea, hr, CEvalResult.bind],
              Or.inl ⟨k, rfl⟩⟩
          | stuck =>
            exact (plainNeg_not_stuck t x hr).elim
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inr hfa⟩
      · cases h
  | .bit op a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
      · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
          exact Or.inl ⟨.int t (Core.bitwise t op x y),
            by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl, bitwise_inRange t op x y⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
      · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
  | .bitNot a, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i t ha
        cases h
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hna with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          exact Or.inl ⟨.int t (Core.bitNot t x),
            by simp [CExpr.eval, hea, CEvalResult.bind], rfl, bitNot_inRange t x⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], hfa⟩
      · cases h
  | .cmp op a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
      · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
        · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
          exact Or.inl ⟨.bool (op.eval x y),
            by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl, trivial⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
      · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
  | .not a, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i ha
        cases h
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hna with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
          exact Or.inl ⟨.bool (!x), by simp [CExpr.eval, hea, CEvalResult.bind], rfl, trivial⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], hfa⟩
      · cases h
  | .and a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i hbb
        cases h
        obtain ⟨ha, hb⟩ := bothBool_true hbb
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
          · obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
            obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
            exact Or.inl ⟨.bool (x && y),
              by simp [CExpr.eval, hea, heb, twoBools, CEvalResult.bind], rfl, trivial⟩
          · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
      · cases h
  | .or a b, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i hbb
        cases h
        obtain ⟨ha, hb⟩ := bothBool_true hbb
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hns.1 with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · rcases eval_wellTyped_progress env Γ hag b _ hb fuel hns.2 with ⟨vb, heb, htb, _⟩ | ⟨f, heb, hfb⟩
          · obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
            obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
            exact Or.inl ⟨.bool (x || y),
              by simp [CExpr.eval, hea, heb, twoBools, CEvalResult.bind], rfl, trivial⟩
          · exact Or.inr ⟨f, by simp [CExpr.eval, hea, heb, CEvalResult.bind], Or.inr hfb⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], Or.inl hfa⟩
      · cases h
  | .convert t a, T, h, fuel, hn => by
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i s ha
        split at h
        · rename_i hl
          cases h
          rcases eval_wellTyped_progress env Γ hag a _ ha fuel hna with ⟨va, hea, hta, hva⟩ | ⟨f, hea, hfa⟩
          · obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
            exact Or.inl ⟨.int t x,
              by simp [CExpr.eval, hea, hl, CEvalResult.bind], rfl, (lossless_iff s t).1 hl x hva⟩
          · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], hfa⟩
        · cases h
      · rename_i ha
        cases h
        rcases eval_wellTyped_progress env Γ hag a _ ha fuel hna with ⟨va, hea, hta, _⟩ | ⟨f, hea, hfa⟩
        · obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
          exact Or.inl ⟨.int t (boolToInt x),
            by simp [CExpr.eval, hea, CEvalResult.bind], rfl, boolToInt_inRange t x⟩
        · exact Or.inr ⟨f, by simp [CExpr.eval, hea, CEvalResult.bind], hfa⟩
      · cases h

/-- A deliberately broken evaluator, retaining the exact evaluator interface. -/
def noConstValues (_env : AList ConstId CVal) (_fuel : Nat) (_e : CExpr) : CEvalResult :=
  .outOfFuel

/-- Instance (law `ConcreteSpec.Core.eval_wellTyped_progress`, case `div_zero`). -/
example : (∃ v, (CExpr.plain .div (.int .i8 1) (.int .i8 0)).eval [] 2 = .value v ∧
      v.ty = .int .i8 ∧ v.Valid) ∨
    (∃ f, (CExpr.plain .div (.int .i8 1) (.int .i8 0)).eval [] 2 = .failure f ∧
      f.InExpr (.plain .div (.int .i8 1) (.int .i8 0))) :=
  eval_wellTyped_progress [] [] agree_nil
    (.plain .div (.int .i8 1) (.int .i8 0)) (.int .i8) (by decide) 2 (by decide)

/-- Planted (law `ConcreteSpec.Core.eval_wellTyped_progress`, case `div_zero`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.noConstValues`). -/
example : Agree [] [] ∧
    (CExpr.plain .div (.int .i8 1) (.int .i8 0)).ty? [] = some (.int .i8) ∧
    (CExpr.plain .div (.int .i8 1) (.int .i8 0)).height ≤ 2 ∧
    ¬ ((∃ v, noConstValues [] 2 (.plain .div (.int .i8 1) (.int .i8 0)) = .value v ∧
        v.ty = .int .i8 ∧ v.Valid) ∨
      (∃ f, noConstValues [] 2 (.plain .div (.int .i8 1) (.int .i8 0)) = .failure f ∧
        f.InExpr (.plain .div (.int .i8 1) (.int .i8 0)))) := by
  refine ⟨agree_nil, by decide, by decide, ?_⟩
  simp [noConstValues]

/-- Law (subject `ConcreteSpec.Core.CExpr.eval`). The old fragment evaluates with sufficient
fuel; static typing supplies the result type and scalar validity. -/
theorem eval_total (env : AList ConstId CVal) (Γ : AList ConstId CTy) (hag : Agree env Γ) :
    ∀ (e : CExpr) (T : CTy), e.ty? Γ = some T → e.fallibleFree = true →
      ∀ fuel, e.height ≤ fuel → ∃ v, e.eval env fuel = .value v ∧ v.ty = T ∧ v.Valid := by
  intro e T hty hff fuel hfuel
  rcases eval_wellTyped_progress env Γ hag e T hty fuel hfuel with hval | ⟨f, _, hfe⟩
  · exact hval
  · exact (not_inExpr_of_fallibleFree f e hff hfe).elim

/-- Instance (law `ConcreteSpec.Core.eval_total`, case `wrap`). -/
example : ∃ v, (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).eval [] 2 = .value v ∧
    v.ty = .int .u8 ∧ v.Valid :=
  eval_total [] [] agree_nil
    (.wrapping .add (.int .u8 255) (.int .u8 1)) (.int .u8) (by decide) (by decide) 2 (by decide)

/-- Planted (law `ConcreteSpec.Core.eval_total`, case `wrap`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.noConstValues`). -/
example : Agree [] [] ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).ty? [] = some (.int .u8) ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).fallibleFree = true ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).height ≤ 2 ∧
    ¬ ∃ v, noConstValues [] 2 (.wrapping .add (.int .u8 255) (.int .u8 1)) = .value v ∧
      v.ty = .int .u8 ∧ v.Valid := by
  refine ⟨agree_nil, by decide, by decide, by decide, ?_⟩
  simp [noConstValues]

/-! ### Declaration typing, evaluation soundness, refusal progress, and `Copy` derivation -/

/-- Helper (computation). Static typing preserves previous type bindings because declaration
names must be fresh. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_mono_inv (U : ConstTypes) : ∀ ds Γ Γ',
    typeConsts U ds Γ = some Γ' → ∀ c T, AList.lookup Γ c = some T →
      AList.lookup Γ' c = some T
  | [], _, _, h, c, T, hc => by cases h; exact hc
  | d :: ds, Γ, Γ', h, c, T, hc => by
    simp only [typeConsts] at h
    split at h
    · split at h
      · rename_i t ht cond
        apply typeConsts_mono_inv U ds _ Γ' h c T
        have hne : d.id ≠ c := by intro he; rw [he, hc] at cond; simp at cond
        simp [AList.lookup, hne, hc]
      · cases h
    · cases h

/-- Helper (computation). Static declarations determine the type binding for each declared
identity, independently of evaluation. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_decls_inv (U : ConstTypes) : ∀ ds Γ Γ',
    typeConsts U ds Γ = some Γ' → ∀ d ∈ ds,
      ∃ T, AList.lookup Γ' d.id = some T ∧ U.ofC T = d.ty
  | [], _, _, _, _, hd => nomatch hd
  | d :: ds, Γ, Γ', h, d', hd => by
    simp only [typeConsts] at h
    split at h
    · rename_i T ht
      split at h
      · rename_i cond
        cases hd with
        | head => exact ⟨T, typeConsts_mono_inv U ds _ Γ' h d.id T (by simp [AList.lookup]),
            cond.1⟩
        | tail _ hm => exact typeConsts_decls_inv U ds _ Γ' h d' hm
      · cases h
    · cases h

/-- The value environment matches the static type environment in both directions. -/
def Matches (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  ∀ c, (AList.lookup env c).map CVal.ty = AList.lookup Γ c ∧
    ∀ v, AList.lookup env c = some v → v.Valid

/-- Helper (computation). Independent typing and evaluation produce matching environments.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem typed_eval_matches_inv (U : ConstTypes) (fuel : Nat) : ∀ ds env env' Γ Γ',
    Matches env Γ → typeConsts U ds Γ = some Γ' → evalConsts U fuel ds env = .ok env' →
    Matches env' Γ'
  | [], _, _, _, _, hm, ht, he => by cases ht; cases he; exact hm
  | d :: ds, env, env', Γ, Γ', hm, ht, he => by
    simp only [typeConsts] at ht
    split at ht
    · rename_i T hT
      split at ht
      · simp only [evalConsts] at he
        split at he
        · rename_i v hv
          have henv : ∀ c v, AList.lookup env c = some v →
              AList.lookup Γ c = some v.ty ∧ v.Valid := by
            intro c v h
            exact ⟨by rw [← (hm c).1, h]; rfl, (hm c).2 v h⟩
          obtain ⟨hvt, hvvalid⟩ := eval_sound_inv env Γ henv d.init fuel v hv
          have hty : v.ty = T := by rw [hT] at hvt; exact (Option.some.inj hvt).symm
          apply typed_eval_matches_inv U fuel ds _ env' _ Γ' ?_ ht he
          intro c
          by_cases hc : d.id = c
          · subst c; simp [AList.lookup, hty, hvvalid]
          · simpa [AList.lookup, hc] using hm c
        · cases he
        · cases he
        · cases he
      · cases ht
    · cases ht

/-- Helper (computation). Independent static typing and evaluation imply
that every declaration has a valid value of its declared type. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_sound_inv (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) (Γ : AList ConstId CTy)
    (ht : typeConsts U ds [] = some Γ) (he : evalConsts U fuel ds [] = .ok env) :
    ∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧ U.ofC v.ty = d.ty ∧ v.Valid := by
  have hm := typed_eval_matches_inv U fuel ds [] env [] Γ
    (by intro c; exact ⟨rfl, by intro v h; cases h⟩) ht he
  intro d hd
  obtain ⟨T, hT, hty⟩ := typeConsts_decls_inv U ds [] Γ ht d hd
  have hv := (hm d.id).1
  rw [hT] at hv
  cases hc : AList.lookup env d.id with
  | none => simp [hc] at hv
  | some v =>
    simp only [hc, Option.map_some, Option.some.injEq] at hv
    exact ⟨v, rfl, hv ▸ hty, (hm d.id).2 v hc⟩

/-- Law (subject `ConcreteSpec.Core.evalConsts`). When a declaration list is statically typed and
evaluates within fuel, `evalConsts` produces a value environment env in which every declared
constant has a valid value of its declared type. -/
theorem evalConsts_sound (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (ht : ConstsTyped U ds) (he : ConstsEvaluate U fuel ds) :
    ∃ env, evalConsts U fuel ds [] = .ok env ∧
      ∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧ U.ofC v.ty = d.ty ∧ v.Valid := by
  unfold ConstsTyped at ht
  unfold ConstsEvaluate at he
  cases ht' : typeConsts U ds [] with
  | none => simp [ht'] at ht
  | some Γ =>
    cases he' : evalConsts U fuel ds [] with
    | error _ => rw [he'] at he; cases he
    | ok env => exact ⟨env, rfl, evalConsts_sound_inv U fuel ds env Γ ht' he'⟩

/-- A broken declaration evaluator that returns `bool` for every declaration. -/
def boolConstValues (U : ConstTypes) (_fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  .ok (ds.map (fun d => (d.id, CVal.bool true)) ++ env)

/-- A concrete successful division declaration. -/
def divisionDecls : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩]

/-- Instance (law `ConcreteSpec.Core.evalConsts_sound`, case `division`). -/
example : ∃ env, evalConsts ConstDemo.U 3 divisionDecls [] = .ok env ∧
    ∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
      ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid :=
  evalConsts_sound ConstDemo.U 3 divisionDecls (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.evalConsts_sound`, case `division`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.boolConstValues`). -/
example : ConstsTyped ConstDemo.U divisionDecls ∧
    (boolConstValues ConstDemo.U 3 divisionDecls []).isOk = true ∧
    ¬ (∃ env, boolConstValues ConstDemo.U 3 divisionDecls [] = .ok env ∧
      ∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
        ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨env, he, h⟩
  cases he
  obtain ⟨v, hv, hty, _⟩ := h ⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩
    (by simp [divisionDecls])
  simp only [divisionDecls, List.map, List.append_nil, AList.lookup,
    ↓reduceIte, Option.some.injEq] at hv
  change ConstDemo.U.ofC v.ty = .c (.int .i8) at hty
  subst v
  cases hty

/-- Helper (computation). Structural induction for declaration evaluation under sufficient fuel:
every well-typed list either evaluates to .ok env' or stops at a declared constant with a located
operation failure. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typed_evalConsts_refusal_inv (U : ConstTypes) (fuel : Nat) :
    ∀ (ds : List (ConstDecl U)) (env : AList ConstId CVal) (Γ Γ' : AList ConstId CTy),
      Agree env Γ →
      typeConsts U ds Γ = some Γ' →
      (∀ d ∈ ds, d.init.height ≤ fuel) →
      (∃ env', evalConsts U fuel ds env = .ok env') ∨
      (∃ d ∈ ds, ∃ f, evalConsts U fuel ds env = .error (.operation d.id f) ∧ f.InExpr d.init)
  | [], env, _, _, _, _, _ => Or.inl ⟨env, rfl⟩
  | d :: ds, env, Γ, Γ', hag, ht, hfuel => by
    simp only [typeConsts] at ht
    split at ht
    · rename_i T hT
      split at ht
      · have hd_fuel : d.init.height ≤ fuel := hfuel d (List.Mem.head _)
        rcases eval_wellTyped_progress env Γ hag d.init T hT fuel hd_fuel with
          ⟨v, hv, hty, hvalid⟩ | ⟨f, hf, hfe⟩
        · have hag' : Agree ((d.id, v) :: env) ((d.id, T) :: Γ) := by
            intro c T' hc
            by_cases hcd : d.id = c
            · subst c
              simp only [AList.lookup, ↓reduceIte, Option.some.injEq] at hc ⊢
              subst T'
              exact ⟨v, rfl, hty, hvalid⟩
            · simp only [AList.lookup, hcd, ↓reduceIte] at hc ⊢
              exact hag c T' hc
          rcases typed_evalConsts_refusal_inv U fuel ds ((d.id, v) :: env) ((d.id, T) :: Γ) Γ'
              hag' ht (fun d' hd' => hfuel d' (List.Mem.tail _ hd')) with
            ⟨env', he⟩ | ⟨d', hd', f, he, hfe⟩
          · exact Or.inl ⟨env', by simp [evalConsts, hv, he]⟩
          · exact Or.inr ⟨d', List.Mem.tail _ hd', f, by simp [evalConsts, hv, he], hfe⟩
        · exact Or.inr ⟨d, List.Mem.head _, f, by simp [evalConsts, hf], hfe⟩
      · cases ht
    · cases ht

/-- Law (subject `ConcreteSpec.Core.evalConsts`). With sufficient fuel for every initializer (used
or unused), declaration evaluation on a statically typed list never fails with .invalid or
.outOfFuel: it either produces .ok env or refuses with .error (.operation d.id f) for a
declared constant d ∈ ds and a located failure f.InExpr d.init. -/
theorem typed_evalConsts_refusal (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (ht : ConstsTyped U ds) (hfuel : ∀ d ∈ ds, d.init.height ≤ fuel) :
    (∃ env, evalConsts U fuel ds [] = .ok env) ∨
    (∃ d ∈ ds, ∃ f, evalConsts U fuel ds [] = .error (.operation d.id f) ∧ f.InExpr d.init) := by
  unfold ConstsTyped at ht
  cases ht' : typeConsts U ds [] with
  | none => simp [ht'] at ht
  | some Γ =>
    exact typed_evalConsts_refusal_inv U fuel ds [] [] Γ (by intro c T h; cases h) ht' hfuel

/-- A broken declaration evaluator that misclassifies an arithmetic failure as .invalid. -/
def invalidRefusalEvalConsts (U : ConstTypes) (_fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match ds with
  | [] => .ok env
  | d :: _ => .error (.invalid d.id d.init)

/-- Instance (law `ConcreteSpec.Core.typed_evalConsts_refusal`, case `unused_div_zero`). -/
example : (∃ env, evalConsts ConstDemo.U 2
      [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
       ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] = .ok env) ∨
    (∃ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
            ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
              (.plain .div (.int .i8 1) (.int .i8 0))],
      ∃ f, evalConsts ConstDemo.U 2
        [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
         ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
          .error (.operation d.id f) ∧ f.InExpr d.init) :=
  typed_evalConsts_refusal ConstDemo.U 2
    [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
     ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩]
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.typed_evalConsts_refusal`, case `unused_div_zero`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.invalidRefusalEvalConsts`). -/
example : ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
       ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] ∧
    (∀ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
            ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
              (.plain .div (.int .i8 1) (.int .i8 0))], d.init.height ≤ 2) ∧
    ¬ ((∃ env, invalidRefusalEvalConsts ConstDemo.U 2
          [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
           ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] = .ok env) ∨
      (∃ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "USED") (.c (.int .u8)) (.int .u8 1),
              ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "UNUSED") (.c (.int .i8))
                (.plain .div (.int .i8 1) (.int .i8 0))],
        ∃ f, invalidRefusalEvalConsts ConstDemo.U 2
          [⟨ConstDemo.cid "USED", .c (.int .u8), .int .u8 1⟩,
           ⟨ConstDemo.cid "UNUSED", .c (.int .i8), .plain .div (.int .i8 1) (.int .i8 0)⟩] [] =
            .error (.operation d.id f) ∧ f.InExpr d.init)) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro (⟨env, h⟩ | ⟨d, _, f, h, _⟩) <;> cases h

/-- A broken declaration typechecker that binds every declaration to `CTy.unit` without checking
declared-type agreement U.ofC T = d.ty. -/
def unitTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ =>
    if AList.lookup Γ d.id = none then
      unitTypeConsts U ds ((d.id, .unit) :: Γ)
    else none

/-- A broken declaration typechecker that omits the U.ofC T = d.ty check. -/
def uncheckedTypeConsts (U : ConstTypes) : List (ConstDecl U) → AList ConstId CTy →
    Option (AList ConstId CTy)
  | [], Γ => some Γ
  | d :: ds, Γ => match d.init.ty? Γ with
    | some T =>
      if AList.lookup Γ d.id = none then
        uncheckedTypeConsts U ds ((d.id, T) :: Γ)
      else none
    | none => none

/-- Law (subject `ConcreteSpec.Core.typeConsts`, `ConcreteSpec.Core.evalConsts`). Every admitted
declaration list produces a static type environment Γ and value environment env in which every
declaration's declared type, static scalar type, and evaluated value type are Copy in U,
derived structurally from U.ofC T = d.ty and U.ofC_copy without a runtime isCopy check. -/
theorem admitted_copy (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (ht : ConstsTyped U ds) (he : ConstsEvaluate U fuel ds) :
    ∃ Γ env, typeConsts U ds [] = some Γ ∧ evalConsts U fuel ds [] = .ok env ∧
      (∀ d ∈ ds, ∃ T, AList.lookup Γ d.id = some T ∧ U.ofC T = d.ty ∧ U.isCopy d.ty = true) ∧
      (∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧
        U.ofC v.ty = d.ty ∧ U.isCopy (U.ofC v.ty) = true) := by
  unfold ConstsTyped at ht
  unfold ConstsEvaluate at he
  cases ht' : typeConsts U ds [] with
  | none => simp [ht'] at ht
  | some Γ =>
    cases he' : evalConsts U fuel ds [] with
    | error _ => rw [he'] at he; cases he
    | ok env =>
      refine ⟨Γ, env, rfl, rfl, ?_, ?_⟩
      · intro d hd
        obtain ⟨T, hT, hty⟩ := typeConsts_decls_inv U ds [] Γ ht' d hd
        exact ⟨T, hT, hty, hty ▸ U.ofC_copy T⟩
      · intro d hd
        obtain ⟨v, hv, hty, _⟩ := evalConsts_sound_inv U fuel ds env Γ ht' he' d hd
        exact ⟨v, hv, hty, U.ofC_copy v.ty⟩

/-- Instance (law `ConcreteSpec.Core.admitted_copy`, case `division_type`). -/
example : ∃ Γ env, typeConsts ConstDemo.U divisionDecls [] = some Γ ∧
    evalConsts ConstDemo.U 3 divisionDecls [] = .ok env ∧
    (∀ d ∈ divisionDecls, ∃ T, AList.lookup Γ d.id = some T ∧
      ConstDemo.U.ofC T = d.ty ∧ ConstDemo.U.isCopy d.ty = true) ∧
    (∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
      ConstDemo.U.ofC v.ty = d.ty ∧ ConstDemo.U.isCopy (ConstDemo.U.ofC v.ty) = true) :=
  admitted_copy ConstDemo.U 3 divisionDecls (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.admitted_copy`, case `division_type`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.unitTypeConsts`). -/
example : (unitTypeConsts ConstDemo.U divisionDecls []).isSome = true ∧
    ConstsEvaluate ConstDemo.U 3 divisionDecls ∧
    ¬ (∃ Γ env, unitTypeConsts ConstDemo.U divisionDecls [] = some Γ ∧
      evalConsts ConstDemo.U 3 divisionDecls [] = .ok env ∧
      (∀ d ∈ divisionDecls, ∃ T, AList.lookup Γ d.id = some T ∧
        ConstDemo.U.ofC T = d.ty ∧ ConstDemo.U.isCopy d.ty = true) ∧
      (∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
        ConstDemo.U.ofC v.ty = d.ty ∧ ConstDemo.U.isCopy (ConstDemo.U.ofC v.ty) = true)) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨Γ, _, ht, _, hdecl, _⟩
  cases ht
  obtain ⟨T, hT, hty, _⟩ := hdecl
    ⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩
    (by simp [divisionDecls])
  simp only [AList.lookup, ↓reduceIte, Option.some.injEq] at hT
  change ConstDemo.U.ofC T = .c (.int .i8) at hty
  subst T
  cases hty

/-- Planted (law `ConcreteSpec.Core.admitted_copy`, case `division_type`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.boolConstValues`). -/
example : ConstsTyped ConstDemo.U divisionDecls ∧
    (boolConstValues ConstDemo.U 3 divisionDecls []).isOk = true ∧
    ¬ (∃ Γ env, typeConsts ConstDemo.U divisionDecls [] = some Γ ∧
      boolConstValues ConstDemo.U 3 divisionDecls [] = .ok env ∧
      (∀ d ∈ divisionDecls, ∃ T, AList.lookup Γ d.id = some T ∧
        ConstDemo.U.ofC T = d.ty ∧ ConstDemo.U.isCopy d.ty = true) ∧
      (∀ d ∈ divisionDecls, ∃ v, AList.lookup env d.id = some v ∧
        ConstDemo.U.ofC v.ty = d.ty ∧ ConstDemo.U.isCopy (ConstDemo.U.ofC v.ty) = true)) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, env, _, he, _, hval⟩
  cases he
  obtain ⟨v, hv, hty, _⟩ := hval
    ⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩
    (by simp [divisionDecls])
  simp only [divisionDecls, List.map, List.append_nil, AList.lookup,
    ↓reduceIte, Option.some.injEq] at hv
  change ConstDemo.U.ofC v.ty = .c (.int .i8) at hty
  subst v
  cases hty

/-- Law (subject `ConcreteSpec.Core.typeConsts`). Any declaration list containing a non-Copy
declared type is statically refused by `typeConsts`, derived from U.ofC T = d.ty and U.ofC_copy
without a runtime isCopy check. -/
theorem typeConsts_refuses_nonCopy (U : ConstTypes) (ds : List (ConstDecl U))
    (d : ConstDecl U) (hd : d ∈ ds) (hnc : U.isCopy d.ty = false) :
    typeConsts U ds [] = none := by
  cases ht : typeConsts U ds [] with
  | none => rfl
  | some Γ =>
    obtain ⟨T, _, hty⟩ := typeConsts_decls_inv U ds [] Γ ht d hd
    have hcopy : U.isCopy d.ty = true := hty ▸ U.ofC_copy T
    rw [hnc] at hcopy
    cases hcopy

/-- Scalar success followed by a mismatched non-Copy declaration. -/
def copyAuditDecls : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "Q", .c (.int .i8), .int .i8 1⟩,
   ⟨ConstDemo.cid "S", .text, .unit⟩]

/-- Instance (law `ConcreteSpec.Core.typeConsts_refuses_nonCopy`, case `mixed_declarations`). -/
example : typeConsts ConstDemo.U copyAuditDecls [] = none :=
  typeConsts_refuses_nonCopy ConstDemo.U copyAuditDecls
    ⟨ConstDemo.cid "S", .text, .unit⟩ (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.typeConsts_refuses_nonCopy`, case `mixed_declarations`,
subject `ConcreteSpec.Core.typeConsts`, mutant `ConcreteSpec.Core.uncheckedTypeConsts`). -/
example : ⟨ConstDemo.cid "S", .text, .unit⟩ ∈ copyAuditDecls ∧
    ConstDemo.U.isCopy (⟨ConstDemo.cid "S", .text, .unit⟩ : ConstDecl ConstDemo.U).ty = false ∧
    ¬ (uncheckedTypeConsts ConstDemo.U copyAuditDecls [] = none) := by
  refine ⟨by decide, rfl, ?_⟩
  decide

/-- Helper (inversion). Lift primitive-grounded declaration derivations to executable evaluation.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem constsRun_eval_inv {U : ConstTypes} {fuel : Nat} {ds : List (ConstDecl U)}
    {env : AList ConstId CVal} {r : Except ConstEvalRefusal (AList ConstId CVal)}
    (h : ConstsRun U fuel ds env r) : evalConsts U fuel ds env = r := by
  induction h with
  | nil env => rfl
  | value d ds env v result head tail ih =>
    simp [evalConsts, eval_derivation_unique_inv head, ih]
  | failure d ds env f head => simp [evalConsts, eval_derivation_unique_inv head]
  | exhausted d ds env head => simp [evalConsts, eval_derivation_unique_inv head]
  | invalid d ds env e head => simp [evalConsts, eval_derivation_unique_inv head]

/-- Helper (computation). Evaluation of a successful prefix composes with the remaining list.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_append_inv (U : ConstTypes) (fuel : Nat)
    (pre suffix : List (ConstDecl U)) (env out : AList ConstId CVal)
    (h : evalConsts U fuel pre env = .ok out) :
    evalConsts U fuel (pre ++ suffix) env = evalConsts U fuel suffix out := by
  induction pre generalizing env with
  | nil => cases h; rfl
  | cons d ds ih =>
    cases he : d.init.eval env fuel <;> simp only [evalConsts, he] at h
    case value v => exact (by simpa [evalConsts, he] using ih _ h)
    all_goals cases h

/-- Law (subject `ConcreteSpec.Core.evalConsts`, `ConcreteSpec.Core.ConstsAdmitted`). A
primitive-grounded failing initializer after any successful pre refuses the whole list with its
identity and failure, even if unused. There is no typing or usage guard: evaluation failure alone
prevents admission. -/
theorem semantic_failure_refuses (U : ConstTypes) (fuel : Nat)
    (pre suffix : List (ConstDecl U)) (d : ConstDecl U) (env : AList ConstId CVal)
    (f : ConstFailure) (hp : ConstsRun U fuel pre [] (.ok env))
    (hf : ConstEval env fuel d.init (.failure f)) :
    evalConsts U fuel (pre ++ d :: suffix) [] = .error (.operation d.id f) ∧
    ¬ ConstsAdmitted U fuel (pre ++ d :: suffix) := by
  have he : evalConsts U fuel (pre ++ d :: suffix) [] = .error (.operation d.id f) := by
    rw [evalConsts_append_inv U fuel pre (d :: suffix) [] env (constsRun_eval_inv hp)]
    simp [evalConsts, eval_derivation_unique_inv hf]
  refine ⟨he, ?_⟩
  intro h
  have bad := h.2
  unfold ConstsEvaluate at bad
  rw [he] at bad
  cases bad

/-- A demand-driven declaration evaluator that skips intermediate declarations whose identity is
not referenced by any later declaration in the list. -/
def skipUnusedEvalConsts (U : ConstTypes) (fuel : Nat) :
    List (ConstDecl U) → AList ConstId CVal → Except ConstEvalRefusal (AList ConstId CVal)
  | [], env => .ok env
  | [d], env => match d.init.eval env fuel with
    | .value v => .ok ((d.id, v) :: env)
    | .failure f => .error (.operation d.id f)
    | .outOfFuel => .error (.outOfFuel d.id)
    | .invalid e => .error (.invalid d.id e)
  | d :: ds, env =>
    if ds.any (fun d' => d'.init.refs.contains d.id) then
      match d.init.eval env fuel with
      | .value v => skipUnusedEvalConsts U fuel ds ((d.id, v) :: env)
      | .failure f => .error (.operation d.id f)
      | .outOfFuel => .error (.outOfFuel d.id)
      | .invalid e => .error (.invalid d.id e)
    else
      skipUnusedEvalConsts U fuel ds env

/-- A demand-driven admission predicate that ignores evaluation failures in unreferenced
intermediate declarations. -/
def skipUnusedConstsAdmitted (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) : Prop :=
  ConstsTyped U ds ∧ (skipUnusedEvalConsts U fuel ds []).isOk = true

/-- Failure-swallowing expression evaluator. -/
def swallowFailure (env : AList ConstId CVal) (fuel : Nat) (e : CExpr) : CEvalResult :=
  match e.eval env fuel with
  | .failure _ => .value (.int .i8 0)
  | r => r

/-- Fabricated-failure expression evaluator. -/
def fabricateFailure (_env : AList ConstId CVal) (_fuel : Nat) (e : CExpr) : CEvalResult :=
  .failure ⟨.plain .div, e, .arithmetic .divisionByZero⟩

/-- A failing closed division and its primitive diagnostic. -/
def zeroDivision : CExpr := .plain .div (.int .i8 1) (.int .i8 0)

/-- Instance (law `ConcreteSpec.Core.eval_agrees`, case `division_failure`). -/
example : ConstEval [] 3 zeroDivision (zeroDivision.eval [] 3) ∧
    ∀ r, ConstEval [] 3 zeroDivision r → zeroDivision.eval [] 3 = r :=
  eval_agrees [] 3 zeroDivision

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `division_failure`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.swallowFailure`). -/
example : ¬ (ConstEval [] 3 zeroDivision (swallowFailure [] 3 zeroDivision) ∧
    ∀ r, ConstEval [] 3 zeroDivision r → swallowFailure [] 3 zeroDivision = r) := by
  intro h
  have hderiv : ConstEval [] 3 zeroDivision
      (.failure ⟨.plain .div, zeroDivision, .arithmetic .divisionByZero⟩) :=
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl
  have bad := h.2 _ hderiv
  revert bad
  decide

/-- Instance (law `ConcreteSpec.Core.eval_agrees`, case `division_success`). -/
example : ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      ((.plain .div (.int .i8 100) (.int .i8 7) : CExpr).eval [] 3) ∧
    ∀ r, ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) r →
      ((.plain .div (.int .i8 100) (.int .i8 7) : CExpr).eval [] 3) = r :=
  eval_agrees [] 3 (.plain .div (.int .i8 100) (.int .i8 7))

/-- Planted (law `ConcreteSpec.Core.eval_agrees`, case `division_success`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.fabricateFailure`). -/
example : ¬ (ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      (fabricateFailure [] 3 (.plain .div (.int .i8 100) (.int .i8 7))) ∧
    ∀ r, ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) r →
      fabricateFailure [] 3 (.plain .div (.int .i8 100) (.int .i8 7)) = r) := by
  intro h
  have hderiv : ConstEval [] 3 (.plain .div (.int .i8 100) (.int .i8 7))
      (.value (.int .i8 14)) :=
    .plain_ok (.int_ok (by decide)) (.int_ok (by decide)) rfl
  have bad := h.2 _ hderiv
  revert bad
  decide

/-- A preceding successful declaration. -/
def failurePrefix : List (ConstDecl ConstDemo.U) :=
  [⟨ConstDemo.cid "P", .c .unit, .unit⟩]
/-- The failed constant is unused by both neighbours. -/
def unusedFailureDecl : ConstDecl ConstDemo.U :=
  ⟨ConstDemo.cid "unused", .c (.int .i8), zeroDivision⟩
/-- Located primitive failure for the unused declaration. -/
def zeroFailure : ConstFailure :=
  ⟨.plain .div, zeroDivision, .arithmetic .divisionByZero⟩

/-- Instance (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_after_prefix`). -/
example : evalConsts ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) [] =
      .error (.operation unusedFailureDecl.id zeroFailure) ∧
    ¬ ConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) :=
  semantic_failure_refuses ConstDemo.U 3 failurePrefix divisionDecls unusedFailureDecl
    [(ConstDemo.cid "P", .unit)] zeroFailure
    (.value _ [] [] .unit _ (.unit 2) (.nil _))
    (.plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl)

/-- Planted (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_after_prefix`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.skipUnusedEvalConsts`). -/
example : ConstsRun ConstDemo.U 3 failurePrefix [] (.ok [(ConstDemo.cid "P", .unit)]) ∧
    ConstEval [(ConstDemo.cid "P", .unit)] 3 unusedFailureDecl.init (.failure zeroFailure) ∧
    ¬ (skipUnusedEvalConsts ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) [] =
      .error (.operation unusedFailureDecl.id zeroFailure) ∧
      ¬ ConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls)) := by
  refine ⟨.value _ [] [] .unit _ (.unit 2) (.nil _),
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl, ?_⟩
  intro h
  have h1 := h.1
  revert h1
  decide

/-- Planted (law `ConcreteSpec.Core.semantic_failure_refuses`, case `unused_after_prefix`,
subject `ConcreteSpec.Core.ConstsAdmitted`, mutant `ConcreteSpec.Core.skipUnusedConstsAdmitted`). -/
example : ConstsRun ConstDemo.U 3 failurePrefix [] (.ok [(ConstDemo.cid "P", .unit)]) ∧
    ConstEval [(ConstDemo.cid "P", .unit)] 3 unusedFailureDecl.init (.failure zeroFailure) ∧
    ¬ (evalConsts ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls) [] =
      .error (.operation unusedFailureDecl.id zeroFailure) ∧
      ¬ skipUnusedConstsAdmitted ConstDemo.U 3 (failurePrefix ++ unusedFailureDecl :: divisionDecls)) := by
  refine ⟨.value _ [] [] .unit _ (.unit 2) (.nil _),
    .plain_failure (.int_ok (by decide)) (.int_ok (by decide)) rfl, ?_⟩
  intro h
  exact h.2 ⟨by decide, by decide⟩

/-! ### Cycle reflection and mutual exclusion of cycles and dependency orders -/

/-- Helper (computation). Boolean dependency testing agrees with source membership.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem dependsOn_reference_inv (U : ConstTypes) (ds : List (ConstDecl U)) (a b : ConstId) :
    dependsOn U ds a b = true ↔ SourceReference U ds a b := by
  simp [dependsOn, SourceReference, List.any_eq_true, Bool.and_eq_true]

/-- Representation lemma. Diagnostic chain checking agrees with independent source-reference
membership, including every edge and the final declared identity. (approved: APPR-C2-agy-5372ad2a0) -/
theorem cycle_chain_real (U : ConstTypes) (ds : List (ConstDecl U)) :
    ∀ chain, refChain U ds chain = true ↔ ActualRefChain U ds chain
  | [] => by simp [refChain, ActualRefChain]
  | [c] => by simp [refChain, ActualRefChain, List.mem_map]
  | a :: b :: rest => by
    simp only [refChain, ActualRefChain, Bool.and_eq_true,
      dependsOn_reference_inv, cycle_chain_real U ds (b :: rest)]

/-- Position of `c` in `ids`. -/
private def idPos : List ConstId → ConstId → Nat
  | [], _ => 0
  | x :: xs, c => if x = c then 0 else idPos xs c + 1

/-- Helper (computation). Every reference in a list satisfying `ReferencesEarlier` either lies in
earlier or precedes the referencing declaration in ds. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem referencesEarlier_idPos_inv (U : ConstTypes) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId),
      ReferencesEarlier U ds earlier →
      (ds.map ConstDecl.id).Nodup →
      (∀ x ∈ earlier, x ∉ ds.map ConstDecl.id) →
      ∀ d ∈ ds, ∀ c ∈ d.init.refs,
        c ∈ earlier ∨ idPos (ds.map ConstDecl.id) c < idPos (ds.map ConstDecl.id) d.id
  | [], _, _, _, _, _, hd, _, _ => nomatch hd
  | d₀ :: ds, earlier, ⟨hd₀, hds⟩, hnodup, hdisj, d, hd, c, hc => by
    simp only [List.map_cons, List.nodup_cons] at hnodup
    obtain ⟨hd₀_not_mem, hnodup_ds⟩ := hnodup
    cases hd with
    | head =>
      exact Or.inl (hd₀ c hc)
    | tail _ hd_mem =>
      have hd_id_mem : d.id ∈ ds.map ConstDecl.id := List.mem_map_of_mem hd_mem
      have hd₀_ne_d : d₀.id ≠ d.id := fun heq => hd₀_not_mem (heq ▸ hd_id_mem)
      have hdisj' : ∀ x ∈ d₀.id :: earlier, x ∉ ds.map ConstDecl.id := by
        intro x hx hmem
        cases List.mem_cons.mp hx with
        | inl h => exact hd₀_not_mem (h ▸ hmem)
        | inr h => exact hdisj x h (List.mem_cons_of_mem _ hmem)
      rcases referencesEarlier_idPos_inv U ds (d₀.id :: earlier) hds hnodup_ds hdisj' d hd_mem c hc with
        hc_earlier | hc_lt
      · cases List.mem_cons.mp hc_earlier with
        | inl hcd₀ =>
          right
          subst c
          simp [idPos, hd₀_ne_d]
        | inr hc_e =>
          exact Or.inl hc_e
      · right
        by_cases hcd₀ : d₀.id = c
        · subst c
          simp [idPos, hd₀_ne_d]
        · simp only [List.map_cons, idPos, hcd₀, hd₀_ne_d, ↓reduceIte]
          omega

/-- Helper (computation). Under a `DependencyOrder`, every `SourceReference` strictly decreases
position in order. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem sourceReference_idPos_lt (U : ConstTypes) (source order : List (ConstDecl U))
    (hord : DependencyOrder U source order) {a b : ConstId}
    (href : SourceReference U source a b) :
    idPos (order.map ConstDecl.id) b < idPos (order.map ConstDecl.id) a := by
  obtain ⟨hperm, hnodup_src, hearlier⟩ := hord
  have hnodup_ord : (order.map ConstDecl.id).Nodup :=
    (hperm.map ConstDecl.id).nodup_iff.mpr hnodup_src
  obtain ⟨d, hd_src, rfl, hb⟩ := href
  have hd_ord : d ∈ order := hperm.mem_iff.mpr hd_src
  rcases referencesEarlier_idPos_inv U order [] hearlier hnodup_ord (by intro x hx; nomatch hx)
      d hd_ord b hb with hfalse | hlt
  · nomatch hfalse
  · exact hlt

/-- Helper (computation). Any `ActualRefChain` from a to b strictly decreases position in
order. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem actualRefChain_idPos_lt (U : ConstTypes) (source order : List (ConstDecl U))
    (hord : DependencyOrder U source order) :
    ∀ (rest : List ConstId) (a b : ConstId),
      ActualRefChain U source (a :: rest ++ [b]) →
      idPos (order.map ConstDecl.id) b < idPos (order.map ConstDecl.id) a
  | [], _, _, ⟨hab, _⟩ => sourceReference_idPos_lt U source order hord hab
  | mid :: rest, _, b, ⟨ham, hrest⟩ =>
    Nat.lt_trans (actualRefChain_idPos_lt U source order hord rest mid b hrest)
      (sourceReference_idPos_lt U source order hord ham)

/-- Law (subject `ConcreteSpec.Core.DependencyOrder`). A real reference cycle in source precludes
every `DependencyOrder` of source: the ordered and cycle constructors of `ElaboratedConsts` are
mutually exclusive. -/
theorem cycle_excludes_dependencyOrder (U : ConstTypes) (source : List (ConstDecl U))
    (chain : List ConstId) (order : List (ConstDecl U))
    (hcycle : CycleChain U source chain) :
    ¬ DependencyOrder U source order := by
  intro hord
  obtain ⟨c, rest, rfl, href⟩ := hcycle
  have hactual : ActualRefChain U source (c :: rest ++ [c]) :=
    (cycle_chain_real U source (c :: rest ++ [c])).1 href
  exact Nat.lt_irrefl _ (actualRefChain_idPos_lt U source order hord rest c c hactual)

/-- Helper (computation). A `DependencyOrder` witnesses `Acyclic`. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem acyclic_of_dependencyOrder (U : ConstTypes) (source order : List (ConstDecl U))
    (hord : DependencyOrder U source order) : Acyclic U source :=
  fun chain hcycle => cycle_excludes_dependencyOrder U source chain order hcycle hord

/-- A broken ordering relation that checks permutation and duplicate-free names without checking
that references appear earlier. -/
def unsortedPermOrder (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order.Perm source ∧ (source.map ConstDecl.id).Nodup

/-- Instance (law `ConcreteSpec.Core.cycle_excludes_dependencyOrder`, case `two_constants`). -/
example : ¬ DependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] :=
  cycle_excludes_dependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "A"]
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
    ⟨ConstDemo.cid "A", [ConstDemo.cid "B"], rfl, by decide⟩

/-- Planted (law `ConcreteSpec.Core.cycle_excludes_dependencyOrder`, case `two_constants`,
subject `ConcreteSpec.Core.DependencyOrder`, mutant `ConcreteSpec.Core.unsortedPermOrder`). -/
example : CycleChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
      [ConstDemo.cid "A", ConstDemo.cid "B", ConstDemo.cid "A"] ∧
    ¬ (¬ unsortedPermOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]) := by
  refine ⟨⟨ConstDemo.cid "A", [ConstDemo.cid "B"], rfl, by decide⟩, ?_⟩
  intro hnot
  exact hnot ⟨List.Perm.refl _, by decide⟩

/-! ### Constructive completeness of dependency sorting (`DependencySortingComplete`) -/

/-- Helper (inversion). `readyDecl` reflects membership of every reference in earlier.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem readyDecl_iff (U : ConstTypes) (earlier : List ConstId) (d : ConstDecl U) :
    readyDecl U earlier d = true ↔ ∀ c ∈ d.init.refs, c ∈ earlier := by
  simp [readyDecl, List.all_eq_true]

/-- Helper (inversion). When `takeReady` succeeds, the picked declaration is ready and d :: rest
is a permutation of remaining. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem takeReady_some_inv (U : ConstTypes) (earlier : List ConstId) :
    ∀ (remaining : List (ConstDecl U)) (d : ConstDecl U) (rest : List (ConstDecl U)),
      takeReady U earlier remaining = some (d, rest) →
      readyDecl U earlier d = true ∧ (d :: rest).Perm remaining
  | [], _, _, h => nomatch h
  | d₀ :: ds, d, rest, h => by
    simp only [takeReady] at h
    split at h
    · rename_i hready
      cases h
      exact ⟨hready, List.Perm.refl _⟩
    · simp only [Option.map_eq_some_iff] at h
      obtain ⟨⟨picked, tail⟩, htake, heq⟩ := h
      cases heq
      obtain ⟨hready, hperm⟩ := takeReady_some_inv U earlier ds picked tail htake
      exact ⟨hready, (List.Perm.swap d₀ picked tail).trans (hperm.cons d₀)⟩

/-- Helper (inversion). When `takeReady` returns `none`, no declaration in remaining is ready.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem takeReady_none_inv (U : ConstTypes) (earlier : List ConstId) :
    ∀ (remaining : List (ConstDecl U)),
      takeReady U earlier remaining = none →
      ∀ d ∈ remaining, readyDecl U earlier d = false
  | [], _, _, hd => nomatch hd
  | d₀ :: ds, h, d, hd => by
    simp only [takeReady] at h
    split at h
    · cases h
    · rename_i hnot
      simp only [Option.map_eq_none_iff] at h
      cases hd with
      | head => exact Bool.eq_false_iff.mpr hnot
      | tail _ hm => exact takeReady_none_inv U earlier ds h d hm

/-- Helper (computation). Extending a `refChain` by one edge at the right end.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem refChain_snoc (U : ConstTypes) (source : List (ConstDecl U)) :
    ∀ (xs : List ConstId) (a b : ConstId),
      refChain U source (xs ++ [a]) = true →
      dependsOn U source a b = true →
      b ∈ source.map ConstDecl.id →
      refChain U source (xs ++ [a, b]) = true
  | [], a, b, _, hab, hb => by
    simp [refChain, hab, hb]
  | [x], a, b, hxa, hab, hb => by
    simp only [List.singleton_append, refChain, Bool.and_eq_true] at hxa ⊢
    exact ⟨hxa.1, hab, by simp [hb]⟩
  | x :: y :: rest, a, b, h, hab, hb => by
    simp only [List.cons_append, refChain, Bool.and_eq_true] at h ⊢
    exact ⟨h.1, refChain_snoc U source (y :: rest) a b h.2 hab hb⟩

/-- Helper (computation). Any non-empty suffix of a valid `refChain` is a valid `refChain`.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem refChain_suffix (U : ConstTypes) (source : List (ConstDecl U)) :
    ∀ (pre : List ConstId) (c : ConstId) (suf : List ConstId),
      refChain U source (pre ++ c :: suf) = true →
      refChain U source (c :: suf) = true
  | [], _, _, h => h
  | [x], c, suf, h => by
    simp only [List.singleton_append, refChain, Bool.and_eq_true] at h
    exact h.2
  | x :: y :: rest, c, suf, h => by
    simp only [List.cons_append, refChain, Bool.and_eq_true] at h
    exact refChain_suffix U source (y :: rest) c suf h.2

/-- Helper (inversion). Splitting a list at a member element. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem mem_split_at {α : Type} :
    ∀ (xs : List α) (v : α), v ∈ xs → ∃ pre suf, xs = pre ++ v :: suf
  | [], _, h => nomatch h
  | x :: xs, v, h => by
    cases List.mem_cons.mp h with
    | inl heq => subst heq; exact ⟨[], xs, rfl⟩
    | inr hmem =>
      obtain ⟨pre, suf, rfl⟩ := mem_split_at xs v hmem
      exact ⟨x :: pre, suf, rfl⟩

/-- Helper (arithmetic). Filtering out a member strictly decreases list length.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem length_filter_ne_lt (v : ConstId) :
    ∀ (xs : List ConstId), v ∈ xs → (xs.filter (· != v)).length < xs.length
  | [], h => nomatch h
  | x :: xs, h => by
    have hlen : (xs.filter (· != v)).length ≤ xs.length := List.length_filter_le _ _
    cases List.mem_cons.mp h with
    | inl heq =>
      subst heq
      simp only [List.filter_cons, bne_self_eq_false, Bool.false_eq_true, ↓reduceIte,
        List.length_cons]
      omega
    | inr hmem =>
      have ih := length_filter_ne_lt v xs hmem
      simp only [List.filter_cons]
      split <;> simp only [List.length_cons] <;> omega

/-- Helper (computation). A walk in a finite vertex subset where every vertex has an outgoing edge
eventually closes a `CycleChain`. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem walk_finds_cycle (U : ConstTypes) (source : List (ConstDecl U))
    (V : List ConstId)
    (hV_src : ∀ u ∈ V, u ∈ source.map ConstDecl.id)
    (hstep : ∀ u ∈ V, ∃ v ∈ V, dependsOn U source u v = true) :
    ∀ (n : Nat) (unseen path : List ConstId) (last : ConstId),
      unseen.length ≤ n →
      last ∈ V →
      refChain U source (path ++ [last]) = true →
      (∀ x ∈ V, x ∈ path ++ [last] ∨ x ∈ unseen) →
      ∃ chain, CycleChain U source chain
  | n, unseen, path, last, hlen, hlast, hchain, hcov => by
    obtain ⟨v, hv_V, hdep⟩ := hstep last hlast
    have hv_src : v ∈ source.map ConstDecl.id := hV_src v hv_V
    have hchain' : refChain U source ((path ++ [last]) ++ [v]) = true := by
      simpa [List.append_assoc] using refChain_snoc U source path last v hchain hdep hv_src
    by_cases hv_path : v ∈ path ++ [last]
    · obtain ⟨pre, suf, hsplit⟩ := mem_split_at (path ++ [last]) v hv_path
      have hchain_split : refChain U source (pre ++ (v :: suf ++ [v])) = true := by
        simpa [hsplit, List.append_assoc] using hchain'
      have hcycle_ref : refChain U source (v :: suf ++ [v]) = true :=
        refChain_suffix U source pre v (suf ++ [v]) hchain_split
      exact ⟨v :: suf ++ [v], v, suf, rfl, hcycle_ref⟩
    · rcases hcov v hv_V with hv_in | hv_unseen
      · exact (hv_path hv_in).elim
      · cases n with
        | zero =>
          have hlt := length_filter_ne_lt v unseen hv_unseen
          omega
        | succ m =>
          have hlt : (unseen.filter (· != v)).length < unseen.length :=
            length_filter_ne_lt v unseen hv_unseen
          apply walk_finds_cycle U source V hV_src hstep m (unseen.filter (· != v))
            (path ++ [last]) v (by omega) hv_V hchain'
          intro x hx
          rcases hcov x hx with hx_path | hx_unseen
          · exact Or.inl (List.mem_append_left [v] hx_path)
          · by_cases hxv : x = v
            · subst hxv
              exact Or.inl (List.mem_append_right (path ++ [last]) (List.Mem.head _))
            · right
              exact List.mem_filter.mpr ⟨hx_unseen, bne_iff_ne.mpr hxv⟩

/-- Helper (inversion). A finite list that does not lie entirely in earlier has a member outside
earlier. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem exists_not_mem_of_not_all (earlier : List ConstId) :
    ∀ (xs : List ConstId), ¬ (∀ c ∈ xs, c ∈ earlier) → ∃ c ∈ xs, c ∉ earlier
  | [], h => (h (fun _ hc => nomatch hc)).elim
  | r :: rs, h => by
    by_cases hr : r ∈ earlier
    · have hnot_rs : ¬ (∀ c ∈ rs, c ∈ earlier) := by
        intro hrs
        apply h
        intro c hc
        cases List.mem_cons.mp hc with
        | inl heq => exact heq ▸ hr
        | inr hmem => exact hrs c hmem
      obtain ⟨c, hc_rs, hc_ne⟩ := exists_not_mem_of_not_all earlier rs hnot_rs
      exact ⟨c, List.Mem.tail _ hc_rs, hc_ne⟩
    · exact ⟨r, List.Mem.head _, hr⟩

/-- Helper (computation). Correctness of `sortConstsAux` on any acyclic suffix with resolved names.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem sortConstsAux_spec (U : ConstTypes) (source : List (ConstDecl U))
    (hacyc : Acyclic U source) :
    ∀ (fuel : Nat) (earlier : List ConstId) (remaining : List (ConstDecl U)),
      remaining.length ≤ fuel →
      (∀ d ∈ remaining, d ∈ source) →
      (∀ d ∈ remaining, ∀ c ∈ d.init.refs, c ∈ earlier ∨ c ∈ remaining.map ConstDecl.id) →
      (sortConstsAux U fuel earlier remaining).Perm remaining ∧
        ReferencesEarlier U (sortConstsAux U fuel earlier remaining) earlier
  | 0, earlier, remaining, hlen, _, _ => by
    cases remaining with
    | nil => exact ⟨List.Perm.refl [], trivial⟩
    | cons d ds => simp at hlen
  | fuel + 1, earlier, remaining, hlen, hsub, hrefs => by
    simp only [sortConstsAux]
    cases htake : takeReady U earlier remaining with
    | some pair =>
      rcases pair with ⟨d, rest⟩
      obtain ⟨hready, hperm⟩ := takeReady_some_inv U earlier remaining d rest htake
      have hlen_rest : rest.length ≤ fuel := by
        have := hperm.length_eq
        simp only [List.length_cons] at this
        omega
      have hsub_rest : ∀ d' ∈ rest, d' ∈ source :=
        fun d' hd' => hsub d' (hperm.subset (List.Mem.tail _ hd'))
      have hrefs_rest : ∀ d' ∈ rest, ∀ c ∈ d'.init.refs,
          c ∈ d.id :: earlier ∨ c ∈ rest.map ConstDecl.id := by
        intro d' hd' c hc
        rcases hrefs d' (hperm.subset (List.Mem.tail _ hd')) c hc with hc_e | hc_rem
        · exact Or.inl (List.Mem.tail _ hc_e)
        · have hc_d_rest : c ∈ (d :: rest).map ConstDecl.id :=
            (hperm.map ConstDecl.id).mem_iff.mpr hc_rem
          cases List.mem_cons.mp hc_d_rest with
          | inl hcd => exact Or.inl (hcd ▸ List.Mem.head _)
          | inr hcrest => exact Or.inr hcrest
      obtain ⟨hperm_rec, hearlier_rec⟩ :=
        sortConstsAux_spec U source hacyc fuel (d.id :: earlier) rest hlen_rest hsub_rest hrefs_rest
      exact ⟨(hperm_rec.cons d).trans hperm, ⟨(readyDecl_iff U earlier d).1 hready, hearlier_rec⟩⟩
    | none =>
      cases remaining with
      | nil => exact ⟨List.Perm.refl [], trivial⟩
      | cons d₀ ds =>
        have hnone := takeReady_none_inv U earlier (d₀ :: ds) htake
        have hstep : ∀ u ∈ (d₀ :: ds).map ConstDecl.id,
            ∃ v ∈ (d₀ :: ds).map ConstDecl.id, dependsOn U source u v = true := by
          intro u hu
          obtain ⟨d, hd_mem, rfl⟩ := List.mem_map.mp hu
          have hnot_ready : ¬ (∀ c ∈ d.init.refs, c ∈ earlier) := by
            intro hall
            have := (readyDecl_iff U earlier d).2 hall
            rw [hnone d hd_mem] at this
            cases this
          obtain ⟨v, hv_refs, hv_not_e⟩ :=
            exists_not_mem_of_not_all earlier d.init.refs hnot_ready
          rcases hrefs d hd_mem v hv_refs with hv_e | hv_rem
          · exact (hv_not_e hv_e).elim
          · refine ⟨v, hv_rem, ?_⟩
            exact (dependsOn_reference_inv U source d.id v).2 ⟨d, hsub d hd_mem, rfl, hv_refs⟩
        have hV_src : ∀ u ∈ (d₀ :: ds).map ConstDecl.id, u ∈ source.map ConstDecl.id := by
          intro u hu
          obtain ⟨d, hd_mem, rfl⟩ := List.mem_map.mp hu
          exact List.mem_map_of_mem (hsub d hd_mem)
        have hd₀_src : d₀.id ∈ source.map ConstDecl.id :=
          List.mem_map_of_mem (hsub d₀ (List.Mem.head _))
        have hchain₀ : refChain U source ([] ++ [d₀.id]) = true := by
          simp [refChain, hd₀_src]
        obtain ⟨chain, hcycle⟩ := walk_finds_cycle U source ((d₀ :: ds).map ConstDecl.id)
          hV_src hstep ((d₀ :: ds).map ConstDecl.id).length ((d₀ :: ds).map ConstDecl.id)
          [] d₀.id (Nat.le_refl _) (List.Mem.head _) hchain₀ (fun x hx => Or.inr hx)
        exact (hacyc chain hcycle).elim

/-- Helper (computation). sortConsts U source produces a valid `DependencyOrder` whenever
source is name-resolved and acyclic. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem sortConsts_dependencyOrder (U : ConstTypes) (source : List (ConstDecl U))
    (hres : SourceNamesResolved U source) (hacyc : Acyclic U source) :
    DependencyOrder U source (sortConsts U source) := by
  obtain ⟨hnodup, hrefs⟩ := hres
  obtain ⟨hperm, hearlier⟩ := sortConstsAux_spec U source hacyc source.length [] source
    (Nat.le_refl _) (fun _ h => h) (fun d hd c hc => Or.inr (hrefs d hd c hc))
  exact ⟨hperm, hnodup, hearlier⟩

/-- Law (subject `ConcreteSpec.Core.DependencyOrder`). Every name-resolved, acyclic source
declaration list has a `DependencyOrder`, constructed by `sortConsts`. -/
theorem DependencySortingComplete (U : ConstTypes) (source : List (ConstDecl U))
    (hres : SourceNamesResolved U source) (hacyc : Acyclic U source) :
    ∃ order, DependencyOrder U source order :=
  ⟨sortConsts U source, sortConsts_dependencyOrder U source hres hacyc⟩

/-- A broken elaborator order criterion that forbids reordering and requires source declaration
order to already satisfy `ReferencesEarlier`. -/
def sourceOrderOnly (U : ConstTypes) (source order : List (ConstDecl U)) : Prop :=
  order = source ∧ DependencyOrder U source order

/-- Instance (law `ConcreteSpec.Core.DependencySortingComplete`, case `forward_ref`). -/
example : ∃ order, DependencyOrder ConstDemo.U
    [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] order :=
  DependencySortingComplete ConstDemo.U
    [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
    (by decide)
    (acyclic_of_dependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩]
      (by decide))

/-- Planted (law `ConcreteSpec.Core.DependencySortingComplete`, case `forward_ref`,
subject `ConcreteSpec.Core.DependencyOrder`, mutant `ConcreteSpec.Core.sourceOrderOnly`). -/
example : SourceNamesResolved ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] ∧
    Acyclic ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] ∧
    ¬ (∃ order, sourceOrderOnly ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩] order) := by
  refine ⟨by decide,
    acyclic_of_dependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 2⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .ref (ConstDemo.cid "B")⟩]
      (by decide), ?_⟩
  rintro ⟨order, rfl, _, _, hearlier⟩
  simp [ReferencesEarlier, CExpr.refs] at hearlier

/-! ### Order independence (`ConstOrderIndependent`) -/

/-- Helper (congruence). Static expression typing depends only on the bindings of e.refs.
(approved: REV-C2-agy-59d4c501d) -/
private theorem ty?_congr_refs (Γ₁ Γ₂ : AList ConstId CTy) :
    ∀ (e : CExpr), (∀ c ∈ e.refs, AList.lookup Γ₁ c = AList.lookup Γ₂ c) →
      e.ty? Γ₁ = e.ty? Γ₂ := by
  intro e
  induction e with
  | int _ _ | bool _ | unit => intro _; rfl
  | ref c => intro h; exact h c (List.Mem.head _)
  | wrapping _ a b iha ihb | saturating _ a b iha ihb | plain _ a b iha ihb
  | bit _ a b iha ihb | cmp _ a b iha ihb | and a b iha ihb | or a b iha ihb =>
    intro h
    have ha := iha (fun c hc => h c (List.mem_append_left _ hc))
    have hb := ihb (fun c hc => h c (List.mem_append_right _ hc))
    simp [CExpr.ty?, ha, hb]
  | plainNeg a iha | bitNot a iha | not a iha | convert _ a iha =>
    intro h
    have ha := iha h
    simp [CExpr.ty?, ha]

/-- Helper (congruence). Initializer evaluation depends only on the bindings of e.refs.
(approved: REV-C2-agy-59d4c501d) -/
private theorem eval_congr_refs (env₁ env₂ : AList ConstId CVal) :
    ∀ (e : CExpr) (fuel : Nat), (∀ c ∈ e.refs, AList.lookup env₁ c = AList.lookup env₂ c) →
      e.eval env₁ fuel = e.eval env₂ fuel := by
  intro e
  induction e with
  | int _ _ | bool _ | unit =>
    intro fuel _
    cases fuel <;> rfl
  | ref c =>
    intro fuel h
    cases fuel with
    | zero => rfl
    | succ fuel => simp [CExpr.eval, h c (List.Mem.head _)]
  | wrapping _ a b iha ihb | saturating _ a b iha ihb | plain _ a b iha ihb
  | bit _ a b iha ihb | cmp _ a b iha ihb | and a b iha ihb | or a b iha ihb =>
    intro fuel h
    cases fuel with
    | zero => rfl
    | succ fuel =>
      have ha := iha fuel (fun c hc => h c (List.mem_append_left _ hc))
      have hb := ihb fuel (fun c hc => h c (List.mem_append_right _ hc))
      simp [CExpr.eval, ha, hb]
  | plainNeg a iha | bitNot a iha | not a iha | convert _ a iha =>
    intro fuel h
    cases fuel with
    | zero => rfl
    | succ fuel =>
      have ha := iha fuel h
      simp [CExpr.eval, ha]

/-- Helper (computation). `typeConsts` preserves bindings of identities absent from ds.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_lookup_not_mem (U : ConstTypes) :
    ∀ (ds : List (ConstDecl U)) (Γ Γ' : AList ConstId CTy) (c : ConstId),
      typeConsts U ds Γ = some Γ' →
      c ∉ ds.map ConstDecl.id →
      AList.lookup Γ' c = AList.lookup Γ c
  | [], _, _, _, h, _ => by cases h; rfl
  | d :: ds, Γ, Γ', c, h, hnot => by
    simp only [List.map_cons, List.mem_cons, not_or] at hnot
    simp only [typeConsts] at h
    split at h
    · split at h
      · rw [typeConsts_lookup_not_mem U ds _ Γ' c h hnot.2]
        have hne : d.id ≠ c := Ne.symm hnot.1
        simp [AList.lookup, hne]
      · cases h
    · cases h

/-- Helper (computation). `evalConsts` preserves bindings of identities absent from ds.
(approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_lookup_not_mem (U : ConstTypes) (fuel : Nat) :
    ∀ (ds : List (ConstDecl U)) (env env' : AList ConstId CVal) (c : ConstId),
      evalConsts U fuel ds env = .ok env' →
      c ∉ ds.map ConstDecl.id →
      AList.lookup env' c = AList.lookup env c
  | [], _, _, _, h, _ => by cases h; rfl
  | d :: ds, env, env', c, h, hnot => by
    simp only [List.map_cons, List.mem_cons, not_or] at hnot
    simp only [evalConsts] at h
    split at h
    · rw [evalConsts_lookup_not_mem U fuel ds _ env' c h hnot.2]
      have hne : d.id ≠ c := Ne.symm hnot.1
      simp [AList.lookup, hne]
    · cases h
    · cases h
    · cases h

/-- Helper (computation). Along a `ReferencesEarlier` suffix with duplicate-free names disjoint
from earlier, a successful `typeConsts` produces a global fixed point for all declarations in
ds. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_fixedPoint_inv (U : ConstTypes) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId) (Γ Γ' : AList ConstId CTy),
      ReferencesEarlier U ds earlier →
      (ds.map ConstDecl.id).Nodup →
      (∀ x ∈ earlier, x ∉ ds.map ConstDecl.id) →
      typeConsts U ds Γ = some Γ' →
      ∀ d ∈ ds, ∃ T, d.init.ty? Γ' = some T ∧ U.ofC T = d.ty ∧ AList.lookup Γ' d.id = some T
  | [], _, _, _, _, _, _, _, _, hd => nomatch hd
  | d₀ :: ds, earlier, Γ, Γ', ⟨hd₀_refs, hds_earlier⟩, hnodup, hdisj, ht, d, hd => by
    simp only [List.map_cons, List.nodup_cons] at hnodup
    obtain ⟨hd₀_not_ds, hnodup_ds⟩ := hnodup
    simp only [typeConsts] at ht
    split at ht
    · rename_i T₀ hT₀
      split at ht
      · rename_i hcond
        have hdisj' : ∀ x ∈ d₀.id :: earlier, x ∉ ds.map ConstDecl.id := by
          intro x hx hmem
          cases List.mem_cons.mp hx with
          | inl h => exact hd₀_not_ds (h ▸ hmem)
          | inr h => exact hdisj x h (List.mem_cons_of_mem _ hmem)
        cases hd with
        | head =>
          have hlookup_d₀ : AList.lookup Γ' d₀.id = some T₀ := by
            rw [typeConsts_lookup_not_mem U ds _ Γ' d₀.id ht hd₀_not_ds]
            simp [AList.lookup]
          have hcongr : d₀.init.ty? Γ' = d₀.init.ty? Γ := by
            apply ty?_congr_refs Γ' Γ d₀.init
            intro c hc
            have hc_e : c ∈ earlier := hd₀_refs c hc
            have hc_not_d₀ds : c ∉ (d₀ :: ds).map ConstDecl.id := hdisj c hc_e
            simp only [List.map_cons, List.mem_cons, not_or] at hc_not_d₀ds
            rw [typeConsts_lookup_not_mem U ds _ Γ' c ht hc_not_d₀ds.2]
            have hne : d₀.id ≠ c := Ne.symm hc_not_d₀ds.1
            simp [AList.lookup, hne]
          exact ⟨T₀, hcongr.trans hT₀, hcond.1, hlookup_d₀⟩
        | tail _ hd_mem =>
          exact typeConsts_fixedPoint_inv U ds (d₀.id :: earlier) ((d₀.id, T₀) :: Γ) Γ'
            hds_earlier hnodup_ds hdisj' ht d hd_mem
      · cases ht
    · cases ht

/-- Helper (computation). Conversely, any global typing fixed point Γ₁ drives `typeConsts` to
succeed along any `ReferencesEarlier` order ds with identical bindings. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_from_fixedPoint_inv (U : ConstTypes) (Γ₁ : AList ConstId CTy) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId) (Γ : AList ConstId CTy),
      ReferencesEarlier U ds earlier →
      (ds.map ConstDecl.id).Nodup →
      (∀ x ∈ earlier, x ∉ ds.map ConstDecl.id) →
      (∀ c, c ∉ earlier → AList.lookup Γ c = none) →
      (∀ c ∈ earlier, AList.lookup Γ c = AList.lookup Γ₁ c) →
      (∀ d ∈ ds, ∃ T, d.init.ty? Γ₁ = some T ∧ U.ofC T = d.ty ∧ AList.lookup Γ₁ d.id = some T) →
      ∃ Γ₂, typeConsts U ds Γ = some Γ₂ ∧
        (∀ c, c ∉ earlier → c ∉ ds.map ConstDecl.id → AList.lookup Γ₂ c = none) ∧
        (∀ c, c ∈ earlier ∨ c ∈ ds.map ConstDecl.id → AList.lookup Γ₂ c = AList.lookup Γ₁ c)
  | [], earlier, Γ, _, _, _, hnone, heq, _ =>
    ⟨Γ, rfl, fun c hce _ => hnone c hce, fun c h => by
      cases h with
      | inl hce => exact heq c hce
      | inr hds => nomatch hds⟩
  | d₀ :: ds, earlier, Γ, ⟨hd₀_refs, hds_earlier⟩, hnodup, hdisj, hnone, heq, hfp => by
    simp only [List.map_cons, List.nodup_cons] at hnodup
    obtain ⟨hd₀_not_ds, hnodup_ds⟩ := hnodup
    obtain ⟨T₀, hT₀_Γ₁, hofC, hlookup_Γ₁⟩ := hfp d₀ (List.Mem.head _)
    have hT₀_Γ : d₀.init.ty? Γ = some T₀ := by
      rw [ty?_congr_refs Γ Γ₁ d₀.init (fun c hc => heq c (hd₀_refs c hc))]
      exact hT₀_Γ₁
    have hd₀_not_e : d₀.id ∉ earlier := fun he => hdisj d₀.id he (List.Mem.head _)
    have hfresh : AList.lookup Γ d₀.id = none := hnone d₀.id hd₀_not_e
    have hdisj' : ∀ x ∈ d₀.id :: earlier, x ∉ ds.map ConstDecl.id := by
      intro x hx hmem
      cases List.mem_cons.mp hx with
      | inl h => exact hd₀_not_ds (h ▸ hmem)
      | inr h => exact hdisj x h (List.mem_cons_of_mem _ hmem)
    have hnone' : ∀ c, c ∉ d₀.id :: earlier → AList.lookup ((d₀.id, T₀) :: Γ) c = none := by
      intro c hc
      simp only [List.mem_cons, not_or] at hc
      have hne : d₀.id ≠ c := Ne.symm hc.1
      simp [AList.lookup, hne, hnone c hc.2]
    have heq' : ∀ c ∈ d₀.id :: earlier,
        AList.lookup ((d₀.id, T₀) :: Γ) c = AList.lookup Γ₁ c := by
      intro c hc
      by_cases hcd₀ : d₀.id = c
      · subst c; simp [AList.lookup, hlookup_Γ₁]
      · cases List.mem_cons.mp hc with
        | inl h => exact (hcd₀ h.symm).elim
        | inr h => simp [AList.lookup, hcd₀, heq c h]
    obtain ⟨Γ₂, ht₂, hnone₂, heq₂⟩ := typeConsts_from_fixedPoint_inv U Γ₁ ds (d₀.id :: earlier)
      ((d₀.id, T₀) :: Γ) hds_earlier hnodup_ds hdisj' hnone' heq'
      (fun d hd => hfp d (List.Mem.tail _ hd))
    refine ⟨Γ₂, by simp [typeConsts, hT₀_Γ, hofC, hfresh, ht₂], ?_, ?_⟩
    · intro c hce hcd₀ds
      simp only [List.map_cons, List.mem_cons, not_or] at hcd₀ds
      exact hnone₂ c (by simp [List.mem_cons, hcd₀ds.1, hce]) hcd₀ds.2
    · intro c hc
      apply heq₂ c
      rcases hc with hce | hcd₀ds
      · exact Or.inl (List.Mem.tail _ hce)
      · cases List.mem_cons.mp hcd₀ds with
        | inl h => exact Or.inl (h ▸ List.Mem.head _)
        | inr h => exact Or.inr h

/-- Helper (computation). Along a `ReferencesEarlier` suffix with duplicate-free names disjoint
from earlier, a successful `evalConsts` produces a global fixed point for all declarations in
ds. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_fixedPoint_inv (U : ConstTypes) (fuel : Nat) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId) (env env' : AList ConstId CVal),
      ReferencesEarlier U ds earlier →
      (ds.map ConstDecl.id).Nodup →
      (∀ x ∈ earlier, x ∉ ds.map ConstDecl.id) →
      evalConsts U fuel ds env = .ok env' →
      ∀ d ∈ ds, ∃ v, d.init.eval env' fuel = .value v ∧ AList.lookup env' d.id = some v
  | [], _, _, _, _, _, _, _, _, hd => nomatch hd
  | d₀ :: ds, earlier, env, env', ⟨hd₀_refs, hds_earlier⟩, hnodup, hdisj, he, d, hd => by
    simp only [List.map_cons, List.nodup_cons] at hnodup
    obtain ⟨hd₀_not_ds, hnodup_ds⟩ := hnodup
    simp only [evalConsts] at he
    split at he
    · rename_i v₀ hv₀
      have hdisj' : ∀ x ∈ d₀.id :: earlier, x ∉ ds.map ConstDecl.id := by
        intro x hx hmem
        cases List.mem_cons.mp hx with
        | inl h => exact hd₀_not_ds (h ▸ hmem)
        | inr h => exact hdisj x h (List.mem_cons_of_mem _ hmem)
      cases hd with
      | head =>
        have hlookup_d₀ : AList.lookup env' d₀.id = some v₀ := by
          rw [evalConsts_lookup_not_mem U fuel ds _ env' d₀.id he hd₀_not_ds]
          simp [AList.lookup]
        have hcongr : d₀.init.eval env' fuel = d₀.init.eval env fuel := by
          apply eval_congr_refs env' env d₀.init fuel
          intro c hc
          have hc_e : c ∈ earlier := hd₀_refs c hc
          have hc_not_d₀ds : c ∉ (d₀ :: ds).map ConstDecl.id := hdisj c hc_e
          simp only [List.map_cons, List.mem_cons, not_or] at hc_not_d₀ds
          rw [evalConsts_lookup_not_mem U fuel ds _ env' c he hc_not_d₀ds.2]
          have hne : d₀.id ≠ c := Ne.symm hc_not_d₀ds.1
          simp [AList.lookup, hne]
        exact ⟨v₀, hcongr.trans hv₀, hlookup_d₀⟩
      | tail _ hd_mem =>
        exact evalConsts_fixedPoint_inv U fuel ds (d₀.id :: earlier) ((d₀.id, v₀) :: env) env'
          hds_earlier hnodup_ds hdisj' he d hd_mem
    · cases he
    · cases he
    · cases he

/-- Helper (computation). Conversely, any global evaluation fixed point env₁ drives `evalConsts`
to succeed along any `ReferencesEarlier` order ds with identical bindings. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_from_fixedPoint_inv (U : ConstTypes) (fuel : Nat)
    (env₁ : AList ConstId CVal) :
    ∀ (ds : List (ConstDecl U)) (earlier : List ConstId) (env : AList ConstId CVal),
      ReferencesEarlier U ds earlier →
      (ds.map ConstDecl.id).Nodup →
      (∀ x ∈ earlier, x ∉ ds.map ConstDecl.id) →
      (∀ c, c ∉ earlier → AList.lookup env c = none) →
      (∀ c ∈ earlier, AList.lookup env c = AList.lookup env₁ c) →
      (∀ d ∈ ds, ∃ v, d.init.eval env₁ fuel = .value v ∧ AList.lookup env₁ d.id = some v) →
      ∃ env₂, evalConsts U fuel ds env = .ok env₂ ∧
        (∀ c, c ∉ earlier → c ∉ ds.map ConstDecl.id → AList.lookup env₂ c = none) ∧
        (∀ c, c ∈ earlier ∨ c ∈ ds.map ConstDecl.id → AList.lookup env₂ c = AList.lookup env₁ c)
  | [], earlier, env, _, _, _, hnone, heq, _ =>
    ⟨env, rfl, fun c hce _ => hnone c hce, fun c h => by
      cases h with
      | inl hce => exact heq c hce
      | inr hds => nomatch hds⟩
  | d₀ :: ds, earlier, env, ⟨hd₀_refs, hds_earlier⟩, hnodup, hdisj, hnone, heq, hfp => by
    simp only [List.map_cons, List.nodup_cons] at hnodup
    obtain ⟨hd₀_not_ds, hnodup_ds⟩ := hnodup
    obtain ⟨v₀, hv₀_env₁, hlookup_env₁⟩ := hfp d₀ (List.Mem.head _)
    have hv₀_env : d₀.init.eval env fuel = .value v₀ := by
      rw [eval_congr_refs env env₁ d₀.init fuel (fun c hc => heq c (hd₀_refs c hc))]
      exact hv₀_env₁
    have hdisj' : ∀ x ∈ d₀.id :: earlier, x ∉ ds.map ConstDecl.id := by
      intro x hx hmem
      cases List.mem_cons.mp hx with
      | inl h => exact hd₀_not_ds (h ▸ hmem)
      | inr h => exact hdisj x h (List.mem_cons_of_mem _ hmem)
    have hnone' : ∀ c, c ∉ d₀.id :: earlier → AList.lookup ((d₀.id, v₀) :: env) c = none := by
      intro c hc
      simp only [List.mem_cons, not_or] at hc
      have hne : d₀.id ≠ c := Ne.symm hc.1
      simp [AList.lookup, hne, hnone c hc.2]
    have heq' : ∀ c ∈ d₀.id :: earlier,
        AList.lookup ((d₀.id, v₀) :: env) c = AList.lookup env₁ c := by
      intro c hc
      by_cases hcd₀ : d₀.id = c
      · subst c; simp [AList.lookup, hlookup_env₁]
      · cases List.mem_cons.mp hc with
        | inl h => exact (hcd₀ h.symm).elim
        | inr h => simp [AList.lookup, hcd₀, heq c h]
    obtain ⟨env₂, he₂, hnone₂, heq₂⟩ := evalConsts_from_fixedPoint_inv U fuel env₁ ds
      (d₀.id :: earlier) ((d₀.id, v₀) :: env) hds_earlier hnodup_ds hdisj' hnone' heq'
      (fun d hd => hfp d (List.Mem.tail _ hd))
    refine ⟨env₂, by simp [evalConsts, hv₀_env, he₂], ?_, ?_⟩
    · intro c hce hcd₀ds
      simp only [List.map_cons, List.mem_cons, not_or] at hcd₀ds
      exact hnone₂ c (by simp [List.mem_cons, hcd₀ds.1, hce]) hcd₀ds.2
    · intro c hc
      apply heq₂ c
      rcases hc with hce | hcd₀ds
      · exact Or.inl (List.Mem.tail _ hce)
      · cases List.mem_cons.mp hcd₀ds with
        | inl h => exact Or.inl (h ▸ List.Mem.head _)
        | inr h => exact Or.inr h

/-- Helper (computation). One direction of `typeConsts` order independence. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem typeConsts_order_transfer (U : ConstTypes)
    (source first second : List (ConstDecl U))
    (hfirst : DependencyOrder U source first)
    (hsecond : DependencyOrder U source second)
    {Γ₁ : AList ConstId CTy} (ht₁ : typeConsts U first [] = some Γ₁) :
    ∃ Γ₂, typeConsts U second [] = some Γ₂ ∧ ∀ c, AList.lookup Γ₂ c = AList.lookup Γ₁ c := by
  obtain ⟨hperm₁, hnodup_src, hearlier₁⟩ := hfirst
  obtain ⟨hperm₂, _, hearlier₂⟩ := hsecond
  have hnodup₁ : (first.map ConstDecl.id).Nodup :=
    (hperm₁.map ConstDecl.id).nodup_iff.mpr hnodup_src
  have hnodup₂ : (second.map ConstDecl.id).Nodup :=
    (hperm₂.map ConstDecl.id).nodup_iff.mpr hnodup_src
  have hfp₁ := typeConsts_fixedPoint_inv U first [] [] Γ₁ hearlier₁ hnodup₁
    (by intro x hx; nomatch hx) ht₁
  have hfp₂ : ∀ d ∈ second,
      ∃ T, d.init.ty? Γ₁ = some T ∧ U.ofC T = d.ty ∧ AList.lookup Γ₁ d.id = some T :=
    fun d hd => hfp₁ d (hperm₁.mem_iff.mpr (hperm₂.mem_iff.mp hd))
  obtain ⟨Γ₂, ht₂, hnone₂, heq₂⟩ := typeConsts_from_fixedPoint_inv U Γ₁ second [] []
    hearlier₂ hnodup₂ (by intro x hx; nomatch hx) (by intro c _; rfl) (by intro c hc; nomatch hc) hfp₂
  refine ⟨Γ₂, ht₂, ?_⟩
  intro c
  by_cases hc : c ∈ second.map ConstDecl.id
  · exact heq₂ c (Or.inr hc)
  · have hc₁ : c ∉ first.map ConstDecl.id := by
      intro hmem
      exact hc ((hperm₂.map ConstDecl.id).mem_iff.mpr
        ((hperm₁.map ConstDecl.id).mem_iff.mp hmem))
    rw [hnone₂ c (by intro h; nomatch h) hc,
      typeConsts_lookup_not_mem U first [] Γ₁ c ht₁ hc₁]
    rfl

/-- Helper (computation). One direction of `evalConsts` order independence. (approved: APPR-C2-agy-5372ad2a0) -/
private theorem evalConsts_order_transfer (U : ConstTypes) (fuel : Nat)
    (source first second : List (ConstDecl U))
    (hfirst : DependencyOrder U source first)
    (hsecond : DependencyOrder U source second)
    {env₁ : AList ConstId CVal} (he₁ : evalConsts U fuel first [] = .ok env₁) :
    ∃ env₂, evalConsts U fuel second [] = .ok env₂ ∧
      ∀ c, AList.lookup env₂ c = AList.lookup env₁ c := by
  obtain ⟨hperm₁, hnodup_src, hearlier₁⟩ := hfirst
  obtain ⟨hperm₂, _, hearlier₂⟩ := hsecond
  have hnodup₁ : (first.map ConstDecl.id).Nodup :=
    (hperm₁.map ConstDecl.id).nodup_iff.mpr hnodup_src
  have hnodup₂ : (second.map ConstDecl.id).Nodup :=
    (hperm₂.map ConstDecl.id).nodup_iff.mpr hnodup_src
  have hfp₁ := evalConsts_fixedPoint_inv U fuel first [] [] env₁ hearlier₁ hnodup₁
    (by intro x hx; nomatch hx) he₁
  have hfp₂ : ∀ d ∈ second,
      ∃ v, d.init.eval env₁ fuel = .value v ∧ AList.lookup env₁ d.id = some v :=
    fun d hd => hfp₁ d (hperm₁.mem_iff.mpr (hperm₂.mem_iff.mp hd))
  obtain ⟨env₂, he₂, hnone₂, heq₂⟩ := evalConsts_from_fixedPoint_inv U fuel env₁ second [] []
    hearlier₂ hnodup₂ (by intro x hx; nomatch hx) (by intro c _; rfl) (by intro c hc; nomatch hc) hfp₂
  refine ⟨env₂, he₂, ?_⟩
  intro c
  by_cases hc : c ∈ second.map ConstDecl.id
  · exact heq₂ c (Or.inr hc)
  · have hc₁ : c ∉ first.map ConstDecl.id := by
      intro hmem
      exact hc ((hperm₂.map ConstDecl.id).mem_iff.mpr
        ((hperm₁.map ConstDecl.id).mem_iff.mp hmem))
    rw [hnone₂ c (by intro h; nomatch h) hc,
      evalConsts_lookup_not_mem U fuel first [] env₁ c he₁ hc₁]
    rfl

/-- Law (subject `ConcreteSpec.Core.ConstsAdmitted`, `ConcreteSpec.Core.evalConsts`). Any two
dependency orders of source agree on static typing, evaluation success, admission, and the
computed value of every constant identity. -/
theorem ConstOrderIndependent (U : ConstTypes) (fuel : Nat)
    (source first second : List (ConstDecl U))
    (hfirst : DependencyOrder U source first)
    (hsecond : DependencyOrder U source second) :
    (ConstsTyped U first ↔ ConstsTyped U second) ∧
    (ConstsEvaluate U fuel first ↔ ConstsEvaluate U fuel second) ∧
    (ConstsAdmitted U fuel first ↔ ConstsAdmitted U fuel second) ∧
    ∀ env₁ env₂, evalConsts U fuel first [] = .ok env₁ →
      evalConsts U fuel second [] = .ok env₂ →
      ∀ c, AList.lookup env₁ c = AList.lookup env₂ c := by
  have htyped : ConstsTyped U first ↔ ConstsTyped U second := by
    unfold ConstsTyped
    constructor
    · intro h
      cases ht₁ : typeConsts U first [] with
      | none => simp [ht₁] at h
      | some Γ₁ =>
        obtain ⟨Γ₂, ht₂, _⟩ := typeConsts_order_transfer U source first second hfirst hsecond ht₁
        simp [ht₂]
    · intro h
      cases ht₂ : typeConsts U second [] with
      | none => simp [ht₂] at h
      | some Γ₂ =>
        obtain ⟨Γ₁, ht₁, _⟩ := typeConsts_order_transfer U source second first hsecond hfirst ht₂
        simp [ht₁]
  have heval : ConstsEvaluate U fuel first ↔ ConstsEvaluate U fuel second := by
    unfold ConstsEvaluate
    constructor
    · intro h
      cases he₁ : evalConsts U fuel first [] with
      | error e => rw [he₁] at h; cases h
      | ok env₁ =>
        obtain ⟨env₂, he₂, _⟩ :=
          evalConsts_order_transfer U fuel source first second hfirst hsecond he₁
        simp [he₂, Except.isOk, Except.toBool]
    · intro h
      cases he₂ : evalConsts U fuel second [] with
      | error e => rw [he₂] at h; cases h
      | ok env₂ =>
        obtain ⟨env₁, he₁, _⟩ :=
          evalConsts_order_transfer U fuel source second first hsecond hfirst he₂
        simp [he₁, Except.isOk, Except.toBool]
  refine ⟨htyped, heval, ⟨fun ⟨ht, he⟩ => ⟨htyped.mp ht, heval.mp he⟩,
    fun ⟨ht, he⟩ => ⟨htyped.mpr ht, heval.mpr he⟩⟩, ?_⟩
  intro env₁ env₂ he₁ he₂ c
  obtain ⟨env₂', he₂', hlookup⟩ :=
    evalConsts_order_transfer U fuel source first second hfirst hsecond he₁
  rw [he₂] at he₂'
  cases he₂'
  exact (hlookup c).symm

/-- A broken admission predicate that depends on which declaration appears first in the list. -/
def firstIdAdmitted (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) : Prop :=
  ConstsAdmitted U fuel ds ∧ ds.head?.map ConstDecl.id = some (ConstDemo.cid "A")

/-- A broken declaration evaluator whose values depend on declaration position rather than only on
initializer semantics. -/
def orderSensitiveEvalConsts (U : ConstTypes) (_fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  match ds with
  | [] => .ok env
  | d :: rest =>
    .ok ((d.id, .int .u8 1) :: rest.map (fun d' => (d'.id, .int .u8 2)) ++ env)

/-- Instance (law `ConcreteSpec.Core.ConstOrderIndependent`, case `admission`). -/
example : (ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    (ConstsEvaluate ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsEvaluate ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    (ConstsAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    ∀ env₁ env₂, evalConsts ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
      evalConsts ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
      ∀ c, AList.lookup env₁ c = AList.lookup env₂ c :=
  ConstOrderIndependent ConstDemo.U 5
    [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
    [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
    [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
     ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `admission`,
subject `ConcreteSpec.Core.ConstsAdmitted`, mutant `ConcreteSpec.Core.firstIdAdmitted`). -/
example : DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧
    DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧
    ¬ ((ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (ConstsEvaluate ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsEvaluate ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (firstIdAdmitted ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      firstIdAdmitted ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      ∀ env₁ env₂, evalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
        evalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
        ∀ c, AList.lookup env₁ c = AList.lookup env₂ c) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, _, hadm, _⟩
  have h1 : firstIdAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] := ⟨by decide, rfl⟩
  have h2 := (hadm.mp h1).2
  revert h2
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `admission`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.orderSensitiveEvalConsts`). -/
example : DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧
    DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧
    ¬ ((ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (((orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] []).isOk = true) ↔
      ((orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] []).isOk = true)) ∧
      ((ConstsTyped ConstDemo.U [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧ (orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] []).isOk = true) ↔
      (ConstsTyped ConstDemo.U [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧ (orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] []).isOk = true)) ∧
      ∀ env₁ env₂, orderSensitiveEvalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
        orderSensitiveEvalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
        ∀ c, AList.lookup env₁ c = AList.lookup env₂ c) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, _, _, hvals⟩
  have h := hvals
    [(ConstDemo.cid "A", .int .u8 1), (ConstDemo.cid "B", .int .u8 2)]
    [(ConstDemo.cid "B", .int .u8 1), (ConstDemo.cid "A", .int .u8 2)]
    rfl rfl (ConstDemo.cid "A")
  revert h
  decide

/-- Instance (law `ConcreteSpec.Core.ConstOrderIndependent`, case `values`). -/
example : (ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsTyped ConstDemo.U
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    (ConstsEvaluate ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsEvaluate ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    (ConstsAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
    ConstsAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
    ∀ env₁ env₂, evalConsts ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
      evalConsts ConstDemo.U 5
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
      ∀ c, AList.lookup env₁ c = AList.lookup env₂ c :=
  ConstOrderIndependent ConstDemo.U 5
    [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
    [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
     ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
    [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
     ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `values`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.orderSensitiveEvalConsts`). -/
example : DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧
    DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧
    ¬ ((ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (((orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] []).isOk = true) ↔
      ((orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] []).isOk = true)) ∧
      ((ConstsTyped ConstDemo.U [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧ (orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] []).isOk = true) ↔
      (ConstsTyped ConstDemo.U [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧ (orderSensitiveEvalConsts ConstDemo.U 5 [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] []).isOk = true)) ∧
      ∀ env₁ env₂, orderSensitiveEvalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
        orderSensitiveEvalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
        ∀ c, AList.lookup env₁ c = AList.lookup env₂ c) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, _, _, hvals⟩
  have h := hvals
    [(ConstDemo.cid "A", .int .u8 1), (ConstDemo.cid "B", .int .u8 2)]
    [(ConstDemo.cid "B", .int .u8 1), (ConstDemo.cid "A", .int .u8 2)]
    rfl rfl (ConstDemo.cid "A")
  revert h
  decide

/-- Planted (law `ConcreteSpec.Core.ConstOrderIndependent`, case `values`,
subject `ConcreteSpec.Core.ConstsAdmitted`, mutant `ConcreteSpec.Core.firstIdAdmitted`). -/
example : DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ∧
    DependencyOrder ConstDemo.U
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩]
      [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
       ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] ∧
    ¬ ((ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsTyped ConstDemo.U
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (ConstsEvaluate ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      ConstsEvaluate ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      (firstIdAdmitted ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] ↔
      firstIdAdmitted ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩]) ∧
      ∀ env₁ env₂, evalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
         ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] [] = .ok env₁ →
        evalConsts ConstDemo.U 5
        [⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩,
         ⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩] [] = .ok env₂ →
        ∀ c, AList.lookup env₁ c = AList.lookup env₂ c) := by
  refine ⟨by decide, by decide, ?_⟩
  rintro ⟨_, _, hadm, _⟩
  have h1 : firstIdAdmitted ConstDemo.U 5
      [⟨ConstDemo.cid "A", .c (.int .u8), .int .u8 10⟩,
       ⟨ConstDemo.cid "B", .c (.int .u8), .int .u8 20⟩] := ⟨by decide, rfl⟩
  have h2 := (hadm.mp h1).2
  revert h2
  decide


end ConcreteSpec.Core
