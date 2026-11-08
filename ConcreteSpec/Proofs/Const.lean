import ConcreteSpec.Spec.Const
import ConcreteSpec.Proofs.Scalar

/-!
# The laws of constants (`Spec/Const.lean`, foundation contract C2)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

- **Totality** (`eval_total`): a well-typed initializer has a value, of its type and valid, when
  the earlier constants' values agree with their types. No operation of the subset can fail.
- **Validity** (`eval_valid`): every value an initializer computes is valid (an integer in range).
- **Copy only** (`admitted_copy`): every constant of admitted declarations has a `Copy` type, and
  so does every value it can hold (`ofC_copy`).
- **Static admission** (`typed_admitted`): declarations that type (`typeConsts`) are admitted, so
  their evaluation at compile time cannot fail.
- **Declaration soundness** (`evalConsts_sound`): after admitted declarations, every declared
  constant has a valid value whose type is its declared type.
-/

namespace ConcreteSpec.Core

/-- The earlier constants' values agree with their types, and are valid. -/
def Agree (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  ∀ c T, AList.lookup Γ c = some T → ∃ v, AList.lookup env c = some v ∧ v.ty = T ∧ v.Valid

theorem CVal.int_of_ty {v : CVal} {t : IntTy} (h : v.ty = .int t) : ∃ n, v = .int t n := by
  cases v with
  | int t' n => cases h; exact ⟨n, rfl⟩
  | bool _ => cases h
  | unit => cases h

theorem CVal.bool_of_ty {v : CVal} (h : v.ty = .bool) : ∃ b, v = .bool b := by
  cases v with
  | int _ _ => cases h
  | bool b => exact ⟨b, rfl⟩
  | unit => cases h

theorem sameIntTy_some {x y : Option CTy} {t : IntTy} (h : sameIntTy x y = some t) :
    x = some (.int t) ∧ y = some (.int t) := by
  match x, y, h with
  | some (.int a), some (.int b), h =>
    simp only [sameIntTy] at h
    split at h
    · cases h; rename_i hab; subst hab; exact ⟨rfl, rfl⟩
    · cases h

theorem bothBool_true {x y : Option CTy} (h : bothBool x y = true) :
    x = some .bool ∧ y = some .bool := by
  match x, y, h with
  | some .bool, some .bool, _ => exact ⟨rfl, rfl⟩

/-- **Totality.** A well-typed initializer has a value of its type, and the value is valid. -/
theorem eval_total (env : AList ConstId CVal) (Γ : AList ConstId CTy) (hag : Agree env Γ) :
    ∀ (e : CExpr) (T : CTy), e.ty? Γ = some T → ∃ v, e.eval env = some v ∧ v.ty = T ∧ v.Valid
  | .int t n, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · cases h
      exact ⟨.int t n, by simp [CExpr.eval, *], rfl, by assumption⟩
    · cases h
  | .bool b, T, h => by cases h; exact ⟨.bool b, rfl, rfl, trivial⟩
  | .unit, T, h => by cases h; exact ⟨.unit, rfl, rfl, trivial⟩
  | .ref c, T, h => hag c T h
  | .wrapping op a b, T, h => by
    simp only [CExpr.ty?, Option.map_eq_some_iff] at h
    obtain ⟨t, ht, rfl⟩ := h
    obtain ⟨ha, hb⟩ := sameIntTy_some ht
    obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
    obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
    obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
    obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
    exact ⟨.int t (Core.wrapping t op x y), by simp [CExpr.eval, hea, heb, sameInt], rfl,
      wrap_inRange t _⟩
  | .saturating op a b, T, h => by
    simp only [CExpr.ty?, Option.map_eq_some_iff] at h
    obtain ⟨t, ht, rfl⟩ := h
    obtain ⟨ha, hb⟩ := sameIntTy_some ht
    obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
    obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
    obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
    obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
    exact ⟨.int t (Core.saturating t op x y), by simp [CExpr.eval, hea, heb, sameInt], rfl,
      saturate_inRange t _⟩
  | .bit op a b, T, h => by
    simp only [CExpr.ty?, Option.map_eq_some_iff] at h
    obtain ⟨t, ht, rfl⟩ := h
    obtain ⟨ha, hb⟩ := sameIntTy_some ht
    obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
    obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
    obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
    obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
    exact ⟨.int t (Core.bitwise t op x y), by simp [CExpr.eval, hea, heb, sameInt], rfl,
      bitwise_inRange t op x y⟩
  | .bitNot a, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · rename_i t ha
      cases h
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
      obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
      exact ⟨.int t (Core.bitNot t x), by simp [CExpr.eval, hea], rfl, bitNot_inRange t x⟩
    · cases h
  | .cmp op a b, T, h => by
    simp only [CExpr.ty?, Option.map_eq_some_iff] at h
    obtain ⟨t, ht, rfl⟩ := h
    obtain ⟨ha, hb⟩ := sameIntTy_some ht
    obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
    obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
    obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
    obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
    exact ⟨.bool (op.eval x y), by simp [CExpr.eval, hea, heb, sameInt], rfl, trivial⟩
  | .not a, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · rename_i ha
      cases h
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
      obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
      exact ⟨.bool (!x), by simp [CExpr.eval, hea], rfl, trivial⟩
    · cases h
  | .and a b, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · rename_i hbb
      cases h
      obtain ⟨ha, hb⟩ := bothBool_true hbb
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
      obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
      exact ⟨.bool (x && y), by simp [CExpr.eval, hea, heb, twoBools], rfl, trivial⟩
    · cases h
  | .or a b, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · rename_i hbb
      cases h
      obtain ⟨ha, hb⟩ := bothBool_true hbb
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb
      obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
      exact ⟨.bool (x || y), by simp [CExpr.eval, hea, heb, twoBools], rfl, trivial⟩
    · cases h
  | .convert t a, T, h => by
    simp only [CExpr.ty?] at h
    split at h
    · rename_i s ha
      split at h
      · rename_i hl
        cases h
        obtain ⟨va, hea, hta, hva⟩ := eval_total env Γ hag a _ ha
        obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
        exact ⟨.int t x, by simp [CExpr.eval, hea, hl], rfl, (lossless_iff s t).1 hl x hva⟩
      · cases h
    · rename_i ha
      cases h
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha
      obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
      exact ⟨.int t (boolToInt x), by simp [CExpr.eval, hea], rfl, boolToInt_inRange t x⟩
    · cases h

/-- **Validity.** Every value an initializer computes from valid constants is valid. -/
theorem eval_valid (env : AList ConstId CVal)
    (henv : ∀ c v, AList.lookup env c = some v → v.Valid) :
    ∀ (e : CExpr) (v : CVal), e.eval env = some v → v.Valid := by
  intro e
  induction e with
  | int t n =>
    intro v h; simp only [CExpr.eval] at h; split at h
    · cases h; assumption
    · cases h
  | bool b => intro v h; cases h; trivial
  | unit => intro v h; cases h; trivial
  | ref c => exact henv c
  | wrapping op a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
                       obtain ⟨⟨t, x, y⟩, _, rfl⟩ := h; exact wrap_inRange t _
  | saturating op a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
                         obtain ⟨⟨t, x, y⟩, _, rfl⟩ := h; exact saturate_inRange t _
  | bit op a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
                  obtain ⟨⟨t, x, y⟩, _, rfl⟩ := h; exact bitwise_inRange t op x y
  | bitNot a => intro v h; simp only [CExpr.eval] at h; split at h
                · cases h; exact bitNot_inRange _ _
                · cases h
  | cmp op a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
                  obtain ⟨_, _, rfl⟩ := h; trivial
  | not a => intro v h; simp only [CExpr.eval] at h; split at h
             · cases h; trivial
             · cases h
  | and a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
               obtain ⟨_, _, rfl⟩ := h; trivial
  | or a b => intro v h; simp only [CExpr.eval, Option.map_eq_some_iff] at h
              obtain ⟨_, _, rfl⟩ := h; trivial
  | convert t a ih =>
    intro v h; simp only [CExpr.eval] at h; split at h
    · rename_i s x hx
      split at h
      · rename_i hl; cases h; exact (lossless_iff s t).1 hl x (ih _ hx)
      · cases h
    · cases h; exact boolToInt_inRange _ _
    · cases h

/-- **Copy only.** Every constant of admitted declarations has a `Copy` declared type. -/
theorem admitted_copy (U : ConstTypes) : ∀ (ds : List (ConstDecl U)) (env : AList ConstId CVal),
    (evalConsts U ds env).isSome = true → ∀ d ∈ ds, U.isCopy d.ty = true
  | [], _, _, d, hd => nomatch hd
  | d :: ds, env, h, d', hd' => by
    simp only [evalConsts] at h
    split at h
    · split at h
      · rename_i v _ hc
        cases hd' with
        | head => exact hc.1
        | tail _ hm => exact admitted_copy U ds _ h d' hm
      · simp at h
    · simp at h

/-- A value of the subset always has a `Copy` type. -/
theorem ofC_copy (U : ConstTypes) (v : CVal) : U.isCopy (U.ofC v.ty) = true := U.ofC_copy _

/-- The constants' values and types correspond: they agree, and a name with no type has no value. -/
def Matches (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  Agree env Γ ∧ ∀ c, AList.lookup Γ c = none → AList.lookup env c = none

theorem Matches.cons {env : AList ConstId CVal} {Γ : AList ConstId CTy} (h : Matches env Γ)
    (c : ConstId) (v : CVal) (hv : v.Valid) : Matches ((c, v) :: env) ((c, v.ty) :: Γ) := by
  refine ⟨fun c' T hT => ?_, fun c' hc' => ?_⟩
  · simp only [AList.lookup] at hT ⊢
    by_cases hc : c = c'
    · simp only [hc, ↓reduceIte, Option.some.injEq] at hT ⊢
      exact ⟨v, rfl, hT, hv⟩
    · simp only [hc, ↓reduceIte] at hT ⊢
      exact h.1 c' T hT
  · simp only [AList.lookup] at hc' ⊢
    by_cases hc : c = c'
    · simp [hc] at hc'
    · simp only [hc, ↓reduceIte] at hc' ⊢
      exact h.2 c' hc'

/-- **Static admission.** Declarations that type, from constants whose values match their types,
evaluate. -/
theorem typed_evaluates (U : ConstTypes) : ∀ (ds : List (ConstDecl U)) (env : AList ConstId CVal)
    (Γ Γ' : AList ConstId CTy), Matches env Γ → typeConsts U ds Γ = some Γ' →
    (evalConsts U ds env).isSome = true
  | [], _, _, _, _, _ => rfl
  | d :: ds, env, Γ, Γ', hm, h => by
    simp only [typeConsts] at h
    split at h
    · rename_i T hT
      split at h
      · rename_i hc
        obtain ⟨hcopy, hty, hfresh⟩ := hc
        obtain ⟨v, hv, hvt, hvv⟩ := eval_total env Γ hm.1 d.init T hT
        have hm' := hm.cons d.id v hvv
        rw [hvt] at hm'
        simp only [evalConsts, hv, hvt, hcopy, hty, hm.2 _ hfresh, and_self, ↓reduceIte]
        exact typed_evaluates U ds _ _ Γ' hm' h
      · cases h
    · cases h

/-- Declarations that type are admitted. -/
theorem typed_admitted (U : ConstTypes) (ds : List (ConstDecl U)) (Γ : AList ConstId CTy)
    (h : typeConsts U ds [] = some Γ) : ConstsAdmitted U ds :=
  typed_evaluates U ds [] [] Γ ⟨fun _ _ h => (by cases h), fun _ _ => rfl⟩ h

/-- Evaluation only adds constants: what was known stays, with its value. -/
theorem evalConsts_mono (U : ConstTypes) : ∀ (ds : List (ConstDecl U)) (env env' : AList ConstId CVal),
    evalConsts U ds env = some env' → ∀ c v, AList.lookup env c = some v → AList.lookup env' c = some v
  | [], _, _, h, c, v, hc => by cases h; exact hc
  | d :: ds, env, env', h, c, v, hc => by
    simp only [evalConsts] at h
    split at h
    · split at h
      · rename_i hcond
        refine evalConsts_mono U ds _ env' h c v ?_
        have hne : d.id ≠ c := by
          intro he; rw [he, hc] at hcond; exact absurd hcond.2.2 (by simp)
        simp [AList.lookup, hne, hc]
      · cases h
    · cases h

/-- **Declaration soundness.** After admitted declarations, from valid constants, every declared
constant has a valid value of its declared type, and every value is valid. -/
theorem evalConsts_sound (U : ConstTypes) : ∀ (ds : List (ConstDecl U)) (env env' : AList ConstId CVal),
    (∀ c v, AList.lookup env c = some v → v.Valid) → evalConsts U ds env = some env' →
    (∀ d ∈ ds, ∃ v, AList.lookup env' d.id = some v ∧ U.ofC v.ty = d.ty ∧ v.Valid) ∧
      (∀ c v, AList.lookup env' c = some v → v.Valid)
  | [], _, _, hv, h => by cases h; exact ⟨fun _ hd => (nomatch hd), hv⟩
  | d :: ds, env, env', hv, h => by
    simp only [evalConsts] at h
    split at h
    · rename_i v hev
      split at h
      · rename_i hcond
        have hvv : v.Valid := eval_valid env hv d.init v hev
        have hv' : ∀ c w, AList.lookup ((d.id, v) :: env) c = some w → w.Valid := by
          intro c w hw
          simp only [AList.lookup] at hw
          split at hw
          · cases hw; exact hvv
          · exact hv c w hw
        obtain ⟨hrest, hall⟩ := evalConsts_sound U ds _ env' hv' h
        refine ⟨fun d' hd' => ?_, hall⟩
        cases hd' with
        | head =>
          refine ⟨v, evalConsts_mono U ds _ env' h d.id v (by simp [AList.lookup]), hcond.2.1, hvv⟩
        | tail _ hm => exact hrest d' hm
      · cases h
    · cases h

end ConcreteSpec.Core
