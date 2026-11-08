import ConcreteSpec.Spec.Const
import ConcreteSpec.Proofs.Scalar

/-!
# The laws of constants (`Spec/Const.lean`, foundation contract C2)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

`eval_total` preserves totality for the fallible-free fragment with sufficient fuel.
`evalConsts_sound` derives declared-type agreement and validity using independent static typing
and structural evaluation. Copy and admission-conjunction projections are explicitly helpers.
-/

namespace ConcreteSpec.Core

/-- The earlier constants' values agree with their types, and are valid. -/
def Agree (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  ∀ c T, AList.lookup Γ c = some T → ∃ v, AList.lookup env c = some v ∧ v.ty = T ∧ v.Valid

/-- Helper (inversion). Successful binding exposes both successful computations.
(approved: PENDING) -/
private theorem bind_value_inv (r : CEvalResult) (f : CVal → CEvalResult) (v : CVal) :
    r.bind f = .value v ↔ ∃ a, r = .value a ∧ f a = .value v := by
  cases r <;> simp [CEvalResult.bind]

/-- Helper (inversion). Integer operand extraction. (approved: PENDING) -/
private theorem sameInt_some_inv {x y : CVal} {t : IntTy} {a b : Int}
    (h : sameInt (some x) (some y) = some (t, a, b)) :
    x = .int t a ∧ y = .int t b := by
  cases x <;> cases y <;> simp [sameInt] at h
  obtain ⟨rfl, rfl, rfl, rfl⟩ := h
  exact ⟨rfl, rfl⟩

/-- Helper (inversion). Boolean operand extraction. (approved: PENDING) -/
private theorem twoBools_some_inv {x y : CVal} {a b : Bool}
    (h : twoBools (some x) (some y) = some (a, b)) :
    x = .bool a ∧ y = .bool b := by
  cases x <;> cases y <;> simp [twoBools] at h
  obtain ⟨rfl, rfl⟩ := h
  exact ⟨rfl, rfl⟩

/-- Helper (computation). Structural preservation for successful initializer evaluation;
used by declaration soundness rather than counted as independent evidence. (approved: PENDING) -/
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
  | tryConvert t a ih =>
    intro fuel v h; cases fuel with
    | zero => cases h
    | succ fuel =>
      simp only [CExpr.eval, bind_value_inv] at h
      obtain ⟨va, ha, h⟩ := h
      obtain ⟨hta, hva⟩ := ih fuel va ha
      cases va with
      | int src x =>
        simp only at h
        cases hr : Core.tryConvert t x with
        | ok r =>
          simp only [hr] at h; cases h
          exact ⟨by simp [CExpr.ty?, hta, CVal.ty], (by
              obtain ⟨hrange, rfl⟩ := (tryConvert_ok_iff _ _ _).1 hr
              exact hrange)⟩
        | error err => cases err; simp [hr] at h
      | bool b =>
        cases h
      | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: PENDING) -/
private theorem CVal.int_of_ty {v : CVal} {t : IntTy} (h : v.ty = .int t) : ∃ n, v = .int t n := by
  cases v with
  | int t' n => cases h; exact ⟨n, rfl⟩
  | bool _ => cases h
  | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: PENDING) -/
private theorem CVal.bool_of_ty {v : CVal} (h : v.ty = .bool) : ∃ b, v = .bool b := by
  cases v with
  | int _ _ => cases h
  | bool b => exact ⟨b, rfl⟩
  | unit => cases h

/-- Helper (inversion). Operand typing inversion. (approved: PENDING) -/
private theorem sameIntTy_some {x y : Option CTy} {t : IntTy} (h : sameIntTy x y = some t) :
    x = some (.int t) ∧ y = some (.int t) := by
  match x, y, h with
  | some (.int a), some (.int b), h =>
    simp only [sameIntTy] at h
    split at h
    · cases h; rename_i hab; subst hab; exact ⟨rfl, rfl⟩
    · cases h

/-- Helper (inversion). Operand typing inversion. (approved: PENDING) -/
private theorem bothBool_true {x y : Option CTy} (h : bothBool x y = true) :
    x = some .bool ∧ y = some .bool := by
  match x, y, h with
  | some .bool, some .bool, _ => exact ⟨rfl, rfl⟩


/-- Law (subject `ConcreteSpec.Core.CExpr.eval`). The old fragment evaluates with sufficient
fuel; static typing supplies the result type and scalar validity. -/
theorem eval_total (env : AList ConstId CVal) (Γ : AList ConstId CTy) (hag : Agree env Γ) :
      ∀ (e : CExpr) (T : CTy), e.ty? Γ = some T → e.fallibleFree = true →
        ∀ fuel, e.height ≤ fuel → ∃ v, e.eval env fuel = .value v ∧ v.ty = T ∧ v.Valid
  | .int t n, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      simp only [CExpr.ty?] at h
      split at h
      · cases h
        exact ⟨.int t n, by simp [CExpr.eval, *], rfl, by assumption⟩
      · cases h
  | .bool b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      cases h; exact ⟨.bool b, rfl, rfl, trivial⟩
  | .unit, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      cases h; exact ⟨.unit, rfl, rfl, trivial⟩
  | .ref c, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      obtain ⟨v, hv, hty, hvalid⟩ := hag c T h
      exact ⟨v, by simp [CExpr.eval, hv], hty, hvalid⟩
  | .wrapping op a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
      obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
      exact ⟨.int t (Core.wrapping t op x y), by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl,
        wrap_inRange t _⟩
  | .saturating op a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
      obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
      exact ⟨.int t (Core.saturating t op x y), by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl,
        saturate_inRange t _⟩
  | .bit op a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
      obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
      exact ⟨.int t (Core.bitwise t op x y), by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl,
        bitwise_inRange t op x y⟩
  | .bitNot a, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfa : a.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i t ha
        cases h
        obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfa fuel hna
        obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
        exact ⟨.int t (Core.bitNot t x), by simp [CExpr.eval, hea, CEvalResult.bind], rfl, bitNot_inRange t x⟩
      · cases h
  | .cmp op a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?, Option.map_eq_some_iff] at h
      obtain ⟨t, ht, rfl⟩ := h
      obtain ⟨ha, hb⟩ := sameIntTy_some ht
      obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
      obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
      obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
      obtain ⟨y, rfl⟩ := CVal.int_of_ty htb
      exact ⟨.bool (op.eval x y), by simp [CExpr.eval, hea, heb, sameInt, CEvalResult.bind], rfl, trivial⟩
  | .not a, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfa : a.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i ha
        cases h
        obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfa fuel hna
        obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
        exact ⟨.bool (!x), by simp [CExpr.eval, hea, CEvalResult.bind], rfl, trivial⟩
      · cases h
  | .and a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i hbb
        cases h
        obtain ⟨ha, hb⟩ := bothBool_true hbb
        obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
        obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
        obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
        obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
        exact ⟨.bool (x && y), by simp [CExpr.eval, hea, heb, twoBools, CEvalResult.bind], rfl, trivial⟩
      · cases h
  | .or a b, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfs : a.fallibleFree = true ∧ b.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hns : a.height ≤ fuel ∧ b.height ≤ fuel := by simpa [CExpr.height, Nat.max_le] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i hbb
        cases h
        obtain ⟨ha, hb⟩ := bothBool_true hbb
        obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfs.1 fuel hns.1
        obtain ⟨vb, heb, htb, _⟩ := eval_total env Γ hag b _ hb hfs.2 fuel hns.2
        obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
        obtain ⟨y, rfl⟩ := CVal.bool_of_ty htb
        exact ⟨.bool (x || y), by simp [CExpr.eval, hea, heb, twoBools, CEvalResult.bind], rfl, trivial⟩
      · cases h
  | .convert t a, T, h => by
    intro hf fuel hn
    cases fuel with
    | zero => simp [CExpr.height] at hn
    | succ fuel =>
      have hfa : a.fallibleFree = true := by simpa [CExpr.fallibleFree] using hf
      have hna : a.height ≤ fuel := by simpa [CExpr.height] using hn
      simp only [CExpr.ty?] at h
      split at h
      · rename_i s ha
        split at h
        · rename_i hl
          cases h
          obtain ⟨va, hea, hta, hva⟩ := eval_total env Γ hag a _ ha hfa fuel hna
          obtain ⟨x, rfl⟩ := CVal.int_of_ty hta
          exact ⟨.int t x, by simp [CExpr.eval, hea, hl, CEvalResult.bind], rfl, (lossless_iff s t).1 hl x hva⟩
        · cases h
      · rename_i ha
        cases h
        obtain ⟨va, hea, hta, _⟩ := eval_total env Γ hag a _ ha hfa fuel hna
        obtain ⟨x, rfl⟩ := CVal.bool_of_ty hta
        exact ⟨.int t (boolToInt x), by simp [CExpr.eval, hea, CEvalResult.bind], rfl, boolToInt_inRange t x⟩
      · cases h

  | .plain _ _ _, _, _ => by intro hf; simp [CExpr.fallibleFree] at hf
  | .plainNeg _, _, _ => by intro hf; simp [CExpr.fallibleFree] at hf
  | .tryConvert _ _, _, _ => by intro hf; simp [CExpr.fallibleFree] at hf

/-- A deliberately broken evaluator, retaining the exact evaluator interface. -/
def noConstValues (_env : AList ConstId CVal) (_fuel : Nat) (_e : CExpr) : CEvalResult :=
  .outOfFuel

/-- Instance (law `ConcreteSpec.Core.eval_total`, case `wrap`). -/
example : ∃ v, (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).eval [] 2 = .value v ∧
    v.ty = .int .u8 ∧ v.Valid :=
  eval_total [] [] (by intro c T h; cases h)
    (.wrapping .add (.int .u8 255) (.int .u8 1)) (.int .u8) (by decide) (by decide) 2 (by decide)

/-- Planted (law `ConcreteSpec.Core.eval_total`, case `wrap`,
subject `ConcreteSpec.Core.CExpr.eval`, mutant `ConcreteSpec.Core.noConstValues`). -/
example : Agree [] [] ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).ty? [] = some (.int .u8) ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).fallibleFree = true ∧
    (CExpr.wrapping .add (.int .u8 255) (.int .u8 1)).height ≤ 2 ∧
    ¬ ∃ v, noConstValues [] 2 (.wrapping .add (.int .u8 255) (.int .u8 1)) = .value v ∧
      v.ty = .int .u8 ∧ v.Valid := by
  refine ⟨(by intro c T h; cases h), by decide, by decide, by decide, ?_⟩
  simp [noConstValues]

/-- Helper (computation). Static typing preserves previous type bindings because declaration
names must be fresh. (approved: PENDING) -/
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
identity, independently of evaluation. (approved: PENDING) -/
private theorem typeConsts_decls_inv (U : ConstTypes) : ∀ ds Γ Γ',
    typeConsts U ds Γ = some Γ' → ∀ d ∈ ds,
      ∃ T, AList.lookup Γ' d.id = some T ∧ U.ofC T = d.ty ∧ U.isCopy d.ty = true
  | [], _, _, _, _, hd => nomatch hd
  | d :: ds, Γ, Γ', h, d', hd => by
    simp only [typeConsts] at h
    split at h
    · rename_i T ht
      split at h
      · rename_i cond
        cases hd with
        | head => exact ⟨T, typeConsts_mono_inv U ds _ Γ' h d.id T (by simp [AList.lookup]),
            cond.2.1, cond.1⟩
        | tail _ hm => exact typeConsts_decls_inv U ds _ Γ' h d' hm
      · cases h
    · cases h

/-- The value environment matches the static type environment in both directions. -/
def Matches (env : AList ConstId CVal) (Γ : AList ConstId CTy) : Prop :=
  ∀ c, (AList.lookup env c).map CVal.ty = AList.lookup Γ c ∧
    ∀ v, AList.lookup env c = some v → v.Valid

/-- Helper (computation). Independent typing and evaluation produce matching environments.
(approved: PENDING) -/
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
that every declaration has a valid value of its declared type. Admission's evaluation conjunct
alone does not supply this property. (approved: PENDING) -/
private theorem evalConsts_sound_inv (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) (Γ : AList ConstId CTy)
    (ht : typeConsts U ds [] = some Γ) (he : evalConsts U fuel ds [] = .ok env) :
    ∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧ U.ofC v.ty = d.ty ∧ v.Valid := by
  have hm := typed_eval_matches_inv U fuel ds [] env [] Γ
    (by intro c; exact ⟨rfl, by intro v h; cases h⟩) ht he
  intro d hd
  obtain ⟨T, hT, hty, _⟩ := typeConsts_decls_inv U ds [] Γ ht d hd
  have hv := (hm d.id).1
  rw [hT] at hv
  cases hc : AList.lookup env d.id with
  | none => simp [hc] at hv
  | some v =>
    simp only [hc, Option.map_some, Option.some.injEq] at hv
    exact ⟨v, rfl, hv ▸ hty, (hm d.id).2 v hc⟩

/-- Law (subject `ConcreteSpec.Core.evalConsts`). Every admitted declaration has a valid value
of its declared type, established by structural evaluation and independent static typing. -/
theorem evalConsts_sound (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (h : ConstsAdmitted U fuel ds) :
    ∃ env, evalConsts U fuel ds [] = .ok env ∧
      ∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧ U.ofC v.ty = d.ty ∧ v.Valid := by
  obtain ⟨ht, he⟩ := h
  unfold ConstsTyped at ht
  unfold ConstsEvaluate at he
  cases ht' : typeConsts U ds [] with
  | none => simp [ht'] at ht
  | some Γ =>
    cases he' : evalConsts U fuel ds [] with
    | error e => rw [he'] at he; cases he
    | ok env => exact ⟨env, rfl, evalConsts_sound_inv U fuel ds env Γ ht' he'⟩

/-- A broken declaration evaluator that returns `bool` for every declaration. -/
def boolConstValues (U : ConstTypes) (_fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  .ok (ds.map (fun d => (d.id, CVal.bool true)) ++ env)

/-- Instance (law `ConcreteSpec.Core.evalConsts_sound`, case `division`). -/
example : ∃ env, evalConsts ConstDemo.U 3
      [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩] [] = .ok env ∧
    ∀ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "Q") (.c (.int .i8))
        (.plain .div (.int .i8 100) (.int .i8 7))],
      ∃ v, AList.lookup env d.id = some v ∧ ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid :=
  evalConsts_sound ConstDemo.U 3
    [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩] (by decide)

/-- Planted (law `ConcreteSpec.Core.evalConsts_sound`, case `division`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.boolConstValues`). -/
example : ((typeConsts ConstDemo.U
      [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩] []).isSome = true ∧
    (boolConstValues ConstDemo.U 3
      [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩] []).isOk = true) ∧
    ¬ ∃ env, boolConstValues ConstDemo.U 3
      [⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩] [] = .ok env ∧
      ∀ d ∈ [ConstDecl.mk (U := ConstDemo.U) (ConstDemo.cid "Q") (.c (.int .i8))
        (.plain .div (.int .i8 100) (.int .i8 7))],
        ∃ v, AList.lookup env d.id = some v ∧ ConstDemo.U.ofC v.ty = d.ty ∧ v.Valid := by
  constructor
  · decide
  · rintro ⟨env, he, hall⟩
    have heq : env = [(ConstDemo.cid "Q", CVal.bool true)] := Except.ok.inj he.symm
    subst env
    obtain ⟨v, hv, hty, _⟩ := hall
      ⟨ConstDemo.cid "Q", .c (.int .i8), .plain .div (.int .i8 100) (.int .i8 7)⟩ (List.Mem.head _)
    simp only [AList.lookup, ↓reduceIte, Option.some.injEq] at hv
    subst v
    cases hty

/-- Helper (projection). Copy is a static typing guard, not an evaluation soundness law.
The value's declared-type agreement is independently established by `evalConsts_sound`.
(approved: PENDING) -/
theorem admitted_copy_proj (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (h : ConstsAdmitted U fuel ds) : ∀ d ∈ ds, U.isCopy d.ty = true := by
  cases ht : typeConsts U ds [] with
  | none => simp [ConstsAdmitted, ConstsTyped, ht] at h
  | some Γ =>
    intro d hd
    obtain ⟨_, _, _, hc⟩ := typeConsts_decls_inv U ds [] Γ ht d hd
    exact hc

/-- Representation lemma. The concrete scalar embedding classifies each scalar as Copy without
assuming a universal Copy field in `ConstTypes`. (approved: PENDING) -/
theorem ofC_copy (v : CVal) : ConstDemo.U.isCopy (ConstDemo.U.ofC v.ty) = true := by
  cases v <;> rfl

/-- Helper (projection). Admission is the conjunction of two separately named checks, rather than
an independent semantic equivalence law. (approved: PENDING) -/
theorem admitted_iff_typed_evaluates_proj (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U)) :
    ConstsAdmitted U fuel ds ↔
      (typeConsts U ds []).isSome = true ∧ (evalConsts U fuel ds []).isOk = true := Iff.rfl

/-- Law (subject `ConcreteSpec.Core.evalConsts`). Copy classification of computed values follows
from independent static declarations and evaluator type soundness, with no Copy guard in evaluation. -/
theorem admitted_copy (U : ConstTypes) (fuel : Nat) (ds : List (ConstDecl U))
    (h : ConstsAdmitted U fuel ds) :
    ∃ env, evalConsts U fuel ds [] = .ok env ∧
      ∀ d ∈ ds, ∃ v, AList.lookup env d.id = some v ∧ U.isCopy (U.ofC v.ty) = true := by
  obtain ⟨env, he, hs⟩ := evalConsts_sound U fuel ds h
  refine ⟨env, he, ?_⟩
  intro d hd
  obtain ⟨v, hv, ht, _⟩ := hs d hd
  exact ⟨v, hv, ht ▸ admitted_copy_proj U fuel ds h d hd⟩

/-- A broken evaluator mapping every declaration to a non-Copy unit in the test universe. -/
def unitConstValues (U : ConstTypes) (_fuel : Nat) (ds : List (ConstDecl U))
    (env : AList ConstId CVal) : Except ConstEvalRefusal (AList ConstId CVal) :=
  .ok (ds.map (fun d => (d.id, CVal.unit)) ++ env)

/-- Instance (law `ConcreteSpec.Core.admitted_copy`, case `noncopy_universe`). -/
example : ∃ env, evalConsts ConstDemo.NonCopyU 1
      [⟨ConstDemo.cid "B", .c .bool, .bool true⟩] [] = .ok env ∧
    ∀ d ∈ [ConstDecl.mk (U := ConstDemo.NonCopyU) (ConstDemo.cid "B") (.c .bool) (.bool true)],
      ∃ v, AList.lookup env d.id = some v ∧
        ConstDemo.NonCopyU.isCopy (ConstDemo.NonCopyU.ofC v.ty) = true :=
  admitted_copy ConstDemo.NonCopyU 1 [⟨ConstDemo.cid "B", .c .bool, .bool true⟩] (by decide)

/-- Planted (law `ConcreteSpec.Core.admitted_copy`, case `noncopy_universe`,
subject `ConcreteSpec.Core.evalConsts`, mutant `ConcreteSpec.Core.unitConstValues`). -/
example : ((typeConsts ConstDemo.NonCopyU
      [⟨ConstDemo.cid "B", .c .bool, .bool true⟩] []).isSome = true ∧
    (unitConstValues ConstDemo.NonCopyU 1
      [⟨ConstDemo.cid "B", .c .bool, .bool true⟩] []).isOk = true) ∧
    ¬ ∃ env, unitConstValues ConstDemo.NonCopyU 1
      [⟨ConstDemo.cid "B", .c .bool, .bool true⟩] [] = .ok env ∧
      ∀ d ∈ [ConstDecl.mk (U := ConstDemo.NonCopyU) (ConstDemo.cid "B") (.c .bool) (.bool true)],
        ∃ v, AList.lookup env d.id = some v ∧
          ConstDemo.NonCopyU.isCopy (ConstDemo.NonCopyU.ofC v.ty) = true := by
  constructor
  · decide
  · rintro ⟨env, he, hall⟩
    have heq : env = [(ConstDemo.cid "B", CVal.unit)] := Except.ok.inj he.symm
    subst env
    obtain ⟨v, hv, hc⟩ := hall ⟨ConstDemo.cid "B", .c .bool, .bool true⟩ (List.Mem.head _)
    simp only [AList.lookup, ↓reduceIte, Option.some.injEq] at hv
    subst v
    cases hc

/-- Helper (computation). Boolean dependency testing agrees with actual source membership.
(approved: PENDING) -/
private theorem dependsOn_reference_inv (U : ConstTypes) (ds : List (ConstDecl U)) (a b : ConstId)
    (h : dependsOn U ds a b = true) : SourceReference U ds a b := by
  simpa [dependsOn, SourceReference, List.any_eq_true, Bool.and_eq_true,
    List.contains_iff_mem] using h

/-- Helper (computation). Structural chain checking implies real edges, by induction on the
chain rather than assuming an edge certificate. (approved: PENDING) -/
private theorem refChain_actual_inv (U : ConstTypes) (ds : List (ConstDecl U)) :
    ∀ chain, refChain U ds chain = true → ActualRefChain U ds chain
  | [], h => by cases h
  | [c], h => by
    simpa [refChain, ActualRefChain, List.contains_iff_mem, List.mem_map] using h
  | a :: b :: rest, h => by
    have hh : dependsOn U ds a b = true ∧ refChain U ds (b :: rest) = true := by
      simpa only [refChain, Bool.and_eq_true] using h
    obtain ⟨hab, hrest⟩ := hh
    exact ⟨dependsOn_reference_inv U ds a b hab, refChain_actual_inv U ds (b :: rest) hrest⟩

/-- Law (subject `ConcreteSpec.Core.refChain`). A reported cycle closes at its starting identity
and every edge is an actual reference in a source initializer. -/
theorem cycle_chain_real (U : ConstTypes) (ds : List (ConstDecl U)) :
    ∀ chain, CycleChain U ds chain →
      ∃ c rest, chain = c :: rest ++ [c] ∧ ActualRefChain U ds chain := by
  intro chain h
  obtain ⟨c, rest, he, hc⟩ := h
  exact ⟨c, rest, he, refChain_actual_inv U ds chain hc⟩

/-- A faulty diagnostic checker accepting every chain. -/
def acceptsEveryChain (_U : ConstTypes) (_ds : List (ConstDecl _U))
    (_chain : List ConstId) : Bool := true

/-- Instance (law `ConcreteSpec.Core.cycle_chain_real`, case `two_constants`). -/
example : ∀ chain, CycleChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] chain →
    ∃ c rest, chain = c :: rest ++ [c] ∧ ActualRefChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] chain :=
  cycle_chain_real ConstDemo.U
    [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
     ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩]

/-- Planted (law `ConcreteSpec.Core.cycle_chain_real`, case `two_constants`,
subject `ConcreteSpec.Core.refChain`, mutant `ConcreteSpec.Core.acceptsEveryChain`). The mutant
accepts `A → A` in the same source, although the source has no self-reference. -/
example : ¬ (∀ chain,
    (∃ c rest, chain = c :: rest ++ [c] ∧ acceptsEveryChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] chain = true) →
    ∃ c rest, chain = c :: rest ++ [c] ∧ ActualRefChain ConstDemo.U
      [⟨ConstDemo.cid "A", .c .bool, .ref (ConstDemo.cid "B")⟩,
       ⟨ConstDemo.cid "B", .c .bool, .ref (ConstDemo.cid "A")⟩] chain) := by
  intro h
  obtain ⟨_, _, _, hc⟩ := h [ConstDemo.cid "A", ConstDemo.cid "A"]
    ⟨ConstDemo.cid "A", [], rfl, rfl⟩
  obtain ⟨d, hd, he, href⟩ := hc.1
  cases hd with
  | head => simp [CExpr.refs, ConstDemo.cid] at href
  | tail _ hd => cases hd with
    | head => exact (by decide : ConstDemo.cid "B" ≠ ConstDemo.cid "A") he
    | tail _ hm => cases hm

end ConcreteSpec.Core
