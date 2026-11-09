import ConcreteSpec.Spec.Scalar

/-!
# The laws of scalars and arithmetic (`Spec/Scalar.lean`, foundation contract C2)

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.

- **Representation**: a bit pattern is below `2^bits` (`toBits_lt`), every pattern denotes a value
  in range (`ofBits_inRange`), and a value's pattern denotes it again (`ofBits_toBits`).
- **Results stay in range**: wrapping, saturating and bitwise results (`wrap_inRange`,
  `saturate_inRange`), and every successful checked operation on values in range
  (`checked_inRange`).
- **One meaning, three failure policies**: a plain operation succeeds exactly when the checked one
  does, with the same result, and traps exactly with the kind of the checked error
  (`plain_ok_iff`, `plain_trap_iff`); wrapping and saturating agree with it where it succeeds
  (`wrapping_of_checked`, `saturating_of_checked`).
- **Conversions**: `Lossless` is exactly the value-domain rule (`lossless_iff`); `tryConvert`
  succeeds exactly in range and keeps the value (`tryConvert_ok_iff`).
- **Machine and exact arithmetic**: an exact term is defined whenever its operands are
  (`exact_defined`); independently typed operands and valid valuations establish width safety (`machine_operands`, `typed_eval_range`); where a
  plain machine term is defined, it equals the exact one (`machine_plain_eq_exact`), and so does a
  whole term read in exact arithmetic (`eval_toExact`).
- **Bits**: wrapping keeps the bit pattern (`toBits_wrap`, `toBits_wrapping`), a valid left shift
  drops the bits shifted out (`toBits_shl`), and an unsigned right shift is the logical shift of
  the pattern (`toBits_shr_unsigned`); a `bool` pattern is `0` or `1` (`boolOfBits_some_iff`).
- **Negation and lossy conversion**: plain negation traps exactly when checked negation fails
  (`plainNeg_ok_iff`, `plainNeg_trap_iff`; there is no wrapping negation, ARITH-4); a lossy
  conversion lands in range and keeps a value that fits (`lossyConvert_inRange`,
  `lossyConvert_of_inRange`); saturation keeps a value in range and clamps one outside to the
  nearer end (`saturate_of_inRange`, `saturate_below`, `saturate_above`); bitwise operations act on
  the patterns (`toBits_bitwise`, `toBits_bitNot`).
- The trap kinds have distinct names, and the arithmetic errors map to distinct kinds.
-/

namespace ConcreteSpec.Core

/-- Distinct inputs have distinct encodings. Kept as a named semantic property so its
universally quantified comparisons belong to the conclusion of the encoding laws. -/
def EncodingInjective {α β : Type} (f : α → β) : Prop := ∀ a b, f a = f b → a = b

/-- Law (subject `ConcreteSpec.Core.TrapKind.name`). -/
theorem TrapKind.name_injective : EncodingInjective TrapKind.name := by
  intro a b h
  cases a <;> cases b <;> first | rfl | (simp [TrapKind.name] at h)

/-- Law (subject `ConcreteSpec.Core.ArithmeticError.trapKind`). -/
theorem ArithmeticError.trapKind_injective :
    EncodingInjective ArithmeticError.trapKind := by
  intro a b h
  cases a <;> cases b <;> first | rfl | cases h

/-! ### Representation -/

/-- Law (subject `ConcreteSpec.Core.toBits`). -/
theorem toBits_lt (t : IntTy) (n : Int) : toBits t n < 2 ^ t.bits := by
  cases t <;> simp only [toBits, IntTy.bits] <;> omega

/-- Law (subject `ConcreteSpec.Core.ofBits`). -/
theorem ofBits_inRange (t : IntTy) (m : Nat) (h : m < 2 ^ t.bits) : t.InRange (ofBits t m) := by
  cases t <;> simp only [IntTy.bits] at h <;>
    simp [ofBits, IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] <;>
    (try split) <;> omega

/-- Law (subject `ConcreteSpec.Core.ofBits`). -/
theorem ofBits_toBits (t : IntTy) (n : Int) (h : t.InRange n) : ofBits t (toBits t n) = n := by
  cases t <;> simp [IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at h <;>
    simp [ofBits, toBits, IntTy.signed, IntTy.bits] <;> (try split) <;> omega

/-- Law (subject `ConcreteSpec.Core.wrap`). -/
theorem wrap_inRange (t : IntTy) (n : Int) : t.InRange (wrap t n) :=
  ofBits_inRange t _ (toBits_lt t n)

/-- Law (subject `ConcreteSpec.Core.wrap`). -/
theorem wrap_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : wrap t n = n :=
  ofBits_toBits t n h

/-- Helper (arithmetic). Finite endpoint ordering used by interval arguments (approved: APPR-C1-codex-5531d9e34). -/
theorem IntTy.min_le_max (t : IntTy) : t.min ≤ t.max := by
  cases t <;> decide

/-- Law (subject `ConcreteSpec.Core.saturate`). -/
theorem saturate_inRange (t : IntTy) (n : Int) : t.InRange (saturate t n) := by
  have := t.min_le_max
  unfold saturate IntTy.InRange
  split
  · omega
  · split <;> omega

/-- Law (subject `ConcreteSpec.Core.saturate`). Saturation keeps a value in range. -/
theorem saturate_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : saturate t n = n := by
  obtain ⟨h1, h2⟩ := h
  unfold saturate
  split
  · omega
  · split
    · omega
    · rfl

/-- Law (subject `ConcreteSpec.Core.bitwise`). -/
theorem bitwise_inRange (t : IntTy) (op : BitOp) (a b : Int) : t.InRange (bitwise t op a b) := by
  have ha := toBits_lt t a
  have hb := toBits_lt t b
  cases op
  · exact ofBits_inRange t _ (Nat.lt_of_le_of_lt Nat.and_le_left ha)
  · exact ofBits_inRange t _ (Nat.or_lt_two_pow ha hb)
  · exact ofBits_inRange t _ (Nat.xor_lt_two_pow ha hb)

/-- Law (subject `ConcreteSpec.Core.bitNot`). Extracts the explicit range validator result (approved: PENDING). -/
theorem bitNot_inRange (t : IntTy) (a : Int) : t.InRange (bitNot t a) := by
  have := toBits_lt t a
  have hp : 0 < 2 ^ t.bits := Nat.two_pow_pos _
  exact ofBits_inRange t _ (by omega)

/-! ### Checked operations stay in range -/

/-- Helper (inversion). Extracts the explicit range validator result (approved: APPR-C1-codex-5531d9e34). -/
theorem inRangeOr_inRange {t : IntTy} {r s : Int} (h : inRangeOr t r = .ok s) : t.InRange s := by
  unfold inRangeOr at h
  split at h
  · cases h; assumption
  · cases h

/-- Helper (arithmetic). Internal integer division bound used in checked shift preservation (approved: APPR-C1-codex-5531d9e34). -/
theorem shr_inRange (t : IntTy) (a : Int) (d : Nat) (ha : t.InRange a) :
    t.InRange (a / (2 : Int) ^ d) := by
  have hd : (0 : Int) < 2 ^ d := Int.pow_pos (by decide)
  have hmin : t.min ≤ 0 := by cases t <;> decide
  have hmax : 0 ≤ t.max := by cases t <;> decide
  obtain ⟨h1, h2⟩ := ha
  constructor
  · by_cases ha0 : 0 ≤ a
    · exact Int.le_trans hmin (Int.ediv_nonneg ha0 (Int.le_of_lt hd))
    · exact Int.le_trans h1 (Int.le_ediv_of_mul_le hd (by
        have : 1 ≤ (2 : Int) ^ d := hd
        have hneg : a < 0 := Int.lt_of_not_ge ha0
        calc a * 2 ^ d ≤ a * 1 := Int.mul_le_mul_of_nonpos_left (Int.le_of_lt hneg) this
          _ = a := Int.mul_one a))
  · by_cases ha0 : 0 ≤ a
    · exact Int.le_trans (Int.ediv_le_self _ ha0) h2
    · exact Int.le_trans (Int.le_of_lt (Int.ediv_neg_of_neg_of_pos (Int.lt_of_not_ge ha0) hd)) hmax

/-- Helper (arithmetic). Internal remainder bound used in checked arithmetic preservation (approved: APPR-C1-codex-5531d9e34). -/
theorem tmod_inRange (t : IntTy) (a b : Int) (ha : t.InRange a) : t.InRange (a.tmod b) := by
  have hmin : t.min ≤ 0 := by cases t <;> decide
  have hmax : 0 ≤ t.max := by cases t <;> decide
  obtain ⟨h1, h2⟩ := ha
  have hab : (a.tmod b).natAbs ≤ a.natAbs := by rw [Int.natAbs_tmod]; exact Nat.mod_le _ _
  by_cases ha0 : 0 ≤ a
  · have hnn : 0 ≤ a.tmod b := Int.tmod_nonneg b ha0
    constructor <;> omega
  · have hnp : 0 ≤ (-a).tmod b := Int.tmod_nonneg b (by omega)
    rw [Int.neg_tmod] at hnp
    constructor <;> omega

/-- Helper (inversion). Checked result bound combining validator inversion and the independent division and remainder bounds (approved: APPR-C1-codex-5531d9e34). -/
theorem checked_inRange (t : IntTy) (op : ArithOp) (a b r : Int) (ha : t.InRange a)
    (h : checked t op a b = .ok r) : t.InRange r := by
  cases op <;> simp only [checked] at h
  · exact inRangeOr_inRange h
  · exact inRangeOr_inRange h
  · exact inRangeOr_inRange h
  · split at h
    · cases h
    · exact inRangeOr_inRange h
  · split at h
    · cases h
    · split at h
      · cases h
      · cases h; exact tmod_inRange t a b ha
  · split at h
    · cases h; exact wrap_inRange t _
    · cases h
  · split at h
    · cases h; exact shr_inRange t a _ ha
    · cases h

/-! ### Plain, wrapping and saturating -/

/-- Representation lemma. Plain/checked adapter representation (approved: APPR-C1-codex-5531d9e34). -/
theorem plain_ok_iff (t : IntTy) (op : ArithOp) (a b r : Int) :
    plain t op a b = .ok r ↔ checked t op a b = .ok r := by
  unfold plain
  cases checked t op a b <;> simp

/-- Representation lemma. Plain/checked error adapter representation (approved: APPR-C1-codex-5531d9e34). -/
theorem plain_trap_iff (t : IntTy) (op : ArithOp) (a b : Int) (k : TrapKind) :
    plain t op a b = .trap k ↔ ∃ e, checked t op a b = .error e ∧ e.trapKind = k := by
  unfold plain
  cases checked t op a b <;> simp

/-- Helper (inversion). Adapter result-shape inversion (approved: APPR-C1-codex-5531d9e34). -/
theorem plain_not_stuck (t : IntTy) (op : ArithOp) (a b : Int) : plain t op a b ≠ .stuck := by
  unfold plain
  cases checked t op a b <;> simp

/-- The arithmetic operation of a wrapping-capable operation. -/
def WrapOp.arith : WrapOp → ArithOp
  | .add => .add
  | .sub => .sub
  | .mul => .mul

/-- Helper (computation). Unfolds the checked arithmetic adapter (approved: APPR-C1-codex-5531d9e34). -/
theorem checked_wrapOp (t : IntTy) (op : WrapOp) (a b : Int) :
    checked t op.arith a b = inRangeOr t (op.exact a b) := by
  cases op <;> rfl

/-- Law (subject `ConcreteSpec.Core.wrapping`). -/
theorem wrapping_of_checked (t : IntTy) (op : WrapOp) (a b r : Int)
    (h : checked t op.arith a b = .ok r) : wrapping t op a b = r := by
  rw [checked_wrapOp] at h
  unfold inRangeOr at h
  split at h
  · cases h; exact wrap_of_inRange t _ (by assumption)
  · cases h

/-- Law (subject `ConcreteSpec.Core.saturating`). Interval representation of the lossless relation (approved: PENDING). -/
theorem saturating_of_checked (t : IntTy) (op : WrapOp) (a b r : Int)
    (h : checked t op.arith a b = .ok r) : saturating t op a b = r := by
  rw [checked_wrapOp] at h
  unfold inRangeOr at h
  split at h
  · cases h
    exact saturate_of_inRange t _ (by assumption)
  · cases h

/-! ### Conversions -/

/-- Law (subject `ConcreteSpec.Core.Lossless`). Source-interval inclusion agrees with
preservation of every source value. -/
theorem lossless_iff (s t : IntTy) : Lossless s t ↔ ∀ n, s.InRange n → t.InRange n := by
  constructor
  · intro ⟨h1, h2⟩ n ⟨hn1, hn2⟩
    exact ⟨Int.le_trans h1 hn1, Int.le_trans hn2 h2⟩
  · intro h
    have hm := s.min_le_max
    exact ⟨(h s.min ⟨Int.le_refl _, hm⟩).1, (h s.max ⟨hm, Int.le_refl _⟩).2⟩

/-- Law (subject `ConcreteSpec.Core.boolToInt`). `bool` converts losslessly to every integer type. -/
theorem boolToInt_inRange (t : IntTy) (b : Bool) : t.InRange (boolToInt b) := by
  cases t <;> cases b <;> decide

/-- Representation lemma. Explicit conversion validator characterization (approved: APPR-C1-codex-5531d9e34). -/
theorem tryConvert_ok_iff (t : IntTy) (n r : Int) :
    tryConvert t n = .ok r ↔ t.InRange n ∧ r = n := by
  unfold tryConvert
  split
  · simp_all [eq_comm]
  · simp_all

/-! ### Machine and exact arithmetic in contracts -/

/-- Helper (computation). Unfolds exact-term evaluation after successful operands (approved: APPR-C1-codex-5531d9e34). -/
theorem exact_defined (ρ : Var → Option Int) (op : WrapOp) (a b : CTerm) (x y : Int)
    (ha : a.eval ρ = some x) (hb : b.eval ρ = some y) :
    (CTerm.exact op a b).eval ρ = some (op.exact x y) := by
  simp [CTerm.eval, ha, hb]

/-- Law (subject `ConcreteSpec.Core.CTerm.eval`). Where a plain machine term is defined, it means the exact operation: the two differ only in
where the machine one is undefined. -/
theorem machine_plain_eq_exact (ρ : Var → Option Int) (t : IntTy) (op : WrapOp) (a b : CTerm)
    (r : Int) (h : (CTerm.machine t (.plain op.arith) a b).eval ρ = some r) :
    (CTerm.exact op a b).eval ρ = some r := by
  simp only [CTerm.eval] at h ⊢
  cases ha : a.eval ρ with
  | none => rw [ha] at h; cases h
  | some x =>
    cases hb : b.eval ρ with
    | none => rw [ha, hb] at h; simp at h
    | some y =>
      rw [ha, hb] at h
      simp only [Option.bind_some] at h
      split at h
      · cases h
      · rw [checked_wrapOp] at h
        unfold inRangeOr at h
        by_cases hr : t.InRange (op.exact x y)
        · simp [hr] at h; subst h; rfl
        · simp [hr] at h

/-- Helper (inversion). Extracts the runtime operand guard; not independent typing soundness (approved: APPR-C1-codex-5531d9e34). -/
theorem machine_operands_inv (ρ : Var → Option Int) (t : IntTy) (op : MachineOp) (a b : CTerm)
    (r : Int) (h : (CTerm.machine t op a b).eval ρ = some r) :
    ∃ x y, a.eval ρ = some x ∧ b.eval ρ = some y ∧ t.InRange x ∧
      (op.countOperand = true ∨ t.InRange y) := by
  simp only [CTerm.eval] at h
  cases ha : a.eval ρ with
  | none => rw [ha] at h; cases h
  | some x =>
    cases hb : b.eval ρ with
    | none => rw [ha, hb] at h; simp at h
    | some y =>
      rw [ha, hb] at h
      simp only [Option.bind_some] at h
      split at h
      · cases h
      · rename_i hok
        simp only [MachineOp.operandsOk, Bool.not_eq_false, Bool.and_eq_true, decide_eq_true_eq,
          Bool.or_eq_true] at hok
        exact ⟨x, y, rfl, rfl, hok.1, hok.2⟩

/-- Helper (inversion). Combines evaluator guard inversion with checked arithmetic bounds; independent soundness is typed_eval_range (approved: APPR-C1-codex-5531d9e34). -/
theorem machine_inRange_inv (ρ : Var → Option Int) (t : IntTy) (op : MachineOp) (a b : CTerm)
    (r : Int) (h : (CTerm.machine t op a b).eval ρ = some r) : t.InRange r := by
  obtain ⟨x, y, ha, hb, hx, _⟩ := machine_operands_inv ρ t op a b r h
  simp only [CTerm.eval, ha, hb, Option.bind_some] at h
  split at h
  · cases h
  · cases op with
    | plain o =>
      simp only at h
      split at h
      · cases h; rename_i hc; exact checked_inRange t o x y r hx hc
      · cases h
    | wrapping o => cases h; exact wrap_inRange t _
    | saturating o => cases h; exact saturate_inRange t _

/-- Helper (inversion). Decodes the machine-operation representation (approved: APPR-C1-codex-5531d9e34). -/
theorem exactOf?_some {op : MachineOp} {w : WrapOp} (h : op.exactOf? = some w) :
    op = .plain w.arith := by
  cases op with
  | plain o => cases o <;> cases h <;> rfl
  | wrapping _ => cases h
  | saturating _ => cases h

/-- Law (subject `ConcreteSpec.Core.CTerm.toExact`). **Reading in exact arithmetic.** Where a contract term is defined, its exact reading is
defined and has the same value. -/
theorem eval_toExact (ρ : Var → Option Int) : ∀ (e : CTerm) (r : Int),
    e.eval ρ = some r → e.toExact.eval ρ = some r
  | .lit _, _, h => h
  | .var _, _, h => h
  | .machine t op a b, r, h => by
    obtain ⟨x, y, ha, hb, _, _⟩ := machine_operands_inv ρ t op a b r h
    have ha' := eval_toExact ρ a x ha
    have hb' := eval_toExact ρ b y hb
    simp only [CTerm.toExact]
    split
    · rename_i w hw
      rw [exactOf?_some hw] at h
      have := machine_plain_eq_exact ρ t w a b r h
      simp only [CTerm.eval, ha, hb, Option.bind_some, Option.map_some, Option.some.injEq] at this
      simp [CTerm.eval, ha', hb', this]
    · simp only [CTerm.eval, ha, hb, Option.bind_some] at h
      simpa only [CTerm.eval, ha', hb', Option.bind_some] using h
  | .exact op a b, r, h => by
    simp only [CTerm.eval] at h
    cases ha : a.eval ρ with
    | none => rw [ha] at h; cases h
    | some x =>
      cases hb : b.eval ρ with
      | none => rw [ha, hb] at h; simp at h
      | some y =>
        rw [ha, hb] at h
        simp only [CTerm.toExact, CTerm.eval, eval_toExact ρ a x ha, eval_toExact ρ b y hb]
        simpa using h

/-! ### Independent width derivations

This is a sufficient proof judgment for contract arithmetic, not an extra run-time check or
a change to the language's typing rules. Exact arithmetic uses an independently proved bound
on the mathematical operation. Shift counts may have a different width. -/

/-- An independent derivation of the width of a contract term. -/
inductive CTerm.HasWidth (Γ : Var → Option IntTy) : CTerm → IntTy → Prop where
  | lit {t n} : t.InRange n → HasWidth Γ (.lit n) t
  | var {t x} : Γ x = some t → HasWidth Γ (.var x) t
  | machine {t u op a b} : HasWidth Γ a t → HasWidth Γ b u →
      (op.countOperand = true ∨ u = t) → HasWidth Γ (.machine t op a b) t
  | exact {t u v op a b} : HasWidth Γ a u → HasWidth Γ b v →
      (∀ x y, u.InRange x → v.InRange y → t.InRange (op.exact x y)) →
      HasWidth Γ (.exact op a b) t

/-- A valuation respects the independently assigned variable widths. -/
def WidthEnv (Γ : Var → Option IntTy) (ρ : Var → Option Int) : Prop :=
  ∀ x t n, Γ x = some t → ρ x = some n → t.InRange n

/-- Every successful evaluation of a term belongs to its independently derived width. -/
def WidthSound (eval : (Var → Option Int) → CTerm → Option Int)
    (ρ : Var → Option Int) (e : CTerm) (t : IntTy) : Prop :=
  ∀ r, eval ρ e = some r → t.InRange r

/-- Operand width preservation, with the count operand of a shift treated separately. -/
def OperandWidths (eval : (Var → Option Int) → CTerm → Option Int)
    (ρ : Var → Option Int) (t : IntTy) (op : MachineOp) (a b : CTerm) : Prop :=
  ∀ x y, eval ρ a = some x → eval ρ b = some y →
    t.InRange x ∧ (op.countOperand = true ∨ t.InRange y)

/-- Law (subject `ConcreteSpec.Core.CTerm.eval`). Independent width derivations and a valid
valuation bound every successful denotation. Operand bounds come from subderivations, never
from extracting the evaluator's operand guard. -/
theorem typed_eval_range (Γ : Var → Option IntTy) (ρ : Var → Option Int) (e : CTerm) (t : IntTy)
    (ht : e.HasWidth Γ t) (hρ : WidthEnv Γ ρ) :
    WidthSound CTerm.eval ρ e t := by
  induction ht with
  | lit hn =>
    intro r hr
    cases hr
    exact hn
  | var hx =>
    intro r hr
    exact hρ _ _ _ hx hr
  | @machine t u op a b ha hb hc iha ihb =>
    intro r hr
    cases hx : a.eval ρ with
    | none => simp [CTerm.eval, hx] at hr
    | some x =>
      cases hy : b.eval ρ with
      | none => simp [CTerm.eval, hx, hy] at hr
      | some y =>
        have hxt := iha x hx
        have hyt := ihb y hy
        simp only [CTerm.eval, hx, hy, Option.bind_some] at hr
        split at hr
        · cases hr
        · cases op with
          | plain o =>
            simp only at hr
            split at hr
            · cases hr; rename_i hcheck; exact checked_inRange t o x y r hxt hcheck
            · cases hr
          | wrapping o => cases hr; exact wrap_inRange t _
          | saturating o => cases hr; exact saturate_inRange t _
  | @exact t u v op a b ha hb hbound iha ihb =>
    intro r hr
    cases hx : a.eval ρ with
    | none => simp [CTerm.eval, hx] at hr
    | some x =>
      cases hy : b.eval ρ with
      | none => simp [CTerm.eval, hx, hy] at hr
      | some y =>
        simp only [CTerm.eval, hx, hy, Option.bind_some, Option.map_some, Option.some.injEq] at hr
        subst r
        exact hbound x y (iha x hx) (ihb y hy)

/-- Law (subject `ConcreteSpec.Core.CTerm.eval`). Independently typed operands have the
operation's width, except that shifts have their separately typed count. -/
theorem machine_operands (Γ : Var → Option IntTy) (ρ : Var → Option Int) (t u : IntTy)
    (op : MachineOp) (a b : CTerm) (ha : a.HasWidth Γ t) (hb : b.HasWidth Γ u)
    (hc : op.countOperand = true ∨ u = t) (hρ : WidthEnv Γ ρ) :
    OperandWidths CTerm.eval ρ t op a b := by
  intro x y hx hy
  have hxt := typed_eval_range Γ ρ a t ha hρ x hx
  have hyu := typed_eval_range Γ ρ b u hb hρ y hy
  cases hc with
  | inl hc => exact ⟨hxt, .inl hc⟩
  | inr hc => subst u; exact ⟨hxt, .inr hyu⟩

/-- Law (subject `ConcreteSpec.Core.CTerm.eval`). A machine expression with independently
derived operand widths preserves its output width on every successful evaluation. -/
theorem machine_inRange (Γ : Var → Option IntTy) (ρ : Var → Option Int) (t u : IntTy)
    (op : MachineOp) (a b : CTerm) (ha : a.HasWidth Γ t) (hb : b.HasWidth Γ u)
    (hc : op.countOperand = true ∨ u = t) (hρ : WidthEnv Γ ρ) :
    WidthSound CTerm.eval ρ (.machine t op a b) t := by
  intro r hr
  exact typed_eval_range Γ ρ (.machine t op a b) t (.machine ha hb hc) hρ r hr

namespace ScalarEvidence
/-- A mutant valuation semantics that invents an out-of-width value for every term. -/
def wideEval (_ρ : Var → Option Int) (_e : CTerm) : Option Int := some 300

/-- Helper (inversion). The closed empty width environment has no assigned variables
(approved: APPR-C1-codex-5531d9e34). -/
private theorem empty_width_env : WidthEnv (fun _ => none) (fun _ => none) := by
  intro x t n hx
  cases hx

/-- Instance (law `ConcreteSpec.Core.typed_eval_range`, case `literal`). -/
example : ∀ r, (CTerm.lit 7).eval (fun _ => none) = some r → IntTy.i8.InRange r :=
  typed_eval_range (fun _ => none) (fun _ => none) (.lit 7) .i8 (.lit (by decide)) empty_width_env

/-- Planted (law `ConcreteSpec.Core.typed_eval_range`, case `literal`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
example : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ r, wideEval (fun _ => none) (.lit 7) = some r → IntTy.i8.InRange r) := by
  refine ⟨.lit (by decide), empty_width_env, fun h => ?_⟩
  have hh := h 300 rfl
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.machine_operands`, case `typed_pair`). -/
example : ∀ x y, (CTerm.lit 7).eval (fun _ => none) = some x →
    (CTerm.lit 8).eval (fun _ => none) = some y →
    IntTy.i8.InRange x ∧ ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8.InRange y) :=
  machine_operands (fun _ => none) (fun _ => none) .i8 .i8 (.plain .add) (.lit 7) (.lit 8)
    (.lit (by decide)) (.lit (by decide)) (.inr rfl) empty_width_env

/-- Planted (law `ConcreteSpec.Core.machine_operands`, case `typed_pair`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
example : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧ (CTerm.lit 8).HasWidth (fun _ => none) .i8 ∧
    ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8 = .i8) ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ x y, wideEval (fun _ => none) (.lit 7) = some x → wideEval (fun _ => none) (.lit 8) = some y →
      IntTy.i8.InRange x ∧ ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8.InRange y)) := by
  refine ⟨.lit (by decide), .lit (by decide), .inr rfl, empty_width_env, fun h => ?_⟩
  have hh := (h 300 300 rfl rfl).1
  revert hh
  decide
/-- Instance (law `ConcreteSpec.Core.machine_inRange`, case `typed_machine`). -/
example : ∀ r, (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).eval (fun _ => none) = some r →
    IntTy.i8.InRange r :=
  machine_inRange (fun _ => none) (fun _ => none) .i8 .i8 (.plain .add) (.lit 7) (.lit 8)
    (.lit (by decide)) (.lit (by decide)) (.inr rfl) empty_width_env

/-- Planted (law `ConcreteSpec.Core.machine_inRange`, case `typed_machine`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
example : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧ (CTerm.lit 8).HasWidth (fun _ => none) .i8 ∧
    ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8 = .i8) ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ r, wideEval (fun _ => none) (.machine .i8 (.plain .add) (.lit 7) (.lit 8)) = some r →
      IntTy.i8.InRange r) := by
  refine ⟨.lit (by decide), .lit (by decide), .inr rfl, empty_width_env, fun h => ?_⟩
  have hh := h 300 rfl
  revert hh
  decide

end ScalarEvidence

/-! ### Negation, `bool` patterns, lossy conversion -/

/-- Representation lemma. Negation success adapter representation (approved: APPR-C1-codex-5531d9e34). -/
theorem plainNeg_ok_iff (t : IntTy) (a r : Int) : plainNeg t a = .ok r ↔ checkedNeg t a = .ok r := by
  unfold plainNeg
  cases checkedNeg t a <;> simp

/-- Representation lemma. Negation error adapter representation (approved: APPR-C1-codex-5531d9e34). -/
theorem plainNeg_trap_iff (t : IntTy) (a : Int) (k : TrapKind) :
    plainNeg t a = .trap k ↔ ∃ e, checkedNeg t a = .error e ∧ e.trapKind = k := by
  unfold plainNeg
  cases checkedNeg t a <;> simp

/-- Representation lemma. Extracts the checked-negation range validator (approved: APPR-C1-codex-5531d9e34). -/
theorem checkedNeg_inRange {t : IntTy} {a r : Int} (h : checkedNeg t a = .ok r) : t.InRange r :=
  inRangeOr_inRange h

/-- Law (subject `ConcreteSpec.Core.boolOfBits`). -/
theorem boolOfBits_boolToBits (b : Bool) : boolOfBits (boolToBits b) = some b := by
  cases b <;> rfl

/-- Representation lemma. Finite Boolean decoder characterization (approved: APPR-C1-codex-5531d9e34). -/
theorem boolOfBits_some_iff (m : Nat) (b : Bool) : boolOfBits m = some b ↔ m = boolToBits b := by
  match m with
  | 0 => cases b <;> simp [boolOfBits, boolToBits]
  | 1 => cases b <;> simp [boolOfBits, boolToBits]
  | m + 2 => cases b <;> simp [boolOfBits, boolToBits]

/-- Helper (computation). Alias consequence of wrap_inRange; no additional conversion semantics (approved: APPR-C1-codex-5531d9e34). -/
theorem lossyConvert_inRange (t : IntTy) (n : Int) : t.InRange (lossyConvert t n) := wrap_inRange t n

/-- Helper (computation). Alias consequence of wrap_of_inRange; no additional conversion semantics (approved: APPR-C1-codex-5531d9e34). -/
theorem lossyConvert_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : lossyConvert t n = n :=
  wrap_of_inRange t n h

/-! ### Bit-level correspondence -/

/-- Law (subject `ConcreteSpec.Core.ofBits`). -/
theorem toBits_ofBits (t : IntTy) (m : Nat) (h : m < 2 ^ t.bits) : toBits t (ofBits t m) = m := by
  cases t <;> simp only [IntTy.bits] at h <;>
    simp [toBits, ofBits, IntTy.signed, IntTy.bits] <;> (try split) <;> omega

/-- Law (subject `ConcreteSpec.Core.wrap`). Wrapping keeps the bit pattern: `wrap` only reads the low `IntTy.bits` bits. -/
theorem toBits_wrap (t : IntTy) (n : Int) : toBits t (wrap t n) = toBits t n :=
  toBits_ofBits t _ (toBits_lt t n)

/-- Law (subject `ConcreteSpec.Core.wrapping`). A wrapping operation's pattern is the exact result's pattern, modulo 2^bits. -/
theorem toBits_wrapping (t : IntTy) (op : WrapOp) (a b : Int) :
    toBits t (wrapping t op a b) = toBits t (op.exact a b) := toBits_wrap t _

/-- Law (subject `ConcreteSpec.Core.toBits`). A valid left shift's pattern is the pattern of a · 2^c: the bits shifted out are gone. -/
theorem toBits_shl (t : IntTy) (a c r : Int) (h : checked t .shl a c = .ok r) :
    toBits t r = toBits t (a * (2 : Int) ^ c.toNat) := by
  simp only [checked] at h
  split at h
  · cases h; exact toBits_wrap t _
  · cases h

/-- Law (subject `ConcreteSpec.Core.toBits`). An unsigned right shift is the logical shift of the pattern. -/
theorem toBits_shr_unsigned (t : IntTy) (hs : t.signed = false) (a c r : Int) (ha : t.InRange a)
    (h : checked t .shr a c = .ok r) : toBits t r = toBits t a >>> c.toNat := by
  simp only [checked] at h
  split at h
  · cases h
    have ha0 : 0 ≤ a := by
      have := ha.1; simp [IntTy.min, hs] at this; exact this
    have hr := shr_inRange t a c.toNat ha
    have hr0 : 0 ≤ a / (2 : Int) ^ c.toNat := Int.ediv_nonneg ha0 (Int.le_of_lt (Int.pow_pos (by decide)))
    have hmax : ∀ n : Int, 0 ≤ n → t.InRange n → toBits t n = n.toNat := by
      intro n hn hin
      have := toBits_lt t n
      have hofb := ofBits_toBits t n hin
      simp only [ofBits, hs, Bool.false_eq_true, false_and, ↓reduceIte] at hofb
      omega
    rw [hmax _ hr0 hr, hmax a ha0 ha, Nat.shiftRight_eq_div_pow]
    obtain ⟨m, rfl⟩ := Int.eq_ofNat_of_zero_le ha0
    have hq : ((m : Int) / (2 : Int) ^ c.toNat) = ((m / 2 ^ c.toNat : Nat) : Int) := by
      rw [Int.natCast_ediv, Int.natCast_pow]; rfl
    rw [hq, Int.toNat_natCast, Int.toNat_natCast]
  · cases h

/-- Law (subject `ConcreteSpec.Core.bitwise`). Bitwise operations act on the patterns, bit by bit. -/
theorem toBits_bitwise (t : IntTy) (op : BitOp) (a b : Int) :
    toBits t (bitwise t op a b) = match op with
      | .and => toBits t a &&& toBits t b
      | .or => toBits t a ||| toBits t b
      | .xor => toBits t a ^^^ toBits t b := by
  have ha := toBits_lt t a
  have hb := toBits_lt t b
  cases op
  · exact toBits_ofBits t _ (Nat.lt_of_le_of_lt Nat.and_le_left ha)
  · exact toBits_ofBits t _ (Nat.or_lt_two_pow ha hb)
  · exact toBits_ofBits t _ (Nat.xor_lt_two_pow ha hb)

/-- Law (subject `ConcreteSpec.Core.bitNot`). Bitwise not flips every bit of the pattern. -/
theorem toBits_bitNot (t : IntTy) (a : Int) :
    toBits t (bitNot t a) = 2 ^ t.bits - 1 - toBits t a := by
  have := toBits_lt t a
  have hp : 0 < 2 ^ t.bits := Nat.two_pow_pos _
  exact toBits_ofBits t _ (by omega)

/-- Law (subject `ConcreteSpec.Core.saturate`). Saturation clamps: below the range it gives the minimum, above it the maximum. -/
theorem saturate_below (t : IntTy) (n : Int) (h : n < t.min) : saturate t n = t.min := by
  simp [saturate, h]

/-- Law (subject `ConcreteSpec.Core.saturate`). -/
theorem saturate_above (t : IntTy) (n : Int) (h : t.max < n) : saturate t n = t.max := by
  have := t.min_le_max
  have : ¬ n < t.min := by omega
  simp [saturate, h, this]

namespace ScalarLawEvidence
/-- Mutant encoders collapse distinct failures. -/
def sameName (_ : TrapKind) : String := "same"
def sameTrap (_ : ArithmeticError) : TrapKind := .abort
/-- Mutant bit encodings and arithmetic results. -/
def hugeBits (_ : IntTy) (_ : Int) : Nat := 999
def wideOfBits (_ : IntTy) (_ : Nat) : Int := 300
def zeroOfBits (_ : IntTy) (_ : Nat) : Int := 0
def wideUnary (_ : IntTy) (_ : Int) : Int := 300
def zeroUnary (_ : IntTy) (_ : Int) : Int := 0
def increment (_ : IntTy) (n : Int) : Int := n + 1
def wideBitwise (_ : IntTy) (_ : BitOp) (_ _ : Int) : Int := 300
def zeroBitwise (_ : IntTy) (_ : BitOp) (_ _ : Int) : Int := 0
def zeroPolicy (_ : IntTy) (_ : WrapOp) (_ _ : Int) : Int := 0
def wideBool (_ : Bool) : Int := 300
def noBool (_ : Nat) : Option Bool := none
/-- This mutant leaves machine evaluation intact but corrupts exact evaluation. -/
def corruptExact (ρ : Var → Option Int) (e : CTerm) : Option Int :=
  match e with
  | .exact .. => some 0
  | _ => e.eval ρ
def eraseExact (_ : CTerm) : CTerm := .lit 0
/-- Forgetting low-bit truncation breaks a signed left shift. -/
def unboundedBits (_ : IntTy) (n : Int) : Nat := n.toNat
/-- An encoder that shifts the bit pattern by one breaks logical right shift. -/
def offsetBits (t : IntTy) (n : Int) : Nat := toBits t n + 1

/-- Instance (law `ConcreteSpec.Core.TrapKind.name_injective`, case `concrete`). -/
example : ∀ (a b : TrapKind), a.name = b.name → a = b :=
  TrapKind.name_injective

/-- Planted (law `ConcreteSpec.Core.TrapKind.name_injective`, case `concrete`, subject `ConcreteSpec.Core.TrapKind.name`, mutant `ConcreteSpec.Core.ScalarLawEvidence.sameName`). -/
example : ¬ (∀ (a b : TrapKind), sameName a = sameName b → a = b) := by
  intro h
  have hh := h .overflow .abort rfl
  cases hh

/-- Instance (law `ConcreteSpec.Core.ArithmeticError.trapKind_injective`, case `concrete`). -/
example : ∀ (a b : ArithmeticError), a.trapKind = b.trapKind → a = b :=
  ArithmeticError.trapKind_injective

/-- Planted (law `ConcreteSpec.Core.ArithmeticError.trapKind_injective`, case `concrete`, subject `ConcreteSpec.Core.ArithmeticError.trapKind`, mutant `ConcreteSpec.Core.ScalarLawEvidence.sameTrap`). -/
example : ¬ (∀ (a b : ArithmeticError), sameTrap a = sameTrap b → a = b) := by
  intro h
  have hh := h .overflow .divisionByZero rfl
  cases hh

/-- Instance (law `ConcreteSpec.Core.toBits_lt`, case `concrete`). -/
example : toBits .i8 7 < 2 ^ IntTy.i8.bits :=
  toBits_lt .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_lt`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.hugeBits`). -/
example : ¬ (hugeBits .i8 7 < 2 ^ IntTy.i8.bits) := by decide

/-- Instance (law `ConcreteSpec.Core.ofBits_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (ofBits .i8 7) :=
  ofBits_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.ofBits_inRange`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideOfBits`). -/
example : 7 < 2 ^ IntTy.i8.bits ∧
    ¬ (IntTy.i8.InRange (wideOfBits .i8 7)) := by decide

/-- Instance (law `ConcreteSpec.Core.ofBits_toBits`, case `concrete`). -/
example : ofBits .i8 (toBits .i8 7) = 7 :=
  ofBits_toBits .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.ofBits_toBits`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroOfBits`). -/
example : IntTy.i8.InRange 7 ∧
    ¬ (zeroOfBits .i8 (toBits .i8 7) = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.wrap_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (wrap .i8 300) :=
  wrap_inRange .i8 300

/-- Planted (law `ConcreteSpec.Core.wrap_inRange`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideUnary`). -/
example : ¬ (IntTy.i8.InRange (wideUnary .i8 300)) := by decide

/-- Instance (law `ConcreteSpec.Core.wrap_of_inRange`, case `concrete`). -/
example : wrap .i8 7 = 7 :=
  wrap_of_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.wrap_of_inRange`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroUnary`). -/
example : IntTy.i8.InRange 7 ∧
    ¬ (zeroUnary .i8 7 = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (saturate .i8 300) :=
  saturate_inRange .i8 300

/-- Planted (law `ConcreteSpec.Core.saturate_inRange`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideUnary`). -/
example : ¬ (IntTy.i8.InRange (wideUnary .i8 300)) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_of_inRange`, case `concrete`). -/
example : saturate .i8 7 = 7 :=
  saturate_of_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_of_inRange`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroUnary`). -/
example : IntTy.i8.InRange 7 ∧
    ¬ (zeroUnary .i8 7 = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.bitwise_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (bitwise .i8 .and 7 3) :=
  bitwise_inRange .i8 .and 7 3

/-- Planted (law `ConcreteSpec.Core.bitwise_inRange`, case `concrete`, subject `ConcreteSpec.Core.bitwise`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideBitwise`). -/
example : ¬ (IntTy.i8.InRange (wideBitwise .i8 .and 7 3)) := by decide

/-- Instance (law `ConcreteSpec.Core.bitNot_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (bitNot .i8 7) :=
  bitNot_inRange .i8 7

/-- Planted (law `ConcreteSpec.Core.bitNot_inRange`, case `concrete`, subject `ConcreteSpec.Core.bitNot`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideUnary`). -/
example : ¬ (IntTy.i8.InRange (wideUnary .i8 7)) := by decide

/-- Instance (law `ConcreteSpec.Core.wrapping_of_checked`, case `concrete`). -/
example : wrapping .i8 .add 7 8 = 15 :=
  wrapping_of_checked .i8 .add 7 8 15 rfl

/-- Planted (law `ConcreteSpec.Core.wrapping_of_checked`, case `concrete`, subject `ConcreteSpec.Core.wrapping`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroPolicy`). -/
example : checked .i8 WrapOp.add.arith 7 8 = .ok 15 ∧
    ¬ (zeroPolicy .i8 .add 7 8 = 15) := by decide

/-- Instance (law `ConcreteSpec.Core.saturating_of_checked`, case `concrete`). -/
example : saturating .i8 .add 7 8 = 15 :=
  saturating_of_checked .i8 .add 7 8 15 rfl

/-- Planted (law `ConcreteSpec.Core.saturating_of_checked`, case `concrete`, subject `ConcreteSpec.Core.saturating`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroPolicy`). -/
example : checked .i8 WrapOp.add.arith 7 8 = .ok 15 ∧
    ¬ (zeroPolicy .i8 .add 7 8 = 15) := by decide

/-- Instance (law `ConcreteSpec.Core.boolToInt_inRange`, case `concrete`). -/
example : IntTy.i8.InRange (boolToInt true) :=
  boolToInt_inRange .i8 true

/-- Planted (law `ConcreteSpec.Core.boolToInt_inRange`, case `concrete`, subject `ConcreteSpec.Core.boolToInt`, mutant `ConcreteSpec.Core.ScalarLawEvidence.wideBool`). -/
example : ¬ (IntTy.i8.InRange (wideBool true)) := by decide

/-- Instance (law `ConcreteSpec.Core.boolOfBits_boolToBits`, case `concrete`). -/
example : boolOfBits (boolToBits true) = some true :=
  boolOfBits_boolToBits true

/-- Planted (law `ConcreteSpec.Core.boolOfBits_boolToBits`, case `concrete`, subject `ConcreteSpec.Core.boolOfBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.noBool`). -/
example : ¬ (noBool (boolToBits true) = some true) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_ofBits`, case `concrete`). -/
example : toBits .i8 (ofBits .i8 7) = 7 :=
  toBits_ofBits .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.toBits_ofBits`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroOfBits`). -/
example : 7 < 2 ^ IntTy.i8.bits ∧
    ¬ (toBits .i8 (zeroOfBits .i8 7) = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_wrap`, case `concrete`). -/
example : toBits .i8 (wrap .i8 7) = toBits .i8 7 :=
  toBits_wrap .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_wrap`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.increment`). -/
example : ¬ (toBits .i8 (increment .i8 7) = toBits .i8 7) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_wrapping`, case `concrete`). -/
example : toBits .i8 (wrapping .i8 .add 7 8) = toBits .i8 (WrapOp.add.exact 7 8) :=
  toBits_wrapping .i8 .add 7 8

/-- Planted (law `ConcreteSpec.Core.toBits_wrapping`, case `concrete`, subject `ConcreteSpec.Core.wrapping`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroPolicy`). -/
example : ¬ (toBits .i8 (zeroPolicy .i8 .add 7 8) = toBits .i8 (WrapOp.add.exact 7 8)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_shl`, case `concrete`). -/
example : toBits .i8 (-56) = toBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat) :=
  toBits_shl .i8 100 1 (-56) rfl

/-- Planted (law `ConcreteSpec.Core.toBits_shl`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unboundedBits`). -/
example :
    (if validShift .i8 1 then .ok (ofBits .i8 (unboundedBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat)))
      else (.error .invalidShift : Except ArithmeticError Int)) = .ok (-56) ∧
    ¬ (unboundedBits .i8 (-56) = unboundedBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_shr_unsigned`, case `concrete`). -/
example : toBits .u8 4 = toBits .u8 8 >>> (1 : Int).toNat :=
  toBits_shr_unsigned .u8 rfl 8 1 4 (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.toBits_shr_unsigned`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.offsetBits`). -/
example : IntTy.u8.signed = false ∧ IntTy.u8.InRange 8 ∧ checked .u8 .shr 8 1 = .ok 4 ∧
    ¬ (offsetBits .u8 4 = offsetBits .u8 8 >>> (1 : Int).toNat) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_bitwise`, case `concrete`). -/
example : toBits .i8 (bitwise .i8 .and 7 3) = (match BitOp.and with | .and => toBits .i8 7 &&& toBits .i8 3 | .or => toBits .i8 7 ||| toBits .i8 3 | .xor => toBits .i8 7 ^^^ toBits .i8 3) :=
  toBits_bitwise .i8 .and 7 3

/-- Planted (law `ConcreteSpec.Core.toBits_bitwise`, case `concrete`, subject `ConcreteSpec.Core.bitwise`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroBitwise`). -/
example : ¬ (toBits .i8 (zeroBitwise .i8 .and 7 3) = (match BitOp.and with | .and => toBits .i8 7 &&& toBits .i8 3 | .or => toBits .i8 7 ||| toBits .i8 3 | .xor => toBits .i8 7 ^^^ toBits .i8 3)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_bitNot`, case `concrete`). -/
example : toBits .i8 (bitNot .i8 7) = 2 ^ IntTy.i8.bits - 1 - toBits .i8 7 :=
  toBits_bitNot .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_bitNot`, case `concrete`, subject `ConcreteSpec.Core.bitNot`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroUnary`). -/
example : ¬ (toBits .i8 (zeroUnary .i8 7) = 2 ^ IntTy.i8.bits - 1 - toBits .i8 7) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_below`, case `concrete`). -/
example : saturate .i8 (-300) = IntTy.i8.min :=
  saturate_below .i8 (-300) (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_below`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroUnary`). -/
example : (-300 : Int) < IntTy.i8.min ∧
    ¬ (zeroUnary .i8 (-300) = IntTy.i8.min) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_above`, case `concrete`). -/
example : saturate .i8 300 = IntTy.i8.max :=
  saturate_above .i8 300 (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_above`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.zeroUnary`). -/
example : IntTy.i8.max < 300 ∧
    ¬ (zeroUnary .i8 300 = IntTy.i8.max) := by decide

/-- Instance (law `ConcreteSpec.Core.machine_plain_eq_exact`, case `concrete`). -/
example : (CTerm.exact .add (.lit 7) (.lit 8)).eval (fun _ => none) = some 15 :=
  machine_plain_eq_exact (fun _ => none) .i8 .add (.lit 7) (.lit 8) 15 rfl

/-- Planted (law `ConcreteSpec.Core.machine_plain_eq_exact`, case `concrete`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarLawEvidence.corruptExact`). -/
example : corruptExact (fun _ => none) (.machine .i8 (.plain WrapOp.add.arith) (.lit 7) (.lit 8)) = some 15 ∧
    ¬ (corruptExact (fun _ => none) (.exact .add (.lit 7) (.lit 8)) = some 15) := by decide

/-- Instance (law `ConcreteSpec.Core.eval_toExact`, case `concrete`). -/
example : (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).toExact.eval (fun _ => none) = some 15 :=
  eval_toExact (fun _ => none) (.machine .i8 (.plain .add) (.lit 7) (.lit 8)) 15 rfl

/-- Planted (law `ConcreteSpec.Core.eval_toExact`, case `concrete`, subject `ConcreteSpec.Core.CTerm.toExact`, mutant `ConcreteSpec.Core.ScalarLawEvidence.eraseExact`). -/
example : (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).eval (fun _ => none) = some 15 ∧
    ¬ ((eraseExact (.machine .i8 (.plain .add) (.lit 7) (.lit 8))).eval (fun _ => none) = some 15) := by decide

/-- Mutant interval admission discards even an identical source and target. -/
def noLossless (_ _ : IntTy) : Prop := False
/-- Instance (law `ConcreteSpec.Core.lossless_iff`, case `same_width`). -/
example : Lossless .i8 .i8 ↔ ∀ n, IntTy.i8.InRange n → IntTy.i8.InRange n :=
  lossless_iff .i8 .i8
/-- Planted (law `ConcreteSpec.Core.lossless_iff`, case `same_width`, subject `ConcreteSpec.Core.Lossless`, mutant `ConcreteSpec.Core.ScalarLawEvidence.noLossless`). -/
example : ¬ (noLossless .i8 .i8 ↔ ∀ n, IntTy.i8.InRange n → IntTy.i8.InRange n) := by
  intro h
  exact h.mpr (fun _ hn => hn)

end ScalarLawEvidence

end ConcreteSpec.Core
