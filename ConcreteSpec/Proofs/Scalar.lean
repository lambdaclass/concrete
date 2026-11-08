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
  (`exact_defined`); where a plain machine term is defined, it equals the exact one
  (`machine_plain_eq_exact`).
- The trap kinds have distinct names, and the arithmetic errors map to distinct kinds.
-/

namespace ConcreteSpec.Core

theorem TrapKind.name_injective {a b : TrapKind} (h : a.name = b.name) : a = b := by
  cases a <;> cases b <;> first | rfl | (simp [TrapKind.name] at h)

theorem ArithmeticError.trapKind_injective {a b : ArithmeticError} (h : a.trapKind = b.trapKind) :
    a = b := by
  cases a <;> cases b <;> first | rfl | cases h

/-! ### Representation -/

theorem toBits_lt (t : IntTy) (n : Int) : toBits t n < 2 ^ t.bits := by
  cases t <;> simp only [toBits, IntTy.bits] <;> omega

theorem ofBits_inRange (t : IntTy) (m : Nat) (h : m < 2 ^ t.bits) : t.InRange (ofBits t m) := by
  cases t <;> simp only [IntTy.bits] at h <;>
    simp [ofBits, IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] <;>
    (try split) <;> omega

theorem ofBits_toBits (t : IntTy) (n : Int) (h : t.InRange n) : ofBits t (toBits t n) = n := by
  cases t <;> simp [IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at h <;>
    simp [ofBits, toBits, IntTy.signed, IntTy.bits] <;> (try split) <;> omega

theorem wrap_inRange (t : IntTy) (n : Int) : t.InRange (wrap t n) :=
  ofBits_inRange t _ (toBits_lt t n)

theorem wrap_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : wrap t n = n :=
  ofBits_toBits t n h

theorem IntTy.min_le_max (t : IntTy) : t.min ≤ t.max := by
  cases t <;> decide

theorem saturate_inRange (t : IntTy) (n : Int) : t.InRange (saturate t n) := by
  have := t.min_le_max
  unfold saturate IntTy.InRange
  split
  · omega
  · split <;> omega

theorem bitwise_inRange (t : IntTy) (op : BitOp) (a b : Int) : t.InRange (bitwise t op a b) := by
  have ha := toBits_lt t a
  have hb := toBits_lt t b
  cases op
  · exact ofBits_inRange t _ (Nat.lt_of_le_of_lt Nat.and_le_left ha)
  · exact ofBits_inRange t _ (Nat.or_lt_two_pow ha hb)
  · exact ofBits_inRange t _ (Nat.xor_lt_two_pow ha hb)

theorem bitNot_inRange (t : IntTy) (a : Int) : t.InRange (bitNot t a) := by
  have := toBits_lt t a
  have hp : 0 < 2 ^ t.bits := Nat.two_pow_pos _
  exact ofBits_inRange t _ (by omega)

/-! ### Checked operations stay in range -/

theorem inRangeOr_inRange {t : IntTy} {r s : Int} (h : inRangeOr t r = .ok s) : t.InRange s := by
  unfold inRangeOr at h
  split at h
  · cases h; assumption
  · cases h

/-- A right shift of a value in range is in range: it moves the value toward the sign's end. -/
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

/-- A remainder of a value in range by a nonzero divisor is in range: it lies between `0` and the
dividend. -/
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

/-- Every successful checked operation on operands in range gives a value in range. -/
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

theorem plain_ok_iff (t : IntTy) (op : ArithOp) (a b r : Int) :
    plain t op a b = .ok r ↔ checked t op a b = .ok r := by
  unfold plain
  cases checked t op a b <;> simp

theorem plain_trap_iff (t : IntTy) (op : ArithOp) (a b : Int) (k : TrapKind) :
    plain t op a b = .trap k ↔ ∃ e, checked t op a b = .error e ∧ e.trapKind = k := by
  unfold plain
  cases checked t op a b <;> simp

/-- A plain operation is never stuck: it has a result or a trap. -/
theorem plain_not_stuck (t : IntTy) (op : ArithOp) (a b : Int) : plain t op a b ≠ .stuck := by
  unfold plain
  cases checked t op a b <;> simp

/-- The arithmetic operation of a wrapping-capable operation. -/
def WrapOp.arith : WrapOp → ArithOp
  | .add => .add
  | .sub => .sub
  | .mul => .mul

theorem checked_wrapOp (t : IntTy) (op : WrapOp) (a b : Int) :
    checked t op.arith a b = inRangeOr t (op.exact a b) := by
  cases op <;> rfl

theorem wrapping_of_checked (t : IntTy) (op : WrapOp) (a b r : Int)
    (h : checked t op.arith a b = .ok r) : wrapping t op a b = r := by
  rw [checked_wrapOp] at h
  unfold inRangeOr at h
  split at h
  · cases h; exact wrap_of_inRange t _ (by assumption)
  · cases h

theorem saturating_of_checked (t : IntTy) (op : WrapOp) (a b r : Int)
    (h : checked t op.arith a b = .ok r) : saturating t op a b = r := by
  rw [checked_wrapOp] at h
  unfold inRangeOr at h
  split at h
  · cases h
    rename_i hr
    obtain ⟨h1, h2⟩ := hr
    unfold saturating saturate
    split
    · omega
    · split
      · omega
      · rfl
  · cases h

/-! ### Conversions -/

theorem lossless_iff (s t : IntTy) : Lossless s t ↔ ∀ n, s.InRange n → t.InRange n := by
  constructor
  · intro ⟨h1, h2⟩ n ⟨hn1, hn2⟩
    exact ⟨Int.le_trans h1 hn1, Int.le_trans hn2 h2⟩
  · intro h
    have hm := s.min_le_max
    exact ⟨(h s.min ⟨Int.le_refl _, hm⟩).1, (h s.max ⟨hm, Int.le_refl _⟩).2⟩

theorem tryConvert_ok_iff (t : IntTy) (n r : Int) :
    tryConvert t n = .ok r ↔ t.InRange n ∧ r = n := by
  unfold tryConvert
  split
  · simp_all [eq_comm]
  · simp_all

/-! ### Machine and exact arithmetic in contracts -/

theorem exact_defined (ρ : Var → Option Int) (op : WrapOp) (a b : CTerm) (x y : Int)
    (ha : a.eval ρ = some x) (hb : b.eval ρ = some y) :
    (CTerm.exact op a b).eval ρ = some (op.exact x y) := by
  simp [CTerm.eval, ha, hb]

/-- Where a plain machine term is defined, it means the exact operation: the two differ only in
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
      rw [checked_wrapOp] at h
      unfold inRangeOr at h
      by_cases hr : t.InRange (op.exact x y)
      · simp [hr] at h; subst h; rfl
      · simp [hr] at h

end ConcreteSpec.Core
