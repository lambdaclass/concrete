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
  (`exact_defined`); a defined machine term has operands of its width (`machine_operands`); where a
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

/-- Saturation keeps a value in range. -/
theorem saturate_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : saturate t n = n := by
  obtain ⟨h1, h2⟩ := h
  unfold saturate
  split
  · omega
  · split
    · omega
    · rfl

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

/-- A remainder of a value in range is in range: it lies between 0 and the dividend. This holds
for every divisor (Lean's tmod by 0 is the dividend), so `checked` needs no extra case. -/
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
    exact saturate_of_inRange t _ (by assumption)
  · cases h

/-! ### Conversions -/

theorem lossless_iff (s t : IntTy) : Lossless s t ↔ ∀ n, s.InRange n → t.InRange n := by
  constructor
  · intro ⟨h1, h2⟩ n ⟨hn1, hn2⟩
    exact ⟨Int.le_trans h1 hn1, Int.le_trans hn2 h2⟩
  · intro h
    have hm := s.min_le_max
    exact ⟨(h s.min ⟨Int.le_refl _, hm⟩).1, (h s.max ⟨hm, Int.le_refl _⟩).2⟩

/-- `bool` converts losslessly to every integer type. -/
theorem boolToInt_inRange (t : IntTy) (b : Bool) : t.InRange (boolToInt b) := by
  cases t <;> cases b <;> decide

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
      split at h
      · cases h
      · rw [checked_wrapOp] at h
        unfold inRangeOr at h
        by_cases hr : t.InRange (op.exact x y)
        · simp [hr] at h; subst h; rfl
        · simp [hr] at h

/-- A defined machine term has operands that are values of its width (a shift count excepted). -/
theorem machine_operands (ρ : Var → Option Int) (t : IntTy) (op : MachineOp) (a b : CTerm)
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

/-- A defined machine term's value is a value of its width. -/
theorem machine_inRange (ρ : Var → Option Int) (t : IntTy) (op : MachineOp) (a b : CTerm)
    (r : Int) (h : (CTerm.machine t op a b).eval ρ = some r) : t.InRange r := by
  obtain ⟨x, y, ha, hb, hx, _⟩ := machine_operands ρ t op a b r h
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

theorem exactOf?_some {op : MachineOp} {w : WrapOp} (h : op.exactOf? = some w) :
    op = .plain w.arith := by
  cases op with
  | plain o => cases o <;> cases h <;> rfl
  | wrapping _ => cases h
  | saturating _ => cases h

/-- **Reading in exact arithmetic.** Where a contract term is defined, its exact reading is
defined and has the same value. -/
theorem eval_toExact (ρ : Var → Option Int) : ∀ (e : CTerm) (r : Int),
    e.eval ρ = some r → e.toExact.eval ρ = some r
  | .lit _, _, h => h
  | .var _, _, h => h
  | .machine t op a b, r, h => by
    obtain ⟨x, y, ha, hb, _, _⟩ := machine_operands ρ t op a b r h
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

/-! ### Negation, `bool` patterns, lossy conversion -/

theorem plainNeg_ok_iff (t : IntTy) (a r : Int) : plainNeg t a = .ok r ↔ checkedNeg t a = .ok r := by
  unfold plainNeg
  cases checkedNeg t a <;> simp

theorem plainNeg_trap_iff (t : IntTy) (a : Int) (k : TrapKind) :
    plainNeg t a = .trap k ↔ ∃ e, checkedNeg t a = .error e ∧ e.trapKind = k := by
  unfold plainNeg
  cases checkedNeg t a <;> simp

/-- Helper (arithmetic). Plain negation is never stuck: it has a result or a trap.
(approved: PENDING) -/
theorem plainNeg_not_stuck (t : IntTy) (a : Int) : plainNeg t a ≠ .stuck := by
  unfold plainNeg
  cases checkedNeg t a <;> simp

/-- Helper (arithmetic). Representability of mathematical negation, independently of the
`checkedNeg` result wrapper. (approved: PENDING) -/
private theorem neg_inRange_iff (t : IntTy) (a : Int) (ha : t.InRange a) :
    t.InRange (-a) ↔ (if t.signed = true then a ≠ t.min else a = 0) := by
  cases t <;> simp [IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at * <;> omega

/-- Law (subject `ConcreteSpec.Core.checkedNeg`). Checked negation succeeds exactly with the mathematical negation, when that integer is
representable: signed MIN and nonzero unsigned inputs are the only invalid values in range. -/
theorem checkedNeg_ok_arithmetic (t : IntTy) (a r : Int) (ha : t.InRange a) :
    checkedNeg t a = .ok r ↔ r = -a ∧
      (if t.signed = true then a ≠ t.min else a = 0) := by
  unfold checkedNeg inRangeOr
  split
  · rename_i hn
    have hc := (neg_inRange_iff t a ha).1 hn
    constructor
    · intro h
      cases h
      exact ⟨rfl, hc⟩
    · rintro ⟨rfl, _⟩
      rfl
  · rename_i hn
    simp only [reduceCtorEq, false_iff, not_and]
    intro _ hc
    exact hn ((neg_inRange_iff t a ha).2 hc)

/-- Law (subject `ConcreteSpec.Core.checkedNeg`). Checked negation fails only with Overflow, exactly for signed MIN or a nonzero unsigned
input. -/
theorem checkedNeg_error_iff (t : IntTy) (a : Int) (e : ArithmeticError) (ha : t.InRange a) :
    checkedNeg t a = .error e ↔ e = .overflow ∧
      ¬(if t.signed = true then a ≠ t.min else a = 0) := by
  unfold checkedNeg inRangeOr
  split
  · rename_i hn
    have hc := (neg_inRange_iff t a ha).1 hn
    constructor
    · intro h
      cases h
    · rintro ⟨_, hnc⟩
      exact (hnc hc).elim
  · rename_i hn
    have hc : ¬(if t.signed = true then a ≠ t.min else a = 0) := by
      intro hc
      exact hn ((neg_inRange_iff t a ha).2 hc)
    constructor
    · intro h
      cases h
      exact ⟨rfl, hc⟩
    · rintro ⟨rfl, _⟩
      rfl

/-- Incorrect negation that preserves its input. -/
def identityNeg (_t : IntTy) (a : Int) : Except ArithmeticError Int := .ok a

/-- Incorrect negation that never reports overflow. -/
def uncheckedNeg (_t : IntTy) (a : Int) : Except ArithmeticError Int := .ok (-a)

/-- Instance (law `ConcreteSpec.Core.checkedNeg_ok_arithmetic`, case `signed`). -/
example : checkedNeg .i8 5 = .ok (-5) ↔ (-5 : Int) = -5 ∧
    (if IntTy.i8.signed = true then (5 : Int) ≠ IntTy.i8.min else (5 : Int) = 0) :=
  checkedNeg_ok_arithmetic .i8 5 (-5) (by decide)

/-- Planted (law `ConcreteSpec.Core.checkedNeg_ok_arithmetic`, case `signed`,
subject `ConcreteSpec.Core.checkedNeg`, mutant `ConcreteSpec.Core.identityNeg`). -/
example : IntTy.i8.InRange 5 ∧ ¬(identityNeg .i8 5 = .ok (-5) ↔ (-5 : Int) = -5 ∧
    (if IntTy.i8.signed = true then (5 : Int) ≠ IntTy.i8.min else (5 : Int) = 0)) := by decide

/-- Instance (law `ConcreteSpec.Core.checkedNeg_error_iff`, case `minimum`). -/
example : checkedNeg .i8 (-128) = .error .overflow ↔ ArithmeticError.overflow = .overflow ∧
    ¬(if IntTy.i8.signed = true then (-128 : Int) ≠ IntTy.i8.min else (-128 : Int) = 0) :=
  checkedNeg_error_iff .i8 (-128) .overflow (by decide)

/-- Planted (law `ConcreteSpec.Core.checkedNeg_error_iff`, case `minimum`,
subject `ConcreteSpec.Core.checkedNeg`, mutant `ConcreteSpec.Core.uncheckedNeg`). -/
example : IntTy.i8.InRange (-128) ∧ ¬(uncheckedNeg .i8 (-128) = .error .overflow ↔
    ArithmeticError.overflow = .overflow ∧
      ¬(if IntTy.i8.signed = true then (-128 : Int) ≠ IntTy.i8.min else (-128 : Int) = 0)) := by decide

theorem boolOfBits_boolToBits (b : Bool) : boolOfBits (boolToBits b) = some b := by
  cases b <;> rfl

theorem boolOfBits_some_iff (m : Nat) (b : Bool) : boolOfBits m = some b ↔ m = boolToBits b := by
  match m with
  | 0 => cases b <;> simp [boolOfBits, boolToBits]
  | 1 => cases b <;> simp [boolOfBits, boolToBits]
  | m + 2 => cases b <;> simp [boolOfBits, boolToBits]

theorem lossyConvert_inRange (t : IntTy) (n : Int) : t.InRange (lossyConvert t n) := wrap_inRange t n

/-- A lossy conversion of a value of the target type keeps it. -/
theorem lossyConvert_of_inRange (t : IntTy) (n : Int) (h : t.InRange n) : lossyConvert t n = n :=
  wrap_of_inRange t n h

/-! ### Bit-level correspondence -/

theorem toBits_ofBits (t : IntTy) (m : Nat) (h : m < 2 ^ t.bits) : toBits t (ofBits t m) = m := by
  cases t <;> simp only [IntTy.bits] at h <;>
    simp [toBits, ofBits, IntTy.signed, IntTy.bits] <;> (try split) <;> omega

/-- Wrapping keeps the bit pattern: `wrap` only reads the low number of bits specified by the integer type. -/
theorem toBits_wrap (t : IntTy) (n : Int) : toBits t (wrap t n) = toBits t n :=
  toBits_ofBits t _ (toBits_lt t n)

/-- A wrapping operation's pattern is the exact result's pattern, modulo 2^bits. -/
theorem toBits_wrapping (t : IntTy) (op : WrapOp) (a b : Int) :
    toBits t (wrapping t op a b) = toBits t (op.exact a b) := toBits_wrap t _

/-- A valid left shift's pattern is the pattern of a · 2^c: the bits shifted out are gone. -/
theorem toBits_shl (t : IntTy) (a c r : Int) (h : checked t .shl a c = .ok r) :
    toBits t r = toBits t (a * (2 : Int) ^ c.toNat) := by
  simp only [checked] at h
  split at h
  · cases h; exact toBits_wrap t _
  · cases h

/-- An unsigned right shift is the logical shift of the pattern. -/
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

/-- Bitwise operations act on the patterns, bit by bit. -/
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

/-- Bitwise not flips every bit of the pattern. -/
theorem toBits_bitNot (t : IntTy) (a : Int) :
    toBits t (bitNot t a) = 2 ^ t.bits - 1 - toBits t a := by
  have := toBits_lt t a
  have hp : 0 < 2 ^ t.bits := Nat.two_pow_pos _
  exact toBits_ofBits t _ (by omega)

/-- Saturation clamps: below the range it gives the minimum, above it the maximum. -/
theorem saturate_below (t : IntTy) (n : Int) (h : n < t.min) : saturate t n = t.min := by
  simp [saturate, h]

theorem saturate_above (t : IntTy) (n : Int) (h : t.max < n) : saturate t n = t.max := by
  have := t.min_le_max
  have : ¬ n < t.min := by omega
  simp [saturate, h, this]

end ConcreteSpec.Core
