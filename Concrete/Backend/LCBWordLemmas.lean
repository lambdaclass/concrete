import ConcreteSpec.Proofs.Scalar
import LeanCompilerBackend.Verified.Source

/-!
# Pure LCB 32-bit word arithmetic, checked-add range tests, and return-word lemmas

Standalone arithmetic and `LeanCompilerBackend.Verified.Source` execution lemmas for the LCB
backend, with zero provisional BIR or legacy LCB imports.

Representation over `ConcreteSpec.Core.IntTy`:
* An integer of type `t` with `t.bits ≤ 16` (`u8`, `i8`, `u16`, `i16`) is represented as the 32-bit
  word `BitVec.ofInt 32 v`; a boolean flag is `0#32` (`false`) or `1#32` (`true`).
* For every `WrapOp` (`add`, `sub`, `mul`) on in-range operands of a profile type `t`, the exact
  mathematical result fits cleanly in 32-bit biased unsigned arithmetic:
  - wrapping: `((raw - t.min) &&& (2 ^ t.bits - 1)) + t.min = wordBits (wrap t (op.exact x y))`;
  - range test: `(raw - t.min).toNat < 2 ^ t.bits ↔ t.InRange (op.exact x y)`;
  - overflow flag: `eqBit (ultBit biased (2 ^ t.bits)) 0`, `uleBit (2 ^ t.bits) biased`, and
    `ultBit (2 ^ t.bits - 1) biased` all equal `boolBits (!decide (t.InRange (op.exact x y)))`.
* Hosted return words reserve `0 .. 255` for ordinary `u8` exits and `256 + slot` for trap slots
  below `trapCapacity = 2 ^ 32 - 256`.

Every theorem in this module is proved constructively without `Classical.choice`.
-/

namespace Concrete.Backend.LCBWordLemmas

open ConcreteSpec.Core
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

/-! ## Word-profile types and 32-bit encodings -/

/-- Integer types whose exact `add`/`sub`/`mul` results fit 32-bit biased unsigned arithmetic. -/
def inWordProfile (t : IntTy) : Bool := t.bits ≤ 16

theorem inWordProfile_cases {t : IntTy} (ht : inWordProfile t = true) :
    t = .u8 ∨ t = .i8 ∨ t = .u16 ∨ t = .i16 := by
  cases t with
  | u8 => exact Or.inl rfl
  | i8 => exact Or.inr (Or.inl rfl)
  | u16 => exact Or.inr (Or.inr (Or.inl rfl))
  | i16 => exact Or.inr (Or.inr (Or.inr rfl))
  | u32 | i32 | u64 | i64 => cases ht

theorem inWordProfile_iff (t : IntTy) :
    inWordProfile t = true ↔ t = .u8 ∨ t = .i8 ∨ t = .u16 ∨ t = .i16 := by
  constructor
  · exact inWordProfile_cases
  · rintro (rfl | rfl | rfl | rfl) <;> rfl

/-- The 32-bit two's-complement word representing an integer payload. -/
def wordBits (n : Int) : BitVec 32 := BitVec.ofInt 32 n

/-- A 32-bit LCB source value holding an integer payload. -/
def word (n : Int) : Source.Value := .word (wordBits n)

/-- The 32-bit bitvector encoding of a boolean flag (`0` or `1`). -/
def boolBits (b : Bool) : BitVec 32 := if b then 1 else 0

/-- A 32-bit LCB source value holding a boolean flag (`0` or `1`). -/
def boolWord (b : Bool) : Source.Value := .word (boolBits b)

/-- LCB binary operator corresponding to a wrapping-capable Core operator. -/
def binaryOfWrapOp : WrapOp → Source.Binary
  | .add => .add
  | .sub => .sub
  | .mul => .mul

/-- Comparison result words matching `Source.evaluateComparison`. -/
def ultBit (p q : BitVec 32) : BitVec 32 := if p.toNat < q.toNat then 1 else 0
def uleBit (p q : BitVec 32) : BitVec 32 := if p.toNat ≤ q.toNat then 1 else 0
def eqBit (p q : BitVec 32) : BitVec 32 := if p.toNat = q.toNat then 1 else 0

theorem word_width (n : Int) : (word n).width = .word := rfl

theorem boolWord_width (b : Bool) : (boolWord b).width = .word := rfl

theorem boolWord_eq_word (b : Bool) : boolWord b = word (if b then 1 else 0) := by
  cases b <;> rfl

theorem boolWord_toNat (b : Bool) : (boolWord b).toNat = if b then 1 else 0 := by
  cases b <;> rfl

theorem boolWord_branch (b : Bool) : ((boolWord b).toNat != 0) = b := by
  cases b <;> rfl

theorem wordBits_ofNat (n : Nat) : wordBits (n : Int) = BitVec.ofNat 32 n := by
  unfold wordBits
  exact BitVec.ofInt_natCast 32 n

theorem word_ofNat (n : Nat) : word (n : Int) = .word (BitVec.ofNat 32 n) :=
  congrArg Source.Value.word (wordBits_ofNat n)

theorem wordBits_toNat_eq_toBitsAtWidth (n : Int) :
    (wordBits n).toNat = toBitsAtWidth 32 n := by
  unfold wordBits toBitsAtWidth
  rw [BitVec.toNat_ofInt]
  rfl

theorem wordBits_toInt_eq {t : IntTy} (ht : inWordProfile t = true) {n : Int}
    (hn : t.InRange n) : (wordBits n).toInt = n := by
  unfold wordBits
  rw [BitVec.toInt_ofInt]
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl <;>
  · cases hn with
    | intro hlo hhi =>
      simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, eq_self,
        Bool.false_eq_true, ↓reduceIte, Nat.reduceSub, Int.reducePow] at hlo hhi
      exact Int.bmod_eq_of_le (by omega) (by omega)

theorem wordBits_injective_inRange {t : IntTy} (ht : inWordProfile t = true) {a b : Int}
    (ha : t.InRange a) (hb : t.InRange b) (heq : wordBits a = wordBits b) : a = b := by
  have h := congrArg BitVec.toInt heq
  rw [wordBits_toInt_eq ht ha, wordBits_toInt_eq ht hb] at h
  exact h

theorem wordBits_unsigned_eq_ofNat_toBits {t : IntTy} (ht : inWordProfile t = true)
    (hu : t.signed = false) {n : Int} (hn : t.InRange n) :
    wordBits n = BitVec.ofNat 32 (toBits t n) := by
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl
  · cases hn with
    | intro hlo hhi =>
      simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
        ↓reduceIte, Int.reducePow] at hlo hhi
      have htb : (toBits .u8 n : Int) = n := by
        unfold toBits toBitsAtWidth
        simp only [IntTy.bits, Int.reducePow]
        omega
      rw [← wordBits_ofNat, htb]
  · cases hu
  · cases hn with
    | intro hlo hhi =>
      simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
        ↓reduceIte, Int.reducePow] at hlo hhi
      have htb : (toBits .u16 n : Int) = n := by
        unfold toBits toBitsAtWidth
        simp only [IntTy.bits, Int.reducePow]
        omega
      rw [← wordBits_ofNat, htb]
  · cases hu

/-! ## 32-bit ring, bit-mask, and wrapping arithmetic -/

theorem wordBits_add (x y : Int) : wordBits (x + y) = wordBits x + wordBits y :=
  BitVec.ofInt_add (n := 32) (x := x) (y := y)

theorem wordBits_sub (x y : Int) : wordBits (x - y) = wordBits x - wordBits y := by
  have h : wordBits x = wordBits (x - y) + wordBits y := by
    rw [← wordBits_add]
    congr 1
    omega
  rw [h, BitVec.add_sub_cancel]

theorem wordBits_mul (x y : Int) : wordBits (x * y) = wordBits x * wordBits y :=
  BitVec.ofInt_mul (n := 32) (x := x) (y := y)

theorem binaryBits_exact (op : WrapOp) (x y : Int) :
    Source.binaryBits (binaryOfWrapOp op) (wordBits x) (wordBits y) = wordBits (op.exact x y) := by
  cases op with
  | add => exact (wordBits_add x y).symm
  | sub => exact (wordBits_sub x y).symm
  | mul => exact (wordBits_mul x y).symm

private theorem testBit_two_pow_sub_one (k i : Nat) :
    (2 ^ k - 1).testBit i = decide (i < k) := by
  induction k generalizing i with
  | zero =>
    rw [Nat.pow_zero, Nat.sub_self, Nat.zero_testBit, decide_eq_false (Nat.not_lt_zero i)]
  | succ k ih =>
    have hpos : 1 ≤ 2 ^ k := Nat.one_le_two_pow
    have heq : 2 ^ (k + 1) - 1 = 2 * (2 ^ k - 1) + 1 := by
      rw [Nat.pow_succ]; omega
    rw [heq]
    cases i with
    | zero =>
      rw [Nat.testBit_zero]
      have hmod : (2 * (2 ^ k - 1) + 1) % 2 = 1 := by omega
      rw [hmod]
      rfl
    | succ i =>
      rw [Nat.testBit_succ, show (2 * (2 ^ k - 1) + 1) / 2 = 2 ^ k - 1 by omega, ih i]
      exact decide_eq_decide.mpr ⟨Nat.succ_lt_succ, Nat.lt_of_succ_lt_succ⟩

private theorem nat_and_two_pow_sub_one_eq_mod (n k : Nat) :
    n &&& (2 ^ k - 1) = n % 2 ^ k := by
  apply Nat.eq_of_testBit_eq
  intro i
  rw [Nat.testBit_and, testBit_two_pow_sub_one, Nat.testBit_mod_two_pow, Bool.and_comm]

/-- Masking a 32-bit word with `2 ^ b - 1` reduces modulo `2 ^ b` for 8- and 16-bit widths. -/
theorem mask_lit (b : Nat) (hb : b = 8 ∨ b = 16) (z : Int) :
    wordBits z &&& wordBits (2 ^ b - 1) = wordBits (z % (2 : Int) ^ b) := by
  apply BitVec.eq_of_toNat_eq
  have hc : (wordBits (2 ^ b - 1)).toNat = 2 ^ b - 1 := by
    rcases hb with rfl | rfl <;> decide
  rw [BitVec.toNat_and, hc, nat_and_two_pow_sub_one_eq_mod]
  unfold wordBits
  rw [BitVec.toNat_ofInt, BitVec.toNat_ofInt]
  rcases hb with rfl | rfl <;>
  · simp only [Nat.reducePow, Int.reducePow]
    omega

theorem mask_ofInt {t : IntTy} (ht : inWordProfile t = true) (z : Int) :
    wordBits z &&& wordBits (2 ^ t.bits - 1) = wordBits (z % (2 : Int) ^ t.bits) := by
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl
  · exact mask_lit 8 (Or.inl rfl) z
  · exact mask_lit 8 (Or.inl rfl) z
  · exact mask_lit 16 (Or.inr rfl) z
  · exact mask_lit 16 (Or.inr rfl) z

/-- Core's `wrap` coincides with the biased modular reduction for every `IntTy`. -/
theorem wrap_eq_biased_mod (t : IntTy) (z : Int) :
    wrap t z = (z - t.min) % (2 : Int) ^ t.bits + t.min := by
  cases t <;>
  · simp only [wrap, ofBits, toBits, toBitsAtWidth, IntTy.min, IntTy.bits, IntTy.signed,
      eq_self, Bool.false_eq_true, ↓reduceIte, Nat.reduceSub, Nat.reducePow,
      Int.reducePow, true_and, false_and]
    (try split) <;> omega

/-- The biased-mask-rebias 32-bit word sequence computes `Core.wrap t z`. -/
theorem wrap_word {t : IntTy} (ht : inWordProfile t = true) (z : Int) :
    ((wordBits z - wordBits t.min) &&& wordBits (2 ^ t.bits - 1)) + wordBits t.min =
      wordBits (wrap t z) := by
  rw [← wordBits_sub, mask_ofInt ht, ← wordBits_add, wrap_eq_biased_mod]

theorem wrap_word_of_inRange {t : IntTy} (ht : inWordProfile t = true) {z : Int}
    (hz : t.InRange z) :
    ((wordBits z - wordBits t.min) &&& wordBits (2 ^ t.bits - 1)) + wordBits t.min = wordBits z := by
  rw [wrap_word ht, wrap_of_inRange t z hz]

theorem wrap_word_unsigned {t : IntTy} (ht : inWordProfile t = true) (hu : t.signed = false)
    (z : Int) :
    wordBits z &&& wordBits (2 ^ t.bits - 1) = wordBits (wrap t z) := by
  have hmin : t.min = 0 := by unfold IntTy.min; rw [hu]; rfl
  have hw := wrap_word ht z
  rw [hmin] at hw
  have hsub : wordBits z - wordBits 0 = wordBits z := by
    rw [← wordBits_sub, Int.sub_zero]
  have hzero : wordBits 0 = 0#32 := rfl
  have hadd : (wordBits z &&& wordBits (2 ^ t.bits - 1)) + wordBits 0 =
      wordBits z &&& wordBits (2 ^ t.bits - 1) := by
    rw [hzero, BitVec.add_zero]
  rw [hsub, hadd] at hw
  exact hw

theorem wrap_word_u8 (z : Int) :
    wordBits z &&& wordBits 255 = wordBits (wrap .u8 z) :=
  wrap_word_unsigned (t := .u8) rfl rfl z

theorem wrapping_word {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) (x y : Int) :
    ((Source.binaryBits (binaryOfWrapOp op) (wordBits x) (wordBits y) - wordBits t.min) &&&
      wordBits (2 ^ t.bits - 1)) + wordBits t.min = wordBits (wrapping t op x y) := by
  rw [binaryBits_exact, wrap_word ht]
  rfl

/-! ## Checked-add and small-width range-test arithmetic -/

/-- The intermediate words of a checked or wrapping small-width expansion. -/
def raw (op : WrapOp) (x y : Int) : BitVec 32 :=
  Source.binaryBits (binaryOfWrapOp op) (wordBits x) (wordBits y)

def biased (t : IntTy) (z : BitVec 32) : BitVec 32 :=
  Source.binaryBits .sub z (wordBits t.min)

def masked (t : IntTy) (z : BitVec 32) : BitVec 32 :=
  Source.binaryBits .andBits z (wordBits (2 ^ t.bits - 1))

theorem raw_eq (op : WrapOp) (x y : Int) : raw op x y = wordBits (op.exact x y) :=
  binaryBits_exact op x y

theorem biased_raw_eq (t : IntTy) (op : WrapOp) (x y : Int) :
    biased t (raw op x y) = wordBits (op.exact x y - t.min) := by
  unfold biased Source.binaryBits
  rw [raw_eq, wordBits_sub]

theorem natAbs_le_of_inRange {t : IntTy} (ht : inWordProfile t = true) {x : Int}
    (hx : t.InRange x) : x.natAbs ≤ 65535 := by
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl <;>
  · cases hx with
    | intro hlo hhi =>
      simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, eq_self,
        Bool.false_eq_true, ↓reduceIte, Nat.reduceSub, Int.reducePow] at hlo hhi
      omega

theorem exact_natAbs_le {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) : (op.exact x y).natAbs ≤ 4294836225 := by
  have bx := natAbs_le_of_inRange ht hx
  have by' := natAbs_le_of_inRange ht hy
  cases op with
  | add => dsimp only [WrapOp.exact]; omega
  | sub => dsimp only [WrapOp.exact]; omega
  | mul =>
    dsimp only [WrapOp.exact]
    rw [Int.natAbs_mul]
    exact Nat.le_trans (Nat.mul_le_mul bx by') (by decide)

theorem add_natAbs_le {t : IntTy} (ht : inWordProfile t = true) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) : (x + y).natAbs ≤ 131070 := by
  have bx := natAbs_le_of_inRange ht hx
  have by' := natAbs_le_of_inRange ht hy
  omega

theorem pow_bits_toNat {t : IntTy} (ht : inWordProfile t = true) :
    (wordBits (2 ^ t.bits)).toNat = 2 ^ t.bits := by
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl <;> decide

theorem pow_bits_sub_one_toNat {t : IntTy} (ht : inWordProfile t = true) :
    (wordBits (2 ^ t.bits - 1)).toNat = 2 ^ t.bits - 1 := by
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl <;> decide

/-- Exact range-test characterization: for any profile type and `WrapOp`, the 32-bit unsigned
biased word is below `2 ^ t.bits` iff the mathematical result is in `t.InRange`. -/
theorem biased_toNat_lt_iff {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    (biased t (raw op x y)).toNat < 2 ^ t.bits ↔ t.InRange (op.exact x y) := by
  have hb := exact_natAbs_le ht op hx hy
  rw [biased_raw_eq]
  unfold wordBits
  rw [BitVec.toNat_ofInt]
  generalize op.exact x y = r at hb ⊢
  rcases inWordProfile_cases ht with rfl | rfl | rfl | rfl <;>
  · simp only [IntTy.InRange, IntTy.min, IntTy.max, IntTy.bits, IntTy.signed,
      eq_self, Bool.false_eq_true, ↓reduceIte, Nat.reduceSub, Nat.reducePow,
      Int.reducePow]
    constructor
    · intro h
      exact ⟨by omega, by omega⟩
    · intro h
      cases h with
      | intro hlo hhi => omega

theorem biased_toNat_le_iff {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    (biased t (raw op x y)).toNat ≤ 2 ^ t.bits - 1 ↔ t.InRange (op.exact x y) := by
  have hpos : 0 < 2 ^ t.bits := Nat.two_pow_pos _
  constructor
  · intro hle
    exact (biased_toNat_lt_iff ht op hx hy).mp (by omega)
  · intro hin
    have hlt := (biased_toNat_lt_iff ht op hx hy).mpr hin
    omega

theorem rangeTest_word {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    decide ((biased t (raw op x y)).toNat < 2 ^ t.bits) = decide (t.InRange (op.exact x y)) := by
  exact decide_eq_decide.mpr (biased_toNat_lt_iff ht op hx hy)

/-- Two-comparison overflow flag: `(biased <u 2^bits) == 0` equals `1` iff overflow. -/
theorem flag_ult_eq {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    eqBit (ultBit (biased t (raw op x y)) (wordBits (2 ^ t.bits))) (wordBits 0) =
      boolBits (!decide (t.InRange (op.exact x y))) := by
  unfold eqBit ultBit boolBits
  rw [pow_bits_toNat ht]
  have hiff := biased_toNat_lt_iff ht op hx hy
  if hin : t.InRange (op.exact x y) then
    have hlt : (biased t (raw op x y)).toNat < 2 ^ t.bits := hiff.mpr hin
    rw [ite_eq_left hlt, decide_eq_true hin]
    rfl
  else
    have hnot : ¬ (biased t (raw op x y)).toNat < 2 ^ t.bits := fun h => hin (hiff.mp h)
    rw [ite_eq_right hnot, decide_eq_false hin]
    rfl

/-- Single-comparison overflow flag: `2^bits <=u biased` equals `1` iff overflow. -/
theorem flag_ule {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    uleBit (wordBits (2 ^ t.bits)) (biased t (raw op x y)) =
      boolBits (!decide (t.InRange (op.exact x y))) := by
  unfold uleBit boolBits
  rw [pow_bits_toNat ht]
  have hiff := biased_toNat_lt_iff ht op hx hy
  if hin : t.InRange (op.exact x y) then
    have hlt : (biased t (raw op x y)).toNat < 2 ^ t.bits := hiff.mpr hin
    have hnot : ¬ 2 ^ t.bits ≤ (biased t (raw op x y)).toNat := Nat.not_le_of_lt hlt
    rw [ite_eq_right hnot, decide_eq_true hin]
    rfl
  else
    have hle : 2 ^ t.bits ≤ (biased t (raw op x y)).toNat :=
      Nat.le_of_not_lt (fun h => hin (hiff.mp h))
    rw [ite_eq_left hle, decide_eq_false hin]
    rfl

/-- Single-comparison overflow flag: `(2^bits - 1) <u biased` equals `1` iff overflow. -/
theorem flag_ult_max {t : IntTy} (ht : inWordProfile t = true) (op : WrapOp) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    ultBit (wordBits (2 ^ t.bits - 1)) (biased t (raw op x y)) =
      boolBits (!decide (t.InRange (op.exact x y))) := by
  unfold ultBit boolBits
  rw [pow_bits_sub_one_toNat ht]
  have hiff := biased_toNat_le_iff ht op hx hy
  if hin : t.InRange (op.exact x y) then
    have hle : (biased t (raw op x y)).toNat ≤ 2 ^ t.bits - 1 := hiff.mpr hin
    have hnot : ¬ 2 ^ t.bits - 1 < (biased t (raw op x y)).toNat := Nat.not_lt_of_ge hle
    rw [ite_eq_right hnot, decide_eq_true hin]
    rfl
  else
    have hlt : 2 ^ t.bits - 1 < (biased t (raw op x y)).toNat :=
      Nat.lt_of_not_ge (fun h => hin (hiff.mp h))
    rw [ite_eq_left hlt, decide_eq_false hin]
    rfl

/-- Direct checked-add connection: the biased range test succeeds iff `Core.checked t .add`
returns `.ok (x + y)`. -/
theorem checkedAdd_rangeTest_iff {t : IntTy} (ht : inWordProfile t = true) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    (biased t (raw .add x y)).toNat < 2 ^ t.bits ↔ checked t .add x y = .ok (x + y) := by
  rw [biased_toNat_lt_iff ht .add hx hy]
  dsimp only [checked, inRangeOr, WrapOp.exact]
  if hin : t.InRange (x + y) then
    rw [ite_eq_left hin]
    exact ⟨fun _ => rfl, fun _ => hin⟩
  else
    rw [ite_eq_right hin]
    exact ⟨fun h => (hin h).elim, fun h => by cases h⟩

theorem checkedAdd_overflow_iff {t : IntTy} (ht : inWordProfile t = true) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    2 ^ t.bits ≤ (biased t (raw .add x y)).toNat ↔ checked t .add x y = .error .overflow := by
  have hiff := biased_toNat_lt_iff ht .add hx hy
  dsimp only [checked, inRangeOr]
  dsimp only [WrapOp.exact] at hiff
  if hin : t.InRange (x + y) then
    rw [ite_eq_left hin]
    have hlt := hiff.mpr hin
    exact ⟨fun hle => (Nat.not_le_of_lt hlt hle).elim, fun h => by cases h⟩
  else
    rw [ite_eq_right hin]
    have hle : 2 ^ t.bits ≤ (biased t (raw .add x y)).toNat :=
      Nat.le_of_not_lt (fun h => hin (hiff.mp h))
    exact ⟨fun _ => rfl, fun _ => hle⟩

theorem checkedAdd_plain_ok_iff {t : IntTy} (ht : inWordProfile t = true) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    (biased t (raw .add x y)).toNat < 2 ^ t.bits ↔ plain t .add x y = .ok (x + y) :=
  (checkedAdd_rangeTest_iff ht hx hy).trans (plain_ok_iff t .add x y (x + y)).symm

theorem checkedAdd_plain_trap_iff {t : IntTy} (ht : inWordProfile t = true) {x y : Int}
    (hx : t.InRange x) (hy : t.InRange y) :
    2 ^ t.bits ≤ (biased t (raw .add x y)).toNat ↔ plain t .add x y = .trap .overflow := by
  rw [checkedAdd_overflow_iff ht hx hy, plain_trap_iff]
  constructor
  · intro h
    exact ⟨.overflow, h, rfl⟩
  · rintro ⟨e, he, hk⟩
    cases e with
    | overflow => exact he
    | divisionByZero => cases hk
    | invalidShift => cases hk

/-- Specialized `u8` checked-add arithmetic (zero bias, direct 32-bit unsigned sum). -/
theorem checkedAdd_u8_toNat {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y) :
    (wordBits x + wordBits y).toNat = (x + y).toNat := by
  rw [← wordBits_add]
  unfold wordBits
  rw [BitVec.toNat_ofInt]
  cases hx with
  | intro hxlo hxhi =>
    cases hy with
    | intro hylo hyhi =>
      simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
        ↓reduceIte, Nat.reducePow, Int.reducePow] at hxlo hxhi hylo hyhi ⊢
      omega

theorem checkedAdd_u8_lt_iff {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y) :
    (wordBits x + wordBits y).toNat < 256 ↔ checked .u8 .add x y = .ok (x + y) := by
  have h := checkedAdd_rangeTest_iff (t := .u8) rfl hx hy
  have hbias : biased .u8 (raw .add x y) = wordBits x + wordBits y := by
    unfold biased raw binaryOfWrapOp Source.binaryBits IntTy.min IntTy.signed
    exact BitVec.sub_zero _
  rw [hbias] at h
  exact h

theorem checkedAdd_u8_ge_iff {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y) :
    256 ≤ (wordBits x + wordBits y).toNat ↔ checked .u8 .add x y = .error .overflow := by
  have h := checkedAdd_overflow_iff (t := .u8) rfl hx hy
  have hbias : biased .u8 (raw .add x y) = wordBits x + wordBits y := by
    unfold biased raw binaryOfWrapOp Source.binaryBits IntTy.min IntTy.signed
    exact BitVec.sub_zero _
  rw [hbias] at h
  exact h

theorem checkedAdd_u8_flag_ult {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y) :
    ultBit (wordBits 255) (wordBits x + wordBits y) =
      boolBits (!decide (IntTy.u8.InRange (x + y))) := by
  have h := flag_ult_max (t := .u8) rfl .add hx hy
  have hbias : biased .u8 (raw .add x y) = wordBits x + wordBits y := by
    unfold biased raw binaryOfWrapOp Source.binaryBits IntTy.min IntTy.signed
    exact BitVec.sub_zero _
  rw [hbias] at h
  exact h

theorem checkedAdd_u8_flag_ule {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y) :
    uleBit (wordBits 256) (wordBits x + wordBits y) =
      boolBits (!decide (IntTy.u8.InRange (x + y))) := by
  have h := flag_ule (t := .u8) rfl .add hx hy
  have hbias : biased .u8 (raw .add x y) = wordBits x + wordBits y := by
    unfold biased raw binaryOfWrapOp Source.binaryBits IntTy.min IntTy.signed
    exact BitVec.sub_zero _
  rw [hbias] at h
  exact h

theorem checkedAdd_u8_wrap (x y : Int) :
    (wordBits x + wordBits y) &&& wordBits 255 = wordBits (wrap .u8 (x + y)) := by
  rw [← wordBits_add]
  exact wrap_word_u8 (x + y)

/-! ## Hosted return-word ABI encoding and decoding -/

/-- Return words strictly below `exitLimit` encode ordinary `u8` exit statuses. -/
def exitLimit : Nat := 256

/-- Number of distinct trap slots representable in a 32-bit return word alongside `0..255` exits. -/
def trapCapacity : Nat := 2 ^ 32 - exitLimit

/-- The 32-bit return word encoding an ordinary exit status. -/
def exitWordBits (status : Nat) : BitVec 32 := BitVec.ofNat 32 status

/-- The LCB source value encoding an ordinary exit status. -/
def exitWord (status : Nat) : Source.Value := .word (exitWordBits status)

/-- The 32-bit return word encoding a trap at table slot `slot`. -/
def trapWordBits (slot : Nat) : BitVec 32 := BitVec.ofNat 32 (exitLimit + slot)

/-- The LCB source value encoding a trap at table slot `slot`. -/
def trapWord (slot : Nat) : Source.Value := .word (trapWordBits slot)

/-- Pure ABI classification of a 32-bit LCB body return word, matching the runtime shim. -/
inductive DecodedReturnWord where
  | exit (status : UInt8)
  | trap (slot : Nat)
  deriving DecidableEq, Repr

/-- Decode a 32-bit body return word into either an exit byte or a trap-table slot index. -/
def decodeReturnWord (w : BitVec 32) : DecodedReturnWord :=
  if w.toNat < exitLimit then .exit (UInt8.ofNat w.toNat)
  else .trap (w.toNat - exitLimit)

theorem exitWordBits_toNat {n : Nat} (hn : n < exitLimit) :
    (exitWordBits n).toNat = n := by
  unfold exitWordBits
  rw [BitVec.toNat_ofNat]
  unfold exitLimit at hn
  exact Nat.mod_eq_of_lt (by omega)

theorem exitWordBits_lt {n : Nat} (hn : n < exitLimit) :
    (exitWordBits n).toNat < exitLimit := by
  rw [exitWordBits_toNat hn]
  exact hn

theorem exitWordBits_ofInt {n : Int} (hn : IntTy.u8.InRange n) :
    wordBits n = exitWordBits n.toNat := by
  cases hn with
  | intro hlo hhi =>
    simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
      ↓reduceIte, Int.reducePow] at hlo hhi
    unfold exitWordBits
    rw [← wordBits_ofNat]
    exact congrArg wordBits (Int.toNat_of_nonneg hlo).symm

theorem u8_wordBits_toNat {n : Int} (hn : IntTy.u8.InRange n) :
    (wordBits n).toNat = n.toNat := by
  rw [exitWordBits_ofInt hn]
  cases hn with
  | intro hlo hhi =>
    simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
      ↓reduceIte, Int.reducePow] at hlo hhi
    exact exitWordBits_toNat (by unfold exitLimit; omega)

theorem u8_wordBits_lt {n : Int} (hn : IntTy.u8.InRange n) :
    (wordBits n).toNat < exitLimit := by
  rw [u8_wordBits_toNat hn]
  cases hn with
  | intro hlo hhi =>
    simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
      ↓reduceIte, Int.reducePow] at hlo hhi
    unfold exitLimit
    omega

theorem decodeReturnWord_exit {n : Nat} (hn : n < exitLimit) :
    decodeReturnWord (exitWordBits n) = .exit (UInt8.ofNat n) := by
  unfold decodeReturnWord
  rw [exitWordBits_toNat hn, ite_eq_left hn]

theorem decodeReturnWord_u8 {n : Int} (hn : IntTy.u8.InRange n) :
    decodeReturnWord (wordBits n) = .exit (UInt8.ofNat n.toNat) := by
  rw [exitWordBits_ofInt hn]
  cases hn with
  | intro hlo hhi =>
    simp only [IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true,
      ↓reduceIte, Int.reducePow] at hlo hhi
    exact decodeReturnWord_exit (by unfold exitLimit; omega)

theorem exitWordBits_injective {a b : Nat} (ha : a < exitLimit) (hb : b < exitLimit)
    (heq : exitWordBits a = exitWordBits b) : a = b := by
  have h := congrArg BitVec.toNat heq
  rw [exitWordBits_toNat ha, exitWordBits_toNat hb] at h
  exact h

theorem exitWordBits_uint8 (b : UInt8) :
    decodeReturnWord (exitWordBits b.toNat) = .exit b := by
  have hlt : b.toNat < exitLimit := b.toBitVec.isLt
  rw [decodeReturnWord_exit hlt, UInt8.ofNat_toNat]

theorem trapWordBits_toNat {slot : Nat} (hs : slot < trapCapacity) :
    (trapWordBits slot).toNat = exitLimit + slot := by
  unfold trapWordBits
  rw [BitVec.toNat_ofNat]
  unfold trapCapacity exitLimit at hs
  unfold exitLimit
  exact Nat.mod_eq_of_lt (by omega)

theorem trapWordBits_not_lt {slot : Nat} (hs : slot < trapCapacity) :
    ¬ (trapWordBits slot).toNat < exitLimit := by
  rw [trapWordBits_toNat hs]
  omega

theorem trapWordBits_sub {slot : Nat} (hs : slot < trapCapacity) :
    (trapWordBits slot).toNat - exitLimit = slot := by
  rw [trapWordBits_toNat hs]
  exact Nat.add_sub_cancel_left exitLimit slot

theorem trapWordBits_ofInt (slot : Nat) :
    wordBits (256 + (slot : Int)) = trapWordBits slot := by
  rw [show (256 : Int) + (slot : Int) = ((exitLimit + slot : Nat) : Int) by unfold exitLimit; omega]
  exact wordBits_ofNat (exitLimit + slot)

theorem decodeReturnWord_trap {slot : Nat} (hs : slot < trapCapacity) :
    decodeReturnWord (trapWordBits slot) = .trap slot := by
  unfold decodeReturnWord
  rw [ite_eq_right (trapWordBits_not_lt hs), trapWordBits_sub hs]

theorem trapWordBits_injective {a b : Nat} (ha : a < trapCapacity) (hb : b < trapCapacity)
    (heq : trapWordBits a = trapWordBits b) : a = b := by
  have h := congrArg BitVec.toNat heq
  rw [trapWordBits_toNat ha, trapWordBits_toNat hb] at h
  omega

theorem exitWordBits_ne_trapWordBits {status slot : Nat}
    (hstatus : status < exitLimit) (hslot : slot < trapCapacity) :
    exitWordBits status ≠ trapWordBits slot := by
  intro heq
  have h := congrArg BitVec.toNat heq
  rw [exitWordBits_toNat hstatus, trapWordBits_toNat hslot] at h
  omega

/-- The largest admissible slot `trapCapacity - 1` maps to `2 ^ 32 - 1` and decodes cleanly. -/
theorem trapCapacity_max_decodes :
    (trapWordBits (trapCapacity - 1)).toNat = 2 ^ 32 - 1 ∧
      decodeReturnWord (trapWordBits (trapCapacity - 1)) = .trap (trapCapacity - 1) := by
  have hlt : trapCapacity - 1 < trapCapacity := by decide
  exact ⟨by rw [trapWordBits_toNat hlt]; decide, decodeReturnWord_trap hlt⟩

/-- Capacity tightness: slot `trapCapacity` wraps modulo `2 ^ 32` to exit word `0`. -/
theorem trapCapacity_wraps_to_zero : trapWordBits trapCapacity = exitWordBits 0 := by
  decide

/-- Every 32-bit return word decodes to an exit below 256 or a trap slot below `trapCapacity`
and round-trips to itself. -/
theorem decodeReturnWord_spec (w : BitVec 32) :
    (w.toNat < exitLimit ∧ decodeReturnWord w = .exit (UInt8.ofNat w.toNat) ∧
      exitWordBits w.toNat = w) ∨
    (exitLimit ≤ w.toNat ∧ w.toNat - exitLimit < trapCapacity ∧
      decodeReturnWord w = .trap (w.toNat - exitLimit) ∧
      trapWordBits (w.toNat - exitLimit) = w) := by
  if h : w.toNat < exitLimit then
    refine Or.inl ⟨h, by unfold decodeReturnWord; rw [ite_eq_left h], ?_⟩
    unfold exitWordBits
    exact BitVec.eq_of_toNat_eq (by rw [BitVec.toNat_ofNat, Nat.mod_eq_of_lt w.isLt])
  else
    have hge : exitLimit ≤ w.toNat := Nat.le_of_not_lt h
    have hw := w.isLt
    have hslot : w.toNat - exitLimit < trapCapacity := by unfold trapCapacity exitLimit at *; omega
    refine Or.inr ⟨hge, hslot, by unfold decodeReturnWord; rw [ite_eq_right h], ?_⟩
    unfold trapWordBits
    rw [Nat.add_sub_of_le hge]
    exact BitVec.eq_of_toNat_eq (by rw [BitVec.toNat_ofNat, Nat.mod_eq_of_lt hw])

/-- Runtime dispatch comparison (`cultw %w, 256`) and slot extraction (`sub %w, 256`). -/
theorem runtime_ult_exit {n : Nat} (hn : n < exitLimit) :
    ultBit (exitWordBits n) (BitVec.ofNat 32 exitLimit) = 1 := by
  unfold ultBit
  have hlt : (exitWordBits n).toNat < (BitVec.ofNat 32 exitLimit).toNat := by
    rw [exitWordBits_toNat hn]
    exact hn
  rw [ite_eq_left hlt]

theorem runtime_ult_trap {slot : Nat} (hs : slot < trapCapacity) :
    ultBit (trapWordBits slot) (BitVec.ofNat 32 exitLimit) = 0 := by
  unfold ultBit
  have hnot : ¬ (trapWordBits slot).toNat < (BitVec.ofNat 32 exitLimit).toNat := by
    rw [trapWordBits_toNat hs]
    change ¬ exitLimit + slot < exitLimit
    omega
  rw [ite_eq_right hnot]

theorem runtime_sub_trap {slot : Nat} (hs : slot < trapCapacity) :
    trapWordBits slot - BitVec.ofNat 32 exitLimit = BitVec.ofNat 32 slot := by
  apply BitVec.eq_of_toNat_eq
  rw [BitVec.toNat_sub, trapWordBits_toNat hs]
  change (2 ^ 32 - exitLimit + (exitLimit + slot)) % 2 ^ 32 = slot % 2 ^ 32
  unfold trapCapacity exitLimit at hs
  unfold exitLimit
  omega

theorem runtime_sub_trap_toNat {slot : Nat} (hs : slot < trapCapacity) :
    (trapWordBits slot - BitVec.ofNat 32 exitLimit).toNat = slot := by
  rw [runtime_sub_trap hs, BitVec.toNat_ofNat]
  unfold trapCapacity exitLimit at hs
  exact Nat.mod_eq_of_lt (by omega)

/-! ## Pure `LeanCompilerBackend.Verified.Source` step and expansion lemmas -/

/-- Construct a 32-bit LCB source instruction. -/
def wordIns (d : Source.ValueId) (op : Source.Operation) : Source.Instruction :=
  { destination := d, width := .word, operation := op }

/-- Four consecutive environment slot writes, matching a 4-instruction wrapping expansion. -/
def set4 (e : Source.Environment) (n : Nat) (u₀ u₁ u₂ v : Option Source.Value) :
    Source.Environment :=
  (((e.set! n u₀).set! (n + 1) u₁).set! (n + 2) u₂).set! (n + 3) v

theorem word_bne : (Width.word != Width.word) = false := rfl

theorem size_set! (e : Source.Environment) (m : Nat) (u : Option Source.Value) :
    (e.set! m u).size = e.size := by
  rw [Array.set!_eq_setIfInBounds, Array.size_setIfInBounds]

theorem size_set4 (e : Source.Environment) (n : Nat) (u₀ u₁ u₂ v : Option Source.Value) :
    (set4 e n u₀ u₁ u₂ v).size = e.size := by
  unfold set4
  rw [size_set!, size_set!, size_set!, size_set!]

theorem read_literal (env : Source.Environment) (v : BitVec 32) :
    Source.read env .word (.literal (.word v)) = .ok (.word v) := rfl

theorem read_word_literal (env : Source.Environment) (z : Int) :
    Source.read env .word (.literal (word z)) = .ok (word z) := rfl

theorem read_set_self {env : Source.Environment} {n : Nat} {v : Source.Value}
    (hs : n < env.size) (hv : v.width = .word) :
    Source.read (env.set! n (some v)) .word (.local n) = .ok v := by
  simp only [Source.read, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_self_of_lt hs,
    hv, word_bne, Bool.false_eq_true, ↓reduceIte, bind, Except.bind, pure, Except.pure]

theorem read_set_other {env : Source.Environment} {m k : Nat} {u : Option Source.Value}
    {w : Width} (h : m ≠ k) :
    Source.read (env.set! m u) w (.local k) = Source.read env w (.local k) := by
  simp only [Source.read, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne h]

theorem write_ok {f : Source.Function} {env : Source.Environment} {n : Nat}
    {val : Source.Value} (hw : f.valueWidths[n]? = some .word) (hs : n < env.size)
    (hv : val.width = .word) :
    Source.write f env n val = .ok (env.set! n (some val)) := by
  have hge : ¬ n ≥ env.size := Nat.not_le_of_lt hs
  simp only [Source.write, hw, hge, ↓reduceIte, hv, word_bne, Bool.false_eq_true,
    pure, Except.pure]

theorem eval_copy {env : Source.Environment} {d : Nat} {a : Source.Operand} {p : BitVec 32}
    (ha : Source.read env .word a = .ok (.word p)) :
    Source.evaluate env (wordIns d (.copy a)) = .ok (.word p) := by
  unfold Source.evaluate wordIns
  exact ha

theorem eval_bin {env : Source.Environment} {d : Nat} {op : Source.Binary}
    {a b : Source.Operand} {p q : BitVec 32}
    (ha : Source.read env .word a = .ok (.word p))
    (hb : Source.read env .word b = .ok (.word q)) :
    Source.evaluate env (wordIns d (.binary op a b)) = .ok (.word (Source.binaryBits op p q)) := by
  unfold Source.evaluate wordIns
  dsimp only
  rw [ha, hb]
  rfl

theorem eval_cmp {env : Source.Environment} {d : Nat} {op : Source.Comparison}
    {a b : Source.Operand} {p q : BitVec 32}
    (ha : Source.read env .word a = .ok (.word p))
    (hb : Source.read env .word b = .ok (.word q)) :
    Source.evaluate env (wordIns d (.compare op .word a b)) =
      Source.evaluateComparison op (.word p) (.word q) := by
  unfold Source.evaluate wordIns
  dsimp only
  rw [word_bne, ha, hb]
  rfl

theorem cmp_ult (p q : BitVec 32) :
    Source.evaluateComparison .unsignedLT (.word p) (.word q) = .ok (.word (ultBit p q)) := by
  unfold Source.evaluateComparison ultBit
  change (if Width.word != Width.word then _ else
    Except.ok (Source.Value.word (if decide (p.toNat < q.toNat) = true then 1 else 0))) = _
  rw [word_bne]
  if h : p.toNat < q.toNat then
    rw [ite_eq_left h, decide_eq_true h]; rfl
  else
    rw [ite_eq_right h, decide_eq_false h]; rfl

theorem cmp_ule (p q : BitVec 32) :
    Source.evaluateComparison .unsignedLE (.word p) (.word q) = .ok (.word (uleBit p q)) := by
  unfold Source.evaluateComparison uleBit
  change (if Width.word != Width.word then _ else
    Except.ok (Source.Value.word (if decide (p.toNat ≤ q.toNat) = true then 1 else 0))) = _
  rw [word_bne]
  if h : p.toNat ≤ q.toNat then
    rw [ite_eq_left h, decide_eq_true h]; rfl
  else
    rw [ite_eq_right h, decide_eq_false h]; rfl

theorem cmp_eq (p q : BitVec 32) :
    Source.evaluateComparison .eq (.word p) (.word q) = .ok (.word (eqBit p q)) := by
  unfold Source.evaluateComparison eqBit
  change (if Width.word != Width.word then _ else
    Except.ok (Source.Value.word (if (p.toNat == q.toNat) = true then 1 else 0))) = _
  rw [word_bne]
  if h : p.toNat = q.toNat then
    rw [ite_eq_left h, beq_iff_eq.mpr h]; rfl
  else
    rw [ite_eq_right h, beq_false_of_ne h]; rfl

theorem step_body {f : Source.Function} {s : Source.State} {lb : Source.Block}
    {i : Source.Instruction} {v : Source.Value} {env' : Source.Environment}
    (hb : f.blocks[s.block]? = some lb) (hi : lb.instructions[s.position]? = some i)
    (he : Source.evaluate s.environment i = .ok v)
    (hw : Source.write f s.environment i.destination v = .ok env') :
    Source.step f s = .ok (.next { s with position := s.position + 1, environment := env' }) := by
  have hp : s.position < lb.instructions.size := by
    rcases Nat.lt_or_ge s.position lb.instructions.size with h | h
    · exact h
    · rw [Array.getElem?_eq_none h] at hi; cases hi
  simp only [Source.step, hb, hp, ↓reduceIte, hi, he, hw, bind, Except.bind, pure, Except.pure]

theorem step_ret {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {a : Source.Operand} (ht : lb.terminator = .ret a) {v : Source.Value}
    (hr : Source.read s.environment f.resultWidth a = .ok v) :
    Source.step f s = .ok (.returned v) := by
  have hlt : ¬ s.position < lb.instructions.size := by omega
  simp only [Source.step, hb, hlt, ↓reduceIte, beq_iff_eq.mpr hp, ht, hr,
    bind, Except.bind, pure, Except.pure]

theorem step_jump {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {t : Nat} (ht : lb.terminator = .jump t) {s' : Source.State}
    (htr : Source.transfer f s.block t s.environment = .ok s') :
    Source.step f s = .ok (.next s') := by
  have hlt : ¬ s.position < lb.instructions.size := by omega
  simp only [Source.step, hb, hlt, ↓reduceIte, beq_iff_eq.mpr hp, ht, htr,
    Functor.map, Except.map]

theorem step_branch {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {c : Source.Operand} {y n : Nat} (ht : lb.terminator = .branch c y n) {cv : Source.Value}
    (hc : Source.read s.environment .word c = .ok cv) {s' : Source.State}
    (htr : Source.transfer f s.block (if cv.toNat != 0 then y else n) s.environment = .ok s') :
    Source.step f s = .ok (.next s') := by
  have hlt : ¬ s.position < lb.instructions.size := by omega
  simp only [Source.step, hb, hlt, ↓reduceIte, beq_iff_eq.mpr hp, ht, hc,
    bind, Except.bind, Functor.map, Except.map]
  rw [htr]

theorem steps_append {f : Source.Function} {m k : Nat} {s t u : Source.State} :
    Source.Steps f m s t → Source.Steps f k t u → Source.Steps f (k + m) s u
  | .zero _, h => h
  | .succ h₁ h₂, h => .succ h₁ (steps_append h₂ h)

theorem initial_no_params {f : Source.Function} {entry : Source.Block}
    (hp : f.parameters = #[]) (hb : f.blocks[0]? = some entry)
    (hphi : entry.phis.isEmpty = true) :
    Source.initial f #[] = .ok ⟨0, 0, Array.replicate f.valueWidths.size none⟩ := by
  unfold Source.initial
  rw [hp, hb]
  dsimp only
  rw [hphi]
  with_unfolding_all rfl

theorem transfer_no_phis {f : Source.Function} {pred target : Source.BlockId} {tb : Source.Block}
    {env : Source.Environment} (hb : f.blocks[target]? = some tb) (hphi : tb.phis = #[]) :
    Source.transfer f pred target env = .ok ⟨target, 0, env⟩ := by
  unfold Source.transfer
  rw [hb]
  dsimp only
  rw [hphi]
  rfl

theorem steps_prefix {f : Source.Function} :
    ∀ {k : Nat} {s u : Source.State}, Source.Steps f k s u →
      ∀ n, n ≤ k → ∃ t, Source.Steps f n s t
  | _, s, _, _, 0, _ => ⟨s, .zero _⟩
  | _, _, _, .succ h₁ h₂, n + 1, hn => by
    obtain ⟨t, ht⟩ := steps_prefix h₂ n (Nat.le_of_succ_le_succ hn)
    exact ⟨t, .succ h₁ ht⟩
  | _, _, _, .zero _, _ + 1, hn => False.elim (Nat.not_succ_le_zero _ hn)

theorem steps_split {f : Source.Function} :
    ∀ {n m : Nat} {s t u : Source.State}, Source.Steps f n s t →
      Source.Steps f (n + m) s u → Source.Steps f m t u
  | 0, _, _, _, _, .zero _, h => by rw [Nat.zero_add] at h; exact h
  | n + 1, m, _, _, _, .succ h₁ h₂, h => by
    rw [show n + 1 + m = (n + m) + 1 by omega] at h
    cases h with
    | succ h₁' h₂' =>
      cases Source.step_deterministic f _ _ _ h₁ h₁'
      exact steps_split h₂ h₂'

theorem returned_not_step {f : Source.Function} {t u : Source.State} {v : Source.Value}
    (hr : Source.step f t = .ok (.returned v)) (hs : Source.Step f t u) : False := by
  unfold Source.Step at hs
  rw [hr] at hs
  cases hs

private theorem returns_le {f : Source.Function} {s t₁ t₂ : Source.State} {v₁ v₂ : Source.Value}
    {n k : Nat} (h₁ : Source.Steps f n s t₁) (r₁ : Source.step f t₁ = .ok (.returned v₁))
    (h₂ : Source.Steps f (n + k) s t₂) (r₂ : Source.step f t₂ = .ok (.returned v₂)) :
    v₁ = v₂ := by
  have h := steps_split h₁ h₂
  cases k with
  | zero =>
    cases h
    rw [r₁] at r₂
    cases r₂
    rfl
  | succ k =>
    cases h with
    | succ hs _ => exact (returned_not_step r₁ hs).elim

theorem returns_unique {f : Source.Function} {s : Source.State} {v₁ v₂ : Source.Value}
    (h₁ : Source.Returns f s v₁) (h₂ : Source.Returns f s v₂) : v₁ = v₂ := by
  obtain ⟨n₁, t₁, s₁, r₁⟩ := h₁
  obtain ⟨n₂, t₂, s₂, r₂⟩ := h₂
  rcases Nat.le_total n₁ n₂ with h | h
  · obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le h
    exact returns_le s₁ r₁ s₂ r₂
  · obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le h
    exact (returns_le s₂ r₂ s₁ r₁).symm

theorem returns_not_diverges {f : Source.Function} {s : Source.State} {v : Source.Value}
    (hr : Source.Returns f s v) (hd : Source.Diverges f s) : False := by
  obtain ⟨n, t, hs, ht⟩ := hr
  obtain ⟨u, hu⟩ := hd (n + 1)
  cases steps_split hs hu with
  | succ h _ => exact returned_not_step ht h

theorem hasBehavior_unique {f : Source.Function} {args : Array Source.Value}
    {b₁ b₂ : Source.Behavior} (h₁ : Source.HasBehavior f args b₁)
    (h₂ : Source.HasBehavior f args b₂) : b₁ = b₂ := by
  obtain ⟨s₁, i₁, h₁⟩ := h₁
  obtain ⟨s₂, i₂, h₂⟩ := h₂
  rw [i₁] at i₂
  cases i₂
  cases b₁ with
  | returned v₁ =>
    cases b₂ with
    | returned v₂ => rw [returns_unique h₁ h₂]
    | diverges => exact (returns_not_diverges h₁ h₂).elim
  | diverges =>
    cases b₂ with
    | returned v₂ => exact (returns_not_diverges h₂ h₁).elim
    | diverges => rfl

/-- The 4-instruction wrapping expansion computes `word (Core.wrapping t op x y)` at `n + 3`. -/
def wrapExpansion (op : WrapOp) (t : IntTy) (n : Nat) (a' b' : Source.Operand) :
    List Source.Instruction :=
  [wordIns n (.binary (binaryOfWrapOp op) a' b'),
   wordIns (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
   wordIns (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
   wordIns (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))]

/-- The 6-instruction checked-operation expansion: wrapped value at `n + 3`, overflow flag at `n + 5`. -/
def checkedOpExpansion (op : WrapOp) (t : IntTy) (n : Nat) (a' b' : Source.Operand) :
    List Source.Instruction :=
  wrapExpansion op t n a' b' ++
  [wordIns (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
   wordIns (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))]

/-- Compact 3-instruction `u8` checked-add expansion: raw sum at `n`, wrapped byte at `n + 1`,
overflow flag at `n + 2`. -/
def checkedAddU8Expansion (n : Nat) (a' b' : Source.Operand) : List Source.Instruction :=
  [wordIns n (.binary .add a' b'),
   wordIns (n + 1) (.binary .andBits (.local n) (.literal (word 255))),
   wordIns (n + 2) (.compare .unsignedLT .word (.literal (word 255)) (.local n))]

theorem wrap_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {op : WrapOp} {t : IntTy} {a' b' : Source.Operand} {x y : Int}
    (ht : inWordProfile t = true) (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 4 → lb.instructions[s.position + k]? = (wrapExpansion op t n a' b')[k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 3 < s.environment.size) :
    Source.Steps f 4 s ⟨s.block, s.position + 4,
      set4 s.environment n
        (some (.word (raw op x y)))
        (some (.word (biased t (raw op x y))))
        (some (.word (masked t (biased t (raw op x y)))))
        (some (word (wrapping t op x y)))⟩ := by
  cases s with
  | mk blk pos env =>
  dsimp only at hb hq hA hB hW hs ⊢
  let p₀ := raw op x y
  let p₁ := biased t p₀
  let p₂ := masked t p₁
  let p₃ := Source.binaryBits .add p₂ (wordBits t.min)
  let e₁ := env.set! n (some (.word p₀))
  let e₂ := e₁.set! (n + 1) (some (.word p₁))
  let e₃ := e₂.set! (n + 2) (some (.word p₂))
  have z₁ : e₁.size = env.size := size_set! ..
  have z₂ : e₂.size = env.size := (size_set! ..).trans z₁
  have z₃ : e₃.size = env.size := (size_set! ..).trans z₂
  have hv₃ : word (wrapping t op x y) = .word p₃ := by
    have hwrap := wrapping_word ht op x y
    exact congrArg Source.Value.word hwrap.symm
  show Source.Steps f 4 ⟨blk, pos, env⟩ ⟨blk, pos + 4, e₃.set! (n + 3) (some (word _))⟩
  rw [hv₃]
  have s₀ := step_body (s := ⟨blk, pos, env⟩) hb
    (by have := hq 0 (by omega); rwa [Nat.add_zero] at this)
    (eval_bin (d := n) (op := binaryOfWrapOp op) hA hB)
    (write_ok (n := n) (hW n (by omega)) (show n < env.size by omega) rfl)
  have s₁ := step_body (s := ⟨blk, pos + 1, e₁⟩) hb (hq 1 (by omega))
    (eval_bin (d := n + 1) (op := .sub) (read_set_self (by omega) rfl) (read_word_literal _ t.min))
    (write_ok (n := n + 1) (hW (n + 1) (by omega)) (show n + 1 < e₁.size by rw [z₁]; omega) rfl)
  have s₂ := step_body (s := ⟨blk, pos + 1 + 1, e₂⟩) hb
    (by have := hq 2 (by omega); rwa [show pos + 2 = pos + 1 + 1 by omega] at this)
    (eval_bin (d := n + 2) (op := .andBits) (read_set_self (by rw [z₁]; omega) rfl)
      (read_word_literal _ (2 ^ t.bits - 1)))
    (write_ok (n := n + 2) (hW (n + 2) (by omega)) (show n + 2 < e₂.size by rw [z₂]; omega) rfl)
  have s₃ := step_body (s := ⟨blk, pos + 1 + 1 + 1, e₃⟩) hb
    (by have := hq 3 (by omega); rwa [show pos + 3 = pos + 1 + 1 + 1 by omega] at this)
    (eval_bin (d := n + 3) (op := .add) (read_set_self (by rw [z₂]; omega) rfl)
      (read_word_literal _ t.min))
    (write_ok (n := n + 3) (hW (n + 3) (by omega)) (show n + 3 < e₃.size by rw [z₃]; omega) rfl)
  exact .succ s₀ (.succ s₁ (.succ s₂ (.succ s₃ (.zero _))))

theorem flag_steps {f : Source.Function} {lb : Source.Block} {blk : Source.BlockId}
    {pos n : Nat} {E : Source.Environment} {p Q : BitVec 32}
    (hb : f.blocks[blk]? = some lb)
    (hi₄ : lb.instructions[pos]? =
      some (wordIns (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (.word Q)))))
    (hi₅ : lb.instructions[pos + 1]? =
      some (wordIns (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))))
    (r₁ : Source.read E .word (.local (n + 1)) = .ok (.word p))
    (hW₄ : f.valueWidths[n + 4]? = some .word) (hW₅ : f.valueWidths[n + 5]? = some .word)
    (hs : n + 5 < E.size) :
    Source.Steps f 2 ⟨blk, pos, E⟩ ⟨blk, pos + 1 + 1,
      (E.set! (n + 4) (some (.word (ultBit p Q)))).set! (n + 5)
        (some (.word (eqBit (ultBit p Q) (wordBits 0))))⟩ := by
  have s₄ := step_body (s := ⟨blk, pos, E⟩) hb hi₄
    ((eval_cmp (d := n + 4) r₁ (read_literal E Q)).trans (cmp_ult p Q))
    (write_ok (n := n + 4) hW₄ (show n + 4 < E.size by omega) rfl)
  have r₄ := read_set_self (env := E) (n := n + 4) (v := .word (ultBit p Q)) (by omega) rfl
  have s₅ := step_body (s := ⟨blk, pos + 1, E.set! (n + 4) (some (.word (ultBit p Q)))⟩) hb hi₅
    ((eval_cmp (d := n + 5) r₄ (read_word_literal _ 0)).trans (cmp_eq _ _))
    (write_ok (n := n + 5) hW₅ (show n + 5 < (E.set! _ _).size by rw [size_set!]; omega) rfl)
  exact .succ s₄ (.succ s₅ (.zero _))

theorem checkedOp_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {op : WrapOp} {t : IntTy} {a' b' : Source.Operand} {x y : Int}
    (ht : inWordProfile t = true) (hx : t.InRange x) (hy : t.InRange y)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 6 → lb.instructions[s.position + k]? = (checkedOpExpansion op t n a' b')[k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 5 < s.environment.size) :
    ∃ u₄, Source.Steps f 6 s ⟨s.block, s.position + 6,
      ((set4 s.environment n
        (some (.word (raw op x y)))
        (some (.word (biased t (raw op x y))))
        (some (.word (masked t (biased t (raw op x y)))))
        (some (word (wrapping t op x y)))).set! (n + 4) u₄).set!
        (n + 5) (some (boolWord (!decide (t.InRange (op.exact x y)))))⟩ := by
  cases s with
  | mk blk pos env =>
  dsimp only at hb hq hA hB hW hs ⊢
  let E := set4 env n
    (some (.word (raw op x y)))
    (some (.word (biased t (raw op x y))))
    (some (.word (masked t (biased t (raw op x y)))))
    (some (word (wrapping t op x y)))
  have h4 : Source.Steps f 4 ⟨blk, pos, env⟩ ⟨blk, pos + 4, E⟩ :=
    wrap_steps (s := ⟨blk, pos, env⟩) ht hb (fun k hk => by
      rw [hq k (by omega)]
      rcases k with _ | _ | _ | _ | k <;> first | rfl | omega)
      hA hB hW (show n + 3 < env.size by omega)
  have zE : E.size = env.size := size_set4 ..
  have r₁ : Source.read E .word (.local (n + 1)) = .ok (.word (biased t (raw op x y))) := by
    dsimp only [E, set4]
    rw [read_set_other (by omega), read_set_other (by omega)]
    exact read_set_self (by show n + 1 < (env.set! _ _).size; rw [size_set!]; omega) rfl
  have h2 := flag_steps (E := E) (pos := pos + 4) (Q := wordBits (2 ^ t.bits)) hb
    (hq 4 (by omega))
    (by have := hq 5 (by omega); rwa [show pos + 5 = pos + 4 + 1 by omega] at this)
    r₁ (hW (n + 4) (by omega)) (hW (n + 5) (by omega)) (by rw [zE]; omega)
  refine ⟨some (.word (ultBit (biased t (raw op x y)) (wordBits (2 ^ t.bits)))), ?_⟩
  have hflag : boolWord (!decide (t.InRange (op.exact x y))) =
      .word (eqBit (ultBit (biased t (raw op x y)) (wordBits (2 ^ t.bits))) (wordBits 0)) :=
    congrArg Source.Value.word (flag_ult_eq ht op hx hy).symm
  rw [hflag, show pos + 6 = pos + 4 + 1 + 1 from rfl]
  exact steps_append h4 h2

/-- Checked-add specialization of `checkedOp_steps`: when `checked t .add x y = .ok (x + y)` the
result slot holds `word (x + y)` and the flag holds `boolWord false`; when it overflows the result
slot holds `word (wrap t (x + y))` and the flag holds `boolWord true`. -/
theorem checkedAdd_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {t : IntTy} {a' b' : Source.Operand} {x y : Int}
    (ht : inWordProfile t = true) (hx : t.InRange x) (hy : t.InRange y)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 6 → lb.instructions[s.position + k]? = (checkedOpExpansion .add t n a' b')[k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 5 < s.environment.size) :
    ∃ u₄, Source.Steps f 6 s ⟨s.block, s.position + 6,
      ((set4 s.environment n
        (some (.word (raw .add x y)))
        (some (.word (biased t (raw .add x y))))
        (some (.word (masked t (biased t (raw .add x y)))))
        (some (word (wrap t (x + y))))).set! (n + 4) u₄).set!
        (n + 5) (some (boolWord (decide (checked t .add x y = .error .overflow))))⟩ := by
  obtain ⟨u₄, hsteps⟩ := checkedOp_steps (op := .add) ht hx hy hb hq hA hB hW hs
  have hdec : (!decide (t.InRange (x + y))) = decide (checked t .add x y = .error .overflow) := by
    if hin : t.InRange (x + y) then
      have hne : checked t .add x y ≠ .error .overflow := by
        dsimp only [checked, inRangeOr]; rw [ite_eq_left hin]; intro h; cases h
      rw [decide_eq_true hin, decide_eq_false hne]; rfl
    else
      have heq : checked t .add x y = .error .overflow := by
        dsimp only [checked, inRangeOr]; rw [ite_eq_right hin]
      rw [decide_eq_false hin, decide_eq_true heq]; rfl
  refine ⟨u₄, ?_⟩
  rw [← hdec]
  exact hsteps

/-- Compact 3-step `u8` checked-add execution: wrapped byte at `n + 1` and overflow flag at `n + 2`. -/
theorem checkedAddU8_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {a' b' : Source.Operand} {x y : Int}
    (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 3 → lb.instructions[s.position + k]? = (checkedAddU8Expansion n a' b')[k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 2 < s.environment.size) :
    Source.Steps f 3 s ⟨s.block, s.position + 3,
      (((s.environment.set! n (some (.word (wordBits x + wordBits y)))).set!
        (n + 1) (some (word (wrap .u8 (x + y))))).set!
        (n + 2) (some (boolWord (decide (checked .u8 .add x y = .error .overflow)))))⟩ := by
  cases s with
  | mk blk pos env =>
  dsimp only at hb hq hA hB hW hs ⊢
  let p₀ := wordBits x + wordBits y
  let e₁ := env.set! n (some (.word p₀))
  let e₂ := e₁.set! (n + 1) (some (word (wrap .u8 (x + y))))
  have z₁ : e₁.size = env.size := size_set! ..
  have z₂ : e₂.size = env.size := (size_set! ..).trans z₁
  have hmask : Source.binaryBits .andBits p₀ (wordBits 255) = wordBits (wrap .u8 (x + y)) :=
    checkedAdd_u8_wrap x y
  have hdec : (!decide (IntTy.u8.InRange (x + y))) =
      decide (checked .u8 .add x y = .error .overflow) := by
    if hin : IntTy.u8.InRange (x + y) then
      have hne : checked .u8 .add x y ≠ .error .overflow := by
        dsimp only [checked, inRangeOr]; rw [ite_eq_left hin]; intro h; cases h
      rw [decide_eq_true hin, decide_eq_false hne]; rfl
    else
      have heq : checked .u8 .add x y = .error .overflow := by
        dsimp only [checked, inRangeOr]; rw [ite_eq_right hin]
      rw [decide_eq_false hin, decide_eq_true heq]; rfl
  have hflag : ultBit (wordBits 255) p₀ =
      boolBits (decide (checked .u8 .add x y = .error .overflow)) := by
    rw [← hdec]
    exact checkedAdd_u8_flag_ult hx hy
  have s₀ := step_body (s := ⟨blk, pos, env⟩) hb
    (by have := hq 0 (by omega); rwa [Nat.add_zero] at this)
    (eval_bin (d := n) (op := .add) hA hB)
    (write_ok (n := n) (hW n (by omega)) (show n < env.size by omega) rfl)
  have s₁ := step_body (s := ⟨blk, pos + 1, e₁⟩) hb (hq 1 (by omega))
    (by
      have he := eval_bin (d := n + 1) (op := .andBits)
        (read_set_self (env := env) (n := n) (v := .word p₀) (by omega) rfl)
        (read_word_literal e₁ 255)
      rw [hmask] at he
      exact he)
    (write_ok (n := n + 1) (hW (n + 1) (by omega)) (show n + 1 < e₁.size by rw [z₁]; omega) rfl)
  have r₀ : Source.read e₂ .word (.local n) = .ok (.word p₀) := by
    dsimp only [e₂, e₁]
    rw [read_set_other (by omega)]
    exact read_set_self (by omega) rfl
  have s₂ := step_body (s := ⟨blk, pos + 1 + 1, e₂⟩) hb
    (by have := hq 2 (by omega); rwa [show pos + 2 = pos + 1 + 1 by omega] at this)
    (by
      have he := (eval_cmp (d := n + 2) (read_word_literal e₂ 255) r₀).trans (cmp_ult _ _)
      rw [hflag] at he
      exact he)
    (write_ok (n := n + 2) (hW (n + 2) (by omega)) (show n + 2 < e₂.size by rw [z₂]; omega) rfl)
  exact .succ s₀ (.succ s₁ (.succ s₂ (.zero _)))

end Concrete.Backend.LCBWordLemmas
