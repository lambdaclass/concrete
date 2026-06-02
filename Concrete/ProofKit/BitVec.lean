import Concrete.Proof

/-!
# Concrete Proof Kit — BitVec / Int / Nat bridges

The round-trip and indexing lemmas that let `bv_decide` close a goal after the
interpreter has reconstructed a value through `Int`/`Nat`/`BitVec` casts.
Reusable by any proof over fixed-width machine integers. Extracted from
`Concrete.Sha256Refine` (Proof Kit v1).
-/

namespace Concrete.Proof

/-- `eval.lookupIndex` agrees with `List.get?`. -/
theorem lookupIndex_eq (l : List PVal) (m : Nat) : eval.lookupIndex l m = l[m]? := by
  induction l generalizing m with
  | nil => simp [eval.lookupIndex]
  | cons x xs ih => cases m with
    | zero => simp [eval.lookupIndex]
    | succ n => simp [eval.lookupIndex, ih]

/-- Indexing a range-map at an in-bounds position returns the mapped value. -/
theorem lookupIndex_range_map (f : Nat → PVal) (n m : Nat) (h : m < n) :
    eval.lookupIndex ((List.range n).map f) m = some (f m) := by
  rw [lookupIndex_eq]; simp [h]

/-- A `u8` widened to `u32` via `Int.ofNat`. -/
theorem ofInt32_byte (B : BitVec 8) :
    BitVec.ofInt 32 (Int.ofNat B.toNat) = B.setWidth 32 := by
  apply BitVec.eq_of_toNat_eq; simp [BitVec.toNat_setWidth]

/-- A `u8` widened to `u32` via `Nat.cast`. -/
theorem ofInt32_byte_cast (B : BitVec 8) :
    BitVec.ofInt 32 (B.toNat : Int) = B.setWidth 32 := by
  apply BitVec.eq_of_toNat_eq; simp [BitVec.toNat_setWidth]

/-- Round-trip through `Nat.cast` (how `eval` binds a `u32` argument). -/
theorem ofInt_natCast_toNat (W : BitVec 32) :
    BitVec.ofInt 32 (W.toNat : Int) = W := by
  apply BitVec.eq_of_toNat_eq; simp

/-- Round-trip through `Int.ofNat` (how `evalBinOp` reconstructs a u32
    bitwise result). -/
theorem ofInt_ofNat_toNat (W : BitVec 32) :
    BitVec.ofInt 32 (Int.ofNat W.toNat) = W := by
  apply BitVec.eq_of_toNat_eq; simp

/-- Round-trip through `Nat.cast` at width 8. -/
theorem ofInt8_natCast_toNat (b : BitVec 8) : BitVec.ofInt 8 (↑b.toNat) = b := by
  rw [BitVec.ofInt_natCast, BitVec.ofNat_toNat, BitVec.setWidth_eq]

end Concrete.Proof
