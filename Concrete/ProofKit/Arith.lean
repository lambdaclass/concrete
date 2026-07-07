import Concrete.Proof.Proof
import Std.Tactic.BVDecide

/-!
# Concrete Proof Kit — Arithmetic bridges

Domain-agnostic `Int` / `Nat` / `BitVec` arithmetic primitives that recur in
refinement proofs: width conversions, byte masking, sign-bit facts, and the
signed-division bridge (`sdiv` over non-negative operands collapses to `Nat`
division). Harvested from the HMAC-SHA256 flagship so future proofs over
fixed-width machine integers — padding/block-count arithmetic, byte/word packing
— do not re-derive them as one-off lemmas.

All lemmas are kernel-checked (`bv_decide` replays a certificate; `omega`/`simp`
are kernel tactics). Nothing here enters the trusted base. Lemmas live in the
`Concrete.Proof` namespace alongside the rest of the kit.
-/

namespace Concrete.Proof

/-! ## Width conversions -/

/-- A `Nat` below `2^32` widened to 64 bits equals its 32-bit form set-width to 64. -/
theorem ofNat64_eq_setWidth32 (n : Nat) (h : n < 2^32) :
    BitVec.ofNat 64 n = (BitVec.ofNat 32 n).setWidth 64 := by
  apply BitVec.toNat_inj.mp
  simp only [BitVec.toNat_ofNat, BitVec.toNat_setWidth]
  omega

/-! ## Sign bit -/

/-- A `Nat` below `2^31` has a clear sign bit when taken as a 32-bit value. The
    keystone for treating an unsigned-range value's `sdiv` as `udiv`. -/
theorem ofNat32_msb_false (a : Nat) (ha : a < 2^31) : (BitVec.ofNat 32 a).msb = false := by
  rw [BitVec.msb_eq_false_iff_two_mul_lt, BitVec.toNat_ofNat, Nat.mod_eq_of_lt (by omega)]; omega

/-! ## Byte masking -/

/-- Masking a 32-bit word with `0xff` keeps exactly its low byte. The byte-packing
    primitive behind big-endian state/length serialization. -/
theorem and255_lo (y : BitVec 32) :
    (y &&& BitVec.ofInt 32 255).toNat = (BitVec.setWidth 8 y).toNat := by
  have h : y &&& BitVec.ofInt 32 255 = (BitVec.setWidth 8 y).setWidth 32 := by bv_decide
  rw [h, BitVec.toNat_setWidth,
    Nat.mod_eq_of_lt (Nat.lt_of_lt_of_le (BitVec.setWidth 8 y).isLt (by decide))]

/-! ## Signed-division bridge

The recurring obligation behind length/block-count arithmetic: a machine `sdiv`
over two non-negative, unsigned-range operands is exactly `Nat` division. Proving
this once (rather than per flagship) is the point of the bridge library — it is
the case `range_block_count` would otherwise reach for an SMT solver. -/

/-- `sdiv` of two `Nat`s, both below `2^31`, agrees with `Nat` division. -/
theorem sdiv_ofNat_eq_natDiv (a b : Nat) (ha : a < 2^31) (hb : b < 2^31) :
    ((BitVec.ofInt 32 (a : Int)).sdiv (BitVec.ofInt 32 (b : Int))).toInt
      = ((a / b : Nat) : Int) := by
  have hself : a / b ≤ a := Nat.div_le_self a b
  rw [BitVec.ofInt_natCast, BitVec.ofInt_natCast, BitVec.sdiv_eq,
      ofNat32_msb_false a ha, ofNat32_msb_false b hb, BitVec.udiv_eq]
  rw [BitVec.toInt_eq_toNat_of_lt (by
        rw [BitVec.toNat_udiv, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
            Nat.mod_eq_of_lt (show a < 2^32 by omega), Nat.mod_eq_of_lt (show b < 2^32 by omega)]
        omega)]
  rw [BitVec.toNat_udiv, BitVec.toNat_ofNat, BitVec.toNat_ofNat,
      Nat.mod_eq_of_lt (show a < 2^32 by omega), Nat.mod_eq_of_lt (show b < 2^32 by omega)]

/-! ## Sanity checks — the bridge library kernel-checks on concrete values

These pin that a future edit to the lemmas keeps the arithmetic facts true, and
double as the smallest "the library computes what it claims" regression. -/

example : ofNat32_msb_false 0 (by decide) = ofNat32_msb_false 0 (by decide) := rfl
-- HMAC-shaped block count: ⌈(len+9)/64⌉ via the sdiv bridge, len = 312 → 6 blocks.
example : ((BitVec.ofInt 32 ((312 : Nat) + 72 : Nat)).sdiv (BitVec.ofInt 32 (64 : Nat))).toInt
            = (((312 + 72) / 64 : Nat) : Int) :=
  sdiv_ofNat_eq_natDiv (312 + 72) 64 (by omega) (by omega)

end Concrete.Proof
