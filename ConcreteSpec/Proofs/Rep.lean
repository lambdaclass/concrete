import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax

/-!
# Part 2: `rep_roundtrip`, decoding an encoded value gives the value back.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Rep
open ConcreteSpec

/-- Helper: byte-width computation used by the representation round trip. -/
theorem encodeNat_length : ∀ (w n : Nat), (encodeNat w n).length = w
  | 0, _ => rfl
  | w + 1, n => by simp [encodeNat, encodeNat_length w (n / 256)]

/-- Helper: modular byte codec identity used by the representation round trip. -/
theorem decodeNat_encodeNat : ∀ (w n : Nat), decodeNat (encodeNat w n) = some (n % 256 ^ w)
  | 0, n => by simp [encodeNat, decodeNat, Nat.mod_one]
  | w + 1, n => by
    simp only [encodeNat, decodeNat, decodeNat_encodeNat w (n / 256), Option.map_some,
      Option.some.injEq]
    rw [Nat.pow_succ, Nat.mul_comm (256 ^ w) 256, Nat.mod_mul]

/-- Helper: the integer round trip, for every integer type: the bit pattern of an in-range value decodes
to the value. -/
theorem int_roundtrip (t : IntTy) (n : Int) (h : t.inRange n = true) :
    ofBits t ((n % (2 : Int) ^ t.bits).toNat % 256 ^ (t.bits / 8)) = n := by
  have hpos : (0 : Int) < (2 : Int) ^ t.bits := by
    cases t <;> decide
  have hnn : 0 ≤ n % (2 : Int) ^ t.bits := Int.emod_nonneg _ (by omega)
  have hlt : n % (2 : Int) ^ t.bits < (2 : Int) ^ t.bits := Int.emod_lt_of_pos _ hpos
  have hpow : ((256 ^ (t.bits / 8) : Nat) : Int) = (2 : Int) ^ t.bits := by
    cases t <;> decide
  have hsmall : (n % (2 : Int) ^ t.bits).toNat < 256 ^ (t.bits / 8) := by
    have : (((n % (2 : Int) ^ t.bits).toNat : Nat) : Int) < ((256 ^ (t.bits / 8) : Nat) : Int) := by
      rw [Int.toNat_of_nonneg hnn, hpow]; exact hlt
    exact Int.ofNat_lt.mp this
  rw [Nat.mod_eq_of_lt hsmall]
  unfold ofBits
  have hm := Int.toNat_of_nonneg hnn
  generalize (n % (2 : Int) ^ t.bits).toNat = M at hm ⊢
  simp only [IntTy.inRange, decide_eq_true_eq] at h
  split
  · rename_i hc
    cases t <;>
      simp only [IntTy.signed, IntTy.min, IntTy.max, IntTy.bits, Nat.reducePow, Int.reducePow,
        Nat.reduceSub, ↓reduceIte, Bool.false_eq_true, false_and, true_and] at hc h hm ⊢ <;>
      omega
  · rename_i hc
    cases t <;>
      simp only [IntTy.signed, IntTy.min, IntTy.max, IntTy.bits, Nat.reducePow, Int.reducePow,
        Nat.reduceSub, ↓reduceIte, Bool.false_eq_true, false_and, true_and, Nat.not_le] at hc h hm ⊢ <;>
      omega

/-- Law: **`rep_roundtrip`** (Q9.14, Q3.11). For every slice-1 type this part covers (`u8`…`u64`,
`i8`…`i64`, `bool` and a struct with no fields), encoding a well-typed value gives exactly the
type's size in bytes, and decoding those bytes gives the value back. -/
theorem rep_roundtrip {Δ : Decls} {v : Val} {t : Ty} (h : RepVal Δ v t) :
    Ty.size Δ t = some (encodeVal v).length ∧ decodeVal Δ t (encodeVal v) = some v := by
  cases h with
  | empty hd hf => simp [Ty.size, encodeVal, decodeVal, hd, hf]
  | scalar hs =>
    cases hs with
    | bool =>
      rename_i b
      cases b <;> simp [Ty.size, scalarSize, encodeVal, encodeScalar, decodeVal, decodeScalar]
    | int hin =>
      rename_i t n
      refine ⟨by simp [Ty.size, scalarSize, encodeVal, encodeScalar, encodeNat_length], ?_⟩
      simp only [decodeVal, encodeVal, encodeScalar, decodeScalar, encodeNat_length,
        ↓reduceIte, decodeNat_encodeNat, Option.map_some, int_roundtrip t n hin]

end ConcreteSpec.Rep
