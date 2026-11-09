import ConcreteSpec.Proofs.Slice1

/-!
# Part 2: `rep_roundtrip`, decoding an encoded value gives the value back.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Rep
open ConcreteSpec

/-- Helper (computation). Induction computes the width of the radix-256 expansion used by the representation proof. (approved: REV-base1c-agy-01025713b) -/
theorem encodeNat_length : ∀ (w n : Nat), (encodeNat w n).length = w
  | 0, _ => rfl
  | w + 1, n => by simp [encodeNat, encodeNat_length w (n / 256)]

/-- Law (subject `ConcreteSpec.Rep.encodeNat`, `ConcreteSpec.Rep.decodeNat`). Little-endian encoding and decoding reconstruct the residue at the specified width. -/
theorem decodeNat_encodeNat : ∀ (w n : Nat), decodeNat (encodeNat w n) = some (n % 256 ^ w)
  | 0, n => by simp [encodeNat, decodeNat, Nat.mod_one]
  | w + 1, n => by
    simp only [encodeNat, decodeNat, decodeNat_encodeNat w (n / 256), Option.map_some,
      Option.some.injEq]
    rw [Nat.pow_succ, Nat.mul_comm (256 ^ w) 256, Nat.mod_mul]

/-- Law (subject `ConcreteSpec.Rep.ofBits`). Decoding an in-range integer bit pattern recovers the signed or unsigned value. -/
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

/-- Law (subject `ConcreteSpec.Rep.encodeVal`, `ConcreteSpec.Rep.decodeVal`, `ConcreteSpec.Rep.Ty.size`).
**Representation round trip** (Q9.14, Q3.11). For every slice-1 type this part covers (unsigned and signed integers,
booleans and a struct with no fields), encoding a well-typed value gives exactly the
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

namespace RoundtripEvidence

/-- A broken encoder drops the Boolean byte. -/
def dropBytes : Val → List MByte := fun _ => []

/-- A broken encoder inserts an undefined byte even for an empty structure. -/
def spuriousByte : Val → List MByte := fun _ => [.undef]

/-- A broken decoder refuses initialized, correctly sized Boolean encodings. -/
def rejectValue : Decls → Ty → List MByte → Option Val := fun _ _ _ => none

/-- A broken layout refuses the size of a scalar. -/
def missingSize : Decls → Ty → Option Nat := fun _ _ => none

/-- Instance (law `ConcreteSpec.Rep.rep_roundtrip`, case `boolean`). -/
example : Ty.size [] (.scalar .bool) = some (encodeVal (.scalar (.bool true))).length ∧
    decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool true))) = some (.scalar (.bool true)) :=
  @rep_roundtrip [] (.scalar (.bool true)) (.scalar .bool) (.scalar .bool)

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `boolean`, subject `ConcreteSpec.Rep.encodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.dropBytes`). -/
example : RepVal [] (.scalar (.bool true)) (.scalar .bool) ∧
    ¬ (Ty.size [] (.scalar .bool) = some (dropBytes (.scalar (.bool true))).length ∧
      decodeVal [] (.scalar .bool) (dropBytes (.scalar (.bool true))) = some (.scalar (.bool true))) :=
  ⟨.scalar .bool, by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `boolean`, subject `ConcreteSpec.Rep.decodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.rejectValue`). -/
example : RepVal [] (.scalar (.bool true)) (.scalar .bool) ∧
    ¬ (Ty.size [] (.scalar .bool) = some (encodeVal (.scalar (.bool true))).length ∧
      rejectValue [] (.scalar .bool) (encodeVal (.scalar (.bool true))) = some (.scalar (.bool true))) :=
  ⟨.scalar .bool, by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `boolean`, subject `ConcreteSpec.Rep.Ty.size`, mutant `ConcreteSpec.Rep.RoundtripEvidence.missingSize`). -/
example : RepVal [] (.scalar (.bool true)) (.scalar .bool) ∧
    ¬ (missingSize [] (.scalar .bool) = some (encodeVal (.scalar (.bool true))).length ∧
      decodeVal [] (.scalar .bool) (encodeVal (.scalar (.bool true))) = some (.scalar (.bool true))) :=
  ⟨.scalar .bool, by decide⟩

def zeroEncodeNat : Nat → Nat → List MByte := fun w _ => encodeNat w 0
def noDecodeNat : List MByte → Option Nat := fun _ => none
def unsignedOnlyOfBits : IntTy → Nat → Int := fun _ n => n

/-- Instance (law `ConcreteSpec.Rep.decodeNat_encodeNat`, case `twoBytes`). -/
example := decodeNat_encodeNat 2 65534

/-- Planted (law `ConcreteSpec.Rep.decodeNat_encodeNat`, case `twoBytes`, subject `ConcreteSpec.Rep.encodeNat`, mutant `ConcreteSpec.Rep.RoundtripEvidence.zeroEncodeNat`). -/
example : ¬ (decodeNat (zeroEncodeNat 2 65534) = some (65534 % 256 ^ 2)) := by decide

/-- Planted (law `ConcreteSpec.Rep.decodeNat_encodeNat`, case `twoBytes`, subject `ConcreteSpec.Rep.decodeNat`, mutant `ConcreteSpec.Rep.RoundtripEvidence.noDecodeNat`). -/
example : ¬ (noDecodeNat (encodeNat 2 65534) = some (65534 % 256 ^ 2)) := by decide

/-- Instance (law `ConcreteSpec.Rep.int_roundtrip`, case `negativeSigned`). -/
example := int_roundtrip .i16 (-2) (by decide)

/-- Planted (law `ConcreteSpec.Rep.int_roundtrip`, case `negativeSigned`, subject `ConcreteSpec.Rep.ofBits`, mutant `ConcreteSpec.Rep.RoundtripEvidence.unsignedOnlyOfBits`). -/
example : IntTy.inRange .i16 (-2) = true ∧
    ¬ (unsignedOnlyOfBits .i16 (((-2 : Int) % (2 : Int) ^ IntTy.bits .i16).toNat %
      256 ^ (IntTy.bits .i16 / 8)) = -2) := by decide

/-- Instance (law `ConcreteSpec.Rep.rep_roundtrip`, case `negativeSigned`). -/
example : Ty.size [] (.scalar (.int .i16)) = some (encodeVal (.scalar (.int .i16 (-2)))).length ∧ decodeVal [] (.scalar (.int .i16)) (encodeVal (.scalar (.int .i16 (-2)))) = some (.scalar (.int .i16 (-2))) := @rep_roundtrip [] (.scalar (.int .i16 (-2))) (.scalar (.int .i16)) (.scalar (.int (by decide)))

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `negativeSigned`, subject `ConcreteSpec.Rep.decodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.rejectValue`). -/
example : RepVal [] (.scalar (.int .i16 (-2))) (.scalar (.int .i16)) ∧ ¬ (Ty.size [] (.scalar (.int .i16)) = some (encodeVal (.scalar (.int .i16 (-2)))).length ∧ rejectValue [] (.scalar (.int .i16)) (encodeVal (.scalar (.int .i16 (-2)))) = some (.scalar (.int .i16 (-2)))) := ⟨(.scalar (.int (by decide))), by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `negativeSigned`, subject `ConcreteSpec.Rep.Ty.size`, mutant `ConcreteSpec.Rep.RoundtripEvidence.missingSize`). -/
example : RepVal [] (.scalar (.int .i16 (-2))) (.scalar (.int .i16)) ∧ ¬ (missingSize [] (.scalar (.int .i16)) = some (encodeVal (.scalar (.int .i16 (-2)))).length ∧ decodeVal [] (.scalar (.int .i16)) (encodeVal (.scalar (.int .i16 (-2)))) = some (.scalar (.int .i16 (-2)))) := ⟨(.scalar (.int (by decide))), by decide⟩

/-- Instance (law `ConcreteSpec.Rep.rep_roundtrip`, case `emptyStruct`). -/
example : Ty.size [⟨"Token", false, []⟩] (.struct "Token") = some (encodeVal (.struct "Token" [])).length ∧ decodeVal [⟨"Token", false, []⟩] (.struct "Token") (encodeVal (.struct "Token" [])) = some (.struct "Token" []) := @rep_roundtrip [⟨"Token", false, []⟩] (.struct "Token" []) (.struct "Token") (.empty rfl rfl)

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `emptyStruct`, subject `ConcreteSpec.Rep.decodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.rejectValue`). -/
example : RepVal [⟨"Token", false, []⟩] (.struct "Token" []) (.struct "Token") ∧ ¬ (Ty.size [⟨"Token", false, []⟩] (.struct "Token") = some (encodeVal (.struct "Token" [])).length ∧ rejectValue [⟨"Token", false, []⟩] (.struct "Token") (encodeVal (.struct "Token" [])) = some (.struct "Token" [])) := ⟨(.empty rfl rfl), by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `emptyStruct`, subject `ConcreteSpec.Rep.Ty.size`, mutant `ConcreteSpec.Rep.RoundtripEvidence.missingSize`). -/
example : RepVal [⟨"Token", false, []⟩] (.struct "Token" []) (.struct "Token") ∧ ¬ (missingSize [⟨"Token", false, []⟩] (.struct "Token") = some (encodeVal (.struct "Token" [])).length ∧ decodeVal [⟨"Token", false, []⟩] (.struct "Token") (encodeVal (.struct "Token" [])) = some (.struct "Token" [])) := ⟨(.empty rfl rfl), by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `negativeSigned`, subject `ConcreteSpec.Rep.encodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.spuriousByte`). -/
example : RepVal [] (.scalar (.int .i16 (-2))) (.scalar (.int .i16)) ∧
    ¬ (Ty.size [] (.scalar (.int .i16)) = some (spuriousByte (.scalar (.int .i16 (-2)))).length ∧
      decodeVal [] (.scalar (.int .i16)) (spuriousByte (.scalar (.int .i16 (-2)))) = some (.scalar (.int .i16 (-2)))) := ⟨(.scalar (.int (by decide))), by decide⟩

/-- Planted (law `ConcreteSpec.Rep.rep_roundtrip`, case `emptyStruct`, subject `ConcreteSpec.Rep.encodeVal`, mutant `ConcreteSpec.Rep.RoundtripEvidence.spuriousByte`). -/
example : RepVal [⟨"Token", false, []⟩] (.struct "Token" []) (.struct "Token") ∧
    ¬ (Ty.size [⟨"Token", false, []⟩] (.struct "Token") = some (spuriousByte (.struct "Token" [])).length ∧
      decodeVal [⟨"Token", false, []⟩] (.struct "Token") (spuriousByte (.struct "Token" [])) = some (.struct "Token" [])) := ⟨(.empty rfl rfl), by decide⟩

end RoundtripEvidence

end ConcreteSpec.Rep
