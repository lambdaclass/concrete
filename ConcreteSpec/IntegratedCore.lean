import ConcreteSpec.StageInterface
import ConcreteSpec.Proofs.Memory

/-! Representation laws for the frozen early Core fragment. -/

namespace ConcreteSpec.StageInterface

open Core

/-- Integer operands of early addition use Core's target-independent,
little-endian literal encoding and the approved overflow trap kind. -/
theorem earlyAddLiteralCondition (slice : Slice) (ty : Core.IntTy) (left right : Int) :
    (sliceLanguage slice).safety
      (.expression (.inl (.add ty (.literal (.int ty left)) (.literal (.int ty right))))) =
      [⟨0, .overflow, [
        (.inl (.scalar (.int ty)), .literal ((Mem.leBytes ty.byteSize (toBits ty left)).map
          fun byte => UInt8.ofNat byte.val)),
        (.inl (.scalar (.int ty)), .literal ((Mem.leBytes ty.byteSize (toBits ty right)).map
          fun byte => UInt8.ofNat byte.val))]⟩] := by
  cases ty <;> rfl

private theorem literalOctets (bytes : List (Fin 256)) :
    (bytes.map (fun byte => UInt8.ofNat byte.val)).map UInt8.toFin = bytes := by
  induction bytes with
  | nil => rfl
  | cons byte bytes ih =>
    have same : (UInt8.ofNat byte.val).toFin = byte :=
      Fin.ext (Nat.mod_eq_of_lt byte.isLt)
    rw [List.map_cons, List.map_cons, same, ih]

/-- Every in-range integer round-trips through the literal encoding used in
Core operation identities. The decoder does not depend on the target. -/
theorem earlyIntegerLiteralRoundTrip (slice : Slice) (ty : Core.IntTy) (n : Int)
    (inRange : ty.InRange n) :
    (sliceLanguage slice).literal (.inl (.scalar (.int ty)))
      ((Mem.leBytes ty.byteSize (toBits ty n)).map fun byte => UInt8.ofNat byte.val) =
      some (.scalar (.int ty n)) := by
  change (if ((Mem.leBytes ty.byteSize (toBits ty n)).map
      (fun byte => UInt8.ofNat byte.val)).length = ty.byteSize then
    some (CoreValue.scalar (.int ty (ofBits ty (Mem.leValue
      (((Mem.leBytes ty.byteSize (toBits ty n)).map
        (fun byte => UInt8.ofNat byte.val)).map UInt8.toFin)))))
    else none) = some (.scalar (.int ty n))
  simp only [List.length_map, Mem.leBytes_length, ite_true, literalOctets]
  have fits : toBits ty n < 256 ^ ty.byteSize := by
    rw [show 256 ^ ty.byteSize = 2 ^ ty.bits from Mem.intByte_power ty]
    exact toBits_lt ty n
  rw [Mem.leValue_leBytes _ _ fits, ofBits_toBits ty n inRange]

/-- The Core encoding can be passed directly to the materialized integer
decoder on little-endian targets. Core literal order itself is fixed. -/
theorem earlyLiteralMatchesStorage (target : Target) (little : target.byteOrder = .little)
    (ty : Core.IntTy) (n : Int) :
    (((Mem.leBytes ty.byteSize (toBits ty n)).map fun byte => UInt8.ofNat byte.val).map
      fun byte => Mem.MByte.init byte.toFin none) = Mem.encodeInt target ty n := by
  calc
    _ = (((Mem.leBytes ty.byteSize (toBits ty n)).map fun byte => UInt8.ofNat byte.val).map
        UInt8.toFin).map (fun byte => Mem.MByte.init byte none) := by simp only [List.map_map, Function.comp_def]
    _ = _ := by rw [literalOctets]; simp only [Mem.encodeInt, Mem.intBytes, little, Mem.ordered]

/-- An overflow from the shared checked addition is a trap in the
integrated early interpreter, at every integer width. -/
theorem earlyAddOverflow (slice : Slice) (ty : Core.IntTy) (left right : Int)
    (locals : AList Var EarlyValue) (store : Hybrid.State)
    (overflow : checked ty .add left right = .error .overflow) :
    (sliceLanguage slice).operands.eval locals store
      (.inl (.add ty (.literal (.int ty left)) (.literal (.int ty right)))) = .trap .overflow := by
  have evaluation :
      (sliceLanguage slice).operands.eval locals store
        (.inl (.add ty (.literal (.int ty left)) (.literal (.int ty right)))) =
      ((plain ty .add left right).map (CoreValue.scalar ∘ CVal.int ty)).map
        (fun value => (value, store)) := by
    cases ty <;> rfl
  rw [evaluation]
  unfold plain
  rw [overflow]
  rfl

end ConcreteSpec.StageInterface
