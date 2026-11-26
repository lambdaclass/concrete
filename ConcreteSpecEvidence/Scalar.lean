import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Scalar

namespace ConcreteSpec.Core

namespace ScalarEvidence
/-- A mutant valuation forgets to reduce the carry bit after evaluating a term. -/
def wideEval (ρ : Var → Option Int) (e : CTerm) : Option Int :=
  (e.eval ρ).map fun n => n + 256

/-- Helper (inversion). The closed empty width environment has no assigned variables. -/
private theorem empty_width_env : WidthEnv (fun _ => none) (fun _ => none) := by
  intro x t n hx
  cases hx

/-- Instance (law `ConcreteSpec.Core.typed_eval_range`, case `literal`). -/
theorem typed_eval_range_example_literal : ∀ r, (CTerm.lit 7).eval (fun _ => none) = some r → IntTy.i8.InRange r :=
  typed_eval_range (fun _ => none) (fun _ => none) (.lit 7) .i8 (.lit (by decide)) empty_width_env

/-- Planted (law `ConcreteSpec.Core.typed_eval_range`, case `literal`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
theorem typed_eval_range_planted_literal_wideEval : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ r, wideEval (fun _ => none) (.lit 7) = some r → IntTy.i8.InRange r) := by
  refine ⟨.lit (by decide), empty_width_env, fun h => ?_⟩
  have hh := h 263 rfl
  revert hh
  decide

/-- Instance (law `ConcreteSpec.Core.machine_operands`, case `typed_pair`). -/
theorem machine_operands_example_typed_pair : ∀ x y, (CTerm.lit 7).eval (fun _ => none) = some x →
    (CTerm.lit 8).eval (fun _ => none) = some y →
    IntTy.i8.InRange x ∧ ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8.InRange y) :=
  machine_operands (fun _ => none) (fun _ => none) .i8 .i8 (.plain .add) (.lit 7) (.lit 8)
    (.lit (by decide)) (.lit (by decide)) (.inr rfl) empty_width_env

/-- Planted (law `ConcreteSpec.Core.machine_operands`, case `typed_pair`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
theorem machine_operands_planted_typed_pair_wideEval : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧ (CTerm.lit 8).HasWidth (fun _ => none) .i8 ∧
    ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8 = .i8) ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ x y, wideEval (fun _ => none) (.lit 7) = some x → wideEval (fun _ => none) (.lit 8) = some y →
      IntTy.i8.InRange x ∧ ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8.InRange y)) := by
  refine ⟨.lit (by decide), .lit (by decide), .inr rfl, empty_width_env, fun h => ?_⟩
  have hh := (h 263 264 rfl rfl).1
  revert hh
  decide
/-- Instance (law `ConcreteSpec.Core.machine_inRange`, case `typed_machine`). -/
theorem machine_inRange_example_typed_machine : ∀ r, (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).eval (fun _ => none) = some r →
    IntTy.i8.InRange r :=
  machine_inRange (fun _ => none) (fun _ => none) .i8 .i8 (.plain .add) (.lit 7) (.lit 8)
    (.lit (by decide)) (.lit (by decide)) (.inr rfl) empty_width_env

/-- Planted (law `ConcreteSpec.Core.machine_inRange`, case `typed_machine`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarEvidence.wideEval`). -/
theorem machine_inRange_planted_typed_machine_wideEval : (CTerm.lit 7).HasWidth (fun _ => none) .i8 ∧ (CTerm.lit 8).HasWidth (fun _ => none) .i8 ∧
    ((MachineOp.plain .add).countOperand = true ∨ IntTy.i8 = .i8) ∧
    WidthEnv (fun _ => none) (fun _ => none) ∧
    ¬ (∀ r, wideEval (fun _ => none) (.machine .i8 (.plain .add) (.lit 7) (.lit 8)) = some r →
      IntTy.i8.InRange r) := by
  refine ⟨.lit (by decide), .lit (by decide), .inr rfl, empty_width_env, fun h => ?_⟩
  have hh := h 271 rfl
  revert hh
  decide

end ScalarEvidence

namespace ScalarLawEvidence
/-- A checked implementation that forgets to reduce a spurious high carry bit. -/
def carriedChecked (t : IntTy) (op : ArithOp) (a b : Int) : Except ArithmeticError Int :=
  (checked t op a b).map fun r => r + (2 : Int) ^ t.bits

/-- Instance (law `ConcreteSpec.Core.checked_inRange`, case `addition`). -/
theorem checked_inRange_example_addition : IntTy.i8.InRange 15 := checked_inRange .i8 .add 7 8 15 (by decide) rfl
/-- Planted (law `ConcreteSpec.Core.checked_inRange`, case `addition`,
subject `ConcreteSpec.Core.checked`, mutant `ConcreteSpec.Core.ScalarLawEvidence.carriedChecked`). -/
theorem checked_inRange_planted_addition_carriedChecked : IntTy.i8.InRange 7 ∧ carriedChecked .i8 .add 7 8 = .ok 271 ∧
    ¬ IntTy.i8.InRange 271 := by decide
/-- Mutant encoders collapse distinct failures. -/
def sameName (_ : TrapKind) : String := "same"
def sameTrap (_ : ArithmeticError) : TrapKind := .abort
/-- Mutant bit encodings and arithmetic results. -/
def unreducedBits (t : IntTy) (n : Int) : Nat := toBits t n + 2 ^ t.bits
def offsetOfBits (t : IntTy) (m : Nat) : Int := ofBits t m + (2 : Int) ^ t.bits
def offByOneOfBits (t : IntTy) (m : Nat) : Int := ofBits t (m + 1)
def unreducedUnary (t : IntTy) (n : Int) : Int := n + (2 : Int) ^ t.bits
def incrementWrap (t : IntTy) (n : Int) : Int := wrap t (n + 1)
def increment (_ : IntTy) (n : Int) : Int := n + 1
def unreducedBitwise (t : IntTy) (op : BitOp) (a b : Int) : Int :=
  bitwise t op a b + (2 : Int) ^ t.bits
def xorForAll (t : IntTy) (_ : BitOp) (a b : Int) : Int := bitwise t .xor a b
def subtractForAll (t : IntTy) (_ : WrapOp) (a b : Int) : Int := wrapping t .sub a b
/-- This mutant leaves machine evaluation intact but corrupts exact evaluation. -/
def corruptExact (ρ : Var → Option Int) (e : CTerm) : Option Int :=
  match e with
  | .exact op a b => (a.eval ρ).bind fun x => (b.eval ρ).map fun y => wrap .u8 (op.exact x y + 1)
  | _ => e.eval ρ
def eraseExact (e : CTerm) : CTerm := match e with
  | .machine _ _ a _ => a.toExact
  | _ => e.toExact
/-- Forgetting low-bit truncation breaks a signed left shift. -/
def unboundedBits (_ : IntTy) (n : Int) : Nat := n.toNat
/-- An encoder that shifts the bit pattern by one breaks logical right shift. -/
def offsetBits (t : IntTy) (n : Int) : Nat := toBits t n + 1

/-- Instance (law `ConcreteSpec.Core.TrapKind.name_injective`, case `concrete`). -/
theorem name_injective_example_concrete : ∀ (a b : TrapKind), a.name = b.name → a = b :=
  TrapKind.name_injective

/-- Planted (law `ConcreteSpec.Core.TrapKind.name_injective`, case `concrete`, subject `ConcreteSpec.Core.TrapKind.name`, mutant `ConcreteSpec.Core.ScalarLawEvidence.sameName`). -/
theorem name_injective_planted_concrete_sameName : ¬ (∀ (a b : TrapKind), sameName a = sameName b → a = b) := by
  intro h
  have hh := h .overflow .abort rfl
  cases hh

/-- Instance (law `ConcreteSpec.Core.ArithmeticError.trapKind_injective`, case `concrete`). -/
theorem trapKind_injective_example_concrete : ∀ (a b : ArithmeticError), a.trapKind = b.trapKind → a = b :=
  ArithmeticError.trapKind_injective

/-- Planted (law `ConcreteSpec.Core.ArithmeticError.trapKind_injective`, case `concrete`, subject `ConcreteSpec.Core.ArithmeticError.trapKind`, mutant `ConcreteSpec.Core.ScalarLawEvidence.sameTrap`). -/
theorem trapKind_injective_planted_concrete_sameTrap : ¬ (∀ (a b : ArithmeticError), sameTrap a = sameTrap b → a = b) := by
  intro h
  have hh := h .overflow .divisionByZero rfl
  cases hh

/-- Instance (law `ConcreteSpec.Core.toBits_lt`, case `concrete`). -/
theorem toBits_lt_example_concrete : toBits .i8 7 < 2 ^ IntTy.i8.bits :=
  toBits_lt .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_lt`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unreducedBits`). -/
theorem toBits_lt_planted_concrete_unreducedBits : ¬ (unreducedBits .i8 7 < 2 ^ IntTy.i8.bits) := by decide

/-- Instance (law `ConcreteSpec.Core.ofBits_inRange`, case `concrete`). -/
theorem ofBits_inRange_example_concrete : IntTy.i8.InRange (ofBits .i8 7) :=
  ofBits_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.ofBits_inRange`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.offsetOfBits`). -/
theorem ofBits_inRange_planted_concrete_offsetOfBits : 7 < 2 ^ IntTy.i8.bits ∧
    ¬ (IntTy.i8.InRange (offsetOfBits .i8 7)) := by decide

/-- Instance (law `ConcreteSpec.Core.ofBits_toBits`, case `concrete`). -/
theorem ofBits_toBits_example_concrete : ofBits .i8 (toBits .i8 7) = 7 :=
  ofBits_toBits .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.ofBits_toBits`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.offByOneOfBits`). -/
theorem ofBits_toBits_planted_concrete_offByOneOfBits : IntTy.i8.InRange 7 ∧
    ¬ (offByOneOfBits .i8 (toBits .i8 7) = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.wrap_inRange`, case `concrete`). -/
theorem wrap_inRange_example_concrete : IntTy.i8.InRange (wrap .i8 300) :=
  wrap_inRange .i8 300

/-- Planted (law `ConcreteSpec.Core.wrap_inRange`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unreducedUnary`). -/
theorem wrap_inRange_planted_concrete_unreducedUnary : ¬ (IntTy.i8.InRange (unreducedUnary .i8 300)) := by decide

/-- Instance (law `ConcreteSpec.Core.wrap_of_inRange`, case `concrete`). -/
theorem wrap_of_inRange_example_concrete : wrap .i8 7 = 7 :=
  wrap_of_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.wrap_of_inRange`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.incrementWrap`). -/
theorem wrap_of_inRange_planted_concrete_incrementWrap : IntTy.i8.InRange 7 ∧
    ¬ (incrementWrap .i8 7 = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_inRange`, case `concrete`). -/
theorem saturate_inRange_example_concrete : IntTy.i8.InRange (saturate .i8 300) :=
  saturate_inRange .i8 300

/-- Planted (law `ConcreteSpec.Core.saturate_inRange`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unreducedUnary`). -/
theorem saturate_inRange_planted_concrete_unreducedUnary : ¬ (IntTy.i8.InRange (unreducedUnary .i8 300)) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_of_inRange`, case `concrete`). -/
theorem saturate_of_inRange_example_concrete : saturate .i8 7 = 7 :=
  saturate_of_inRange .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_of_inRange`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.incrementWrap`). -/
theorem saturate_of_inRange_planted_concrete_incrementWrap : IntTy.i8.InRange 7 ∧
    ¬ (incrementWrap .i8 7 = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.bitwise_inRange`, case `concrete`). -/
theorem bitwise_inRange_example_concrete : IntTy.i8.InRange (bitwise .i8 .and 7 3) :=
  bitwise_inRange .i8 .and 7 3

/-- Planted (law `ConcreteSpec.Core.bitwise_inRange`, case `concrete`, subject `ConcreteSpec.Core.bitwise`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unreducedBitwise`). -/
theorem bitwise_inRange_planted_concrete_unreducedBitwise : ¬ (IntTy.i8.InRange (unreducedBitwise .i8 .and 7 3)) := by decide

/-- Instance (law `ConcreteSpec.Core.bitNot_inRange`, case `concrete`). -/
theorem bitNot_inRange_example_concrete : IntTy.i8.InRange (bitNot .i8 7) :=
  bitNot_inRange .i8 7

/-- Planted (law `ConcreteSpec.Core.bitNot_inRange`, case `concrete`, subject `ConcreteSpec.Core.bitNot`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unreducedUnary`). -/
theorem bitNot_inRange_planted_concrete_unreducedUnary : ¬ (IntTy.i8.InRange (unreducedUnary .i8 7)) := by decide

/-- Instance (law `ConcreteSpec.Core.wrapping_of_checked`, case `concrete`). -/
theorem wrapping_of_checked_example_concrete : wrapping .i8 .add 7 8 = 15 :=
  wrapping_of_checked .i8 .add 7 8 15 rfl

/-- Planted (law `ConcreteSpec.Core.wrapping_of_checked`, case `concrete`, subject `ConcreteSpec.Core.wrapping`, mutant `ConcreteSpec.Core.ScalarLawEvidence.subtractForAll`). -/
theorem wrapping_of_checked_planted_concrete_subtractForAll : checked .i8 WrapOp.add.arith 7 8 = .ok 15 ∧
    ¬ (subtractForAll .i8 .add 7 8 = 15) := by decide

/-- Instance (law `ConcreteSpec.Core.saturating_of_checked`, case `concrete`). -/
theorem saturating_of_checked_example_concrete : saturating .i8 .add 7 8 = 15 :=
  saturating_of_checked .i8 .add 7 8 15 rfl

/-- Planted (law `ConcreteSpec.Core.saturating_of_checked`, case `concrete`, subject `ConcreteSpec.Core.saturating`, mutant `ConcreteSpec.Core.ScalarLawEvidence.subtractForAll`). -/
theorem saturating_of_checked_planted_concrete_subtractForAll : checked .i8 WrapOp.add.arith 7 8 = .ok 15 ∧
    ¬ (subtractForAll .i8 .add 7 8 = 15) := by decide





/-- Instance (law `ConcreteSpec.Core.toBits_ofBits`, case `concrete`). -/
theorem toBits_ofBits_example_concrete : toBits .i8 (ofBits .i8 7) = 7 :=
  toBits_ofBits .i8 7 (by decide)

/-- Planted (law `ConcreteSpec.Core.toBits_ofBits`, case `concrete`, subject `ConcreteSpec.Core.ofBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.offByOneOfBits`). -/
theorem toBits_ofBits_planted_concrete_offByOneOfBits : 7 < 2 ^ IntTy.i8.bits ∧
    ¬ (toBits .i8 (offByOneOfBits .i8 7) = 7) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_wrap`, case `concrete`). -/
theorem toBits_wrap_example_concrete : toBits .i8 (wrap .i8 7) = toBits .i8 7 :=
  toBits_wrap .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_wrap`, case `concrete`, subject `ConcreteSpec.Core.wrap`, mutant `ConcreteSpec.Core.ScalarLawEvidence.increment`). -/
theorem toBits_wrap_planted_concrete_increment : ¬ (toBits .i8 (increment .i8 7) = toBits .i8 7) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_wrapping`, case `concrete`). -/
theorem toBits_wrapping_example_concrete : toBits .i8 (wrapping .i8 .add 7 8) = toBits .i8 (WrapOp.add.exact 7 8) :=
  toBits_wrapping .i8 .add 7 8

/-- Planted (law `ConcreteSpec.Core.toBits_wrapping`, case `concrete`, subject `ConcreteSpec.Core.wrapping`, mutant `ConcreteSpec.Core.ScalarLawEvidence.subtractForAll`). -/
theorem toBits_wrapping_planted_concrete_subtractForAll : ¬ (toBits .i8 (subtractForAll .i8 .add 7 8) = toBits .i8 (WrapOp.add.exact 7 8)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_shl`, case `concrete`). -/
theorem toBits_shl_example_concrete : toBits .i8 (-56) = toBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat) :=
  toBits_shl .i8 100 1 (-56) rfl

/-- Planted (law `ConcreteSpec.Core.toBits_shl`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.unboundedBits`). -/
theorem toBits_shl_planted_concrete_unboundedBits :
    (if validShift .i8 1 then .ok (ofBits .i8 (unboundedBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat)))
      else (.error .invalidShift : Except ArithmeticError Int)) = .ok (-56) ∧
    ¬ (unboundedBits .i8 (-56) = unboundedBits .i8 (100 * (2 : Int) ^ (1 : Int).toNat)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_shr_unsigned`, case `concrete`). -/
theorem toBits_shr_unsigned_example_concrete : toBits .u8 4 = toBits .u8 8 >>> (1 : Int).toNat :=
  toBits_shr_unsigned .u8 rfl 8 1 4 (by decide) rfl

/-- Planted (law `ConcreteSpec.Core.toBits_shr_unsigned`, case `concrete`, subject `ConcreteSpec.Core.toBits`, mutant `ConcreteSpec.Core.ScalarLawEvidence.offsetBits`). -/
theorem toBits_shr_unsigned_planted_concrete_offsetBits : IntTy.u8.signed = false ∧ IntTy.u8.InRange 8 ∧ checked .u8 .shr 8 1 = .ok 4 ∧
    ¬ (offsetBits .u8 4 = offsetBits .u8 8 >>> (1 : Int).toNat) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_bitwise`, case `concrete`). -/
theorem toBits_bitwise_example_concrete : toBits .i8 (bitwise .i8 .and 7 3) = (match BitOp.and with | .and => toBits .i8 7 &&& toBits .i8 3 | .or => toBits .i8 7 ||| toBits .i8 3 | .xor => toBits .i8 7 ^^^ toBits .i8 3) :=
  toBits_bitwise .i8 .and 7 3

/-- Planted (law `ConcreteSpec.Core.toBits_bitwise`, case `concrete`, subject `ConcreteSpec.Core.bitwise`, mutant `ConcreteSpec.Core.ScalarLawEvidence.xorForAll`). -/
theorem toBits_bitwise_planted_concrete_xorForAll : ¬ (toBits .i8 (xorForAll .i8 .and 7 3) = (match BitOp.and with | .and => toBits .i8 7 &&& toBits .i8 3 | .or => toBits .i8 7 ||| toBits .i8 3 | .xor => toBits .i8 7 ^^^ toBits .i8 3)) := by decide

/-- Instance (law `ConcreteSpec.Core.toBits_bitNot`, case `concrete`). -/
theorem toBits_bitNot_example_concrete : toBits .i8 (bitNot .i8 7) = 2 ^ IntTy.i8.bits - 1 - toBits .i8 7 :=
  toBits_bitNot .i8 7

/-- Planted (law `ConcreteSpec.Core.toBits_bitNot`, case `concrete`, subject `ConcreteSpec.Core.bitNot`, mutant `ConcreteSpec.Core.ScalarLawEvidence.incrementWrap`). -/
theorem toBits_bitNot_planted_concrete_incrementWrap : ¬ (toBits .i8 (incrementWrap .i8 7) = 2 ^ IntTy.i8.bits - 1 - toBits .i8 7) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_below`, case `concrete`). -/
theorem saturate_below_example_concrete : saturate .i8 (-300) = IntTy.i8.min :=
  saturate_below .i8 (-300) (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_below`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.incrementWrap`). -/
theorem saturate_below_planted_concrete_incrementWrap : (-300 : Int) < IntTy.i8.min ∧
    ¬ (incrementWrap .i8 (-300) = IntTy.i8.min) := by decide

/-- Instance (law `ConcreteSpec.Core.saturate_above`, case `concrete`). -/
theorem saturate_above_example_concrete : saturate .i8 300 = IntTy.i8.max :=
  saturate_above .i8 300 (by decide)

/-- Planted (law `ConcreteSpec.Core.saturate_above`, case `concrete`, subject `ConcreteSpec.Core.saturate`, mutant `ConcreteSpec.Core.ScalarLawEvidence.incrementWrap`). -/
theorem saturate_above_planted_concrete_incrementWrap : IntTy.i8.max < 300 ∧
    ¬ (incrementWrap .i8 300 = IntTy.i8.max) := by decide

/-- Instance (law `ConcreteSpec.Core.machine_plain_eq_exact`, case `concrete`). -/
theorem machine_plain_eq_exact_example_concrete : (CTerm.exact .add (.lit 7) (.lit 8)).eval (fun _ => none) = some 15 :=
  machine_plain_eq_exact (fun _ => none) .i8 .add (.lit 7) (.lit 8) 15 rfl

/-- Planted (law `ConcreteSpec.Core.machine_plain_eq_exact`, case `concrete`, subject `ConcreteSpec.Core.CTerm.eval`, mutant `ConcreteSpec.Core.ScalarLawEvidence.corruptExact`). -/
theorem machine_plain_eq_exact_planted_concrete_corruptExact : corruptExact (fun _ => none) (.machine .i8 (.plain WrapOp.add.arith) (.lit 7) (.lit 8)) = some 15 ∧
    ¬ (corruptExact (fun _ => none) (.exact .add (.lit 7) (.lit 8)) = some 15) := by decide

/-- Instance (law `ConcreteSpec.Core.eval_toExact`, case `concrete`). -/
theorem eval_toExact_example_concrete : (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).toExact.eval (fun _ => none) = some 15 :=
  eval_toExact (fun _ => none) (.machine .i8 (.plain .add) (.lit 7) (.lit 8)) 15 rfl

/-- Planted (law `ConcreteSpec.Core.eval_toExact`, case `concrete`, subject `ConcreteSpec.Core.CTerm.toExact`, mutant `ConcreteSpec.Core.ScalarLawEvidence.eraseExact`). -/
theorem eval_toExact_planted_concrete_eraseExact : (CTerm.machine .i8 (.plain .add) (.lit 7) (.lit 8)).eval (fun _ => none) = some 15 ∧
    ¬ ((eraseExact (.machine .i8 (.plain .add) (.lit 7) (.lit 8))).eval (fun _ => none) = some 15) := by decide

/-- Wrong strict interval inclusion refuses a valid conversion at equal endpoints. -/
def strictLossless (s t : IntTy) : Prop := t.min < s.min ∧ s.max < t.max
/-- Instance (law `ConcreteSpec.Core.lossless_iff`, case `same_width`). -/
theorem lossless_iff_example_same_width : Lossless .i8 .i8 ↔ ∀ n, IntTy.i8.InRange n → IntTy.i8.InRange n :=
  lossless_iff .i8 .i8
/-- Planted (law `ConcreteSpec.Core.lossless_iff`, case `same_width`, subject `ConcreteSpec.Core.Lossless`, mutant `ConcreteSpec.Core.ScalarLawEvidence.strictLossless`). -/
theorem lossless_iff_planted_same_width_strictLossless : ¬ (strictLossless .i8 .i8 ↔ ∀ n, IntTy.i8.InRange n → IntTy.i8.InRange n) := by
  intro h
  have bad := h.mpr (fun _ hn => hn)
  exact (Int.lt_irrefl _) bad.1

end ScalarLawEvidence

namespace ScalarVariableEvidence
/-- A scalar valuation over two different numeric identities. -/
def values : AList Var Int := [(⟨7⟩, 12), (⟨8⟩, 19)]

/-- Reading a numeric variable does not merge it with another number. -/
theorem example_Reading_numeric_variable_does_not_merge : (CTerm.var ⟨7⟩).eval (AList.lookup values) = some 12 ∧
    (CTerm.var ⟨8⟩).eval (AList.lookup values) = some 19 := by decide

/-- Assignment uses the shared map; the other number retains its value. -/
theorem example_Assignment_uses_shared_map_other_number : (CTerm.var ⟨7⟩).eval (AList.lookup (AList.update values ⟨7⟩ (fun _ => 42))) = some 42 ∧
    (CTerm.var ⟨8⟩).eval (AList.lookup (AList.update values ⟨7⟩ (fun _ => 42))) = some 19 := by decide

/-- Scope removal erases only the chosen number. -/
theorem example_Scope_removal_erases_only_chosen_number : (CTerm.var ⟨7⟩).eval (AList.lookup (AList.erase values ⟨7⟩)) = none ∧
    (CTerm.var ⟨8⟩).eval (AList.lookup (AList.erase values ⟨7⟩)) = some 19 := by decide
end ScalarVariableEvidence

namespace CharEvidence
/-- Wrong construction through Lean's replacement-character conversion accepts invalid scalars. -/
def replacementConstructor (n : UInt32) : Option Char := some (Char.ofNat n.toNat)
/-- Wrong ASCII construction admits all Unicode Latin-1 bytes. -/
def latin1Constructor (b : UInt8) : Option Char := charFromCodePoint b.toUInt32

/-- Instance (law `ConcreteSpec.Core.charFromCodePoint_map`, case `surrogate`). -/
theorem charFromCodePoint_map_example_surrogate : (charFromCodePoint 0xd800).map charCodePoint = none := charFromCodePoint_map 0xd800
/-- Planted (law `ConcreteSpec.Core.charFromCodePoint_map`, case `surrogate`,
subject `ConcreteSpec.Core.charFromCodePoint`, mutant `ConcreteSpec.Core.CharEvidence.replacementConstructor`). -/
theorem charFromCodePoint_map_planted_surrogate_replacementConstructor : ¬ ((replacementConstructor 0xd800).map charCodePoint = none) := by decide
/-- Instance (law `ConcreteSpec.Core.charFromCodePoint_roundtrip`, case `ascii`). -/
theorem charFromCodePoint_roundtrip_example_ascii : charFromCodePoint (charCodePoint (Char.ofNat 65)) = some (Char.ofNat 65) :=
  charFromCodePoint_roundtrip (Char.ofNat 65)
/-- Wrong construction increments the code point before checking. -/
def nextConstructor (n : UInt32) : Option Char := charFromCodePoint (n + 1)
/-- Planted (law `ConcreteSpec.Core.charFromCodePoint_roundtrip`, case `ascii`,
subject `ConcreteSpec.Core.charFromCodePoint`, mutant `ConcreteSpec.Core.CharEvidence.nextConstructor`). -/
theorem charFromCodePoint_roundtrip_planted_ascii_nextConstructor : nextConstructor (charCodePoint (Char.ofNat 65)) ≠ some (Char.ofNat 65) := by decide
/-- Instance (law `ConcreteSpec.Core.charFromAscii_map`, case `high_byte`). -/
theorem charFromAscii_map_example_high_byte : (charFromAscii 128).map charCodePoint = none := charFromAscii_map 128
/-- Planted (law `ConcreteSpec.Core.charFromAscii_map`, case `high_byte`,
subject `ConcreteSpec.Core.charFromAscii`, mutant `ConcreteSpec.Core.CharEvidence.latin1Constructor`). -/
theorem charFromAscii_map_planted_high_byte_latin1Constructor : ¬ ((latin1Constructor 128).map charCodePoint = none) := by decide
end CharEvidence

/-- Incorrect negation that preserves its input. -/
def identityNeg (_t : IntTy) (a : Int) : Except ArithmeticError Int := .ok a

/-- Incorrect negation that never reports overflow. -/
def uncheckedNeg (_t : IntTy) (a : Int) : Except ArithmeticError Int := .ok (-a)

/-- Instance (law `ConcreteSpec.Core.checkedNeg_ok_arithmetic`, case `signed`). -/
theorem checkedNeg_ok_arithmetic_example_signed : checkedNeg .i8 5 = .ok (-5) ↔ (-5 : Int) = -5 ∧
    (if IntTy.i8.signed = true then (5 : Int) ≠ IntTy.i8.min else (5 : Int) = 0) :=
  checkedNeg_ok_arithmetic .i8 5 (-5) (by decide)

/-- Planted (law `ConcreteSpec.Core.checkedNeg_ok_arithmetic`, case `signed`,
subject `ConcreteSpec.Core.checkedNeg`, mutant `ConcreteSpec.Core.identityNeg`). -/
theorem checkedNeg_ok_arithmetic_planted_signed_identityNeg : IntTy.i8.InRange 5 ∧ ¬(identityNeg .i8 5 = .ok (-5) ↔ (-5 : Int) = -5 ∧
    (if IntTy.i8.signed = true then (5 : Int) ≠ IntTy.i8.min else (5 : Int) = 0)) := by decide

/-- Instance (law `ConcreteSpec.Core.checkedNeg_error_iff`, case `minimum`). -/
theorem checkedNeg_error_iff_example_minimum : checkedNeg .i8 (-128) = .error .overflow ↔ ArithmeticError.overflow = .overflow ∧
    ¬(if IntTy.i8.signed = true then (-128 : Int) ≠ IntTy.i8.min else (-128 : Int) = 0) :=
  checkedNeg_error_iff .i8 (-128) .overflow (by decide)

/-- Planted (law `ConcreteSpec.Core.checkedNeg_error_iff`, case `minimum`,
subject `ConcreteSpec.Core.checkedNeg`, mutant `ConcreteSpec.Core.uncheckedNeg`). -/
theorem checkedNeg_error_iff_planted_minimum_uncheckedNeg : IntTy.i8.InRange (-128) ∧ ¬(uncheckedNeg .i8 (-128) = .error .overflow ↔
    ArithmeticError.overflow = .overflow ∧
      ¬(if IntTy.i8.signed = true then (-128 : Int) ≠ IntTy.i8.min else (-128 : Int) = 0)) := by decide

end ConcreteSpec.Core
