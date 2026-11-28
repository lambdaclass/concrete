import Concrete.Backend.LCBWordLemmas
import Concrete.Interfaces.Pipeline

/-!
# Controls for `Concrete.Backend.LCBWordLemmas`

Positive and negative controls covering:
* profile membership (`u8`/`i8`/`u16`/`i16` vs `u32`/`i32`/`u64`/`i64`);
* checked-add range tests, overflow flags, and wrapping across signed and unsigned boundaries;
* load-bearing failure controls for out-of-profile widths (`u32`, `i32`), out-of-range operands,
  missing signed bias, and off-by-one thresholds;
* hosted return-word ABI encoding/decoding, exit/trap disjointness (`return 134` vs trap),
  exact agreement with frozen `LCBInput.trapCapacity`, and wraparound collision at `trapCapacity`;
* concrete `LeanCompilerBackend.Verified.Source` execution of both the 6-instruction general
  checked-add expansion and the 3-instruction `u8` checked-add expansion.
-/

namespace StageInterfaceControls.LCBWordLemmas

open ConcreteSpec.Core
open Concrete.Interfaces
open Concrete.Backend.LCBWordLemmas
open LeanCompilerBackend.Verified

/-! ## Profile boundary and ABI alignment -/

example : inWordProfile .u8 = true ∧ inWordProfile .i8 = true ∧
    inWordProfile .u16 = true ∧ inWordProfile .i16 = true ∧
    inWordProfile .u32 = false ∧ inWordProfile .i32 = false ∧
    inWordProfile .u64 = false ∧ inWordProfile .i64 = false := by
  decide

/-- The pure capacity constant equals the frozen LCB pipeline ABI capacity. -/
example : Concrete.Backend.LCBWordLemmas.trapCapacity = LCBInput.trapCapacity := rfl

/-! ## Checked-add boundary positive controls across all four profile types -/

example :
    (biased .u8 (raw .add 20 22)).toNat = 42 ∧
    (biased .u8 (raw .add 20 22)).toNat < 256 ∧
    eqBit (ultBit (biased .u8 (raw .add 20 22)) (wordBits 256)) (wordBits 0) = boolBits false ∧
    uleBit (wordBits 256) (biased .u8 (raw .add 20 22)) = boolBits false ∧
    ultBit (wordBits 255) (biased .u8 (raw .add 20 22)) = boolBits false := by
  decide

-- u8 boundary: 200 + 55 = 255 (fits) vs 200 + 56 = 256 (overflows, wraps to 0)
example :
    checked .u8 .add 200 55 = .ok 255 ∧
    (wordBits 200 + wordBits 55).toNat < 256 ∧
    ultBit (wordBits 255) (wordBits 200 + wordBits 55) = boolBits false ∧
    ((wordBits 200 + wordBits 55) &&& wordBits 255) = wordBits 255 := by
  decide

example :
    checked .u8 .add 200 56 = .error .overflow ∧
    256 ≤ (wordBits 200 + wordBits 56).toNat ∧
    ultBit (wordBits 255) (wordBits 200 + wordBits 56) = boolBits true ∧
    uleBit (wordBits 256) (wordBits 200 + wordBits 56) = boolBits true ∧
    ((wordBits 200 + wordBits 56) &&& wordBits 255) = wordBits 0 := by
  decide

-- i8 negative and positive boundaries: -100 + -28 = -128 (ok), -100 + -29 = -129 (overflow),
-- 100 + 27 = 127 (ok), 100 + 28 = 128 (overflow)
example :
    (biased .i8 (raw .add (-100) (-28))).toNat < 256 ∧
    eqBit (ultBit (biased .i8 (raw .add (-100) (-28))) (wordBits 256)) (wordBits 0) = boolBits false ∧
    256 ≤ (biased .i8 (raw .add (-100) (-29))).toNat ∧
    eqBit (ultBit (biased .i8 (raw .add (-100) (-29))) (wordBits 256)) (wordBits 0) = boolBits true ∧
    (((wordBits (-129) - wordBits IntTy.i8.min) &&& wordBits 255) + wordBits IntTy.i8.min) =
      wordBits 127 := by
  decide

example :
    (biased .i8 (raw .add 100 27)).toNat < 256 ∧
    256 ≤ (biased .i8 (raw .add 100 28)).toNat ∧
    uleBit (wordBits 256) (biased .i8 (raw .add 100 28)) = boolBits true ∧
    (((wordBits 128 - wordBits IntTy.i8.min) &&& wordBits 255) + wordBits IntTy.i8.min) =
      wordBits (-128) := by
  decide

-- u16 and i16 boundaries, including maximum product 65535 * 65535 without 32-bit wrap aliasing
example :
    (biased .u16 (raw .add 65535 0)).toNat < 65536 ∧
    2 ^ 16 ≤ (biased .u16 (raw .add 65535 1)).toNat ∧
    2 ^ 16 ≤ (biased .u16 (raw .mul 65535 65535)).toNat ∧
    (biased .i16 (raw .add (-32768) 0)).toNat < 65536 ∧
    2 ^ 16 ≤ (biased .i16 (raw .add (-32768) (-1))).toNat ∧
    2 ^ 16 ≤ (biased .i16 (raw .add 32767 1)).toNat ∧
    2 ^ 16 ≤ (biased .i16 (raw .mul (-32768) (-32768))).toNat := by
  decide

/-! ## Negative controls: why profile, range, bias, and threshold hypotheses are load-bearing -/

-- Out-of-profile u32: 2^32 wraps to 0 in BitVec 32, so the 32-bit unsigned-LT threshold
-- degenerates even for in-range 0 + 0, while the mathematical bound < 2^32 accepts 4294967295 + 1.
example :
    inWordProfile .u32 = false ∧
    wordBits (2 ^ IntTy.u32.bits) = wordBits 0 ∧
    ultBit (biased .u32 (raw .add 0 0)) (wordBits (2 ^ IntTy.u32.bits)) = 0 ∧
    (biased .u32 (raw .add 4294967295 1)).toNat < 2 ^ IntTy.u32.bits ∧
    checked .u32 .add 4294967295 1 = .error .overflow := by
  decide

-- Out-of-profile i32: 2147483647 + 1 overflows i32, yet its 32-bit biased word still has toNat < 2^32.
example :
    inWordProfile .i32 = false ∧
    (biased .i32 (raw .add 2147483647 1)).toNat < 2 ^ IntTy.i32.bits ∧
    checked .i32 .add 2147483647 1 = .error .overflow := by
  decide

-- Out-of-range operand: 2^32 aliases to 0#32, making the word test pass for an out-of-range sum.
example :
    ¬ IntTy.u8.InRange (2 ^ 32) ∧
    (wordBits (2 ^ 32) + wordBits 1).toNat < 256 ∧
    checked .u8 .add (2 ^ 32) 1 = .error .overflow := by
  decide

-- Dropping the signed bias on i8 falsely flags the in-range sum (-100) + (-28) = -128 as overflow.
example :
    IntTy.i8.InRange ((-100) + (-28)) ∧
    ¬ (raw .add (-100) (-28)).toNat < 256 ∧
    (biased .i8 (raw .add (-100) (-28))).toNat < 256 := by
  decide

-- Off-by-one threshold (<u 255 instead of <u 256) falsely rejects boundary u8 sum 200 + 55 = 255.
example :
    checked .u8 .add 200 55 = .ok 255 ∧
    ultBit (wordBits 200 + wordBits 55) (wordBits 255) = 0 ∧
    ultBit (wordBits 200 + wordBits 55) (wordBits 256) = 1 := by
  decide

/-! ## Return-word ABI controls -/

example :
    decodeReturnWord (exitWordBits 0) = .exit 0 ∧
    decodeReturnWord (exitWordBits 42) = .exit 42 ∧
    decodeReturnWord (exitWordBits 134) = .exit 134 ∧
    decodeReturnWord (exitWordBits 255) = .exit 255 ∧
    runtime_ult_exit (n := 134) (by decide) = rfl := by
  exact ⟨by decide, by decide, by decide, by decide, rfl⟩

example :
    decodeReturnWord (trapWordBits 0) = .trap 0 ∧
    decodeReturnWord (trapWordBits 1) = .trap 1 ∧
    decodeReturnWord (trapWordBits (trapCapacity - 1)) = .trap (trapCapacity - 1) ∧
    ultBit (trapWordBits 0) (BitVec.ofNat 32 exitLimit) = 0 ∧
    (trapWordBits 0 - BitVec.ofNat 32 exitLimit).toNat = 0 ∧
    (trapWordBits (trapCapacity - 1) - BitVec.ofNat 32 exitLimit).toNat = trapCapacity - 1 := by
  decide

-- Normal exit 134 is strictly disjoint from every admissible trap word.
example (slot : Nat) (hslot : slot < trapCapacity) :
    exitWordBits 134 ≠ trapWordBits slot :=
  exitWordBits_ne_trapWordBits (by decide) hslot

-- Negative capacity control: slot = trapCapacity wraps to exit word 0 and collides with normal exit 0.
example :
    trapWordBits trapCapacity = exitWordBits 0 ∧
    decodeReturnWord (trapWordBits trapCapacity) = .exit 0 := by
  exact ⟨trapCapacity_wraps_to_zero, by rw [trapCapacity_wraps_to_zero]; decide⟩

/-! ## Concrete `Source.Function` execution controls using the step theorems -/

/-- A 3-block LCB function implementing checked `u8` addition with `checkedOpExpansion`:
returns the sum when in range, or `trapWord slot` on overflow. -/
private def checkedAdd6Fn (a b : Int) (slot : Nat) : Source.Function where
  resultWidth := .word
  parameters := #[]
  valueWidths := Array.replicate 6 .word
  blocks := #[
    { phis := #[]
      instructions := (checkedOpExpansion .add .u8 0 (.literal (word a)) (.literal (word b))).toArray
      terminator := .branch (.local 5) 1 2 },
    { phis := #[], instructions := #[], terminator := .ret (.literal (trapWord slot)) },
    { phis := #[], instructions := #[], terminator := .ret (.local 3) }
  ]

/-- A 3-block LCB function implementing compact 3-instruction `u8` checked addition. -/
private def checkedAdd3Fn (a b : Int) (slot : Nat) : Source.Function where
  resultWidth := .word
  parameters := #[]
  valueWidths := Array.replicate 3 .word
  blocks := #[
    { phis := #[]
      instructions := (checkedAddU8Expansion 0 (.literal (word a)) (.literal (word b))).toArray
      terminator := .branch (.local 2) 1 2 },
    { phis := #[], instructions := #[], terminator := .ret (.literal (trapWord slot)) },
    { phis := #[], instructions := #[], terminator := .ret (.local 1) }
  ]

/-- Universal behavior theorem for `checkedAdd6Fn` via `checkedAdd_steps`: proves the emitted LCB
function returns `exitWord (x + y).toNat` on in-range sums and `trapWord slot` on overflow. -/
theorem checkedAdd6Fn_behavior {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y)
    (slot : Nat) :
    Source.HasBehavior (checkedAdd6Fn x y slot) #[]
      (.returned (if checked .u8 .add x y = .error .overflow then trapWord slot
        else exitWord (x + y).toNat)) := by
  let f := checkedAdd6Fn x y slot
  let b0 : Source.Block :=
    { phis := #[]
      instructions := (checkedOpExpansion .add .u8 0 (.literal (word x)) (.literal (word y))).toArray
      terminator := .branch (.local 5) 1 2 }
  let b1 : Source.Block := { phis := #[], instructions := #[], terminator := .ret (.literal (trapWord slot)) }
  let b2 : Source.Block := { phis := #[], instructions := #[], terminator := .ret (.local 3) }
  let env0 : Source.Environment := Array.replicate 6 none
  have hb0 : f.blocks[0]? = some b0 := rfl
  have hb1 : f.blocks[1]? = some b1 := rfl
  have hb2 : f.blocks[2]? = some b2 := rfl
  have hinit : Source.initial f #[] = .ok ⟨0, 0, env0⟩ :=
    initial_no_params (f := f) (entry := b0) rfl hb0 rfl
  obtain ⟨u₄, h6⟩ := checkedAdd_steps (f := f) (s := ⟨0, 0, env0⟩) (lb := b0) (n := 0)
    (t := .u8) (a' := .literal (word x)) (b' := .literal (word y)) (x := x) (y := y)
    rfl hx hy hb0
    (fun k hk => by rcases k with _ | _ | _ | _ | _ | _ | k <;> first | rfl | omega)
    rfl rfl
    (fun m hm => by
      have hm6 : m < 6 := hm
      change (Array.replicate 6 LeanCompilerBackend.Width.word)[m]? = some .word
      rw [Array.getElem?_replicate, ite_eq_left hm6])
    (by decide)
  let env1 := ((set4 env0 0
    (some (.word (raw .add x y)))
    (some (.word (biased .u8 (raw .add x y))))
    (some (.word (masked .u8 (biased .u8 (raw .add x y)))))
    (some (word (wrap .u8 (x + y))))).set! 4 u₄).set!
    5 (some (boolWord (decide (checked .u8 .add x y = .error .overflow))))
  have hr5 : Source.read env1 .word (.local 5) =
      .ok (boolWord (decide (checked .u8 .add x y = .error .overflow))) :=
    read_set_self (by show 5 < ((set4 env0 0 _ _ _ _).set! 4 u₄).size; rw [size_set!, size_set4]; decide) rfl
  refine ⟨⟨0, 0, env0⟩, hinit, ?_⟩
  if hovf : checked .u8 .add x y = .error .overflow then
    rw [ite_eq_left hovf]
    have htr : Source.transfer f 0
        (if (boolWord (decide (checked .u8 .add x y = .error .overflow))).toNat != 0 then 1 else 2)
        env1 = .ok ⟨1, 0, env1⟩ := by
      rw [boolWord_branch, decide_eq_true hovf]
      exact transfer_no_phis (f := f) (tb := b1) hb1 rfl
    have hstep7 : Source.Step f ⟨0, 6, env1⟩ ⟨1, 0, env1⟩ :=
      step_branch (f := f) (s := ⟨0, 6, env1⟩) (lb := b0) hb0 rfl rfl hr5 htr
    have hret : Source.step f ⟨1, 0, env1⟩ = .ok (.returned (trapWord slot)) :=
      step_ret (f := f) (s := ⟨1, 0, env1⟩) (lb := b1) hb1 rfl rfl rfl
    exact ⟨7, ⟨1, 0, env1⟩, steps_append h6 (.succ hstep7 (.zero _)), hret⟩
  else
    rw [ite_eq_right hovf]
    have htr : Source.transfer f 0
        (if (boolWord (decide (checked .u8 .add x y = .error .overflow))).toNat != 0 then 1 else 2)
        env1 = .ok ⟨2, 0, env1⟩ := by
      rw [boolWord_branch, decide_eq_false hovf]
      exact transfer_no_phis (f := f) (tb := b2) hb2 rfl
    have hstep7 : Source.Step f ⟨0, 6, env1⟩ ⟨2, 0, env1⟩ :=
      step_branch (f := f) (s := ⟨0, 6, env1⟩) (lb := b0) hb0 rfl rfl hr5 htr
    have hin : IntTy.u8.InRange (x + y) := by
      dsimp only [checked, inRangeOr] at hovf
      if h : IntTy.u8.InRange (x + y) then exact h
      else rw [ite_eq_right h] at hovf; exact (hovf rfl).elim
    have hr3 : Source.read env1 .word (.local 3) = .ok (exitWord (x + y).toNat) := by
      dsimp only [env1, set4]
      rw [read_set_other (by decide), read_set_other (by decide)]
      rw [read_set_self (by show 3 < (((env0.set! 0 _).set! 1 _).set! 2 _).size; rw [size_set!, size_set!, size_set!]; decide) rfl]
      rw [wrap_of_inRange .u8 (x + y) hin]
      exact congrArg Except.ok (congrArg Source.Value.word (exitWordBits_ofInt hin))
    have hret : Source.step f ⟨2, 0, env1⟩ = .ok (.returned (exitWord (x + y).toNat)) :=
      step_ret (f := f) (s := ⟨2, 0, env1⟩) (lb := b2) hb2 rfl rfl hr3
    exact ⟨7, ⟨2, 0, env1⟩, steps_append h6 (.succ hstep7 (.zero _)), hret⟩

-- Concrete evaluation checks on both the 6-instruction and 3-instruction expansions:
-- exit 42, boundary exit 255, trap slot 0 (word 256), and second trap slot 1 (word 257).
example :
    Source.evaluateFunction (checkedAdd6Fn 20 22 0) #[] 16 = .returned (exitWord 42) ∧
    Source.evaluateFunction (checkedAdd6Fn 200 55 0) #[] 16 = .returned (exitWord 255) ∧
    Source.evaluateFunction (checkedAdd6Fn 200 56 0) #[] 16 = .returned (trapWord 0) ∧
    Source.evaluateFunction (checkedAdd6Fn 200 56 1) #[] 16 = .returned (trapWord 1) ∧
    Source.evaluateFunction (checkedAdd3Fn 20 22 0) #[] 16 = .returned (exitWord 42) ∧
    Source.evaluateFunction (checkedAdd3Fn 200 55 0) #[] 16 = .returned (exitWord 255) ∧
    Source.evaluateFunction (checkedAdd3Fn 200 56 0) #[] 16 = .returned (trapWord 0) ∧
    Source.evaluateFunction (checkedAdd3Fn 200 56 1) #[] 16 = .returned (trapWord 1) := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

/-- Universal behavior theorem for the compact 3-instruction `checkedAdd3Fn` via `checkedAddU8_steps`. -/
theorem checkedAdd3Fn_behavior {x y : Int} (hx : IntTy.u8.InRange x) (hy : IntTy.u8.InRange y)
    (slot : Nat) :
    Source.HasBehavior (checkedAdd3Fn x y slot) #[]
      (.returned (if checked .u8 .add x y = .error .overflow then trapWord slot
        else exitWord (x + y).toNat)) := by
  let f := checkedAdd3Fn x y slot
  let b0 : Source.Block :=
    { phis := #[]
      instructions := (checkedAddU8Expansion 0 (.literal (word x)) (.literal (word y))).toArray
      terminator := .branch (.local 2) 1 2 }
  let b1 : Source.Block := { phis := #[], instructions := #[], terminator := .ret (.literal (trapWord slot)) }
  let b2 : Source.Block := { phis := #[], instructions := #[], terminator := .ret (.local 1) }
  let env0 : Source.Environment := Array.replicate 3 none
  have hb0 : f.blocks[0]? = some b0 := rfl
  have hb1 : f.blocks[1]? = some b1 := rfl
  have hb2 : f.blocks[2]? = some b2 := rfl
  have hinit : Source.initial f #[] = .ok ⟨0, 0, env0⟩ :=
    initial_no_params (f := f) (entry := b0) rfl hb0 rfl
  have h3 := checkedAddU8_steps (f := f) (s := ⟨0, 0, env0⟩) (lb := b0) (n := 0)
    (a' := .literal (word x)) (b' := .literal (word y)) (x := x) (y := y)
    hx hy hb0
    (fun k hk => by rcases k with _ | _ | _ | k <;> first | rfl | omega)
    rfl rfl
    (fun m hm => by
      have hm3 : m < 3 := hm
      change (Array.replicate 3 LeanCompilerBackend.Width.word)[m]? = some .word
      rw [Array.getElem?_replicate, ite_eq_left hm3])
    (by decide)
  let env1 := ((env0.set! 0 (some (.word (wordBits x + wordBits y)))).set!
    1 (some (word (wrap .u8 (x + y))))).set!
    2 (some (boolWord (decide (checked .u8 .add x y = .error .overflow))))
  have hr2 : Source.read env1 .word (.local 2) =
      .ok (boolWord (decide (checked .u8 .add x y = .error .overflow))) :=
    read_set_self (by show 2 < ((env0.set! 0 _).set! 1 _).size; rw [size_set!, size_set!]; decide) rfl
  refine ⟨⟨0, 0, env0⟩, hinit, ?_⟩
  if hovf : checked .u8 .add x y = .error .overflow then
    rw [ite_eq_left hovf]
    have htr : Source.transfer f 0
        (if (boolWord (decide (checked .u8 .add x y = .error .overflow))).toNat != 0 then 1 else 2)
        env1 = .ok ⟨1, 0, env1⟩ := by
      rw [boolWord_branch, decide_eq_true hovf]
      exact transfer_no_phis (f := f) (tb := b1) hb1 rfl
    have hstep4 : Source.Step f ⟨0, 3, env1⟩ ⟨1, 0, env1⟩ :=
      step_branch (f := f) (s := ⟨0, 3, env1⟩) (lb := b0) hb0 rfl rfl hr2 htr
    have hret : Source.step f ⟨1, 0, env1⟩ = .ok (.returned (trapWord slot)) :=
      step_ret (f := f) (s := ⟨1, 0, env1⟩) (lb := b1) hb1 rfl rfl rfl
    exact ⟨4, ⟨1, 0, env1⟩, steps_append h3 (.succ hstep4 (.zero _)), hret⟩
  else
    rw [ite_eq_right hovf]
    have htr : Source.transfer f 0
        (if (boolWord (decide (checked .u8 .add x y = .error .overflow))).toNat != 0 then 1 else 2)
        env1 = .ok ⟨2, 0, env1⟩ := by
      rw [boolWord_branch, decide_eq_false hovf]
      exact transfer_no_phis (f := f) (tb := b2) hb2 rfl
    have hstep4 : Source.Step f ⟨0, 3, env1⟩ ⟨2, 0, env1⟩ :=
      step_branch (f := f) (s := ⟨0, 3, env1⟩) (lb := b0) hb0 rfl rfl hr2 htr
    have hin : IntTy.u8.InRange (x + y) := by
      dsimp only [checked, inRangeOr] at hovf
      if h : IntTy.u8.InRange (x + y) then exact h
      else rw [ite_eq_right h] at hovf; exact (hovf rfl).elim
    have hr1 : Source.read env1 .word (.local 1) = .ok (exitWord (x + y).toNat) := by
      dsimp only [env1]
      rw [read_set_other (by decide)]
      rw [read_set_self (by show 1 < (env0.set! 0 _).size; rw [size_set!]; decide) rfl]
      rw [wrap_of_inRange .u8 (x + y) hin]
      exact congrArg Except.ok (congrArg Source.Value.word (exitWordBits_ofInt hin))
    have hret : Source.step f ⟨2, 0, env1⟩ = .ok (.returned (exitWord (x + y).toNat)) :=
      step_ret (f := f) (s := ⟨2, 0, env1⟩) (lb := b2) hb2 rfl rfl hr1
    exact ⟨4, ⟨2, 0, env1⟩, steps_append h3 (.succ hstep4 (.zero _)), hret⟩

end StageInterfaceControls.LCBWordLemmas
