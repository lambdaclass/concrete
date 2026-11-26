import Concrete.Backend.LLVMIR
import Concrete.Backend.Runtime
import ConcreteSpec.Proofs.Scalar

/-!
# BIR → LLVM text (UNVERIFIED)

PROVISIONAL (slice 0). The printer is labelled unverified: what its text means is premise
`P_print` (`Concrete/README.md`), and LLVM itself is inside `P_toolchain[llvm]`. It prints `main`'s
body as `define i32 @concrete_body()` returning the result word; the runtime entry
(`Runtime.llvmItems`) is linked with it.

* an integer of type `t` is an `i{bits}` holding its two's-complement bit pattern; a flag is `i1`;
* `binOvf` is `@llvm.{s,u}{add,sub,mul}.with.overflow.i{bits}`;
* `trunc` is `trunc`, `sext`/`zext` by the source's signedness, or a copy at equal width;
* block parameters become `phi` nodes over the predecessors' arguments;
* `ret a` returns `zext a`; `trap s` returns `256 + s`.
-/

namespace Concrete.Backend.LLVM
open Concrete.BIR
open ConcreteSpec (IntTy)
open ConcreteSpec.Core (toBits toBitsAtWidth ofBits)
open ScalarBridge (toCore)
open Concrete.Backend.Runtime (indexed)

def ty : Ty → TypeCode
  | .int t => .integer (Width.ofCore (toCore t))
  | .bool => .flag

/-- The bit pattern of `v` in `t`, as an unsigned decimal. -/
def pattern (t : IntTy) (v : Int) : Nat := toBits (toCore t) v

/-- Encode the payload at the width of its LLVM operand slot. Raw BIR can
carry a different immediate tag, so the slot determines the bit pattern.
BIR well-formedness excludes integer immediates in Boolean slots. -/
private def operandAt (width : Nat) : Operand → Argument
  | .reg r => .local (.register r)
  | .imm _ v => .immediate (.natural (toBitsAtWidth width v))

def arithmeticOp : BinOp → Arithmetic
  | .add => .add
  | .sub => .sub
  | .mul => .mul

def inst : Inst → List Item
  | .bin op t d a b =>
    let width := Width.ofCore (toCore t)
    [.instruction (.bin (.register d) (arithmeticOp op) (.integer width)
      (operandAt width.bits a) (operandAt width.bits b))]
  | .binOvf op t d f a b =>
    let t := toCore t
    let width := Width.ofCore t
    let i := TypeCode.integer width
    let pair := TypeCode.overflowPair width
    [.instruction (.call (some (.overflow d)) pair (.overflow (arithmeticOp op) t)
      [(i, operandAt width.bits a), (i, operandAt width.bits b)]),
     .instruction (.extract (.register d) pair (.overflow d) 0),
     .instruction (.extract (.register f) pair (.overflow d) 1)]
  | .trunc fr to d a =>
    let source := toCore fr
    let target := toCore to
    if source.bits = target.bits then
      [.instruction (.bin (.register d) .add (.integer (Width.ofCore target)) (operandAt (Width.ofCore target).bits a) (.immediate (.natural 0)))]
    else
      let conv := if target.bits < source.bits then .trunc else if source.signed then .sext else .zext
      [.instruction (.convert (.register d) conv (.integer (Width.ofCore source), operandAt (Width.ofCore source).bits a) (.integer (Width.ofCore target)))]

/-- The edges into each block: (target, predecessor, arguments). -/
def edges (p : Program) : List (Nat × Nat × List Operand) :=
  (indexed p.blocks).flatMap fun (j, b) =>
    match b.term with
    | .br t args => [(t, j, args)]
    | .condbr _ t ta e ea => [(t, j, ta), (e, j, ea)]
    | _ => []

def phis (p : Program) (j : Nat) (b : Block) : List Item :=
  let incoming := (edges p).filter (·.1 == j)
  (indexed b.params).map fun (k, (r, pty)) =>
    let arms := incoming.map fun (_, pred, args) =>
      (operandAt ((ty pty).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block pred)
    .instruction (.phi (.register r) (ty pty) arms)

def term (j : Nat) : Term → List Item
  | .ret a =>
    [.instruction (.convert (.result j) .zext (.integer .u8, operandAt 8 a) (.integer .u32)),
     .terminator (.ret (.integer .u32, .local (.result j)))]
  | .trap s => [.terminator (.ret (.integer .u32, .immediate (.natural (toBits .u32 (Int.ofNat (256 + s))))))]
  | .br t _ => [.terminator (.branch (.block t))]
  | .condbr c t _ e _ => [.terminator (.cond (.register c) (.block t) (.block e))]

/-- The intrinsics the body uses, each declared once. -/
private def usedIntrinsics (p : Program) : List (BinOp × IntTy) :=
  ((p.blocks.flatMap (·.insts)).filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none
  ).eraseDups

def declarations (p : Program) : List Item :=
  (usedIntrinsics p).map fun (op, t) =>
    let t := toCore t
    let width := Width.ofCore t
    .declare (.overflowPair width) (.overflow (arithmeticOp op) t) [.integer width, .integer width]

def body (p : Program) : List Item :=
  declarations p ++ [.blank, .define false (.integer .u32) .body []] ++
  ((indexed p.blocks).flatMap fun (j, b) =>
    [.label (.block j)] ++ phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++
  [.endFunction]

/-- `main`'s body as LLVM text. -/
def print (p : Program) : String := render (body p)

/-- The LLVM assembly for the BIR body together with its runtime entry. -/
def moduleItems (p : Program) : List Item := body p ++ [.blank] ++ Runtime.llvmItems p.sites

/-- The entire rendered LLVM module. -/
def module (p : Program) : String := render (moduleItems p)

/-! ## Scalar encoding laws -/

/-- Representation lemma. Existing LLVM immediate text uses the same decimal number
for every payload, including out-of-range ones. -/
theorem pattern_agrees (t : IntTy) (v : Int) :
    pattern t v = (v % (2 : Int) ^ t.bits).toNat :=
  (ScalarBridge.toBits t v).symm

/-- Law. An encoded immediate fits its declared integer width.
Authority: docs/decisions.md#od126. -/
theorem pattern_fits (t : IntTy) (v : Int) : pattern t v < 2 ^ (toCore t).bits :=
  ConcreteSpec.Core.toBits_lt _ _

/-- Law. Every representable immediate survives decoding at its declared type.
Authority: docs/decisions.md#od126. -/
theorem pattern_roundTrip (t : IntTy) (v : Int) (h : (toCore t).InRange v) :
    ofBits (toCore t) (pattern t v) = v :=
  ConcreteSpec.Core.ofBits_toBits _ _ h

end Concrete.Backend.LLVM
