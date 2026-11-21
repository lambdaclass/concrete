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
open ConcreteSpec (IntTy Trap)
open ConcreteSpec.Core (toBits ofBits)
open ScalarBridge (toCore)
open Concrete.Backend.Runtime (indexed)

def ty : Ty → TypeCode
  | .int t => .integer (toCore t)
  | .bool => .flag

/-- The bit pattern of `v` in `t`, as an unsigned decimal. -/
def pattern (t : IntTy) (v : Int) : Nat := toBits (toCore t) v

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

def operand : Operand → Argument
  | .reg r => .local (.register r)
  | .imm t v => .immediate (.scalar (toCore t) v)

/-- The token carried by a scalar immediate in the renderer. -/
def operandToken (t : IntTy) (v : Int) : String :=
  (Immediate.scalar (toCore t) v).token

/-- Law. Every emitted BIR immediate is a token in the decimal grammar.
Authority: docs/decisions.md#od126. -/
theorem operand_token (t : IntTy) (v : Int) :
    DecimalToken (operandToken t v) (Int.ofNat (pattern t v)) :=
  decimal_token (pattern t v)

/-- Law. The emitted decimal token parses to exactly the encoded bit pattern.
Authority: docs/decisions.md#od126. -/
theorem operand_parse (t : IntTy) (v : Int) :
    parseDecimal (operandToken t v) = some (Int.ofNat (pattern t v)) :=
  (parseDecimal_exact _ _).2 (operand_token t v)

private def decodePattern (t : IntTy) : Int → Option Int
  | .ofNat n => if n < 2 ^ (toCore t).bits then some (ofBits (toCore t) n) else none
  | .negSucc _ => none

/-- Law. A representable immediate's emitted decimal token denotes its Core value.
The interpretation of the enclosing LLVM text still uses `P_print`.
Authority: docs/decisions.md#od126. -/
theorem operand_roundTrip (t : IntTy) (v : Int) (h : (toCore t).InRange v) :
    (parseDecimal (operandToken t v)).bind (decodePattern t) = some v := by
  rw [operand_parse, Option.bind_some, decodePattern]
  simp [pattern_fits t v, pattern_roundTrip t v h]

def arithmeticOp : BinOp → Arithmetic
  | .add => .add
  | .sub => .sub
  | .mul => .mul

def inst : Inst → List Item
  | .bin op t d a b =>
    [.instruction (.bin (.register d) (arithmeticOp op) (.integer (toCore t)) (operand a) (operand b))]
  | .binOvf op t d f a b =>
    let t := toCore t
    let i := TypeCode.integer t
    let pair := TypeCode.overflowPair t
    [.instruction (.call (some (.overflow d)) pair (.overflow (arithmeticOp op) t)
      [(i, operand a), (i, operand b)]),
     .instruction (.extract (.register d) pair (.overflow d) 0),
     .instruction (.extract (.register f) pair (.overflow d) 1)]
  | .trunc fr to d a =>
    let source := toCore fr
    let target := toCore to
    if source.bits = target.bits then
      [.instruction (.bin (.register d) .add (.integer target) (operand a) (.immediate (.scalar target 0)))]
    else
      let conv := if target.bits < source.bits then .trunc else if source.signed then .sext else .zext
      [.instruction (.convert (.register d) conv (.integer source, operand a) (.integer target))]

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
      (operand (args.getD k (.imm .u8 0)), Label.block pred)
    .instruction (.phi (.register r) (ty pty) arms)

def term (j : Nat) : Term → List Item
  | .ret a =>
    [.instruction (.convert (.result j) .zext (.integer .u8, operand a) (.integer .u32)),
     .terminator (.ret (.integer .u32, .local (.result j)))]
  | .trap s => [.terminator (.ret (.integer .u32, .immediate (.natural (256 + s))))]
  | .br t _ => [.terminator (.branch (.block t))]
  | .condbr c t _ e _ => [.terminator (.cond (.register c) (.block t) (.block e))]

/-- The intrinsics the body uses, each declared once. -/
def declarations (p : Program) : List Item :=
  let used := (p.blocks.flatMap (·.insts)).filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none
  used.eraseDups.map fun (op, t) =>
    let t := toCore t
    .declare (.overflowPair t) (.overflow (arithmeticOp op) t) [.integer t, .integer t]

def body (p : Program) : List Item :=
  declarations p ++ [.blank, .define false (.integer .u32) .body []] ++
  ((indexed p.blocks).flatMap fun (j, b) =>
    [.label (.block j)] ++ phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++
  [.endFunction]

/-- `main`'s body as LLVM text. -/
def print (p : Program) : String := render (body p)

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := render (body p ++ [.blank] ++ Runtime.llvmItems p.sites)

namespace PatternEvidence

/-- Instance (law `operand_token`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_token :
    DecimalToken (operandToken .u8 17) 17 :=
  operand_token .u8 17

/-- Instance (law `pattern_fits`, case `unsigned_overflow`). -/
theorem unsigned_overflow : pattern .u8 256 < 2 ^ (toCore .u8).bits :=
  pattern_fits .u8 256

/-- Instance (law `pattern_roundTrip`, case `negative_signed`). -/
theorem negative_signed : ofBits (toCore .i8) (pattern .i8 (-1)) = -1 :=
  pattern_roundTrip .i8 (-1) (by decide)

/-- Preserve a nonnegative payload without reducing it to the declared width. -/
def unreduced (_ : IntTy) (v : Int) : Nat := v.toNat

/-- Lose every nonzero payload by always producing zero. -/
def zeroed (_ : IntTy) (_ : Int) : Nat := 0

/-- Planted (law `pattern_fits`, case `unsigned_overflow`, subject `pattern`, mutant `unreduced`). -/
theorem unreduced_rejected : ¬ (unreduced .u8 256 < 2 ^ (toCore .u8).bits) := by decide

/-- Planted (law `pattern_roundTrip`, case `negative_signed`, subject `pattern`, mutant `zeroed`). -/
theorem zeroed_rejected :
    (toCore .i8).InRange (-1) ∧ ofBits (toCore .i8) (zeroed .i8 (-1)) ≠ -1 := by decide

/-- Emit an oversized decimal token for one valid immediate. -/
def oversized : IntTy → Int → String
  | .u8, 17 | .i8, 0 => "256"
  | t, v => operandToken t v

/-- Instance (law `operand_parse`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_parses :
    parseDecimal (operandToken .u8 17) = some (Int.ofNat (pattern .u8 17)) :=
  operand_parse .u8 17

/-- Instance (law `operand_roundTrip`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_roundTrips :
    (parseDecimal (operandToken .u8 17)).bind (decodePattern .u8) = some 17 :=
  operand_roundTrip .u8 17 (by decide)

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `operandToken`, mutant `oversized`). -/
theorem oversized_parse_rejected :
    parseDecimal (oversized .u8 17) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `operandToken`, mutant `oversized`). -/
theorem oversized_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (oversized .u8 17)).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Emit a digit separator for one valid immediate. -/
def underscored : IntTy → Int → String
  | .u8, 17 => "1_7"
  | t, v => operandToken t v

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `operandToken`, mutant `underscored`). -/
theorem underscored_parse_rejected :
    parseDecimal (underscored .u8 17) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `operandToken`, mutant `underscored`). -/
theorem underscored_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (underscored .u8 17)).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `operandToken`, mutant `underscored`). -/
theorem underscored_token_rejected : ¬ DecimalToken (underscored .u8 17) 17 :=
  fun h => underscored_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `operandToken`, mutant `oversized`). -/
theorem oversized_token_rejected : ¬ DecimalToken (oversized .u8 17) 17 :=
  fun h => oversized_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Instance (law `operand_roundTrip`, case `signed_zero`). -/
theorem signed_zero_roundTrips :
    (parseDecimal (operandToken .i8 0)).bind (decodePattern .i8) = some 0 :=
  operand_roundTrip .i8 0 (by decide)

/-- Planted (law `operand_roundTrip`, case `signed_zero`, subject `operandToken`, mutant `oversized`). -/
theorem oversized_signed_zero_rejected :
    (parseDecimal (oversized .i8 0)).bind (decodePattern .i8) = none := by decide

end PatternEvidence

end Concrete.Backend.LLVM
