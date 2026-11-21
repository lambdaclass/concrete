import Concrete.Backend.Decimal
import Concrete.Backend.Runtime
import ConcreteSpec.Proofs.Scalar

/-!
# BIR → LLVM text (UNVERIFIED)

PROVISIONAL (slice 0). The printer is labelled unverified: what its text means is premise
`P_print` (`Concrete/README.md`), and LLVM itself is inside `P_toolchain[llvm]`. It prints `main`'s
body as `define i32 @concrete_body()` returning the result word; the runtime entry
(`Runtime.llvm`) is linked with it.

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

def intTy (t : IntTy) : String := integerType (toCore t).bits

def ty : Ty → String
  | .int t => intTy t
  | .bool => integerType 1

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

def operand : Operand → String
  | .reg r => s!"%r{decimal r}"
  | .imm t v => decimal (pattern t v)

/-- Law. Every emitted BIR immediate is a token in the decimal grammar.
Authority: docs/decisions.md#od126. -/
theorem operand_token (t : IntTy) (v : Int) :
    DecimalToken (operand (.imm t v)) (Int.ofNat (pattern t v)) :=
  decimal_token (pattern t v)

/-- Law. The emitted decimal token parses to exactly the encoded bit pattern.
Authority: docs/decisions.md#od126. -/
theorem operand_parse (t : IntTy) (v : Int) :
    parseDecimal (operand (.imm t v)) = some (Int.ofNat (pattern t v)) :=
  (parseDecimal_exact _ _).2 (operand_token t v)

private def decodePattern (t : IntTy) : Int → Option Int
  | .ofNat n => if n < 2 ^ (toCore t).bits then some (ofBits (toCore t) n) else none
  | .negSucc _ => none

/-- Law. A representable immediate's emitted decimal token denotes its Core value.
The interpretation of the enclosing LLVM text still uses `P_print`.
Authority: docs/decisions.md#od126. -/
theorem operand_roundTrip (t : IntTy) (v : Int) (h : (toCore t).InRange v) :
    (parseDecimal (operand (.imm t v))).bind (decodePattern t) = some v := by
  rw [operand_parse, Option.bind_some, decodePattern]
  simp [pattern_fits t v, pattern_roundTrip t v h]

def opName : BinOp → String
  | .add => "add"
  | .sub => "sub"
  | .mul => "mul"

def intrinsic (op : BinOp) (t : IntTy) : String :=
  s!"llvm.{if (toCore t).signed then "s" else "u"}{opName op}.with.overflow.{intTy t}"

def inst : Inst → String
  | .bin op t d a b => s!"  %r{decimal d} = {opName op} {intTy t} {operand a}, {operand b}\n"
  | .binOvf op t d f a b =>
    let pair := s!"\{{intTy t}, {integerType 1}}"
    s!"  %o{decimal d} = call {pair} @{intrinsic op t}({intTy t} {operand a}, {intTy t} {operand b})\n" ++
    s!"  %r{decimal d} = extractvalue {pair} %o{decimal d}, {decimal 0}\n" ++
    s!"  %r{decimal f} = extractvalue {pair} %o{decimal d}, {decimal 1}\n"
  | .trunc fr to d a =>
    let source := toCore fr
    let target := toCore to
    let conv := if target.bits < source.bits then "trunc" else if source.signed then "sext" else "zext"
    if source.bits = target.bits then s!"  %r{decimal d} = add {intTy to} {operand a}, {decimal 0}\n"
    else s!"  %r{decimal d} = {conv} {intTy fr} {operand a} to {intTy to}\n"

/-- The edges into each block: (target, predecessor, arguments). -/
def edges (p : Program) : List (Nat × Nat × List Operand) :=
  (indexed p.blocks).flatMap fun (j, b) =>
    match b.term with
    | .br t args => [(t, j, args)]
    | .condbr _ t ta e ea => [(t, j, ta), (e, j, ea)]
    | _ => []

def phis (p : Program) (j : Nat) (b : Block) : String :=
  let incoming := (edges p).filter (·.1 == j)
  String.join ((indexed b.params).map fun (k, (r, pty)) =>
    let arms := incoming.map fun (_, pred, args) =>
      s!"[ {operand (args.getD k (.imm .u8 0))}, %b{decimal pred} ]"
    s!"  %r{decimal r} = phi {ty pty} {", ".intercalate arms}\n")

def term (j : Nat) : Term → String
  | .ret a => s!"  %ret{decimal j} = zext {integerType 8} {operand a} to {integerType 32}\n  ret {integerType 32} %ret{decimal j}\n"
  | .trap s => s!"  ret {integerType 32} {decimal (256 + s)}\n"
  | .br t _ => s!"  br label %b{decimal t}\n"
  | .condbr c t _ e _ => s!"  br {integerType 1} %r{decimal c}, label %b{decimal t}, label %b{decimal e}\n"

/-- The intrinsics the body uses, each declared once. -/
def declarations (p : Program) : String :=
  let used := (p.blocks.flatMap (·.insts)).filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none
  let distinct := used.eraseDups
  String.join (distinct.map fun (op, t) =>
    s!"declare \{{intTy t}, {integerType 1}} @{intrinsic op t}({intTy t}, {intTy t})\n")

/-- `main`'s body as LLVM text. -/
def print (p : Program) : String :=
  declarations p ++ s!"\ndefine {integerType 32} @concrete_body() \{\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{decimal j}:\n" ++ phis p j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := print p ++ "\n" ++ Runtime.llvm p.sites

namespace PatternEvidence

/-- Instance (law `operand_token`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_token :
    DecimalToken (operand (.imm .u8 17)) 17 :=
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
def oversized : Operand → String
  | .imm .u8 17 | .imm .i8 0 => "256"
  | a => operand a

/-- Instance (law `operand_parse`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_parses :
    parseDecimal (operand (.imm .u8 17)) = some (Int.ofNat (pattern .u8 17)) :=
  operand_parse .u8 17

/-- Instance (law `operand_roundTrip`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_roundTrips :
    (parseDecimal (operand (.imm .u8 17))).bind (decodePattern .u8) = some 17 :=
  operand_roundTrip .u8 17 (by decide)

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `operand`, mutant `oversized`). -/
theorem oversized_parse_rejected :
    parseDecimal (oversized (.imm .u8 17)) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `operand`, mutant `oversized`). -/
theorem oversized_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (oversized (.imm .u8 17))).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Emit a digit separator for one valid immediate. -/
def underscored : Operand → String
  | .imm .u8 17 => "1_7"
  | a => operand a

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `operand`, mutant `underscored`). -/
theorem underscored_parse_rejected :
    parseDecimal (underscored (.imm .u8 17)) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `operand`, mutant `underscored`). -/
theorem underscored_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (underscored (.imm .u8 17))).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `operand`, mutant `underscored`). -/
theorem underscored_token_rejected : ¬ DecimalToken (underscored (.imm .u8 17)) 17 :=
  fun h => underscored_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `operand`, mutant `oversized`). -/
theorem oversized_token_rejected : ¬ DecimalToken (oversized (.imm .u8 17)) 17 :=
  fun h => oversized_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Instance (law `operand_roundTrip`, case `signed_zero`). -/
theorem signed_zero_roundTrips :
    (parseDecimal (operand (.imm .i8 0))).bind (decodePattern .i8) = some 0 :=
  operand_roundTrip .i8 0 (by decide)

/-- Planted (law `operand_roundTrip`, case `signed_zero`, subject `operand`, mutant `oversized`). -/
theorem oversized_signed_zero_rejected :
    (parseDecimal (oversized (.imm .i8 0))).bind (decodePattern .i8) = none := by decide

end PatternEvidence

end Concrete.Backend.LLVM
