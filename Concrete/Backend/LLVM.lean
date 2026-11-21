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

def intTy (t : IntTy) : String := s!"i{(toCore t).bits}"

def ty : Ty → String
  | .int t => intTy t
  | .bool => "i1"

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

/-! The decimal integer token subset used at this boundary follows LLVM's
`LLLexer::LexDigitOrNegative`. The emitter uses canonical magnitudes (a single
zero, or an ASCII nonzero digit followed by ASCII digits). -/

def DecimalMagnitude (digits : List Char) : Prop :=
  digits ≠ [] ∧ (∀ c ∈ digits, c.isDigit = true) ∧
    (digits.head? ≠ some '0' ∨ digits = ['0'])

/-- The grammar and value of a complete decimal integer token. -/
def DecimalToken (token : String) (value : Int) : Prop :=
  (DecimalMagnitude token.toList ∧ value = Int.ofNat (Nat.ofDigitChars 10 token.toList 0)) ∨
  ∃ digits, token.toList = '-' :: digits ∧ DecimalMagnitude digits ∧
    value = -Int.ofNat (Nat.ofDigitChars 10 digits 0)

private def parseMagnitude (digits : List Char) : Option Nat :=
  if !digits.isEmpty && digits.all Char.isDigit &&
      (digits.head? != some '0' || digits == ['0']) then
    some (Nat.ofDigitChars 10 digits 0)
  else none

def parseDecimal (token : String) : Option Int :=
  match token.toList with
  | '-' :: digits => (parseMagnitude digits).map fun n => -Int.ofNat n
  | digits => (parseMagnitude digits).map Int.ofNat

private theorem parseMagnitude_exact (digits : List Char) (n : Nat) :
    parseMagnitude digits = some n ↔
      DecimalMagnitude digits ∧ n = Nat.ofDigitChars 10 digits 0 := by
  simp [parseMagnitude, DecimalMagnitude, Bool.and_eq_true, Bool.or_eq_true,
    List.all_eq_true, eq_comm (a := n), and_assoc]

/-- The parser accepts exactly the specified tokens and returns their specified values. -/
theorem parseDecimal_exact (token : String) (value : Int) :
    parseDecimal token = some value ↔ DecimalToken token value := by
  have mapped (digits : List Char) (signed : Nat → Int) :
      (parseMagnitude digits).map signed = some value ↔
        DecimalMagnitude digits ∧ value = signed (Nat.ofDigitChars 10 digits 0) := by
    simp only [Option.map_eq_some_iff, parseMagnitude_exact]
    constructor
    · rintro ⟨_, ⟨hm, rfl⟩, hv⟩
      exact ⟨hm, hv.symm⟩
    · rintro ⟨hm, rfl⟩
      exact ⟨_, ⟨hm, rfl⟩, rfl⟩
  have neg : ∀ digits, ¬ DecimalMagnitude ('-' :: digits) := by
    intro digits h
    have bad := h.2.1 '-' (by simp)
    contradiction
  cases h : token.toList with
  | nil => simp [parseDecimal, DecimalToken, h, parseMagnitude, DecimalMagnitude]
  | cons c digits =>
    by_cases hc : c = '-'
    · subst c
      simp [parseDecimal, DecimalToken, h, neg, mapped]
    · simp [parseDecimal, DecimalToken, h, hc, mapped]

private theorem positive_digits_head (n : Nat) (h : 0 < n) :
    (Nat.toDigits 10 n).head? ≠ some '0' := by
  induction n using Nat.strongRecOn with
  | ind n ih =>
    by_cases small : n < 10
    · rw [Nat.toDigits_of_lt_base small]
      change some n.digitChar ≠ some '0'
      simpa using Nat.ne_of_gt h
    · have large : 10 ≤ n := by omega
      rw [Nat.toDigits_of_base_le (by decide) large]
      cases hd : Nat.toDigits 10 (n / 10) with
      | nil => exact (Nat.toDigits_ne_nil hd).elim
      | cons c cs =>
        simpa [hd] using ih (n / 10) (by omega) (by omega)

private theorem repr_magnitude (n : Nat) : DecimalMagnitude n.repr.toList := by
  rw [Nat.toList_repr]
  refine ⟨Nat.toDigits_ne_nil, fun c hc => Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc, ?_⟩
  cases n with
  | zero => exact Or.inr (Nat.toDigits_zero 10)
  | succ n => exact Or.inl (positive_digits_head (n + 1) (by omega))

def operand : Operand → String
  | .reg r => s!"%r{r}"
  | .imm t v => Nat.repr (pattern t v)

/-- Law. The emitted decimal token parses to exactly the encoded bit pattern.
Authority: docs/decisions.md#od126. -/
theorem operand_parse (t : IntTy) (v : Int) :
    parseDecimal (operand (.imm t v)) = some (Int.ofNat (pattern t v)) := by
  apply (parseDecimal_exact _ _).2
  exact Or.inl ⟨repr_magnitude _, by
    rw [operand, Nat.toList_repr, Nat.ofDigitChars_ten_toDigits]⟩

/-- Law. Every emitted immediate is a token in the decimal grammar. -/
theorem operand_token (t : IntTy) (v : Int) :
    DecimalToken (operand (.imm t v)) (Int.ofNat (pattern t v)) :=
  (parseDecimal_exact _ _).1 (operand_parse t v)

private def decodePattern (t : IntTy) : Int → Option Int
  | .ofNat n => some (ofBits (toCore t) n)
  | .negSucc _ => none

/-- Law. A representable immediate's emitted decimal token denotes its Core value.
The interpretation of the enclosing LLVM text still uses `P_print`.
Authority: docs/decisions.md#od126. -/
theorem operand_roundTrip (t : IntTy) (v : Int) (h : (toCore t).InRange v) :
    (parseDecimal (operand (.imm t v))).bind (decodePattern t) = some v := by
  rw [operand_parse, Option.bind_some, decodePattern, pattern_roundTrip t v h]

def opName : BinOp → String
  | .add => "add"
  | .sub => "sub"
  | .mul => "mul"

def intrinsic (op : BinOp) (t : IntTy) : String :=
  s!"llvm.{if (toCore t).signed then "s" else "u"}{opName op}.with.overflow.{intTy t}"

def inst : Inst → String
  | .bin op t d a b => s!"  %r{d} = {opName op} {intTy t} {operand a}, {operand b}\n"
  | .binOvf op t d f a b =>
    let pair := s!"\{{intTy t}, i1}"
    s!"  %o{d} = call {pair} @{intrinsic op t}({intTy t} {operand a}, {intTy t} {operand b})\n" ++
    s!"  %r{d} = extractvalue {pair} %o{d}, 0\n" ++
    s!"  %r{f} = extractvalue {pair} %o{d}, 1\n"
  | .trunc fr to d a =>
    let source := toCore fr
    let target := toCore to
    let conv := if target.bits < source.bits then "trunc" else if source.signed then "sext" else "zext"
    if source.bits = target.bits then s!"  %r{d} = add {intTy to} {operand a}, 0\n"
    else s!"  %r{d} = {conv} {intTy fr} {operand a} to {intTy to}\n"

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
      s!"[ {operand (args.getD k (.imm .u8 0))}, %b{pred} ]"
    s!"  %r{r} = phi {ty pty} {", ".intercalate arms}\n")

def term (j : Nat) : Term → String
  | .ret a => s!"  %ret{j} = zext i8 {operand a} to i32\n  ret i32 %ret{j}\n"
  | .trap s => s!"  ret i32 {256 + s}\n"
  | .br t _ => s!"  br label %b{t}\n"
  | .condbr c t _ e _ => s!"  br i1 %r{c}, label %b{t}, label %b{e}\n"

/-- The intrinsics the body uses, each declared once. -/
def declarations (p : Program) : String :=
  let used := (p.blocks.flatMap (·.insts)).filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none
  let distinct := used.eraseDups
  String.join (distinct.map fun (op, t) =>
    s!"declare \{{intTy t}, i1} @{intrinsic op t}({intTy t}, {intTy t})\n")

/-- `main`'s body as LLVM text. -/
def print (p : Program) : String :=
  declarations p ++ s!"\ndefine i32 @concrete_body() \{\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{j}:\n" ++ phis p j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := print p ++ "\n" ++ Runtime.llvm p.sites

namespace PatternEvidence

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
  | .imm .u8 17 => "256"
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

end PatternEvidence

end Concrete.Backend.LLVM
