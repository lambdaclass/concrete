import Concrete.Backend.Runtime
import LeanCompilerBackend.Verified.Job
import LeanCompilerBackend.AMD64
import LeanCompilerBackend.AArch64

/-!
# BIR → LCB-IR: the adapter (the first backend proof target)

PROVISIONAL (slice 0). The adapter turns `main`'s body into a function of LCB's verified source
profile (`LeanCompilerBackend.Verified.Source`, `ssa-int-v2`: integer operations, comparisons,
branches and phis, no calls, memory or traps), returning the result word. Its correctness against
LCB's own semantics, `adapter_correct`, is the first backend proof target; until it is proved it is
premise `P_adapter` (`Concrete/README.md`).

Representation: an integer of type `t` is the 32-bit word `BitVec.ofInt 32 v`; a flag is the word
0 or 1. Slice 0's profile covers the types of at most 16 bits, whose exact results fit a word:

* `bin op t d a b`: `e := a op b`, `x := e - min`, `m := x & (2^bits - 1)`, `d := m + min`
  (`wrap t (a op b)`, two's complement for a signed type);
* `binOvf` also sets `l := x <u 2^bits` and `f := (l = 0)`: out of range exactly when
  `(a op b) - min` is not below `2^bits`;
* `trunc from to d a`: `x := a - to.min`, `m := x & (2^to.bits - 1)`, `d := m + to.min`;
* a block parameter is a phi over its predecessors' arguments, ordered by predecessor;
* `ret a` returns `a`; `trap s` returns `256 + s`.

`assemble` hands the `Source.Function` value to LCB IN PROCESS: no text is printed or parsed on the
path the adapter proof covers. On `aarch64-macos` it goes through LCB's verified first path
(`Verified.compileFunction`: source validation, stack MIR, typed A64, rendering); on the other
targets through LCB's compatibility backends, reached with LCB's own `Verified.reify`. LCB's
lowering, assembly rendering, the assembler and the linker are inside `P_toolchain[lcb]`. `render`
prints the function as QBE text only as a debug dump.
-/

namespace Concrete.Backend.LCB
open Concrete.BIR
open ConcreteSpec (IntTy Trap)
open Concrete.Backend.Runtime (indexed)
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

/-- The integer types slice 0's LCB profile covers: exact results of every `BinOp` on two
in-range operands fit a 32-bit word. -/
def inProfile (t : IntTy) : Bool := t.bits ≤ 16

def word (n : Int) : Source.Value := .word (BitVec.ofInt 32 n)

/-- How many LCB values an instruction defines. -/
def Inst.width : Inst → Nat
  | .bin .. => 4
  | .binOvf .. => 6
  | .trunc .. => 3

/-- The LCB value id of every BIR register, numbered in block order: each block's parameters,
then each instruction's values (its result is the last but one for `binOvf`'s `d`, the last for
`f`, the last for `bin` and `trunc`). -/
def numbering (p : Program) : List (Reg × Nat) :=
  let step := fun (acc : List (Reg × Nat) × Nat) (b : Block) =>
    let (m, n) := acc
    let (m, n) := b.params.foldl (fun (m, n) (r, _) => ((r, n) :: m, n + 1)) (m, n)
    b.insts.foldl (fun (m, n) i => match i with
      | .bin _ _ d _ _ => ((d, n + 3) :: m, n + 4)
      | .binOvf _ _ d f _ _ => ((f, n + 5) :: (d, n + 3) :: m, n + 6)
      | .trunc _ _ d _ => ((d, n + 2) :: m, n + 3)) (m, n)
  (p.blocks.foldl step ([], 0)).1

/-- The number of LCB values the body defines. -/
def valueCount (p : Program) : Nat :=
  p.blocks.foldl (fun n b => n + b.params.length +
    b.insts.foldl (fun n i => n + Inst.width i) 0) 0

def idOf (m : List (Reg × Nat)) (r : Reg) : Except String Nat :=
  match m.find? (·.1 == r) with
  | some (_, n) => .ok n
  | none => .error s!"register %r{r} has no definition"

def operand (m : List (Reg × Nat)) : Operand → Except String Source.Operand
  | .reg r => return .local (← idOf m r)
  | .imm _ v => return .literal (word v)

def opBinary : BinOp → Source.Binary
  | .add => .add
  | .sub => .sub
  | .mul => .mul

def ins (d : Nat) (op : Source.Operation) : Source.Instruction :=
  { destination := d, width := .word, operation := op }

/-- An instruction's LCB instructions, its first value numbered `n`. -/
def inst (m : List (Reg × Nat)) (n : Nat) : Inst → Except String (List Source.Instruction)
  | .bin op t _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operand m a
    let b ← operand m b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))]
  | .binOvf op t _ _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operand m a
    let b ← operand m b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min))),
      ins (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
      ins (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))]
  | .trunc fr to _ a => do
    unless inProfile fr && inProfile to do throw s!"{repr fr} → {repr to} is outside LCB's slice-0 profile"
    let a ← operand m a
    return [ins n (.binary .sub a (.literal (word to.min))),
      ins (n + 1) (.binary .andBits (.local n) (.literal (word (2 ^ to.bits - 1)))),
      ins (n + 2) (.binary .add (.local (n + 1)) (.literal (word to.min)))]

/-- The edges into block `j`: (predecessor, arguments), by predecessor. -/
def incoming (p : Program) (j : Nat) : List (Nat × List Operand) :=
  (indexed p.blocks).flatMap fun (k, b) =>
    match b.term with
    | .br t args => if t = j then [(k, args)] else []
    | .condbr _ t ta e ea => (if t = j then [(k, ta)] else []) ++ (if e = j then [(k, ea)] else [])
    | _ => []

def term (m : List (Reg × Nat)) : Term → Except String Source.Terminator
  | .ret a => return .ret (← operand m a)
  | .trap s => return .ret (.literal (word (256 + s)))
  | .br t _ => return .jump t
  | .condbr c t _ e _ => return .branch (.local (← idOf m c)) t e

/-- `main`'s body as a function of LCB's verified source profile. -/
def adapt (p : Program) : Except String Source.Function := do
  let m := numbering p
  let blocks ← (indexed p.blocks).foldlM (fun (acc : Array Source.Block × Nat) (j, b) => do
    let (out, n) := acc
    let phis ← (indexed b.params).mapM fun (k, (r, _)) => do
      let arms ← (incoming p j).mapM fun (pred, args) => do
        let a ← match args[k]? with
          | some a => operand m a
          | none => throw s!"block {pred} passes too few arguments to block {j}"
        pure (pred, a)
      pure ({ destination := ← idOf m r, width := .word, incoming := arms.toArray } : Source.Phi)
    let n := n + b.params.length
    let (insts, n) ← b.insts.foldlM (fun (acc : Array Source.Instruction × Nat) i => do
      let (is, n) := acc
      let new ← inst m n i
      pure (is ++ new.toArray, n + Inst.width i)) (#[], n)
    pure (out.push { phis := phis.toArray, instructions := insts, terminator := ← term m b.term }, n))
    (#[], 0)
  return { resultWidth := .word, parameters := #[],
           valueWidths := Array.replicate (valueCount p) .word, blocks := blocks.1 }

/-! ## Assembly, in process (inside `P_toolchain[lcb]`) -/

/-- The targets the gate runs LCB's output on. -/
inductive Target where
  | x86_64Linux | aarch64Linux | aarch64Macos
  deriving DecidableEq, Repr

def Target.ofName : String → Option Target
  | "x86_64-linux" => some .x86_64Linux
  | "aarch64-linux" => some .aarch64Linux
  | "aarch64-macos" => some .aarch64Macos
  | _ => none

/-- The body's symbol: the runtime entry calls `concrete_body`. -/
def bodySymbol : LeanCompilerBackend.Verified.Symbol := { namespaceName := "concrete", name := "concrete_body", exported := true }

/-- The adapted body as assembly text for `target`, computed in process from the value. -/
def assemble (target : Target) (f : Source.Function) : Except String String :=
  match target with
  | .aarch64Macos => do
    let job : LeanCompilerBackend.Verified.FunctionJob :=
      { code := { symbol := bodySymbol, body := f, abi := LeanCompilerBackend.Verified.planAbi f,
                  layout := LeanCompilerBackend.Verified.planFrame f } }
    let artifact ← (LeanCompilerBackend.Verified.compileFunction job).mapError (fun e => s!"LCB first path: {repr e}")
    return ".text\n" ++ artifact.code.fragment ++ ".subsections_via_symbols\n"
  | .x86_64Linux | .aarch64Linux => do
    let m : LeanCompilerBackend.Module :=
      { functions := #[LeanCompilerBackend.Verified.reify f bodySymbol.name bodySymbol.exported] }
    LeanCompilerBackend.validateModule m
    match target with
    | .x86_64Linux => LeanCompilerBackend.AMD64.emitModuleValidated m false false
    | _ => LeanCompilerBackend.AArch64.emitModuleValidated m false false

/-! ## `adapter_correct`, value level

The first layer of the adapter proof: each BIR instruction's expansion computes, in LCB's 32-bit
words, the encoding of the BIR result. `enc` is the value representation; `wrap_word` and
`flag_word` are the two facts the instruction expansions rest on. -/

namespace Proof

/-- How a BIR value is represented in LCB: an integer as its 32-bit word, a flag as 0 or 1. -/
def enc : Val → Source.Value
  | .int _ v => word v
  | .bool b => word (if b then 1 else 0)

theorem ofInt_sub (x y : Int) :
    BitVec.ofInt 32 (x - y) = BitVec.ofInt 32 x - BitVec.ofInt 32 y := by
  have h : BitVec.ofInt 32 x = BitVec.ofInt 32 (x - y) + BitVec.ofInt 32 y := by
    rw [← BitVec.ofInt_add]; congr 1; omega
  rw [h, BitVec.add_sub_cancel]

theorem binaryBits_exact (op : BinOp) (x y : Int) :
    Source.binaryBits (opBinary op) (BitVec.ofInt 32 x) (BitVec.ofInt 32 y) =
      BitVec.ofInt 32 (op.exact x y) := by
  cases op <;> simp [Source.binaryBits, opBinary, BinOp.exact, BitVec.ofInt_add, BitVec.ofInt_mul,
    ofInt_sub]

/-- The profile's types, by cases. -/
theorem inProfile_cases {t : IntTy} (ht : inProfile t = true) :
    t = .u8 ∨ t = .i8 ∨ t = .u16 ∨ t = .i16 := by
  cases t <;> simp_all [inProfile, IntTy.bits]

/-- Masking with `2^b - 1` is reduction modulo `2^b`, for the profile's widths. -/
theorem mask_lit (b : Nat) (hb : b = 8 ∨ b = 16) (z : Int) :
    BitVec.ofInt 32 z &&& BitVec.ofInt 32 (2 ^ b - 1) = BitVec.ofInt 32 (z % 2 ^ b) := by
  apply BitVec.eq_of_toNat_eq
  have hc : (BitVec.ofInt 32 (2 ^ b - 1)).toNat = 2 ^ b - 1 := by
    rcases hb with rfl | rfl <;> decide
  rw [BitVec.toNat_and, hc, Nat.and_two_pow_sub_one_eq_mod, BitVec.toNat_ofInt, BitVec.toNat_ofInt]
  rcases hb with rfl | rfl <;>
  · simp only [Nat.reducePow, Int.reducePow]
    omega

theorem mask_ofInt {t : IntTy} (ht : inProfile t = true) (z : Int) :
    BitVec.ofInt 32 z &&& BitVec.ofInt 32 (2 ^ t.bits - 1) = BitVec.ofInt 32 (z % 2 ^ t.bits) := by
  rcases inProfile_cases ht with rfl | rfl | rfl | rfl <;>
    exact mask_lit _ (by simp [IntTy.bits]) z

/-- The `bin`/`binOvf`/`trunc` expansion's result word is the word of `wrap t z`. -/
theorem wrap_word {t : IntTy} (ht : inProfile t = true) (z : Int) :
    ((BitVec.ofInt 32 z - BitVec.ofInt 32 t.min) &&& BitVec.ofInt 32 (2 ^ t.bits - 1)) +
      BitVec.ofInt 32 t.min = BitVec.ofInt 32 (wrap t z) := by
  rw [← ofInt_sub, mask_ofInt ht, ← BitVec.ofInt_add, wrap]

/-- In range for a profile type: magnitude at most `2^16 - 1`. -/
theorem natAbs_le_of_inRange {t : IntTy} (ht : inProfile t = true) {x : Int}
    (hx : t.inRange x = true) : x.natAbs ≤ 65535 := by
  rcases inProfile_cases ht with rfl | rfl | rfl | rfl <;>
  · simp [IntTy.inRange, IntTy.min, IntTy.max, IntTy.bits, IntTy.signed] at hx
    omega

/-- Every exact result over a profile type fits well inside a 32-bit word. -/
theorem exact_natAbs_le {t : IntTy} (ht : inProfile t = true) (op : BinOp) {x y : Int}
    (hx : t.inRange x = true) (hy : t.inRange y = true) :
    (op.exact x y).natAbs ≤ 4294836225 := by
  have bx := natAbs_le_of_inRange ht hx
  have by' := natAbs_le_of_inRange ht hy
  cases op with
  | add => simp only [BinOp.exact]; omega
  | sub => simp only [BinOp.exact]; omega
  | mul =>
    simp only [BinOp.exact]
    rw [Int.natAbs_mul]
    exact Nat.le_trans (Nat.mul_le_mul bx by') (by decide)

/-- The `binOvf` flag: `(a op b) - min` is below `2^bits` as an unsigned word exactly when the
result is in range. -/
theorem flag_word {t : IntTy} (ht : inProfile t = true) (op : BinOp) {x y : Int}
    (hx : t.inRange x = true) (hy : t.inRange y = true) :
    decide ((BitVec.ofInt 32 (op.exact x y) - BitVec.ofInt 32 t.min).toNat < 2 ^ t.bits) =
      t.inRange (op.exact x y) := by
  have hb := exact_natAbs_le ht op hx hy
  rw [← ofInt_sub, BitVec.toNat_ofInt]
  generalize op.exact x y = r at hb ⊢
  rcases inProfile_cases ht with rfl | rfl | rfl | rfl <;>
  · simp only [IntTy.inRange, IntTy.min, IntTy.max, IntTy.bits, IntTy.signed, Bool.false_eq_true, ↓reduceIte,
      Nat.reduceSub, Nat.reducePow, Int.reducePow, decide_eq_decide]
    omega

end Proof

/-! ## The QBE debug dump (not on the build path) -/

def renderWidth : Width → String
  | .word => "w"
  | .long => "l"

def renderOperand : Source.Operand → String
  | .local id => s!"%v{id}"
  | .literal (.word b) => toString b.toNat
  | .literal (.long b) => toString b.toNat

def renderBinary : Source.Binary → String
  | .add => "add" | .sub => "sub" | .mul => "mul"
  | .andBits => "and" | .orBits => "or" | .xorBits => "xor"
  | .shiftLeft => "shl" | .logicalRight => "shr" | .arithmeticRight => "sar"
  -- no QBE spelling (LCB admits them only from a job); debug names
  | .mulHighUnsigned => "umulh" | .mulHighSigned => "smulh"

def renderDivision : Source.Division → String
  | .unsignedDiv => "udiv" | .unsignedRem => "urem" | .signedDiv => "div" | .signedRem => "rem"

def renderOperation : Source.Operation → String
  | .copy a => s!"copy {renderOperand a}"
  | .binary op a b => s!"{renderBinary op} {renderOperand a}, {renderOperand b}"
  | .compare op w a b =>
    let c := match op with
      | .eq => "ceq"
      | .unsignedLT => "cult"
      | .unsignedLE => "cule"
      | .signedLT => "cslt"
      | .signedLE => "csle"
    s!"{c}{renderWidth w} {renderOperand a}, {renderOperand b}"
  | .divide op a b => s!"{renderDivision op} {renderOperand a}, {renderOperand b}"

def renderTerminator : Source.Terminator → String
  | .ret a => s!"    ret {renderOperand a}\n"
  | .jump t => s!"    jmp @b{t}\n"
  | .branch c y n => s!"    jnz {renderOperand c}, @b{y}, @b{n}\n"

/-- The body as QBE text for `lcb`. -/
def render (f : Source.Function) : String :=
  let blocks := (indexed f.blocks.toList).map fun (j, b) =>
    s!"@b{j}\n" ++
    String.join (b.phis.toList.map fun ph =>
      let arms := ph.incoming.toList.map fun (pred, a) => s!"@b{pred} {renderOperand a}"
      s!"    %v{ph.destination} ={renderWidth ph.width} phi {", ".intercalate arms}\n") ++
    String.join (b.instructions.toList.map fun i =>
      s!"    %v{i.destination} ={renderWidth i.width} {renderOperation i.operation}\n") ++
    renderTerminator b.terminator
  "export function w $concrete_body() {\n" ++ String.join blocks ++ "}\n"

/-! ## The in-process leg: LCB's own evaluator against BIR -/

/-- The result word LCB's semantics gives the adapted body, with `fuel` steps. -/
def evalBody (f : Source.Function) (fuel : Nat) : Option Nat :=
  match Source.evaluateFunction f #[] fuel with
  | .returned (.word w) => some w.toNat
  | _ => none

end Concrete.Backend.LCB
