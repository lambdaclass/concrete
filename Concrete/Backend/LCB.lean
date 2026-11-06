import Concrete.Backend.Runtime
import LeanCompilerBackend.Verified.Source

/-!
# BIR → LCB-IR: the adapter (the first backend proof target)

PROVISIONAL (slice 0). The adapter turns `main`'s body into a function of LCB's verified source
profile (`LeanCompilerBackend.Verified.Source`, `ssa-int-v1`: integer operations, comparisons,
branches and phis, no calls, memory or traps), returning the result word. Its correctness against
LCB's own semantics, `adapter_correct`, is the first backend proof target; until it is proved it is
premise `P_adapter` (`Concrete/README.md`).

Representation: an integer of type `t` is the 32-bit word `BitVec.ofInt 32 v`; a flag is the word
0 or 1. Slice 0's profile covers the types of at most 16 bits, whose exact results fit a word:

* `bin op t d a b`: `e := a op b`, `x := e - min`, `m := x & (2^bits - 1)`, `d := m + min`
  (`wrap t (a op b)`, two's complement for a signed type);
* `binOvf` also sets `l := x <u 2^bits` and `f := (l = 0)`: out of range exactly when
  `(a op b) - min` is not below `2^bits`;
* a block parameter is a phi over its predecessors' arguments, ordered by predecessor;
* `ret a` returns `a`; `trap s` returns `256 + s`.

`render` prints the function as QBE text for LCB's command line. That printing, LCB's importer,
lowering, assembler and linker are inside `P_toolchain[lcb]`.
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

/-- The LCB value id of every BIR register, numbered in block order: each block's parameters,
then each instruction's values (its result is the last but one for `binOvf`'s `d`, the last for
`f`). -/
def numbering (p : Program) : List (Reg × Nat) :=
  let step := fun (acc : List (Reg × Nat) × Nat) (b : Block) =>
    let (m, n) := acc
    let (m, n) := b.params.foldl (fun (m, n) (r, _) => ((r, n) :: m, n + 1)) (m, n)
    b.insts.foldl (fun (m, n) i => match i with
      | .bin _ _ d _ _ => ((d, n + 3) :: m, n + 4)
      | .binOvf _ _ d f _ _ => ((f, n + 5) :: (d, n + 3) :: m, n + 6)) (m, n)
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

/-! ## Printing for LCB's command line (inside `P_toolchain[lcb]`) -/

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

def renderOperation : Source.Operation → String
  | .copy a => s!"copy {renderOperand a}"
  | .binary op a b => s!"{renderBinary op} {renderOperand a}, {renderOperand b}"
  | .compare op w a b =>
    let c := match op with
      | .eq => "ceq"
      | .unsignedLT => "cult"
    s!"{c}{renderWidth w} {renderOperand a}, {renderOperand b}"

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

/-- The whole QBE module for `lcb`: the body and the runtime entry. -/
def module (p : Program) : Except String String := do
  return render (← adapt p) ++ "\n" ++ Runtime.qbe p.sites

/-! ## The in-process leg: LCB's own evaluator against BIR -/

/-- The result word LCB's semantics gives the adapted body, with `fuel` steps. -/
def evalBody (f : Source.Function) (fuel : Nat) : Option Nat :=
  match Source.evaluateFunction f #[] fuel with
  | .returned (.word w) => some w.toNat
  | _ => none

end Concrete.Backend.LCB
