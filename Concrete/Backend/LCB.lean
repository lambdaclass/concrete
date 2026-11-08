import Concrete.Backend.Runtime
import Concrete.BIR.Progress
import LeanCompilerBackend.Verified.Job
import LeanCompilerBackend.AMD64
import LeanCompilerBackend.AArch64

/-!
# BIR → LCB-IR: the adapter (the first backend proof target)

PROVISIONAL (slice 0). The adapter turns `main`'s body into a function of LCB's verified source
profile (`LeanCompilerBackend.Verified.Source`, `ssa-int-v2`: integer operations, comparisons,
branches and phis, no calls, memory or traps), returning the result word. Its correctness against
LCB's own semantics is proved: `Proof.adapter_correct` for an accepted program, and the complete
backend contract `Proof.emit_correct` / `Proof.artifact_correct` (a refused adaptation produces no
artifact), which retired premise `P_adapter` (`Concrete/README.md`).

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

/-! The numbering is a closed function of position: block `j`'s values start at `blockBase p j`;
its parameters come first, then each instruction's values, instruction `i` at `instBase b i`.
This is LCB's canonical order (block, phi, instruction). -/

def blockSize (b : Block) : Nat := b.params.length + (b.insts.map Inst.width).sum

def blockBase (p : Program) (j : Nat) : Nat := ((p.blocks.take j).map blockSize).sum

def instBase (b : Block) (i : Nat) : Nat := b.params.length + ((b.insts.take i).map Inst.width).sum

/-- The number of LCB values the body defines. -/
def valueCount (p : Program) : Nat := (p.blocks.map blockSize).sum

/-- The registers an instruction defines, with their offset among its values. -/
def instDefs : Inst → List (Reg × Nat)
  | .bin _ _ d _ _ => [(d, 3)]
  | .binOvf _ _ d f _ _ => [(d, 3), (f, 5)]
  | .trunc _ _ d _ => [(d, 2)]

/-- Every register definition with its LCB value id, in definition order. -/
def ids (p : Program) : List (Reg × Nat) :=
  (indexed p.blocks).flatMap fun (j, b) =>
    (indexed b.params).map (fun (k, (r, _)) => (r, blockBase p j + k)) ++
    (indexed b.insts).flatMap fun (i, ins) =>
      (instDefs ins).map fun (r, off) => (r, blockBase p j + instBase b i + off)

/-- The offsets of an instruction's intermediate values (those no register names). -/
def interOffsets : Inst → List Nat
  | .bin .. => [0, 1, 2]
  | .binOvf .. => [0, 1, 2, 4]
  | .trunc .. => [0, 1]

/-- Every intermediate value's id. -/
def intermediates (p : Program) : List Nat :=
  (indexed p.blocks).flatMap fun (j, b) => (indexed b.insts).flatMap fun (i, ins) =>
    (interOffsets ins).map (blockBase p j + instBase b i + ·)

/-- The numbering invariants the simulation proof uses, checked on every program `adapt`
accepts (translation validation): registers and their ids are both duplicate-free, every id is
below `valueCount`, and no intermediate value shares an id with a register. -/
def numberingOk (p : Program) : Bool :=
  let slots := (ids p).map Prod.snd
  decide ((ids p).map Prod.fst).Nodup && decide slots.Nodup &&
  slots.all (· < valueCount p) && (intermediates p).all (· < valueCount p) &&
  (intermediates p).all (fun m => !slots.contains m)

/-- The LCB value id of a register. `adapt` refuses a program where a register has two
definitions, so this is a function of the register. -/
def regId (p : Program) (r : Reg) : Option Nat := (ids p).lookup r

def idOfWith (lookup : Reg → Option Nat) (r : Reg) : Except String Nat :=
  match lookup r with
  | some n => .ok n
  | none => .error s!"register %r{r} has no definition"

def idOf (p : Program) := idOfWith (regId p)

def operandWith (lookup : Reg → Option Nat) : Operand → Except String Source.Operand
  | .reg r => return .local (← idOfWith lookup r)
  | .imm _ v => return .literal (word v)

def operand (p : Program) := operandWith (regId p)

theorem operand_eq_with (p : Program) : operand p = operandWith (regId p) := rfl
theorem idOf_eq_with (p : Program) : idOf p = idOfWith (regId p) := rfl

def opBinary : BinOp → Source.Binary
  | .add => .add
  | .sub => .sub
  | .mul => .mul

def ins (d : Nat) (op : Source.Operation) : Source.Instruction :=
  { destination := d, width := .word, operation := op }

/-- An instruction's LCB instructions, its first value numbered `n`. -/
def instWith (lookup : Reg → Option Nat) (n : Nat) : Inst → Except String (List Source.Instruction)
  | .bin op t _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operandWith lookup a
    let b ← operandWith lookup b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))]
  | .binOvf op t _ _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operandWith lookup a
    let b ← operandWith lookup b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min))),
      ins (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
      ins (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))]
  | .trunc fr to _ a => do
    unless inProfile fr && inProfile to do throw s!"{repr fr} → {repr to} is outside LCB's slice-0 profile"
    let a ← operandWith lookup a
    return [ins n (.binary .sub a (.literal (word to.min))),
      ins (n + 1) (.binary .andBits (.local n) (.literal (word (2 ^ to.bits - 1)))),
      ins (n + 2) (.binary .add (.local (n + 1)) (.literal (word to.min)))]

/-- The edges into block `j` from block `k`, whose terminator is `term`: (k, arguments). -/
def inst (p : Program) := instWith (regId p)

def edgesTo (j k : Nat) : Term → List (Nat × List Operand)
  | .br t args => if t = j then [(k, args)] else []
  | .condbr _ t ta e ea => (if t = j then [(k, ta)] else []) ++ (if e = j then [(k, ea)] else [])
  | _ => []

/-- The edges into block `j`: (predecessor, arguments), by predecessor. -/
def incoming (p : Program) (j : Nat) : List (Nat × List Operand) :=
  (indexed p.blocks).flatMap fun (k, b) => edgesTo j k b.term

def termWith (lookup : Reg → Option Nat) : Term → Except String Source.Terminator
  | .ret a => return .ret (← operandWith lookup a)
  | .trap s => return .ret (.literal (word (256 + s)))
  | .br t _ => return .jump t
  | .condbr c t _ e _ => return .branch (.local (← idOfWith lookup c)) t e

/-- Block `j`'s phi for its parameter `k`: one arm per incoming edge, by predecessor. -/
def term (p : Program) := termWith (regId p)

def adaptPhi (p : Program) (j k : Nat) : Except String Source.Phi := do
  let arms ← (incoming p j).mapM fun (pred, args) => do
    let a ← match args[k]? with
      | some a => operand p a
      | none => throw s!"block {pred} passes too few arguments to block {j}"
    pure (pred, a)
  pure { destination := blockBase p j + k, width := .word, incoming := arms.toArray }

/-- Instruction `i` of block `j`, numbered from its slot. -/
def adaptInst (p : Program) (j : Nat) (b : Block) : Nat × Inst → Except String (List Source.Instruction)
  | (i, ins) => inst p (blockBase p j + instBase b i) ins

/-- Block `j`: its phis, its instructions' expansions in order, its terminator. -/
def adaptBlock (p : Program) : Nat × Block → Except String Source.Block
  | (j, b) => do
    let phis ← (indexed b.params).mapM fun (k, _) => adaptPhi p j k
    let insts ← (indexed b.insts).mapM (adaptInst p j b)
    pure { phis := phis.toArray, instructions := insts.flatten.toArray, terminator := ← term p b.term }

/-- `main`'s body as a function of LCB's verified source profile. -/
def adaptSpec (p : Program) : Except String Source.Function := do
  unless numberingOk p do throw "the numbering is not injective (a register defined twice?)"
  let blocks ← (indexed p.blocks).mapM (adaptBlock p)
  return { resultWidth := .word, parameters := #[],
           valueWidths := Array.replicate (valueCount p) .word, blocks := blocks.toArray }

/-- The reference LCB emit, retained for the executable/reference equality. -/
def emitSpec (p : Program) : Except String (Source.Function × SiteTable) := do
  let f ← adaptSpec p
  return (f, p.sites)

/-! ## Prepared numbering and predecessors -/

structure InstNumberingData where
  ids : List (Reg × Nat)
  intermediates : List Nat
  offsets : List Nat

structure BlockNumberingData where
  ids : List (Reg × Nat)
  intermediates : List Nat
  instOffsets : List Nat

structure NumberingData where
  ids : List (Reg × Nat)
  intermediates : List Nat
  blocks : List (Nat × List Nat)

def instNumbering (base : Nat) : List Inst → InstNumberingData
  | [] => ⟨[], [], []⟩
  | ins :: rest =>
    let tail := instNumbering (base + Inst.width ins) rest
    ⟨(instDefs ins).map (fun (r, off) => (r, base + off)) ++ tail.ids,
      (interOffsets ins).map (base + ·) ++ tail.intermediates, base :: tail.offsets⟩

def blockNumbering (base : Nat) (b : Block) : BlockNumberingData :=
  let tail := instNumbering (base + b.params.length) b.insts
  ⟨(indexed b.params).map (fun (k, (r, _)) => (r, base + k)) ++ tail.ids,
    tail.intermediates, tail.offsets⟩

def numberingFrom (base : Nat) : List Block → NumberingData
  | [] => ⟨[], [], []⟩
  | b :: rest =>
    let head := blockNumbering base b
    let tail := numberingFrom (base + blockSize b) rest
    ⟨head.ids ++ tail.ids, head.intermediates ++ tail.intermediates,
      (base, head.instOffsets) :: tail.blocks⟩

def numbering (p : Program) : NumberingData := numberingFrom 0 p.blocks

/-- The same prepared data, expressed by the closed reference functions. -/
def numberingSpec (p : Program) : NumberingData :=
  ⟨ids p, intermediates p, (indexed p.blocks).map fun (j, b) =>
    (blockBase p j, (indexed b.insts).map fun (i, _) => blockBase p j + instBase b i)⟩

/-- One more than the largest register, including every malformed register. -/
def regBound : List (Reg × Nat) → Nat
  | [] => 0
  | (r, _) :: rest => max (r + 1) (regBound rest)

/-- Mark previously unseen natural numbers, refusing a duplicate or an out-of-bounds number. -/
def markUnique (xs : List Nat) (a : Array Bool) : Option (Array Bool) :=
  match xs with
  | [] => some a
  | x :: xs =>
    if h : x < a.size then
      if a[x] then none else markUnique xs (a.setIfInBounds x true)
    else none

def regCheck (ids : List (Reg × Nat)) : Bool :=
  let regs := ids.map Prod.fst
  (markUnique regs (Array.replicate (regBound ids) false)).isSome

def slotInterCheck (slots inters : List Nat) (count : Nat) : Bool :=
  match markUnique slots (Array.replicate count false) with
  | none => false
  | some a => inters.all fun m => if h : m < a.size then !a[m] else false

def numberingOkFastData (data : NumberingData) (count : Nat) : Bool :=
  regCheck data.ids && slotInterCheck (data.ids.map Prod.snd) data.intermediates count

/-- Targeted incoming edges, before grouping by target. -/
def targetedEdges (p : Program) : List (Nat × (Nat × List Operand)) :=
  (indexed p.blocks).flatMap fun (k, b) =>
    match b.term with
    | .br t args => [(t, (k, args))]
    | .condbr _ t ta e ea => [(t, (k, ta)), (e, (k, ea))]
    | _ => []

def incomingBound : List (Nat × (Nat × List Operand)) → Nat
  | [] => 0
  | e :: es => max (e.1 + 1) (incomingBound es)

def addIncoming (es : List (Nat × (Nat × List Operand)))
    (bs : Array (List (Nat × List Operand))) : Array (List (Nat × List Operand)) :=
  match es with
  | [] => bs
  | e :: es =>
    let bs := if h : e.1 < bs.size then bs.setIfInBounds e.1 (e.2 :: bs[e.1]) else bs
    addIncoming es bs

def incomingBuckets (p : Program) : Array (List (Nat × List Operand)) :=
  let es := targetedEdges p
  addIncoming es (Array.replicate (incomingBound es) [])

def incomingBucket (bs : Array (List (Nat × List Operand))) (j : Nat) : List (Nat × List Operand) :=
  (bs[j]?).getD []

structure Prepared where
  data : NumberingData
  offsets : Array (Nat × Array Nat)
  regs : Array (Option Nat)
  incoming : Array (List (Nat × List Operand))
  ok : Bool

/-- Insert only an absent register, so the first definition wins exactly as in `List.lookup`. -/
def addRegIds (ids : List (Reg × Nat)) (a : Array (Option Nat)) : Array (Option Nat) :=
  match ids with
  | [] => a
  | (r, n) :: ids =>
    let a := if h : r < a.size then
      if a[r].isSome then a else a.setIfInBounds r (some n)
    else a
    addRegIds ids a

def regTable (data : NumberingData) : Array (Option Nat) :=
  addRegIds data.ids (Array.replicate (regBound data.ids) none)

def blockOffset (prep : Prepared) (j : Nat) : Nat :=
  (prep.offsets[j]?.map (·.1)).getD 0

def instOffset (prep : Prepared) (j i : Nat) : Nat :=
  ((prep.offsets[j]?.map (·.2)).getD #[])[i]?.getD 0

def prepare (p : Program) : Prepared :=
  let data := numbering p
  { data, offsets := (data.blocks.map fun (base, offsets) => (base, offsets.toArray)).toArray,
    regs := regTable data, incoming := incomingBuckets p,
    ok := numberingOkFastData data (valueCount p) }

def regIdFast (prep : Prepared) (r : Reg) : Option Nat := (prep.regs[r]?).getD none

def idOfFast (prep : Prepared) := idOfWith (regIdFast prep)

def operandFast (prep : Prepared) := operandWith (regIdFast prep)

def instFast (prep : Prepared) (n : Nat) := instWith (regIdFast prep) n

def termFast (prep : Prepared) := termWith (regIdFast prep)

def adaptPhiFast (prep : Prepared) (j k : Nat) : Except String Source.Phi := do
  let arms ← (incomingBucket prep.incoming j).reverse.mapM fun (pred, args) => do
    let a ← match args[k]? with
      | some a => operandFast prep a
      | none => throw s!"block {pred} passes too few arguments to block {j}"
    pure (pred, a)
  pure { destination := blockOffset prep j + k, width := .word, incoming := arms.toArray }

def adaptInstFast (prep : Prepared) (j : Nat) : Nat × Inst → Except String (List Source.Instruction)
  | (i, ins) => instFast prep (instOffset prep j i) ins

def adaptBlockFast (prep : Prepared) : Nat × Block → Except String Source.Block
  | (j, b) => do
    let phis ← (indexed b.params).mapM fun (k, _) => adaptPhiFast prep j k
    let insts ← (indexed b.insts).mapM (adaptInstFast prep j)
    pure { phis := phis.toArray, instructions := insts.flatten.toArray, terminator := ← termFast prep b.term }

/-- The executable adapter: all tables and validation are prepared once per program. -/
def adaptFast (p : Program) : Except String Source.Function := do
  let prep := prepare p
  unless prep.ok do throw "the numbering is not injective (a register defined twice?)"
  let blocks ← (indexed p.blocks).mapM (adaptBlockFast prep)
  return { resultWidth := .word, parameters := #[],
           valueWidths := Array.replicate (valueCount p) .word, blocks := blocks.toArray }

def adapt (p : Program) : Except String Source.Function := adaptFast p

/-- The LCB backend's emit: the adapted body paired with the program's site table (the table that
decodes the body's result is part of the emitted artifact), or the adapter's refusal, in which case
no artifact exists (`artifact` and the emitter build only from `.ok`). `Proof.emit_correct` and
`Proof.artifact_correct` are the backend contract: no premise. -/
def emit (p : Program) : Except String (Source.Function × SiteTable) := do
  let f ← adapt p
  return (f, p.sites)

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

/-- The LCB body artifact for `target`: the assembly of a successful `emit`, or its refusal. -/
def artifact (target : Target) (p : Program) : Except String String := do
  let (f, _) ← emit p
  assemble target f

/-! ## `adapter_correct`, value level

The first layer of the adapter proof: each BIR instruction's expansion computes, in LCB's 32-bit
words, the encoding of the BIR result. `enc` is the value representation; `wrap_word` and
`flag_word` are the two facts the instruction expansions rest on. -/

/-- What the adapted body produces, from LCB's own source semantics with no arguments: `returns w`
iff `Source.HasBehavior f #[] (.returned (.word w))`, `diverges` iff
`Source.HasBehavior f #[] .diverges`, and `stuck` when neither holds (an `initial` fault, a `.long`
return, a stuck fault). LCB's behaviour is unique (`Proof.hasBehavior_unique`), so exactly one
holds. -/
def bodyResult (f : Source.Function) : BodyResult → Prop
  | .returns w => Source.HasBehavior f #[] (.returned (.word w))
  | .diverges => Source.HasBehavior f #[] .diverges
  | .stuck => (∀ w, ¬ Source.HasBehavior f #[] (.returned (.word w))) ∧
      ¬ Source.HasBehavior f #[] .diverges

namespace Proof

/-! ## Prepared-data reference equalities -/

def instSpecFrom (b : Block) (base i : Nat) : InstNumberingData :=
  let xs := Runtime.indexedFrom i (b.insts.drop i)
  ⟨xs.flatMap fun (q, ins) => (instDefs ins).map fun (r, off) =>
      (r, base + instBase b q + off),
    xs.flatMap fun (q, ins) => (interOffsets ins).map (base + instBase b q + ·),
    xs.map fun (q, _) => base + instBase b q⟩

def blockSpec (base : Nat) (b : Block) : BlockNumberingData :=
  let tail := instSpecFrom b base 0
  ⟨(indexed b.params).map (fun (k, (r, _)) => (r, base + k)) ++ tail.ids,
    tail.intermediates, tail.offsets⟩

def numberingSpecFrom (p : Program) (j : Nat) : NumberingData :=
  let bs := Runtime.indexedFrom j (p.blocks.drop j)
  ⟨bs.flatMap fun (q, b) => (blockSpec (blockBase p q) b).ids,
    bs.flatMap fun (q, b) => (blockSpec (blockBase p q) b).intermediates,
    bs.map fun (q, b) => (blockBase p q, (blockSpec (blockBase p q) b).instOffsets)⟩

theorem instBase_succ {b : Block} {i : Nat} {ins : Inst} (hi : b.insts[i]? = some ins) :
    instBase b (i + 1) = instBase b i + Inst.width ins := by
  simp [instBase, List.take_add_one, hi, List.sum_append, Nat.add_assoc]

theorem instNumbering_from (b : Block) (base : Nat) :
    ∀ (i : Nat) (l : List Inst), b.insts.drop i = l →
      instNumbering (base + instBase b i) l = instSpecFrom b base i
  | i, [], hd => by simp [instNumbering, instSpecFrom, hd, Runtime.indexedFrom]
  | i, ins :: rest, hd => by
    have hi : b.insts[i]? = some ins := by
      have := congrArg (·[0]?) hd
      simpa [List.getElem?_drop] using this
    have hd' : b.insts.drop (i + 1) = rest := by
      rw [← List.tail_drop, hd]
      rfl
    have ih := instNumbering_from b base (i + 1) rest hd'
    rw [instBase_succ hi] at ih
    simp only [instNumbering, instSpecFrom, hd, Runtime.indexedFrom, List.flatMap_cons,
      List.map_cons]
    rw [Nat.add_assoc, ih]
    simp only [instSpecFrom, hd']

theorem blockNumbering_eq (base : Nat) (b : Block) : blockNumbering base b = blockSpec base b := by
  have h := instNumbering_from b base 0 b.insts (by simp)
  simp only [instBase, List.take_zero, List.map_nil, List.sum_nil, Nat.add_zero] at h
  simp only [blockNumbering, blockSpec, h]

theorem blockBase_succ {p : Program} {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) :
    blockBase p (j + 1) = blockBase p j + blockSize b := by
  simp [blockBase, List.take_add_one, hb, List.sum_append]

theorem numberingFrom_correct (p : Program) :
    ∀ (j : Nat) (l : List Block), p.blocks.drop j = l →
      numberingFrom (blockBase p j) l = numberingSpecFrom p j
  | j, [], hd => by simp [numberingFrom, numberingSpecFrom, hd, Runtime.indexedFrom]
  | j, b :: rest, hd => by
    have hb : p.blocks[j]? = some b := by
      have := congrArg (·[0]?) hd
      simpa [List.getElem?_drop] using this
    have hd' : p.blocks.drop (j + 1) = rest := by
      rw [← List.tail_drop, hd]
      rfl
    have ih := numberingFrom_correct p (j + 1) rest hd'
    rw [blockBase_succ hb] at ih
    simp only [numberingFrom, numberingSpecFrom, hd, Runtime.indexedFrom, blockNumbering_eq,
      List.flatMap_cons, List.map_cons]
    rw [ih]
    simp only [numberingSpecFrom, hd']

theorem numbering_eq (p : Program) : numbering p = numberingSpec p := by
  have h := numberingFrom_correct p 0 p.blocks (by simp)
  exact h

theorem regBound_lt {ids : List (Reg × Nat)} {r n : Nat} (h : (r, n) ∈ ids) : r < regBound ids := by
  induction ids with
  | nil => simp at h
  | cons a ids ih =>
    rcases a with ⟨q, m⟩
    simp only [List.mem_cons, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩ | h
    · simp only [regBound]; omega
    · simp only [regBound]; have := ih h; omega

theorem lookup_none_of_not_mem {ids : List (Reg × Nat)} {r : Reg}
    (h : r ∉ ids.map Prod.fst) : ids.lookup r = none := by
  induction ids with
  | nil => rfl
  | cons a ids ih =>
    rcases a with ⟨q, n⟩
    simp only [List.map_cons, List.mem_cons] at h
    have hq : r ≠ q := fun e => h (Or.inl e)
    have hb : (r == q) = false := by simpa using hq
    simp [List.lookup, hb, ih (fun hm => h (Or.inr hm))]

theorem size_addRegIds : ∀ (ids : List (Reg × Nat)) (a : Array (Option Nat)),
    (addRegIds ids a).size = a.size
  | [], _ => rfl
  | (r, n) :: ids, a => by
    simp only [addRegIds]
    rw [size_addRegIds]
    split <;> (try split) <;> first | rfl | simp

theorem addRegIds_lookup : ∀ (ids : List (Reg × Nat)) (a : Array (Option Nat)) (r : Reg),
    r < a.size → ((addRegIds ids a)[r]?).getD none = ((a[r]?).getD none).or (ids.lookup r)
  | [], a, r, _ => by simp [addRegIds]
  | (x, n) :: ids, a, r, hr => by
    simp only [addRegIds]
    by_cases hx : x < a.size
    · simp only [hx, ↓reduceDIte]
      cases ha : a[x] with
      | none =>
        simp only [Option.isSome_none, Bool.false_eq_true, ↓reduceIte]
        rw [addRegIds_lookup ids _ r (by simpa using hr)]
        by_cases h : r = x
        · subst h
          simp [List.lookup, Array.getElem?_setIfInBounds_self_of_lt hx,
            Array.getElem?_eq_getElem hx, ha]
        · have hne : r ≠ x := h
          have hb : (r == x) = false := by simpa using hne
          simp [List.lookup, hb, Array.getElem?_setIfInBounds_ne (Ne.symm hne)]
      | some v =>
        simp only [Option.isSome_some, ↓reduceIte]
        rw [addRegIds_lookup ids a r hr]
        by_cases h : r = x
        · subst h
          simp [List.lookup, Array.getElem?_eq_getElem hx, ha]
        · have hne : r ≠ x := h
          have hb : (r == x) = false := by simpa using hne
          simp [List.lookup, hb]
    · simp only [hx, ↓reduceDIte]
      rw [addRegIds_lookup ids a r hr]
      have hne : r ≠ x := fun h => hx (h ▸ hr)
      have hb : (r == x) = false := by simpa using hne
      simp [List.lookup, hb]

theorem regTable_lookup (data : NumberingData) (r : Reg) :
    ((regTable data)[r]?).getD none = data.ids.lookup r := by
  by_cases hr : r < regBound data.ids
  · rw [regTable, addRegIds_lookup _ _ r (by simpa using hr)]
    rw [Array.getElem?_eq_getElem (show r < (Array.replicate (regBound data.ids) none).size by simpa)]
    simp
  · have hnone : ((regTable data)[r]?) = none := by
      apply Array.getElem?_eq_none
      rw [regTable, size_addRegIds]
      simpa using hr
    have hnot : r ∉ data.ids.map Prod.fst := by
      intro hm
      obtain ⟨a, ha, hra⟩ := List.mem_map.mp hm
      change a.1 = r at hra
      have hb : (r, a.2) ∈ data.ids := by rwa [← hra]
      exact hr (regBound_lt hb)
    simp [hnone, lookup_none_of_not_mem hnot]

theorem prepare_regId (p : Program) (r : Reg) : regIdFast (prepare p) r = regId p r := by
  simp only [regIdFast, prepare, regTable_lookup, regId]
  rw [numbering_eq]
  rfl

theorem blockOffset_prepare {p : Program} {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) :
    blockOffset (prepare p) j = blockBase p j := by
  simp [blockOffset, prepare, numbering_eq, numberingSpec, List.getElem?_toArray,
    List.getElem?_map, Runtime.getElem?_indexed, hb]

theorem instOffset_prepare {p : Program} {j i : Nat} {b : Block} {ins : Inst}
    (hb : p.blocks[j]? = some b) (hi : b.insts[i]? = some ins) :
    instOffset (prepare p) j i = blockBase p j + instBase b i := by
  simp [instOffset, prepare, numbering_eq, numberingSpec, List.getElem?_toArray,
    List.getElem?_map, Runtime.getElem?_indexed, hb, hi]

theorem incomingBound_lt {e : Nat × (Nat × List Operand)} {es : List (Nat × (Nat × List Operand))}
    (h : e ∈ es) : e.1 < incomingBound es := by
  induction es with
  | nil => simp at h
  | cons a es ih =>
    simp only [List.mem_cons] at h
    rcases h with rfl | h
    · simp only [incomingBound]; omega
    · simp only [incomingBound]; have := ih h; omega

theorem size_addIncoming : ∀ (es : List (Nat × (Nat × List Operand)))
    (bs : Array (List (Nat × List Operand))), (addIncoming es bs).size = bs.size
  | [], _ => rfl
  | e :: es, bs => by
    simp only [addIncoming]
    rw [size_addIncoming]
    split <;> simp

theorem incomingBucket_add : ∀ (es : List (Nat × (Nat × List Operand)))
    (bs : Array (List (Nat × List Operand))) (j : Nat), j < bs.size →
    incomingBucket (addIncoming es bs) j =
      ((es.filter (·.1 == j)).map Prod.snd).reverse ++ incomingBucket bs j
  | [], bs, j, _ => by simp [addIncoming, incomingBucket]
  | e :: es, bs, j, hj => by
    simp only [addIncoming]
    by_cases hlt : e.1 < bs.size
    · simp only [hlt, ↓reduceDIte]
      rw [incomingBucket_add es _ j (by simpa using hj)]
      by_cases h : e.1 = j
      · subst h
        have hget : bs[e.1]?.getD [] = bs[e.1] := by
          rw [Array.getElem?_eq_getElem hlt]; rfl
        simp [incomingBucket, Array.getElem?_setIfInBounds_self_of_lt (by simpa using hlt), hget,
          List.reverse_cons, List.append_assoc]
      · have hne : j ≠ e.1 := Ne.symm h
        simp [incomingBucket, h]
    · simp only [hlt, ↓reduceDIte]
      rw [incomingBucket_add es bs j hj]
      have hne : e.1 ≠ j := by omega
      simp [hne]

theorem incomingBucket_buckets (p : Program) (j : Nat) :
    (incomingBucket (incomingBuckets p) j).reverse =
      ((targetedEdges p).filter (·.1 == j)).map Prod.snd := by
  by_cases hj : j < (Array.replicate (incomingBound (targetedEdges p))
    ([] : List (Nat × List Operand))).size
  · have h := incomingBucket_add (targetedEdges p)
      (Array.replicate (incomingBound (targetedEdges p)) ([] : List (Nat × List Operand))) j hj
    have hinit : incomingBucket (Array.replicate (incomingBound (targetedEdges p))
        ([] : List (Nat × List Operand))) j = [] := by
      simp only [incomingBucket]
      rw [Array.getElem?_eq_getElem hj]
      simp
    change (incomingBucket (addIncoming (targetedEdges p)
      (Array.replicate (incomingBound (targetedEdges p)) [])) j).reverse = _
    rw [h, hinit]
    simp
  · have hnone : (incomingBuckets p)[j]? = none := by
      apply Array.getElem?_eq_none
      rw [incomingBuckets, size_addIncoming]
      simpa using hj
    have hnoedge : (targetedEdges p).filter (·.1 == j) = [] := by
      apply List.filter_eq_nil_iff.mpr
      intro e he
      have hlt := incomingBound_lt he
      simp only [beq_iff_eq]
      intro h
      subst h
      exact hj (by simpa using hlt)
    simp [incomingBucket, hnone, hnoedge]

theorem targetedEdges_incoming (p : Program) (j : Nat) :
    ((targetedEdges p).filter (·.1 == j)).map Prod.snd = incoming p j := by
  unfold targetedEdges incoming
  generalize indexed p.blocks = bs
  induction bs with
  | nil => rfl
  | cons a bs ih =>
    rcases a with ⟨k, b⟩
    simp only [List.flatMap_cons, List.filter_append, List.map_append, ih]
    rcases b with ⟨params, insts, tm⟩
    cases tm <;> simp [edgesTo] <;> (try split) <;> (try split) <;> simp_all

theorem prepare_incoming (p : Program) (j : Nat) :
    (incomingBucket (prepare p).incoming j).reverse = incoming p j := by
  rw [prepare, incomingBucket_buckets, targetedEdges_incoming]

/-- The marked entries of an array are exactly the previously seen natural numbers. -/
def Marks (a : Array Bool) (seen : List Nat) : Prop :=
  ∀ m, m < a.size → (a[m]?).getD false = decide (m ∈ seen)

theorem marks_replicate (n : Nat) : Marks (Array.replicate n false) [] := by
  intro m hm
  rw [Array.getElem?_eq_getElem hm]
  simp

theorem markUnique_ok : ∀ (xs : List Nat) (a : Array Bool) (seen : List Nat), Marks a seen →
    (markUnique xs a).isSome =
      (decide xs.Nodup && xs.all (· < a.size) && decide (∀ x ∈ xs, x ∉ seen))
  | [], a, seen, h => by simp [markUnique]
  | x :: xs, a, seen, h => by
    by_cases hx : x < a.size
    · cases ha : a[x] with
      | false =>
        have hxnot : x ∉ seen := by
          have hm := h x hx
          rw [Array.getElem?_eq_getElem hx, ha] at hm
          simpa using hm
        have hmarks : Marks (a.setIfInBounds x true) (x :: seen) := by
          intro m hm
          simp only [Array.size_setIfInBounds] at hm
          by_cases hmx : m = x
          · subst m
            simp [Array.getElem?_setIfInBounds_self_of_lt hx]
          · rw [Array.getElem?_setIfInBounds_ne (Ne.symm hmx)]
            rw [h m hm]
            simp [hmx]
        simp only [markUnique, hx, ↓reduceDIte, ha, Bool.false_eq_true, ↓reduceIte]
        rw [markUnique_ok xs _ (x :: seen) hmarks]
        have hdisj : (∀ y ∈ xs, y ∉ x :: seen) ↔ x ∉ xs ∧ ∀ y ∈ xs, y ∉ seen := by
          constructor
          · intro hd
            exact ⟨fun hm => hd x hm (by simp), fun y hy hys => hd y hy (by simp [hys])⟩
          · rintro ⟨hxs, hd⟩ y hy
            simp only [List.mem_cons, not_or]
            exact ⟨fun e => hxs (e ▸ hy), hd y hy⟩
        simp only [Array.size_setIfInBounds, hdisj]
        simp [List.nodup_cons, List.all_cons, hx, hxnot, Bool.and_assoc, Bool.and_left_comm,
          Bool.and_comm]
      | true =>
        have hxmem : x ∈ seen := by
          have hm := h x hx
          rw [Array.getElem?_eq_getElem hx, ha] at hm
          simpa using hm
        simp [markUnique, hx, ha, hxmem]
    · simp [markUnique, hx]

theorem size_markUnique {xs : List Nat} {a a' : Array Bool}
    (h : markUnique xs a = some a') : a'.size = a.size := by
  induction xs generalizing a with
  | nil => cases h; rfl
  | cons x xs ih =>
    simp only [markUnique] at h
    split at h
    · next hx =>
      split at h
      · cases h
      · exact ih h |>.trans (Array.size_setIfInBounds)
    · cases h

theorem markUnique_marks : ∀ (xs : List Nat) (a a' : Array Bool) (seen : List Nat),
    Marks a seen → markUnique xs a = some a' →
      ∀ m, m < a'.size → (a'[m]?).getD false = decide (m ∈ xs ∨ m ∈ seen)
  | [], a, a', seen, h, hm => by
    cases hm
    intro m hlt
    rw [h m hlt]
    simp
  | x :: xs, a, a', seen, h, hm => by
    simp only [markUnique] at hm
    split at hm
    · next hx =>
      split at hm
      · cases hm
      · next ha =>
        have ha' : a[x] = false := by simpa using ha
        have hxnot : x ∉ seen := by
          have hxm := h x hx
          rw [Array.getElem?_eq_getElem hx, ha'] at hxm
          simpa using hxm
        have hmarks : Marks (a.setIfInBounds x true) (x :: seen) := by
          intro m hlt
          simp only [Array.size_setIfInBounds] at hlt
          by_cases hmx : m = x
          · subst m
            simp [Array.getElem?_setIfInBounds_self_of_lt hx]
          · rw [Array.getElem?_setIfInBounds_ne (Ne.symm hmx)]
            rw [h m hlt]
            simp [hmx]
        have ih := markUnique_marks xs _ a' (x :: seen) hmarks hm
        intro m hlt
        rw [ih m hlt]
        simp only [List.mem_cons]
        congr 1
        simp [or_left_comm, or_assoc]
    · simp at hm

theorem all_and {α : Type} (f g : α → Bool) (l : List α) :
    l.all (fun x => f x && g x) = (l.all f && l.all g) := by
  induction l with
  | nil => rfl
  | cons x l ih =>
    simp only [List.all_cons]
    rw [ih]
    cases f x <;> cases g x <;> cases l.all f <;> cases l.all g <;> rfl

theorem regCheck_eq (ids : List (Reg × Nat)) : regCheck ids = decide ((ids.map Prod.fst).Nodup) := by
  have h := markUnique_ok (ids.map Prod.fst) (Array.replicate (regBound ids) false) []
    (marks_replicate _)
  simp only [Array.size_replicate] at h
  have hall : (ids.map Prod.fst).all (· < regBound ids) = true := by
    apply List.all_eq_true.mpr
    intro r hr
    obtain ⟨a, ha, hra⟩ := List.mem_map.mp hr
    change a.1 = r at hra
    have hb : (r, a.2) ∈ ids := by rwa [← hra]
    simpa using regBound_lt hb
  simp only [regCheck, h, hall, List.not_mem_nil, not_false_eq_true, implies_true, decide_true,
    Bool.and_true]

theorem slotInterCheck_eq (slots inters : List Nat) (count : Nat) :
    slotInterCheck slots inters count =
      (decide slots.Nodup && slots.all (· < count) && inters.all (· < count) &&
        inters.all (fun m => !slots.contains m)) := by
  cases h : markUnique slots (Array.replicate count false) with
  | none =>
    have ho := markUnique_ok slots (Array.replicate count false) [] (marks_replicate _)
    rw [h] at ho
    have hb : (decide slots.Nodup && slots.all (· < count)) = false := by
      have := ho.symm
      simpa using this
    simp [slotInterCheck, h, hb]
  | some a =>
    have ho := markUnique_ok slots (Array.replicate count false) [] (marks_replicate _)
    rw [h] at ho
    have hs : (decide slots.Nodup && slots.all (· < count)) = true := by
      have := ho.symm
      simpa using this
    have hsize : a.size = count := by
      have := size_markUnique h
      simpa using this
    have hmarks := markUnique_marks slots _ a [] (marks_replicate _) h
    have hfun : (fun m => if hm : m < a.size then !a[m] else false) =
        (fun m => decide (m < count) && !slots.contains m) := by
      funext m
      by_cases hm : m < a.size
      · have hm' := hmarks m hm
        rw [Array.getElem?_eq_getElem hm] at hm'
        have hma : a[m] = decide (m ∈ slots) := by simpa using hm'
        have hmc : m < count := by simpa [hsize] using hm
        simp [hm, hmc, hma, List.contains_eq_mem]
      · have hmc : ¬ m < count := by simpa [hsize] using hm
        simp [hm, hmc]
    simp only [slotInterCheck, h]
    rw [hfun, all_and (fun m => decide (m < count)) (fun m => !slots.contains m) inters, hs]
    simp

theorem numberingOkFastData_eq (data : NumberingData) (count : Nat) :
    numberingOkFastData data count =
      (decide ((data.ids.map Prod.fst).Nodup) && decide ((data.ids.map Prod.snd).Nodup) &&
        (data.ids.map Prod.snd).all (· < count) && data.intermediates.all (· < count) &&
        data.intermediates.all (fun m => !(data.ids.map Prod.snd).contains m)) := by
  rw [numberingOkFastData, regCheck_eq, slotInterCheck_eq]
  cases decide ((data.ids.map Prod.fst).Nodup) <;> simp

theorem prepare_ok (p : Program) : (prepare p).ok = numberingOk p := by
  rw [prepare, numberingOkFastData_eq, numbering_eq]
  rfl

theorem mapM_ext {α β ε : Type} {f g : α → Except ε β} : ∀ (l : List α),
    (∀ x ∈ l, f x = g x) → l.mapM f = l.mapM g
  | [], _ => rfl
  | x :: l, h => by
    simp only [List.mapM_cons, h x (by simp)]
    rw [mapM_ext l (fun y hy => h y (by simp [hy]))]

theorem getElem_of_mem_indexed {α : Type} {l : List α} {i : Nat} {x : α}
    (hm : (i, x) ∈ indexed l) : l[i]? = some x := by
  obtain ⟨q, hq, he⟩ := List.mem_iff_getElem.mp hm
  have hs : (indexed l)[q]? = some (i, x) := by
    rw [List.getElem?_eq_getElem hq, he]
  rw [Runtime.getElem?_indexed] at hs
  cases hi : l[q]? with
  | none => simp [hi] at hs
  | some v =>
    simp only [hi, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hs
    obtain ⟨rfl, rfl⟩ := hs
    exact hi

theorem idOfFast_eq (p : Program) (r : Reg) : idOfFast (prepare p) r = idOf p r := by
  simp only [idOfFast, idOf, idOfWith, funext (prepare_regId p)]

theorem operandFast_eq (p : Program) (a : Operand) : operandFast (prepare p) a = operand p a := by
  simp only [operandFast, operand, operandWith, funext (prepare_regId p)]

theorem instFast_eq (p : Program) (n : Nat) (ins : Inst) :
    instFast (prepare p) n ins = inst p n ins := by
  simp only [instFast, inst, instWith, funext (prepare_regId p)]

theorem termFast_eq (p : Program) (tm : Term) : termFast (prepare p) tm = term p tm := by
  simp only [termFast, term, termWith, funext (prepare_regId p)]

theorem adaptPhiFast_eq {p : Program} {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) (k : Nat) :
    adaptPhiFast (prepare p) j k = adaptPhi p j k := by
  simp [adaptPhiFast, adaptPhi, prepare_incoming, operandFast_eq, blockOffset_prepare hb]

theorem adaptInstFast_eq {p : Program} {j i : Nat} {b : Block} {ins : Inst}
    (hb : p.blocks[j]? = some b) (hi : b.insts[i]? = some ins) :
    adaptInstFast (prepare p) j (i, ins) = adaptInst p j b (i, ins) := by
  simp [adaptInstFast, adaptInst, instOffset_prepare hb hi, instFast_eq]

theorem adaptBlockFast_eq {p : Program} {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) :
    adaptBlockFast (prepare p) (j, b) = adaptBlock p (j, b) := by
  have hp : (indexed b.params).mapM (fun (k, _) => adaptPhiFast (prepare p) j k) =
      (indexed b.params).mapM (fun (k, _) => adaptPhi p j k) := by
    apply mapM_ext
    rintro ⟨k, ty⟩ hm
    exact adaptPhiFast_eq hb k
  have hi : (indexed b.insts).mapM (adaptInstFast (prepare p) j) =
      (indexed b.insts).mapM (adaptInst p j b) := by
    apply mapM_ext
    rintro ⟨i, ins⟩ hm
    exact adaptInstFast_eq hb (getElem_of_mem_indexed hm)
  simp only [adaptBlockFast, adaptBlock, hp, hi, termFast_eq]

theorem adaptFast_eq (p : Program) : adaptFast p = adaptSpec p := by
  have hblocks : (indexed p.blocks).mapM (adaptBlockFast (prepare p)) =
      (indexed p.blocks).mapM (adaptBlock p) := by
    apply mapM_ext
    rintro ⟨j, b⟩ hm
    exact adaptBlockFast_eq (getElem_of_mem_indexed hm)
  unfold adaptFast adaptSpec
  simp only [prepare_ok]
  cases hn : numberingOk p <;> simp [hblocks, bind, Except.bind, pure, Except.pure]

theorem emit_eq_spec (p : Program) : emit p = emitSpec p := by
  simp [emit, emitSpec, adapt, adaptFast_eq]

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

/-! ## `adapter_correct`, the adapter's output shape -/

theorem mapM_ok {α β ε : Type} {g : α → Except ε β} :
    ∀ {l : List α} {l' : List β}, l.mapM g = .ok l' →
      l'.length = l.length ∧ ∀ (i : Nat) (x : α), l[i]? = some x → ∃ y, g x = .ok y ∧ l'[i]? = some y
  | [], l', h => by
    simp only [List.mapM_nil] at h
    cases h; exact ⟨rfl, fun i x hx => by simp at hx⟩
  | a :: l, l', h => by
    simp only [List.mapM_cons] at h
    cases ha : g a with
    | error e => simp [ha] at h; cases h
    | ok b =>
      cases hl : l.mapM g with
      | error e => simp [ha, hl] at h; cases h
      | ok bs =>
        simp [ha, hl] at h
        cases h
        obtain ⟨hlen, hel⟩ := mapM_ok hl
        refine ⟨by simp [hlen], fun i x hx => ?_⟩
        cases i with
        | zero => simp at hx; subst hx; exact ⟨b, ha, rfl⟩
        | succ i => simpa using hel i x (by simpa using hx)

/-- An instruction's expansion has `Inst.width` instructions. -/
theorem inst_length {p : Program} {n : Nat} {ins : Inst} {xs : List Source.Instruction}
    (h : inst p n ins = .ok xs) : xs.length = Inst.width ins := by
  cases ins with
  | bin op t d a b =>
    unfold inst instWith at h
    simp only [← operand_eq_with] at h
    cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
      simp_all [Inst.width, bind, Except.bind, pure, Except.pure] <;> (subst h; rfl)
  | binOvf op t d f a b =>
    unfold inst instWith at h
    simp only [← operand_eq_with] at h
    cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
      simp_all [Inst.width, bind, Except.bind, pure, Except.pure] <;> (subst h; rfl)
  | trunc fr to d a =>
    unfold inst instWith at h
    simp only [← operand_eq_with] at h
    cases hp : inProfile fr <;> cases hq : inProfile to <;> cases ha : operand p a <;>
      simp_all [Inst.width, bind, Except.bind, pure, Except.pure] <;> (subst h; rfl)

/-! ## `adapter_correct`, numbering -/

theorem lookup_of_mem {l : List (Reg × Nat)} (hn : (l.map Prod.fst).Nodup) {r : Reg} {n : Nat}
    (hm : (r, n) ∈ l) : l.lookup r = some n := by
  induction l with
  | nil => simp at hm
  | cons a l ih =>
    obtain ⟨r', n'⟩ := a
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hn
    simp only [List.mem_cons, Prod.mk.injEq] at hm
    rcases hm with ⟨rfl, rfl⟩ | hm
    · simp [List.lookup]
    · have hne : r ≠ r' := by
        rintro rfl; exact hn.1 ⟨(r, n), hm, rfl⟩
      have hb : (r == r') = false := by simpa using hne
      simp only [List.lookup, hb]
      exact ih hn.2 hm

/-- A block parameter's id is its position. -/
theorem regId_param {p : Program} (hn : ((ids p).map Prod.fst).Nodup) {j k : Nat} {b : Block}
    {r : Reg} {ty : Ty} (hb : p.blocks[j]? = some b) (hk : b.params[k]? = some (r, ty)) :
    regId p r = some (blockBase p j + k) := by
  apply lookup_of_mem hn
  simp only [ids, List.mem_flatMap, List.mem_append, List.mem_map]
  exact ⟨(j, b), Runtime.mem_indexed hb, Or.inl ⟨(k, (r, ty)), Runtime.mem_indexed hk, rfl⟩⟩

/-- A register an instruction defines has the id of its slot among the instruction's values. -/
theorem regId_inst {p : Program} (hn : ((ids p).map Prod.fst).Nodup) {j i : Nat} {b : Block}
    {ins : Inst} {r : Reg} {off : Nat} (hb : p.blocks[j]? = some b) (hi : b.insts[i]? = some ins)
    (hd : (r, off) ∈ instDefs ins) :
    regId p r = some (blockBase p j + instBase b i + off) := by
  apply lookup_of_mem hn
  simp only [ids, List.mem_flatMap, List.mem_append, List.mem_map]
  exact ⟨(j, b), Runtime.mem_indexed hb,
    Or.inr ⟨(i, ins), Runtime.mem_indexed hi, ⟨(r, off), hd, rfl⟩⟩⟩

/-! ## `adapter_correct`, environments -/

theorem enc_width (v : Val) : (enc v).width = .word := by cases v <;> rfl

theorem word_bne : (Width.word != Width.word) = false := by decide

theorem mem_of_lookup {l : List (Reg × Nat)} {r : Reg} {n : Nat} (h : l.lookup r = some n) :
    (r, n) ∈ l := by
  obtain ⟨l₁, l₂, rfl, _⟩ := List.lookup_eq_some_iff.mp h
  simp

theorem eq_of_snd {l : List (Reg × Nat)} (hn : (l.map Prod.snd).Nodup) {a b : Reg} {n : Nat}
    (ha : (a, n) ∈ l) (hb : (b, n) ∈ l) : a = b := by
  induction l with
  | nil => simp at ha
  | cons x l ih =>
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hn
    simp only [List.mem_cons] at ha hb
    rcases ha with rfl | ha <;> rcases hb with hb | hb
    · cases hb; rfl
    · exact absurd ⟨(b, n), hb, rfl⟩ hn.1
    · subst hb; exact absurd ⟨(a, n), ha, rfl⟩ hn.1
    · exact ih hn.2 ha hb

/-- Agreement: every BIR register's newest value sits, encoded, at its LCB id. -/
def Agrees (p : Program) (benv : Env) (lenv : Source.Environment) : Prop :=
  ∀ r v, lookup benv r = some v → ∃ n, regId p r = some n ∧ lenv[n]? = some (some (enc v))

theorem regId_injective {p : Program} (hok : numberingOk p = true) {a b : Reg} {n : Nat}
    (ha : regId p a = some n) (hb : regId p b = some n) : a = b := by
  simp only [numberingOk, Bool.and_eq_true, decide_eq_true_eq] at hok
  exact eq_of_snd hok.1.1.1.2 (mem_of_lookup ha) (mem_of_lookup hb)

/-- Reading a translated operand gives the encoded BIR value. -/
theorem read_operand {p : Program} {benv : Env} {lenv : Source.Environment}
    (hag : Agrees p benv lenv) {a : Operand} {a' : Source.Operand} {v : Val}
    (ha : operand p a = .ok a') (hv : evalOp benv a = some v) :
    Source.read lenv .word a' = .ok (enc v) := by
  cases a with
  | reg r =>
    obtain ⟨n, hn, hl⟩ := hag r v hv
    simp [operand, operandWith, idOfWith, hn, bind, Except.bind, pure, Except.pure] at ha
    subst ha
    simp [Source.read, hl, enc_width, word_bne, bind, Except.bind, pure, Except.pure]
  | imm t x =>
    simp only [operand, operandWith, pure, Except.pure] at ha
    cases ha
    simp only [evalOp] at hv
    split at hv
    · cases hv; simp [Source.read, enc, word, Source.Value.width, word_bne, bind, Except.bind, pure, Except.pure]
    · cases hv

/-- A write at a word slot inside the environment succeeds. -/
theorem write_ok {f : Source.Function} {env : Source.Environment} {n : Nat} {val : Source.Value}
    (hw : f.valueWidths[n]? = some .word) (hs : n < env.size) (hv : val.width = .word) :
    Source.write f env n val = .ok (env.set! n (some val)) := by
  have : ¬ n ≥ env.size := by omega
  simp [Source.write, hw, this, hv, word_bne, pure, Except.pure, Array.set!_eq_setIfInBounds]

/-- Writing a slot no register owns keeps agreement. -/
theorem agrees_set_other {p : Program} {benv : Env} {lenv : Source.Environment} {m : Nat}
    {x : Option Source.Value} (hag : Agrees p benv lenv) (hm : ∀ r n, regId p r = some n → n ≠ m) :
    Agrees p benv (lenv.set! m x) := by
  intro r v hv
  obtain ⟨n, hn, hl⟩ := hag r v hv
  refine ⟨n, hn, ?_⟩
  rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (hm r n hn).symm]
  exact hl

/-- Defining register `d` with `v` at its own slot extends agreement. -/
theorem agrees_define {p : Program} (hok : numberingOk p = true) {benv : Env}
    {lenv : Source.Environment} {d : Reg} {nd : Nat} {v : Val} (hag : Agrees p benv lenv)
    (hd : regId p d = some nd) (hs : nd < lenv.size) :
    Agrees p ((d, v) :: benv) (lenv.set! nd (some (enc v))) := by
  intro r w hw
  simp only [lookup] at hw
  by_cases hdr : d = r
  · subst hdr
    simp at hw
    subst hw
    refine ⟨nd, hd, ?_⟩
    simp [Array.set!_eq_setIfInBounds, hs]
  · simp [hdr] at hw
    obtain ⟨n, hn, hl⟩ := hag r w hw
    refine ⟨n, hn, ?_⟩
    have hne : nd ≠ n := by
      rintro rfl; exact hdr (regId_injective hok hd hn)
    rw [Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne hne]
    exact hl

/-! ## `adapter_correct`, instructions -/

/-- One body step: the instruction at the current position evaluates and writes. -/
theorem step_body {f : Source.Function} {s : Source.State} {lb : Source.Block}
    {i : Source.Instruction} {v : Source.Value} {env' : Source.Environment}
    (hb : f.blocks[s.block]? = some lb) (hi : lb.instructions[s.position]? = some i)
    (he : Source.evaluate s.environment i = .ok v)
    (hw : Source.write f s.environment i.destination v = .ok env') :
    Source.step f s = .ok (.next { s with position := s.position + 1, environment := env' }) := by
  have hp : s.position < lb.instructions.size := by
    rcases Nat.lt_or_ge s.position lb.instructions.size with h | h
    · exact h
    · simp [Array.getElem?_eq_none h] at hi
  have hi' : lb.instructions[s.position] = i := by
    rw [Array.getElem?_eq_getElem hp] at hi; exact Option.some.inj hi
  simp [Source.step, hb, hp, hi', he, hw, bind, Except.bind, pure, Except.pure]

/-- What a successful `bin` expansion is. -/
theorem inst_bin {p : Program} {n : Nat} {op : BinOp} {t : IntTy} {d : Reg} {a b : Operand}
    {xs : List Source.Instruction} (h : inst p n (.bin op t d a b) = .ok xs) :
    inProfile t = true ∧ ∃ a' b', operand p a = .ok a' ∧ operand p b = .ok b' ∧
      xs = [ins n (.binary (opBinary op) a' b'),
        ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
        ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
        ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))] := by
  unfold inst instWith at h
  simp only [← operand_eq_with] at h
  cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- What a successful `binOvf` expansion is. -/
theorem inst_binOvf {p : Program} {n : Nat} {op : BinOp} {t : IntTy} {d fl : Reg} {a b : Operand}
    {xs : List Source.Instruction} (h : inst p n (.binOvf op t d fl a b) = .ok xs) :
    inProfile t = true ∧ ∃ a' b', operand p a = .ok a' ∧ operand p b = .ok b' ∧
      xs = [ins n (.binary (opBinary op) a' b'),
        ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
        ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
        ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min))),
        ins (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
        ins (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))] := by
  unfold inst instWith at h
  simp only [← operand_eq_with] at h
  cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- What a successful `trunc` expansion is. -/
theorem inst_trunc {p : Program} {n : Nat} {fr to : IntTy} {d : Reg} {a : Operand}
    {xs : List Source.Instruction} (h : inst p n (.trunc fr to d a) = .ok xs) :
    inProfile to = true ∧ ∃ a', operand p a = .ok a' ∧
      xs = [ins n (.binary .sub a' (.literal (word to.min))),
        ins (n + 1) (.binary .andBits (.local n) (.literal (word (2 ^ to.bits - 1)))),
        ins (n + 2) (.binary .add (.local (n + 1)) (.literal (word to.min)))] := by
  unfold inst instWith at h
  simp only [← operand_eq_with] at h
  cases hp : inProfile fr <;> cases hq : inProfile to <;> cases ha : operand p a <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- Reading a slot just written. -/
theorem read_set_self {env : Source.Environment} {n : Nat} {v : Source.Value} (hs : n < env.size)
    (hv : v.width = .word) : Source.read (env.set! n (some v)) .word (.local n) = .ok v := by
  simp [Source.read, Array.set!_eq_setIfInBounds, hs, hv, word_bne, bind, Except.bind, pure,
    Except.pure]

theorem read_literal (env : Source.Environment) (z : Int) : Source.read env .word (.literal (word z)) = .ok (word z) := by
  simp [Source.read, word, Source.Value.width, word_bne, bind, Except.bind, pure, Except.pure]

/-- Evaluating an expansion instruction on two word reads. -/
theorem eval_bin {env : Source.Environment} {d : Nat} {op : Source.Binary} {a b : Source.Operand}
    {p q : BitVec 32} (ha : Source.read env .word a = .ok (.word p))
    (hb : Source.read env .word b = .ok (.word q)) :
    Source.evaluate env (ins d (.binary op a b)) = .ok (.word (Source.binaryBits op p q)) := by
  simp [Source.evaluate, ins, ha, hb, bind, Except.bind, Source.evaluateBinary]

/-- Four consecutive slot writes, as a `bin` expansion makes them. -/
def set4 (e : Source.Environment) (n : Nat) (u₀ u₁ u₂ v : Option Source.Value) : Source.Environment :=
  (((e.set! n u₀).set! (n + 1) u₁).set! (n + 2) u₂).set! (n + 3) v

theorem size_set! (e : Source.Environment) (m : Nat) (u : Option Source.Value) :
    (e.set! m u).size = e.size := by
  simp [Array.set!_eq_setIfInBounds]

/-- The intermediate words of a `bin`/`binOvf` expansion: the raw result, minus `t.min`, masked. -/
def raw (op : BinOp) (x y : Int) : BitVec 32 :=
  Source.binaryBits (opBinary op) (BitVec.ofInt 32 x) (BitVec.ofInt 32 y)
def biased (t : IntTy) (z : BitVec 32) : BitVec 32 := Source.binaryBits .sub z (BitVec.ofInt 32 t.min)
def masked (t : IntTy) (z : BitVec 32) : BitVec 32 :=
  Source.binaryBits .andBits z (BitVec.ofInt 32 (2 ^ t.bits - 1))

/-- `Steps` compose. -/
theorem steps_append {f : Source.Function} {m k : Nat} {s t u : Source.State} :
    Source.Steps f m s t → Source.Steps f k t u → Source.Steps f (k + m) s u
  | .zero _, h => h
  | .succ h₁ h₂, h => .succ h₁ (steps_append h₂ h)

/-- The four steps of a `bin` expansion compute the word of `wrap t (a op b)` at slot `n + 3`. -/
theorem bin_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {op : BinOp} {t : IntTy} {a' b' : Source.Operand} {x y : Int} (ht : inProfile t = true)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 4 → lb.instructions[s.position + k]? =
      [ins n (.binary (opBinary op) a' b'),
        ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
        ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
        ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))][k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 3 < s.environment.size) :
    Source.Steps f 4 s ⟨s.block, s.position + 4,
      set4 s.environment n (some (.word (raw op x y))) (some (.word (biased t (raw op x y))))
        (some (.word (masked t (biased t (raw op x y))))) (some (word (wrap t (op.exact x y))))⟩ := by
  obtain ⟨blk, pos, env⟩ := s
  simp only at hb hq hA hB hW hs ⊢
  let p₀ := raw op x y
  let p₁ := biased t p₀
  let p₂ := masked t p₁
  let p₃ := Source.binaryBits .add p₂ (BitVec.ofInt 32 t.min)
  let e₁ := env.set! n (some (.word p₀))
  let e₂ := e₁.set! (n + 1) (some (.word p₁))
  let e₃ := e₂.set! (n + 2) (some (.word p₂))
  have z₁ : e₁.size = env.size := size_set! ..
  have z₂ : e₂.size = env.size := (size_set! ..).trans z₁
  have z₃ : e₃.size = env.size := (size_set! ..).trans z₂
  have hv₃ : word (wrap t (op.exact x y)) = .word p₃ := by
    show word _ = .word (Source.binaryBits .add (Source.binaryBits .andBits (Source.binaryBits .sub
      (Source.binaryBits (opBinary op) (BitVec.ofInt 32 x) (BitVec.ofInt 32 y)) _) _) _)
    rw [binaryBits_exact]
    show word _ = Source.Value.word ((BitVec.ofInt 32 (op.exact x y) - BitVec.ofInt 32 t.min &&&
      BitVec.ofInt 32 (2 ^ t.bits - 1)) + BitVec.ofInt 32 t.min)
    rw [wrap_word ht]; rfl
  show Source.Steps f 4 ⟨blk, pos, env⟩ ⟨blk, pos + 4, e₃.set! (n + 3) (some (word _))⟩
  rw [hv₃]
  have s₀ := step_body (s := ⟨blk, pos, env⟩) hb (by simpa using hq 0 (by omega))
    (eval_bin (d := n) (op := opBinary op) hA hB)
    (write_ok (n := n) (hW n (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  have s₁ := step_body (s := ⟨blk, pos + 1, e₁⟩) hb (by simpa [Nat.add_assoc] using hq 1 (by omega))
    (eval_bin (d := n + 1) (op := .sub) (read_set_self (by omega) rfl) (read_literal _ t.min))
    (write_ok (n := n + 1) (hW (n + 1) (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  have s₂ := step_body (s := ⟨blk, pos + 1 + 1, e₂⟩) hb
    (by simpa [Nat.add_assoc] using hq 2 (by omega))
    (eval_bin (d := n + 2) (op := .andBits) (read_set_self (by omega) rfl)
      (read_literal _ (2 ^ t.bits - 1)))
    (write_ok (n := n + 2) (hW (n + 2) (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  have s₃ := step_body (s := ⟨blk, pos + 1 + 1 + 1, e₃⟩) hb
    (by simpa [Nat.add_assoc] using hq 3 (by omega))
    (eval_bin (d := n + 3) (op := .add) (read_set_self (by omega) rfl) (read_literal _ t.min))
    (write_ok (n := n + 3) (hW (n + 3) (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  exact .succ s₀ (.succ s₁ (.succ s₂ (.succ s₃ (.zero _))))

/-- Reading a slot past a write to another slot. -/
theorem read_set_other {env : Source.Environment} {m k : Nat} {u : Option Source.Value} {w : Width}
    (h : m ≠ k) : Source.read (env.set! m u) w (.local k) = Source.read env w (.local k) := by
  simp [Source.read, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne h]

/-- Evaluating an expansion comparison on two word reads. -/
theorem eval_cmp {env : Source.Environment} {d : Nat} {op : Source.Comparison}
    {a b : Source.Operand} {p q : BitVec 32} (ha : Source.read env .word a = .ok (.word p))
    (hb : Source.read env .word b = .ok (.word q)) :
    Source.evaluate env (ins d (.compare op .word a b)) =
      Source.evaluateComparison op (.word p) (.word q) := by
  simp [Source.evaluate, ins, ha, hb, word_bne, bind, Except.bind]

/-- The words LCB's comparisons give. Named, so that unification never evaluates them. -/
def ultBit (p q : BitVec 32) : BitVec 32 := if p.toNat < q.toNat then 1 else 0
def eqBit (p q : BitVec 32) : BitVec 32 := if p.toNat = q.toNat then 1 else 0

theorem cmp_ult (p q : BitVec 32) : Source.evaluateComparison .unsignedLT (.word p) (.word q) =
    .ok (.word (ultBit p q)) := by
  by_cases h : p.toNat < q.toNat <;>
    simp [Source.evaluateComparison, Source.Value.width, Source.Value.toNat, word_bne, pure,
      Except.pure, ultBit, h]

theorem cmp_eq (p q : BitVec 32) : Source.evaluateComparison .eq (.word p) (.word q) =
    .ok (.word (eqBit p q)) := by
  by_cases h : p.toNat = q.toNat <;>
    simp [Source.evaluateComparison, Source.Value.width, Source.Value.toNat, word_bne, pure,
      Except.pure, eqBit, h]

theorem pow_bits_toNat {t : IntTy} (ht : inProfile t = true) :
    (BitVec.ofInt 32 (2 ^ t.bits)).toNat = 2 ^ t.bits := by
  rcases inProfile_cases ht with rfl | rfl | rfl | rfl <;> decide

/-- The overflow flag word: `1` exactly when the result is outside `t`. -/
theorem flag_value {t : IntTy} (ht : inProfile t = true) (op : BinOp) {x y : Int}
    (hx : t.inRange x = true) (hy : t.inRange y = true) :
    eqBit (ultBit (biased t (raw op x y)) (BitVec.ofInt 32 (2 ^ t.bits))) (BitVec.ofInt 32 0) =
      BitVec.ofInt 32 (if !t.inRange (op.exact x y) then 1 else 0) := by
  unfold eqBit ultBit
  have hf := flag_word ht op hx hy
  have hr : biased t (raw op x y) = BitVec.ofInt 32 (op.exact x y) - BitVec.ofInt 32 t.min := by
    simp only [biased, raw, binaryBits_exact]; rfl
  rw [hr, pow_bits_toNat ht]
  revert hf
  generalize (BitVec.ofInt 32 (op.exact x y) - BitVec.ofInt 32 t.min).toNat = v
  intro hf
  cases h : t.inRange (op.exact x y)
  · have hv : ¬ v < 2 ^ t.bits := by simpa [h] using hf
    simp [hv]
  · have hv : v < 2 ^ t.bits := by simpa [h] using hf
    simp [hv]

/-- The two flag steps of a `binOvf` expansion, from any slot `n + 1` holding the word `p`. -/
theorem flag_steps {f : Source.Function} {lb : Source.Block} {blk : Source.BlockId} {pos n : Nat}
    {E : Source.Environment} {p Q : BitVec 32} (hb : f.blocks[blk]? = some lb)
    (hi₄ : lb.instructions[pos]? = some (ins (n + 4) (.compare .unsignedLT .word (.local (n + 1))
      (.literal (.word Q)))))
    (hi₅ : lb.instructions[pos + 1]? = some (ins (n + 5) (.compare .eq .word (.local (n + 4))
      (.literal (word 0)))))
    (r₁ : Source.read E .word (.local (n + 1)) = .ok (.word p))
    (hW₄ : f.valueWidths[n + 4]? = some .word) (hW₅ : f.valueWidths[n + 5]? = some .word)
    (hs : n + 5 < E.size) :
    Source.Steps f 2 ⟨blk, pos, E⟩ ⟨blk, pos + 1 + 1,
      (E.set! (n + 4) (some (.word (ultBit p Q)))).set! (n + 5)
        (some (.word (eqBit (ultBit p Q) (BitVec.ofInt 32 0))))⟩ := by
  have rQ : Source.read E .word (.literal (.word Q)) = .ok (.word Q) := by
    simp [Source.read, Source.Value.width, word_bne, bind, Except.bind, pure, Except.pure]
  have s₄ := step_body (s := ⟨blk, pos, E⟩) hb hi₄
    ((eval_cmp (d := n + 4) r₁ rQ).trans (cmp_ult _ _))
    (write_ok (n := n + 4) hW₄ (by show n + 4 < E.size; omega) rfl)
  have r₄ := read_set_self (env := E) (n := n + 4) (v := .word (ultBit p Q)) (by omega) rfl
  have s₅ := step_body (s := ⟨blk, pos + 1, E.set! (n + 4) (some (.word (ultBit p Q)))⟩) hb hi₅
    ((eval_cmp (d := n + 5) r₄ (read_literal _ 0)).trans (cmp_eq _ _))
    (write_ok (n := n + 5) hW₅ (by show n + 5 < (E.set! _ _).size; rw [size_set!]; omega) rfl)
  exact .succ s₄ (.succ s₅ (.zero _))

/-- The six steps of a `binOvf` expansion: the wrapped result at slot `n + 3` and the overflow flag
at slot `n + 5`, both as `enc` gives them. -/
theorem binOvf_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {op : BinOp} {t : IntTy} {a' b' : Source.Operand} {x y : Int} (ht : inProfile t = true)
    (hx : t.inRange x = true) (hy : t.inRange y = true)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 6 → lb.instructions[s.position + k]? =
      [ins n (.binary (opBinary op) a' b'),
        ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
        ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
        ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min))),
        ins (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
        ins (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))][k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hB : Source.read s.environment .word b' = .ok (word y))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 5 < s.environment.size) :
    ∃ u₄, Source.Steps f 6 s ⟨s.block, s.position + 6,
      ((set4 s.environment n (some (.word (raw op x y))) (some (.word (biased t (raw op x y))))
        (some (.word (masked t (biased t (raw op x y))))) (some (word (wrap t (op.exact x y))))).set!
        (n + 4) u₄).set! (n + 5) (some (enc (.bool (!t.inRange (op.exact x y)))))⟩ := by
  obtain ⟨blk, pos, env⟩ := s
  simp only at hb hq hA hB hW hs ⊢
  let E := set4 env n (some (.word (raw op x y))) (some (.word (biased t (raw op x y))))
    (some (.word (masked t (biased t (raw op x y))))) (some (word (wrap t (op.exact x y))))
  have h4 : Source.Steps f 4 ⟨blk, pos, env⟩ ⟨blk, pos + 4, E⟩ :=
    bin_steps (s := ⟨blk, pos, env⟩) ht hb (fun k hk => by
      rw [hq k (by omega)]; rcases k with _ | _ | _ | _ | k <;> first | rfl | omega)
      hA hB hW (by first | omega | (dsimp only; omega))
  have zE : E.size = env.size := by simp only [E, set4, size_set!]
  have r₁ : Source.read E .word (.local (n + 1)) = .ok (.word (biased t (raw op x y))) := by
    simp only [E, set4]
    rw [read_set_other (by omega), read_set_other (by omega)]
    exact read_set_self (by simp only [size_set!]; omega) rfl
  have h2 := flag_steps (E := E) (pos := pos + 4) (Q := BitVec.ofInt 32 (2 ^ t.bits)) hb
    (hq 4 (by omega)) (by simpa [Nat.add_assoc] using hq 5 (by omega)) r₁
    (hW (n + 4) (by omega)) (hW (n + 5) (by omega)) (by omega)
  refine ⟨some (.word (ultBit (biased t (raw op x y)) (BitVec.ofInt 32 (2 ^ t.bits)))), ?_⟩
  have hflag : enc (.bool (!t.inRange (op.exact x y))) = .word (eqBit (ultBit (biased t
      (raw op x y)) (BitVec.ofInt 32 (2 ^ t.bits))) (BitVec.ofInt 32 0)) := by
    simp only [enc, word]; rw [flag_value ht op hx hy]
  rw [hflag, show pos + 6 = pos + 4 + 1 + 1 from rfl]
  exact steps_append h4 h2

/-- The three steps of a `trunc` expansion compute the word of `wrap to x` at slot `n + 2`. -/
theorem trunc_steps {f : Source.Function} {s : Source.State} {lb : Source.Block} {n : Nat}
    {to : IntTy} {a' : Source.Operand} {x : Int} (ht : inProfile to = true)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 3 → lb.instructions[s.position + k]? =
      [ins n (.binary .sub a' (.literal (word to.min))),
        ins (n + 1) (.binary .andBits (.local n) (.literal (word (2 ^ to.bits - 1)))),
        ins (n + 2) (.binary .add (.local (n + 1)) (.literal (word to.min)))][k]?)
    (hA : Source.read s.environment .word a' = .ok (word x))
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hs : n + 2 < s.environment.size) :
    Source.Steps f 3 s ⟨s.block, s.position + 3,
      ((s.environment.set! n (some (.word (biased to (BitVec.ofInt 32 x))))).set! (n + 1)
        (some (.word (masked to (biased to (BitVec.ofInt 32 x)))))).set! (n + 2)
        (some (word (wrap to x)))⟩ := by
  obtain ⟨blk, pos, env⟩ := s
  simp only at hb hq hA hW hs ⊢
  let p₀ := biased to (BitVec.ofInt 32 x)
  let p₁ := masked to p₀
  let e₁ := env.set! n (some (.word p₀))
  let e₂ := e₁.set! (n + 1) (some (.word p₁))
  have z₁ : e₁.size = env.size := size_set! ..
  have z₂ : e₂.size = env.size := (size_set! ..).trans z₁
  have hv : word (wrap to x) = .word (Source.binaryBits .add p₁ (BitVec.ofInt 32 to.min)) := by
    show word _ = Source.Value.word ((BitVec.ofInt 32 x - BitVec.ofInt 32 to.min &&&
      BitVec.ofInt 32 (2 ^ to.bits - 1)) + BitVec.ofInt 32 to.min)
    rw [wrap_word ht]; rfl
  show Source.Steps f 3 ⟨blk, pos, env⟩ ⟨blk, pos + 3, e₂.set! (n + 2) (some (word _))⟩
  rw [hv]
  have s₀ := step_body (s := ⟨blk, pos, env⟩) hb (by simpa using hq 0 (by omega))
    (eval_bin (d := n) (op := .sub) hA (read_literal _ to.min))
    (write_ok (n := n) (hW n (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  have s₁ := step_body (s := ⟨blk, pos + 1, e₁⟩) hb (by simpa [Nat.add_assoc] using hq 1 (by omega))
    (eval_bin (d := n + 1) (op := .andBits) (read_set_self (by omega) rfl)
      (read_literal _ (2 ^ to.bits - 1)))
    (write_ok (n := n + 1) (hW (n + 1) (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  have s₂ := step_body (s := ⟨blk, pos + 1 + 1, e₂⟩) hb
    (by simpa [Nat.add_assoc] using hq 2 (by omega))
    (eval_bin (d := n + 2) (op := .add) (read_set_self (by omega) rfl) (read_literal _ to.min))
    (write_ok (n := n + 2) (hW (n + 2) (by omega)) (by first | omega | (dsimp only; omega)) rfl)
  exact .succ s₀ (.succ s₁ (.succ s₂ (.zero _)))

/-! ## `adapter_correct`, the invariant through one instruction -/

/-- Every integer a BIR environment binds is in its type's range (immediates are, by WF; results
are, by `wrap_inRange`). `binOvf_steps` needs it: agreement alone would let a `.u8` register hold
`2 ^ 32`, whose word is `0`. -/
def InRange (benv : Env) : Prop := ∀ r t x, lookup benv r = some (.int t x) → t.inRange x = true

theorem inRange_cons {benv : Env} {d : Reg} {v : Val} (hr : InRange benv)
    (hv : ∀ t x, v = .int t x → t.inRange x = true) : InRange ((d, v) :: benv) := by
  intro r t x h
  simp only [lookup] at h
  by_cases hdr : d = r
  · simp [hdr] at h; exact hv t x h
  · simp [hdr] at h; exact hr r t x h

theorem evalOp_inRange {benv : Env} {a : Operand} {t : IntTy} {x : Int} (hr : InRange benv)
    (h : evalOp benv a = some (.int t x)) : t.inRange x = true := by
  cases a with
  | reg r => exact hr r t x h
  | imm t' v =>
    simp only [evalOp] at h
    split at h
    · cases h; assumption
    · cases h

theorem intArgs_ok {benv : Env} {t : IntTy} {a b : Operand} {x y : Int}
    (h : intArgs benv t a b = some (x, y)) :
    evalOp benv a = some (.int t x) ∧ evalOp benv b = some (.int t y) := by
  unfold intArgs at h
  split at h
  · rename_i ta x' tb y' ha hb
    split at h
    · rename_i hab
      obtain ⟨rfl, rfl⟩ := hab
      cases h
      exact ⟨ha, hb⟩
    · cases h
  · cases h

/-- A register's id is a slot below `valueCount`. -/
theorem regId_lt {p : Program} (hok : numberingOk p = true) {r : Reg} {n : Nat}
    (h : regId p r = some n) : n < valueCount p := by
  simp only [numberingOk, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true] at hok
  exact hok.1.1.2 n (List.mem_map.mpr ⟨(r, n), mem_of_lookup h, rfl⟩)

theorem ids_nodup {p : Program} (hok : numberingOk p = true) : ((ids p).map Prod.fst).Nodup := by
  simp only [numberingOk, Bool.and_eq_true, decide_eq_true_eq] at hok
  exact hok.1.1.1.1

theorem inter_mem {p : Program} {j i off : Nat} {b : Block} {ins : Inst}
    (hb : p.blocks[j]? = some b) (hi : b.insts[i]? = some ins) (ho : off ∈ interOffsets ins) :
    blockBase p j + instBase b i + off ∈ intermediates p := by
  simp only [intermediates, List.mem_flatMap, List.mem_map]
  exact ⟨(j, b), Runtime.mem_indexed hb, (i, ins), Runtime.mem_indexed hi, off, ho, rfl⟩

/-- No register has an intermediate's id. -/
theorem inter_not_reg {p : Program} (hok : numberingOk p = true) {m : Nat}
    (hm : m ∈ intermediates p) : ∀ r n, regId p r = some n → n ≠ m := by
  intro r n hr heq
  subst heq
  simp only [numberingOk, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true] at hok
  have h := hok.2 n hm
  have hmem : n ∈ (ids p).map Prod.snd := List.mem_map.mpr ⟨(r, n), mem_of_lookup hr, rfl⟩
  simp [hmem] at h

theorem size_set4 (e : Source.Environment) (n : Nat) (u₀ u₁ u₂ v : Option Source.Value) :
    (set4 e n u₀ u₁ u₂ v).size = e.size := by
  simp only [set4, size_set!]

/-- One `bin` instruction: its four LCB steps keep agreement, range and size, with the result
bound. -/
theorem bin_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    {j i : Nat} {b : Block} {op : BinOp} {t : IntTy} {d : Reg} {a c : Operand}
    {xs : List Source.Instruction} {s : Source.State} {lb : Source.Block} {benv benv' : Env}
    (hbj : p.blocks[j]? = some b) (hi : b.insts[i]? = some (.bin op t d a c))
    (hx : inst p (blockBase p j + instBase b i) (.bin op t d a c) = .ok xs)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 4 → lb.instructions[s.position + k]? = xs[k]?)
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hsz : s.environment.size = valueCount p)
    (hag : Agrees p benv s.environment) (hr : InRange benv)
    (he : execInst benv (.bin op t d a c) = some benv') :
    ∃ env', Source.Steps f 4 s ⟨s.block, s.position + 4, env'⟩ ∧ env'.size = valueCount p ∧
      Agrees p benv' env' ∧ InRange benv' := by
  obtain ⟨ht, a', c', ha', hc', rfl⟩ := inst_bin hx
  simp only [execInst, bind, Option.bind] at he
  split at he
  · cases he
  · rename_i xy hxy
    obtain ⟨x, y⟩ := xy
    cases he
    obtain ⟨hax, hcy⟩ := intArgs_ok hxy
    let n := blockBase p j + instBase b i
    have hd : regId p d = some (n + 3) := regId_inst (ids_nodup hok) hbj hi (by simp [instDefs])
    have hn3 : n + 3 < s.environment.size := hsz ▸ regId_lt hok hd
    have hst := bin_steps (s := s) (n := n) ht hb hq (read_operand hag ha' hax)
      (read_operand hag hc' hcy) hW hn3
    refine ⟨_, hst, by rw [size_set4]; exact hsz, ?_, ?_⟩
    · have m₀ := inter_not_reg hok (inter_mem (off := 0) hbj hi (by simp [interOffsets]))
      have m₁ := inter_not_reg hok (inter_mem (off := 1) hbj hi (by simp [interOffsets]))
      have m₂ := inter_not_reg hok (inter_mem (off := 2) hbj hi (by simp [interOffsets]))
      exact agrees_define hok (agrees_set_other (agrees_set_other (agrees_set_other hag m₀) m₁) m₂)
        hd (by simp only [size_set!]; omega)
    · exact inRange_cons hr (fun t' x' h => by cases h; exact wrap_inRange _ _)

/-- One `binOvf` instruction: its six LCB steps keep agreement, range and size, with the result
and the flag bound. -/
theorem binOvf_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    {j i : Nat} {b : Block} {op : BinOp} {t : IntTy} {d fl : Reg} {a c : Operand}
    {xs : List Source.Instruction} {s : Source.State} {lb : Source.Block} {benv benv' : Env}
    (hbj : p.blocks[j]? = some b) (hi : b.insts[i]? = some (.binOvf op t d fl a c))
    (hx : inst p (blockBase p j + instBase b i) (.binOvf op t d fl a c) = .ok xs)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 6 → lb.instructions[s.position + k]? = xs[k]?)
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hsz : s.environment.size = valueCount p)
    (hag : Agrees p benv s.environment) (hr : InRange benv)
    (he : execInst benv (.binOvf op t d fl a c) = some benv') :
    ∃ env', Source.Steps f 6 s ⟨s.block, s.position + 6, env'⟩ ∧ env'.size = valueCount p ∧
      Agrees p benv' env' ∧ InRange benv' := by
  obtain ⟨ht, a', c', ha', hc', rfl⟩ := inst_binOvf hx
  simp only [execInst, bind, Option.bind] at he
  split at he
  · cases he
  · rename_i xy hxy
    obtain ⟨x, y⟩ := xy
    cases he
    obtain ⟨hax, hcy⟩ := intArgs_ok hxy
    let n := blockBase p j + instBase b i
    have hd : regId p d = some (n + 3) := regId_inst (ids_nodup hok) hbj hi (by simp [instDefs])
    have hf : regId p fl = some (n + 5) := regId_inst (ids_nodup hok) hbj hi (by simp [instDefs])
    have hn5 : n + 5 < s.environment.size := hsz ▸ regId_lt hok hf
    obtain ⟨u₄, hst⟩ := binOvf_steps (s := s) (n := n) ht (evalOp_inRange hr hax)
      (evalOp_inRange hr hcy) hb hq (read_operand hag ha' hax) (read_operand hag hc' hcy) hW hn5
    refine ⟨_, hst, by simp only [size_set!, size_set4]; exact hsz, ?_, ?_⟩
    · have m₀ := inter_not_reg hok (inter_mem (off := 0) hbj hi (by simp [interOffsets]))
      have m₁ := inter_not_reg hok (inter_mem (off := 1) hbj hi (by simp [interOffsets]))
      have m₂ := inter_not_reg hok (inter_mem (off := 2) hbj hi (by simp [interOffsets]))
      have m₄ := inter_not_reg hok (inter_mem (off := 4) hbj hi (by simp [interOffsets]))
      have hdef : Agrees p ((d, .int t (wrap t (op.exact x y))) :: benv)
          (set4 s.environment n (some (.word (raw op x y))) (some (.word (biased t (raw op x y))))
            (some (.word (masked t (biased t (raw op x y))))) (some (word (wrap t (op.exact x y))))) :=
        agrees_define hok (agrees_set_other (agrees_set_other (agrees_set_other hag m₀) m₁) m₂) hd
          (by simp only [size_set!]; omega)
      exact agrees_define hok (agrees_set_other (x := u₄) hdef m₄) hf
        (by simp only [size_set!, size_set4]; omega)
    · exact inRange_cons (inRange_cons hr (fun t' x' h => by cases h; exact wrap_inRange _ _))
        (fun t' x' h => by cases h)

/-- One `trunc` instruction: its three LCB steps keep agreement, range and size, with the result
bound. -/
theorem trunc_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    {j i : Nat} {b : Block} {fr to : IntTy} {d : Reg} {a : Operand}
    {xs : List Source.Instruction} {s : Source.State} {lb : Source.Block} {benv benv' : Env}
    (hbj : p.blocks[j]? = some b) (hi : b.insts[i]? = some (.trunc fr to d a))
    (hx : inst p (blockBase p j + instBase b i) (.trunc fr to d a) = .ok xs)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < 3 → lb.instructions[s.position + k]? = xs[k]?)
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hsz : s.environment.size = valueCount p)
    (hag : Agrees p benv s.environment) (hr : InRange benv)
    (he : execInst benv (.trunc fr to d a) = some benv') :
    ∃ env', Source.Steps f 3 s ⟨s.block, s.position + 3, env'⟩ ∧ env'.size = valueCount p ∧
      Agrees p benv' env' ∧ InRange benv' := by
  obtain ⟨ht, a', ha', rfl⟩ := inst_trunc hx
  simp only [execInst] at he
  split at he
  · rename_i t x hax
    split at he
    · cases he
      let n := blockBase p j + instBase b i
      have hd : regId p d = some (n + 2) := regId_inst (ids_nodup hok) hbj hi (by simp [instDefs])
      have hn2 : n + 2 < s.environment.size := hsz ▸ regId_lt hok hd
      have hst := trunc_steps (s := s) (n := n) ht hb hq (read_operand hag ha' hax) hW hn2
      refine ⟨_, hst, by simp only [size_set!]; exact hsz, ?_, ?_⟩
      · have m₀ := inter_not_reg hok (inter_mem (off := 0) hbj hi (by simp [interOffsets]))
        have m₁ := inter_not_reg hok (inter_mem (off := 1) hbj hi (by simp [interOffsets]))
        exact agrees_define hok (agrees_set_other (agrees_set_other hag m₀) m₁) hd
          (by simp only [size_set!]; omega)
      · exact inRange_cons hr (fun t' x' h => by cases h; exact wrap_inRange _ _)
    · cases he
  · cases he

/-- One instruction, whichever it is: its `width` LCB steps keep agreement, range and size. -/
theorem inst_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    {j i : Nat} {b : Block} {ins : Inst}
    {xs : List Source.Instruction} {s : Source.State} {lb : Source.Block} {benv benv' : Env}
    (hbj : p.blocks[j]? = some b) (hi : b.insts[i]? = some ins)
    (hx : inst p (blockBase p j + instBase b i) ins = .ok xs)
    (hb : f.blocks[s.block]? = some lb)
    (hq : ∀ k, k < (Inst.width ins) → lb.instructions[s.position + k]? = xs[k]?)
    (hW : ∀ m, m < s.environment.size → f.valueWidths[m]? = some .word)
    (hsz : s.environment.size = valueCount p)
    (hag : Agrees p benv s.environment) (hr : InRange benv)
    (he : execInst benv ins = some benv') :
    ∃ env', Source.Steps f (Inst.width ins) s ⟨s.block, s.position + (Inst.width ins), env'⟩ ∧
      env'.size = valueCount p ∧ Agrees p benv' env' ∧ InRange benv' := by
  cases ins with
  | bin => exact bin_sim hok hbj hi hx hb hq hW hsz hag hr he
  | binOvf => exact binOvf_sim hok hbj hi hx hb hq hW hsz hag hr he
  | trunc => exact trunc_sim hok hbj hi hx hb hq hW hsz hag hr he

/-! ## `adapter_correct`, the shape of the adapted body -/

/-- Where instruction `i`'s expansion starts in its LCB block. -/
def instPos (b : Block) (i : Nat) : Nat := ((b.insts.take i).map Inst.width).sum

theorem getElem?_flatten_at {α : Type} : ∀ (xss : List (List α)) (i k : Nat) (xs : List α),
    xss[i]? = some xs → k < xs.length →
      xss.flatten[((xss.take i).map List.length).sum + k]? = xs[k]?
  | [], _, _, _, h, _ => by simp at h
  | ys :: xss, 0, k, xs, h, hk => by
    simp only [List.getElem?_cons_zero, Option.some.injEq] at h
    subst h
    simp [List.getElem?_append_left hk]
  | ys :: xss, i + 1, k, xs, h, hk => by
    simp only [List.getElem?_cons_succ] at h
    rw [List.flatten_cons, List.take_succ_cons, List.map_cons, List.sum_cons,
      List.getElem?_append_right (by omega)]
    rw [show ys.length + ((xss.take i).map List.length).sum + k - ys.length =
      ((xss.take i).map List.length).sum + k by omega]
    exact getElem?_flatten_at xss i k xs h hk

theorem mapM_ok_ok {α β ε : Type} {g : α → Except ε β} {l : List α} {l' : List β}
    (h : l.mapM g = .ok l') {i : Nat} {x : α} (hx : l[i]? = some x) :
    ∃ y, g x = .ok y ∧ l'[i]? = some y := (mapM_ok h).2 i x hx

theorem length_indexedFrom {α : Type} : ∀ (n : Nat) (l : List α), (Runtime.indexedFrom n l).length = l.length
  | _, [] => rfl
  | n, _ :: l => by simp [Runtime.indexedFrom, length_indexedFrom (n + 1) l]

theorem length_indexed {α : Type} (l : List α) : (indexed l).length = l.length :=
  length_indexedFrom 0 l

/-- What a successful `adapt` is. -/
theorem adapt_ok {p : Program} {f : Source.Function} (h : adapt p = .ok f) :
    numberingOk p = true ∧ ∃ bs, (indexed p.blocks).mapM (adaptBlock p) = .ok bs ∧
      f = { resultWidth := .word, parameters := #[],
            valueWidths := Array.replicate (valueCount p) .word, blocks := bs.toArray } := by
  rw [adapt, adaptFast_eq] at h
  unfold adaptSpec at h
  cases hn : numberingOk p <;> cases hb : (indexed p.blocks).mapM (adaptBlock p) <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- What a successful `adaptBlock` is. -/
theorem adaptBlock_ok {p : Program} {j : Nat} {b : Block} {lb : Source.Block}
    (h : adaptBlock p (j, b) = .ok lb) :
    ∃ phis xss tm, (indexed b.params).mapM (fun (k, _) => adaptPhi p j k) = .ok phis ∧
      (indexed b.insts).mapM (adaptInst p j b) = .ok xss ∧ term p b.term = .ok tm ∧
      lb = { phis := phis.toArray, instructions := xss.flatten.toArray, terminator := tm } := by
  unfold adaptBlock at h
  cases hp : (indexed b.params).mapM (fun (k, _) => adaptPhi p j k) <;>
    cases hi : (indexed b.insts).mapM (adaptInst p j b) <;> cases ht : term p b.term <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- The expansions of a block's instructions have the instructions' widths. -/
theorem insts_lengths {p : Program} {j : Nat} {b : Block} {xss : List (List Source.Instruction)}
    (hx : (indexed b.insts).mapM (adaptInst p j b) = .ok xss) :
    xss.map List.length = b.insts.map Inst.width := by
  have hlen := (mapM_ok hx).1
  rw [length_indexed] at hlen
  apply List.ext_getElem?
  intro m
  simp only [List.getElem?_map]
  cases hm : b.insts[m]? with
  | none =>
    have : xss.length ≤ m := by
      have := List.getElem?_eq_none_iff.mp hm; omega
    simp [List.getElem?_eq_none this]
  | some ins' =>
    obtain ⟨ys, hys, hym⟩ := mapM_ok_ok hx (i := m) (x := (m, ins'))
      (by simp [Runtime.getElem?_indexed, hm])
    simp [hym, inst_length (show inst p _ ins' = .ok ys from hys)]

/-- An adapted block has exactly its instructions' expansions: the terminator follows them. -/
theorem block_size {p : Program} {j : Nat} {b : Block} {lb : Source.Block}
    (h : adaptBlock p (j, b) = .ok lb) : lb.instructions.size = (b.insts.map Inst.width).sum := by
  obtain ⟨phis, xss, tm, -, hx, -, rfl⟩ := adaptBlock_ok h
  simp only [List.size_toArray, List.length_flatten, insts_lengths hx]

/-- In an adapted body, block `j` is `adaptBlock` of BIR block `j`, and every slot is a word. -/
theorem adapt_block {p : Program} {f : Source.Function} (h : adapt p = .ok f) {j : Nat} {b : Block}
    (hbj : p.blocks[j]? = some b) :
    ∃ lb, f.blocks[j]? = some lb ∧ adaptBlock p (j, b) = .ok lb := by
  obtain ⟨-, bs, hbs, rfl⟩ := adapt_ok h
  obtain ⟨lb, hlb, hi⟩ := mapM_ok_ok hbs (i := j) (x := (j, b)) (by simp [Runtime.getElem?_indexed, hbj])
  exact ⟨lb, by simpa using hi, hlb⟩

theorem adapt_widths {p : Program} {f : Source.Function} (h : adapt p = .ok f) :
    ∀ m, m < valueCount p → f.valueWidths[m]? = some .word := by
  obtain ⟨-, bs, -, rfl⟩ := adapt_ok h
  intro m hm
  simp [hm]

/-- In an adapted block, instruction `i`'s expansion sits at `instPos b i`. -/
theorem block_insts {p : Program} {j : Nat} {b : Block} {lb : Source.Block}
    (h : adaptBlock p (j, b) = .ok lb) {i : Nat} {ins : Inst} (hi : b.insts[i]? = some ins) :
    ∃ xs, inst p (blockBase p j + instBase b i) ins = .ok xs ∧
      ∀ k, k < Inst.width ins → lb.instructions[instPos b i + k]? = xs[k]? := by
  obtain ⟨phis, xss, tm, -, hx, -, rfl⟩ := adaptBlock_ok h
  obtain ⟨xs, hxs, hxi⟩ := mapM_ok_ok hx (i := i) (x := (i, ins)) (by simp [Runtime.getElem?_indexed, hi])
  refine ⟨xs, hxs, fun k hk => ?_⟩
  have hw := insts_lengths hx
  have hpos : instPos b i = ((xss.take i).map List.length).sum := by
    simp only [instPos, ← hw, List.map_take]
  simp only [List.getElem?_toArray]
  rw [hpos, getElem?_flatten_at xss i k xs hxi (by rw [inst_length hxs]; exact hk)]

theorem instPos_succ {b : Block} {i : Nat} {ins : Inst} (hi : b.insts[i]? = some ins) :
    instPos b (i + 1) = instPos b i + Inst.width ins := by
  simp [instPos, List.take_add_one, hi, List.sum_append]

/-! ## `adapter_correct`, blocks -/

/-- A block's instructions from the `i`-th on: the LCB steps from `instPos b i` keep the
invariant through all of them, as BIR's `execInsts` does. -/
theorem body_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    (hW : ∀ m, m < valueCount p → f.valueWidths[m]? = some .word)
    {j : Nat} {b : Block} {lb : Source.Block} (hbj : p.blocks[j]? = some b)
    (hlb : adaptBlock p (j, b) = .ok lb) (hfb : f.blocks[j]? = some lb) :
    ∀ (l : List Inst) (i : Nat) (benv benv' : Env) (env : Source.Environment),
      b.insts.drop i = l → execInsts benv l = some benv' → env.size = valueCount p →
      Agrees p benv env → InRange benv →
      ∃ env', Source.Steps f (l.map Inst.width).sum ⟨j, instPos b i, env⟩
          ⟨j, instPos b i + (l.map Inst.width).sum, env'⟩ ∧
        env'.size = valueCount p ∧ Agrees p benv' env' ∧ InRange benv'
  | [], _, benv, benv', env, _, he, hsz, hag, hr => by
    simp only [execInsts, Option.some.injEq] at he
    subst he
    exact ⟨env, .zero _, hsz, hag, hr⟩
  | ins :: rest, i, benv, benv', env, hd, he, hsz, hag, hr => by
    have hi : b.insts[i]? = some ins := by
      have := congrArg (·[0]?) hd
      simpa [List.getElem?_drop] using this
    have hd' : b.insts.drop (i + 1) = rest := by
      rw [← List.tail_drop, hd]; rfl
    simp only [execInsts] at he
    split at he
    · rename_i benv₁ he₁
      obtain ⟨xs, hxs, hq⟩ := block_insts hlb hi
      obtain ⟨env₁, hst, hsz₁, hag₁, hr₁⟩ := inst_sim (s := ⟨j, instPos b i, env⟩) hok hbj hi hxs hfb
        hq (fun m hm => hW m (hsz ▸ hm)) hsz hag hr he₁
      obtain ⟨env', hst', hsz', hag', hr'⟩ := body_sim hok hW hbj hlb hfb rest (i + 1) benv₁ benv'
        env₁ hd' he hsz₁ hag₁ hr₁
      rw [instPos_succ hi] at hst'
      refine ⟨env', ?_, hsz', hag', hr'⟩
      simp only [List.map_cons, List.sum_cons]
      have h := steps_append hst hst'
      rw [Nat.add_comm (List.map Inst.width rest).sum, Nat.add_assoc] at h
      exact h
    · cases he

/-! ## `adapter_correct`, terminators -/

/-- LCB's step at a block's terminator position, one lemma per terminator. -/
theorem step_ret {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {a : Source.Operand} (ht : lb.terminator = .ret a) {v : Source.Value}
    (hr : Source.read s.environment f.resultWidth a = .ok v) :
    Source.step f s = .ok (.returned v) := by
  simp [Source.step, hb, hp, ht, hr, bind, Except.bind, pure, Except.pure]

theorem step_jump {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {t : Nat} (ht : lb.terminator = .jump t) {s' : Source.State}
    (htr : Source.transfer f s.block t s.environment = .ok s') :
    Source.step f s = .ok (.next s') := by
  simp [Source.step, hb, hp, ht, htr, Functor.map, Except.map]

theorem step_branch {f : Source.Function} {s : Source.State} {lb : Source.Block}
    (hb : f.blocks[s.block]? = some lb) (hp : s.position = lb.instructions.size)
    {c : Source.Operand} {y n : Nat} (ht : lb.terminator = .branch c y n) {cv : Source.Value}
    (hc : Source.read s.environment .word c = .ok cv) {s' : Source.State}
    (htr : Source.transfer f s.block (if cv.toNat != 0 then y else n) s.environment = .ok s') :
    Source.step f s = .ok (.next s') := by
  simp only [Source.step, hb, hp, Nat.lt_irrefl, ↓reduceIte, beq_self_eq_true, ht, hc, bind,
    Except.bind]
  rw [htr]; rfl

theorem adapt_result {p : Program} {f : Source.Function} (h : adapt p = .ok f) :
    f.resultWidth = .word := by
  obtain ⟨-, bs, -, rfl⟩ := adapt_ok h
  rfl

theorem block_term {p : Program} {j : Nat} {b : Block} {lb : Source.Block}
    (h : adaptBlock p (j, b) = .ok lb) : term p b.term = .ok lb.terminator := by
  obtain ⟨phis, xss, tm, -, -, ht, rfl⟩ := adaptBlock_ok h
  exact ht

/-- `ret a`: LCB returns the encoded value of `a`. -/
theorem ret_sim {p : Program} {f : Source.Function} (hf : adapt p = .ok f) {j : Nat} {b : Block}
    {lb : Source.Block} (hfb : f.blocks[j]? = some lb) (hlb : adaptBlock p (j, b) = .ok lb)
    {a : Operand} (ht : b.term = .ret a) {benv : Env} {env : Source.Environment} {v : Val}
    (hag : Agrees p benv env) (hv : evalOp benv a = some v) :
    Source.step f ⟨j, (b.insts.map Inst.width).sum, env⟩ = .ok (.returned (enc v)) := by
  have hterm := block_term hlb
  rw [ht] at hterm
  simp only [term, termWith, ← operand_eq_with, bind, Except.bind, pure, Except.pure] at hterm
  cases ha : operand p a with
  | error e => simp [ha] at hterm
  | ok a' =>
    simp only [ha] at hterm
    injection hterm with hterm
    exact step_ret hfb (block_size hlb).symm hterm.symm
      (by rw [adapt_result hf]; exact read_operand hag ha hv)

/-- `trap s`: LCB returns the word `256 + s`. -/
theorem trap_sim {p : Program} {f : Source.Function} (hf : adapt p = .ok f) {j : Nat} {b : Block}
    {lb : Source.Block} (hfb : f.blocks[j]? = some lb) (hlb : adaptBlock p (j, b) = .ok lb)
    {st : Nat} (ht : b.term = .trap st) (env : Source.Environment) :
    Source.step f ⟨j, (b.insts.map Inst.width).sum, env⟩ = .ok (.returned (word (256 + st))) := by
  have hterm := block_term hlb
  rw [ht] at hterm
  simp only [term, termWith, pure, Except.pure] at hterm
  injection hterm with hterm
  exact step_ret hfb (block_size hlb).symm hterm.symm
    (by rw [adapt_result hf]; exact read_literal _ _)

/-! ## `adapter_correct`, edges: LCB's `transfer` -/

/-- A `forIn` whose body yields `g a s` at every element, from the state the earlier elements
left, folds `g`. -/
theorem forIn_yields {ε σ α : Type} (F : α → σ → Except ε (ForInStep σ)) (g : α → σ → σ) :
    ∀ (l : List α) (init : σ),
      (∀ i (h : i < l.length), F l[i] ((l.take i).foldl (fun s a => g a s) init) =
        .ok (.yield (g l[i] ((l.take i).foldl (fun s a => g a s) init)))) →
      forIn l init F = .ok (l.foldl (fun s a => g a s) init)
  | [], _, _ => rfl
  | a :: l, init, h => by
    rw [List.forIn_cons]
    have h0 := h 0 (by simp)
    simp only [List.getElem_cons_zero, List.take_zero, List.foldl_nil] at h0
    simp only [h0, bind, Except.bind, List.foldl_cons]
    exact forIn_yields F g l (g a init) fun i hi => by
      have := h (i + 1) (by simp; omega)
      simpa only [List.getElem_cons_succ, List.take_succ_cons, List.foldl_cons] using this

theorem size_foldl_set {α : Type} (g : α → Nat) (v : α → Option Source.Value) :
    ∀ (l : List α) (e : Source.Environment), (l.foldl (fun u x => u.set! (g x) (v x)) e).size = e.size
  | [], _ => rfl
  | x :: l, e => by simp only [List.foldl_cons, size_foldl_set g v l, size_set!]

theorem foldl_push_toList {α β : Type} (h : α → β) :
    ∀ (l : List α) (acc : Array β), (l.foldl (fun pd a => pd.push (h a)) acc).toList =
      acc.toList ++ l.map h
  | [], acc => by simp
  | a :: l, acc => by simp

theorem not_mem_take_of_nodup {α β : Type} (f : α → β) {l : List α} (hn : (l.map f).Nodup)
    {i : Nat} (hi : i < l.length) : f l[i] ∉ (l.take i).map f := by
  intro hm
  have hl : l = l.take i ++ l[i] :: l.drop (i + 1) := by simp
  rw [hl, List.map_append, List.map_cons] at hn
  exact (List.nodup_append.mp hn).2.2 _ hm _ (by simp) rfl

/-- LCB's `transfer` along an edge whose every phi has exactly one arm from the predecessor,
reading `w phi`: all phis are read from the old environment, then written. -/
theorem transfer_ok {f : Source.Function} {pred target : Nat} {env : Source.Environment}
    {tb : Source.Block} (htb : f.blocks[target]? = some tb) (w : Source.Phi → Source.Value)
    (hphi : ∀ i (h : i < tb.phis.toList.length), ∃ x op,
      tb.phis.toList[i].incoming.filter (·.1 == pred) = #[(x, op)] ∧
      Source.read env tb.phis.toList[i].width op = .ok (w tb.phis.toList[i]))
    (hdist : (tb.phis.toList.map (·.destination)).Nodup)
    (hW : ∀ phi ∈ tb.phis.toList, f.valueWidths[phi.destination]? = some .word ∧
      phi.destination < env.size ∧ (w phi).width = .word) :
    Source.transfer f pred target env = .ok ⟨target, 0,
      tb.phis.toList.foldl (fun u phi => u.set! phi.destination (some (w phi))) env⟩ := by
  unfold Source.transfer
  simp only [htb]
  rw [← Array.forIn_toList, forIn_yields _
    (fun phi (pd : Array (Source.ValueId × Source.Value)) => pd.push (phi.destination, w phi)) _ _
    ?loop]
  · simp only [bind, Except.bind]
    rw [← Array.forIn_toList, foldl_push_toList (fun phi => (phi.destination, w phi))]
    simp only [List.nil_append]
    rw [forIn_yields _ (fun (x : Source.ValueId × Source.Value) (u : Source.Environment) =>
      u.set! x.1 (some x.2)) _ _ ?writes]
    · simp [List.foldl_map, pure, Except.pure]
    case writes =>
      intro i hi
      have hm : (tb.phis.toList.map fun phi => (phi.destination, w phi))[i] ∈
          tb.phis.toList.map fun phi => (phi.destination, w phi) := List.getElem_mem hi
      obtain ⟨phi, hphm, he⟩ := List.mem_map.mp hm
      obtain ⟨hw, hs, hv⟩ := hW phi hphm
      rw [← he]
      have hsz := size_foldl_set (fun (x : Source.ValueId × Source.Value) => x.1)
        (fun x => some x.2) ((tb.phis.toList.map fun phi => (phi.destination, w phi)).take i) env
      simp only [write_ok hw (by rw [hsz]; exact hs) hv, pure, Except.pure]
  case loop =>
    intro i hi
    obtain ⟨x, op, hf, hr⟩ := hphi i hi
    have hany : ((tb.phis.toList.take i).foldl
        (fun (pd : Array (Source.ValueId × Source.Value)) phi => pd.push (phi.destination, w phi))
          #[]).any
        (fun y => y.fst == tb.phis.toList[i].destination) = false := by
      rw [Array.any_eq_false]
      intro k hk
      have hl := foldl_push_toList (fun phi => (phi.destination, w phi)) (tb.phis.toList.take i) #[]
      simp only [List.nil_append] at hl
      have hmem : (((tb.phis.toList.take i).foldl
          (fun (pd : Array (Source.ValueId × Source.Value)) phi => pd.push (phi.destination, w phi))
            #[])[k]).1 ∈
          (tb.phis.toList.take i).map (·.destination) := by
        have := Array.getElem_mem_toList hk
        rw [hl] at this
        obtain ⟨phi, hm, he⟩ := List.mem_map.mp this
        exact List.mem_map.mpr ⟨phi, hm, by rw [← he]⟩
      intro he
      exact not_mem_take_of_nodup (·.destination) hdist hi (by
        rw [beq_iff_eq] at he; rw [← he]; exact hmem)
    have h1 : (#[(x, op)].size != 1 || false) = false := rfl
    have h0 : (#[(x, op)] : Array (Source.BlockId × Source.Operand))[0]? = some (x, op) := rfl
    simp only [hf, hany, h1, h0, Bool.false_eq_true, ↓reduceIte, hr, bind, Except.bind, pure,
      Except.pure]

/-! ## `adapter_correct`, edges: the adapter's phis -/

theorem edgesTo_fst {j k : Nat} {term : Term} : ∀ x ∈ edgesTo j k term, x.1 = k := by
  intro x hx
  cases term <;> simp only [edgesTo] at hx <;> (try split at hx) <;> (try split at hx) <;>
    simp at hx <;> (try rcases hx with rfl | rfl) <;> (try subst hx) <;> rfl

/-- Picking one index out of an indexed flatMap. -/
theorem flatMap_indexedFrom_pick {α β : Type} (F : Nat → α → List β) (j : Nat) :
    ∀ (n : Nat) (l : List α), (Runtime.indexedFrom n l).flatMap
      (fun (kx : Nat × α) => if kx.1 = j then F kx.1 kx.2 else []) =
      if n ≤ j then (match l[j - n]? with | some x => F j x | none => []) else []
  | n, [] => by simp [Runtime.indexedFrom]
  | n, x :: l => by
    simp only [Runtime.indexedFrom, List.flatMap_cons]
    rw [flatMap_indexedFrom_pick F j (n + 1) l]
    rcases Nat.lt_trichotomy n j with h | h | h
    · have h1 : j - n = (j - (n + 1)) + 1 := by omega
      have h2 : n ≠ j := by omega
      simp only [h2, ↓reduceIte, List.nil_append, h1, List.getElem?_cons_succ,
        show n + 1 ≤ j by omega, show n ≤ j by omega]
    · subst h
      simp only [↓reduceIte, Nat.sub_self, List.getElem?_cons_zero, Nat.le_refl,
        show ¬ n + 1 ≤ n by omega, List.append_nil]
    · have h2 : n ≠ j := by omega
      simp only [h2, ↓reduceIte, List.nil_append, show ¬ n + 1 ≤ j by omega,
        show ¬ n ≤ j by omega]

/-- The edges into `t` from block `j` are exactly `j`'s own. -/
theorem incoming_filter {p : Program} {j t : Nat} {bj : Block} (hbj : p.blocks[j]? = some bj) :
    (incoming p t).filter (·.1 == j) = edgesTo t j bj.term := by
  unfold incoming
  rw [List.filter_flatMap]
  have hc : (fun (kx : Nat × Block) => (match kx with | (k, b) => edgesTo t k b.term).filter
      (·.1 == j)) = (fun (kx : Nat × Block) => if kx.1 = j then edgesTo t kx.1 kx.2.term else []) := by
    funext kx
    obtain ⟨k, b⟩ := kx
    simp only
    by_cases h : k = j
    · simp only [h, ↓reduceIte]
      apply List.filter_eq_self.mpr
      intro x hx
      simp [edgesTo_fst x hx]
    · simp only [h, ↓reduceIte]
      apply List.filter_eq_nil_iff.mpr
      intro x hx
      simp [edgesTo_fst x hx, h]
  rw [hc]
  have := flatMap_indexedFrom_pick (fun k (b : Block) => edgesTo t k b.term) j 0 p.blocks
  simp only [Nat.sub_zero, hbj, Nat.zero_le, ↓reduceIte] at this
  exact this

/-- A first-component-preserving `mapM` commutes with filtering on the first component. -/
theorem mapM_filter_fst {α β γ ε : Type} [BEq α] {G : α × β → Except ε (α × γ)}
    (hG : ∀ x y, G x = .ok y → y.1 = x.1) (j : α) :
    ∀ {l : List (α × β)} {l' : List (α × γ)}, l.mapM G = .ok l' →
      (l.filter (·.1 == j)).mapM G = .ok (l'.filter (·.1 == j))
  | [], l', h => by simp only [List.mapM_nil, pure, Except.pure, Except.ok.injEq] at h; subst h; rfl
  | x :: l, l', h => by
    simp only [List.mapM_cons, bind, Except.bind] at h
    cases hx : G x with
    | error e => simp [hx] at h
    | ok y =>
      cases hl : l.mapM G with
      | error e => simp [hx, hl] at h
      | ok ys =>
        simp only [hx, hl, pure, Except.pure, Except.ok.injEq] at h
        subst h
        have ih := mapM_filter_fst hG j hl
        have hy := hG x y hx
        simp only [List.filter_cons, hy]
        split
        · simp [List.mapM_cons, hx, ih, bind, Except.bind, pure, Except.pure]
        · exact ih

/-- What a successful `adaptPhi` is. -/
theorem adaptPhi_ok {p : Program} {t k : Nat} {phi : Source.Phi} (h : adaptPhi p t k = .ok phi) :
    ∃ arms, (incoming p t).mapM (fun (pa : Nat × List Operand) => (do
        let a ← match pa.2[k]? with
          | some a => operand p a
          | none => throw s!"block {pa.1} passes too few arguments to block {t}"
        pure (pa.1, a) : Except String (Nat × Source.Operand))) = .ok arms ∧
      phi = { destination := blockBase p t + k, width := .word, incoming := arms.toArray } := by
  unfold adaptPhi at h
  cases ha : (incoming p t).mapM (fun (pa : Nat × List Operand) => (do
        let a ← match pa.2[k]? with
          | some a => operand p a
          | none => throw s!"block {pa.1} passes too few arguments to block {t}"
        pure (pa.1, a) : Except String (Nat × Source.Operand))) <;>
    simp_all [bind, Except.bind, pure, Except.pure]

/-- An adapted phi has exactly one arm from a predecessor with exactly one edge into its block:
the translated `k`-th argument of that edge. -/
theorem phi_arm {p : Program} {t k j : Nat} {phi : Source.Phi} (h : adaptPhi p t k = .ok phi)
    {bj : Block} (hbj : p.blocks[j]? = some bj) {args : List Operand}
    (hedge : edgesTo t j bj.term = [(j, args)]) {a : Operand} (ha : args[k]? = some a) :
    ∃ a', operand p a = .ok a' ∧ phi.incoming.filter (·.1 == j) = #[(j, a')] := by
  obtain ⟨arms, harms, rfl⟩ := adaptPhi_ok h
  have hG : ∀ (x : Nat × List Operand) (y : Nat × Source.Operand), (do
        let a ← match x.2[k]? with
          | some a => operand p a
          | none => throw s!"block {x.1} passes too few arguments to block {t}"
        pure (x.1, a) : Except String (Nat × Source.Operand)) = .ok y → y.1 = x.1 := by
    intro x y hy
    cases hx : x.2[k]? with
    | none => simp [hx, bind, Except.bind, throw, throwThe, MonadExceptOf.throw] at hy
    | some a =>
      cases hop : operand p a with
      | error e => simp [hx, hop, bind, Except.bind] at hy
      | ok a'' =>
        simp only [hx, hop, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at hy
        rw [← hy]
  have hf := mapM_filter_fst hG j harms
  rw [incoming_filter hbj, hedge] at hf
  cases ha' : operand p a with
  | error e => simp [List.mapM_cons, ha, ha', bind, Except.bind] at hf
  | ok a' =>
    refine ⟨a', rfl, ?_⟩
    simp only [List.mapM_cons, List.mapM_nil, ha, ha', bind, Except.bind, pure, Except.pure,
      Except.ok.injEq] at hf
    simp only [List.filter_toArray, ← hf]

theorem getElem?_foldl_set_other {α : Type} (g : α → Nat) (v : α → Option Source.Value) {m : Nat} :
    ∀ (l : List α) (e : Source.Environment), (∀ x ∈ l, g x ≠ m) →
      (l.foldl (fun u x => u.set! (g x) (v x)) e)[m]? = e[m]?
  | [], _, _ => rfl
  | x :: l, e, h => by
    simp only [List.foldl_cons]
    rw [getElem?_foldl_set_other g v l _ (fun y hy => h y (by simp [hy])),
      Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds_ne (h x (by simp))]

theorem getElem?_foldl_set_mem {α : Type} (g : α → Nat) (v : α → Option Source.Value) :
    ∀ (l : List α) (e : Source.Environment), (l.map g).Nodup → ∀ x ∈ l, g x < e.size →
      (l.foldl (fun u x => u.set! (g x) (v x)) e)[g x]? = some (v x)
  | [], _, _, _, hx, _ => by simp at hx
  | y :: l, e, hn, x, hx, hs => by
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hn
    simp only [List.foldl_cons]
    rcases List.mem_cons.mp hx with rfl | hx
    · rw [getElem?_foldl_set_other g v l _ (fun z hz he => hn.1 ⟨z, hz, he⟩)]
      simp [Array.set!_eq_setIfInBounds, hs]
    · exact getElem?_foldl_set_mem g v l _ hn.2 x hx (by rw [size_set!]; exact hs)

theorem lookup_append {l₁ l₂ : Env} {r : Reg} :
    lookup (l₁ ++ l₂) r = match lookup l₁ r with | some v => some v | none => lookup l₂ r := by
  induction l₁ with
  | nil => rfl
  | cons x l ih =>
    obtain ⟨r', v'⟩ := x
    simp only [List.cons_append, lookup]
    split <;> simp_all

theorem lookup_zip {r : Reg} : ∀ {ps : List Reg} {vs : List Val}, ps.length = vs.length →
    (∀ v, lookup (ps.zip vs) r = some v → ∃ i : Nat, ps[i]? = some r ∧ vs[i]? = some v) ∧
    (lookup (ps.zip vs) r = none → r ∉ ps)
  | [], [], _ => by simp [lookup]
  | [], _ :: _, h => by simp at h
  | _ :: _, [], h => by simp at h
  | q :: ps, w :: vs, h => by
    simp only [List.length_cons, Nat.add_right_cancel_iff] at h
    obtain ⟨ih₁, ih₂⟩ := lookup_zip (r := r) h
    simp only [List.zip_cons_cons, lookup]
    by_cases hq : q = r
    · subst hq
      simp only [↓reduceIte, Option.some.injEq, reduceCtorEq, false_implies, and_true]
      intro v hv
      exact ⟨0, rfl, by simp [hv]⟩
    · simp only [hq, ↓reduceIte]
      refine ⟨fun v hv => ?_, fun hn => ?_⟩
      · obtain ⟨i, h1, h2⟩ := ih₁ v hv
        exact ⟨i + 1, by simpa using h1, by simpa using h2⟩
      · simp only [List.mem_cons, not_or]
        exact ⟨fun e => hq e.symm, ih₂ hn⟩

theorem optionMapM_ok {α β : Type} {g : α → Option β} :
    ∀ {l : List α} {l' : List β}, l.mapM g = some l' →
      l'.length = l.length ∧ ∀ (i : Nat) (x : α), l[i]? = some x → ∃ y, g x = some y ∧ l'[i]? = some y
  | [], l', h => by
    simp only [List.mapM_nil, pure, Option.some.injEq] at h
    subst h; exact ⟨rfl, fun i x hx => by simp at hx⟩
  | a :: l, l', h => by
    simp only [List.mapM_cons, bind, Option.bind] at h
    cases ha : g a with
    | none => simp [ha] at h
    | some b =>
      cases hl : l.mapM g with
      | none => simp [ha, hl] at h
      | some bs =>
        simp only [ha, hl, pure, Option.some.injEq] at h
        subst h
        obtain ⟨hlen, hel⟩ := optionMapM_ok hl
        refine ⟨by simp [hlen], fun i x hx => ?_⟩
        cases i with
        | zero => simp at hx; subst hx; exact ⟨b, ha, rfl⟩
        | succ i => simpa using hel i x (by simpa using hx)

/-- One BIR edge `j → t` with arguments `args`, the only edge from `j` into `t`: LCB's `transfer`
lands at `t`'s first position with the invariant for BIR's `enter`. -/
theorem edge_sim {p : Program} (hok : numberingOk p = true) {f : Source.Function}
    (hf : adapt p = .ok f) {j t : Nat} {bj : Block} (hbj : p.blocks[j]? = some bj)
    {args : List Operand} (hedge : edgesTo t j bj.term = [(j, args)])
    {benv benv' : Env} {env : Source.Environment} (hen : enter p t args benv = some benv')
    (hsz : env.size = valueCount p) (hag : Agrees p benv env) (hr : InRange benv) :
    ∃ env', Source.transfer f j t env = .ok ⟨t, 0, env'⟩ ∧ env'.size = valueCount p ∧
      Agrees p benv' env' ∧ InRange benv' := by
  simp only [enter] at hen
  cases hbt : p.blocks[t]? with
  | none => simp [hbt] at hen
  | some bt =>
  by_cases hl : bt.params.length ≠ args.length
  · simp [hbt, hl] at hen
  cases hvals : args.mapM (evalOp benv) with
  | none => simp [hbt, hvals] at hen
  | some vals =>
  simp only [hbt, hl, hvals, Option.bind_some, ↓reduceIte, Option.pure_def, Option.bind_eq_bind,
    Option.some.injEq] at hen
  subst hen
  simp only [ne_eq, Decidable.not_not] at hl
  obtain ⟨hvlen, hvel⟩ := optionMapM_ok hvals
  obtain ⟨tb, hfb, htb⟩ := adapt_block hf hbt
  obtain ⟨phis, xss, tm, hphis, -, -, rfl⟩ := adaptBlock_ok htb
  obtain ⟨hplen, hpel⟩ := mapM_ok hphis
  rw [length_indexed] at hplen
  -- phi `i` is `adaptPhi p t i`, for parameter `i`
  have hphi_i : ∀ i (hi : i < phis.length), adaptPhi p t i = .ok phis[i] := by
    intro i hi
    have hpi : bt.params[i]? = some bt.params[i] := List.getElem?_eq_getElem (by omega)
    obtain ⟨y, hy, hyi⟩ := hpel i (i, bt.params[i]) (by simp [Runtime.getElem?_indexed, hpi])
    rw [List.getElem?_eq_getElem hi, Option.some.injEq] at hyi
    rw [hyi]; exact hy
  have hdest : ∀ i (hi : i < phis.length), phis[i].destination = blockBase p t + i := by
    intro i hi
    obtain ⟨arms, -, he⟩ := adaptPhi_ok (hphi_i i hi)
    rw [he]
  have hreg : ∀ i (hi : i < phis.length), regId p (bt.params[i]'(by omega)).1 = some (blockBase p t + i) :=
    fun i hi => regId_param (ids_nodup hok) hbt (List.getElem?_eq_getElem (by omega))
  let w : Source.Phi → Source.Value := fun phi =>
    match vals[phi.destination - blockBase p t]? with
    | some v => enc v
    | none => word 0
  have hw : ∀ i (hi : i < phis.length), w phis[i] = enc (vals[i]'(by omega)) := by
    intro i hi
    simp only [w, hdest i hi, Nat.add_sub_cancel_left, List.getElem?_eq_getElem (show i < vals.length by omega)]
  have hev : ∀ i (hi : i < phis.length), evalOp benv (args[i]'(by omega)) = some (vals[i]'(by omega)) := by
    intro i hi
    obtain ⟨y, hy, hyi⟩ := hvel i (args[i]'(by omega)) (List.getElem?_eq_getElem (by omega))
    rw [List.getElem?_eq_getElem (show i < vals.length by omega), Option.some.injEq] at hyi
    rw [hyi]; exact hy
  have hnodup : (phis.map (·.destination)).Nodup := by
    have : phis.map (·.destination) = (List.range phis.length).map (blockBase p t + ·) := by
      apply List.ext_getElem (by simp)
      intro i h1 h2
      simp [hdest i (by simpa using h1)]
    rw [this]
    exact List.pairwise_map.mpr (List.nodup_range.imp fun h e => h (Nat.add_left_cancel e))
  have htr := transfer_ok (pred := j) (env := env) hfb w
    (fun i hi => by
      simp only [List.toList_toArray] at hi ⊢
      obtain ⟨a', hop, hfil⟩ := phi_arm (hphi_i i hi) hbj hedge
        (List.getElem?_eq_getElem (show i < args.length by omega))
      refine ⟨j, a', hfil, ?_⟩
      obtain ⟨arms, -, he⟩ := adaptPhi_ok (hphi_i i hi)
      rw [hw i hi, show phis[i].width = .word by rw [he]]
      exact read_operand hag hop (hev i hi))
    (by simpa using hnodup)
    (fun phi hm => by
      obtain ⟨i, hi, rfl⟩ := List.mem_iff_getElem.mp hm
      have hlt := regId_lt hok (hreg i hi)
      rw [hdest i hi]
      exact ⟨adapt_widths hf _ hlt, by rw [hsz]; exact hlt, by rw [hw i hi]; exact enc_width _⟩)
  refine ⟨_, htr, by rw [size_foldl_set]; exact hsz, ?_, ?_⟩
  · intro r v hv
    rw [lookup_append] at hv
    have hz := lookup_zip (r := r) (show (bt.params.map Prod.fst).length = vals.length by simp; omega)
    cases hlz : lookup ((bt.params.map Prod.fst).zip vals) r with
    | none =>
      simp only [hlz] at hv
      have hnot := hz.2 hlz
      obtain ⟨n, hn, hl⟩ := hag r v hv
      refine ⟨n, hn, ?_⟩
      rw [getElem?_foldl_set_other (·.destination) (fun phi => some (w phi)) phis env ?_]
      · exact hl
      · intro phi hm hdn
        obtain ⟨i, hi, rfl⟩ := List.mem_iff_getElem.mp hm
        rw [hdest i hi] at hdn
        subst hdn
        have := regId_injective hok hn (hreg i hi)
        exact hnot (this ▸ List.mem_map.mpr ⟨_, List.getElem_mem _, rfl⟩)
    | some v' =>
      simp only [hlz, Option.some.injEq] at hv
      subst hv
      obtain ⟨i, hpi, hvi⟩ := hz.1 v' hlz
      have hi : i < phis.length := by
        have := (List.getElem?_eq_some_iff.mp hpi).1; simp at this; omega
      have hr' : r = (bt.params[i]'(by omega)).1 := by
        simp [List.getElem?_map, List.getElem?_eq_getElem (show i < bt.params.length by omega)] at hpi
        exact hpi.symm
      refine ⟨blockBase p t + i, hr' ▸ hreg i hi, ?_⟩
      have hmem := getElem?_foldl_set_mem (·.destination) (fun phi => some (w phi)) phis env hnodup
        phis[i] (List.getElem_mem hi) (by rw [hdest i hi, hsz]; exact regId_lt hok (hreg i hi))
      simp only [hdest i hi] at hmem
      rw [hmem, hw i hi]
      rw [List.getElem?_eq_getElem (show i < vals.length by omega), Option.some.injEq] at hvi
      rw [hvi]
  · intro r t' x hv
    rw [lookup_append] at hv
    have hz := lookup_zip (r := r) (show (bt.params.map Prod.fst).length = vals.length by simp; omega)
    cases hlz : lookup ((bt.params.map Prod.fst).zip vals) r with
    | none => simp only [hlz] at hv; exact hr r t' x hv
    | some v' =>
      simp only [hlz, Option.some.injEq] at hv
      subst hv
      obtain ⟨i, hpi, hvi⟩ := hz.1 _ hlz
      have hi : i < phis.length := by
        have := (List.getElem?_eq_some_iff.mp hpi).1; simp at this; omega
      rw [List.getElem?_eq_getElem (show i < vals.length by omega), Option.some.injEq] at hvi
      exact evalOp_inRange hr (hvi ▸ hev i hi)

/-! ## `adapter_correct`, runs -/

theorem block_wf {p : Program} (hwf : WF p) {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) :
    Block.wf p b = true := by
  unfold WF wf at hwf
  simp only [Bool.and_eq_true, List.all_eq_true] at hwf
  exact hwf.2 b (List.mem_of_getElem? hb)

theorem term_wf {p : Program} (hwf : WF p) {j : Nat} {b : Block} (hb : p.blocks[j]? = some b) :
    ∃ scope, b.term.wf p scope = true := by
  have h := block_wf hwf hb
  unfold Block.wf at h
  split at h
  rename_i ok scope _
  simp only [Bool.and_eq_true] at h
  exact ⟨scope, h.2⟩

theorem condbr_ne {p : Program} (hwf : WF p) {j : Nat} {b : Block} (hb : p.blocks[j]? = some b)
    {c : Reg} {t e : Nat} {ta ea : List Operand} (ht : b.term = .condbr c t ta e ea) : t ≠ e := by
  obtain ⟨scope, h⟩ := term_wf hwf hb
  rw [ht] at h
  simp only [Term.wf, Bool.and_eq_true, bne_iff_ne] at h
  exact h.1.1.2

/-- Each BIR run from block `j` is matched by LCB from `⟨j, 0, env⟩`: an exit or a trap by a
return of its encoding, running out of fuel by at least `fuel` LCB steps. -/
theorem runBlock_sim {p : Program} (hwf : WF p) (hok : numberingOk p = true) {f : Source.Function}
    (hf : adapt p = .ok f) :
    ∀ (fuel j : Nat) (benv : Env) (env : Source.Environment), env.size = valueCount p →
      Agrees p benv env → InRange benv →
      match runBlock p fuel j benv with
      | .exited c => ∃ n s, Source.Steps f n ⟨j, 0, env⟩ s ∧
          Source.step f s = .ok (.returned (word c))
      | .trapped st => ∃ n s, Source.Steps f n ⟨j, 0, env⟩ s ∧
          Source.step f s = .ok (.returned (word (256 + st)))
      | .outOfFuel => ∃ n s, fuel ≤ n ∧ Source.Steps f n ⟨j, 0, env⟩ s
      | .stuck => True
  | 0, j, benv, env, _, _, _ => by
    simp only [runBlock]
    exact ⟨0, _, Nat.le_refl 0, .zero _⟩
  | fuel + 1, j, benv, env, hsz, hag, hr => by
    simp only [runBlock]
    cases hbj : p.blocks[j]? with
    | none => trivial
    | some bj =>
    simp only
    cases hex : execInsts benv bj.insts with
    | none => trivial
    | some benv₁ =>
    simp only
    obtain ⟨lb, hfb, hlb⟩ := adapt_block hf hbj
    obtain ⟨env₁, hst, hsz₁, hag₁, hr₁⟩ := body_sim hok (adapt_widths hf) hbj hlb hfb bj.insts 0
      benv benv₁ env (by simp) hex hsz hag hr
    simp only [instPos, List.take_zero, List.map_nil, List.sum_nil, Nat.zero_add] at hst
    -- after a jump to `tgt`, the rest of the run is the run from `tgt`
    have cont : ∀ (tgt : Nat) (benv₂ : Env) (env₂ : Source.Environment),
        Source.Steps f ((bj.insts.map Inst.width).sum + 1) ⟨j, 0, env⟩ ⟨tgt, 0, env₂⟩ →
        env₂.size = valueCount p → Agrees p benv₂ env₂ → InRange benv₂ →
        match runBlock p fuel tgt benv₂ with
        | .exited c => ∃ n s, Source.Steps f n ⟨j, 0, env⟩ s ∧
            Source.step f s = .ok (.returned (word c))
        | .trapped st => ∃ n s, Source.Steps f n ⟨j, 0, env⟩ s ∧
            Source.step f s = .ok (.returned (word (256 + st)))
        | .outOfFuel => ∃ n s, fuel + 1 ≤ n ∧ Source.Steps f n ⟨j, 0, env⟩ s
        | .stuck => True := by
      intro tgt benv₂ env₂ hpre hsz₂ hag₂ hr₂
      have ih := runBlock_sim hwf hok hf fuel tgt benv₂ env₂ hsz₂ hag₂ hr₂
      revert ih
      cases runBlock p fuel tgt benv₂ with
      | exited c =>
        rintro ⟨n, s, hs, hret⟩
        exact ⟨_, s, steps_append hpre hs, hret⟩
      | trapped st =>
        rintro ⟨n, s, hs, hret⟩
        exact ⟨_, s, steps_append hpre hs, hret⟩
      | outOfFuel =>
        rintro ⟨n, s, hn, hs⟩
        exact ⟨_, s, by omega, steps_append hpre hs⟩
      | stuck => intro; trivial
    have pre : ∀ {tgt : Nat} {env₂ : Source.Environment},
        Source.step f ⟨j, (bj.insts.map Inst.width).sum, env₁⟩ = .ok (.next ⟨tgt, 0, env₂⟩) →
        Source.Steps f ((bj.insts.map Inst.width).sum + 1) ⟨j, 0, env⟩ ⟨tgt, 0, env₂⟩ := by
      intro tgt env₂ hstep
      have := steps_append hst (.succ hstep (.zero _))
      rwa [Nat.add_comm] at this
    cases htm : bj.term with
    | ret a =>
      cases hv : evalOp benv₁ a with
      | none => simp only [hv]
      | some v =>
        rcases v with ⟨ty, c⟩ | bb
        · cases ty
          case u8 =>
            simp only [hv]
            have hc0 : 0 ≤ c := by
              have := evalOp_inRange hr₁ hv
              simp [IntTy.inRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at this
              omega
            refine ⟨_, _, hst, ?_⟩
            rw [ret_sim hf hfb hlb htm hag₁ hv]
            simp [enc, Int.toNat_of_nonneg hc0]
          all_goals simp only [hv]
        · simp only [hv]
    | trap st =>
      by_cases hs : st < p.sites.length
      · simp only [hs, ↓reduceIte]
        exact ⟨_, _, hst, trap_sim hf hfb hlb htm env₁⟩
      · simp only [hs, ↓reduceIte]
    | br t args =>
      simp only
      cases hen : enter p t args benv₁ with
      | none => trivial
      | some benv₂ =>
      simp only
      have hedge : edgesTo t j bj.term = [(j, args)] := by simp [htm, edgesTo]
      obtain ⟨env₂, htr, hsz₂, hag₂, hr₂⟩ := edge_sim hok hf hbj hedge hen hsz₁ hag₁ hr₁
      have hterm := block_term hlb
      rw [htm] at hterm
      simp only [term, termWith, pure, Except.pure, Except.ok.injEq] at hterm
      exact cont t benv₂ env₂ (pre (step_jump hfb (block_size hlb).symm hterm.symm htr)) hsz₂ hag₂ hr₂
    | condbr c t ta e ea =>
      have hne := condbr_ne hwf hbj htm
      have hterm := block_term hlb
      rw [htm] at hterm
      simp only [term, termWith, ← idOf_eq_with, bind, Except.bind] at hterm
      cases hid : idOf p c with
      | error e => simp [hid] at hterm
      | ok nc =>
      simp only [hid, pure, Except.pure, Except.ok.injEq] at hterm
      cases hl : lookup benv₁ c with
      | none => simp only [hl]
      | some v =>
      rcases v with ⟨ty, x⟩ | bb
      · simp only [hl]
      have hread : Source.read env₁ .word (.local nc) = .ok (enc (.bool bb)) :=
        read_operand (a := .reg c) hag₁ (by simp [operand, operandWith, ← idOf_eq_with, hid, bind, Except.bind, pure, Except.pure])
          (by simpa [evalOp] using hl)
      cases bb with
      | true =>
        simp only [hl]
        cases hen : enter p t ta benv₁ with
        | none => simp only
        | some benv₂ =>
        simp only
        have hedge : edgesTo t j bj.term = [(j, ta)] := by
          simp [htm, edgesTo, Ne.symm hne]
        obtain ⟨env₂, htr, hsz₂, hag₂, hr₂⟩ := edge_sim hok hf hbj hedge hen hsz₁ hag₁ hr₁
        exact cont t benv₂ env₂ (pre (step_branch hfb (block_size hlb).symm hterm.symm hread
          (by simpa [enc, word, Source.Value.toNat] using htr))) hsz₂ hag₂ hr₂
      | false =>
        simp only [hl]
        cases hen : enter p e ea benv₁ with
        | none => simp only
        | some benv₂ =>
        simp only
        have hedge : edgesTo e j bj.term = [(j, ea)] := by
          simp [htm, edgesTo, hne]
        obtain ⟨env₂, htr, hsz₂, hag₂, hr₂⟩ := edge_sim hok hf hbj hedge hen hsz₁ hag₁ hr₁
        exact cont e benv₂ env₂ (pre (step_branch hfb (block_size hlb).symm hterm.symm hread
          (by simpa [enc, word, Source.Value.toNat] using htr))) hsz₂ hag₂ hr₂

theorem steps_prefix {f : Source.Function} :
    ∀ {k : Nat} {s u : Source.State}, Source.Steps f k s u → ∀ n, n ≤ k → ∃ t, Source.Steps f n s t
  | _, s, _, _, 0, _ => ⟨s, .zero _⟩
  | _, _, _, .succ h₁ h₂, n + 1, hn => by
    obtain ⟨t, ht⟩ := steps_prefix h₂ n (by omega)
    exact ⟨t, .succ h₁ ht⟩
  | _, _, _, .zero _, _ + 1, hn => by omega

theorem entry_params {p : Program} (hwf : WF p) {b : Block} (hb : p.blocks[0]? = some b) :
    b.params = [] := by
  unfold WF wf at hwf
  simp only [Bool.and_eq_true, hb, List.isEmpty_iff] at hwf
  exact hwf.1.1.1.1.1

/-- LCB's initial state for the adapted body: the entry block, an empty environment. -/
theorem initial_ok {p : Program} (hwf : WF p) {f : Source.Function} (hf : adapt p = .ok f)
    {b : Block} (hb : p.blocks[0]? = some b) :
    Source.initial f #[] = .ok ⟨0, 0, Array.replicate (valueCount p) none⟩ := by
  obtain ⟨lb, hfb, hlb⟩ := adapt_block hf hb
  obtain ⟨phis, xss, tm, hphis, -, -, rfl⟩ := adaptBlock_ok hlb
  rw [entry_params hwf hb] at hphis
  simp only [indexed, Runtime.indexedFrom, List.mapM_nil, pure, Except.pure, Except.ok.injEq] at hphis
  subst hphis
  have hf' := adapt_ok hf
  obtain ⟨-, bs, -, rfl⟩ := hf'
  simp [Source.initial, hfb, bind, Except.bind, pure, Except.pure]

/-- **`adapter_correct`, over runs.** For a well-formed BIR program that the adapter accepts, under
LCB's own semantics (`Source.HasBehavior`, imported and never restated): a BIR run that exits with
`c` makes the adapted body return the word `c`; one that traps at site `s` makes it return the
word `256 + s`; and a program that runs out of fuel at every fuel makes it diverge. -/
theorem adapter_correct_run {p : Program} (hwf : WF p) {f : Source.Function}
    (hf : adapt p = .ok f) :
    (∀ fuel c, run p fuel = .exited c → Source.HasBehavior f #[] (.returned (word c))) ∧
    (∀ fuel st, run p fuel = .trapped st →
      Source.HasBehavior f #[] (.returned (word (256 + st)))) ∧
    ((∀ fuel, run p fuel = .outOfFuel) → Source.HasBehavior f #[] .diverges) := by
  have hok := (adapt_ok hf).1
  have sim := fun fuel => runBlock_sim hwf hok hf fuel 0 [] (Array.replicate (valueCount p) none)
    (by simp) (fun r v h => by simp [lookup] at h) (fun r t x h => by simp [lookup] at h)
  refine ⟨fun fuel c h => ?_, fun fuel st h => ?_, fun h => ?_⟩
  · unfold run at h
    cases hb : p.blocks[0]? with
    | none => simp [hb] at h
    | some b =>
      simp only [hb] at h
      have := sim fuel
      rw [h] at this
      exact ⟨_, initial_ok hwf hf hb, this⟩
  · unfold run at h
    cases hb : p.blocks[0]? with
    | none => simp [hb] at h
    | some b =>
      simp only [hb] at h
      have := sim fuel
      rw [h] at this
      exact ⟨_, initial_ok hwf hf hb, this⟩
  · have h0 := h 0
    unfold run at h0
    cases hb : p.blocks[0]? with
    | none => simp [hb] at h0
    | some b =>
      refine ⟨_, initial_ok hwf hf hb, fun n => ?_⟩
      have hn := h n
      unfold run at hn
      simp only [hb] at hn
      have := sim n
      rw [hn] at this
      obtain ⟨k, s, hk, hs⟩ := this
      exact steps_prefix hs n hk

/-! ### LCB's behaviour is unique -/

/-- Determinism: the state `n` steps reach is the prefix of any longer run. -/
theorem steps_split {f : Source.Function} :
    ∀ {n m : Nat} {s t u : Source.State}, Source.Steps f n s t → Source.Steps f (n + m) s u →
      Source.Steps f m t u
  | 0, _, _, _, _, .zero _, h => by simpa using h
  | n + 1, m, _, _, _, .succ h₁ h₂, h => by
    rw [show n + 1 + m = (n + m) + 1 by omega] at h
    cases h with
    | succ h₁' h₂' =>
      cases Source.step_deterministic f _ _ _ h₁ h₁'
      exact steps_split h₂ h₂'

theorem returned_not_step {f : Source.Function} {t u : Source.State} {v : Source.Value}
    (hr : Source.step f t = .ok (.returned v)) (hs : Source.Step f t u) : False := by
  unfold Source.Step at hs
  rw [hr] at hs
  cases hs

theorem returns_le {f : Source.Function} {s t₁ t₂ : Source.State} {v₁ v₂ : Source.Value}
    {n k : Nat} (h₁ : Source.Steps f n s t₁) (r₁ : Source.step f t₁ = .ok (.returned v₁))
    (h₂ : Source.Steps f (n + k) s t₂) (r₂ : Source.step f t₂ = .ok (.returned v₂)) :
    v₁ = v₂ := by
  have h := steps_split h₁ h₂
  cases k with
  | zero =>
    cases h
    rw [r₁] at r₂
    cases r₂
    rfl
  | succ k =>
    cases h with
    | succ hs _ => exact (returned_not_step r₁ hs).elim

theorem returns_unique {f : Source.Function} {s : Source.State} {v₁ v₂ : Source.Value}
    (h₁ : Source.Returns f s v₁) (h₂ : Source.Returns f s v₂) : v₁ = v₂ := by
  obtain ⟨n₁, t₁, s₁, r₁⟩ := h₁
  obtain ⟨n₂, t₂, s₂, r₂⟩ := h₂
  rcases Nat.le_total n₁ n₂ with h | h
  · obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le h
    exact returns_le s₁ r₁ s₂ r₂
  · obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le h
    exact (returns_le s₂ r₂ s₁ r₁).symm

theorem returns_not_diverges {f : Source.Function} {s : Source.State} {v : Source.Value}
    (hr : Source.Returns f s v) (hd : Source.Diverges f s) : False := by
  obtain ⟨n, t, hs, ht⟩ := hr
  obtain ⟨u, hu⟩ := hd (n + 1)
  cases steps_split hs hu with
  | succ h _ => exact returned_not_step ht h

/-- LCB's behaviour of a function on given arguments is unique. -/
theorem hasBehavior_unique {f : Source.Function} {args : Array Source.Value}
    {b₁ b₂ : Source.Behavior} (h₁ : Source.HasBehavior f args b₁)
    (h₂ : Source.HasBehavior f args b₂) : b₁ = b₂ := by
  obtain ⟨s₁, i₁, h₁⟩ := h₁
  obtain ⟨s₂, i₂, h₂⟩ := h₂
  rw [i₁] at i₂
  cases i₂
  cases b₁ with
  | returned v₁ =>
    cases b₂ with
    | returned v₂ => rw [returns_unique h₁ h₂]
    | diverges => exact (returns_not_diverges h₁ h₂).elim
  | diverges =>
    cases b₂ with
    | returned v₂ => exact (returns_not_diverges h₂ h₁).elim
    | diverges => rfl

/-! ### `adapter_correct` -/

theorem word_nat (n : Nat) : word (n : Int) = .word (BitVec.ofNat 32 n) := by
  simp [word]

/-- The adapted body has a behaviour, and it decodes to BIR's: a return of the word that BIR's
outcome encodes, or divergence exactly when BIR diverges. -/
theorem adapter_correct_behaviour {p : Program} (hwf : WF p) {f : Source.Function}
    (hf : adapt p = .ok f) :
    (∃ w, Source.HasBehavior f #[] (.returned (.word w)) ∧ behaviour p = decode p.sites w) ∨
    (Source.HasBehavior f #[] .diverges ∧ behaviour p = .diverges) := by
  obtain ⟨hexit, htrap, hdiv⟩ := adapter_correct_run hwf hf
  by_cases hex : ∃ n, (run p n).final = true
  · obtain ⟨n, hn⟩ := hex
    have hb := behaviour_of_run hn
    have henc : (run p n).Encodable p.sites := by
      rcases run_progress hwf n with h | h
      · exact h
      · rw [h] at hn
        cases hn
    cases hr : run p n with
    | exited c =>
      rw [hr] at hb henc
      refine .inl ⟨BitVec.ofNat 32 c, ?_, ?_⟩
      · rw [← word_nat]
        exact hexit n c hr
      · rw [hb, decode_encode p.sites _ c rfl henc]
    | trapped st =>
      rw [hr] at hb henc
      refine .inl ⟨BitVec.ofNat 32 (256 + st), ?_, ?_⟩
      · rw [← word_nat, show (((256 + st : Nat)) : Int) = 256 + (st : Int) by omega]
        exact htrap n st hr
      · rw [hb, decode_encode p.sites _ (256 + st) rfl henc]
    | stuck =>
      rw [hr] at henc
      exact henc.elim
    | outOfFuel =>
      rw [hr] at hn
      cases hn
  · have hall : ∀ fuel, run p fuel = .outOfFuel := by
      intro fuel
      cases hr : run p fuel with
      | outOfFuel => rfl
      | _ => exact absurd ⟨fuel, by simp [hr, Outcome.final]⟩ hex
    exact .inr ⟨hdiv hall, (behaviour_diverges_iff p).mpr hall⟩

/-- **`adapter_correct`.** For a well-formed BIR program that the adapter accepts, whatever result
the adapted body produces under LCB's own semantics (`bodyResult`, from `Source.HasBehavior`,
imported and never restated) decodes, with the program's site table, to the BIR behaviour. -/
theorem adapter_correct {p : Program} (hwf : WF p) {f : Source.Function}
    (hf : adapt p = .ok f) (r : BodyResult) (hr : bodyResult f r) :
    r.behaviour p.sites = behaviour p := by
  rcases adapter_correct_behaviour hwf hf with ⟨w, hw, hb⟩ | ⟨hd, hb⟩
  · cases r with
    | returns w' =>
      have := hasBehavior_unique (by unfold bodyResult at hr; exact hr) hw
      simp only [Source.Behavior.returned.injEq, Source.Value.word.injEq] at this
      subst this
      exact hb.symm
    | diverges => cases hasBehavior_unique (by unfold bodyResult at hr; exact hr) hw
    | stuck => exact (hr.1 w hw).elim
  · cases r with
    | returns w' => cases hasBehavior_unique (by unfold bodyResult at hr; exact hr) hd
    | diverges => exact hb.symm
    | stuck => exact (hr.2 hd).elim

theorem emit_ok {p : Program} {f : Source.Function} {tbl : SiteTable}
    (h : emit p = .ok (f, tbl)) : adapt p = .ok f ∧ tbl = p.sites := by
  unfold emit at h
  cases ha : adapt p with
  | error e => rw [ha] at h; cases h
  | ok g =>
    rw [ha] at h
    cases h
    exact ⟨rfl, rfl⟩

/-- **The LCB backend contract, complete.** A refused adaptation emits nothing (`emit` is
`.error`); a successful one emits a body whose every result, under LCB's own semantics, decodes
with the emitted site table to the BIR behaviour. Retires `P_adapter`. -/
theorem emit_correct {p : Program} (hwf : WF p) {f : Source.Function} {tbl : SiteTable}
    (h : emit p = .ok (f, tbl)) (r : BodyResult) (hr : bodyResult f r) :
    r.behaviour tbl = behaviour p := by
  obtain ⟨ha, rfl⟩ := emit_ok h
  exact adapter_correct hwf ha r hr

/-- An adapter refusal produces no artifact, for every target. -/
theorem artifact_of_emit_error {p : Program} {e : String} (h : emit p = .error e) (t : Target) :
    artifact t p = .error e := by
  unfold artifact
  rw [h]
  rfl

/-- Every artifact comes from a successful `emit` whose body is correct. -/
theorem artifact_correct {p : Program} (hwf : WF p) {t : Target} {s : String}
    (h : artifact t p = .ok s) : ∃ f tbl, emit p = .ok (f, tbl) ∧ assemble t f = .ok s ∧
      ∀ r, bodyResult f r → r.behaviour tbl = behaviour p := by
  unfold artifact at h
  cases he : emit p with
  | error e => rw [he] at h; cases h
  | ok ft =>
    obtain ⟨f, tbl⟩ := ft
    rw [he] at h
    exact ⟨f, tbl, rfl, h, emit_correct hwf he⟩

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
