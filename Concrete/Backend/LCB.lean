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

/-- The LCB value id of a register. `adapt` refuses a program where a register has two
definitions, so this is a function of the register. -/
def regId (p : Program) (r : Reg) : Option Nat := (ids p).lookup r

def idOf (p : Program) (r : Reg) : Except String Nat :=
  match regId p r with
  | some n => .ok n
  | none => .error s!"register %r{r} has no definition"

def operand (p : Program) : Operand → Except String Source.Operand
  | .reg r => return .local (← idOf p r)
  | .imm _ v => return .literal (word v)

def opBinary : BinOp → Source.Binary
  | .add => .add
  | .sub => .sub
  | .mul => .mul

def ins (d : Nat) (op : Source.Operation) : Source.Instruction :=
  { destination := d, width := .word, operation := op }

/-- An instruction's LCB instructions, its first value numbered `n`. -/
def inst (p : Program) (n : Nat) : Inst → Except String (List Source.Instruction)
  | .bin op t _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operand p a
    let b ← operand p b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min)))]
  | .binOvf op t _ _ a b => do
    unless inProfile t do throw s!"{repr t} is outside LCB's slice-0 profile"
    let a ← operand p a
    let b ← operand p b
    return [ins n (.binary (opBinary op) a b),
      ins (n + 1) (.binary .sub (.local n) (.literal (word t.min))),
      ins (n + 2) (.binary .andBits (.local (n + 1)) (.literal (word (2 ^ t.bits - 1)))),
      ins (n + 3) (.binary .add (.local (n + 2)) (.literal (word t.min))),
      ins (n + 4) (.compare .unsignedLT .word (.local (n + 1)) (.literal (word (2 ^ t.bits)))),
      ins (n + 5) (.compare .eq .word (.local (n + 4)) (.literal (word 0)))]
  | .trunc fr to _ a => do
    unless inProfile fr && inProfile to do throw s!"{repr fr} → {repr to} is outside LCB's slice-0 profile"
    let a ← operand p a
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

def term (p : Program) : Term → Except String Source.Terminator
  | .ret a => return .ret (← operand p a)
  | .trap s => return .ret (.literal (word (256 + s)))
  | .br t _ => return .jump t
  | .condbr c t _ e _ => return .branch (.local (← idOf p c)) t e

/-- `main`'s body as a function of LCB's verified source profile. -/
def adapt (p : Program) : Except String Source.Function := do
  unless decide ((ids p).map Prod.fst).Nodup do throw "a register is defined twice"
  let blocks ← (indexed p.blocks).mapM fun (j, b) => do
    let phis ← (indexed b.params).mapM fun (k, (_, _)) => do
      let arms ← (incoming p j).mapM fun (pred, args) => do
        let a ← match args[k]? with
          | some a => operand p a
          | none => throw s!"block {pred} passes too few arguments to block {j}"
        pure (pred, a)
      pure ({ destination := blockBase p j + k, width := .word, incoming := arms.toArray } : Source.Phi)
    let insts ← (indexed b.insts).mapM fun (i, ins) => inst p (blockBase p j + instBase b i) ins
    pure ({ phis := phis.toArray, instructions := insts.flatten.toArray,
            terminator := ← term p b.term } : Source.Block)
  return { resultWidth := .word, parameters := #[],
           valueWidths := Array.replicate (valueCount p) .word, blocks := blocks.toArray }

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
    unfold inst at h
    cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
      simp_all [Inst.width, bind, Except.bind, pure, Except.pure] <;> (subst h; rfl)
  | binOvf op t d f a b =>
    unfold inst at h
    cases hp : inProfile t <;> cases ha : operand p a <;> cases hb : operand p b <;>
      simp_all [Inst.width, bind, Except.bind, pure, Except.pure] <;> (subst h; rfl)
  | trunc fr to d a =>
    unfold inst at h
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
