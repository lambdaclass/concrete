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
  | .int t => .integer (Width.ofCore (toCore t))
  | .bool => .flag

/-- The bit pattern of `v` in `t`, as an unsigned decimal. -/
def pattern (t : IntTy) (v : Int) : Nat := toBits (toCore t) v

/-- Encode the payload at the width of its LLVM operand slot. Raw BIR can
carry a different immediate tag, so the slot determines the bit pattern. -/
private def operandAt (width : Nat) : Operand → Argument
  | .reg r => .local (.register r)
  | .imm _ v => .immediate (.natural ((v % (2 : Int) ^ width).toNat))

/-- Encode a standalone BIR scalar at the width of its own tag. -/
def operand : Operand → Argument
  | .reg r => .local (.register r)
  | .imm t v => operandAt (toCore t).bits (.imm t v)

/-- Read the immediate field of the actual LLVM argument. -/
def Argument.immediateValue : Argument → Option Nat
  | .immediate v => some v.value
  | _ => none

/-- Expected immediate value at the emitted width, using Core's modulo rule.
A register contributes no immediate. -/
def expectedImmediate (width : Nat) : Operand → Option Nat
  | .reg _ => none
  | .imm _ value => some ((value % (2 : Int) ^ width).toNat)

/-- Read the token through the actual LLVM argument. -/
def Argument.immediateToken : Argument → Option String
  | .immediate v => some v.token
  | _ => none

/-- Decode a nonnegative decimal value that fits the declared Core width. -/
def decodePattern (t : IntTy) : Int → Option Int
  | .ofNat n => if n < 2 ^ (toCore t).bits then some (ofBits (toCore t) n) else none
  | .negSucc _ => none

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

/-- Read the operands of the leading arithmetic instruction or checked call. -/
def binarySources : List Item → List Argument
  | .instruction (.bin _ _ _ lhs rhs) :: _ => [lhs, rhs]
  | .instruction (.call _ _ _ args) :: _ => args.map Prod.snd
  | _ => []

/-- Read the immediate source of a conversion or its equal-width copy. -/
def conversionSource : List Item → Option Nat
  | .instruction (.convert _ _ (_, source) _) :: _ => source.immediateValue
  | .instruction (.bin _ .add _ source _) :: _ => source.immediateValue
  | _ => none

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

/-- Read each phi arm’s immediate field in predecessor order. -/
def phiSources : Item → List (Option Nat)
  | .instruction (.phi _ _ arms) => arms.map fun (value, _) => value.immediateValue
  | _ => []

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

private def expectedOperand (role : IntegerRole) (width : Nat) : Operand → List IntegerField
  | .reg _ => []
  | .imm _ v => [(role, some width, (v % (2 : Int) ^ width).toNat)]

private def widthField (position width : Nat) : IntegerField := (.typeWidth position 0, none, width)

private def pairFields (width : Nat) : List IntegerField :=
  [widthField 0 width, (.typeWidth 0 1, none, 1)]

private def expectedInst : Inst → List (List IntegerField)
  | .bin _ t _ a b =>
    let width := (toCore t).bits
    [expectedOperand .left width a ++ expectedOperand .right width b ++ [widthField 0 width]]
  | .binOvf _ t _ _ a b =>
    let width := (toCore t).bits
    [expectedOperand (.callArgument 0) width a ++ [widthField 1 width] ++
      expectedOperand (.callArgument 1) width b ++ [widthField 2 width] ++
      pairFields width ++ [(.intrinsicWidth, none, width)],
      [(.extractIndex, none, 0)] ++ pairFields width, [(.extractIndex, none, 1)] ++ pairFields width]
  | .trunc source target _ a =>
    let src := (toCore source).bits
    let dst := (toCore target).bits
    if src = dst then
      [expectedOperand .left dst a ++ [(.right, some dst, 0), widthField 0 dst]]
    else [expectedOperand .conversionSource src a ++ [widthField 0 src, widthField 1 dst]]

private def expectedTerm : Term → List (List IntegerField)
  | .ret a => [expectedOperand .conversionSource 8 a ++ [widthField 0 8, widthField 1 32], [widthField 0 32]]
  | .trap s => [[(.returnValue, some 32, toBits .u32 (Int.ofNat (256 + s))), widthField 0 32]]
  | .br .. => [[]]
  | .condbr .. => [[widthField 0 1]]

private def expectedPhiSource (j k pred : Nat) (b : Block) : List (Operand × Nat) :=
  let incoming (target : Nat) (args : List Operand) :=
    if target == j then [(args.getD k (.imm .u8 0), pred)] else []
  match b.term with
  | .br target args => incoming target args
  | .condbr _ yes yesArgs no noArgs => incoming yes yesArgs ++ incoming no noArgs
  | .ret _ | .trap _ => []

private def expectedPhi (p : Program) (j k : Nat) (t : Ty) : List IntegerField :=
  let width := match t with | .int t => (toCore t).bits | .bool => 1
  let sources := p.blocks.zipIdx.flatMap fun (b, pred) => expectedPhiSource j k pred b
  [widthField 0 width] ++ sources.zipIdx.flatMap fun ((a, pred), index) =>
    expectedOperand (.phiArm index (.block pred)) width a

/-- The integer inventory specified by BIR lowering and the runtime contract. -/
def expectedInventory (p : Program) : List (List IntegerField) :=
  let checkedTypes := (p.blocks.flatMap fun b => b.insts.filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none).eraseDups
  checkedTypes.map (fun (_, t) =>
    let width := (toCore t).bits
    pairFields width ++ [(.intrinsicWidth, none, width), widthField 1 width, widthField 2 width]) ++
  [[], [widthField 0 32]] ++
  (p.blocks.zipIdx.flatMap fun (b, j) =>
    [[]] ++ (b.params.zipIdx.map fun ((_, t), k) => expectedPhi p j k t) ++
      b.insts.flatMap expectedInst ++ expectedTerm b.term) ++ [[], []] ++
  Runtime.expectedInventory p.sites

/-- The entire rendered LLVM module. -/
def module (p : Program) : String := render (moduleItems p)

/-! ## Producer and text laws -/

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

private theorem operand_immediate (width : Nat) (a : Operand) :
    (operandAt width a).immediateValue = expectedImmediate width a := by
  cases a <;> rfl

/-- Law. The LLVM producer puts the Spec/Scalar encoding of the BIR payload
in the immediate field consumed by the renderer. -/
theorem operand_field (t : IntTy) (v : Int) :
    (operand (.imm t v)).immediateValue = some (toBits (toCore t) v) :=
  rfl

/-- Law. Every emitted BIR immediate is a token in the decimal grammar.
Authority: docs/decisions.md#od126. -/
theorem operand_token (t : IntTy) (v : Int) :
    ∃ token, (operand (.imm t v)).immediateToken = some token ∧
      DecimalToken token (Int.ofNat (pattern t v)) :=
  ⟨_, rfl, decimal_token (pattern t v)⟩

/-- Law. The emitted decimal token parses to exactly the encoded bit pattern.
Authority: docs/decisions.md#od126. -/
theorem operand_parse (t : IntTy) (v : Int) :
    (operand (.imm t v)).immediateToken.bind parseDecimal = some (Int.ofNat (pattern t v)) := by
  obtain ⟨token, field, valid⟩ := operand_token t v
  rw [field, Option.bind_some]
  exact (parseDecimal_exact _ _).2 valid

/-- Law. A representable immediate's emitted decimal token denotes its Core value.
The interpretation of the enclosing LLVM text still uses `P_print`.
Authority: docs/decisions.md#od126. -/
theorem operand_roundTrip (t : IntTy) (v : Int) (h : (toCore t).InRange v) :
    ((operand (.imm t v)).immediateToken.bind parseDecimal).bind (decodePattern t) = some v := by
  rw [operand_parse, Option.bind_some, decodePattern]
  simp [pattern_fits t v, pattern_roundTrip t v h]

private theorem width_bits (t : ConcreteSpec.Core.IntTy) :
    (Width.ofCore t).bits = t.bits := by cases t <;> rfl

/-- The arithmetic and checked-arithmetic items carry the BIR operands in order. -/
theorem bin_fields (op : BinOp) (t : IntTy) (d : Nat) (a b : Operand) :
    (binarySources (inst (.bin op t d a b))).map Argument.immediateValue =
      [expectedImmediate (toCore t).bits a, expectedImmediate (toCore t).bits b] := by
  simp [inst, binarySources, operand_immediate, width_bits]

theorem binOvf_fields (op : BinOp) (t : IntTy) (d f : Nat) (a b : Operand) :
    (binarySources (inst (.binOvf op t d f a b))).map Argument.immediateValue =
      [expectedImmediate (toCore t).bits a, expectedImmediate (toCore t).bits b] := by
  simp [inst, binarySources, operand_immediate, width_bits]

/-- Both the width-changing instruction and the equal-width copy use the
encoded BIR operand as their source. -/
theorem trunc_field (source target : IntTy) (d : Nat) (a : Operand) :
    conversionSource (inst (.trunc source target d a)) = expectedImmediate (toCore source).bits a := by
  simp only [inst]
  split
  · rename_i sameWidth
    rw [sameWidth]
    exact (operand_immediate _ a).trans (by rw [width_bits])
  · exact (operand_immediate _ a).trans (by rw [width_bits])

/-- Each phi arm uses the encoding of the corresponding incoming edge operand. -/
theorem phi_fields (p : Program) (j : Nat) (b : Block) :
    (phis p j b).map phiSources =
      (indexed b.params).map fun (k, _, t) =>
        ((edges p).filter (·.1 == j)).map fun (_, _, args) =>
          expectedImmediate (match t with | .int t => (toCore t).bits | .bool => 1)
            (args.getD k (.imm .u8 0)) := by
  simp only [phis, List.map_map]
  apply List.map_congr_left
  intro entry _
  rcases entry with ⟨k, r, ty⟩
  simp only [Function.comp_apply, phiSources, List.map_map]
  apply List.map_congr_left
  intro edge _
  rcases edge with ⟨target, pred, args⟩
  cases ty <;> simp [LLVM.ty, TypeCode.integerWidth, width_bits, operand_immediate]

/-- The BIR return's immediate flows into the source operand of the actual
zero-extension item. The following LLVM return reads that item's result. -/
theorem ret_immediate_field (j : Nat) (t : IntTy) (v : Int) :
    (match (term j (.ret (.imm t v))).head? with
     | some (.instruction (.convert _ _ (_, source) _)) => source.immediateValue
     | _ => none) = some (toBits .u8 v) :=
  rfl

private theorem operand_inventory (a : Operand) (role : IntegerRole) (width : Width) :
    (operandAt width.bits a).inventory role (.integer width) = expectedOperand role width.bits a := by
  cases a <;> rfl

private theorem core_operand_inventory (a : Operand) (role : IntegerRole) (t : ConcreteSpec.Core.IntTy) :
    (operandAt t.bits a).inventory role (.integer (Width.ofCore t)) = expectedOperand role t.bits a := by
  simpa only [width_bits] using operand_inventory a role (Width.ofCore t)

private theorem inst_inventory (i : Inst) :
    (inst i).map Item.inventory = expectedInst i := by
  cases i <;> simp only [inst, expectedInst]
  case bin => simp [Item.inventory, Instruction.inventory, TypeCode.widthFields, widthField, width_bits, core_operand_inventory]
  case binOvf =>
    simp [Item.inventory, Instruction.inventory, TypeCode.widthFields, Global.widthFields, widthField, pairFields, List.zipIdx_cons, width_bits, core_operand_inventory, List.append_assoc]
  case trunc fr target d a =>
    split <;> cases a <;> simp [Item.inventory, Instruction.inventory, operandAt, expectedOperand,
      width_bits, widthField, TypeCode.widthFields, Argument.inventory, TypeCode.integerWidth, Immediate.natural]

private theorem term_inventory (j : Nat) (t : Term) :
    (term j t).map Item.inventory = expectedTerm t := by
  cases t
  case ret a => cases a <;> rfl
  all_goals rfl

private theorem phiSources_scan (j k n : Nat) (blocks : List Block) :
    (((Runtime.indexedFrom n blocks).flatMap fun (pred, b) =>
      (match b.term with
        | .br target args => [(target, pred, args)]
        | .condbr _ yes yesArgs no noArgs => [(yes, pred, yesArgs), (no, pred, noArgs)]
        | _ => [] : List (Nat × Nat × List Operand))).filter (·.1 == j)).map
          (fun (_, pred, args) => (args.getD k (.imm .u8 0), pred)) =
      (blocks.zipIdx n).flatMap (fun (b, pred) => expectedPhiSource j k pred b) := by
  induction blocks generalizing n with
  | nil => rfl
  | cons b blocks ih =>
    simp only [Runtime.indexedFrom, List.zipIdx_cons, List.flatMap_cons, List.filter_append,
      List.map_append, ih]
    congr 1
    cases h : b.term with
    | br target args =>
      by_cases eq : (target == j) = true <;> simp [expectedPhiSource, h, eq]
    | condbr _ yes _ no _ =>
      by_cases hy : (yes == j) = true <;> by_cases hn : (no == j) = true <;>
        simp [expectedPhiSource, h, hy, hn]
    | ret | trap => simp [expectedPhiSource, h]

private theorem phi_inventory (p : Program) (j : Nat) (b : Block) :
    (phis p j b).map Item.inventory =
      (indexed b.params).map (fun (k, _, t) => expectedPhi p j k t) := by
  simp only [phis, List.map_map]
  apply congrArg (fun f => (indexed b.params).map f)
  funext param
  rcases param with ⟨k, r, t⟩
  have sources := phiSources_scan j k 0 p.blocks
  change (((edges p).filter (·.1 == j)).map _) = _ at sources
  have arms :
      (((edges p).filter (·.1 == j)).map fun (_, pred, args) =>
        (operandAt ((ty t).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block pred)) =
      ((p.blocks.zipIdx.flatMap fun (b, pred) => expectedPhiSource j k pred b).map
        fun (a, pred) => (operandAt ((ty t).integerWidth.getD 1) a, Label.block pred)) := by
    rw [← sources, List.map_map]
    rfl
  simp only [Function.comp_apply, Item.inventory, Instruction.inventory, arms, expectedPhi, List.zipIdx_map,
    List.flatMap_map]
  have width : (ty t).widthFields 0 =
      [widthField 0 (match t with | .int t => (toCore t).bits | .bool => 1)] := by
    cases t <;> simp [ty, TypeCode.widthFields, widthField, width_bits]
  rw [width]
  congr 1
  · cases t <;> rfl
  apply congrArg (fun f => ((p.blocks.zipIdx.flatMap fun (b, pred) =>
    expectedPhiSource j k pred b).zipIdx).flatMap f)
  funext entry
  rcases entry with ⟨⟨a, pred⟩, index⟩
  cases t
  case bool => cases a <;> rfl
  case int => simp [ty, TypeCode.integerWidth, width_bits, core_operand_inventory]

private theorem indexed_eq_zipIdx {α : Type} (xs : List α) (n : Nat) :
    Runtime.indexedFrom n xs = (xs.zipIdx n).map Prod.swap := by
  induction xs generalizing n with
  | nil => rfl
  | cons x xs ih => simp [Runtime.indexedFrom, ih, List.zipIdx_cons]

/-- Producer-side inventory law for the actual module items, before rendering. -/
theorem moduleItems_inventory (p : Program) :
    (moduleItems p).map Item.inventory = expectedInventory p := by
  simp only [moduleItems, body, expectedInventory, List.map_append, List.map_cons,
    List.map_nil, Item.inventory, Runtime.llvmItems_inventory, declarations, List.map_map, Function.comp_def]
  have blocks : ((indexed p.blocks).flatMap fun (j, b) =>
      [.label (.block j)] ++ phis p j b ++ b.insts.flatMap inst ++ term j b.term).map
      Item.inventory = p.blocks.zipIdx.flatMap (fun (b, j) =>
        [[]] ++ (b.params.zipIdx.map fun ((_, t), k) => expectedPhi p j k t) ++
          b.insts.flatMap expectedInst ++ expectedTerm b.term) := by
    simp only [List.map_flatMap, Runtime.indexed, indexed_eq_zipIdx, List.flatMap_map]
    apply congrArg (fun f => p.blocks.zipIdx.flatMap f)
    funext entry
    rcases entry with ⟨b, j⟩
    simp [Item.inventory, phi_inventory, term_inventory, List.map_flatMap, inst_inventory,
      Runtime.indexed, indexed_eq_zipIdx, List.map_map, Function.comp_def]
  rw [blocks]
  simp [TypeCode.widthFields, Global.widthFields, widthField, pairFields, width_bits, usedIntrinsics,
    List.filterMap_flatMap, List.zipIdx_cons, List.append_assoc, show Width.bits .u32 = 32 from rfl]

/-- Law. The exact LLVM string written by the normal BIR emitter carries the
integer inventory prescribed by the BIR and runtime contracts. -/
theorem module_inventory (p : Program) :
    Assembly.inventory (module p) = some (expectedInventory p) := by
  rw [module, Assembly.inventory_render, moduleItems_inventory]

namespace PatternEvidence

/-- Plant the payload mutation in the producer, before constructing the LLVM field. -/
def addOneOperand : Operand → Argument
  | .reg r => operand (.reg r)
  | .imm t v => operand (.imm t (v + 1))

/-- Planted (law `operand_field`, case `unsigned_seventeen`, subject `operand`, mutant `addOneOperand`). -/
theorem add_one_rejected :
    (addOneOperand (.imm .u8 17)).immediateValue ≠ some (toBits (toCore .u8) 17) := by decide

/-- Instance (law `operand_token`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_token :
    ∃ token, (operand (.imm .u8 17)).immediateToken = some token ∧ DecimalToken token 17 :=
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
  | t, v => ((operand (.imm t v)).immediateToken.getD "")

/-- Instance (law `operand_parse`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_parses :
    (operand (.imm .u8 17)).immediateToken.bind parseDecimal = some (Int.ofNat (pattern .u8 17)) :=
  operand_parse .u8 17

/-- Instance (law `operand_roundTrip`, case `unsigned_seventeen`). -/
theorem unsigned_seventeen_roundTrips :
    ((operand (.imm .u8 17)).immediateToken.bind parseDecimal).bind (decodePattern .u8) = some 17 :=
  operand_roundTrip .u8 17 (by decide)

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `oversized`). -/
theorem oversized_parse_rejected :
    parseDecimal (oversized .u8 17) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `oversized`). -/
theorem oversized_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (oversized .u8 17)).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Emit a digit separator for one valid immediate. -/
def underscored : IntTy → Int → String
  | .u8, 17 => "1_7"
  | t, v => ((operand (.imm t v)).immediateToken.getD "")

/-- Planted (law `operand_parse`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `underscored`). -/
theorem underscored_parse_rejected :
    parseDecimal (underscored .u8 17) ≠ some (Int.ofNat (pattern .u8 17)) := by decide

/-- Planted (law `operand_roundTrip`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `underscored`). -/
theorem underscored_roundTrip_rejected :
    (toCore .u8).InRange 17 ∧
      (parseDecimal (underscored .u8 17)).bind (decodePattern .u8) ≠ some 17 := by decide

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `underscored`). -/
theorem underscored_token_rejected : ¬ DecimalToken (underscored .u8 17) 17 :=
  fun h => underscored_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Planted (law `operand_token`, case `unsigned_seventeen`, subject `Argument.immediateToken`, mutant `oversized`). -/
theorem oversized_token_rejected : ¬ DecimalToken (oversized .u8 17) 17 :=
  fun h => oversized_parse_rejected ((parseDecimal_exact _ _).2 h)

/-- Instance (law `operand_roundTrip`, case `signed_zero`). -/
theorem signed_zero_roundTrips :
    ((operand (.imm .i8 0)).immediateToken.bind parseDecimal).bind (decodePattern .i8) = some 0 :=
  operand_roundTrip .i8 0 (by decide)

/-- Planted (law `operand_roundTrip`, case `signed_zero`, subject `Argument.immediateToken`, mutant `oversized`). -/
theorem oversized_signed_zero_rejected :
    (parseDecimal (oversized .i8 0)).bind (decodePattern .i8) = none := by decide

end PatternEvidence

end Concrete.Backend.LLVM
