import Concrete.Interfaces.Pipeline
import Concrete.Backend.LCBJob
import Concrete.Backend.LCBWordLemmas
import Concrete.Backend.Runtime
import LeanCompilerBackend.Verified.Job
import LeanCompilerBackend.AMD64
import LeanCompilerBackend.AArch64

/-!
# Production pure LCB emitter and in-process assembler

Lowers any well-formed `Concrete.Interfaces.BIR` program targeting an LCB `Profile`
into a `Concrete.Interfaces.LCBInput`, and assembles verified `Source.Function` bodies
in process across `aarch64-macos`, `x86_64-linux`, and `aarch64-linux`.

* **Zero provisional BIR imports**: depends only on frozen `Concrete.Interfaces.Pipeline`,
  `Concrete.Backend.LCBJob`, `Concrete.Backend.LCBWordLemmas`, `Concrete.Backend.Runtime`,
  and `LeanCompilerBackend`.
* **Exact frozen refusal precedence** matching `Concrete.Interfaces.emitGuarded`:
  1. wrong backend (`profile.backend != .lcb` → `.unsupportedProfile`);
  2. hosted trap-word capacity (`LCBInput.trapCapacity < bir.sites.size` → `.resource`);
  3. single-job linking boundary (`bir.requiresLCBLinking` → `.lcbLinkingUnavailable`);
  4. frozen backend feature policy (`firstUnsupported` → `.unsupportedFeature`);
  5. emission (`emitUnvalidated`) and keyed `validateJob` validation (`emit` → `.invalidLCBJob`).
* **Canonical LCB source lowering**:
  - prunes unreachable blocks via `BIR.reachableBlocks` (admitting unreachable calls);
  - inserts a synthetic entry trampoline at block `0` whenever `graph.entry` has reachable
    predecessors (so block `0` is never a jump target and has empty `phis`);
  - splits same-target parameterized branches into fresh 0-instruction split blocks so `phi`
    incoming lists have distinct, canonically ordered predecessor block indices;
  - expands `.read` via `wordIns (.copy _)`, `.arithmetic (.plain | .wrapping) (.add | .sub | .mul)`
    via `LCBWordLemmas.wrapExpansion`, and `.addWithOverflow` via
    `LCBWordLemmas.checkedOpExpansion .add`;
  - builds validated, uniquely named `additionalJobs` for all non-entry functions in `bir.functions`
    (guaranteed unreachable from `bir.entry` when `bir.requiresLCBLinking = false`).

No theorem uses `sorry`; any `Classical.choice` dependency comes only from the pinned LCB
`validateJob`/`compileFunction` definitions.
-/

namespace Concrete.Backend.EmitLCB

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Interfaces
open Concrete.Backend.LCBWordLemmas
open Concrete.Backend.Runtime (indexed)
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

/-- Abbreviation for a BIR CFG block. -/
abbrev BIRBlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

/-- Abbreviation for a BIR CFG edge carrying typed SSA reads. -/
abbrev BIREdge {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFGEdge (Sigma (SSARead Ty lang.literal))

/-- Rule identifier cited when an off-domain (non-`BIR.WF`) input fails structural lowering. -/
def invalidInputRule : RuleId := ⟨``BIR.WF, 1⟩

/-- Rule identifier cited when `emit`/`emitUnvalidated` is invoked directly on a non-LCB profile. -/
def wrongBackendRule : RuleId := ⟨``BackendPolicy.Backend.lcb, 1⟩

/-! ## Diagnostics and runtime input -/

/-- Resolve every trap entry in `bir.sites` to its canonical newline-terminated
diagnostic line in `bir.project`. -/
def siteDiagnostics {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Option (List String) :=
  bir.sites.toList.mapM fun entry => entry.diagnosticLine bir.project

/-- Build the UTF-8 QBE runtime input from ordered diagnostic lines. -/
def runtimeBytes (diagnostics : List String) : ByteArray :=
  (Backend.Runtime.qbeDiagnostics diagnostics).toUTF8

/-! ## Pure decimal helper symbols and validated unreachable-helper jobs -/

private def natDigitsRev : Nat → List Char
  | 0 => []
  | n + 1 => Nat.digitChar ((n + 1) % 10) :: natDigitsRev ((n + 1) / 10)

private def parseBytesRev : List UInt8 → Nat
  | [] => 0
  | b :: bs => (b.toNat - 48) + 10 * parseBytesRev bs

/-- Canonical decimal string for a natural number, with a constructive left inverse. -/
def helperDecimal (n : Nat) : String :=
  match n with
  | 0 => "0"
  | n + 1 => String.ofList (natDigitsRev (n + 1)).reverse

/-- Canonical unique symbol name for the `idx`-th non-entry function job. -/
def helperSymbolName (idx : Nat) : String :=
  "concrete_fn_" ++ helperDecimal idx

/-- Canonical symbol for the `idx`-th non-entry function job. -/
def helperSymbol (idx : Nat) : Symbol :=
  { namespaceName := "concrete", name := helperSymbolName idx, exported := false }

/-- Minimal valid LCB `Source.Function` body for an unreachable non-entry helper job. -/
def unreachableHelperBody : Source.Function :=
  { resultWidth := .word
    parameters := #[]
    valueWidths := #[]
    blocks := #[{ phis := #[], instructions := #[], terminator := .ret (.literal (exitWord 0)) }] }

/-- Validated LCB `FunctionJob` for the `idx`-th unreachable non-entry function. -/
def helperJob (idx : Nat) : FunctionJob :=
  { code := { symbol := helperSymbol idx
              body := unreachableHelperBody
              abi := planAbi unreachableHelperBody
              layout := planFrame unreachableHelperBody } }

/-- Build keyed `additionalJobs` for every non-entry function in `bir.functions`. -/
def buildAdditionalJobs {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : AList FnId FunctionJob :=
  let helpers := bir.functions.filter fun entry => entry.1 != bir.entry
  (indexed helpers).map fun (idx, (fn, _, _)) => (fn, helperJob idx)

/-! ## Scalar operations and literals -/

/-- Map a supported Core `MachineOp` (plain or wrapping add/sub/mul) to its `WrapOp`. -/
def wrapOpOfMachineOp : MachineOp → Option WrapOp
  | .plain .add | .wrapping .add => some .add
  | .plain .sub | .wrapping .sub => some .sub
  | .plain .mul | .wrapping .mul => some .mul
  | _ => none

/-- Decode a Core literal in `language` into its 32-bit LCB `Source.Value`. -/
def decodeLiteralWord (language : CoreLanguage) {ty : language.operands.Ty}
    (lit : Literal language.operands.Ty ty) : Option Source.Value :=
  (language.literal ty lit.encoding).map fun v =>
    match language.valueShape ▸ v with
    | .scalar (.int _ n) => word n
    | .scalar (.bool b) => boolWord b
    | .scalar (.char c) => word (Int.ofNat c.val.toNat)
    | .scalar .unit | .other _ => word 0

/-! ## Reachable-block normalization for LCB's canonical source profile -/

/-- Normalized block terminator using dense `0 .. blocks.length - 1` block indices. -/
inductive NormTerm {Ty : Type} (lang : LoweringLanguage Ty) where
  | ret (value : Option (Sigma (SSARead Ty lang.literal)))
  | trap (slot : TrapIndex)
  | jump (target : Nat) (args : List (Sigma (SSARead Ty lang.literal)))
  | jumpEntryParams (target : Nat) (paramCount : Nat)
  | branch (test : SSARead Ty lang.literal lang.bool)
      (yes : Nat) (yesArgs : List (Sigma (SSARead Ty lang.literal)))
      (no : Nat) (noArgs : List (Sigma (SSARead Ty lang.literal)))

/-- One block of the normalized LCB lowering graph. -/
structure NormBlock {Ty : Type} (lang : LoweringLanguage Ty) where
  sourceId : Option BlockId
  params : List (Sigma (SSAValue Ty))
  body : List (LocatedOp (SSAInst lang))
  term : NormTerm lang

/-- Normalized, reachable-only CFG with entry trampoline (when needed) and split edges. -/
structure NormGraph {Ty : Type} (lang : LoweringLanguage Ty) where
  paramCount : Nat
  entryHasPreds : Bool
  blocks : List (NormBlock lang)

/-- Ordered list of blocks reachable from `graph.entry`, placing the entry block first
and preserving stored order for all other reachable blocks. -/
def orderedReachableBlocks {Ty : Type} {lang : LoweringLanguage Ty} (graph : BIRGraph lang) :
    Option (List (BIRBlock lang)) :=
  match graph.blocks.toList.find? (·.id == graph.entry) with
  | none => none
  | some entryBlk =>
    let rIds := BIR.reachableBlocks graph
    let rest := graph.blocks.toList.filter fun blk =>
      blk.id != graph.entry && rIds.contains blk.id
    some (entryBlk :: rest)

/-- Whether any reachable block has an outgoing CFG edge targeting `targetId`. -/
def hasIncomingEdge {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (targetId : BlockId) : Bool :=
  blocks.any fun blk => blk.ending.successors.any fun edge => edge.target == targetId

/-- Look up whether `targetId` in `blocks` declares at least one block parameter. -/
def targetHasParams {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (targetId : BlockId) : Bool :=
  match blocks.find? (·.id == targetId) with
  | some tgt => !tgt.params.isEmpty
  | none => false

/-- Dense index map for real reachable blocks, shifted by `offset` (`1` when an entry
trampoline sits at block `0`, `0` otherwise). -/
def blockIndexTable {Ty : Type} {lang : LoweringLanguage Ty} (offset : Nat)
    (blocks : List (BIRBlock lang)) : AList BlockId Nat :=
  (indexed blocks).map fun (idx, blk) => (blk.id, offset + idx)

/-- Synthetic edge-split block created for same-target parameterized branches. -/
structure SplitEdge {Ty : Type} (lang : LoweringLanguage Ty) where
  predIndex : Nat
  splitIndex : Nat
  targetId : BlockId
  targetIndex : Nat
  arguments : List (Sigma (SSARead Ty lang.literal))

/-- Collect all synthetic split blocks needed for same-target parameterized branches. -/
def collectSplitEdges {Ty : Type} {lang : LoweringLanguage Ty} (offset : Nat)
    (blocks : List (BIRBlock lang)) (indices : AList BlockId Nat) :
    List (SplitEdge lang) :=
  let rec go : List (Nat × BIRBlock lang) → Nat → List (SplitEdge lang) → List (SplitEdge lang)
    | [], _, revAcc => revAcc.reverse
    | (j, blk) :: rest, nextIdx, revAcc =>
      match blk.ending with
      | .branch _ yes no =>
        if yes.target == no.target && targetHasParams blocks yes.target then
          match AList.lookup indices yes.target with
          | some tgtIdx =>
            go rest (nextIdx + 1) (⟨offset + j, nextIdx, yes.target, tgtIdx, no.arguments⟩ :: revAcc)
          | none => go rest nextIdx revAcc
        else
          go rest nextIdx revAcc
      | _ => go rest nextIdx revAcc
  go (indexed blocks) (offset + blocks.length) []

/-- Normalize one `BIRTerm` to a dense-indexed `NormTerm`. -/
def normalizeTerm {Ty : Type} {lang : LoweringLanguage Ty}
    (indices : AList BlockId Nat) (splits : List (SplitEdge lang)) (predIdx : Nat)
    (ending : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) :
    Except StageFailure (NormTerm lang) :=
  match ending with
  | .ret v => .ok (.ret v)
  | .trap slot => .ok (.trap slot)
  | .jump edge =>
    match AList.lookup indices edge.target with
    | some tgtIdx => .ok (.jump tgtIdx edge.arguments)
    | none => .error (.invalidInput invalidInputRule)
  | .branch test yes no =>
    match AList.lookup indices yes.target, AList.lookup indices no.target with
    | some yesIdx, some rawNoIdx =>
      match splits.find? (·.predIndex == predIdx) with
      | some sp => .ok (.branch test yesIdx yes.arguments sp.splitIndex [])
      | none => .ok (.branch test yesIdx yes.arguments rawNoIdx no.arguments)
    | _, _ => .error (.invalidInput invalidInputRule)

/-- Normalize one reachable `(j, blk)` into a `NormBlock`. -/
def normalizeBlock {Ty : Type} {lang : LoweringLanguage Ty} (offset : Nat)
    (indices : AList BlockId Nat) (splits : List (SplitEdge lang))
    (pair : Nat × BIRBlock lang) : Except StageFailure (NormBlock lang) := do
  let term ← normalizeTerm indices splits (offset + pair.1) pair.2.ending
  return { sourceId := some pair.2.id, params := pair.2.params, body := pair.2.body, term := term }

/-- Normalize a `BIRGraph` into a `NormGraph` satisfying LCB's CFG structural invariants. -/
def normalizeGraph {Ty : Type} {lang : LoweringLanguage Ty} (graph : BIRGraph lang) :
    Except StageFailure (NormGraph lang) := do
  let some blocks := orderedReachableBlocks graph
    | throw (.invalidInput invalidInputRule)
  let paramCount := match blocks with
    | entryBlk :: _ => entryBlk.params.length
    | [] => 0
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let offset := if entryHasPreds then 1 else 0
  let indices := blockIndexTable offset blocks
  let splits := collectSplitEdges offset blocks indices
  let realBlocks ← (indexed blocks).mapM (normalizeBlock offset indices splits)
  let splitBlocks : List (NormBlock lang) := splits.map fun sp =>
    { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments }
  let trampoline : List (NormBlock lang) :=
    if entryHasPreds then
      [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 paramCount }]
    else []
  return { paramCount, entryHasPreds, blocks := trampoline ++ realBlocks ++ splitBlocks }

/-! ## Canonical LCB value numbering -/

/-- Number of LCB `Source.Instruction`s emitted for one `SSAInst`. -/
def instWidth {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → Nat
  | @SSAInst.op _ _ .read .. => 1
  | @SSAInst.op _ _ (.arithmetic _) .. => 4
  | .addWithOverflow .. => 6
  | _ => 0

/-- Register definitions `(regId, offset)` introduced by one `SSAInst`. -/
def instDefs {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → List (Nat × Nat)
  | @SSAInst.op _ _ .read _ _ dst .. => [(dst.id, 0)]
  | @SSAInst.op _ _ (.arithmetic _) _ _ dst .. => [(dst.id, 3)]
  | .addWithOverflow _ dst flag .. => [(dst.id, 3), (flag.id, 5)]
  | _ => []

/-- Intermediate (unnamed) value offsets introduced by one `SSAInst`. -/
def instInterOffsets {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → List Nat
  | @SSAInst.op _ _ .read .. => []
  | @SSAInst.op _ _ (.arithmetic _) .. => [0, 1, 2]
  | .addWithOverflow .. => [0, 1, 2, 4]
  | _ => []

/-- Number of `Source.Phi` nodes emitted in block `j` (block `0` never has phis). -/
def blockPhiCount {Ty : Type} {lang : LoweringLanguage Ty} (j : Nat) (blk : NormBlock lang) : Nat :=
  if j == 0 then 0 else blk.params.length

/-- Number of LCB values defined by block `j`. -/
def blockSize {Ty : Type} {lang : LoweringLanguage Ty} (j : Nat) (blk : NormBlock lang) : Nat :=
  blockPhiCount j blk + (blk.body.map fun op => instWidth op.instruction).sum

/-- First LCB value id defined in block `j`. -/
def blockBase {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) (j : Nat) : Nat :=
  norm.paramCount + (((indexed norm.blocks).take j).map fun (idx, blk) => blockSize idx blk).sum

/-- Offset of instruction `i`'s first value within block `j`. -/
def instBase {Ty : Type} {lang : LoweringLanguage Ty} (j : Nat) (blk : NormBlock lang) (i : Nat) :
    Nat :=
  blockPhiCount j blk + ((blk.body.take i).map fun op => instWidth op.instruction).sum

/-- Total number of LCB values in `norm` (parameters plus all block phis and instructions). -/
def valueCount {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) : Nat :=
  norm.paramCount + ((indexed norm.blocks).map fun (idx, blk) => blockSize idx blk).sum

/-- Association list mapping every BIR register id defined in `norm` to its LCB `Source.ValueId`. -/
def regIds {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) : AList Nat Nat :=
  (indexed norm.blocks).flatMap fun (j, blk) =>
    let paramIds := (indexed blk.params).map fun (k, ⟨_, reg⟩) =>
      (reg.id, if j == 0 then k else blockBase norm j + k)
    let bodyIds := (indexed blk.body).flatMap fun (i, op) =>
      (instDefs op.instruction).map fun (r, off) => (r, blockBase norm j + instBase j blk i + off)
    paramIds ++ bodyIds

/-- All intermediate (non-register) LCB value ids in `norm`. -/
def intermediates {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) : List Nat :=
  (indexed norm.blocks).flatMap fun (j, blk) =>
    (indexed blk.body).flatMap fun (i, op) =>
      (instInterOffsets op.instruction).map fun off => blockBase norm j + instBase j blk i + off

/-- Look up the LCB `Source.ValueId` assigned to BIR register `r`. -/
def regId {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) (r : Nat) :
    Option Source.ValueId :=
  AList.lookup (regIds norm) r

/-- Executable numbering invariant check for simulation/validation consumers. -/
def numberingOk {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang) : Bool :=
  let ids := regIds norm
  let slots := List.map Prod.snd ids
  decide (List.map Prod.fst ids).Nodup && decide slots.Nodup &&
    slots.all (· < valueCount norm) && (intermediates norm).all (· < valueCount norm) &&
    (intermediates norm).all fun m => !slots.contains m

/-! ## Operand, phi, instruction, terminator, and function lowering -/

/-- Lower a typed `SSARead` to an LCB `Source.Operand`. -/
def emitRead (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (norm : NormGraph lang) {ty : language.operands.Ty} :
    SSARead language.operands.Ty lang.literal ty → Except StageFailure Source.Operand
  | .value v =>
    match regId norm v.id with
    | some id => .ok (.local id)
    | none => .error (.invalidInput invalidInputRule)
  | .literal lit =>
    match decodeLiteralWord language lit with
    | some val => .ok (.literal val)
    | none => .error (.invalidInput invalidInputRule)

/-- Incoming phi operand choice from a single predecessor edge into target block `targetIdx`. -/
inductive PhiEdgeArg {Ty : Type} (lang : LoweringLanguage Ty) where
  | reads (args : List (Sigma (SSARead Ty lang.literal)))
  | entryParam (count : Nat)

/-- Incoming edges targeting `targetIdx` from block `predIdx`. -/
def edgesTo {Ty : Type} {lang : LoweringLanguage Ty} (targetIdx predIdx : Nat) :
    NormTerm lang → List (Nat × PhiEdgeArg lang)
  | .jump t args => if t == targetIdx then [(predIdx, .reads args)] else []
  | .jumpEntryParams t count => if t == targetIdx then [(predIdx, .entryParam count)] else []
  | .branch _ y yArgs n nArgs =>
    (if y == targetIdx then [(predIdx, .reads yArgs)] else []) ++
    (if n == targetIdx then [(predIdx, .reads nArgs)] else [])
  | .ret _ | .trap _ => []

/-- All incoming edges into block `targetIdx`, in strictly increasing predecessor index order. -/
def incomingEdges {Ty : Type} {lang : LoweringLanguage Ty} (norm : NormGraph lang)
    (targetIdx : Nat) : List (Nat × PhiEdgeArg lang) :=
  (indexed norm.blocks).flatMap fun (predIdx, blk) => edgesTo targetIdx predIdx blk.term

/-- Emit the `k`-th `Source.Phi` of block `j` (`j > 0`). -/
def emitPhi (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (norm : NormGraph lang) (j k : Nat) : Except StageFailure Source.Phi := do
  let arms ← (incomingEdges norm j).mapM fun (predIdx, edgeArg) => do
    let opnd ← match edgeArg with
      | .entryParam count =>
        if k < count then .ok (.local k) else .error (.invalidInput invalidInputRule)
      | .reads args =>
        match args[k]? with
        | some ⟨_, read⟩ => emitRead language norm read
        | none => .error (.invalidInput invalidInputRule)
    return (predIdx, opnd)
  return { destination := blockBase norm j + k, width := .word, incoming := arms.toArray }

/-- Emit the LCB `Source.Instruction` expansion of one `SSAInst` starting at value id `base`. -/
def emitInst (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (norm : NormGraph lang) (base : Nat) (inst : SSAInst lang) :
    Except StageFailure (List Source.Instruction) :=
  match inst with
  | @SSAInst.op _ _ opcode _ ty _ _ args addresses =>
    if !addresses.isEmpty then
      .error (.invalidInput invalidInputRule)
    else match opcode with
      | .read =>
        match args with
        | .cons arg rest =>
          match rest with
          | .nil => do
            let arg' ← emitRead language norm arg
            return [wordIns base (.copy arg')]
          | .cons _ _ => .error (.invalidInput invalidInputRule)
        | .nil => .error (.invalidInput invalidInputRule)
      | .arithmetic mOp =>
        match wrapOpOfMachineOp mOp with
        | none => .error (.invalidInput invalidInputRule)
        | some wOp =>
          match language.scalarType ty with
          | some (.int t) =>
            if !inWordProfile t then
              .error (.invalidInput invalidInputRule)
            else match args with
              | .cons lhs r1 =>
                match r1 with
                | .cons rhs r2 =>
                  match r2 with
                  | .nil => do
                    let lhs' ← emitRead language norm lhs
                    let rhs' ← emitRead language norm rhs
                    return wrapExpansion wOp t base lhs' rhs'
                  | .cons _ _ => .error (.invalidInput invalidInputRule)
                | .nil => .error (.invalidInput invalidInputRule)
              | .nil => .error (.invalidInput invalidInputRule)
          | _ => .error (.invalidInput invalidInputRule)
      | .named _ => .error (.invalidInput invalidInputRule)
  | .addWithOverflow t _ _ lhs rhs =>
    if !inWordProfile t then
      .error (.invalidInput invalidInputRule)
    else do
      let lhs' ← emitRead language norm lhs
      let rhs' ← emitRead language norm rhs
      return checkedOpExpansion .add t base lhs' rhs'
  | .load .. | .store .. | .beginLifetime _ | .endLifetime _ | .destructure .. | .call .. =>
    .error (.invalidInput invalidInputRule)

/-- Emit the LCB `Source.Terminator` for a normalized block terminator. -/
def emitTerm (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (norm : NormGraph lang) : NormTerm lang → Except StageFailure Source.Terminator
  | .ret none => .ok (.ret (.literal (exitWord 0)))
  | .ret (some ⟨_, arg⟩) => do
    let arg' ← emitRead language norm arg
    return .ret arg'
  | .trap slot => .ok (.ret (.literal (trapWord slot.index)))
  | .jump tgt _ => .ok (.jump tgt)
  | .jumpEntryParams tgt _ => .ok (.jump tgt)
  | .branch test yes _ no _ => do
    let test' ← emitRead language norm test
    return .branch test' yes no

/-- Emit one LCB `Source.Block` from normalized block `(j, blk)`. -/
def emitBlock (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (norm : NormGraph lang) (pair : Nat × NormBlock lang) :
    Except StageFailure Source.Block := do
  let j := pair.1
  let blk := pair.2
  let phis ← if j == 0 then Except.ok [] else (indexed blk.params).mapM fun p => emitPhi language norm j p.1
  let insts ← (indexed blk.body).mapM fun p =>
    emitInst language norm (blockBase norm j + instBase j blk p.1) p.2.instruction
  let term ← emitTerm language norm blk.term
  return { phis := phis.toArray, instructions := insts.flatten.toArray, terminator := term }

/-- Lower a `BIRGraph` into an LCB `Source.Function` (prior to `validateJob`). -/
def adaptGraph (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (graph : BIRGraph lang) : Except StageFailure Source.Function := do
  let norm ← normalizeGraph graph
  let blocks ← (indexed norm.blocks).mapM (emitBlock language norm)
  return { resultWidth := .word
           parameters := Array.replicate norm.paramCount .word
           valueWidths := Array.replicate (valueCount norm) .word
           blocks := blocks.toArray }

/-! ## Production LCB emitter (`emitUnvalidated`, `validateJobs`, `emit`) -/

/-- Validate the body job and every additional helper job of an `LCBInput`.
Matches `Concrete.Interfaces.LCBInput.validateJobs` exactly. -/
def validateJobs {profile : Profile} (output : LCBInput profile) :
    Except CompileError Unit := do
  validateJob (LCBJob.lcbFunctionJob output.body)
  output.additionalJobs.forM fun pair => validateJob pair.2

/-- Emit an `LCBInput` enforcing backend kind, trap capacity, linking boundary, and
`firstUnsupported`, prior to `validateJobs`. Wired into `PassFunctions.emit` so
`emitGuarded` (and `PassFunctions.emitEntry.refusal` Case 4) can separate `LCBEncoding`
from `validateJobs` failures. -/
def emitUnvalidated {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) :
    Except StageFailure (LCBInput profile) := do
  if profile.backend != .lcb then
    throw (.unsupportedProfile wrongBackendRule)
  if LCBInput.trapCapacity < bir.sites.size then
    throw (.resource (LCBInput.trapCapacityRefusal bir.sites.size))
  if bir.requiresLCBLinking then
    throw .lcbLinkingUnavailable
  match firstUnsupported (language := language) bir with
  | some cause => throw (.unsupportedFeature profile cause)
  | none =>
    let some diagnostics := siteDiagnostics bir
      | throw (.invalidInput invalidInputRule)
    let some (_, graph) := AList.lookup bir.functions bir.entry
      | throw (.invalidInput invalidInputRule)
    let body ← adaptGraph language graph
    return { body := body
             additionalJobs := buildAdditionalJobs bir
             sites := bir.sites
             runtimeInput := runtimeBytes diagnostics }

/-- Full production LCB emitter: runs `emitUnvalidated` and then `validateJobs`, matching
`Concrete.Interfaces.emitGuarded EmitLCB.emitUnvalidated` exactly on `.lcb` profiles. -/
def emit {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) :
    Except StageFailure (LCBInput profile) := do
  let output ← emitUnvalidated (language := language) bir
  (validateJobs output).mapError StageFailure.invalidLCBJob
  return output

/-! ## Pure in-process assembler (zero provisional BIR imports) -/

/-- Typed error returned by the pure in-process LCB assembler. -/
inductive AssembleError where
  | compile (error : CompileError)
  | unsupportedReifyBinary (op : Source.Binary)
  | compatValidate (reason : String)
  | compatEmit (reason : String)
  deriving DecidableEq, Repr

/-- Assemble an LCB `Source.Function` in process for `target`:
* `aarch64Macos` goes through LCB's verified first path (`compileFunction`);
* `x86_64Linux` and `aarch64Linux` validate the job, reify to a single-function module,
  run `validateModule`, and emit through `AMD64`/`AArch64` compatibility backends. -/
def assembleFunction (target : Backend.LCB.Target) (f : Source.Function) :
    Except AssembleError String :=
  match target with
  | .aarch64Macos => do
    let artifact ← (compileFunction (LCBJob.lcbFunctionJob f)).mapError AssembleError.compile
    return ".text\n" ++ artifact.code.fragment ++ ".subsections_via_symbols\n"
  | .x86_64Linux | .aarch64Linux => do
    (validateJob (LCBJob.lcbFunctionJob f)).mapError AssembleError.compile
    let body ← (reify f LCBJob.bodySymbol.name LCBJob.bodySymbol.exported).mapError fun
      | .unsupportedBinary op => .unsupportedReifyBinary op
    let m : LeanCompilerBackend.Module := { functions := #[body] }
    (LeanCompilerBackend.validateModule m).mapError AssembleError.compatValidate
    let emitted := match target with
      | .x86_64Linux => LeanCompilerBackend.AMD64.emitModuleValidated m false false
      | _ => LeanCompilerBackend.AArch64.emitModuleValidated m false false
    emitted.mapError AssembleError.compatEmit

/-- Assemble the body of a produced `LCBInput` in process for its `profile.backendTarget`. -/
def assemble {profile : Profile} (output : LCBInput profile) : Except AssembleError String :=
  assembleFunction profile.backendTarget output.body

/-! ## Constructive structural, refusal, and totality theorems -/

private theorem digitChar_utf8 {d : Nat} (hd : d < 10) :
    [Nat.digitChar d].utf8Encode.data.toList = [UInt8.ofNat (48 + d)] := by
  rcases d with _ | _ | _ | _ | _ | _ | _ | _ | _ | _ | d
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · rfl
  · omega

private theorem digitChar_isAsciiAlnum {d : Nat} (hd : d < 10) :
    (Nat.digitChar d).toNat < 128 ∧ ((Nat.digitChar d).isAlphanum || Nat.digitChar d == '_') = true := by
  rcases d with _ | _ | _ | _ | _ | _ | _ | _ | _ | _ | d
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · exact ⟨by decide, rfl⟩
  · omega

private theorem utf8Encode_append_bytes (l₁ l₂ : List Char) :
    (l₁ ++ l₂).utf8Encode.data.toList = l₁.utf8Encode.data.toList ++ l₂.utf8Encode.data.toList := by
  induction l₁ with
  | nil => rfl
  | cons a rest ih =>
    rw [List.cons_append, List.utf8Encode_cons, ByteArray.data_append, Array.toList_append, ih]
    conv => rhs; rw [List.utf8Encode_cons, ByteArray.data_append, Array.toList_append]
    rw [List.append_assoc]

private theorem parseBytesRev_natDigitsRev :
    ∀ n : Nat, parseBytesRev ((natDigitsRev n).reverse.utf8Encode.data.toList.reverse) = n := by
  intro n
  refine Nat.strongRecOn (motive := fun k =>
      parseBytesRev ((natDigitsRev k).reverse.utf8Encode.data.toList.reverse) = k) n ?_
  intro k ih
  cases k with
  | zero => simp [natDigitsRev, parseBytesRev]
  | succ m =>
    unfold natDigitsRev
    rw [List.reverse_cons, utf8Encode_append_bytes,
      digitChar_utf8 (Nat.mod_lt _ (by decide)), List.reverse_append]
    simp only [List.reverse_cons, List.reverse_nil, List.nil_append, List.cons_append, parseBytesRev]
    have hlt : (m + 1) / 10 < m + 1 := Nat.div_lt_self (Nat.succ_pos m) (by decide)
    rw [ih _ hlt]
    have hModLt : (m + 1) % 10 < 10 := Nat.mod_lt _ (by decide)
    have hByte : (UInt8.ofNat (48 + (m + 1) % 10)).toNat = 48 + (m + 1) % 10 := by
      simp [UInt8.toNat_ofNat]
      omega
    rw [hByte]
    omega

private theorem natDigitsRev_all_valid : ∀ n : Nat, ∀ c ∈ natDigitsRev n,
    c.toNat < 128 ∧ (c.isAlphanum || c == '_') = true := by
  intro n
  refine Nat.strongRecOn (motive := fun k => ∀ c ∈ natDigitsRev k,
      c.toNat < 128 ∧ (c.isAlphanum || c == '_') = true) n ?_
  intro k ih c hc
  cases k with
  | zero => simp [natDigitsRev] at hc
  | succ m =>
    unfold natDigitsRev at hc
    cases hc with
    | head => exact digitChar_isAsciiAlnum (Nat.mod_lt _ (by decide))
    | tail _ htl =>
      exact ih ((m + 1) / 10) (Nat.div_lt_self (Nat.succ_pos m) (by decide)) c htl

private theorem helperDecimal_parseBytes (n : Nat) :
    parseBytesRev ((helperDecimal n).toByteArray.data.toList.reverse) = n := by
  cases n with
  | zero => rfl
  | succ m =>
    unfold helperDecimal
    rw [String.toByteArray_ofList]
    exact parseBytesRev_natDigitsRev (m + 1)

private theorem helperDecimal_all_valid (n : Nat) :
    ∀ c ∈ (helperDecimal n).toList, c.toNat < 128 ∧ (c.isAlphanum || c == '_') = true := by
  cases n with
  | zero =>
    intro c hc
    cases List.mem_singleton.mp hc
    exact ⟨by decide, rfl⟩
  | succ m =>
    intro c hc
    unfold helperDecimal at hc
    rw [String.toList_ofList] at hc
    exact natDigitsRev_all_valid (m + 1) c (List.mem_reverse.mp hc)

theorem helperSymbolName_injective {i j : Nat}
    (h : helperSymbolName i = helperSymbolName j) : i = j := by
  unfold helperSymbolName at h
  have hB := congrArg (fun s => s.toByteArray.data.toList) h
  simp only [String.toByteArray_append, ByteArray.data_append, Array.toList_append] at hB
  have hDec := List.append_cancel_left hB
  have hP := congrArg (fun bs => parseBytesRev bs.reverse) hDec
  rwa [helperDecimal_parseBytes i, helperDecimal_parseBytes j] at hP

theorem helperSymbolName_ne_body (idx : Nat) :
    helperSymbolName idx ≠ LCBJob.bodySymbol.name := by
  intro h
  have hB := congrArg (fun s => s.toByteArray.data.toList) h
  unfold helperSymbolName LCBJob.bodySymbol at hB
  simp only [String.toByteArray_append, ByteArray.data_append, Array.toList_append] at hB
  have h9 := congrArg (fun bs => bs[9]?) hB
  cases h9

private theorem helperSymbolName_valid (idx : Nat) :
    (let name := helperSymbolName idx
     !name.isEmpty && name.toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_'))
       && (name.toList.head!.isAlpha || name.toList.head! == '_')) = true := by
  have hPref : ("concrete_fn_".toList ++ (helperDecimal idx).toList) = (helperSymbolName idx).toList := by
    unfold helperSymbolName
    rw [String.toList_append]
  have hNonEmpty : (helperSymbolName idx).isEmpty = false := by
    cases hE : (helperSymbolName idx).isEmpty
    · rfl
    · have h := String.isEmpty_iff.mp hE
      have hL := congrArg String.toList h
      rw [← hPref] at hL
      cases hL
  have hAll : (helperSymbolName idx).toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_')) = true := by
    rw [← hPref, List.all_append, Bool.and_eq_true]
    refine ⟨by decide, List.all_eq_true.mpr ?_⟩
    intro c hc
    obtain ⟨h1, h2⟩ := helperDecimal_all_valid idx c hc
    exact Bool.and_eq_true_iff.mpr ⟨decide_eq_true h1, h2⟩
  have hHead : (helperSymbolName idx).toList.head! = 'c' := by
    rw [← hPref]
    rfl
  dsimp only
  rw [hNonEmpty, hAll, hHead]
  rfl

/-- Every unreachable-helper job produced by `helperJob` passes `validateJob` unconditionally. -/
theorem helperJob_validateJob (idx : Nat) :
    validateJob (helperJob idx) = .ok () := by
  have hSym := helperSymbolName_valid idx
  unfold validateJob validateJobWithWork helperJob helperSymbol
  dsimp only
  have hSym' : (let name := helperSymbolName idx
      !name.isEmpty && name.toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_')) &&
        (name.toList.head!.isAlpha || name.toList.head! == '_')) = true := hSym
  change ((do
      if ("aarch64-apple-darwin-leaf-int-v1" != "aarch64-apple-darwin-leaf-int-v1") then
        throw (CompileError.unsupportedTarget "aarch64-apple-darwin-leaf-int-v1")
      if (1 != 1 || "ssa-int-v2" != "ssa-int-v2" || "a64-leaf-v2" != "a64-leaf-v2" ||
          "stack-baseline-v1" != "stack-baseline-v1" ||
          "lcb-foundations-dev1" != "lcb-foundations-dev1" ||
          compilerBuildIdentity != compilerBuildIdentity) then
        throw (CompileError.unsupported "job version, semantics or compiler revision")
      if !((!((helperSymbolName idx).isEmpty) &&
            (helperSymbolName idx).toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_')) &&
            ((helperSymbolName idx).toList.head!.isAlpha || (helperSymbolName idx).toList.head! == '_'))) ||
          ("concrete").isEmpty then
        throw (CompileError.invalidJob "invalid symbol identity")
      if !(#[] : Array String).isEmpty || !(#[] : Array String).isEmpty then
        throw (CompileError.unsupported "imports or optimization facts in a leaf job")
      let work ← (validateSourceWithWork unreachableHelperBody).mapError CompileError.admission
      if planAbi unreachableHelperBody != planAbi unreachableHelperBody then
        throw (CompileError.invalidJob "ABI plan does not match source signature/profile")
      if planFrame unreachableHelperBody != planFrame unreachableHelperBody then
        throw (CompileError.invalidJob "frame plan does not match source definitions")
      pure ()
      return work) : Except CompileError Nat).map (fun _ => ()) = .ok ()
  dsimp only at hSym'
  rw [hSym']
  with_unfolding_all rfl

private theorem list_mapM_option_some {α β : Type} {f : α → Option β} {xs : List α}
    (hAll : ∀ x ∈ xs, ∃ y, f x = some y) :
    ∃ ys, xs.mapM f = some ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons a rest ih =>
    obtain ⟨b, hb⟩ := hAll a List.mem_cons_self
    obtain ⟨bs, hbs⟩ := ih fun x hx => hAll x (List.mem_cons_of_mem _ hx)
    refine ⟨b :: bs, ?_⟩
    simp only [List.mapM_cons, bind, Option.bind, pure, hb, hbs]

private theorem list_mapM_except_ok {α β ε : Type} {f : α → Except ε β} {xs : List α}
    (hAll : ∀ x ∈ xs, ∃ y, f x = .ok y) :
    ∃ ys, xs.mapM f = .ok ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons a rest ih =>
    obtain ⟨b, hb⟩ := hAll a List.mem_cons_self
    obtain ⟨bs, hbs⟩ := ih fun x hx => hAll x (List.mem_cons_of_mem _ hx)
    refine ⟨b :: bs, ?_⟩
    simp only [List.mapM_cons, bind, Except.bind, pure, Except.pure, hb, hbs]

private theorem alist_lookup_mem_local {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} {v : α} (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq; subst heq; cases Option.some.inj h; exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_isSome_of_mem {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} {v : α} (h : (k, v) ∈ l) : ∃ v', AList.lookup l k = some v' := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup]
    split
    · exact ⟨v', rfl⟩
    · rename_i hne
      cases h with
      | head => exact absurd rfl hne
      | tail _ htl => exact ih htl

private theorem find?_some_of_mem {α : Type} {xs : List α} {p : α → Bool}
    (hEx : ∃ x ∈ xs, p x = true) :
    ∃ x, xs.find? p = some x ∧ x ∈ xs ∧ p x = true := by
  induction xs with
  | nil => obtain ⟨_, hm, _⟩ := hEx; cases hm
  | cons a rest ih =>
    simp only [List.find?]
    cases hpa : p a
    · obtain ⟨x, hx, hpx⟩ := hEx
      cases hx with
      | head => rw [hpa] at hpx; cases hpx
      | tail _ htl =>
        obtain ⟨y, hy, hymem, hpy⟩ := ih ⟨x, htl, hpx⟩
        exact ⟨y, hy, List.mem_cons_of_mem _ hymem, hpy⟩
    · exact ⟨a, rfl, List.mem_cons_self, hpa⟩

private theorem mem_indexed_left {α : Type} {l : List α} {p : Nat × α}
    (h : p ∈ indexed l) : p.2 ∈ l := by
  have go : ∀ n (xs : List α), p ∈ Backend.Runtime.indexedFrom n xs → p.2 ∈ xs := by
    intro n xs hm
    induction xs generalizing n with
    | nil => cases hm
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.mem_cons] at hm
      rcases hm with rfl | htl
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih (n + 1) htl)
  exact go 0 l h

private theorem mem_indexed_exists {α : Type} {l : List α} {x : α} (h : x ∈ l) :
    ∃ n, (n, x) ∈ indexed l := by
  have go : ∀ k (xs : List α), x ∈ xs → ∃ n, (n, x) ∈ Backend.Runtime.indexedFrom k xs := by
    intro k xs hm
    induction xs generalizing k with
    | nil => cases hm
    | cons a rest ih =>
      cases hm with
      | head => exact ⟨k, List.mem_cons_self⟩
      | tail _ htl =>
        obtain ⟨n, hn⟩ := ih (k + 1) htl
        exact ⟨n, List.mem_cons_of_mem _ hn⟩
  exact go 0 l h

private theorem indexed_map_fst_nodup {α : Type} (l : List α) :
    ((indexed l).map Prod.fst).Nodup := by
  have go : ∀ n (xs : List α), ((Backend.Runtime.indexedFrom n xs).map Prod.fst).Nodup ∧
      ∀ m ∈ (Backend.Runtime.indexedFrom n xs).map Prod.fst, n ≤ m := by
    intro n xs
    induction xs generalizing n with
    | nil => exact ⟨List.nodup_nil, fun _ hm => nomatch hm⟩
    | cons a rest ih =>
      obtain ⟨hnd, hge⟩ := ih (n + 1)
      refine ⟨?_, ?_⟩
      · simp only [Backend.Runtime.indexedFrom, List.map_cons, List.nodup_cons]
        exact ⟨fun hmem => by have := hge n hmem; omega, hnd⟩
      · intro m hm
        simp only [Backend.Runtime.indexedFrom, List.map_cons, List.mem_cons] at hm
        rcases hm with rfl | htl
        · exact Nat.le_refl _
        · have := hge m htl; omega
  exact (go 0 l).1

private theorem indexed_map_snd {α : Type} (l : List α) :
    (indexed l).map Prod.snd = l := by
  have go : ∀ n (xs : List α), (Backend.Runtime.indexedFrom n xs).map Prod.snd = xs := by
    intro n xs
    induction xs generalizing n with
    | nil => rfl
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.map_cons, ih]
  exact go 0 l

private theorem mem_zipIdx_of_mem {α : Type} {xs : List α} {x : α} (h : x ∈ xs) :
    ∃ idx, (x, idx) ∈ xs.zipIdx := by
  have go : ∀ k (ys : List α), x ∈ ys → ∃ idx, (x, idx) ∈ ys.zipIdx k := by
    intro k ys hm
    induction ys generalizing k with
    | nil => cases hm
    | cons a rest ih =>
      cases hm with
      | head => exact ⟨k, List.mem_cons_self⟩
      | tail _ htl =>
        obtain ⟨idx, hidx⟩ := ih (k + 1) htl
        exact ⟨idx, List.mem_cons_of_mem _ hidx⟩
  exact go 0 xs h

private theorem getElem?_some_of_lt {α : Type} {xs : List α} {k : Nat} (h : k < xs.length) :
    ∃ x, xs[k]? = some x ∧ x ∈ xs :=
  ⟨xs[k], List.getElem?_eq_getElem h, List.getElem_mem h⟩

private theorem getElem?_indexed_lt {α : Type} {xs : List α} {k : Nat} {x : α}
    (h : (k, x) ∈ indexed xs) : k < xs.length := by
  have go : ∀ n (ys : List α), (k, x) ∈ Backend.Runtime.indexedFrom n ys →
      n ≤ k ∧ k < n + ys.length := by
    intro n ys hm
    induction ys generalizing n with
    | nil => cases hm
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.mem_cons, Prod.mk.injEq] at hm
      rcases hm with ⟨rfl, rfl⟩ | htl
      · exact ⟨Nat.le_refl _, by simp⟩
      · obtain ⟨hle, hlt⟩ := ih (n + 1) htl
        exact ⟨by omega, by simp; omega⟩
  have h0 := (go 0 xs h).2
  simpa using h0

private theorem list_nodup_map_of_inj {α β : Type} {f : α → β} {xs : List α}
    (hnd : xs.Nodup) (hinj : ∀ a b, f a = f b → a = b) : (xs.map f).Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons a rest ih =>
    rw [List.nodup_cons] at hnd
    rw [List.map_cons, List.nodup_cons]
    refine ⟨?_, ih hnd.2⟩
    intro hmem
    obtain ⟨b, hb, heq⟩ := List.mem_map.mp hmem
    exact hnd.1 (hinj b a heq ▸ hb)

private theorem list_forM_except_ok_iff {α ε : Type} {f : α → Except ε Unit} (xs : List α) :
    xs.forM f = .ok () ↔ ∀ x ∈ xs, f x = .ok () := by
  induction xs with
  | nil =>
    constructor
    · intro _ _ hm; cases hm
    · intro _; rfl
  | cons a rest ih =>
    unfold List.forM
    cases hfa : f a with
    | error e =>
      simp only [bind, Except.bind]
      constructor
      · intro h; cases h
      · intro hall
        have := hall a List.mem_cons_self
        rw [hfa] at this
        cases this
    | ok u =>
      cases u
      simp only [bind, Except.bind]
      rw [ih]
      constructor
      · intro hrest x hx
        cases hx with
        | head => exact hfa
        | tail _ htl => exact hrest x htl
      · intro hall x hx
        exact hall x (List.mem_cons_of_mem _ hx)

/-- Every `BIR` whose trap entries all bind in `bir.project` resolves to exact diagnostic lines. -/
theorem siteDiagnostics_of_sites_bind {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBind : ∀ site ∈ bir.sites.toList, ∃ span, site.origin.lexical.Binds bir.project span) :
    ∃ diagnostics, siteDiagnostics bir = some diagnostics := by
  apply list_mapM_option_some
  intro entry hEntry
  obtain ⟨span, hSpan⟩ := hBind entry hEntry
  obtain ⟨file, _, hSite⟩ := runtimeSite_of_binds hSpan
  exact ⟨renderTrapLine entry.kind ⟨entry.origin.operation, ⟨file.path.render.text, entry.origin.lexical.line, entry.origin.lexical.col⟩⟩,
    by simp [TrapEntry.diagnosticLine, hSite]⟩

/-! ### Refusal theorems and agreement with `emitGuarded` -/

theorem emitUnvalidated_refusal_capacity {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBackend : profile.backend = .lcb)
    (hCap : LCBInput.trapCapacity < bir.sites.size) :
    emitUnvalidated (language := language) bir =
      .error (.resource (LCBInput.trapCapacityRefusal bir.sites.size)) := by
  unfold emitUnvalidated
  simp [hBackend, hCap, throw, throwThe, MonadExceptOf.throw, bind, Except.bind]

theorem emitUnvalidated_refusal_linking {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBackend : profile.backend = .lcb)
    (hCap : ¬ (LCBInput.trapCapacity < bir.sites.size))
    (hLink : bir.requiresLCBLinking = true) :
    emitUnvalidated (language := language) bir = .error .lcbLinkingUnavailable := by
  unfold emitUnvalidated
  simp [hBackend, hCap, hLink, throw, throwThe, MonadExceptOf.throw, bind, Except.bind]

theorem emitUnvalidated_refusal_firstUnsupported {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (hBackend : profile.backend = .lcb)
    (hCap : ¬ (LCBInput.trapCapacity < bir.sites.size))
    (hLink : bir.requiresLCBLinking = false)
    (hSup : firstUnsupported (language := language) bir = some cause) :
    emitUnvalidated (language := language) bir = .error (.unsupportedFeature profile cause) := by
  unfold emitUnvalidated
  simp [hBackend, hCap, hLink, hSup, throw, throwThe, MonadExceptOf.throw]

theorem emit_refusal_capacity {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBackend : profile.backend = .lcb)
    (hCap : LCBInput.trapCapacity < bir.sites.size) :
    emit (language := language) bir =
      .error (.resource (LCBInput.trapCapacityRefusal bir.sites.size)) := by
  unfold emit
  rw [emitUnvalidated_refusal_capacity hBackend hCap]
  rfl

theorem emit_refusal_linking {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBackend : profile.backend = .lcb)
    (hCap : ¬ (LCBInput.trapCapacity < bir.sites.size))
    (hLink : bir.requiresLCBLinking = true) :
    emit (language := language) bir = .error .lcbLinkingUnavailable := by
  unfold emit
  rw [emitUnvalidated_refusal_linking hBackend hCap hLink]
  rfl

theorem emit_refusal_firstUnsupported {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (hBackend : profile.backend = .lcb)
    (hCap : ¬ (LCBInput.trapCapacity < bir.sites.size))
    (hLink : bir.requiresLCBLinking = false)
    (hSup : firstUnsupported (language := language) bir = some cause) :
    emit (language := language) bir = .error (.unsupportedFeature profile cause) := by
  unfold emit
  rw [emitUnvalidated_refusal_firstUnsupported hBackend hCap hLink hSup]
  rfl

theorem emit_refusal_invalidJob {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LCBInput profile} {reason : CompileError}
    (hUnval : emitUnvalidated (language := language) bir = .ok output)
    (hVal : validateJobs output = .error reason) :
    emit (language := language) bir = .error (.invalidLCBJob reason) := by
  unfold emit
  rw [hUnval]
  simp [hVal, Except.mapError, bind, Except.bind]

/-- On every `.lcb` profile, `EmitLCB.emit` agrees identically with
`Concrete.Interfaces.emitGuarded EmitLCB.emitUnvalidated`. -/
theorem emit_eq_emitGuarded {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {target : Backend.LCB.Target}
    {maxSlots maxStack : Nat} (bir : BIR lang ⟨.lcb, target, maxSlots, maxStack⟩) :
    emit (language := language) bir =
      emitGuarded (language := language) (emitUnvalidated (language := language)) bir := by
  unfold emitGuarded
  by_cases hCap : LCBInput.trapCapacity < bir.sites.size
  · simp only [hCap, ↓reduceIte]
    exact emit_refusal_capacity (language := language) rfl hCap
  · simp only [hCap, ↓reduceIte]
    cases hLink : bir.requiresLCBLinking
    · simp only [Bool.false_eq_true, ↓reduceIte]
      cases hSup : firstUnsupported (language := language) bir with
      | some cause =>
        exact emit_refusal_firstUnsupported (language := language) rfl hCap hLink hSup
      | none =>
        rfl
    · simp only [↓reduceIte]
      exact emit_refusal_linking (language := language) rfl hCap hLink

/-- Every output accepted by `EmitLCB.emit` passes `validateJob` on `output.body` and on
every job in `output.additionalJobs`. -/
theorem emit_validated_jobs {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LCBInput profile}
    (h : emit (language := language) bir = .ok output) :
    validateJob (LCBJob.lcbFunctionJob output.body) = .ok () ∧
      ∀ pair ∈ output.additionalJobs, validateJob pair.2 = .ok () := by
  unfold emit at h
  cases hUnval : emitUnvalidated (language := language) bir with
  | error e => rw [hUnval] at h; cases h
  | ok out =>
    rw [hUnval] at h
    cases hVal : validateJobs out with
    | error reason =>
      simp [hVal, Except.mapError, bind, Except.bind] at h
    | ok u =>
      cases u
      simp only [hVal, Except.mapError, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
      subst h
      unfold validateJobs at hVal
      cases hBody : validateJob (LCBJob.lcbFunctionJob out.body) with
      | error e => rw [hBody] at hVal; cases hVal
      | ok u' =>
        cases u'
        rw [hBody] at hVal
        exact ⟨rfl, (list_forM_except_ok_iff out.additionalJobs).mp hVal⟩

/-! ### Structural invariants of `emitUnvalidated` (all non-refinement conjuncts of `LCBEncoding`) -/

private theorem buildAdditionalJobs_keys {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (bir : BIR lang profile) :
    List.map Prod.fst (buildAdditionalJobs bir) =
      (List.map Prod.fst bir.functions).filter (· != bir.entry) := by
  unfold buildAdditionalJobs
  rw [List.map_map]
  have h1 : (indexed (bir.functions.filter fun entry => entry.1 != bir.entry)).map
      (Prod.fst ∘ fun (pair : Nat × (FnId × LocalTypes Ty × BIRGraph lang)) =>
        (pair.2.1, helperJob pair.1)) =
      ((indexed (bir.functions.filter fun entry => entry.1 != bir.entry)).map Prod.snd).map Prod.fst := by
    rw [List.map_map]
    rfl
  rw [h1, indexed_map_snd, List.filter_map]
  rfl

private theorem buildAdditionalJobs_symbols_nodup {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (bir : BIR lang profile) (body : Source.Function) :
    ((LCBJob.lcbFunctionJob body :: List.map Prod.snd (buildAdditionalJobs bir)).map
      (fun job => job.code.symbol.name)).Nodup := by
  rw [List.map_cons, List.nodup_cons]
  have hMap : (List.map Prod.snd (buildAdditionalJobs bir)).map (fun job => job.code.symbol.name) =
      ((indexed (bir.functions.filter fun entry => entry.1 != bir.entry)).map Prod.fst).map
        helperSymbolName := by
    unfold buildAdditionalJobs
    rw [List.map_map, List.map_map, List.map_map]
    rfl
  rw [hMap]
  refine ⟨?_, list_nodup_map_of_inj (indexed_map_fst_nodup _) (fun _ _ h => helperSymbolName_injective h)⟩
  intro hMem
  obtain ⟨idx, _, hEq⟩ := List.mem_map.mp hMem
  exact helperSymbolName_ne_body idx hEq

/-- Every successful `emitUnvalidated` on a `BIR.WF` program satisfies `profile.backend = .lcb`,
`firstUnsupported = none`, and all 8 structural conjuncts of `LCBEncoding` (everything except
`BehaviorRefinement`, which belongs to unit P6b). -/
theorem emitUnvalidated_structural {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LCBInput profile}
    (hWF : BIR.WF (language := language) bir)
    (h : emitUnvalidated (language := language) bir = .ok output) :
    profile.backend = .lcb ∧
    firstUnsupported (language := language) bir = none ∧
    ((LCBJob.lcbFunctionJob output.body :: List.map Prod.snd output.additionalJobs).map
      (fun job => job.code.symbol.name)).Nodup ∧
    output.sites = bir.sites ∧
    output.RuntimeMatches bir.project ∧
    (List.map Prod.fst bir.functions).Nodup ∧
    (bir.entry :: List.map Prod.fst output.additionalJobs).Nodup ∧
    (∀ fn, fn ∈ List.map Prod.fst bir.functions ↔ fn ∈ bir.entry :: List.map Prod.fst output.additionalJobs) ∧
    bir.requiresLCBLinking = false ∧
    bir.TrapSlotsWF := by
  unfold emitUnvalidated at h
  cases hBack : (profile.backend != .lcb)
  · have hBackendEq : profile.backend = .lcb := by
      cases hb : profile.backend
      · rw [hb] at hBack; cases hBack
      · rfl
    simp only [hBack, Bool.false_eq_true, ↓reduceIte] at h
    by_cases hCap : LCBInput.trapCapacity < bir.sites.size
    · simp only [hCap, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
      cases h
    · simp only [hCap, ↓reduceIte] at h
      cases hLink : bir.requiresLCBLinking
      · simp only [hLink, Bool.false_eq_true, ↓reduceIte] at h
        cases hSup : firstUnsupported (language := language) bir with
        | some cause =>
          simp only [hSup, throw, throwThe, MonadExceptOf.throw] at h
          cases h
        | none =>
          simp only [hSup] at h
          cases hDiag : siteDiagnostics bir with
          | none =>
            simp only [hDiag, throw, throwThe, MonadExceptOf.throw] at h
            cases h
          | some diagnostics =>
            simp only [hDiag] at h
            cases hFn : AList.lookup bir.functions bir.entry with
            | none =>
              simp only [hFn, throw, throwThe, MonadExceptOf.throw] at h
              cases h
            | some fnPair =>
              rcases fnPair with ⟨locals, graph⟩
              simp only [hFn] at h
              cases hBody : adaptGraph language graph with
              | error e =>
                simp only [hBody, bind, Except.bind] at h
                cases h
              | ok body =>
                simp only [hBody, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
                subst h
                have hFnsNd : (List.map Prod.fst bir.functions).Nodup :=
                  hWF.registerFormation.functions_nodup
                have hEntryMem : bir.entry ∈ List.map Prod.fst bir.functions :=
                  hWF.registerFormation.entry_mem
                have hAddKeys := buildAdditionalJobs_keys bir
                have hAddNd : (bir.entry :: List.map Prod.fst (buildAdditionalJobs bir)).Nodup := by
                  rw [hAddKeys, List.nodup_cons]
                  refine ⟨?_, hFnsNd.filter _⟩
                  intro hMem
                  have hf := List.mem_filter.mp hMem
                  simp at hf
                have hBiject : ∀ fn, fn ∈ List.map Prod.fst bir.functions ↔
                    fn ∈ bir.entry :: List.map Prod.fst (buildAdditionalJobs bir) := by
                  intro fn
                  rw [hAddKeys, List.mem_cons, List.mem_filter]
                  constructor
                  · intro hIn
                    by_cases hEq : fn = bir.entry
                    · exact Or.inl hEq
                    · exact Or.inr ⟨hIn, bne_iff_ne.mpr hEq⟩
                  · rintro (rfl | ⟨hIn, _⟩)
                    · exact hEntryMem
                    · exact hIn
                exact ⟨hBackendEq, rfl, buildAdditionalJobs_symbols_nodup bir body, rfl,
                  ⟨diagnostics, hDiag, rfl⟩, hFnsNd, hAddNd, hBiject, rfl, hWF.trapSlotsWF⟩
      · simp only [hLink, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
        cases h
  · simp only [hBack, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
    cases h

/-! ### Constructive dominance-implies-reachability and totality on `BIR.WF` -/

/-- If `target` is reachable from `entry` and `dom` dominates `target` (no path from `entry`
to `target` avoids `dom`), then `dom` itself is reachable from `entry`. -/
theorem graphPath_hits_dominator {α : Type} [DecidableEq α]
    {nodes : List α} {edges : List (α × α)} {entry dom target : α}
    (hReach : GraphPath nodes edges none entry target)
    (hDom : ¬ GraphPath nodes edges (some dom) entry target) :
    GraphPath nodes edges none entry dom := by
  induction hReach with
  | refl hMem _ =>
    by_cases hEq : dom = entry
    · subst hEq
      exact .refl hMem (by intro h; cases h)
    · exact absurd (.refl hMem (fun h => hEq (Option.some.inj h))) hDom
  | @snoc b c hPrev hEdge hMem _ ih =>
    by_cases hEq : dom = c
    · subst hEq
      exact .snoc hPrev hEdge hMem (by intro h; cases h)
    · apply ih
      intro hPrevAvoid
      exact hDom (.snoc hPrevAvoid hEdge hMem (fun h => hEq (Option.some.inj h)))

private theorem decodeLiteralWord_some_of_ok {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {ty : language.operands.Ty}
    {lit : Literal language.operands.Ty ty}
    (hOk : readLiteralOk language (lang := lang) ⟨ty, .literal lit⟩ = true) :
    ∃ val, decodeLiteralWord language lit = some val := by
  simp only [readLiteralOk] at hOk
  cases hl : language.literal ty lit.encoding with
  | none => rw [hl] at hOk; cases hOk
  | some v => exact ⟨_, by unfold decodeLiteralWord; rw [hl]; rfl⟩

private theorem mem_reachableBlocks_entry {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {entryBlk : BIRBlock lang}
    (hMem : entryBlk ∈ graph.blocks.toList) (hId : entryBlk.id = graph.entry) :
    graph.entry ∈ BIR.reachableBlocks graph := by
  unfold BIR.reachableBlocks
  rw [mem_reachGraph_iff]
  exact .refl (List.mem_map.mpr ⟨entryBlk, hMem, hId⟩) (by intro h; cases h)

private theorem mem_reachableBlocks_succ {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blk tgt : BIRBlock lang} {edge : BIREdge lang}
    (hReach : blk.id ∈ BIR.reachableBlocks graph)
    (hBlk : blk ∈ graph.blocks.toList) (hEdge : edge ∈ blk.ending.successors)
    (hTgt : tgt ∈ graph.blocks.toList) (hTgtId : tgt.id = edge.target) :
    edge.target ∈ BIR.reachableBlocks graph := by
  unfold BIR.reachableBlocks at hReach ⊢
  rw [mem_reachGraph_iff] at hReach ⊢
  refine .snoc hReach ?_ (List.mem_map.mpr ⟨tgt, hTgt, hTgtId⟩) (by intro h; cases h)
  exact List.mem_flatMap.mpr ⟨blk, hBlk, List.mem_map.mpr ⟨edge, hEdge, rfl⟩⟩

private theorem mem_reachableFunctions_entry {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {bir : BIR lang profile}
    (hEntry : bir.entry ∈ List.map Prod.fst bir.functions) :
    bir.entry ∈ bir.reachableFunctions := by
  unfold BIR.reachableFunctions
  rw [mem_reachGraph_iff]
  exact .refl hEntry (by intro h; cases h)

private theorem block_eq_of_id_eq {Ty : Type} {lang : LoweringLanguage Ty}
    {xs : List (BIRBlock lang)} (hnd : (xs.map Block.id).Nodup)
    {b₁ b₂ : BIRBlock lang} (h₁ : b₁ ∈ xs) (h₂ : b₂ ∈ xs) (hid : b₁.id = b₂.id) :
    b₁ = b₂ := by
  induction xs with
  | nil => cases h₁
  | cons a rest ih =>
    rw [List.map_cons, List.nodup_cons] at hnd
    cases h₁ with
    | head =>
      cases h₂ with
      | head => rfl
      | tail _ h2t => exact absurd (hid ▸ List.mem_map.mpr ⟨b₂, h2t, rfl⟩) hnd.1
    | tail _ h1t =>
      cases h₂ with
      | head => exact absurd (hid.symm ▸ List.mem_map.mpr ⟨b₁, h1t, rfl⟩) hnd.1
      | tail _ h2t => exact ih hnd.2 h1t h2t

private theorem block_in_orderedReachable {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blocks : List (BIRBlock lang)}
    (hOrd : orderedReachableBlocks graph = some blocks)
    (hNodup : (graph.blocks.toList.map Block.id).Nodup)
    {blk : BIRBlock lang} (hBlk : blk ∈ graph.blocks.toList)
    (hReach : blk.id ∈ BIR.reachableBlocks graph) :
    blk ∈ blocks := by
  unfold orderedReachableBlocks at hOrd
  cases hFind : graph.blocks.toList.find? (·.id == graph.entry) with
  | none => rw [hFind] at hOrd; cases hOrd
  | some entryBlk =>
    rw [hFind] at hOrd
    cases Option.some.inj hOrd
    by_cases hEq : blk.id = graph.entry
    · have hEntryMem : entryBlk ∈ graph.blocks.toList := List.mem_of_find?_eq_some hFind
      have hEntryId : entryBlk.id = graph.entry := eq_of_beq (by simpa using List.find?_some hFind)
      have hBlkEq := block_eq_of_id_eq hNodup hBlk hEntryMem (hEq.trans hEntryId.symm)
      subst hBlkEq
      exact List.mem_cons_self
    · apply List.mem_cons_of_mem
      exact List.mem_filter.mpr ⟨hBlk, by
        simp only [Bool.and_eq_true, bne_iff_ne, ne_eq, List.contains_eq_mem, decide_eq_true_eq]
        exact ⟨hEq, hReach⟩⟩

private theorem orderedReachable_mem_subset {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blocks : List (BIRBlock lang)}
    (hOrd : orderedReachableBlocks graph = some blocks)
    {blk : BIRBlock lang} (hBlk : blk ∈ blocks) :
    blk ∈ graph.blocks.toList ∧ blk.id ∈ BIR.reachableBlocks graph := by
  unfold orderedReachableBlocks at hOrd
  cases hFind : graph.blocks.toList.find? (·.id == graph.entry) with
  | none => rw [hFind] at hOrd; cases hOrd
  | some entryBlk =>
    rw [hFind] at hOrd
    cases Option.some.inj hOrd
    cases hBlk with
    | head =>
      have hMem := List.mem_of_find?_eq_some hFind
      have hId : blk.id = graph.entry := eq_of_beq (by simpa using List.find?_some hFind)
      exact ⟨hMem, hId ▸ mem_reachableBlocks_entry hMem hId⟩
    | tail _ hTail =>
      have hf := List.mem_filter.mp hTail
      simp only [Bool.and_eq_true, bne_iff_ne, List.contains_eq_mem, decide_eq_true_eq] at hf
      exact ⟨hf.1, hf.2.2⟩

/-- Classification of any reachable instruction when `firstUnsupported = none` on an LCB profile:
it is `.read`, `.arithmetic` (plain/wrapping add/sub/mul on an `inWordProfile` integer type),
or `.addWithOverflow` on an `inWordProfile` integer type. -/
private theorem supported_lcb_inst_shape {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {fn : FnId} {locals : LocalTypes language.operands.Ty}
    {graph : BIRGraph lang} {blk : BIRBlock lang} {op : LocatedOp (SSAInst lang)} {idx : Nat}
    (hBackend : profile.backend = .lcb)
    (hFn : (fn, locals, graph) ∈ bir.functions)
    (hEntryReach : fn ∈ bir.reachableFunctions)
    (hBlk : blk ∈ graph.blocks.toList)
    (hBlkReach : blk.id ∈ BIR.reachableBlocks graph)
    (hOp : (op, idx) ∈ blk.body.zipIdx)
    (hLocal : instLocalOk language bir.declarations profile.memoryTarget bir.signatures locals op.instruction = true)
    (hAllSup : ∀ cause ∈ bir.featureCandidates (language := language),
      BackendPolicy.FeatureSupported profile.policyProfile cause.feature) :
    (∀ id ty, (id, ty) ∈ op.instruction.defs → ∃ off, (id, off) ∈ instDefs op.instruction) ∧
    (∀ norm base,
      (∀ r ∈ op.instruction.reads, ∃ opnd, emitRead language norm r.2 = .ok opnd) →
      ∃ insts, emitInst language norm base op.instruction = .ok insts) := by
  have hBodyLocal := (Bool.and_eq_true_iff.mp hLocal).2
  have hER : (bir.reachableFunctions.contains fn && (BIR.reachableBlocks graph).contains blk.id) = true := by
    simp only [Bool.and_eq_true, List.contains_eq_mem, decide_eq_true_eq]
    exact ⟨hEntryReach, hBlkReach⟩
  have hPolicyLcb : profile.policyProfile.backend = .lcb := by
    unfold Profile.policyProfile
    cases hb : profile.backend
    · rw [hb] at hBackend; cases hBackend
    · rfl
  let wCand : language.operands.Ty → List (FeatureCause profile) := fun ty =>
    match language.scalarType ty with
    | some (.int intTy) => [.unsupportedWidth intTy fn (.instruction blk.id idx)]
    | _ => []
  have hInstIn : ∀ c : FeatureCause profile,
      c ∈ ((match op.instruction with
        | @SSAInst.op _ _ opcode types ty _ _ _ addresses =>
          (if addresses.isEmpty then [.unsupportedStorage .registerOnly fn (.instruction blk.id idx)]
           else [.unsupportedStorage .addressOperand fn (.instruction blk.id idx)]) ++
          [.unsupportedOperation (Operator.policyOperation opcode) fn (.instruction blk.id idx)] ++
          (wCand ty ++ types.flatMap wCand ++ addresses.flatMap (fun p => wCand p.1))
        | .addWithOverflow t _ _ _ _ =>
          [.unsupportedOperation .addWithOverflow fn (.instruction blk.id idx),
           .unsupportedWidth t fn (.instruction blk.id idx)]
        | .load (ty := ty) _ _ =>
          .unsupportedStorage .load fn (.instruction blk.id idx) :: wCand ty
        | .store (ty := ty) _ _ =>
          .unsupportedStorage .store fn (.instruction blk.id idx) :: wCand ty
        | .beginLifetime _ => [.unsupportedStorage .beginLifetime fn (.instruction blk.id idx)]
        | .endLifetime _ => [.unsupportedStorage .endLifetime fn (.instruction blk.id idx)]
        | .destructure (ty := ty) src bindings =>
          let form := match src with
            | .register _ => BackendPolicy.StorageForm.destructureRegister
            | .materialized _ => .destructureMaterialized
          .unsupportedStorage form fn (.instruction blk.id idx) ::
            (wCand ty ++ bindings.flatMap (fun (_, ⟨fty, _⟩) => wCand fty))
        | .call callee dst args =>
          let form :=
            if !(bir.reachableFunctions.contains fn && (BIR.reachableBlocks graph).contains blk.id) then
              BackendPolicy.CallForm.unreachableCall
            else if (AList.lookup bir.functions callee).isSome then .reachableInternalCall
            else .foreignCall
          .unsupportedCall form (some callee) fn (.instruction blk.id idx) ::
            (dst.toList.flatMap (fun d => wCand d.1) ++ args.flatMap (fun a => wCand a.1))) :
        List (FeatureCause profile)) →
      BackendPolicy.FeatureSupported profile.policyProfile c.feature := by
    intro c hc
    apply hAllSup
    unfold BIR.featureCandidates
    refine List.mem_flatMap.mpr ⟨(fn, locals, graph), hFn, ?_⟩
    apply List.mem_append_right
    refine List.mem_flatMap.mpr ⟨blk, hBlk, ?_⟩
    apply List.mem_append_left
    apply List.mem_append_right
    exact List.mem_flatMap.mpr ⟨(op, idx), hOp, hc⟩
  have hWidthProf : ∀ {t : IntTy},
      BackendPolicy.FeatureSupported profile.policyProfile (.intWidth t) → inWordProfile t = true := by
    intro t ht
    unfold BackendPolicy.FeatureSupported BackendPolicy.IntWidthSupported
      BackendPolicy.supportsIntWidth at ht
    dsimp only at ht
    rw [hPolicyLcb] at ht
    exact ht
  cases hInst : op.instruction with
  | op dst fop args addresses =>
    rename_i opcode types ty
    rw [hInst] at hBodyLocal hInstIn
    have hSig := (Bool.and_eq_true_iff.mp hBodyLocal).1
    have hOpSup := hInstIn (.unsupportedOperation (Operator.policyOperation opcode) fn (.instruction blk.id idx))
      (by simp)
    have hAddrEmpty : addresses.isEmpty = true := by
      cases hAe : addresses.isEmpty
      · have hBad := hInstIn (.unsupportedStorage .addressOperand fn (.instruction blk.id idx))
          (by simp [hAe])
        cases hBad
      · rfl
    cases opcode with
    | read =>
      refine ⟨?_, ?_⟩
      · intro id _ hid
        cases List.mem_singleton.mp hid
        exact ⟨0, List.mem_cons_self⟩
      · intro norm base hReads
        simp only [opcodeSignatureOk, Bool.and_eq_true] at hSig
        cases types with
        | nil => simp [hAddrEmpty] at hSig
        | cons t0 rest =>
          cases rest with
          | nil =>
            cases args with
            | cons arg restArgs =>
              cases restArgs with
              | nil =>
                obtain ⟨arg', hArg⟩ := hReads ⟨t0, arg⟩ List.mem_cons_self
                unfold emitInst
                simp only [hAddrEmpty, Bool.not_true, Bool.false_eq_true, ↓reduceIte, hArg,
                  bind, Except.bind, pure, Except.pure]
                exact ⟨_, rfl⟩
          | cons _ _ => simp [hAddrEmpty] at hSig
    | arithmetic mOp =>
      have hWrap : ∃ wOp, wrapOpOfMachineOp mOp = some wOp := by
        cases mOp with
        | plain ao =>
          cases ao with
          | add => exact ⟨.add, rfl⟩
          | sub => exact ⟨.sub, rfl⟩
          | mul => exact ⟨.mul, rfl⟩
          | div | rem | shl | shr => cases hOpSup
        | wrapping wo =>
          cases wo with
          | add => exact ⟨.add, rfl⟩
          | sub => exact ⟨.sub, rfl⟩
          | mul => exact ⟨.mul, rfl⟩
        | saturating _ => cases hOpSup
      obtain ⟨wOp, hWOp⟩ := hWrap
      refine ⟨?_, ?_⟩
      · intro id _ hid
        cases List.mem_singleton.mp hid
        exact ⟨3, List.mem_cons_self⟩
      · intro norm base hReads
        simp only [opcodeSignatureOk, Bool.and_eq_true] at hSig
        cases types with
        | nil => simp [hAddrEmpty] at hSig
        | cons t0 rest =>
          cases rest with
          | nil => simp [hAddrEmpty] at hSig
          | cons t1 rest2 =>
            cases rest2 with
            | nil =>
              have hIsInt : isIntType language ty = true := by
                simp [hAddrEmpty] at hSig
                exact hSig.2.1.1
              cases hSc : language.scalarType ty with
              | none => simp [isIntType, hSc] at hIsInt
              | some ct =>
                cases ct with
                | int t =>
                  have htSup : BackendPolicy.FeatureSupported profile.policyProfile (.intWidth t) :=
                    hInstIn (.unsupportedWidth t fn (.instruction blk.id idx)) (by
                      simp only [wCand, hSc, List.mem_append, List.mem_cons, List.not_mem_nil, or_false]
                      exact Or.inr (Or.inl (Or.inl trivial)))
                  have htProf := hWidthProf htSup
                  cases args with
                  | cons lhs r1 =>
                    cases r1 with
                    | cons rhs r2 =>
                      cases r2 with
                      | nil =>
                        obtain ⟨lhs', hLhs⟩ := hReads ⟨t0, lhs⟩ List.mem_cons_self
                        obtain ⟨rhs', hRhs⟩ := hReads ⟨t1, rhs⟩ (List.mem_cons_of_mem _ List.mem_cons_self)
                        unfold emitInst
                        simp only [hAddrEmpty, Bool.not_true, Bool.false_eq_true, ↓reduceIte,
                          hWOp, hSc, htProf, hLhs, hRhs, bind, Except.bind, pure, Except.pure]
                        exact ⟨_, rfl⟩
                | bool | char | unit => simp [isIntType, hSc] at hIsInt
            | cons _ _ => simp [hAddrEmpty] at hSig
    | named _ => cases hOpSup
  | addWithOverflow t dst flag lhs rhs =>
    rw [hInst] at hInstIn
    have htProf := hWidthProf (hInstIn (.unsupportedWidth t fn (.instruction blk.id idx))
      (List.mem_cons_of_mem _ List.mem_cons_self))
    refine ⟨?_, ?_⟩
    · intro id _ hid
      simp only [SSAInst.defs, List.mem_cons, List.not_mem_nil, or_false, Prod.mk.injEq] at hid
      rcases hid with ⟨rfl, _⟩ | ⟨rfl, _⟩
      · exact ⟨3, List.mem_cons_self⟩
      · exact ⟨5, List.mem_cons_of_mem _ List.mem_cons_self⟩
    · intro norm base hReads
      obtain ⟨lhs', hLhs⟩ := hReads ⟨lang.intTy t, lhs⟩ List.mem_cons_self
      obtain ⟨rhs', hRhs⟩ := hReads ⟨lang.intTy t, rhs⟩ (List.mem_cons_of_mem _ List.mem_cons_self)
      unfold emitInst
      simp only [htProf, Bool.not_true, Bool.false_eq_true, ↓reduceIte, hLhs, hRhs,
        bind, Except.bind, pure, Except.pure]
      exact ⟨_, rfl⟩
  | load _ _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .load fn (.instruction blk.id idx)) List.mem_cons_self
  | store _ _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .store fn (.instruction blk.id idx)) List.mem_cons_self
  | beginLifetime _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .beginLifetime fn (.instruction blk.id idx)) List.mem_cons_self
  | endLifetime _ =>
    rw [hInst] at hInstIn
    cases hInstIn (.unsupportedStorage .endLifetime fn (.instruction blk.id idx)) List.mem_cons_self
  | destructure src _ =>
    rw [hInst] at hInstIn
    cases src with
    | register _ =>
      cases hInstIn (.unsupportedStorage .destructureRegister fn (.instruction blk.id idx)) List.mem_cons_self
    | materialized _ =>
      cases hInstIn (.unsupportedStorage .destructureMaterialized fn (.instruction blk.id idx)) List.mem_cons_self
  | call callee _ _ =>
    rw [hInst] at hInstIn
    cases hCal : (AList.lookup bir.functions callee).isSome with
    | true =>
      have hBad := hInstIn (.unsupportedCall .reachableInternalCall (some callee) fn (.instruction blk.id idx)) (by
        dsimp only
        rw [hER, hCal]
        exact List.mem_cons_self)
      cases hBad
    | false =>
      have hBad := hInstIn (.unsupportedCall .foreignCall (some callee) fn (.instruction blk.id idx)) (by
        dsimp only
        rw [hER, hCal]
        exact List.mem_cons_self)
      cases hBad

/-- Universal totality theorem for `EmitLCB.emitUnvalidated`: every well-formed LCB `BIR`
within trap capacity, without reachable cross-function calls, and with `firstUnsupported = none`
succeeds in `emitUnvalidated` (producing a `Source.Function` and validated `additionalJobs`
satisfying all structural conjuncts of `LCBEncoding`). -/
theorem emitUnvalidated_ok_of_wf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := language) bir)
    (hCap : ¬ (LCBInput.trapCapacity < bir.sites.size))
    (hLink : bir.requiresLCBLinking = false)
    (hSup : firstUnsupported (language := language) bir = none) :
    ∃ output, emitUnvalidated (language := language) bir = .ok output := by
  obtain ⟨diagnostics, hDiag⟩ := siteDiagnostics_of_sites_bind hWF.sites_bind
  have hEntryMem := hWF.registerFormation.entry_mem
  have hEntryLook : ∃ entryVal, AList.lookup bir.functions bir.entry = some entryVal := by
    obtain ⟨⟨fn, locals, graph⟩, hMem, hEq⟩ := List.mem_map.mp hEntryMem
    dsimp only at hEq
    subst hEq
    exact alist_lookup_isSome_of_mem hMem
  obtain ⟨⟨locals, graph⟩, hFnLook⟩ := hEntryLook
  have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem_local hFnLook
  let normGraphCfg : NormalizedCFG lang :=
    ⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩
  have hNormMem : (bir.entry, locals, normGraphCfg) ∈ (RegisterIR.bir bir).normalize.functions := by
    unfold RegisterIR.normalize
    exact List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
  have hFnFormed := hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem
  have hNodesNodup : (graph.blocks.toList.map Block.id).Nodup := by
    have hnd := hFnFormed.2.2.1
    rw [List.map_map] at hnd
    exact hnd
  obtain ⟨sig, _, ⟨normEntry, hNormEntryMem, hNormEntryId, _⟩, _, _, hNormBlks⟩ := hFnFormed.2.2.2
  obtain ⟨entryBlk, hEntryBlkMem, hEntryBlkEq⟩ := List.mem_map.mp hNormEntryMem
  have hEntryId : entryBlk.id = graph.entry := (congrArg Block.id hEntryBlkEq).trans hNormEntryId
  obtain ⟨foundEntry, hFindEntry, _, _⟩ :=
    find?_some_of_mem (p := (·.id == graph.entry)) ⟨entryBlk, hEntryBlkMem, beq_iff_eq.mpr hEntryId⟩
  let blocks := foundEntry :: (graph.blocks.toList.filter fun blk =>
    blk.id != graph.entry && (BIR.reachableBlocks graph).contains blk.id)
  have hOrd : orderedReachableBlocks graph = some blocks := by
    simp [orderedReachableBlocks, hFindEntry, blocks]
  have hNormNodesEq : normGraphCfg.blocks.map Block.id = graph.blocks.toList.map Block.id := by
    simp [normGraphCfg, List.map_map]
  have hNormEdgesEq :
      (normGraphCfg.blocks.flatMap fun blk => blk.ending.successors.map fun edge => (blk.id, edge.target)) =
      (graph.blocks.toList.flatMap fun blk => blk.ending.successors.map fun edge => (blk.id, edge.target)) := by
    simp only [normGraphCfg, List.flatMap_map]
    congr 1
    funext blk
    cases blk.ending with
    | trap _ => simp [RegTerminator.ofBIR]; split <;> rfl
    | jump _ | branch _ _ _ | ret _ => rfl
  let defs := functionDefTable normGraphCfg
  let gNodes := graph.blocks.toList.map Block.id
  let gEdges := graph.blocks.toList.flatMap fun blk => blk.ending.successors.map fun edge => (blk.id, edge.target)
  have hBlkSpec : ∀ blk ∈ graph.blocks.toList,
      (∀ edge ∈ blk.ending.successors,
        ∃ tgt ∈ graph.blocks.toList, tgt.id = edge.target ∧
          edge.arguments.map Sigma.fst = tgt.params.map Sigma.fst) ∧
      (∀ r ∈ blk.ending.regReads, readLiteralOk language r = true) ∧
      (∀ use ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).regUses,
        ∃ defSite, AList.lookup defs use.1 = some (use.2, defSite) ∧
          defSite.DominatesUse gNodes gEdges graph.entry ⟨blk.id, .term⟩) ∧
      (∀ pair ∈ blk.body.zipIdx,
        instLocalOk language bir.declarations profile.memoryTarget bir.signatures locals pair.1.instruction = true ∧
        ∀ use ∈ pair.1.instruction.regUses,
          ∃ defSite, AList.lookup defs use.1 = some (use.2, defSite) ∧
            defSite.DominatesUse gNodes gEdges graph.entry ⟨blk.id, .inst pair.2⟩) := by
    intro blk hBlk
    have hnb : (⟨blk.id, blk.params, blk.body, RegTerminator.ofBIR bir.sites.size blk.ending,
        blk.boundaryOrigins⟩ : Block _ _ (RegTerminator lang)) ∈ normGraphCfg.blocks :=
      List.mem_map.mpr ⟨blk, hBlk, rfl⟩
    obtain ⟨hSucc, _, hTermReads, hTermUses, hBody⟩ := hNormBlks _ hnb
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro edge hEdge
      have hEdgeNorm : edge ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).successors := by
        cases hEnd : blk.ending with
        | jump e => rw [hEnd] at hEdge; exact hEdge
        | branch _ y n => rw [hEnd] at hEdge; exact hEdge
        | ret _ => rw [hEnd] at hEdge; cases hEdge
        | trap _ => rw [hEnd] at hEdge; cases hEdge
      obtain ⟨ntgt, hntgt, hntgtId, hntgtArgs⟩ := hSucc edge hEdgeNorm
      obtain ⟨tgt, hTgtMem, hTgtEq⟩ := List.mem_map.mp hntgt
      refine ⟨tgt, hTgtMem, (congrArg Block.id hTgtEq).trans hntgtId, ?_⟩
      exact hntgtArgs.trans (congrArg (List.map Sigma.fst) (congrArg Block.params hTgtEq).symm)
    · intro r hr
      have hrNorm : r ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).reads := by
        cases hEnd : blk.ending with
        | jump e => rw [hEnd] at hr; exact hr
        | branch _ y n => rw [hEnd] at hr; exact hr
        | ret v => cases v <;> (rw [hEnd] at hr; exact hr)
        | trap _ => rw [hEnd] at hr; cases hr
      exact hTermReads r hrNorm
    · intro use huse
      obtain ⟨defSite, hLook, hDom⟩ := hTermUses use huse
      rw [hNormNodesEq, hNormEdgesEq] at hDom
      exact ⟨defSite, hLook, hDom⟩
    · intro pair hPair
      obtain ⟨hLoc, hUses⟩ := hBody pair hPair
      refine ⟨hLoc, fun use huse => ?_⟩
      obtain ⟨defSite, hLook, hDom⟩ := hUses use huse
      rw [hNormNodesEq, hNormEdgesEq] at hDom
      exact ⟨defSite, hLook, hDom⟩
  have hAllSup := (firstUnsupported_none_iff (language := language) bir).mp hSup
  have hEntryReach := mem_reachableFunctions_entry hEntryMem
  let paramCount := match blocks with | entryBlk :: _ => entryBlk.params.length | [] => 0
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let offset := if entryHasPreds then 1 else 0
  let indices := blockIndexTable offset blocks
  let splits := collectSplitEdges offset blocks indices
  have hIndexOfMem : ∀ blk ∈ blocks, ∃ idx, AList.lookup indices blk.id = some idx := by
    intro blk hBlk
    obtain ⟨n, hn⟩ := mem_indexed_exists hBlk
    exact alist_lookup_isSome_of_mem (List.mem_map.mpr ⟨(n, blk), hn, rfl⟩)
  have hSuccIndex : ∀ blk ∈ blocks, ∀ edge ∈ blk.ending.successors,
      ∃ idx, AList.lookup indices edge.target = some idx := by
    intro blk hBlk edge hEdge
    obtain ⟨hBlkGraph, hBlkReach⟩ := orderedReachable_mem_subset hOrd hBlk
    obtain ⟨tgt, hTgtGraph, hTgtId, _⟩ := (hBlkSpec blk hBlkGraph).1 edge hEdge
    have hTgtReach := mem_reachableBlocks_succ hBlkReach hBlkGraph hEdge hTgtGraph hTgtId
    have hTgtMem := block_in_orderedReachable hOrd hNodesNodup hTgtGraph (hTgtId.symm ▸ hTgtReach)
    rw [← hTgtId]
    exact hIndexOfMem tgt hTgtMem
  -- Show `normalizeGraph graph` succeeds and characterize every normalized block and incoming edge.
  have hRealBlocksOk : ∃ realBlocks,
      (indexed blocks).mapM (normalizeBlock offset indices splits) = .ok realBlocks := by
    apply list_mapM_except_ok
    intro pair hPair
    rcases pair with ⟨j, blk⟩
    have hBlkMem : blk ∈ blocks := mem_indexed_left hPair
    unfold normalizeBlock normalizeTerm
    cases hEnd : blk.ending with
    | ret v => exact ⟨_, rfl⟩
    | trap slot => exact ⟨_, rfl⟩
    | jump edge =>
      obtain ⟨tgtIdx, hTgt⟩ := hSuccIndex blk hBlkMem edge (by simp [hEnd, BIRTerm.successors])
      dsimp only
      rw [hTgt]
      exact ⟨_, rfl⟩
    | branch test yes no =>
      obtain ⟨yesIdx, hYes⟩ := hSuccIndex blk hBlkMem yes (by simp [hEnd, BIRTerm.successors])
      obtain ⟨noIdx, hNo⟩ := hSuccIndex blk hBlkMem no (by simp [hEnd, BIRTerm.successors])
      dsimp only
      rw [hYes, hNo]
      cases splits.find? (·.predIndex == offset + j) <;> exact ⟨_, rfl⟩
  obtain ⟨realBlocks, hRealBlocks⟩ := hRealBlocksOk
  let splitBlocks : List (NormBlock lang) := splits.map fun sp =>
    { sourceId := none, params := [], body := [], term := .jump sp.targetIndex sp.arguments }
  let trampoline : List (NormBlock lang) :=
    if entryHasPreds then
      [{ sourceId := none, params := [], body := [], term := .jumpEntryParams 1 paramCount }]
    else []
  let norm : NormGraph lang :=
    { paramCount, entryHasPreds, blocks := trampoline ++ realBlocks ++ splitBlocks }
  have hNormOk : normalizeGraph graph = .ok norm := by
    unfold normalizeGraph
    simp only [hOrd]
    rw [hRealBlocks]
    rfl
  -- Correspondence between `blocks` and `realBlocks`: every `blk ∈ blocks` appears in `realBlocks`
  -- with the exact same `params` and `body`, and at index `offset + j` in `norm.blocks`.
  have hMapM_elem : ∀ (xs : List (Nat × BIRBlock lang)) (ys : List (NormBlock lang)),
      xs.mapM (normalizeBlock offset indices splits) = .ok ys →
      ys.length = xs.length ∧
      (∀ k, k < xs.length →
        ∃ (nb : NormBlock lang), ys[k]? = some nb ∧
          ∃ (j : Nat) (blk : BIRBlock lang), xs[k]? = some (j, blk) ∧
            nb.sourceId = some blk.id ∧ nb.params = blk.params ∧ nb.body = blk.body ∧
            (∀ t c, nb.term ≠ .jumpEntryParams t c) ∧
            (∀ r, (match nb.term with
              | .ret (some r') => r = r'
              | .jump _ args => r ∈ args
              | .branch test _ yArgs _ nArgs => r = ⟨lang.bool, test⟩ ∨ r ∈ yArgs ∨ r ∈ nArgs
              | _ => False) → r ∈ blk.ending.regReads) ∧
            (∀ tgt args, (nb.term = .jump tgt args ∨
                ∃ test y ya n na, nb.term = .branch test y ya n na ∧
                  ((tgt = y ∧ args = ya) ∨ (tgt = n ∧ args = na))) →
              (args = [] ∧ ∃ sp ∈ splits, sp.predIndex = offset + j ∧ sp.splitIndex = tgt) ∨
              ∃ edge ∈ blk.ending.successors,
                AList.lookup indices edge.target = some tgt ∧ edge.arguments = args)) := by
    intro xs
    induction xs with
    | nil =>
      intro ys h
      cases h
      exact ⟨rfl, fun _ hk => absurd hk (Nat.not_lt_zero _)⟩
    | cons hd tl ih =>
      intro ys h
      rcases hd with ⟨j, blk⟩
      simp only [List.mapM_cons] at h
      cases hTl : tl.mapM (normalizeBlock offset indices splits) with
      | error e =>
        rw [hTl] at h
        cases hHead : normalizeBlock offset indices splits (j, blk) with
        | error _ => rw [hHead] at h; cases h
        | ok _ => simp [hHead, bind, Except.bind] at h
      | ok ytl =>
        rw [hTl] at h
        unfold normalizeBlock at h
        cases hTerm : normalizeTerm indices splits (offset + j) blk.ending with
        | error e =>
          simp only [hTerm, bind, Except.bind] at h
          cases h
        | ok nt =>
          simp only [hTerm, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
          cases h
          obtain ⟨hLen, hRest⟩ := ih ytl hTl
          unfold normalizeTerm at hTerm
          refine ⟨by simp [hLen], ?_⟩
          intro k hk
          cases k with
          | zero =>
            refine ⟨⟨some blk.id, blk.params, blk.body, nt⟩, rfl, j, blk, rfl, rfl, rfl, rfl, ?_, ?_, ?_⟩
            · intro t c hJep
              cases hEnd : blk.ending with
              | ret _ => rw [hEnd] at hTerm; cases hTerm; cases hJep
              | trap _ => rw [hEnd] at hTerm; cases hTerm; cases hJep
              | jump edge =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hLk : AList.lookup indices edge.target with
                | none => rw [hLk] at hTerm; cases hTerm
                | some _ => rw [hLk] at hTerm; cases hTerm; cases hJep
              | branch _ yes no =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hY : AList.lookup indices yes.target with
                | none => rw [hY] at hTerm; cases hTerm
                | some _ =>
                  cases hN : AList.lookup indices no.target with
                  | none => rw [hY, hN] at hTerm; cases hTerm
                  | some _ =>
                    rw [hY, hN] at hTerm; dsimp only at hTerm
                    cases hSp : splits.find? (·.predIndex == offset + j) with
                    | none => rw [hSp] at hTerm; cases hTerm; cases hJep
                    | some _ => rw [hSp] at hTerm; cases hTerm; cases hJep
            · intro r hr
              cases hEnd : blk.ending with
              | ret ov =>
                rw [hEnd] at hTerm; cases hTerm
                cases ov with
                | none => cases hr
                | some r' => subst hr; simp [BIRTerm.regReads]
              | trap _ => rw [hEnd] at hTerm; cases hTerm; cases hr
              | jump edge =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hLk : AList.lookup indices edge.target with
                | none => rw [hLk] at hTerm; cases hTerm
                | some tgtIdx =>
                  rw [hLk] at hTerm; cases hTerm
                  simpa [BIRTerm.regReads] using hr
              | branch test yes no =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hY : AList.lookup indices yes.target with
                | none => rw [hY] at hTerm; cases hTerm
                | some yIdx =>
                  cases hN : AList.lookup indices no.target with
                  | none => rw [hY, hN] at hTerm; cases hTerm
                  | some nIdx =>
                    rw [hY, hN] at hTerm; dsimp only at hTerm
                    cases hSp : splits.find? (·.predIndex == offset + j) with
                    | none =>
                      rw [hSp] at hTerm; cases hTerm
                      rcases hr with rfl | hya | hna
                      · exact List.mem_cons_self
                      · exact List.mem_cons_of_mem _ (List.mem_append_left _ hya)
                      · exact List.mem_cons_of_mem _ (List.mem_append_right _ hna)
                    | some sp =>
                      rw [hSp] at hTerm; cases hTerm
                      rcases hr with rfl | hya | hna
                      · exact List.mem_cons_self
                      · exact List.mem_cons_of_mem _ (List.mem_append_left _ hya)
                      · cases hna
            · intro tgt args hEdgeTerm
              cases hEnd : blk.ending with
              | ret _ =>
                rw [hEnd] at hTerm; cases hTerm
                rcases hEdgeTerm with h1 | ⟨_, _, _, _, _, h2, _⟩
                · cases h1
                · cases h2
              | trap _ =>
                rw [hEnd] at hTerm; cases hTerm
                rcases hEdgeTerm with h1 | ⟨_, _, _, _, _, h2, _⟩
                · cases h1
                · cases h2
              | jump edge =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hLk : AList.lookup indices edge.target with
                | none => rw [hLk] at hTerm; cases hTerm
                | some tgtIdx =>
                  rw [hLk] at hTerm; cases hTerm
                  rcases hEdgeTerm with h1 | ⟨_, _, _, _, _, h2, _⟩
                  · cases h1
                    exact Or.inr ⟨edge, by simp [BIRTerm.successors], hLk, rfl⟩
                  · cases h2
              | branch test yes no =>
                rw [hEnd] at hTerm; dsimp only at hTerm
                cases hY : AList.lookup indices yes.target with
                | none => rw [hY] at hTerm; cases hTerm
                | some yIdx =>
                  cases hN : AList.lookup indices no.target with
                  | none => rw [hY, hN] at hTerm; cases hTerm
                  | some nIdx =>
                    rw [hY, hN] at hTerm; dsimp only at hTerm
                    cases hSp : splits.find? (·.predIndex == offset + j) with
                    | none =>
                      rw [hSp] at hTerm; cases hTerm
                      rcases hEdgeTerm with h1 | ⟨_, _, _, _, _, h2, hChoice⟩
                      · cases h1
                      · cases h2
                        rcases hChoice with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
                        · exact Or.inr ⟨yes, by simp [BIRTerm.successors], hY, rfl⟩
                        · exact Or.inr ⟨no, by simp [BIRTerm.successors], hN, rfl⟩
                    | some sp =>
                      rw [hSp] at hTerm; cases hTerm
                      rcases hEdgeTerm with h1 | ⟨_, _, _, _, _, h2, hChoice⟩
                      · cases h1
                      · cases h2
                        rcases hChoice with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
                        · exact Or.inr ⟨yes, by simp [BIRTerm.successors], hY, rfl⟩
                        · exact Or.inl ⟨rfl, sp, List.mem_of_find?_eq_some hSp,
                            eq_of_beq (by simpa using List.find?_some hSp), rfl⟩
          | succ k' =>
            exact hRest k' (Nat.lt_of_succ_lt_succ hk)
  obtain ⟨hRealLen, hRealSpec⟩ := hMapM_elem (indexed blocks) realBlocks hRealBlocks
  -- Every reachable `blk ∈ blocks` corresponds to a `nb ∈ norm.blocks` with identical `params` and `body`.
  have hBlkInNorm : ∀ blk ∈ blocks, ∃ nb ∈ norm.blocks, nb.params = blk.params ∧ nb.body = blk.body := by
    intro blk hBlk
    have hEx : ∃ k, k < blocks.length ∧ blocks[k]? = some blk := by
      obtain ⟨k, hk⟩ := List.mem_iff_getElem?.mp hBlk
      have hlt : k < blocks.length := by
        rcases Nat.lt_or_ge k blocks.length with h | h
        · exact h
        · rw [List.getElem?_eq_none h] at hk; cases hk
      exact ⟨k, hlt, hk⟩
    obtain ⟨k, hkLt, hkGet⟩ := hEx
    have hIdxLen : (indexed blocks).length = blocks.length := by
      have h := congrArg List.length (indexed_map_snd blocks)
      simpa only [List.length_map] using h
    obtain ⟨nb, hNbGet, j, blk', hIdxGet, _, hP, hB, _, _, _⟩ := hRealSpec k (hIdxLen.symm ▸ hkLt)
    rw [Backend.Runtime.getElem?_indexed, hkGet] at hIdxGet
    cases hIdxGet
    have hNbReal : nb ∈ realBlocks := List.mem_of_getElem? hNbGet
    exact ⟨nb, List.mem_append_left _ (List.mem_append_right _ hNbReal), hP, hB⟩
  -- Every register `r` defined at a site dominating a reachable block `blk` is in `regIds norm`!
  have hRegIdOfDom : ∀ {blk : BIRBlock lang} {r : Nat} {rty : language.operands.Ty} {usePt : UsePoint},
      blk ∈ blocks →
      (∃ defSite, AList.lookup defs r = some (rty, defSite) ∧
        defSite.DominatesUse gNodes gEdges graph.entry ⟨blk.id, usePt⟩) →
      ∃ slot, regId norm r = some slot := by
    intro blk r rty usePt hBlkMem ⟨defSite, hDefLook, hDom⟩
    obtain ⟨_, hBlkReach⟩ := orderedReachable_mem_subset hOrd hBlkMem
    have hDefBlkReach : defSite.block ∈ BIR.reachableBlocks graph := by
      unfold DefSite.DominatesUse at hDom
      by_cases hSame : defSite.block = blk.id
      · exact hSame ▸ hBlkReach
      · simp only [hSame, ↓reduceIte] at hDom
        unfold BIR.reachableBlocks at hBlkReach ⊢
        rw [mem_reachGraph_iff] at hBlkReach ⊢
        exact graphPath_hits_dominator hBlkReach hDom
    have hDefIn := alist_lookup_mem_local hDefLook
    unfold defs functionDefTable at hDefIn
    obtain ⟨nDefBlk, hNDefBlkMem, hInBlk⟩ := List.mem_flatMap.mp hDefIn
    obtain ⟨defBlk, hDefBlkGraph, hDefBlkEq⟩ := List.mem_map.mp hNDefBlkMem
    have hDefBlkId : defBlk.id = nDefBlk.id := congrArg Block.id hDefBlkEq
    have hDefBlkParams : defBlk.params = nDefBlk.params := congrArg Block.params hDefBlkEq
    have hDefBlkBody : defBlk.body = nDefBlk.body := congrArg Block.body hDefBlkEq
    rcases List.mem_append.mp hInBlk with hParam | hBodyDef
    · obtain ⟨p, hpMem, hpEq⟩ := List.mem_map.mp hParam
      have hSiteEq : defSite = ⟨nDefBlk.id, .param⟩ := (Prod.mk.inj (Prod.mk.inj hpEq).2).2.symm
      have hIdEq : p.2.id = r := (Prod.mk.inj hpEq).1
      have hDefReach : defBlk.id ∈ BIR.reachableBlocks graph := by
        rw [hDefBlkId, ← (show defSite.block = nDefBlk.id by rw [hSiteEq])]
        exact hDefBlkReach
      have hDefInBlocks := block_in_orderedReachable hOrd hNodesNodup hDefBlkGraph hDefReach
      obtain ⟨nb, hNbMem, hNbParams, _⟩ := hBlkInNorm defBlk hDefInBlocks
      obtain ⟨j, hj⟩ := mem_indexed_exists hNbMem
      obtain ⟨k, hk⟩ := mem_indexed_exists (show p ∈ nb.params by rw [hNbParams, hDefBlkParams]; exact hpMem)
      have hInRegIds : (r, if j == 0 then k else blockBase norm j + k) ∈ regIds norm := by
        unfold regIds
        refine List.mem_flatMap.mpr ⟨(j, nb), hj, ?_⟩
        apply List.mem_append_left
        exact List.mem_map.mpr ⟨(k, p), hk, by cases p; dsimp only; exact congrArg (·, _) hIdEq⟩
      exact alist_lookup_isSome_of_mem hInRegIds
    · obtain ⟨⟨defOp, defIdx⟩, hDefOpMem, hOpDef⟩ := List.mem_flatMap.mp hBodyDef
      obtain ⟨⟨defId, defTy⟩, hIdInDefs, hDefPairEq⟩ := List.mem_map.mp hOpDef
      have hSiteEq : defSite = ⟨nDefBlk.id, .inst defIdx⟩ := (Prod.mk.inj (Prod.mk.inj hDefPairEq).2).2.symm
      have hIdEq : defId = r := (Prod.mk.inj hDefPairEq).1
      have hDefReach : defBlk.id ∈ BIR.reachableBlocks graph := by
        rw [hDefBlkId, ← (show defSite.block = nDefBlk.id by rw [hSiteEq])]
        exact hDefBlkReach
      have hDefInBlocks := block_in_orderedReachable hOrd hNodesNodup hDefBlkGraph hDefReach
      have hDefOpInBlk : (defOp, defIdx) ∈ defBlk.body.zipIdx := by
        rw [hDefBlkBody]; exact hDefOpMem
      obtain ⟨hDefsCov, _⟩ := supported_lcb_inst_shape hBackend hFnMem hEntryReach hDefBlkGraph
        hDefReach hDefOpInBlk ((hBlkSpec defBlk hDefBlkGraph).2.2.2 (defOp, defIdx) hDefOpInBlk).1 hAllSup
      obtain ⟨off, hOff⟩ := hDefsCov defId defTy hIdInDefs
      obtain ⟨nb, hNbMem, _, hNbBody⟩ := hBlkInNorm defBlk hDefInBlocks
      obtain ⟨j, hj⟩ := mem_indexed_exists hNbMem
      have hDefOpMemBody : defOp ∈ nb.body := by
        rw [hNbBody, hDefBlkBody]
        exact List.mem_of_getElem? (List.mem_zipIdx_iff_getElem?.mp hDefOpMem)
      obtain ⟨i, hi⟩ := mem_indexed_exists hDefOpMemBody
      have hInRegIds : (r, blockBase norm j + instBase j nb i + off) ∈ regIds norm := by
        unfold regIds
        refine List.mem_flatMap.mpr ⟨(j, nb), hj, ?_⟩
        apply List.mem_append_right
        refine List.mem_flatMap.mpr ⟨(i, defOp), hi, ?_⟩
        exact List.mem_map.mpr ⟨(defId, off), hOff, by simp [hIdEq]⟩
      exact alist_lookup_isSome_of_mem hInRegIds
  -- Helper: every `SSARead` used in a reachable block's terminator or instruction lowers via `emitRead`.
  have hReadTermOk : ∀ {blk : BIRBlock lang} {r : Sigma (SSARead language.operands.Ty lang.literal)},
      blk ∈ blocks → r ∈ blk.ending.regReads →
      ∃ opnd, emitRead language norm r.2 = .ok opnd := by
    intro blk r hBlkMem hInReads
    obtain ⟨hBlkGraph, _⟩ := orderedReachable_mem_subset hOrd hBlkMem
    rcases r with ⟨rty, read⟩
    cases read with
    | literal lit =>
      obtain ⟨val, hVal⟩ := decodeLiteralWord_some_of_ok ((hBlkSpec blk hBlkGraph).2.1 _ hInReads)
      exact ⟨.literal val, by simp [emitRead, hVal]⟩
    | value v =>
      have hInUses : (v.id, rty) ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).regUses := by
        unfold RegTerminator.regUses
        apply List.mem_filterMap.mpr
        refine ⟨⟨rty, .value v⟩, ?_, rfl⟩
        cases hEnd : blk.ending with
        | jump e => rw [hEnd] at hInReads; exact hInReads
        | branch _ y n => rw [hEnd] at hInReads; exact hInReads
        | ret ov => cases ov <;> (rw [hEnd] at hInReads; exact hInReads)
        | trap _ => rw [hEnd] at hInReads; cases hInReads
      obtain ⟨slot, hSlot⟩ := hRegIdOfDom hBlkMem ((hBlkSpec blk hBlkGraph).2.2.1 _ hInUses)
      exact ⟨.local slot, by simp [emitRead, hSlot]⟩
  have hReadInstOk : ∀ {blk : BIRBlock lang} {op : LocatedOp (SSAInst lang)} {idx : Nat}
      {r : Sigma (SSARead language.operands.Ty lang.literal)},
      blk ∈ blocks → (op, idx) ∈ blk.body.zipIdx → r ∈ op.instruction.reads →
      ∃ opnd, emitRead language norm r.2 = .ok opnd := by
    intro blk op idx r hBlkMem hOp hInReads
    obtain ⟨hBlkGraph, _⟩ := orderedReachable_mem_subset hOrd hBlkMem
    have hOpSpec := (hBlkSpec blk hBlkGraph).2.2.2 (op, idx) hOp
    rcases r with ⟨rty, read⟩
    cases read with
    | literal lit =>
      have hLitAll := List.all_eq_true.mp (Bool.and_eq_true_iff.mp hOpSpec.1).1
      obtain ⟨val, hVal⟩ := decodeLiteralWord_some_of_ok (hLitAll _ hInReads)
      exact ⟨.literal val, by simp [emitRead, hVal]⟩
    | value v =>
      have hInUses : (v.id, rty) ∈ op.instruction.regUses := by
        unfold SSAInst.regUses
        cases hI : op.instruction with
        | destructure src _ =>
          rw [hI] at hInReads
          cases src with
          | register _ => exact List.mem_cons_of_mem _ (List.mem_filterMap.mpr ⟨⟨rty, .value v⟩, hInReads, rfl⟩)
          | materialized _ => exact List.mem_filterMap.mpr ⟨⟨rty, .value v⟩, hInReads, rfl⟩
        | _ =>
          rw [hI] at hInReads
          exact List.mem_filterMap.mpr ⟨⟨rty, .value v⟩, hInReads, rfl⟩
      obtain ⟨slot, hSlot⟩ := hRegIdOfDom hBlkMem (hOpSpec.2 _ hInUses)
      exact ⟨.local slot, by simp [emitRead, hSlot]⟩
  -- Characterize split edges back to their originating reachable block & edge.
  have hSplitOrigin : ∀ sp ∈ splits,
      ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
        AList.lookup indices edge.target = some sp.targetIndex ∧ edge.arguments = sp.arguments := by
    have go : ∀ (xs : List (Nat × BIRBlock lang)) (nextIdx : Nat) (revAcc : List (SplitEdge lang)),
        (∀ x ∈ xs, x.2 ∈ blocks) →
        (∀ sp ∈ revAcc, ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
          AList.lookup indices edge.target = some sp.targetIndex ∧ edge.arguments = sp.arguments) →
        ∀ sp ∈ collectSplitEdges.go offset blocks indices xs nextIdx revAcc,
          ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
            AList.lookup indices edge.target = some sp.targetIndex ∧ edge.arguments = sp.arguments := by
      intro xs
      induction xs with
      | nil =>
        intro nextIdx revAcc _ hAcc sp hsp
        exact hAcc sp (List.mem_reverse.mp hsp)
      | cons pair rest ih =>
        intro nextIdx revAcc hXs hAcc sp hsp
        rcases pair with ⟨j, blk⟩
        have hBlkMem : blk ∈ blocks := hXs (j, blk) List.mem_cons_self
        have hRestMem : ∀ x ∈ rest, x.2 ∈ blocks := fun x hx => hXs x (List.mem_cons_of_mem _ hx)
        unfold collectSplitEdges.go at hsp
        cases hEnd : blk.ending with
        | jump _ | ret _ | trap _ =>
          rw [hEnd] at hsp
          exact ih nextIdx revAcc hRestMem hAcc sp hsp
        | branch test yes no =>
          rw [hEnd] at hsp
          dsimp only at hsp
          split at hsp
          · rename_i hCond
            have hTgtEq : yes.target = no.target := eq_of_beq (Bool.and_eq_true_iff.mp hCond).1
            cases hLk : AList.lookup indices yes.target with
            | none =>
              rw [hLk] at hsp
              exact ih nextIdx revAcc hRestMem hAcc sp hsp
            | some tgtIdx =>
              rw [hLk] at hsp
              apply ih (nextIdx + 1) _ hRestMem _ sp hsp
              intro sp' hsp'
              cases hsp' with
              | head =>
                refine ⟨blk, hBlkMem, no, by simp [hEnd, BIRTerm.successors], hTgtEq ▸ hLk, rfl⟩
              | tail _ htl => exact hAcc sp' htl
          · exact ih nextIdx revAcc hRestMem hAcc sp hsp
    exact go (indexed blocks) (offset + blocks.length) [] (fun _ hx => mem_indexed_left hx)
      (fun _ hm => nomatch hm)
  -- Lookup invertibility on `indices`: `AList.lookup indices tid = some idx` means `norm.blocks[idx]?`
  -- is the real block for `tid`.
  have hIndexTargetBlk : ∀ {tid : BlockId} {j : Nat} {nb : NormBlock lang},
      AList.lookup indices tid = some j → norm.blocks[j]? = some nb →
      ∃ tgtBlk ∈ blocks, tgtBlk.id = tid ∧ nb.params = tgtBlk.params := by
    intro tid j nb hLk hGet
    have hInIdx := alist_lookup_mem_local hLk
    unfold indices blockIndexTable at hInIdx
    obtain ⟨⟨k, tgtBlk⟩, hkMem, hkEq⟩ := List.mem_map.mp hInIdx
    have hTgtId : tgtBlk.id = tid := (Prod.mk.inj hkEq).1
    have hJEq : offset + k = j := (Prod.mk.inj hkEq).2
    have hTgtMem : tgtBlk ∈ blocks := mem_indexed_left hkMem
    have hkLt : k < blocks.length := getElem?_indexed_lt hkMem
    have hIdxLen : (indexed blocks).length = blocks.length := by
      have h := congrArg List.length (indexed_map_snd blocks)
      simpa only [List.length_map] using h
    obtain ⟨nb', hNb'Get, j', blk', hIdxGet, _, hNb'Params, _, _, _, _⟩ :=
      hRealSpec k (hIdxLen.symm ▸ hkLt)
    have hkGet : blocks[k]? = some tgtBlk := by
      have hGo : ∀ n (ys : List (BIRBlock lang)), (k, tgtBlk) ∈ Backend.Runtime.indexedFrom n ys →
          n ≤ k ∧ ys[k - n]? = some tgtBlk := by
        intro n ys hm
        induction ys generalizing n with
        | nil => cases hm
        | cons a rest ih =>
          simp only [Backend.Runtime.indexedFrom, List.mem_cons, Prod.mk.injEq] at hm
          rcases hm with ⟨rfl, rfl⟩ | htl
          · exact ⟨Nat.le_refl _, by simp⟩
          · obtain ⟨hle, hgt⟩ := ih (n + 1) htl
            have hPos : k - n = (k - (n + 1)) + 1 := by omega
            refine ⟨by omega, ?_⟩
            rw [hPos, List.getElem?_cons_succ]
            exact hgt
      simpa using (hGo 0 blocks hkMem).2
    rw [Backend.Runtime.getElem?_indexed, hkGet] at hIdxGet
    cases hIdxGet
    have hTrampLen : trampoline.length = offset := by
      unfold trampoline offset
      cases entryHasPreds <;> rfl
    have hNormGet : norm.blocks[j]? = some nb' := by
      show (trampoline ++ realBlocks ++ splitBlocks)[j]? = some nb'
      rw [← hJEq, List.append_assoc]
      rw [List.getElem?_append_right (by rw [hTrampLen]; omega)]
      rw [hTrampLen, Nat.add_sub_cancel_left]
      rw [List.getElem?_append_left (by rw [hRealLen, hIdxLen]; exact hkLt)]
      exact hNb'Get
    rw [hNormGet] at hGet
    cases Option.some.inj hGet
    exact ⟨tgtBlk, hTgtMem, hTgtId, hNb'Params⟩
  -- Every block in `norm.blocks` lowers cleanly via `emitBlock`!
  have hBlocksEmitOk : ∃ sBlocks,
      (indexed norm.blocks).mapM (emitBlock language norm) = .ok sBlocks := by
    apply list_mapM_except_ok
    intro pair hPair
    rcases pair with ⟨j, nb⟩
    have hJlt : j < norm.blocks.length := getElem?_indexed_lt hPair
    have hNbGet : norm.blocks[j]? = some nb := by
      have hGo : ∀ n (ys : List (NormBlock lang)), (j, nb) ∈ Backend.Runtime.indexedFrom n ys →
          n ≤ j ∧ ys[j - n]? = some nb := by
        intro n ys hm
        induction ys generalizing n with
        | nil => cases hm
        | cons a rest ih =>
          simp only [Backend.Runtime.indexedFrom, List.mem_cons, Prod.mk.injEq] at hm
          rcases hm with ⟨rfl, rfl⟩ | htl
          · exact ⟨Nat.le_refl _, by simp⟩
          · obtain ⟨hle, hgt⟩ := ih (n + 1) htl
            have hPos : j - n = (j - (n + 1)) + 1 := by omega
            refine ⟨by omega, ?_⟩
            rw [hPos, List.getElem?_cons_succ]
            exact hgt
      simpa using (hGo 0 norm.blocks hPair).2
    have hTrampLen : trampoline.length = offset := by
      unfold trampoline offset
      cases entryHasPreds <;> rfl
    have hIdxLen : (indexed blocks).length = blocks.length := by
      have h := congrArg List.length (indexed_map_snd blocks)
      simpa only [List.length_map] using h
    -- 1. Phis succeed
    have hPhisOk : ∃ phis,
        (if j == 0 then Except.ok []
         else (indexed nb.params).mapM fun p => emitPhi language norm j p.1) = .ok phis := by
      by_cases hj0 : j = 0
      · subst hj0; exact ⟨[], rfl⟩
      · have hjBne : (j == 0) = false := beq_false_of_ne hj0
        simp only [hjBne, Bool.false_eq_true, ↓reduceIte]
        apply list_mapM_except_ok
        intro pPair hPPair
        rcases pPair with ⟨k, p⟩
        have hkLt : k < nb.params.length := getElem?_indexed_lt hPPair
        unfold emitPhi
        have hArmsOk : ∃ arms,
            (incomingEdges norm j).mapM (fun (edgePair : Nat × PhiEdgeArg lang) => do
              let opnd ← match edgePair.2 with
                | .entryParam count =>
                  if k < count then Except.ok (Source.Operand.local k)
                  else Except.error (StageFailure.invalidInput invalidInputRule)
                | .reads args =>
                  match args[k]? with
                  | some ⟨_, read⟩ => emitRead language norm read
                  | none => Except.error (StageFailure.invalidInput invalidInputRule)
              return (edgePair.1, opnd)) = .ok arms := by
          apply list_mapM_except_ok
          intro edgePair hEdgePair
          rcases edgePair with ⟨predIdx, edgeArg⟩
          unfold incomingEdges at hEdgePair
          obtain ⟨⟨pIdx, pNb⟩, hPNbPair, hInEdgesTo⟩ := List.mem_flatMap.mp hEdgePair
          dsimp only at hInEdgesTo
          have hPNbMem : pNb ∈ norm.blocks := mem_indexed_left hPNbPair
          rcases List.mem_append.mp hPNbMem with hPre | hInSplit
          · rcases List.mem_append.mp hPre with hInTramp | hInReal
            · -- Pred is trampoline (`jumpEntryParams 1 paramCount`), so `j = 1` and `nb = entryBlk`!
              unfold trampoline at hInTramp
              cases hEP : entryHasPreds
              · rw [hEP] at hInTramp; cases hInTramp
              · rw [hEP] at hInTramp
                cases List.mem_singleton.mp hInTramp
                simp only [edgesTo] at hInEdgesTo
                split at hInEdgesTo
                · rename_i hTgt1
                  have hJ1 : j = 1 := (eq_of_beq hTgt1).symm
                  cases List.mem_singleton.mp hInEdgesTo
                  -- `nb` is `norm.blocks[1]` = `realBlocks[0]` = `foundEntry`, whose params length is `paramCount`!
                  have hReal0 : 0 < (indexed blocks).length := by
                    rw [hIdxLen]; simp [blocks]
                  obtain ⟨nb0, hNb0Get, _, blk0, hIdx0, _, hNb0Params, _, _, _, _⟩ := hRealSpec 0 hReal0
                  rw [Backend.Runtime.getElem?_indexed] at hIdx0
                  simp only [blocks, List.getElem?_cons_zero, Option.map_some, Option.some.injEq,
                    Prod.mk.injEq] at hIdx0
                  have hBlk0Eq : blk0 = foundEntry := hIdx0.2.symm
                  have hOff1 : offset = 1 := by simp [offset, hEP]
                  have hNorm1 : norm.blocks[1]? = some nb0 := by
                    show (trampoline ++ realBlocks ++ splitBlocks)[1]? = some nb0
                    rw [List.append_assoc, List.getElem?_append_right (by rw [hTrampLen, hOff1]; exact Nat.le_refl _)]
                    rw [hTrampLen, hOff1, Nat.sub_self]
                    rw [List.getElem?_append_left (by rw [hRealLen]; exact hReal0)]
                    exact hNb0Get
                  rw [hJ1, hNorm1] at hNbGet
                  cases Option.some.inj hNbGet
                  have hParamLen : nb.params.length = paramCount := by
                    rw [hNb0Params, hBlk0Eq]
                  have hkLtCount : k < paramCount := hParamLen ▸ hkLt
                  dsimp only
                  rw [ite_eq_left hkLtCount]
                  exact ⟨_, rfl⟩
                · cases hInEdgesTo
            · -- Pred is a real block in `realBlocks`
              obtain ⟨rk, hrkGet⟩ := List.mem_iff_getElem?.mp hInReal
              have hrkLt : rk < realBlocks.length := by
                rcases Nat.lt_or_ge rk realBlocks.length with h | h
                · exact h
                · rw [List.getElem?_eq_none h] at hrkGet; cases hrkGet
              obtain ⟨nbR, hNbRGet, jR, srcBlk, hIdxR, _, _, _, hNoJep, _, hTermEdgeSpec⟩ :=
                hRealSpec rk (hRealLen ▸ hrkLt)
              rw [hrkGet] at hNbRGet; cases Option.some.inj hNbRGet
              have hSrcMem : srcBlk ∈ blocks := mem_indexed_left (List.mem_of_getElem? hIdxR)
              obtain ⟨hSrcGraph, _⟩ := orderedReachable_mem_subset hOrd hSrcMem
              -- Extract `(j, args)` from `edgesTo j pIdx pNb.term`
              have hCaseArgs : ∃ args, edgeArg = .reads args ∧
                  (pNb.term = .jump j args ∨
                    ∃ test y ya n na, pNb.term = .branch test y ya n na ∧
                      ((j = y ∧ args = ya) ∨ (j = n ∧ args = na))) := by
                cases hPT : pNb.term with
                | ret _ | trap _ => rw [hPT] at hInEdgesTo; cases hInEdgesTo
                | jumpEntryParams t c => exact (hNoJep t c hPT).elim
                | jump t args =>
                  rw [hPT] at hInEdgesTo
                  simp only [edgesTo] at hInEdgesTo
                  split at hInEdgesTo
                  · rename_i heq
                    cases List.mem_singleton.mp hInEdgesTo
                    exact ⟨args, rfl, Or.inl (eq_of_beq heq ▸ rfl)⟩
                  · cases hInEdgesTo
                | branch test y ya n na =>
                  rw [hPT] at hInEdgesTo
                  simp only [edgesTo] at hInEdgesTo
                  rcases List.mem_append.mp hInEdgesTo with hY | hN
                  · split at hY
                    · rename_i heq
                      cases List.mem_singleton.mp hY
                      exact ⟨ya, rfl, Or.inr ⟨test, y, ya, n, na, rfl, Or.inl ⟨(eq_of_beq heq).symm, rfl⟩⟩⟩
                    · cases hY
                  · split at hN
                    · rename_i heq
                      cases List.mem_singleton.mp hN
                      exact ⟨na, rfl, Or.inr ⟨test, y, ya, n, na, rfl, Or.inr ⟨(eq_of_beq heq).symm, rfl⟩⟩⟩
                    · cases hN
              obtain ⟨args, hArgEq, hTermProp⟩ := hCaseArgs
              subst hArgEq
              rcases hTermEdgeSpec j args hTermProp with ⟨_, sp, hSpMem, _, hSpIdx⟩ | ⟨edge, hEdge, hLkTgt, hEdgeArgs⟩
              · -- Split target block has empty params, contradicting `k < nb.params.length`!
                exfalso
                have hSplitGe : offset + blocks.length ≤ sp.splitIndex := by
                  have go : ∀ (xs : List (Nat × BIRBlock lang)) (nxt : Nat) (acc : List (SplitEdge lang)),
                      (offset + blocks.length ≤ nxt) →
                      (∀ s ∈ acc, offset + blocks.length ≤ s.splitIndex) →
                      ∀ s ∈ collectSplitEdges.go offset blocks indices xs nxt acc,
                        offset + blocks.length ≤ s.splitIndex := by
                    intro xs
                    induction xs with
                    | nil => intro _ acc _ hA s hs; exact hA s (List.mem_reverse.mp hs)
                    | cons pr rst ih =>
                      intro nxt acc hN hA s hs
                      unfold collectSplitEdges.go at hs
                      cases hPrEnd : pr.2.ending with
                      | jump _ | ret _ | trap _ =>
                        rw [hPrEnd] at hs
                        exact ih nxt acc hN hA s hs
                      | branch _ y n =>
                        rw [hPrEnd] at hs
                        dsimp only at hs
                        split at hs
                        · cases hL : AList.lookup indices y.target with
                          | none => rw [hL] at hs; exact ih nxt acc hN hA s hs
                          | some ti =>
                            rw [hL] at hs
                            apply ih (nxt + 1) _ (by omega) _ s hs
                            intro s' hs'
                            cases hs' with
                            | head => exact hN
                            | tail _ htl => exact hA s' htl
                        · exact ih nxt acc hN hA s hs
                  exact go (indexed blocks) (offset + blocks.length) [] (Nat.le_refl _) (fun _ hm => nomatch hm) sp hSpMem
                have hNbSplit : nb ∈ splitBlocks := by
                  change (trampoline ++ realBlocks ++ splitBlocks)[j]? = some nb at hNbGet
                  rw [List.getElem?_append_right (by rw [List.length_append, hTrampLen, hRealLen, hIdxLen]; omega)] at hNbGet
                  exact List.mem_of_getElem? hNbGet
                obtain ⟨sp', _, hNbEq⟩ := List.mem_map.mp hNbSplit
                rw [← hNbEq] at hkLt
                cases hkLt
              · obtain ⟨tgtBlk, hTgtMem, hTgtId, hNbParams⟩ := hIndexTargetBlk hLkTgt hNbGet
                obtain ⟨tgtGraphBlk, hTgtGraph, hTgtGId, hArgTypes⟩ := (hBlkSpec srcBlk hSrcGraph).1 edge hEdge
                obtain ⟨hTgtBlkGraph, _⟩ := orderedReachable_mem_subset hOrd hTgtMem
                have hSameTgt := block_eq_of_id_eq hNodesNodup hTgtGraph hTgtBlkGraph (hTgtGId.trans hTgtId.symm)
                subst hSameTgt
                have hLenEq : args.length = nb.params.length := by
                  rw [hNbParams, ← hEdgeArgs]
                  have h := congrArg List.length hArgTypes
                  simpa only [List.length_map] using h
                obtain ⟨⟨rty, read⟩, hGetArg, hArgMem⟩ := getElem?_some_of_lt (hLenEq.symm ▸ hkLt)
                have hInReads : (⟨rty, read⟩ : Sigma (SSARead language.operands.Ty lang.literal)) ∈
                    srcBlk.ending.regReads := by
                  cases hSrcEnd : srcBlk.ending with
                  | jump e' =>
                    rw [hSrcEnd] at hEdge
                    cases List.mem_singleton.mp hEdge
                    simpa [BIRTerm.regReads, hEdgeArgs] using hArgMem
                  | branch _ y n =>
                    rw [hSrcEnd] at hEdge
                    simp only [BIRTerm.successors, List.mem_cons, List.not_mem_nil, or_false] at hEdge
                    rcases hEdge with hEq | hEq
                    · cases hEq; exact List.mem_cons_of_mem _ (List.mem_append_left _ (hEdgeArgs ▸ hArgMem))
                    · cases hEq; exact List.mem_cons_of_mem _ (List.mem_append_right _ (hEdgeArgs ▸ hArgMem))
                  | ret _ => rw [hSrcEnd] at hEdge; cases hEdge
                  | trap _ => rw [hSrcEnd] at hEdge; cases hEdge
                obtain ⟨opnd, hOpnd⟩ := hReadTermOk hSrcMem hInReads
                dsimp only
                rw [hGetArg]
                dsimp only
                rw [hOpnd]
                exact ⟨_, rfl⟩
          · -- Pred is a synthetic split block in `splitBlocks`
            obtain ⟨sp, hSpMem, hPNbEq⟩ := List.mem_map.mp hInSplit
            rw [← hPNbEq] at hInEdgesTo
            simp only [edgesTo] at hInEdgesTo
            split at hInEdgesTo
            · rename_i hTgtEq
              have hJEq : sp.targetIndex = j := eq_of_beq hTgtEq
              cases List.mem_singleton.mp hInEdgesTo
              obtain ⟨srcBlk, hSrcMem, edge, hEdge, hLkTgt, hEdgeArgs⟩ := hSplitOrigin sp hSpMem
              obtain ⟨hSrcGraph, _⟩ := orderedReachable_mem_subset hOrd hSrcMem
              obtain ⟨tgtBlk, hTgtMem, hTgtId, hNbParams⟩ := hIndexTargetBlk (hJEq ▸ hLkTgt) hNbGet
              obtain ⟨tgtGraphBlk, hTgtGraph, hTgtGId, hArgTypes⟩ := (hBlkSpec srcBlk hSrcGraph).1 edge hEdge
              obtain ⟨hTgtBlkGraph, _⟩ := orderedReachable_mem_subset hOrd hTgtMem
              have hSameTgt := block_eq_of_id_eq hNodesNodup hTgtGraph hTgtBlkGraph (hTgtGId.trans hTgtId.symm)
              subst hSameTgt
              have hLenEq : sp.arguments.length = nb.params.length := by
                rw [hNbParams, ← hEdgeArgs]
                have h := congrArg List.length hArgTypes
                simpa only [List.length_map] using h
              obtain ⟨⟨rty, read⟩, hGetArg, hArgMem⟩ := getElem?_some_of_lt (hLenEq.symm ▸ hkLt)
              have hInReads : (⟨rty, read⟩ : Sigma (SSARead language.operands.Ty lang.literal)) ∈
                  srcBlk.ending.regReads := by
                cases hSrcEnd : srcBlk.ending with
                | jump e' =>
                  rw [hSrcEnd] at hEdge
                  cases List.mem_singleton.mp hEdge
                  simpa [BIRTerm.regReads, hEdgeArgs] using hArgMem
                | branch _ y n =>
                  rw [hSrcEnd] at hEdge
                  simp only [BIRTerm.successors, List.mem_cons, List.not_mem_nil, or_false] at hEdge
                  rcases hEdge with hEq | hEq
                  · cases hEq; exact List.mem_cons_of_mem _ (List.mem_append_left _ (hEdgeArgs ▸ hArgMem))
                  · cases hEq; exact List.mem_cons_of_mem _ (List.mem_append_right _ (hEdgeArgs ▸ hArgMem))
                | ret _ => rw [hSrcEnd] at hEdge; cases hEdge
                | trap _ => rw [hSrcEnd] at hEdge; cases hEdge
              obtain ⟨opnd, hOpnd⟩ := hReadTermOk hSrcMem hInReads
              dsimp only
              rw [hGetArg]
              dsimp only
              rw [hOpnd]
              exact ⟨_, rfl⟩
            · cases hInEdgesTo
        obtain ⟨arms, hArms⟩ := hArmsOk
        dsimp only
        rw [hArms]
        exact ⟨_, rfl⟩
    -- 2. Body instructions and terminator succeed (by case on trampoline / real / split block)
    have hBodyTermOk :
        (∃ insts, (indexed nb.body).mapM (fun (pair : Nat × LocatedOp (SSAInst lang)) =>
          emitInst language norm (blockBase norm j + instBase j nb pair.1) pair.2.instruction) = .ok insts) ∧
        (∃ term, emitTerm language norm nb.term = .ok term) := by
      have hNbMem : nb ∈ norm.blocks := mem_indexed_left hPair
      rcases List.mem_append.mp hNbMem with hPre | hInSplit
      · rcases List.mem_append.mp hPre with hInTramp | hInReal
        · unfold trampoline at hInTramp
          cases hEP : entryHasPreds
          · rw [hEP] at hInTramp; cases hInTramp
          · rw [hEP] at hInTramp
            cases List.mem_singleton.mp hInTramp
            exact ⟨⟨[], rfl⟩, ⟨.jump 1, rfl⟩⟩
        · obtain ⟨rk, hrkGet⟩ := List.mem_iff_getElem?.mp hInReal
          have hrkLt : rk < realBlocks.length := by
            rcases Nat.lt_or_ge rk realBlocks.length with h | h
            · exact h
            · rw [List.getElem?_eq_none h] at hrkGet; cases hrkGet
          obtain ⟨nbR, hNbRGet, _, srcBlk, hIdxR, _, _, hNbBody, _, hReadsProp, _⟩ :=
            hRealSpec rk (hRealLen ▸ hrkLt)
          rw [hrkGet] at hNbRGet; cases Option.some.inj hNbRGet
          have hSrcMem : srcBlk ∈ blocks := mem_indexed_left (List.mem_of_getElem? hIdxR)
          obtain ⟨hSrcGraph, hSrcReach⟩ := orderedReachable_mem_subset hOrd hSrcMem
          refine ⟨?_, ?_⟩
          · apply list_mapM_except_ok
            intro opPair hOpPair
            rcases opPair with ⟨i, op⟩
            have hOpMem : op ∈ srcBlk.body := hNbBody ▸ mem_indexed_left hOpPair
            obtain ⟨idx, hIdx⟩ := mem_zipIdx_of_mem hOpMem
            obtain ⟨_, hEmitInst⟩ := supported_lcb_inst_shape hBackend hFnMem hEntryReach hSrcGraph
              hSrcReach hIdx ((hBlkSpec srcBlk hSrcGraph).2.2.2 (op, idx) hIdx).1 hAllSup
            exact hEmitInst norm (blockBase norm j + instBase j nb i)
              (fun r hr => hReadInstOk hSrcMem hIdx hr)
          · cases hTerm : nb.term with
            | ret ov =>
              cases ov with
              | none => exact ⟨_, rfl⟩
              | some r =>
                obtain ⟨opnd, hOpnd⟩ := hReadTermOk hSrcMem (hReadsProp r (by simp [hTerm]))
                unfold emitTerm
                simp only [hOpnd, bind, Except.bind, pure, Except.pure]
                exact ⟨_, rfl⟩
            | trap _ => exact ⟨_, rfl⟩
            | jump _ _ => exact ⟨_, rfl⟩
            | jumpEntryParams _ _ => exact ⟨_, rfl⟩
            | branch test y _ n _ =>
              obtain ⟨opnd, hOpnd⟩ := hReadTermOk hSrcMem (hReadsProp ⟨lang.bool, test⟩ (by simp [hTerm]))
              unfold emitTerm
              simp only [hOpnd, bind, Except.bind, pure, Except.pure]
              exact ⟨_, rfl⟩
      · obtain ⟨sp, _, hNbEq⟩ := List.mem_map.mp hInSplit
        rw [← hNbEq]
        exact ⟨⟨[], rfl⟩, ⟨.jump sp.targetIndex, rfl⟩⟩
    obtain ⟨phis, hPhis⟩ := hPhisOk
    obtain ⟨⟨insts, hInsts⟩, ⟨term, hTerm⟩⟩ := hBodyTermOk
    unfold emitBlock
    dsimp only
    cases hJ0 : (j == 0)
    · rw [hJ0] at hPhis
      simp only [Bool.false_eq_true, ↓reduceIte] at hPhis ⊢
      rw [hPhis, hInsts, hTerm]
      exact ⟨_, rfl⟩
    · simp only [↓reduceIte]
      rw [hInsts, hTerm]
      exact ⟨_, rfl⟩
  obtain ⟨sBlocks, hSBlocks⟩ := hBlocksEmitOk
  have hAdaptOk : ∃ body, adaptGraph language graph = .ok body := by
    unfold adaptGraph
    simp only [hNormOk, bind, Except.bind]
    rw [hSBlocks]
    exact ⟨_, rfl⟩
  obtain ⟨body, hBody⟩ := hAdaptOk
  have hEmitOk : emitUnvalidated (language := language) bir = .ok
      { body := body, additionalJobs := buildAdditionalJobs bir,
        sites := bir.sites, runtimeInput := runtimeBytes diagnostics } := by
    unfold emitUnvalidated
    simp [hBackend, hCap, hLink, hSup, hDiag, hFnLook, hBody, bind, Except.bind, pure, Except.pure]
  exact ⟨_, hEmitOk⟩

/-- Universal refusal classification for `EmitLCB.emit` on any well-formed LCB `BIR`,
matching the first three branches of `Concrete.Interfaces.PassFunctions.emitEntry.refusal`
and the fourth branch up to the `LCBEncoding` conjunct (which P6b owns). -/
theorem emit_error_of_wf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {failure : StageFailure}
    (hBackend : profile.backend = .lcb)
    (hWF : BIR.WF (language := language) bir)
    (hErr : emit (language := language) bir = .error failure) :
    (LCBInput.trapCapacity < bir.sites.size ∧
      failure = .resource (LCBInput.trapCapacityRefusal bir.sites.size)) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = true ∧
      failure = .lcbLinkingUnavailable) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = false ∧
      ∃ cause, firstUnsupported (language := language) bir = some cause ∧
        failure = .unsupportedFeature profile cause) ∨
    (¬ (LCBInput.trapCapacity < bir.sites.size) ∧
      bir.requiresLCBLinking = false ∧
      firstUnsupported (language := language) bir = none ∧
      ∃ reason output, emitUnvalidated (language := language) bir = .ok output ∧
        validateJobs output = .error reason ∧
        failure = .invalidLCBJob reason) := by
  by_cases hCap : LCBInput.trapCapacity < bir.sites.size
  · have hRef := emit_refusal_capacity (language := language) hBackend hCap
    rw [hRef] at hErr; cases Except.error.inj hErr
    exact Or.inl ⟨hCap, rfl⟩
  · cases hLink : bir.requiresLCBLinking with
    | true =>
      have hRef := emit_refusal_linking (language := language) hBackend hCap hLink
      rw [hRef] at hErr; cases Except.error.inj hErr
      exact Or.inr (Or.inl ⟨hCap, rfl, rfl⟩)
    | false =>
      cases hSup : firstUnsupported (language := language) bir with
      | some cause =>
        have hRef := emit_refusal_firstUnsupported (language := language) hBackend hCap hLink hSup
        rw [hRef] at hErr; cases Except.error.inj hErr
        exact Or.inr (Or.inr (Or.inl ⟨hCap, rfl, cause, rfl, rfl⟩))
      | none =>
        obtain ⟨output, hUnval⟩ := emitUnvalidated_ok_of_wf hBackend hWF hCap hLink hSup
        cases hVal : validateJobs output with
        | error reason =>
          have hRef := emit_refusal_invalidJob hUnval hVal
          rw [hRef] at hErr; cases Except.error.inj hErr
          exact Or.inr (Or.inr (Or.inr ⟨hCap, rfl, rfl, reason, output, hUnval, hVal, rfl⟩))
        | ok u =>
          cases u
          unfold emit at hErr
          simp [hUnval, hVal, Except.mapError, bind, Except.bind, pure, Except.pure] at hErr

/-! ## Focused kernel-checked controls -/

section Controls

private abbrev cLang : CoreLanguage := sliceLanguage slice0
private abbrev cLow : LoweringLanguage cLang.operands.Ty := sliceLoweringLanguage slice0
private abbrev cU8 : cLang.operands.Ty := cLow.intTy .u8
private abbrev cI8 : cLang.operands.Ty := cLow.intTy .i8
private abbrev cU32 : cLang.operands.Ty := cLow.intTy .u32

private def ctrlMain : FnId := ⟨⟨⟨⟨"p6a"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def ctrlHelper : FnId := ⟨⟨⟨⟨"p6a"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"helper"⟩], by decide⟩, ⟨"helper"⟩⟩

private def ctrlPath : SourcePath :=
  match SourcePath.parse (SourceText.ofBytes "src/main.con".toUTF8.data.toList) with
  | .ok p => p
  | .error _ => mainSourcePath

private def ctrlProject : Project :=
  Project.singleton ctrlPath (SourceText.ofBytes r#"fn main() -> u8 { return 255 + 1; }"#.toUTF8.data.toList)

private def ctrlOrigin0 : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 29, 30, 1, 30⟩, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def ctrlOrigin1 : Origin :=
  ⟨⟨1⟩, ⟨⟨0⟩, 25, 28, 1, 26⟩, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def ctrlProfile : Profile := ⟨.lcb, .x86_64Linux, 8, 4096⟩

private def addOvfBIR (p : Profile) (t : IntTy) (lhsBytes rhsBytes : List UInt8) : BIR cLow p :=
  let ty := cLow.intTy t
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩
      params := []
      body := [⟨ctrlOrigin0, .addWithOverflow t ⟨0⟩ ⟨1⟩ (.literal ⟨lhsBytes⟩) (.literal ⟨rhsBytes⟩)⟩]
      ending := .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨ty, .value ⟨0⟩⟩]⟩
      boundaryOrigins := [] }
  let b1 : BIRBlock cLow :=
    { id := ⟨1⟩, params := [], body := [], ending := .trap ⟨0⟩, boundaryOrigins := [] }
  let b2 : BIRBlock cLow :=
    { id := ⟨2⟩, params := [⟨ty, ⟨2⟩⟩], body := [], ending := .ret (some ⟨ty, .value ⟨2⟩⟩), boundaryOrigins := [] }
  { project := ctrlProject
    entry := ctrlMain
    signatures := [(ctrlMain, ⟨[], some ty⟩)]
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)]
    layouts := []
    sites := #[⟨.overflow, ctrlOrigin0⟩]
    mutableLocals := []
    declarations := [] }

private theorem emit_ok_of_unval_val {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {out : LCBInput profile}
    (hU : emitUnvalidated (language := language) bir = .ok out)
    (hV : validateJobs out = .ok ()) :
    emit (language := language) bir = .ok out := by
  unfold emit
  rw [hU]
  simp [hV, Except.mapError, bind, Except.bind, pure, Except.pure]

private def unwrapLCB {p : Profile} (e : Except StageFailure (LCBInput p)) : LCBInput p :=
  match e with
  | .ok o => o
  | .error _ => ⟨unreachableHelperBody, [], #[], ByteArray.empty⟩

private def ctrlBirU8 : BIR cLow ctrlProfile := addOvfBIR ctrlProfile .u8 [20] [22]
private def ctrlOutU8 : LCBInput ctrlProfile := unwrapLCB (emitUnvalidated (language := cLang) ctrlBirU8)

private def ctrlBirI8 : BIR cLow ctrlProfile := addOvfBIR ctrlProfile .i8 [127] [1]
private def ctrlOutI8 : LCBInput ctrlProfile := unwrapLCB (emitUnvalidated (language := cLang) ctrlBirI8)

private def ctrlBirLoop : BIR cLow ctrlProfile :=
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩, params := [], body := [], ending := .jump ⟨⟨0⟩, []⟩, boundaryOrigins := [] }
  { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
    mutableLocals := [], declarations := [] }
private def ctrlOutLoop : LCBInput ctrlProfile := unwrapLCB (emitUnvalidated (language := cLang) ctrlBirLoop)

private def ctrlBirSplit : BIR cLow ctrlProfile :=
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩, params := [],
      body := [⟨ctrlOrigin0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
      ending := .branch (.value ⟨1⟩) ⟨⟨1⟩, [⟨cU8, .literal ⟨[7]⟩⟩]⟩ ⟨⟨1⟩, [⟨cU8, .value ⟨0⟩⟩]⟩,
      boundaryOrigins := [] }
  let b1 : BIRBlock cLow :=
    { id := ⟨1⟩, params := [⟨cU8, ⟨2⟩⟩], body := [], ending := .ret (some ⟨cU8, .value ⟨2⟩⟩),
      boundaryOrigins := [] }
  { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1]⟩)], layouts := [], sites := #[],
    mutableLocals := [], declarations := [] }
private def ctrlOutSplit : LCBInput ctrlProfile := unwrapLCB (emitUnvalidated (language := cLang) ctrlBirSplit)

private def ctrlBirDeadHelper : BIR cLow ctrlProfile :=
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩, params := [], body := [], ending := .ret (some ⟨cU8, .literal ⟨[42]⟩⟩),
      boundaryOrigins := [] }
  let bDead : BIRBlock cLow :=
    { id := ⟨1⟩, params := [],
      body := [⟨ctrlOrigin0, .call ctrlHelper (some ⟨cU8, ⟨0⟩⟩) []⟩],
      ending := .ret (some ⟨cU8, .value ⟨0⟩⟩), boundaryOrigins := [] }
  let h0 : BIRBlock cLow :=
    { id := ⟨0⟩, params := [],
      body := [⟨ctrlOrigin1, .call ctrlMain (some ⟨cU8, ⟨0⟩⟩) []⟩],
      ending := .ret (some ⟨cU8, .value ⟨0⟩⟩), boundaryOrigins := [] }
  { project := ctrlProject, entry := ctrlMain,
    signatures := [(ctrlMain, ⟨[], some cU8⟩), (ctrlHelper, ⟨[], some cU8⟩)],
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0, bDead]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)],
    layouts := [], sites := #[], mutableLocals := [], declarations := [] }
private def ctrlOutDeadHelper : LCBInput ctrlProfile :=
  unwrapLCB (emitUnvalidated (language := cLang) ctrlBirDeadHelper)

private def ctrlBirTooManyParams : BIR cLow ctrlProfile :=
  let params : List (Sigma (SSAValue cLang.operands.Ty)) :=
    (List.range 9).map fun i => ⟨cU8, ⟨i⟩⟩
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩, params := params, body := [], ending := .ret (some ⟨cU8, .literal ⟨[42]⟩⟩),
      boundaryOrigins := [] }
  { project := ctrlProject, entry := ctrlMain,
    signatures := [(ctrlMain, ⟨List.replicate 9 cU8, some cU8⟩)],
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
    mutableLocals := [], declarations := [] }
private def ctrlOutTooManyParams : LCBInput ctrlProfile :=
  unwrapLCB (emitUnvalidated (language := cLang) ctrlBirTooManyParams)

-- Positive control: `u8` `addWithOverflow` passes `emit` (including `validateJob = .ok ()`),
-- satisfies `RuntimeMatches`, and produces exact `qbeDiagnostics`.
example :
    emit (language := cLang) ctrlBirU8 = .ok ctrlOutU8 ∧
    validateJob (LCBJob.lcbFunctionJob ctrlOutU8.body) = .ok () ∧
    ctrlOutU8.runtimeInput =
      (Backend.Runtime.qbeDiagnostics ["concrete: trap: overflow at src/main.con:1:30 (site 0)\n"]).toUTF8 := by
  have hU : emitUnvalidated (language := cLang) ctrlBirU8 = .ok ctrlOutU8 := rfl
  have hV : validateJobs ctrlOutU8 = .ok () := by decide +kernel
  exact ⟨emit_ok_of_unval_val hU hV, by decide +kernel, rfl⟩

-- Positive control: signed `i8` `addWithOverflow` is in LCB's 16-bit word profile and passes `emit`.
example :
    emit (language := cLang) ctrlBirI8 = .ok ctrlOutI8 ∧
    validateJob (LCBJob.lcbFunctionJob ctrlOutI8.body) = .ok () := by
  have hU : emitUnvalidated (language := cLang) ctrlBirI8 = .ok ctrlOutI8 := rfl
  have hV : validateJobs ctrlOutI8 = .ok () := by decide +kernel
  exact ⟨emit_ok_of_unval_val hU hV, by decide +kernel⟩

-- Positive control: zero-slot self-loop on entry inserts block-0 trampoline and passes `validateJob`.
example :
    emit (language := cLang) ctrlBirLoop = .ok ctrlOutLoop ∧
    ctrlOutLoop.body.blocks.size = 2 ∧
    validateJob (LCBJob.lcbFunctionJob ctrlOutLoop.body) = .ok () := by
  have hU : emitUnvalidated (language := cLang) ctrlBirLoop = .ok ctrlOutLoop := rfl
  have hV : validateJobs ctrlOutLoop = .ok () := by decide +kernel
  exact ⟨emit_ok_of_unval_val hU hV, rfl, by decide +kernel⟩

-- Positive control: same-target parameterized branch splits the `no` edge so `phi.incoming`
-- has two distinct canonical predecessor labels and passes `validateJob`.
example :
    emit (language := cLang) ctrlBirSplit = .ok ctrlOutSplit ∧
    ctrlOutSplit.body.blocks.size = 3 ∧
    validateJob (LCBJob.lcbFunctionJob ctrlOutSplit.body) = .ok () := by
  have hU : emitUnvalidated (language := cLang) ctrlBirSplit = .ok ctrlOutSplit := rfl
  have hV : validateJobs ctrlOutSplit = .ok () := by decide +kernel
  exact ⟨emit_ok_of_unval_val hU hV, rfl, by decide +kernel⟩

-- Positive control: unreachable helper function (with a call inside it!) and unreachable block
-- in entry are handled cleanly: `additionalJobs` gets a validated, uniquely named job.
example :
    emit (language := cLang) ctrlBirDeadHelper = .ok ctrlOutDeadHelper ∧
    List.map Prod.fst ctrlOutDeadHelper.additionalJobs = [ctrlHelper] ∧
    List.map (fun p => p.2.code.symbol.name) ctrlOutDeadHelper.additionalJobs = ["concrete_fn_0"] ∧
    validateJob (LCBJob.lcbFunctionJob ctrlOutDeadHelper.body) = .ok () ∧
    (∀ pair ∈ ctrlOutDeadHelper.additionalJobs, validateJob pair.2 = .ok ()) := by
  have hU : emitUnvalidated (language := cLang) ctrlBirDeadHelper = .ok ctrlOutDeadHelper := rfl
  have hV : validateJobs ctrlOutDeadHelper = .ok () := by decide +kernel
  refine ⟨emit_ok_of_unval_val hU hV, rfl, rfl, by decide +kernel, ?_⟩
  intro pair hPair
  cases List.mem_singleton.mp hPair
  exact helperJob_validateJob 0

-- Negative control: `u32` width exceeds LCB's 16-bit profile and is refused with exact `firstUnsupported`.
example :
    emit (language := cLang) (addOvfBIR ctrlProfile .u32 [1, 0, 0, 0] [2, 0, 0, 0]) =
      .error (.unsupportedFeature ctrlProfile (.unsupportedWidth .u32 ctrlMain .signatureResult)) := by
  rfl

-- Negative control: `.load` storage form is refused with exact `firstUnsupported`.
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [],
        body := [⟨ctrlOrigin0, .load (ty := cU8) ⟨0⟩ ⟨⟨0⟩, []⟩⟩],
        ending := .ret (some ⟨cU8, .value ⟨0⟩⟩), boundaryOrigins := [] }
    let bir : BIR cLow ctrlProfile :=
      { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
        mutableLocals := [], declarations := [] }
    emit (language := cLang) bir =
      .error (.unsupportedFeature ctrlProfile (.unsupportedStorage .load ctrlMain (.instruction ⟨0⟩ 0))) := by
  rfl

-- Negative control: linking refusal (`requiresLCBLinking`) takes precedence over `firstUnsupported`
-- (even when the entry also uses unsupported width `u32`).
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [],
        body := [⟨ctrlOrigin0, .call ctrlHelper (some ⟨cU32, ⟨0⟩⟩) []⟩],
        ending := .ret (some ⟨cU32, .value ⟨0⟩⟩), boundaryOrigins := [] }
    let h0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [], body := [], ending := .ret (some ⟨cU32, .literal ⟨[1, 0, 0, 0]⟩⟩),
        boundaryOrigins := [] }
    let bir : BIR cLow ctrlProfile :=
      { project := ctrlProject, entry := ctrlMain,
        signatures := [(ctrlMain, ⟨[], some cU32⟩), (ctrlHelper, ⟨[], some cU32⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)],
        layouts := [], sites := #[], mutableLocals := [], declarations := [] }
    emit (language := cLang) bir = .error .lcbLinkingUnavailable := by
  rfl

-- Negative control: >8 entry parameters succeeds in `emitUnvalidated` and is refused by `emit`
-- with `.invalidLCBJob (.admission (.resourceLimit "parameters" 9 8))`.
example :
    emitUnvalidated (language := cLang) ctrlBirTooManyParams = .ok ctrlOutTooManyParams ∧
    emit (language := cLang) ctrlBirTooManyParams =
      .error (.invalidLCBJob (.admission (.resourceLimit "parameters" 9 8))) := by
  have hU : emitUnvalidated (language := cLang) ctrlBirTooManyParams = .ok ctrlOutTooManyParams := rfl
  have hV : validateJobs ctrlOutTooManyParams =
      .error (.admission (.resourceLimit "parameters" 9 8)) := by
    decide +kernel
  exact ⟨hU, emit_refusal_invalidJob hU hV⟩

-- Negative control: calling `EmitLCB.emit` on an LLVM profile fails closed.
example :
    let llvmProf : Profile := ⟨.llvm, .x86_64Linux, 8, 4096⟩
    emit (language := cLang) (addOvfBIR llvmProf .u8 [20] [22]) = .error (.unsupportedProfile wrongBackendRule) := by
  rfl

end Controls

end Concrete.Backend.EmitLCB
