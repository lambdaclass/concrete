import Concrete.Interfaces.Carriers
import Concrete.Lex
import Concrete.Interfaces.Behavior
import Concrete.Backend.Observation
import Concrete.Backend.Runtime
import Concrete.Backend.LCBJob
import Concrete.Interfaces.SurfaceElaboration
import ConcreteSpec.StageGrammar

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Contracts (ProcessEvent Semantics EvidenceKind Graph)

inductive Observation where
  | returned (status : UInt8) (stderr : ByteArray)
  | trapped (status : UInt8) (kind : TrapKind) (site : FileSpan) (stderr : ByteArray)
  | startupRefused (status : UInt8) (stderr : ByteArray)
  | resourceExhausted

def Observation.stderr : Observation → ByteArray
  | .returned _ bytes | .trapped _ _ _ bytes | .startupRefused _ bytes => bytes
  | .resourceExhausted => ByteArray.empty

open Concrete.Contracts (Steps)

/-- Match representation checkpoints along target executions. The source
can choose a path that follows the target's allocation and control decisions.
A target step that takes no source steps strictly decreases the rank. -/
structure CheckpointSimulation {A B E O : Type} (s : Semantics A E) (t : Semantics B E)
    (sourceResult : A → s.State → O → Prop) (targetResult : B → t.State → O → Prop)
    (a : A) (b : B) where
  related : s.State → t.State → Prop
  rank : s.State → t.State → Nat
  targetInitial : ∀ q, t.initial b q → ∃ p, s.initial a p ∧ related p q
  targetAdvance : ∀ p q q', s.finite a p → t.finite b q → related p q → t.step b q q' →
    let atOld := fun x y => s.step a x y ∧ related y q
    (∃ before after, Steps atOld p before ∧ s.step a before after ∧ related after q') ∨
    (related p q' ∧ rank p q' < rank p q)
  targetFinish : ∀ p q o, s.finite a p → t.finite b q → related p q → targetResult b q o →
    ∃ p', Steps (fun x y => s.step a x y ∧ related y q) p p' ∧
      sourceResult a p' o ∧ s.trace a p' = t.trace b q

/-- The registry owns the function value and uses it for both execution and the proof graph. -/
structure Stage (Input Failure Output : Type) where
  run : Input → Except Failure Output
  kind : EvidenceKind
  inputInvariant : Input → Prop
  outputInvariant : Output → Prop
  success : Input → Output → Prop
  refusal : Input → Failure → Prop
  trust : Input → Output → Prop := fun _ _ => True

def Stage.Trusted {A R B : Type} (edge : Stage A R B) : Prop :=
  ∀ a b, edge.inputInvariant a → Graph edge.run a b → edge.trust a b

def Stage.Validated {A R B : Type} (edge : Stage A R B) : Prop :=
  (∀ a b, edge.inputInvariant a → Graph edge.run a b →
    edge.outputInvariant b ∧ edge.success a b) ∧
  (∀ a r, edge.inputInvariant a → edge.run a = .error r → edge.refusal a r)

def Stage.Checks {A R B : Type} (edge : Stage A R B) : Prop :=
  edge.Validated ∧ edge.Trusted

def Stage.Compatible {A R B S C : Type} (left : Stage A R B) (right : Stage B S C) : Prop :=
  ∀ b, left.outputInvariant b → right.inputInvariant b

/-- Composition uses heterogeneous stage entries directly, including their refusal channels. -/
inductive Pipeline : Type → Type → Type 1 where
  | done {A : Type} : Pipeline A A
  | next {A R B C : Type} (edge : Stage A R B)
      (rest : Pipeline B C) : Pipeline A C

def Pipeline.run {A C : Type} (pipeline : Pipeline A C) (input : A) :
    Except (Σ R : Type, R) C :=
  match pipeline with
  | .done => .ok input
  | .next stage rest =>
    match stage.run input with
    | .error failure => .error ⟨_, failure⟩
    | .ok output => rest.run output

inductive Pipeline.Validated : {A B : Type} → Pipeline A B → Prop where
  | done {A : Type} : Validated (.done : Pipeline A A)
  | last {A R B : Type} {edge : Stage A R B} : edge.Validated → Validated (.next edge .done)
  | next {A R B S C D : Type} {edge : Stage A R B} {after : Stage B S C}
      {rest : Pipeline C D} : edge.Validated → edge.Compatible after →
      Validated (.next after rest) → Validated (.next edge (.next after rest))

def Pipeline.Trusted {A B : Type} : Pipeline A B → Prop
  | .done => True
  | .next edge rest => edge.Trusted ∧ rest.Trusted

def Pipeline.Sound {A B : Type} (pipeline : Pipeline A B) : Prop :=
  pipeline.Validated ∧ pipeline.Trusted

def Pipeline.Ready {A B : Type} (pipeline : Pipeline A B) (input : A) : Prop :=
  match pipeline with
  | .done => True
  | .next edge _ => edge.inputInvariant input

/-- A successful run retains the exact intermediate values, their invariants,
and the verified and trusted correspondence obligations at each edge. -/
def Pipeline.Corresponds {A B : Type} (pipeline : Pipeline A B) (input : A) (output : B) : Prop :=
  match pipeline with
  | .done => input = output
  | .next edge rest =>
    ∃ middle, Graph edge.run input middle ∧ edge.outputInvariant middle ∧
      edge.success input middle ∧ edge.trust input middle ∧ rest.Corresponds middle output

theorem Pipeline.compose {A B : Type} {pipeline : Pipeline A B}
    (validated : pipeline.Validated) (trusted : pipeline.Trusted)
    {input : A} {output : B} (ready : pipeline.Ready input)
    (ran : pipeline.run input = .ok output) : pipeline.Corresponds input output := by
  induction validated with
  | done => exact Except.ok.inj ran
  | @last _ _ _ edge proof =>
    change (match edge.run input with
      | .error failure => Except.error (⟨_, failure⟩ : Σ R : Type, R)
      | .ok middle => Except.ok middle) = Except.ok output at ran
    cases h : edge.run input with
    | error failure => rw [h] at ran; cases ran
    | ok middle =>
      have same : middle = output := by rw [h] at ran; exact Except.ok.inj ran
      subst output
      have checked := proof.1 _ _ ready h
      exact ⟨middle, h, checked.1, checked.2, trusted.1 _ _ ready h, rfl⟩
  | @next _ _ _ _ _ _ edge after rest proof compatible _ ih =>
    change (match edge.run input with
      | .error failure => Except.error (⟨_, failure⟩ : Σ R : Type, R)
      | .ok middle => (Pipeline.next after rest).run middle) = .ok output at ran
    cases h : edge.run input with
    | error failure => rw [h] at ran; cases ran
    | ok middle =>
      rw [h] at ran
      have checked := proof.1 _ _ ready h
      exact ⟨middle, h, checked.1, checked.2, trusted.1 _ _ ready h,
        ih trusted.2 (compatible middle checked.1) ran⟩

/-- Boundary compatibility between two sequentially composed pipelines: when both are
non-empty, the final stage of `left` establishes the input invariant of the first stage
of `right`. An empty pipeline (`.done`) imposes no extra boundary obligation. -/
def Pipeline.Compatible {A B C : Type} : Pipeline A B → Pipeline B C → Prop
  | .done, _ => True
  | _, .done => True
  | .next edge .done, .next after _ => edge.Compatible after
  | .next _ (.next nextEdge rest), right => Pipeline.Compatible (.next nextEdge rest) right

/-- Sequential composition of two heterogeneous stage pipelines. -/
def pipelineAppend {A B C : Type} : Pipeline A B → Pipeline B C → Pipeline A C
  | .done, right => right
  | .next edge rest, right => .next edge (pipelineAppend rest right)

theorem pipelineAppend_done_left {A B : Type} (right : Pipeline A B) :
    pipelineAppend .done right = right := rfl

theorem pipelineAppend_done_right {A B : Type} (left : Pipeline A B) :
    pipelineAppend left .done = left := by
  induction left with
  | done => rfl
  | next edge rest ih => simp only [pipelineAppend, ih]

theorem pipelineAppend_assoc {A B C D : Type}
    (first : Pipeline A B) (second : Pipeline B C) (third : Pipeline C D) :
    pipelineAppend (pipelineAppend first second) third =
      pipelineAppend first (pipelineAppend second third) := by
  induction first with
  | done => rfl
  | next edge rest ih => simp only [pipelineAppend, ih]

/-- Executing `pipelineAppend left right` is exact sequential monadic bind of `left.run`
followed by `right.run`. -/
theorem pipelineAppend_run {A B C : Type}
    (left : Pipeline A B) (right : Pipeline B C) (input : A) :
    (pipelineAppend left right).run input = (left.run input).bind right.run := by
  induction left with
  | done => rfl
  | next edge rest ih =>
    simp only [pipelineAppend, Pipeline.run]
    cases edge.run input with
    | error failure => rfl
    | ok output => exact ih right output

theorem pipelineAppend_run_ok_iff {A B C : Type}
    (left : Pipeline A B) (right : Pipeline B C) (input : A) (output : C) :
    (pipelineAppend left right).run input = .ok output ↔
      ∃ middle, left.run input = .ok middle ∧ right.run middle = .ok output := by
  rw [pipelineAppend_run]
  cases hLeft : left.run input with
  | error failure => simp [Except.bind]
  | ok middle => simp [Except.bind]

theorem Pipeline.corresponds_run {A B : Type} {pipeline : Pipeline A B} {input : A} {output : B}
    (corr : pipeline.Corresponds input output) : pipeline.run input = .ok output := by
  induction pipeline with
  | done => exact congrArg Except.ok corr
  | next edge rest ih =>
    obtain ⟨middle, hGraph, _, _, _, hRest⟩ := corr
    unfold Graph at hGraph
    simp only [Pipeline.run, hGraph]
    exact ih hRest

theorem pipelineAppend_validated {A B C : Type} {left : Pipeline A B} {right : Pipeline B C}
    (hLeft : left.Validated) (hRight : right.Validated) (hCompat : left.Compatible right) :
    (pipelineAppend left right).Validated := by
  induction hLeft with
  | done => exact hRight
  | @last _ _ _ edge hEdge =>
    cases right with
    | done => exact .last hEdge
    | next after rest => exact .next hEdge hCompat hRight
  | @next _ _ _ _ _ _ edge after rest hEdge hEdgeCompat _ ih =>
    cases right with
    | done =>
      have hRest := ih .done trivial
      exact .next hEdge hEdgeCompat hRest
    | next rEdge rRest =>
      exact .next hEdge hEdgeCompat (ih hRight hCompat)

theorem pipelineAppend_trusted {A B C : Type} (left : Pipeline A B) (right : Pipeline B C) :
    (pipelineAppend left right).Trusted ↔ left.Trusted ∧ right.Trusted := by
  induction left with
  | done => simp [pipelineAppend, Pipeline.Trusted]
  | next edge rest ih =>
    simp only [pipelineAppend, Pipeline.Trusted, ih, and_assoc]

theorem pipelineAppend_sound {A B C : Type} {left : Pipeline A B} {right : Pipeline B C}
    (hLeft : left.Sound) (hRight : right.Sound) (hCompat : left.Compatible right) :
    (pipelineAppend left right).Sound :=
  ⟨pipelineAppend_validated hLeft.1 hRight.1 hCompat,
    (pipelineAppend_trusted left right).mpr ⟨hLeft.2, hRight.2⟩⟩

/-- Correspondence across `pipelineAppend left right` decomposes at the exact intermediate
value `middle : B` handed from `left` to `right`. -/
theorem pipelineAppend_corresponds {A B C : Type}
    (left : Pipeline A B) (right : Pipeline B C) (input : A) (output : C) :
    (pipelineAppend left right).Corresponds input output ↔
      ∃ middle, left.Corresponds input middle ∧ right.Corresponds middle output := by
  induction left with
  | done =>
    simp only [pipelineAppend, Pipeline.Corresponds]
    constructor
    · intro h; exact ⟨input, rfl, h⟩
    · rintro ⟨middle, rfl, h⟩; exact h
  | next edge rest ih =>
    simp only [pipelineAppend, Pipeline.Corresponds]
    constructor
    · rintro ⟨stepOut, hGraph, hInv, hSucc, hTrust, hRest⟩
      obtain ⟨middle, hLeft, hRight⟩ := (ih right stepOut).mp hRest
      exact ⟨middle, ⟨stepOut, hGraph, hInv, hSucc, hTrust, hLeft⟩, hRight⟩
    · rintro ⟨middle, ⟨stepOut, hGraph, hInv, hSucc, hTrust, hLeft⟩, hRight⟩
      exact ⟨stepOut, hGraph, hInv, hSucc, hTrust, (ih right stepOut).mpr ⟨middle, hLeft, hRight⟩⟩

theorem pipelineAppend_corresponds_of_run {A B C : Type}
    {left : Pipeline A B} {right : Pipeline B C}
    (hLeft : left.Sound) (hRight : right.Sound) (hCompat : left.Compatible right)
    {input : A} {output : C}
    (ready : (pipelineAppend left right).Ready input)
    (ran : (pipelineAppend left right).run input = .ok output) :
    ∃ middle, left.run input = .ok middle ∧ right.run middle = .ok output ∧
      left.Corresponds input middle ∧ right.Corresponds middle output := by
  have hSound := pipelineAppend_sound hLeft hRight hCompat
  have hCorr := Pipeline.compose hSound.1 hSound.2 ready ran
  obtain ⟨middle, hL, hR⟩ := (pipelineAppend_corresponds left right input output).mp hCorr
  exact ⟨middle, Pipeline.corresponds_run hL, Pipeline.corresponds_run hR, hL, hR⟩

theorem Pipeline.compatible_append_right_of_next {A B R C D : Type}
    (first : Pipeline A B) (edge : Stage B R C) (rest : Pipeline C D) :
    first.Compatible (pipelineAppend (.next edge .done) rest) ↔
      first.Compatible (.next edge .done) := by
  induction first with
  | done => rfl
  | next _ tail ih =>
    cases tail with
    | done => rfl
    | next _ _ => exact ih edge

theorem Pipeline.compatible_append_left_of_next {A B R C D E : Type}
    (first : Pipeline A B) (edge : Stage B R C) (rest : Pipeline C D) (after : Pipeline D E) :
    (pipelineAppend first (.next edge rest)).Compatible after ↔
      (Pipeline.next edge rest).Compatible after := by
  induction first with
  | done => rfl
  | next _ tail ih =>
    cases tail with
    | done => cases after <;> rfl
    | next _ _ => cases after <;> exact ih edge

private def fileSpan (span : ConcreteSpec.Surface0.Span) : FileSpan :=
  ⟨span.fileId, span.start, span.stop, span.line, span.column⟩

private def decodedValue : Concrete.Lex.LosslessValue → Option TokenValue
  | .parsed (.fixed kind _) => if h : Terminal.HasPayload kind = false then some (.fixed kind h) else none
  | .parsed (.name name) => some (.name name)
  | .parsed (.integer value radix suffix) => some (.integer value radix suffix)
  | .stringLiteral text => some (.stringLiteral text.toUTF8.data.toList)
  | .byteLiteral byte => some (.byteLiteral byte)
  | .label name => some (.label ⟨name⟩)

private def referencePiece : Concrete.Lex.Piece → Option (FileSpan × Option TokenValue)
  | .token value span => (decodedValue value).map fun token => (fileSpan span, some token)
  | .whitespace span | .lineComment span => some (fileSpan span, none)

private def retainedPieces (stream : TokenStream) : List (FileSpan × Option TokenValue) :=
  (stream.tokens.toList.zipIdx.flatMap fun (token, index) =>
    (stream.trivia[index]?.getD #[]).toList.map (fun piece => (piece.span, none)) ++
      [(token.decoded.span, some token.decoded.value)]) ++
    (stream.trivia[stream.tokens.size]?.getD #[]).toList.map (fun piece => (piece.span, none))

/-- The proved lossless lexer gives the fixed lexical meaning of the retained bytes,
including token boundaries, trivia and the full decoded payloads. -/
def Lexes (source : Source) (stream : TokenStream) : Prop :=
  stream.source = source ∧ stream.parserView.WF ∧
  ∃ file, source.project.files[source.entryFile.index]? = some file ∧
    ∃ bytes, reconstruct source.entryFile stream.tokens stream.trivia = some bytes ∧
      bytes.data.toList = file.source.toBytes ∧
      ∃ reference, Concrete.Lex.lexLossless bytes source.entryFile = .ok reference ∧
        reference.pieces.mapM referencePiece = some (retainedPieces stream)

def LexRefuses (source : Source) (failure : LexRefusal) : Prop :=
  match source.project.fileAt source.entryFile with
  | none => failure = .missingFile source.entryFile
  | some file =>
    let bytes : ByteArray := ⟨file.source.toBytes.toArray⟩
    match failure with
    | .missingFile _ => False
    | .source cause => Concrete.Lex.lexLossless bytes source.entryFile = .error (.source cause)

/-- Each slice contributes a repository rule over that member of the AST and Core
families. Slice 0 uses the canonical injections of the shared carriers. -/
def SliceElaboration (slice : Slice) (project : Project)
    (ast : SurfaceAST ConcreteSpec.StageGrammar.sliceASTExtra slice)
    (result : Except ElabRefusal (AdmissionInput (sliceLanguage slice))) : Prop :=
  if h : slice = slice0 then
    match ast with
    | .inl tree => ∃ program, Concrete.Elab.surfaceProgram tree = some program ∧
        .ok (.pending project (.zero program) (congrArg sliceLanguage h)) = result
    | .inr _ => False
  else result = .error (.unsupportedSlice slice)

/-- Origin maps enumerate emitted copies as well as discharged and residual operations. -/
def PreservesOrigins (source target : List Origin) : Prop :=
  (∀ o ∈ target, o ∈ source) ∧ (∀ o ∈ source, o ∈ target)

def CFG.origins {P I T : Type} (graph : CFG P (LocatedOp I) T)
    (termOrigins : T → List Origin) : List Origin :=
  graph.blocks.toList.flatMap fun block =>
    block.body.map LocatedOp.origin ++ block.boundaryOrigins ++ termOrigins block.ending

def Terminator.origins {A C : Type} : Terminator A C → List Origin
  | .trap _ origin => [origin]
  | _ => []

def Read.identity {Ty : Type} {ty : Ty} : Read Ty (Literal Ty) ty → OperandRead
  | .literal value => .literal value.encoding
  | .copy place => .copy place.root place.path
  | .move place => .move place.root place.path

def Args.identities {Ty : Type} {types : List Ty} :
    Args (Read Ty (Literal Ty)) types → List (Ty × OperandRead)
  | .nil => []
  | @Args.cons _ _ ty _ read rest => (ty, read.identity) :: rest.identities

structure MIR.Occurrence (Ty : Type) where
  fn : FnId
  block : BlockId
  index : Nat
  origin : Origin
  opcode : Opcode
  operands : List (Ty × OperandRead)
  checks : List OperationCheck

def MIR.operations {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List (MIR.Occurrence Ty) :=
  mir.functions.flatMap fun (fn, _, graph) =>
    graph.blocks.toList.flatMap fun block =>
      block.body.zipIdx.filterMap fun (inst, index) => match inst.instruction with
        | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
          some ⟨fn, block.id, index, inst.origin, .operator opcode,
            args.identities ++ addresses.map (fun p => (p.1, .address p.2.root p.2.path)), checks⟩
        | .call callee _ args checks =>
          some ⟨fn, block.id, index, inst.origin, .call callee,
            args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
        | _ => none

def MIR.Occurrence.Justifies {language : CoreLanguage}
    (op : MIR.Occurrence language.operands.Ty) (check : OperationCheck)
    (d : Discharge language) : Prop :=
  check.disposition = .discharged ∧ op.origin.operation = d.operation ∧
  op.origin.evidenceKey = d.key ∧ check.discriminator = d.obligation.discriminator ∧
  check.trap = d.trap ∧ op.operands = d.operands

/-- Enumerate actual emitted operations. Each Core condition is handled at that occurrence,
and every discharge is consumed once at an occurrence with exactly the same typed reads. -/
def ConsumesEvidence {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (core : CoreInput language) (discharges : Discharges language)
    (mir : MIR lang profile) : Prop :=
  ((mir.operations.map fun op => (op.fn, op.block, op.index)).Nodup) ∧
  (∀ site opcode conditions, core.machineProgram.ConditionsAt site opcode conditions → conditions ≠ [] →
    ∃ op ∈ mir.operations, op.origin.operation = site ∧ op.opcode = opcode) ∧
  (∀ op ∈ mir.operations,
    ∃ conditions, core.machineProgram.ConditionsAt op.origin.operation op.opcode conditions ∧
      (op.checks.map OperationCheck.discriminator).Nodup ∧
      conditions = op.checks.map (fun check => ⟨check.discriminator, check.trap, op.operands⟩) ∧
      (∀ callee, op.opcode = .call callee →
        ∃ args, (op.origin.operation, .call callee args) ∈ core.machineProgram.operations ∧
          args.mapM language.argumentRead = some op.operands) ∧
      ∀ check ∈ op.checks, check.disposition = .discharged →
        ∃ d, discharges.Contains op.origin.operation d ∧ op.Justifies check d) ∧
  (∀ site d, discharges.Contains site d →
    ∃ op ∈ mir.operations, ∃ check ∈ op.checks, op.Justifies check d ∧
      ∀ other ∈ mir.operations, ∀ c ∈ other.checks, other.Justifies c d →
        other = op ∧ c = check) ∧ core.ArgumentsWF

def MIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) : List Origin :=
  mir.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def Memory.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : List Origin :=
  memory.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def SSA.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : List Origin :=
  ssa.functions.flatMap fun fn => fn.2.2.origins Terminator.origins

def BIR.origins {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : List Origin :=
  bir.functions.flatMap fun fn => fn.2.2.origins fun
    | .trap slot => (bir.sites[slot.index]?.map TrapEntry.origin).toList
    | _ => []

def BIR.TrapSlotsWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ bir.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ slot,
    block.ending = .trap slot → ∃ entry, bir.sites[slot.index]? = some entry

/-- Every materialized root gets the spec layout. Virtual roots need no byte allocation. -/
def LayoutsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (layout : LayoutOf Ty) (memory : Memory lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ memory.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes, AList.lookup memory.storage fn = some classes ∧
        (AList.lookup classes place = some .virtual ∨
          (AList.lookup classes place = some .materialized ∧
            ∃ slots description, AList.lookup memory.layouts fn = some slots ∧
              AList.lookup slots place = some description ∧
              layout memory.declarations profile.memoryTarget ty = some description))) ∧
  ∀ fn slots place description, (fn, slots) ∈ memory.layouts → (place, description) ∈ slots →
    ∃ locals graph ty classes, (fn, locals, graph) ∈ memory.functions ∧ (place, ty) ∈ locals ∧
      AList.lookup memory.storage fn = some classes ∧
      AList.lookup classes place = some .materialized ∧ layout memory.declarations profile.memoryTarget ty = some description

def MIR.Escapes {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (fn : FnId) (place : PlaceId) : Prop :=
  ∃ op ∈ mir.operations, op.fn = fn ∧
    ∃ ty path, (ty, .address place path) ∈ op.operands

def Memory.SignatureMatches {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) (fn : FnId) (graph : MemoryGraph lang) : Prop :=
  ∃ signature, AList.lookup memory.signatures fn = some signature ∧
    (∃ entry ∈ graph.blocks.toList, entry.id = graph.entry ∧
      entry.params.map Sigma.fst = signature.parameters) ∧
    ∀ block ∈ graph.blocks.toList,
      (match block.ending with
      | .ret operand => operand.map Sigma.fst = signature.result
      | .invalid => False
      | _ => True) ∧
      ∀ op ∈ block.body, match op.instruction with
        | .call callee destination args _ =>
          ∃ target, AList.lookup memory.signatures callee = some target ∧
            args.map Sigma.fst = target.parameters ∧ destination.map Sigma.fst = target.result
        | _ => True

def Memory.LayoutsWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : Prop :=
  ∀ table ∈ memory.layouts, ∀ slot ∈ table.2, slot.2.WellFormed profile.memoryTarget

def Memory.TablesWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) : Prop :=
  (List.map Prod.fst memory.functions).Nodup ∧ (List.map Prod.fst memory.signatures).Nodup ∧
  (List.map Prod.fst memory.storage).Nodup ∧ (List.map Prod.fst memory.layouts).Nodup ∧
  (∀ fn classes, (fn, classes) ∈ memory.storage → (List.map Prod.fst classes).Nodup) ∧
  (∀ fn layouts, (fn, layouts) ∈ memory.layouts → (List.map Prod.fst layouts).Nodup) ∧
  ∀ fn locals graph, (fn, locals, graph) ∈ memory.functions →
    (List.map Prod.fst locals).Nodup ∧ graph.WF Terminator.successors Sigma.fst Sigma.fst ∧
    memory.SignatureMatches fn graph

def PromotionSound {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ∃ classes storage, AList.lookup memory.storage fn = some classes ∧
        AList.lookup classes place = some storage ∧
        (storage = .virtual ↔ (language.scalarType ty).isSome ∧ ¬ mir.Escapes fn place)) ∧
  ∀ fn classes place, (fn, classes) ∈ memory.storage → (place, .virtual) ∈ classes →
    ∃ ty source sourceGraph target targetGraph,
      (fn, source, sourceGraph) ∈ mir.functions ∧ (place, ty) ∈ source ∧
      (fn, target, targetGraph) ∈ memory.functions ∧ (place, ty) ∈ target ∧
      (language.scalarType ty).isSome ∧ ¬ mir.Escapes fn place

/-- Every root in `mir` that must be materialized on `profile` (either non-scalar or
address-taken) admits a well-formed physical layout in `mir.declarations` under the
single geometry authority `Layout.WellFormed`. -/
def Materializable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (mir : MIR lang profile) : Prop :=
  ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ place ty, (place, ty) ∈ locals →
      ((language.scalarType ty).isNone ∨ mir.Escapes fn place) →
        ∃ d, language.layout mir.declarations profile.memoryTarget ty = some d ∧
          d.WellFormed profile.memoryTarget = true

private theorem alist_lookup_mem_local {κ α : Type} [DecidableEq κ] {l : AList κ α} {k : κ} {v : α}
    (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq
      subst heq
      cases Option.some.inj h
      exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_of_mem_local {κ α : Type} [DecidableEq κ] {l : AList κ α}
    (hNodup : (List.map Prod.fst l).Nodup) {k : κ} {v : α} (hMem : (k, v) ∈ l) :
    AList.lookup l k = some v := by
  induction l with
  | nil => cases hMem
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    rw [List.map_cons, List.nodup_cons] at hNodup
    simp only [AList.lookup]
    cases hMem with
    | head =>
      simp only [↓reduceIte]
    | tail _ hTail =>
      split
      · rename_i heq
        subst heq
        exact absurd (List.mem_map.mpr ⟨(k', v), hTail, rfl⟩) hNodup.1
      · exact ih hNodup.2 hTail

/-- Function-keyed typed root lookup in `mir` (polymorphic in the type carrier). -/
def MIR.rootTypeAt {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) (fn : FnId) (place : PlaceId) : Option Ty :=
  (AList.lookup mir.functions fn).bind fun entry => AList.lookup entry.1 place

/-- Function-keyed typed root lookup in `memory` (polymorphic in the type carrier). -/
def Memory.rootTypeAt {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) (fn : FnId) (place : PlaceId) : Option Ty :=
  (AList.lookup memory.functions fn).bind fun entry => AList.lookup entry.1 place

/-- Function-keyed typed root inventory equality in both directions under unique keys on
both sides: `mir` and `memory` have duplicate-free function keys and duplicate-free
per-function local root keys, declare the exact same set of functions, and declare the
exact same typed local roots for every function (so lowering cannot drop, add, or retype
any function or root). -/
def RetainsTypedRoots {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  (List.map Prod.fst mir.functions).Nodup ∧
  (List.map Prod.fst memory.functions).Nodup ∧
  (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions → (List.map Prod.fst locals).Nodup) ∧
  (∀ fn locals graph, (fn, locals, graph) ∈ memory.functions → (List.map Prod.fst locals).Nodup) ∧
  (∀ fn srcLocals srcGraph, (fn, srcLocals, srcGraph) ∈ mir.functions →
    ∃ dstLocals dstGraph, (fn, dstLocals, dstGraph) ∈ memory.functions ∧
      ∀ place ty, (place, ty) ∈ srcLocals ↔ (place, ty) ∈ dstLocals) ∧
  (∀ fn dstLocals dstGraph, (fn, dstLocals, dstGraph) ∈ memory.functions →
    ∃ srcLocals srcGraph, (fn, srcLocals, srcGraph) ∈ mir.functions ∧
      ∀ place ty, (place, ty) ∈ dstLocals ↔ (place, ty) ∈ srcLocals)

theorem RetainsTypedRoots.mir_fns_nodup {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) :
    (List.map Prod.fst mir.functions).Nodup :=
  h.1

theorem RetainsTypedRoots.memory_fns_nodup {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) :
    (List.map Prod.fst memory.functions).Nodup :=
  h.2.1

theorem RetainsTypedRoots.mir_locals_nodup {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) {fn srcLocals srcGraph}
    (hMem : (fn, srcLocals, srcGraph) ∈ mir.functions) :
    (List.map Prod.fst srcLocals).Nodup :=
  h.2.2.1 fn srcLocals srcGraph hMem

theorem RetainsTypedRoots.memory_locals_nodup {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) {fn dstLocals dstGraph}
    (hMem : (fn, dstLocals, dstGraph) ∈ memory.functions) :
    (List.map Prod.fst dstLocals).Nodup :=
  h.2.2.2.1 fn dstLocals dstGraph hMem

theorem RetainsTypedRoots.forward {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) {fn srcLocals srcGraph}
    (hMem : (fn, srcLocals, srcGraph) ∈ mir.functions) :
    ∃ dstLocals dstGraph, (fn, dstLocals, dstGraph) ∈ memory.functions ∧
      ∀ place ty, (place, ty) ∈ srcLocals ↔ (place, ty) ∈ dstLocals :=
  h.2.2.2.2.1 fn srcLocals srcGraph hMem

theorem RetainsTypedRoots.backward {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) {fn dstLocals dstGraph}
    (hMem : (fn, dstLocals, dstGraph) ∈ memory.functions) :
    ∃ srcLocals srcGraph, (fn, srcLocals, srcGraph) ∈ mir.functions ∧
      ∀ place ty, (place, ty) ∈ dstLocals ↔ (place, ty) ∈ srcLocals :=
  h.2.2.2.2.2 fn dstLocals dstGraph hMem

theorem RetainsTypedRoots.fn_mem_iff {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) (fn : FnId) :
    fn ∈ List.map Prod.fst mir.functions ↔ fn ∈ List.map Prod.fst memory.functions := by
  constructor
  · intro hIn
    obtain ⟨⟨fn', srcLocals, srcGraph⟩, hMem, hEq⟩ := List.mem_map.mp hIn
    dsimp only at hEq
    subst hEq
    obtain ⟨dstLocals, dstGraph, hDst, _⟩ := h.forward hMem
    exact List.mem_map.mpr ⟨(fn', dstLocals, dstGraph), hDst, rfl⟩
  · intro hIn
    obtain ⟨⟨fn', dstLocals, dstGraph⟩, hMem, hEq⟩ := List.mem_map.mp hIn
    dsimp only at hEq
    subst hEq
    obtain ⟨srcLocals, srcGraph, hSrc, _⟩ := h.backward hMem
    exact List.mem_map.mpr ⟨(fn', srcLocals, srcGraph), hSrc, rfl⟩

theorem RetainsTypedRoots.lookup_isSome_iff {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) (fn : FnId) :
    (AList.lookup mir.functions fn).isSome ↔ (AList.lookup memory.functions fn).isSome := by
  constructor
  · intro hSome
    cases hLk : AList.lookup mir.functions fn with
    | none => rw [hLk] at hSome; cases hSome
    | some entry =>
      obtain ⟨srcLocals, srcGraph⟩ := entry
      have hMem := alist_lookup_mem_local hLk
      obtain ⟨dstLocals, dstGraph, hDst, _⟩ := h.forward hMem
      rw [alist_lookup_of_mem_local h.memory_fns_nodup hDst]
      rfl
  · intro hSome
    cases hLk : AList.lookup memory.functions fn with
    | none => rw [hLk] at hSome; cases hSome
    | some entry =>
      obtain ⟨dstLocals, dstGraph⟩ := entry
      have hMem := alist_lookup_mem_local hLk
      obtain ⟨srcLocals, srcGraph, hSrc, _⟩ := h.backward hMem
      rw [alist_lookup_of_mem_local h.mir_fns_nodup hSrc]
      rfl

theorem RetainsTypedRoots.rootTypeAt_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (h : RetainsTypedRoots mir memory) (fn : FnId) (place : PlaceId) :
    mir.rootTypeAt fn place = memory.rootTypeAt fn place := by
  unfold MIR.rootTypeAt Memory.rootTypeAt
  cases hMir : AList.lookup mir.functions fn with
  | none =>
    cases hMem : AList.lookup memory.functions fn with
    | none => rfl
    | some dstEntry =>
      have hSome : (AList.lookup mir.functions fn).isSome :=
        (h.lookup_isSome_iff fn).mpr (by rw [hMem]; rfl)
      rw [hMir] at hSome
      cases hSome
  | some srcEntry =>
    obtain ⟨srcLocals, srcGraph⟩ := srcEntry
    have hSrcMem := alist_lookup_mem_local hMir
    obtain ⟨dstLocals, dstGraph, hDstMem, hEquiv⟩ := h.forward hSrcMem
    rw [alist_lookup_of_mem_local h.memory_fns_nodup hDstMem]
    simp only [Option.bind_some]
    cases hSrcLk : AList.lookup srcLocals place with
    | none =>
      cases hDstLk : AList.lookup dstLocals place with
      | none => rfl
      | some ty =>
        have hInSrc : (place, ty) ∈ srcLocals := (hEquiv place ty).mpr (alist_lookup_mem_local hDstLk)
        have hEq := alist_lookup_of_mem_local (h.mir_locals_nodup hSrcMem) hInSrc
        rw [hSrcLk] at hEq
        cases hEq
    | some ty =>
      have hInDst : (place, ty) ∈ dstLocals := (hEquiv place ty).mp (alist_lookup_mem_local hSrcLk)
      exact (alist_lookup_of_mem_local (h.memory_locals_nodup hDstMem) hInDst).symm

/-- Constructive equivalence between `RetainsTypedRoots` and exact equality of function
domains and function-keyed typed root lookups under unique keys on both sides. -/
theorem RetainsTypedRoots.iff_lookup {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (mir : MIR lang profile) (memory : Memory lang profile) :
    RetainsTypedRoots mir memory ↔
      (List.map Prod.fst mir.functions).Nodup ∧
      (List.map Prod.fst memory.functions).Nodup ∧
      (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions → (List.map Prod.fst locals).Nodup) ∧
      (∀ fn locals graph, (fn, locals, graph) ∈ memory.functions → (List.map Prod.fst locals).Nodup) ∧
      (∀ fn, (AList.lookup mir.functions fn).isSome ↔ (AList.lookup memory.functions fn).isSome) ∧
      (∀ fn place, mir.rootTypeAt fn place = memory.rootTypeAt fn place) := by
  constructor
  · intro h
    exact ⟨h.mir_fns_nodup, h.memory_fns_nodup,
      fun fn locals graph hm => h.mir_locals_nodup hm,
      fun fn locals graph hm => h.memory_locals_nodup hm,
      h.lookup_isSome_iff, h.rootTypeAt_eq⟩
  · rintro ⟨hMirFns, hMemFns, hMirLocals, hMemLocals, hDom, hRootEq⟩
    refine ⟨hMirFns, hMemFns, hMirLocals, hMemLocals, ?_, ?_⟩
    · intro fn srcLocals srcGraph hSrcMem
      have hSrcLk := alist_lookup_of_mem_local hMirFns hSrcMem
      have hMemSome : (AList.lookup memory.functions fn).isSome :=
        (hDom fn).mp (by rw [hSrcLk]; rfl)
      cases hMemLk : AList.lookup memory.functions fn with
      | none => rw [hMemLk] at hMemSome; cases hMemSome
      | some dstEntry =>
        obtain ⟨dstLocals, dstGraph⟩ := dstEntry
        have hDstMem := alist_lookup_mem_local hMemLk
        refine ⟨dstLocals, dstGraph, hDstMem, fun place ty => ?_⟩
        have hEq := hRootEq fn place
        unfold MIR.rootTypeAt Memory.rootTypeAt at hEq
        rw [hSrcLk, hMemLk] at hEq
        simp only [Option.bind_some] at hEq
        constructor
        · intro hIn
          have hSL := alist_lookup_of_mem_local (hMirLocals fn srcLocals srcGraph hSrcMem) hIn
          rw [hSL] at hEq
          exact alist_lookup_mem_local hEq.symm
        · intro hIn
          have hDL := alist_lookup_of_mem_local (hMemLocals fn dstLocals dstGraph hDstMem) hIn
          rw [hDL] at hEq
          exact alist_lookup_mem_local hEq
    · intro fn dstLocals dstGraph hDstMem
      have hMemLk := alist_lookup_of_mem_local hMemFns hDstMem
      have hMirSome : (AList.lookup mir.functions fn).isSome :=
        (hDom fn).mpr (by rw [hMemLk]; rfl)
      cases hSrcLk : AList.lookup mir.functions fn with
      | none => rw [hSrcLk] at hMirSome; cases hMirSome
      | some srcEntry =>
        obtain ⟨srcLocals, srcGraph⟩ := srcEntry
        have hSrcMem := alist_lookup_mem_local hSrcLk
        refine ⟨srcLocals, srcGraph, hSrcMem, fun place ty => ?_⟩
        have hEq := hRootEq fn place
        unfold MIR.rootTypeAt Memory.rootTypeAt at hEq
        rw [hSrcLk, hMemLk] at hEq
        simp only [Option.bind_some] at hEq
        constructor
        · intro hIn
          have hDL := alist_lookup_of_mem_local (hMemLocals fn dstLocals dstGraph hDstMem) hIn
          rw [hDL] at hEq
          exact alist_lookup_mem_local hEq
        · intro hIn
          have hSL := alist_lookup_of_mem_local (hMirLocals fn srcLocals srcGraph hSrcMem) hIn
          rw [hSL] at hEq
          exact alist_lookup_mem_local hEq.symm

/-- Any lowering preserving the ordered `(fn, locals)` inventory (as production `prepareGraph`
and `lowerGraph` do) satisfies `RetainsTypedRoots` under unique source keys. -/
theorem RetainsTypedRoots.of_inventories_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (hInv : List.map (fun e => (e.1, e.2.1)) memory.functions =
      List.map (fun e => (e.1, e.2.1)) mir.functions)
    (hSrcFns : (List.map Prod.fst mir.functions).Nodup)
    (hSrcLocals : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      (List.map Prod.fst locals).Nodup) :
    RetainsTypedRoots mir memory := by
  have hMemFnsEq : List.map Prod.fst memory.functions = List.map Prod.fst mir.functions := by
    have h := congrArg (List.map Prod.fst) hInv
    simpa only [List.map_map, Function.comp_def] using h
  refine ⟨hSrcFns, hMemFnsEq ▸ hSrcFns, hSrcLocals, ?_, ?_, ?_⟩
  · intro fn dstLocals dstGraph hDstMem
    have hPair : (fn, dstLocals) ∈ List.map (fun e => (e.1, e.2.1)) mir.functions := by
      rw [← hInv]
      exact List.mem_map.mpr ⟨(fn, dstLocals, dstGraph), hDstMem, rfl⟩
    obtain ⟨⟨srcFn, srcLocals, srcGraph⟩, hSrcMem, hEq⟩ := List.mem_map.mp hPair
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hEq
    exact hSrcLocals srcFn srcLocals srcGraph hSrcMem
  · intro fn srcLocals srcGraph hSrcMem
    have hPair : (fn, srcLocals) ∈ List.map (fun e => (e.1, e.2.1)) memory.functions := by
      rw [hInv]
      exact List.mem_map.mpr ⟨(fn, srcLocals, srcGraph), hSrcMem, rfl⟩
    obtain ⟨⟨dstFn, dstLocals, dstGraph⟩, hDstMem, hEq⟩ := List.mem_map.mp hPair
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hEq
    exact ⟨dstLocals, dstGraph, hDstMem, fun _ _ => Iff.rfl⟩
  · intro fn dstLocals dstGraph hDstMem
    have hPair : (fn, dstLocals) ∈ List.map (fun e => (e.1, e.2.1)) mir.functions := by
      rw [← hInv]
      exact List.mem_map.mpr ⟨(fn, dstLocals, dstGraph), hDstMem, rfl⟩
    obtain ⟨⟨srcFn, srcLocals, srcGraph⟩, hSrcMem, hEq⟩ := List.mem_map.mp hPair
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hEq
    exact ⟨srcLocals, srcGraph, hSrcMem, fun _ _ => Iff.rfl⟩

/-- Direct corollary for any pass (such as `prepareGraph` composed with `placeFunction` in
production `lowerGraph`) that maps each function's CFG while retaining `(fn, locals)`. -/
theorem RetainsTypedRoots.of_map_functions {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {mir : MIR lang profile} {memory : Memory lang profile}
    (lowerBody : FnId → LocalTypes Ty → MIRGraph lang → MemoryGraph lang)
    (hFns : memory.functions = List.map (fun e =>
      (e.1, e.2.1, lowerBody e.1 e.2.1 e.2.2)) mir.functions)
    (hSrcFns : (List.map Prod.fst mir.functions).Nodup)
    (hSrcLocals : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      (List.map Prod.fst locals).Nodup) :
    RetainsTypedRoots mir memory := by
  apply RetainsTypedRoots.of_inventories_eq _ hSrcFns hSrcLocals
  rw [hFns, List.map_map]
  rfl

/-- The logical MIR frame used to verify Memory's representation of live places. -/
structure LogicalFrame (Value : Type) where
  fn : FnId
  live : List PlaceId
  values : AList PlaceId Value
  movedFields : List (PlaceId × List FieldId) := []

abbrev MemoryRegisters (language : CoreLanguage) (profile : Profile) :=
  MemoryState language.operands.Ty language.operands.Val language.operands.Event profile

def MemoryRegisters.advance {language : CoreLanguage} {profile : Profile}
    (state : MemoryRegisters language profile) : MemoryRegisters language profile :=
  {state with instruction := state.instruction + 1}

namespace Memory

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

def graphAt (memory : Memory lang profile) (fn : FnId) : Option (MemoryGraph lang) :=
  (AList.lookup memory.functions fn).map Prod.snd

def blockAt (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    Option (MemoryBlock lang) := do
  let graph ← memory.graphAt state.frame
  graph.blocks.find? (·.id == state.block)

def classAt (memory : Memory lang profile) (fn : FnId) (place : PlaceId) :
    Option StorageClass := do
  let classes ← AList.lookup memory.storage fn
  AList.lookup classes place

def layoutAt (memory : Memory lang profile) (fn : FnId) (place : PlaceId) : Option Layout := do
  let layouts ← AList.lookup memory.layouts fn
  AList.lookup layouts place

/-- A materialized handle covers the live block of its provenance in full. -/
abbrev CoversBlock (range : Hybrid.Range) (block : Mem.Block) : Prop :=
  block.live = true ∧ block.space = range.ptr.space ∧ block.base = range.ptr.addr ∧
    block.size = range.extent ∧ block.align = range.align

def rootRange (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (range : Hybrid.Range) : Prop :=
  ∃ layout blockId block,
    memory.layoutAt state.frame place = some layout ∧ range.extent = layout.size ∧
    range.align = layout.align ∧ range.ptr.prov = some blockId ∧
    state.bytes.block blockId = some block ∧ CoversBlock range block

def rootType (memory : Memory lang profile) (fn : FnId) (place : PlaceId) :
    Option language.operands.Ty := do
  let (locals, _) ← AList.lookup memory.functions fn
  AList.lookup locals place

def validPlace (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) : Bool :=
  letI := language.typeDecidableEq
  decide (memory.classAt state.frame place.logical.root = some place.storage ∧
    (do
      let rootType ← memory.rootType state.frame place.logical.root
      place.logical.path.foldlM language.fieldType rootType) = some ty)

def atOffset (ptr : Mem.Ptr) (off : Nat) : Mem.Ptr :=
  {ptr with addr := ptr.addr + off}

def fieldRange (r : Hybrid.Range) (off : Nat) (leaf : Layout) : Hybrid.Range :=
  ⟨atOffset r.ptr off, leaf.size, leaf.align⟩

def rangeAt (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty} (place : Place _ ty) :
    Option Hybrid.Range := do
  let root ← AList.lookup state.materialized place.root
  if place.path.isEmpty then return root
  let layout ← memory.layoutAt state.frame place.root
  let (offset, field) ← layout.project place.path
  if offset + field.size ≤ root.extent then
    some (fieldRange root offset field)
  else none

def zeroRoot (memory : Memory lang profile) (frame : FnId) (root : PlaceId) : Bool :=
  (memory.layoutAt frame root).any (fun layout => layout.size == 0)

private def pathsOverlap (left right : List FieldId) : Bool :=
  left.isPrefixOf right || right.isPrefixOf left

def initializedAt (state : MemoryRegisters language profile) (root : PlaceId)
    (path : List FieldId) : Bool :=
  state.initialized.contains root &&
    !(state.movedFields.any fun (owner, moved) => owner == root && pathsOverlap path moved)

/-- Select the owner whose permission is needed. Storage tracks initialization at
field granularity while the spec tracks loan exclusion at the root. The slot view
uses the selected field's Copy/status information without changing the loan root.
Unaddressed owners use the next unused identity for the permission check. -/
def ownerView (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (root : PlaceId) (ty : language.operands.Ty) (status : Own.Status) :
    Own.State × Hybrid.Place :=
  let id := (AList.lookup state.addressIds root).getD
    ⟨max state.nextAddressId state.ownership.nextPlace⟩
  let slot := Own.slot state.ownership id
  let mutable := (slot.map (·.desc.mutable)).getD
    (((AList.lookup memory.mutableLocals state.frame).getD []).contains root)
  let desc : Own.Descriptor :=
    ⟨(language.scalarType ty).getD .unit, language.operands.isCopy ty, mutable⟩
  let activation := (slot.map (·.activation)).getD 0
  let status := if (slot.map (·.status)).getD .owned == .moved then .moved else status
  ({state.ownership with slots := ⟨id, activation, desc, status⟩ ::
    state.ownership.slots.filter (·.place != id)}, id)

/-- Commit the root status from the spec transition to an existing instrumented
slot. A field transition only changes the interpreter's field liveness. -/
private def commitOwner (state : MemoryRegisters language profile) (root : PlaceId)
    (owners : Own.State) : Own.State :=
  match AList.lookup state.addressIds root with
  | none => state.ownership
  | some id =>
    {state.ownership with slots := state.ownership.slots.map fun slot =>
      if slot.place == id then {slot with status := ((Own.slot owners id).map (·.status)).getD slot.status}
      else slot}

/-- Read the projected byte range of a nonempty materialized place at the
resolved target. This also covers spec targets without a hosted backend. -/
def loadMaterialized (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (target : Target)
    (ceiling : Nat := target.addressCeiling) :
    Option language.operands.Val := do
  let bytes ← (memory.rangeAt state place.logical).bind fun range =>
    state.bytes.load target range.ptr range.extent range.align ceiling
  match language.scalarType ty with
  | some scalar =>
    let value ← Hybrid.decode target scalar bytes
    return language.valueShape.symm ▸ CoreValue.scalar value
  | none =>
    let value ← language.decodeOther target ty bytes
    return language.valueShape.symm ▸ CoreValue.other value

def loadBacking (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Option language.operands.Val := do
  if !(memory.validPlace state place) then none else
  if !state.live.contains place.logical.root then none else
  match place.storage with
  | .virtual =>
    let root ← AList.lookup state.virtualValues place.logical.root
    if place.logical.path.isEmpty then some root else language.field root place.logical.path
  | .materialized =>
    if memory.zeroRoot state.frame place.logical.root then do
      let root ← AList.lookup state.zeroValues place.logical.root
      if place.logical.path.isEmpty then some root else language.field root place.logical.path
    else do
      memory.loadMaterialized state place profile.memoryTarget ceiling

def loadValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Option language.operands.Val :=
  if initializedAt state place.logical.root place.logical.path then memory.loadBacking state place ceiling
  else none

def byteRuns
    (target : Target) (ty : language.operands.Ty) (value : language.operands.Val) :
    Option (List (Nat × List Mem.MByte)) :=
  match language.valueShape ▸ value with
  | .scalar scalar => (Hybrid.encode target scalar).map fun bytes => [(0, bytes)]
  | .other other => language.encodeOther target ty other

def StoresValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Prop :=
  memory.loadValue state place ceiling = some value ∧
  (place.storage = .materialized →
    memory.zeroRoot state.frame place.logical.root = true ∨
    ∃ range runs, memory.rangeAt state place.logical = some range ∧
      byteRuns profile.memoryTarget ty value = some runs ∧
      (0 < range.extent → ∃ offset bytes, (offset, bytes) ∈ runs ∧ bytes ≠ []) ∧
      ∀ offset bytes, (offset, bytes) ∈ runs →
        offset + bytes.length ≤ range.extent ∧ (∀ byte ∈ bytes, byte ≠ .uninit) ∧
        state.bytes.load profile.memoryTarget (atOffset range.ptr offset)
          bytes.length 1 ceiling = some bytes)

/-- Ignore moved subtrees when checking for live linear fields at lifetime end or replacement. -/
private def canDiscard (language : CoreLanguage) (env : Declarations) (allowCopy : Bool := true) :
    Nat → language.operands.Ty → language.operands.Val → List (List FieldId) → Bool
  | fuel, ty, value, moved =>
    if moved.contains [] then true
    else if moved.isEmpty then allowCopy && language.operands.isCopy ty
    else match fuel with
      | 0 => false
      | n + 1 =>
        (language.activeFields env ty value).any fun fields =>
          fields.all fun field =>
            let below := moved.filterMap fun path =>
              match path with
              | first :: rest => if first == field then some rest else none
              | [] => none
            match language.fieldType ty field, language.field value [field] with
            | some fieldTy, some fieldValue => canDiscard language env allowCopy n fieldTy fieldValue below
            | _, _ => false

private def movedBelow (state : MemoryRegisters language profile)
    (root : PlaceId) (path : List FieldId) : List (List FieldId) :=
  state.movedFields.filterMap fun (owner, moved) =>
    if owner == root && path.isPrefixOf moved then some (moved.drop path.length) else none

def discardRemainder (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (allowCopy : Bool) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Bool :=
  let moved := movedBelow state place.logical.root place.logical.path
  !moved.isEmpty && (memory.loadBacking state place ceiling).any fun value =>
    canDiscard language memory.declarations allowCopy (moved.foldl (fun depth suffix => max depth suffix.length) 0)
      ty value moved

def mayInitialize (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Bool :=
  let root := place.logical.root
  let path := place.logical.path
  if !state.initialized.contains root then path.isEmpty
  else if initializedAt state root path then true
  else if state.movedFields.any (fun (owner, moved) =>
      owner == root && moved != path && moved.isPrefixOf path) then false
  else if state.movedFields.contains (root, path) then true
  else memory.discardRemainder state place true ceiling

/-- A write's owner view records whether any active part of the destination
still holds a value. A partially moved destination can have a discardable
remainder, but replacing that remainder requires mutable owner access. -/
def writeOwnerView (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Own.State × Hybrid.Place :=
  let root := place.logical.root
  let path := place.logical.path
  let empty := !state.initialized.contains root ||
    state.movedFields.any (fun (owner, moved) => owner == root && moved.isPrefixOf path) ||
    memory.discardRemainder state place false ceiling
  memory.ownerView state root ty (if empty then .moved else .owned)

/-- A normal reinitialization uses the selected slot directly. Replacing a
live remainder requires both fieldwise discharge and mutable owner access. -/
def authorizeWrite (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Option Own.State :=
  let (owners, id) := memory.writeOwnerView state place ceiling
  if memory.discardRemainder state place true ceiling && Own.ownerAccess owners id .mutable then
    some owners
  else Own.reinitialize owners id

def markWritten (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (owners : Own.State) : MemoryRegisters language profile :=
  {state with
    ownership := if place.logical.path.isEmpty then commitOwner state place.logical.root owners else state.ownership
    initialized := if state.initialized.contains place.logical.root then state.initialized else place.logical.root :: state.initialized
    movedFields := state.movedFields.filter (fun (owner, moved) =>
      owner != place.logical.root || !place.logical.path.isPrefixOf moved)}

def storeRuns (target : Target) (ceiling : Nat) (range : Hybrid.Range)
    (runs : List (Nat × List Mem.MByte)) (heap : Mem.Memory) : Option Mem.Memory :=
  runs.foldlM (fun mem (offset, bytes) =>
    if offset + bytes.length ≤ range.extent then
      mem.store target (atOffset range.ptr offset) bytes 1 ceiling
    else none) heap

def replaceLocal (table : AList PlaceId language.operands.Val) {ty : language.operands.Ty}
    (place : Place _ ty) (value : language.operands.Val) : Option (AList PlaceId language.operands.Val) := do
  let stored ← if place.path.isEmpty then some value else do
    let before ← AList.lookup table place.root
    language.replaceField before place.path value
  return (place.root, stored) :: AList.erase table place.root

def writeValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val) (ceiling : Nat := profile.memoryTarget.addressCeiling) :
    Option (MemoryRegisters language profile) := do
  let root := place.logical.root
  if !state.live.contains root || !memory.mayInitialize state place ceiling then none else
  let owners ← memory.authorizeWrite state place ceiling
  let state := markWritten state place owners
  match place.storage with
  | .virtual =>
    let table ← replaceLocal (language := language) state.virtualValues place.logical value
    return {state with virtualValues := table}
  | .materialized =>
    if memory.zeroRoot state.frame root then do
      let table ← replaceLocal (language := language) state.zeroValues place.logical value
      return {state with zeroValues := table}
    else do
      let range ← memory.rangeAt state place.logical
      let runs ← byteRuns profile.memoryTarget ty value
      let bytes ← storeRuns profile.memoryTarget ceiling range runs state.bytes
      return {state with bytes := bytes}

def readValue (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (read : MemoryRead _ lang.literal ty) :
    Option (language.operands.Val × MemoryRegisters language profile) := do
  match read with
  | .literal value => do
    let decoded ← language.literal ty value.encoding
    return (decoded, state)
  | .copy place => do
    let root := place.logical.root
    let (owners, id) := memory.ownerView state root ty .owned
    if !(language.operands.isCopy ty && Own.ownerAccess owners id .shared) then none else
    return (← memory.loadValue state place, state)
  | .move place => do
    let root := place.logical.root
    let (owners, id) := memory.ownerView state root ty .owned
    let owners ← Own.move owners id
    let value ← memory.loadValue state place
    if !place.logical.path.isEmpty then
      return (value, {state with movedFields := (root, place.logical.path) :: state.movedFields})
    let moved := {state with
      ownership := commitOwner state root owners
      initialized := state.initialized.erase root
      virtualValues := AList.erase state.virtualValues root
      zeroValues := AList.erase state.zeroValues root}
    return (value, moved)

/-- Literal operands always use the Core value codec, including in physical
execution. Only a load from live backing storage interprets target byte order. -/
theorem readLiteral_eq (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (value : lang.literal ty) :
    memory.readValue state (.literal value) =
      (language.literal ty value.encoding).map (fun decoded => (decoded, state)) := by
  cases h : language.literal ty value.encoding <;> simp [readValue, h]

theorem copyRead_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) :
    memory.readValue state (.copy place) = some (value, next) ↔
      let view := memory.ownerView state place.logical.root ty .owned
      language.operands.isCopy ty = true ∧ Own.ownerAccess view.1 view.2 .shared = true ∧
        memory.loadValue state place = some value ∧ next = state := by
  simp only [readValue]
  generalize (memory.ownerView state place.logical.root ty .owned) = view
  cases language.operands.isCopy ty <;>
    cases Own.ownerAccess view.1 view.2 .shared <;>
    cases memory.loadValue state place <;> simp [eq_comm]

theorem moveRead_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) :
    (memory.readValue state (.move place)).isSome = true ↔
      let view := memory.ownerView state place.logical.root ty .owned
      (Own.move view.1 view.2).isSome = true ∧ (memory.loadValue state place).isSome = true := by
  simp only [readValue]
  generalize (memory.ownerView state place.logical.root ty .owned) = view
  cases Own.move view.1 view.2 <;> cases memory.loadValue state place <;> simp
  split <;> rfl

theorem write_authorized (memory : Memory lang profile) (state : MemoryRegisters language profile)
    {ty : language.operands.Ty} (place : MemoryPlace _ ty) (value : language.operands.Val) {next} {ceiling : optParam Nat profile.memoryTarget.addressCeiling} :
    memory.writeValue state place value ceiling = some next →
      let (owners, id) := memory.writeOwnerView state place ceiling
      (Own.reinitialize owners id).isSome = true ∨
        (memory.discardRemainder state place true ceiling = true ∧ Own.ownerAccess owners id .mutable = true) := by
  simp only [writeValue]
  split
  · simp
  · simp only [authorizeWrite]
    generalize memory.writeOwnerView state place ceiling = view
    obtain ⟨owners, id⟩ := view
    dsimp only
    split
    · rename_i permitted
      exact fun _ => Or.inr (Bool.and_eq_true_iff.mp permitted)
    · cases Own.reinitialize owners id <;> simp

def Writes (memory : Memory lang profile)
    (state : MemoryRegisters language profile) {ty : language.operands.Ty}
    (place : MemoryPlace _ ty) (value : language.operands.Val)
    (next : MemoryRegisters language profile) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Prop :=
  (memory.validPlace state place = true ∧
    (initializedAt state place.logical.root place.logical.path = true →
      language.operands.isCopy ty = true)) ∧
  (place.storage = .materialized → ∃ layout,
    memory.layoutAt state.frame place.logical.root = some layout ∧
    (layout.size = 0 ↔ (AList.lookup state.materialized place.logical.root).isNone)) ∧
  memory.writeValue state place value ceiling = some next ∧
  memory.StoresValue next place value ceiling

def Represents (memory : Memory lang profile)
    (logical : LogicalFrame language.operands.Val) (state : MemoryRegisters language profile)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  (state.BytesWF ∧ logical.movedFields = state.movedFields) ∧ logical.fn = state.frame ∧ (∀ place ∈ logical.live, place ∈ state.live) ∧
  (List.map Prod.fst logical.values).Nodup ∧ (List.map Prod.fst state.materialized).Nodup ∧
  (List.map Prod.fst state.virtualValues).Nodup ∧
  (List.map Prod.fst state.zeroValues).Nodup ∧
  (∀ place value, (place, value) ∈ state.zeroValues →
    place ∈ state.initialized ∧ memory.classAt state.frame place = some .materialized ∧
    ∃ layout, memory.layoutAt state.frame place = some layout ∧ layout.size = 0) ∧
  (∀ place, place ∈ state.initialized ↔ (AList.lookup logical.values place).isSome) ∧
  (∀ place value, (place, value) ∈ logical.values → place ∈ logical.live) ∧
  (∀ place value, (place, value) ∈ state.virtualValues →
    place ∈ state.initialized ∧ memory.classAt state.frame place = some .virtual) ∧
  (∀ place range, (place, range) ∈ state.materialized →
    place ∈ state.live ∧ memory.classAt state.frame place = some .materialized ∧
    memory.rootRange state place range) ∧
  ∀ place ∈ state.live, ∃ locals graph ty storage,
    (state.frame, locals, graph) ∈ memory.functions ∧
    AList.lookup locals place = some ty ∧ memory.classAt state.frame place = some storage ∧
    (storage = .materialized → ∃ layout,
      memory.layoutAt state.frame place = some layout ∧
      (layout.size = 0 ↔ (AList.lookup state.materialized place).isNone)) ∧
    ∀ value, AList.lookup logical.values place = some value →
      if initializedAt state place [] then
        ∃ physical, language.representsValue profile.memoryTarget ranges ty value physical ∧
          memory.StoresValue state (ty := ty) ⟨⟨place, []⟩, storage⟩ physical
      else
        ∀ path fieldTy field, initializedAt state place path = true →
          path.foldlM language.fieldType ty = some fieldTy →
          language.field value path = some field →
          ∃ physical, language.representsValue profile.memoryTarget ranges fieldTy field physical ∧
            memory.StoresValue state (ty := fieldTy) ⟨⟨place, path⟩, storage⟩ physical

end Memory

namespace Memory
variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

def readArgs (memory : Memory lang profile) :
    {types : List language.operands.Ty} → Args (MemoryRead _ lang.literal) types →
    MemoryRegisters language profile →
    Option (List language.operands.Val × MemoryRegisters language profile)
  | _, .nil, state => some ([], state)
  | _, .cons read rest, state => do
    let (value, state) ← memory.readValue state read
    let (tail, state) ← memory.readArgs rest state
    return (value :: tail, state)

/-- Walk in allocation order. A root enters the live set at its first use,
including virtual and zero-byte roots. -/
def freshLayoutsFrom (memory : Memory lang profile) (fn : FnId) (live : List PlaceId) :
    List PlaceId → List Layout
  | [] => []
  | root :: rest =>
    if root ∈ live then memory.freshLayoutsFrom fn live rest
    else
      let tail := memory.freshLayoutsFrom fn (root :: live) rest
      if memory.classAt fn root = some .materialized then
        match memory.layoutAt fn root with
        | some layout => if layout.size = 0 then tail else layout :: tail
        | none => tail
      else tail

def freshLayouts (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (roots : List PlaceId) : List Layout :=
  memory.freshLayoutsFrom state.frame state.live roots

/-- Start at the first address beyond the physical space. Each fresh root needs
at most align - 1 bytes of padding and size bytes of payload. The final + 1
includes the last one-past address under the exclusive ceiling. -/
def transferCeiling (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (roots : List PlaceId) : Nat :=
  profile.memoryTarget.addressCeiling + 1 +
    ((memory.freshLayouts state roots).map fun l => (l.align - 1) + l.size).sum

/-- Can the allocator reserve all the bytes needed by this transfer? Each fresh
materialized root consumes its nonempty layout once. Class and layout come from
the frame tables, regardless of the destination annotation or value to write. -/
def ReservationsFit (target : Target) (ceiling : Nat) (layouts : List Layout) (heap : Mem.Memory) : Prop :=
  layouts.foldr (fun layout fits bytes =>
    ∃ space base block reserved,
      bytes.alloc target space base layout.size layout.align ceiling = .ok (block, reserved) ∧
      fits reserved) (fun _ : Mem.Memory => True) heap

def ParamSpaceFits (memory : Memory lang profile)
    (params : List (Sigma (MemoryPlace language.operands.Ty)))
    (state : MemoryRegisters language profile) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Prop :=
  ReservationsFit profile.memoryTarget ceiling
    (memory.freshLayouts state (params.map fun p => p.2.logical.root)) state.bytes

def Begins (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) (ceiling : Nat := profile.memoryTarget.addressCeiling) : Prop :=
  place ∉ state.live ∧
  let live := {state with live := place :: state.live}
  (memory.classAt state.frame place = some .virtual ∧ next = live) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ∃ layout, memory.layoutAt state.frame place = some layout ∧
    ((layout.size = 0 ∧ next = live) ∨
    (0 < layout.size ∧ ∃ space base block bytes,
      state.bytes.alloc profile.memoryTarget space base layout.size layout.align ceiling = .ok (block, bytes) ∧
      next = {live with bytes := bytes, materialized :=
        (place, ⟨⟨space, base, some block⟩, layout.size, layout.align⟩) :: state.materialized})))

/-- Test the same begin with space for this one fresh layout. -/
def BeginValid (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Prop :=
  ∃ next, memory.Begins state place next (memory.transferCeiling state [place])

structure BeginExhausted (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Prop where
  valid : memory.BeginValid state place
  unavailable : ¬ ∃ next, memory.Begins state place next

theorem BeginExhausted.fresh {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {root} (h : memory.BeginExhausted q root) : root ∉ q.live := by
  obtain ⟨_, fresh, _⟩ := h.valid
  exact fresh

/-- A virtual begin in a widened execution is already possible on the
actual target. Only a materialized root can therefore exhaust. -/
theorem BeginExhausted.materialized {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {root} (h : memory.BeginExhausted q root) : memory.classAt q.frame root = some .materialized := by
  obtain ⟨_, fresh, ⟨virtual, _⟩ | ⟨cls, _⟩⟩ := h.valid
  · exact (h.unavailable ⟨_, fresh, .inl ⟨virtual, rfl⟩⟩).elim
  · exact cls

theorem BeginExhausted.nonemptyLayout {memory : Memory lang profile} {q : MemoryRegisters language profile}
    {root} (h : memory.BeginExhausted q root) : ∃ layout, memory.layoutAt q.frame root = some layout ∧ 0 < layout.size := by
  obtain ⟨_, fresh, ⟨virtual, _⟩ | ⟨cls, layout, found, zero | placed⟩⟩ := h.valid
  · exact (h.unavailable ⟨_, fresh, .inl ⟨virtual, rfl⟩⟩).elim
  · exact (h.unavailable ⟨_, fresh, .inr ⟨cls, layout, found, .inl ⟨zero.1, rfl⟩⟩⟩).elim
  · exact ⟨layout, found, placed.1⟩

def endOwnerView (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Option (Own.State × Hybrid.Place) :=
  (memory.rootType state.frame place).map fun ty =>
    let moved := state.movedFields.filterMap fun (owner, path) =>
      if owner == place then some path else none
    let discharged := !state.initialized.contains place ||
      (!moved.isEmpty &&
        (memory.classAt state.frame place).any fun storage =>
          (memory.loadBacking state (ty := ty) ⟨⟨place, []⟩, storage⟩).any fun value =>
            canDiscard language memory.declarations true (moved.foldl (fun depth path => max depth path.length) 0) ty value moved)
    memory.ownerView state place ty (if discharged then .moved else .owned)

def canEnd (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) : Bool :=
  (memory.endOwnerView state place).any fun (owners, id) => Own.canDropPlaces owners [id]

theorem canEnd_iff (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) :
    memory.canEnd state place = true ↔
      ∃ view, memory.endOwnerView state place = some view ∧ Own.canDropPlaces view.1 [view.2] = true := by
  simp only [canEnd]
  cases found : memory.endOwnerView state place
  · simp
  · rename_i view
    cases view
    simp

def Ends (memory : Memory lang profile) (state : MemoryRegisters language profile)
    (place : PlaceId) (next : MemoryRegisters language profile) : Prop :=
  (place ∈ state.live ∧ memory.canEnd state place) ∧
  let ownership := match AList.lookup state.addressIds place with
    | none => state.ownership
    | some id => Own.erasePlaces state.ownership [id]
  let dead := {state with
    ownership
    live := state.live.erase place
    initialized := state.initialized.erase place
    zeroValues := AList.erase state.zeroValues place
    virtualValues := AList.erase state.virtualValues place
    materialized := AList.erase state.materialized place
    addressIds := AList.erase state.addressIds place
    movedFields := state.movedFields.filter (fun (owner, _) => owner != place)}
  (memory.classAt state.frame place = some .virtual ∧ next = dead) ∨
  (memory.classAt state.frame place = some .materialized ∧
    ((∃ layout, memory.layoutAt state.frame place = some layout ∧ layout.size = 0 ∧ next = dead) ∨
    (∃ range block bytes, AList.lookup state.materialized place = some range ∧ range.ptr.prov = some block ∧
      state.bytes.free block = some bytes ∧ next = {dead with bytes := bytes})))

def readOperands (memory : Memory lang profile) :
    List (Sigma (MemoryRead _ lang.literal)) → MemoryRegisters language profile →
    Option (List language.operands.Val × MemoryRegisters language profile)
  | [], state => some ([], state)
  | ⟨_, operand⟩ :: rest, state => do
    let (value, afterRead) ← memory.readValue state operand
    let (values, next) ← memory.readOperands rest afterRead
    return (value :: values, next)

/-- One placement sequence for a parameter list. The flag records whether this
sequence encountered an infeasible joint reservation. The allocator may
check the remaining batch at any prefix, including before its first write. -/
inductive WriteParams (memory : Memory lang profile) :
    List (Sigma (MemoryPlace language.operands.Ty)) → List language.operands.Val →
      MemoryRegisters language profile → MemoryRegisters language profile → (exhausting : optParam Bool false) →
      (ceiling : optParam Nat profile.memoryTarget.addressCeiling) → Prop
  | nil (state) {ceiling} : WriteParams memory [] [] state state false ceiling
  | cons {ty} {place : MemoryPlace _ ty} {params value values state started stored next exhausting ceiling} :
      (if state.live.contains place.logical.root then started = state
        else memory.Begins state place.logical.root started ceiling) →
      memory.Writes started place value stored ceiling →
      WriteParams memory params values stored next exhausting ceiling →
      WriteParams memory (⟨ty, place⟩ :: params) (value :: values) state next exhausting ceiling
  | exhausted {ty} {place : MemoryPlace _ ty} {params value values state} :
      (¬ memory.ParamSpaceFits (⟨ty, place⟩ :: params) state) →
      WriteParams memory (⟨ty, place⟩ :: params) (value :: values) state state true

/-- Materialized handles at the checkpoint name existing live allocations.
A hypothetical transfer changes the address budget for fresh reservations.
Its incoming handles and occupied intervals must already be physical. -/
def resident (memory : Memory lang profile) (q : MemoryRegisters language profile)
    (key : PlaceId) (r : Hybrid.Range) : Bool := (do
  let l ← memory.layoutAt q.frame key
  let id ← r.ptr.prov
  let b ← q.bytes.block id
  return decide (CoversBlock r b ∧ r.extent = l.size ∧ r.align = l.align ∧
    l.align ∣ r.ptr.addr ∧ profile.memoryTarget.null < r.ptr.addr ∧
    r.ptr.space ∈ profile.memoryTarget.spaces ∧ id.id < q.bytes.next)).getD false

def Inside (heap : Mem.Memory) (ceiling : Nat) : Prop :=
  ∀ e ∈ heap.blocks, e.2.live = true → e.2.base + e.2.size < ceiling

def TransferCheckpoint (memory : Memory lang profile) (q : MemoryRegisters language profile) (ceiling : Nat) : Prop :=
  (∀ e ∈ q.materialized, memory.resident q e.1 e.2 = true) ∧
    Inside q.bytes (min profile.memoryTarget.addressCeiling ceiling)

instance (memory : Memory lang profile) (q : MemoryRegisters language profile) (ceiling : Nat) :
    Decidable (memory.TransferCheckpoint q ceiling) := by
  unfold TransferCheckpoint Inside
  infer_instance

/-- The checkpoint and every write fit the given ceiling. The default budgets
for the complete transfer. Codecs and layout limits always use the physical target. -/
def ParamsValid (memory : Memory lang profile)
    (params : List (Sigma (MemoryPlace language.operands.Ty))) (values : List language.operands.Val)
    (state : MemoryRegisters language profile)
    (ceiling : Nat := memory.transferCeiling (language := language) state (params.map fun p => p.2.logical.root)) : Prop :=
  memory.TransferCheckpoint state ceiling ∧ ∃ next, memory.WriteParams params values state next false ceiling

/-- A failed physical prefix rolls back only when the complete transfer is
valid and no joint placement was available at the original checkpoint. -/
def ParamOutcome (memory : Memory lang profile)
    (params : List (Sigma (MemoryPlace language.operands.Ty))) (values : List language.operands.Val)
    (state next : MemoryRegisters language profile) (exhausting : Bool) : Prop :=
  if exhausting then
    memory.WriteParams params values state next true ∧
      memory.ParamsValid params values state ∧ ¬ memory.ParamSpaceFits params state
  else memory.WriteParams params values state next false

theorem ParamOutcome.valid_of_exhausted {memory : Memory lang profile} {params values}
    {state next : MemoryRegisters language profile} (h : memory.ParamOutcome params values state next true) :
    memory.ParamsValid params values state := h.2.1

/-- Joint space alone excludes exhaustion, in particular for every valid transfer. -/
theorem ParamSpaceFits.not_exhausted {memory : Memory lang profile} {params values}
    {state next : MemoryRegisters language profile} (fits : memory.ParamSpaceFits params state) :
    ¬ memory.ParamOutcome params values state next true := fun h => h.2.2 fits

/-- A valid transfer without a physical joint placement can abort atomically
at the checkpoint, before committing any of its writes. -/
theorem ParamsValid.exhausted_of_no_space {memory : Memory lang profile} {params values}
    {state : MemoryRegisters language profile} (valid : memory.ParamsValid params values state)
    (unavailable : ¬ memory.ParamSpaceFits params state) :
    memory.ParamOutcome params values state state true := by
  refine ⟨?_, valid, unavailable⟩
  obtain ⟨_, path⟩ := valid.2
  cases path with
  | nil => exact (unavailable trivial).elim
  | cons => exact .exhausted unavailable

inductive EndLocals (memory : Memory lang profile) :
    List PlaceId → MemoryRegisters language profile → MemoryRegisters language profile → Prop
  | nil (state) : EndLocals memory [] state state
  | cons {place rest state ended next} :
      memory.Ends state place ended → EndLocals memory rest ended next →
      EndLocals memory (place :: rest) state next

def Call (memory : Memory lang profile) (callee : FnId)
    (dst : Option (Sigma (MemoryPlace language.operands.Ty)))
    (args : List (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ values afterRead graph entry,
    memory.readOperands args state = some (values, afterRead) ∧
    memory.graphAt callee = some graph ∧ graph.blocks.find? (·.id == graph.entry) = some entry ∧
    args.map Sigma.fst = entry.params.map Sigma.fst ∧
    memory.ParamOutcome entry.params values
      { toMemoryFrame := ⟨callee, graph.entry, 0, [], [], [], [], [], [], []⟩
        bytes := afterRead.bytes
        ownership := afterRead.ownership
        nextAddressId := afterRead.nextAddressId
        effects := afterRead.effects
        callers := ⟨afterRead.advance.toMemoryFrame, dst⟩ :: afterRead.callers } next exhausting

def Return (memory : Memory lang profile) (operand : Option (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ caller rest values afterRead ended,
    state.callers = caller :: rest ∧
    memory.readOperands operand.toList state = some (values, afterRead) ∧
    memory.EndLocals afterRead.live afterRead ended ∧
    operand.toList.map Sigma.fst = caller.destination.toList.map Sigma.fst ∧
    memory.ParamOutcome caller.destination.toList values
      { toMemoryFrame := caller.caller, bytes := ended.bytes, callers := rest, effects := ended.effects, ownership := ended.ownership, nextAddressId := ended.nextAddressId } next exhausting

def Transfer (memory : Memory lang profile) (edge : CFGEdge (Sigma (MemoryRead _ lang.literal)))
    (state next : MemoryRegisters language profile) (exhausting : Bool := false) : Prop :=
  ∃ values afterRead block,
    memory.readOperands edge.arguments state = some (values, afterRead) ∧
    memory.blockAt {state with block := edge.target} = some block ∧
    edge.arguments.map Sigma.fst = block.params.map Sigma.fst ∧
    memory.ParamOutcome block.params values {afterRead with block := edge.target, instruction := 0} next exhausting

/-- Splitting a value transfers each bound field once. An omitted active field
must be Copy, and every destination has the declared type of its field. -/
def DestructureFields (language : CoreLanguage) (env : Declarations) (ty : language.operands.Ty)
    (value : language.operands.Val) (bindings : List (FieldId × language.operands.Ty)) : Prop :=
  (bindings.map Prod.fst).Nodup ∧
  ∃ fields, language.activeFields env ty value = some fields ∧
    (∀ binding ∈ bindings,
      binding.1 ∈ fields ∧ language.fieldType ty binding.1 = some binding.2) ∧
    ∀ field ∈ fields, field ∉ bindings.map Prod.fst →
      ∃ fieldTy, language.fieldType ty field = some fieldTy ∧
        language.operands.isCopy fieldTy = true

def firstTrap (opcode : Opcode) (values : List language.operands.Val)
    (checks : List OperationCheck) : Option TrapKind :=
  (checks.find? fun check => check.disposition == .residual &&
    language.checkTrap opcode check.discriminator values).map OperationCheck.trap

def instructionTrap (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    MemoryInst lang → Option TrapKind
  | @MemoryInst.op _ _ opcode _ _ _ _ args checks _ => do
    let (values, _) ← memory.readArgs args state
    firstTrap (.operator opcode) values checks
  | .call callee _ args checks => do
    let (values, _) ← memory.readOperands args state
    firstTrap (.call callee) values checks
  | _ => none

/-- Taking an address assigns a fresh identity to the current lifetime on first use.
The identity is independent of the numeric placement and survives suspension. -/
def prepareAddresses (state : MemoryRegisters language profile)
    (operands : List (Sigma (MemoryPlace language.operands.Ty))) :
    MemoryRegisters language profile :=
  operands.foldl (fun state operand =>
    let root := operand.2.logical.root
    if (AList.lookup state.addressIds root).isSome then state
    else {state with
      addressIds := (root, ⟨state.nextAddressId⟩) :: state.addressIds
      nextAddressId := state.nextAddressId + 1}) state

def resolveAddress (memory : Memory lang profile)
    (state : MemoryRegisters language profile) (logical : Bool)
    (operand : Sigma (MemoryPlace language.operands.Ty)) :
    Option (OperationAddress language.operands.Ty) := do
  let place := operand.2
  if !memory.validPlace state place || !state.live.contains place.logical.root ||
      !initializedAt state place.logical.root place.logical.path then none else
  let provenance ← AList.lookup state.addressIds place.logical.root
  if logical then
    return ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .logical⟩
  else
    if place.storage != .materialized then none else
    if memory.zeroRoot state.frame place.logical.root then
      if !place.logical.path.isEmpty then
        let layout ← memory.layoutAt state.frame place.logical.root
        let (offset, field) ← layout.project place.logical.path
        if offset + field.size != 0 then none else
          some ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .zero⟩
      else some ⟨operand.1, place.logical.root, place.logical.path, none, provenance, .zero⟩
    else
      let range ← memory.rangeAt state place.logical
      let _ ← state.bytes.access profile.memoryTarget range.ptr range.extent range.align
      return ⟨operand.1, place.logical.root, place.logical.path, some range, provenance, .bytes⟩

private def addressedValues (logical : Bool) (frame : MemoryFrame language.operands.Val) :
    AList Hybrid.Place language.operands.Val :=
  let values := if logical then frame.virtualValues else frame.zeroValues
  frame.addressIds.filterMap fun (root, id) =>
    (AList.lookup values root).map fun value => (id, value)

def operationStore (state : MemoryRegisters language profile) (logical : Bool) :
    OperationStore language.operands.Val :=
  ⟨state.bytes, state.ownership,
    addressedValues logical state.toMemoryFrame ++
      state.callers.flatMap fun continuation => addressedValues logical continuation.caller⟩

private def updateAddressed (logical : Bool) (values : AList Hybrid.Place language.operands.Val)
    (frame : MemoryFrame language.operands.Val) : MemoryFrame language.operands.Val :=
  let update := List.map fun (root, value) =>
    let changed := (AList.lookup frame.addressIds root).bind (AList.lookup values)
    (root, changed.getD value)
  if logical then {frame with virtualValues := update frame.virtualValues}
    else {frame with zeroValues := update frame.zeroValues}

/-- Install logical writes through references in the owning frame. The operation can
only update addressed values that were present before the step. Physical bytes remain
the authority for nonempty roots. -/
def applyOperation (state : MemoryRegisters language profile) (logical : Bool)
    (store : OperationStore language.operands.Val) : MemoryRegisters language profile :=
  {state with
    toMemoryFrame := updateAddressed logical store.addressedValues state.toMemoryFrame
    callers := state.callers.map fun continuation =>
      {continuation with caller := updateAddressed logical store.addressedValues continuation.caller}
    bytes := store.bytes
    ownership := store.ownership}

/-- Two logical places are disjoint when they name distinct roots or non-overlapping field
paths under the same root. For whole-root places (`path = []`), this reduces to `left.root ≠ right.root`. -/
def placesDisjoint {Ty : Type} {ty₁ ty₂ : Ty}
    (left : Place Ty ty₁) (right : Place Ty ty₂) : Prop :=
  left.root ≠ right.root ∨ pathsOverlap left.path right.path = false

def InstructionStepWith (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (logical startDefinitions : Bool) (instruction : MemoryInst lang)
    (exhausting : Bool := false) : Prop := match instruction with
  | .bind dst src =>
    ∃ value afterRead stored, memory.readValue state src = some (value, afterRead) ∧
      (if logical || startDefinitions then memory.ParamOutcome [⟨_, dst⟩] [value] afterRead stored exhausting
        else if exhausting then False else memory.Writes afterRead dst value stored) ∧ next = stored.advance
  | .assign dst src => if exhausting then False else
    ∃ value afterRead stored, memory.readValue state src = some (value, afterRead) ∧
      memory.Writes afterRead dst value stored ∧ next = stored.advance
  | @MemoryInst.op _ _ opcode types ty dst _ args checks addresses =>
    ∃ values afterReads operands result effects changed stored, memory.readArgs args state = some (values, afterReads) ∧
      firstTrap (.operator opcode) values checks = none ∧
      addresses.mapM (memory.resolveAddress (prepareAddresses afterReads addresses) logical) = some operands ∧
      let ready := prepareAddresses afterReads addresses
      let before := operationStore ready logical
      language.operation memory.declarations opcode types ty values profile.memoryTarget operands before result effects changed ∧
      List.map Prod.fst changed.addressedValues = List.map Prod.fst before.addressedValues ∧
      let afterOperation := applyOperation ready logical changed
      (if logical || startDefinitions then memory.ParamOutcome [⟨_, dst⟩] [result] afterOperation stored exhausting
        else if exhausting then False else memory.Writes afterOperation dst result stored) ∧
      next = {stored.advance with effects := stored.effects ++ effects}
  | @MemoryInst.addWithOverflow _ _ t dst flag lhs rhs =>
    language.scalarType (lang.intTy t) = some (.int t) ∧
    language.scalarType lang.bool = some .bool ∧
    placesDisjoint dst.logical flag.logical ∧
    ∃ lv afterLhs rv afterReads x y stored,
      memory.readValue state lhs = some (lv, afterLhs) ∧
      memory.readValue afterLhs rhs = some (rv, afterReads) ∧
      language.valueShape ▸ lv = CoreValue.scalar (.int t x) ∧
      language.valueShape ▸ rv = CoreValue.scalar (.int t y) ∧
      let sum : language.operands.Val :=
        language.valueShape.symm ▸ CoreValue.scalar (.int t (wrapping t .add x y))
      let overflowed : language.operands.Val :=
        language.valueShape.symm ▸ CoreValue.scalar (.bool (decide (checked t .add x y = .error .overflow)))
      (if logical || startDefinitions then
        memory.ParamOutcome [⟨_, dst⟩, ⟨_, flag⟩] [sum, overflowed] afterReads stored exhausting
      else if exhausting then False
      else ∃ mid, memory.Writes afterReads dst sum mid ∧ memory.Writes mid flag overflowed stored) ∧
      next = stored.advance
  | .beginLifetime place => if exhausting then memory.BeginExhausted state place else
    ∃ started, memory.Begins state place started ∧ next = started.advance
  | .endLifetime place => if exhausting then False else
    ∃ ended, memory.Ends state place ended ∧ next = ended.advance
  | @MemoryInst.destructure _ _ ty src bindings =>
    ∃ value afterRead fields stored,
      memory.readValue state (if language.operands.isCopy ty then .copy src else .move src) =
        some (value, afterRead) ∧
      DestructureFields language memory.declarations ty value (bindings.map fun (field, dst) => (field, dst.1)) ∧
      bindings.mapM (fun binding => language.field value [binding.1]) = some fields ∧
      memory.ParamOutcome (bindings.map Prod.snd) fields afterRead stored exhausting ∧ next = stored.advance
  | .call callee dst args checks =>
    memory.instructionTrap state (.call callee dst args checks) = none ∧
      memory.Call callee dst args state next exhausting

/-! ### `addWithOverflow` step lemmas -/

theorem addWithOverflow_no_trap (memory : Memory lang profile)
    (state : MemoryRegisters language profile) (t : IntTy)
    (dst : MemoryPlace _ (lang.intTy t)) (flag : MemoryPlace _ lang.bool)
    (lhs rhs : MemoryRead _ lang.literal (lang.intTy t)) :
    memory.instructionTrap state (.addWithOverflow t dst flag lhs rhs) = none :=
  rfl

theorem addWithOverflow_cannot_exhaust {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {t : IntTy}
    {dst : MemoryPlace _ (lang.intTy t)} {flag : MemoryPlace _ lang.bool}
    {lhs rhs : MemoryRead _ lang.literal (lang.intTy t)} :
    ¬ memory.InstructionStepWith state next false false (.addWithOverflow t dst flag lhs rhs) true := by
  rintro ⟨_, _, _, _, _, _, _, _, _, _, _, _, _, _, hFail, _⟩
  exact hFail

theorem addWithOverflow_distinct {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {logical startDefinitions exhausting : Bool}
    {t : IntTy} {dst : MemoryPlace _ (lang.intTy t)} {flag : MemoryPlace _ lang.bool}
    {lhs rhs : MemoryRead _ lang.literal (lang.intTy t)}
    (step : memory.InstructionStepWith state next logical startDefinitions
      (.addWithOverflow t dst flag lhs rhs) exhausting) :
    placesDisjoint dst.logical flag.logical :=
  step.2.2.1

theorem addWithOverflow_aliased_stuck {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {logical startDefinitions exhausting : Bool}
    {t : IntTy} {dst : MemoryPlace _ (lang.intTy t)} {flag : MemoryPlace _ lang.bool}
    {lhs rhs : MemoryRead _ lang.literal (lang.intTy t)}
    (hRoot : dst.logical.root = flag.logical.root)
    (hDstPath : dst.logical.path = []) (hFlagPath : flag.logical.path = []) :
    ¬ memory.InstructionStepWith state next logical startDefinitions
      (.addWithOverflow t dst flag lhs rhs) exhausting := by
  intro step
  rcases addWithOverflow_distinct step with hNe | hNoOverlap
  · exact hNe hRoot
  · simp [pathsOverlap, hDstPath, hFlagPath] at hNoOverlap

private theorem readValue_instruction_eq {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {ty : language.operands.Ty}
    {read : MemoryRead _ lang.literal ty} {v : language.operands.Val}
    (h : memory.readValue state read = some (v, next)) :
    next.instruction = state.instruction ∧ next.effects = state.effects := by
  cases read with
  | literal lit =>
    simp only [readValue] at h
    obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp h
    cases eq
    exact ⟨rfl, rfl⟩
  | copy place =>
    obtain rfl := ((copyRead_iff memory state place v next).mp h).2.2.2
    exact ⟨rfl, rfl⟩
  | move place =>
    simp only [readValue, bind, Option.bind] at h
    cases ho : Own.move (memory.ownerView state place.logical.root ty .owned).1
        (memory.ownerView state place.logical.root ty .owned).2 <;>
      cases hv : memory.loadValue state place <;> simp only [ho, hv] at h <;> try contradiction
    split at h <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj h) <;> exact ⟨rfl, rfl⟩

private theorem writeValue_instruction_eq {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {ty : language.operands.Ty}
    {place : MemoryPlace _ ty} {v : language.operands.Val} {ceiling : Nat}
    (h : memory.writeValue state place v ceiling = some next) :
    next.instruction = state.instruction ∧ next.effects = state.effects := by
  simp only [writeValue] at h
  split at h
  · cases h
  · simp only [bind, Option.bind] at h
    cases ha : memory.authorizeWrite state place ceiling with
    | none => simp [ha] at h
    | some owners =>
      simp only [ha] at h
      cases hst : place.storage with
      | virtual =>
        simp only [hst] at h
        cases hr : replaceLocal (language := language) (markWritten state place owners).virtualValues place.logical v with
        | none => simp [hr] at h
        | some tbl =>
          simp only [hr, pure, Option.some.injEq] at h
          subst h; exact ⟨rfl, rfl⟩
      | materialized =>
        simp only [hst] at h
        split at h
        · cases hr : replaceLocal (language := language) (markWritten state place owners).zeroValues place.logical v with
          | none => simp [hr] at h
          | some tbl =>
            simp only [hr, pure, Option.some.injEq] at h
            subst h; exact ⟨rfl, rfl⟩
        · cases hrg : memory.rangeAt (markWritten state place owners) place.logical with
          | none => simp [hrg] at h
          | some rg =>
            simp only [hrg] at h
            cases hbr : byteRuns profile.memoryTarget ty v with
            | none => simp [hbr] at h
            | some runs =>
              simp only [hbr] at h
              cases hsr : storeRuns profile.memoryTarget ceiling rg runs (markWritten state place owners).bytes with
              | none => simp [hsr] at h
              | some bs =>
                simp only [hsr, pure, Option.some.injEq] at h
                subst h; exact ⟨rfl, rfl⟩

private theorem begins_instruction_eq {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {place : PlaceId} {ceiling : Nat}
    (h : memory.Begins state place next ceiling) :
    next.instruction = state.instruction ∧ next.effects = state.effects := by
  rcases h.2 with ⟨_, rfl⟩ | ⟨_, _, _, ⟨_, rfl⟩ | ⟨_, _, _, _, _, _, rfl⟩⟩ <;> exact ⟨rfl, rfl⟩

private theorem writeParams_instruction_eq {memory : Memory lang profile}
    {ps : List (Sigma (MemoryPlace language.operands.Ty))} {vs : List language.operands.Val}
    {state next : MemoryRegisters language profile} {exhausting : Bool} {ceiling : Nat}
    (h : memory.WriteParams ps vs state next exhausting ceiling) :
    next.instruction = state.instruction ∧ next.effects = state.effects := by
  induction h with
  | nil _ => exact ⟨rfl, rfl⟩
  | @cons _ _ _ _ _ state' started' stored' _ _ _ hStart hWrite _ ih =>
    have hs : started'.instruction = state'.instruction ∧ started'.effects = state'.effects := by
      split at hStart
      · subst hStart; exact ⟨rfl, rfl⟩
      · exact begins_instruction_eq hStart
    have hw := writeValue_instruction_eq hWrite.2.2.1
    exact ⟨ih.1.trans (hw.1.trans hs.1), ih.2.trans (hw.2.trans hs.2)⟩
  | exhausted _ => exact ⟨rfl, rfl⟩

/-- A normal `addWithOverflow` step advances the program counter by exactly one and
emits no side effects. -/
theorem addWithOverflow_step_advances {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {logical startDefinitions : Bool}
    {t : IntTy} {dst : MemoryPlace _ (lang.intTy t)} {flag : MemoryPlace _ lang.bool}
    {lhs rhs : MemoryRead _ lang.literal (lang.intTy t)}
    (step : memory.InstructionStepWith state next logical startDefinitions
      (.addWithOverflow t dst flag lhs rhs) false) :
    next.instruction = state.instruction + 1 ∧ next.effects = state.effects := by
  obtain ⟨_, _, _, lv, afterLhs, rv, afterReads, x, y, stored, hLhs, hRhs, _, _, hStore, rfl⟩ := step
  have h1 := readValue_instruction_eq hLhs
  have h2 := readValue_instruction_eq hRhs
  have h3 : stored.instruction = afterReads.instruction ∧ stored.effects = afterReads.effects := by
    cases hMode : (logical || startDefinitions)
    · simp only [hMode, Bool.false_eq_true, ↓reduceIte] at hStore
      obtain ⟨mid, hW1, hW2⟩ := hStore
      have hw1 := writeValue_instruction_eq hW1.2.2.1
      have hw2 := writeValue_instruction_eq hW2.2.2.1
      exact ⟨hw2.1.trans hw1.1, hw2.2.trans hw1.2⟩
    · simp only [hMode, ↓reduceIte, ParamOutcome, Bool.false_eq_true] at hStore
      exact writeParams_instruction_eq hStore
  exact ⟨by simp only [MemoryRegisters.advance, h3.1, h2.1, h1.1],
    by simp only [MemoryRegisters.advance, h3.2, h2.2, h1.2]⟩

/-- Virtual two-result `addWithOverflow` (as decoded from SSA/BIR registers) can never
exhaust physical allocation space. -/
theorem addWithOverflow_virtual_cannot_exhaust {memory : Memory lang profile}
    {state next : MemoryRegisters language profile} {logical startDefinitions : Bool}
    {t : IntTy} {dst : MemoryPlace _ (lang.intTy t)} {flag : MemoryPlace _ lang.bool}
    {lhs rhs : MemoryRead _ lang.literal (lang.intTy t)}
    (hDst : memory.classAt state.frame dst.logical.root = some .virtual)
    (hFlag : memory.classAt state.frame flag.logical.root = some .virtual) :
    ¬ memory.InstructionStepWith state next logical startDefinitions
      (.addWithOverflow t dst flag lhs rhs) true := by
  rintro ⟨_, _, _, lv, afterLhs, rv, afterReads, x, y, stored, hLhs, hRhs, _, _, hStore, _⟩
  cases hMode : (logical || startDefinitions)
  · simp only [hMode, Bool.false_eq_true, ↓reduceIte] at hStore
  · simp only [hMode, ↓reduceIte, ParamOutcome] at hStore
    have hFrame1 : afterLhs.frame = state.frame := by
      cases lhs with
      | literal _ =>
        simp only [readValue] at hLhs
        obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp hLhs
        cases eq; rfl
      | copy place => exact congrArg (fun s => s.frame) (((copyRead_iff ..).mp hLhs).2.2.2)
      | move place =>
        simp only [readValue, bind, Option.bind] at hLhs
        cases ho : Own.move (memory.ownerView state place.logical.root (lang.intTy t) .owned).1
            (memory.ownerView state place.logical.root (lang.intTy t) .owned).2 <;>
          cases hv : memory.loadValue state place <;> simp only [ho, hv] at hLhs <;> try contradiction
        split at hLhs <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj hLhs) <;> rfl
    have hFrame2 : afterReads.frame = afterLhs.frame := by
      cases rhs with
      | literal _ =>
        simp only [readValue] at hRhs
        obtain ⟨_, _, eq⟩ := Option.bind_eq_some_iff.mp hRhs
        cases eq; rfl
      | copy place => exact congrArg (fun s => s.frame) (((copyRead_iff ..).mp hRhs).2.2.2)
      | move place =>
        simp only [readValue, bind, Option.bind] at hRhs
        cases ho : Own.move (memory.ownerView afterLhs place.logical.root (lang.intTy t) .owned).1
            (memory.ownerView afterLhs place.logical.root (lang.intTy t) .owned).2 <;>
          cases hv : memory.loadValue afterLhs place <;> simp only [ho, hv] at hRhs <;> try contradiction
        split at hRhs <;> obtain ⟨_, rfl⟩ := Prod.mk.inj (Option.some.inj hRhs) <;> rfl
    have hFrame : afterReads.frame = state.frame := hFrame2.trans hFrame1
    have hEmpty : memory.freshLayouts afterReads [dst.logical.root, flag.logical.root] = [] := by
      have hSingle (live : List PlaceId) :
          memory.freshLayoutsFrom state.frame live [flag.logical.root] = [] := by
        unfold freshLayoutsFrom
        split
        · rfl
        · simp [freshLayoutsFrom, hFlag]
      unfold freshLayouts freshLayoutsFrom
      rw [hFrame]
      split
      · exact hSingle afterReads.live
      · simp [hDst, hSingle]
    have hFits : memory.ParamSpaceFits [⟨_, dst⟩, ⟨_, flag⟩] afterReads := by
      unfold ParamSpaceFits ReservationsFit
      simp only [List.map_cons, List.map_nil, hEmpty, List.foldr_nil]
    exact hStore.2.2 hFits

def InstructionStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (logical : Bool := false) (op : MemoryInst lang)
    (exhausting : Bool := false) : Prop :=
  memory.InstructionStepWith state next logical false op exhausting

def ControlStep (memory : Memory lang profile)
    (state next : MemoryRegisters language profile)
    (ending : MemoryTerm lang) (exhausting : Bool := false) : Prop := match ending with
  | .jump edge => memory.Transfer edge state next exhausting
  | .branch test yes no =>
    ∃ value afterRead flag, memory.readValue state test = some (value, afterRead) ∧
      language.operands.test value = some flag ∧
      memory.Transfer (if flag then yes else no) afterRead next exhausting
  | .ret value => memory.Return value state next exhausting
  | .trap .. | .invalid => False

/-- Both outcomes share instruction fetch and all guards preceding a reservation.
Only a normal outcome supplies the successor registers to commit. -/
def StepOutcome (memory : Memory lang profile)
    (state next : MemoryRegisters language profile) (exhausting : Bool) (startDefinitions : Bool := false) : Prop :=
  ∃ block, memory.blockAt state = some block ∧
    match block.body[state.instruction]? with
    | some op => memory.InstructionStepWith state next false startDefinitions op.instruction exhausting
    | none => state.instruction = block.body.length ∧ memory.ControlStep state next block.ending exhausting

/-- An instruction commits its ordinary successor or aborts at its original PC.
A failed reservation inside a compound transfer commits no partial writes or effects. -/
def Step (memory : Memory lang profile) (state next : MemoryRegisters language profile)
    (startDefinitions : Bool := false) : Prop :=
  if state.resourceExhausted then False else if next.resourceExhausted then
    next = {state with resourceExhausted := true} ∧
      ∃ partialState, memory.StepOutcome state partialState true startDefinitions
  else memory.StepOutcome state next false startDefinitions

def Initial (memory : Memory lang profile) (state : MemoryRegisters language profile) : Prop :=
  state.frame = memory.entry ∧ state.callers = [] ∧
  (∃ graph, memory.graphAt state.frame = some graph ∧ graph.entry = state.block) ∧
  (∃ block, memory.blockAt state = some block ∧ block.params = []) ∧
  state.instruction = 0 ∧ state.virtualValues = [] ∧
  state.materialized = [] ∧ state.live = [] ∧ state.initialized = [] ∧ state.zeroValues = [] ∧
  state.bytes = Mem.Memory.empty ∧ state.effects = [] ∧ state.ownership = {} ∧
  state.addressIds = [] ∧ state.nextAddressId = 0 ∧ state.movedFields = [] ∧ state.resourceExhausted = false

end Memory

/-! ### `addWithOverflow` arithmetic pair lemmas -/

theorem checked_add_overflow_iff (t : IntTy) (x y : Int) :
    decide (checked t .add x y = .error .overflow) = true ↔ ¬ t.InRange (x + y) := by
  simp only [checked, inRangeOr]
  split
  · rename_i hIn
    constructor
    · intro h; cases (of_decide_eq_true h)
    · intro hNot; exact absurd hIn hNot
  · rename_i hNot
    exact ⟨fun _ => hNot, fun _ => rfl⟩

theorem checked_add_no_overflow_iff (t : IntTy) (x y : Int) :
    decide (checked t .add x y = .error .overflow) = false ↔ t.InRange (x + y) := by
  simp only [checked, inRangeOr]
  split
  · rename_i hIn
    exact ⟨fun _ => hIn, fun _ => decide_eq_false (fun h => by cases h)⟩
  · rename_i hNot
    exact ⟨fun h => absurd rfl (of_decide_eq_false h), fun hIn => absurd hIn hNot⟩

private theorem toBitsAtWidth_lt (width : Nat) (n : Int) :
    toBitsAtWidth width n < 2 ^ width := by
  unfold toBitsAtWidth
  have hPos : (0 : Int) < (2 : Int) ^ width := Int.pow_pos (by decide)
  have hModNonneg : 0 ≤ n % (2 : Int) ^ width := Int.emod_nonneg n (Int.ne_of_gt hPos)
  have hModLt : n % (2 : Int) ^ width < (2 : Int) ^ width := Int.emod_lt_of_pos n hPos
  have hCast : ((n % (2 : Int) ^ width).toNat : Int) = n % (2 : Int) ^ width :=
    Int.toNat_of_nonneg hModNonneg
  have hPow : (((2 ^ width : Nat) : Int)) = (2 : Int) ^ width := by
    exact Int.natCast_pow 2 width
  omega

private theorem wrap_inRange_local (t : IntTy) (n : Int) : t.InRange (wrap t n) := by
  have hLt : toBits t n < 2 ^ t.bits := toBitsAtWidth_lt t.bits n
  unfold wrap ofBits IntTy.InRange
  split
  · rename_i hCond
    obtain ⟨hSigned, hGe⟩ := hCond
    cases t with
    | i8 | i16 | i32 | i64 =>
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, ↓reduceIte, Nat.reduceSub,
        Nat.reducePow, Int.reducePow] at hLt hGe ⊢
      exact ⟨by omega, by omega⟩
    | u8 | u16 | u32 | u64 => cases hSigned
  · rename_i hNot
    have hSignedLt (hs : t.signed = true) : toBits t n < 2 ^ (t.bits - 1) := by
      cases Nat.lt_or_ge (toBits t n) (2 ^ (t.bits - 1)) with
      | inl h => exact h
      | inr h => exact absurd ⟨hs, h⟩ hNot
    cases t with
    | i8 | i16 | i32 | i64 =>
      have hLtHalf := hSignedLt rfl
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, ↓reduceIte, Nat.reduceSub,
        Nat.reducePow, Int.reducePow] at hLt hLtHalf ⊢
      exact ⟨by omega, by omega⟩
    | u8 | u16 | u32 | u64 =>
      simp only [IntTy.min, IntTy.max, IntTy.signed, IntTy.bits, Bool.false_eq_true, ↓reduceIte,
        Nat.reducePow, Int.reducePow] at hLt ⊢
      exact ⟨by omega, by omega⟩

private theorem wrap_of_inRange_local (t : IntTy) (n : Int) (h : t.InRange n) : wrap t n = n := by
  cases t <;>
    simp [IntTy.InRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at h <;>
    simp [wrap, ofBits, toBits, toBitsAtWidth, IntTy.signed, IntTy.bits] <;>
    (try split) <;> omega

theorem wrapping_add_inRange (t : IntTy) (x y : Int) :
    t.InRange (wrapping t .add x y) :=
  wrap_inRange_local t (x + y)

theorem wrapping_add_eq_of_no_overflow (t : IntTy) (x y : Int)
    (h : decide (checked t .add x y = .error .overflow) = false) :
    wrapping t .add x y = x + y :=
  wrap_of_inRange_local t (x + y) ((checked_add_no_overflow_iff t x y).mp h)

theorem plain_add_eq_ok_of_no_overflow (t : IntTy) (x y : Int)
    (h : decide (checked t .add x y = .error .overflow) = false) :
    plain t .add x y = .ok (wrapping t .add x y) := by
  have hIn : t.InRange (x + y) := (checked_add_no_overflow_iff t x y).mp h
  rw [wrapping_add_eq_of_no_overflow t x y h]
  unfold plain checked inRangeOr
  simp [hIn]

theorem plain_add_eq_trap_of_overflow (t : IntTy) (x y : Int)
    (h : decide (checked t .add x y = .error .overflow) = true) :
    plain t .add x y = .trap .overflow := by
  have hErr : checked t .add x y = .error .overflow := of_decide_eq_true h
  unfold plain
  rw [hErr]
  rfl

/-- Combined arithmetic specification of the `(wrapping, overflow)` pair computed by
`addWithOverflow`: the wrapped sum is always in `t.InRange`, equals `x + y` and the plain
result when `overflow = false`, and coincides with `plain t .add x y = .trap .overflow`
when `overflow = true`. -/
theorem addWithOverflow_pair_spec (t : IntTy) (x y : Int) :
    let sum := wrapping t .add x y
    let ovf := decide (checked t .add x y = .error .overflow)
    t.InRange sum ∧
      (ovf = false → plain t .add x y = .ok sum ∧ sum = x + y) ∧
      (ovf = true → plain t .add x y = .trap .overflow) := by
  refine ⟨wrapping_add_inRange t x y,
    fun h => ⟨plain_add_eq_ok_of_no_overflow t x y h, wrapping_add_eq_of_no_overflow t x y h⟩,
    fun h => plain_add_eq_trap_of_overflow t x y h⟩

/-- Render the hosted trap diagnostic from the originating source location using the
single canonical format `Carriers.renderTrapLine`. -/
def trapDiagnostic (project : Project) (kind : TrapKind) (site : SiteId)
    (span : FileSpan) : ByteArray :=
  ((project.fileAt span.file).map fun file =>
    (renderTrapLine kind ⟨site, ⟨file.path.render.text, span.line, span.col⟩⟩).toUTF8)
    |>.getD ByteArray.empty

theorem trapDiagnostic_origin (project : Project) (kind : TrapKind) (origin : Origin) :
    trapDiagnostic project kind origin.operation origin.lexical =
      ((runtimeSite project origin).map fun site =>
        (renderTrapLine kind site).toUTF8).getD ByteArray.empty := by
  unfold trapDiagnostic runtimeSite
  cases project.fileAt origin.lexical.file <;> rfl

def coreTrapDiagnostic {language : CoreLanguage} (core : CoreInput language)
    (kind : TrapKind) (site : SiteId) : ByteArray :=
  match AList.lookup core.machineProgram.origins site with
  | some origin =>
    ((runtimeSite core.project {origin with operation := site}).map fun resolved =>
      (renderTrapLine kind resolved).toUTF8).getD ByteArray.empty
  | none => ByteArray.empty

theorem coreTrapDiagnostic_of_origin {language : CoreLanguage} {core : CoreInput language}
    {kind : TrapKind} {site : SiteId} {origin : Origin}
    {resolved : Concrete.Contracts.DiagnosticSite}
    (hOrigin : AList.lookup core.machineProgram.origins site = some origin)
    (hSite : runtimeSite core.project {origin with operation := site} = some resolved) :
    coreTrapDiagnostic core kind site = (renderTrapLine kind resolved).toUTF8 := by
  simp [coreTrapDiagnostic, hOrigin, hSite]

/-- A hosted observation comes from the terminator at the current PC. Return
operands use the same target-indexed reads as instructions. A callee return
instead steps back to its caller. -/
def Memory.terminalObservation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (memory : Memory lang profile)
    (state : MemoryRegisters language profile) : Option Observation := do
  if state.resourceExhausted then return .resourceExhausted
  let block ← memory.blockAt state
  if let some op := block.body[state.instruction]? then
    let kind ← memory.instructionTrap state op.instruction
    let .trapped status kind _ ← language.hostedResult (.trapped kind op.origin.operation) | none
    return .trapped status kind op.origin.lexical
      (trapDiagnostic memory.project kind op.origin.operation op.origin.lexical)
  else if state.instruction != block.body.length then none else
  match block.ending with
  | .ret (some ⟨_, operand⟩) => do
    if !state.callers.isEmpty || state.frame != memory.entry then none else
    let (value, afterRead) ← memory.readValue state operand
    if !(afterRead.live.all (memory.canEnd afterRead)) then none else
    let .returned status ← language.hostedResult (.returned value) | none
    return .returned status ByteArray.empty
  | .trap kind origin => do
    let .trapped status kind _ ← language.hostedResult (.trapped kind origin.operation) | none
    return .trapped status kind origin.lexical (trapDiagnostic memory.project kind origin.operation origin.lexical)
  | _ => none

namespace MIR

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
  {profile : Profile}

private def logicalPlace {ty : language.operands.Ty} (place : Place _ ty) :
    MemoryPlace _ ty := ⟨place, .virtual⟩

private def logicalRead {ty : language.operands.Ty} :
    Read _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal literal => .literal literal
  | .copy place => .copy (logicalPlace place)
  | .move place => .move (logicalPlace place)

private def logicalOperand (operand : Sigma (Read language.operands.Ty lang.literal)) :
    Sigma (MemoryRead _ lang.literal) := ⟨operand.1, logicalRead operand.2⟩

private def logicalParam (param : Sigma (Place language.operands.Ty)) :
    Sigma (MemoryPlace language.operands.Ty) := ⟨param.1, logicalPlace param.2⟩

private def logicalArgs : {types : List language.operands.Ty} →
    Args (Read _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons read rest => .cons (logicalRead read) (logicalArgs rest)

private def logicalInstruction : MIRInst lang → MemoryInst lang
  | .bind dst src => .bind (logicalPlace dst) (logicalRead src)
  | .assign dst src => .assign (logicalPlace dst) (logicalRead src)
  | .op dst op args checks addresses =>
    .op (logicalPlace dst) op (logicalArgs args) checks (addresses.map logicalParam)
  | .endLifetime place => .endLifetime place
  | .destructure src bindings => .destructure (logicalPlace src)
      (bindings.map fun (field, place) => (field, logicalParam place))
  | .call callee dst args checks => .call callee (dst.map logicalParam) (args.map logicalOperand) checks

private def logicalTerminator : Terminator (Sigma (Read _ lang.literal)) (Read _ lang.literal lang.bool) →
    MemoryTerm lang
  | .ret value => .ret (value.map logicalOperand)
  | .trap kind origin => .trap kind origin
  | .invalid => .invalid
  | .jump edge => .jump ⟨edge.target, edge.arguments.map logicalOperand⟩
  | .branch test yes no => .branch (logicalRead test)
      ⟨yes.target, yes.arguments.map logicalOperand⟩ ⟨no.target, no.arguments.map logicalOperand⟩

/-- The logical evaluator shares CFG and operand rules with Memory. All MIR
roots hold logical values, even when their physical representation needs bytes. -/
def logicalView (mir : MIR lang profile) : Memory lang profile where
  declarations := mir.declarations
  project := mir.project
  entry := mir.entry
  signatures := mir.signatures
  functions := mir.functions.map fun (locals, graph) =>
    let body : MemoryGraph lang := {
      entry := graph.entry
      blocks := graph.blocks.map fun block => {
        id := block.id
        params := block.params.map logicalParam
        body := block.body.map fun inst => ⟨inst.origin, logicalInstruction inst.instruction⟩
        ending := logicalTerminator block.ending
        boundaryOrigins := block.boundaryOrigins } }
    (locals, body)
  layouts := []
  mutableLocals := mir.mutableLocals
  storage := mir.functions.map fun (locals, _) =>
    locals.map fun _ => .virtual

private def instructionAt (mir : MIR lang profile) (state : MemoryRegisters language profile) :
    Option (LocatedOp (MIRInst lang)) := do
  let (_, graph) ← AList.lookup mir.functions state.frame
  let block ← graph.blocks.find? (·.id == state.block)
  block.body[state.instruction]?

def Step (mir : MIR lang profile) (state next : MemoryRegisters language profile) : Prop :=
  let view := mir.logicalView
  match mir.instructionAt state with
  | some op => view.instructionTrap state (logicalInstruction op.instruction) = none ∧
    view.InstructionStep state next true (logicalInstruction op.instruction)
  | none => ∃ block, view.blockAt state = some block ∧ state.instruction = block.body.length ∧
    view.ControlStep state next block.ending

def terminalObservation (mir : MIR lang profile) (state : MemoryRegisters language profile) :
    Option Observation :=
  mir.logicalView.terminalObservation state

end MIR

variable {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

private def entryStateFor (memory : Memory lang profile) (arity : Nat := 0) :
    MemoryRegisters language profile :=
  let graph := memory.graphAt memory.entry
  let id := (graph.map CFG.entry).getD ⟨0⟩
  let block := graph.bind (·.blocks.find? (·.id == id))
  let pc := (block.map fun block =>
    if block.params.length = arity then 0 else block.body.length + 1).getD 0
  ⟨⟨memory.entry, id, pc, [], [], [], [], [], [], []⟩, Mem.Memory.empty, [], [], {}, 0, false⟩

private def hostedResultFor (memory : Memory lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation := do
  match ← memory.terminalObservation state with
  | .returned status bytes => return .returned status bytes
  | .trapped status kind _ bytes =>
    let block ← memory.blockAt state
    let origin ← match block.body[state.instruction]? with
      | some op => some op.origin
      | none => match block.ending with
        | .trap _ origin => some origin
        | _ => none
    let site ← runtimeSite memory.project origin
    return .trapped status kind site bytes
  | .startupRefused status bytes => return .startupRefused status bytes
  | .resourceExhausted => return .resourceExhausted

private def traceFor {A S E : Type}
    (effects : S → List E)
    (result : A → S → Option Concrete.Contracts.Observation)
    (a : A) (state : S) :
    List (ProcessEvent E) :=
  (effects state).map .effect ++
    ((result a state).map fun o => o.stderr.data.toList.map .stderr).getD []

/-- Interpret an invocation of the supplied graph from the selected activation. -/
private def hostedExecution {A S E : Type} (effects : S → List E)
    (step : A → S → S → Prop)
    (start : A → S)
    (observe : A → S → Option Concrete.Contracts.Observation) :
    Semantics A E where
  State := S
  initial a state := state = start a
  initialized _ := ⟨_, rfl⟩
  step a state next :=
    observe a state = none ∧ step a state next ∧
      Concrete.Contracts.TracePrefix (traceFor effects observe a state) (traceFor effects observe a next)
  result a state result := observe a state = some result
  terminal _ _ _ h _ tick := by
    have impossible := h.symm.trans tick.1
    contradiction
  safe a state :=
    (observe a state).isSome = true ∨
    ∃ next, observe a state = none ∧ step a state next ∧
      Concrete.Contracts.TracePrefix (traceFor effects observe a state) (traceFor effects observe a next)
  trace := traceFor effects observe
  permitted _ _ := True
  resultTrace := by
    intro input state result h
    unfold traceFor
    rw [h]
    simp only [Option.map_some, Option.getD_some]
    have stderrExact : ∀ effects : List E, ∀ bytes : List UInt8,
        Concrete.Contracts.stderrBytes (effects.map ProcessEvent.effect ++ bytes.map ProcessEvent.stderr) = bytes := by
      intro effects bytes
      induction effects with
      | nil => induction bytes with
        | nil => rfl
        | cons b rest ih => exact congrArg (b :: ·) ih
      | cons _ _ ih => exact ih
    exact stderrExact _ _
  traceGrows _ _ _ tick := tick.2.2

private def graphExecution {A : Type}
    (step : A → MemoryRegisters language profile → MemoryRegisters language profile → Prop)
    (start : A → MemoryRegisters language profile)
    (observe : A → MemoryRegisters language profile → Option Concrete.Contracts.Observation) :
    Semantics A language.operands.Event :=
  hostedExecution (fun state => state.effects) step start observe

private abbrev CoreState (language : CoreLanguage) :=
  (Σ env : AList ConcreteSpec.Var Own.Descriptor × Declarations, Exec.Config (language.instrumented env.1 env.2)) ×
    List language.operands.Event

/-- Render a Core result using the single runtime diagnostic site resolution on the executing input. -/
def CoreInput.observeHosted (core : CoreInput language) : HostedResult → Option Concrete.Contracts.Observation
  | .returned status => some <| .returned status ByteArray.empty
  | .trapped status kind site => do
    let origin ← AList.lookup core.machineProgram.origins site
    let resolved ← runtimeSite core.project {origin with operation := site}
    return .trapped status kind resolved (coreTrapDiagnostic core kind site)

theorem CoreInput.observeHosted_trapped_eq {core : CoreInput language}
    {status : UInt8} {kind : TrapKind} {site : SiteId} {origin : Origin}
    {resolved : Concrete.Contracts.DiagnosticSite}
    (hOrigin : AList.lookup core.machineProgram.origins site = some origin)
    (hSite : runtimeSite core.project {origin with operation := site} = some resolved) :
    CoreInput.observeHosted core (.trapped status kind site) =
      some (.trapped status kind resolved (renderTrapLine kind resolved).toUTF8) := by
  simp [CoreInput.observeHosted, hOrigin, hSite, coreTrapDiagnostic_of_origin hOrigin hSite]

private def coreHostedResult (core : CoreInput language) (state : CoreState language) :
    Option Concrete.Contracts.Observation := do
  let outcome ← state.1.2.final?
  CoreInput.observeHosted core (← language.hostedResult outcome)

private def coreStep (core : CoreInput language) (state next : CoreState language) : Prop :=
  ∃ p q label, state.1 = ⟨(core.machineProgram.ownership, core.machineProgram.declarations), p⟩ ∧
    next.1 = ⟨(core.machineProgram.ownership, core.machineProgram.declarations), q⟩ ∧ Exec.Step core.machineProgram.body p label q ∧
    next.2 = state.2 ++ label.events

def CoreInput.Execution : Semantics (CoreInput language) language.operands.Event :=
  { hostedExecution Prod.snd coreStep
      (fun core => (⟨(core.machineProgram.ownership, core.machineProgram.declarations), core.machineProgram.initial⟩, [])) coreHostedResult with
    permitted := fun core event =>
      ∃ state events, Exec.Steps core.machineProgram.body core.machineProgram.initial events state ∧
        (event ∈ ((coreHostedResult core (⟨(core.machineProgram.ownership, core.machineProgram.declarations), state⟩, events)).map
          fun result => result.stderr.data.toList.map ProcessEvent.stderr).getD [] ∨
        ∃ label next, Exec.Step core.machineProgram.body state label next ∧
          event ∈ label.events.map ProcessEvent.effect) }

private abbrev coreStateAt (core : CoreInput language) (q : Exec.Config core.machineProgram.machine) :
    CoreState language := (⟨(core.machineProgram.ownership, core.machineProgram.declarations), q⟩, [])

private theorem silent_progress {core : CoreInput language} {p q : Exec.Config core.machineProgram.machine}
    (tick : Exec.step core.machineProgram.body p = some (.next q)) :
    CoreInput.Execution.step core (coreStateAt core p) (coreStateAt core q) ∧
    ∀ r, CoreInput.Execution.step core (coreStateAt core p) r → r = coreStateAt core q := by
  have unfinished : p.final? = none := by
    unfold Exec.Config.final?
    split <;> first | rfl | contradiction | (simp [Exec.step, Exec.stepExit] at tick)
  have unseen : coreHostedResult core (coreStateAt core p) = none := by simp [coreHostedResult, unfinished, coreStateAt]
  constructor
  · exact ⟨unseen, ⟨p, q, .silent, rfl, rfl, .silent tick, rfl⟩,
      by simp [Concrete.Contracts.TracePrefix, traceFor, unseen, coreStateAt]⟩
  · rintro r ⟨_, ⟨before, after, label, start, stop, step, effects⟩, _⟩
    have eq : p = before := by simpa using start
    subst before
    cases step with
    | silent h =>
      rw [tick] at h
      cases Exec.Out.next.inj (Option.some.inj h)
      exact Prod.ext stop effects
    | prim h => rw [tick] at h; cases h

/-- A finite path of deterministic dispatcher steps to a hosted result supplies
progress at every reachable state and fixes the unique process observation. -/
theorem CoreInput.safe_of_silent_path {core : CoreInput language} {q : Exec.Config core.machineProgram.machine}
    (path : Steps (fun p q => Exec.step core.machineProgram.body p = some (.next q)) core.machineProgram.initial q)
    {f hosted result} (final : q.final? = some f) (host : language.hostedResult f = some hosted)
    (render : CoreInput.observeHosted core hosted = some result) :
    ProgramSafe CoreInput.Execution core ∧
    ∀ o, CoreInput.Execution.observes core o ↔ o = result := by
  let S := CoreInput.Execution (language := language)
  have last : S.result core (coreStateAt core q) result := by
    change coreHostedResult _ _ = _
    simp [coreHostedResult, coreStateAt, final, host, render]
  have lift {p r} (h : Steps (fun p r => Exec.step core.machineProgram.body p = some (.next r)) p r) :
      Steps (S.step core) (coreStateAt core p) (coreStateAt core r) := by
    induction h with
    | nil => exact .nil _
    | cons tick _ ih => exact .cons (silent_progress tick).1 ih
  have progress {p r} (h : Steps (fun p r => Exec.step core.machineProgram.body p = some (.next r)) p r)
      (done : S.result core (coreStateAt core r) result) {s}
      (reached : Steps (S.step core) (coreStateAt core p) s) :
      S.result core s result ∨ ∃ next, S.step core s next := by
    induction h generalizing s with
    | nil =>
      cases reached with
      | nil => exact .inl done
      | cons tick _ => exact (S.terminal _ _ _ done _ tick).elim
    | cons tick _ ih =>
      obtain ⟨go, unique⟩ := silent_progress tick
      cases reached with
      | nil => exact .inr ⟨_, go⟩
      | cons first rest => exact ih done ((unique _ first) ▸ rest)
  have allStates {s} (finite : S.finite core s) :
      S.result core s result ∨ ∃ next, S.step core s next := by
    obtain ⟨start, init, steps⟩ := finite
    change start = coreStateAt core core.machineProgram.initial at init
    rw [init] at steps
    exact progress path last steps
  constructor
  · rintro s ⟨finite, noStep, noResult⟩
    rcases allStates finite with observed | ⟨next, tick⟩
    · exact noResult _ observed
    · exact noStep _ tick
  · intro o
    constructor
    · rintro ⟨s, finite, observed⟩
      rcases allStates finite with same | ⟨next, tick⟩
      · exact Option.some.inj (observed.symm.trans same)
      · exact (S.terminal _ _ _ observed _ tick).elim
    · rintro rfl
      exact ⟨_, ⟨_, rfl, lift path⟩, last⟩

/-- Companion to `CoreInput.safe_of_silent_path`: a finite path of deterministic silent
dispatcher steps to a hosted result rules out every infinite execution from the Core entry. -/
theorem CoreInput.noDivergesOfSilentPath {core : CoreInput language}
    {q : Exec.Config core.machineProgram.machine}
    (path : Steps (fun p q => Exec.step core.machineProgram.body p = some (.next q))
      core.machineProgram.initial q)
    {f hosted result} (final : q.final? = some f) (host : language.hostedResult f = some hosted)
    (render : CoreInput.observeHosted core hosted = some result) :
    ¬ ∃ run, CoreInput.Execution.diverges core run := by
  let S := CoreInput.Execution (language := language)
  have last : S.result core (coreStateAt core q) result := by
    change coreHostedResult _ _ = _
    simp [coreHostedResult, coreStateAt, final, host, render]
  rintro ⟨run, init, steps⟩
  change run 0 = coreStateAt core core.machineProgram.initial at init
  have follow {p r}
      (h : Steps (fun a b => Exec.step core.machineProgram.body a = some (.next b)) p r) :
      ∀ k, run k = coreStateAt core p → ∃ n, run n = coreStateAt core r := by
    induction h with
    | nil => intro k hk; exact ⟨k, hk⟩
    | cons tick _ ih =>
      intro k hk
      obtain ⟨_, unique⟩ := silent_progress tick
      exact ih (k + 1) (unique _ (hk ▸ steps k))
  obtain ⟨n, hn⟩ := follow path 0 init
  exact S.terminal core (coreStateAt core q) result last (run (n + 1)) (hn ▸ steps n)

def MIR.Execution : Semantics (MIR lang profile) language.operands.Event :=
  graphExecution MIR.Step (fun mir => entryStateFor mir.logicalView)
    (fun mir => hostedResultFor mir.logicalView)

def Memory.Execution : Semantics (Memory lang profile) language.operands.Event :=
  graphExecution (fun memory state next => memory.Step state next) (fun memory => entryStateFor memory) hostedResultFor

/-- Ordinary hosted results carry the MIR matching obligation. A failed physical
reservation has its own typed observation in Execution. -/
def Memory.MatchingResult (memory : Memory lang profile) (q : MemoryRegisters language profile)
    (o : Concrete.Contracts.Observation) : Prop :=
  if q.resourceExhausted then False else Memory.Execution.result memory q o

theorem Memory.silent_step {memory : Memory lang profile} {q next : MemoryRegisters language profile}
    (ready : memory.terminalObservation q = none) (tick : memory.Step q next)
    (effects : next.effects = q.effects) : Memory.Execution.step memory q next := by
  have absent : hostedResultFor memory q = none := by simp [hostedResultFor, ready]
  refine ⟨absent, tick, ?_⟩
  change Concrete.Contracts.TracePrefix (traceFor _ _ memory q) (traceFor _ _ memory next)
  unfold traceFor
  rw [absent]
  simp only [Option.map_none, Option.getD_none, List.append_nil]
  exact ⟨_, by rw [effects]⟩

theorem Memory.exhausted_result (memory : Memory lang profile) {q : MemoryRegisters language profile}
    (h : q.resourceExhausted = true) : Memory.Execution.result memory q .resourceExhausted := by
  change hostedResultFor memory q = _
  simp [hostedResultFor, Memory.terminalObservation, h]

private def bindsOwner {O : Exec.Operands} (owner : ConcreteSpec.Var) : Exec.Stmt O → Bool
  | .bind var .. => var == owner
  | .call dst .. | .prim dst .. => dst == some owner
  | .seq first second | .cond _ first second _ | .loop _ first _ second _ =>
    bindsOwner owner first || bindsOwner owner second
  | .block _ body => bindsOwner owner body
  | _ => false

private def sourceMutable (core : CoreInput language) (fn : FnId) (root : PlaceId) : Bool :=
  let owner : ConcreteSpec.Var := ⟨root.id⟩
  ((AList.lookup core.machineProgram.ownership owner).any (·.mutable)) &&
    (AList.lookup core.machineProgram.body.fns fn).any (fun function =>
      function.params.any (fun parameter => parameter.1 == owner) || bindsOwner owner function.body)

/-- Retained bindings keep their fresh Core identity as the MIR root number.
Every mutable root must be declared locally and backed by a mutable binding
in the corresponding Core function. Synthetic roots are immutable here. -/
def PreservesOwnerMutability (core : CoreInput language) (mir : MIR lang profile) : Prop :=
  (∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
    ∀ root ty, (root, ty) ∈ locals →
      ((AList.lookup mir.mutableLocals fn).getD []).contains root = sourceMutable core fn root) ∧
  ∀ fn roots root, (fn, roots) ∈ mir.mutableLocals → root ∈ roots →
    sourceMutable core fn root = true ∧
    ∃ locals graph ty, (fn, locals, graph) ∈ mir.functions ∧ (root, ty) ∈ locals

def LowersCore
    (input : Admitted language) (mir : MIR lang profile) : Prop :=
  mir.project = input.core.project ∧ mir.entry = input.core.machineProgram.entry ∧
  PreservesOrigins (List.map Prod.snd input.core.machineProgram.origins) mir.origins ∧
  ConsumesEvidence input.core input.discharges mir ∧
  (PreservesOwnerMutability input.core mir ∧ mir.declarations = input.core.machineProgram.declarations) ∧
  BehaviorRefinement CoreInput.Execution (MIR.Execution (language := language))
    CoreInput.Execution.result (MIR.Execution (language := language)).result input.core mir

def LowersUnchecked
    (input : CoreInput language) (mir : MIR lang profile) : Prop :=
  input.UncheckedWF ∧ LowersCore (language := language) ⟨input, []⟩ mir

def Memory.RepresentsActivation (memory : Memory lang profile)
    (logical physical : MemoryFrame language.operands.Val) (bytes : Mem.Memory)
    (ranges : AList Hybrid.Place (Option Hybrid.Range) := []) : Prop :=
  memory.Represents ⟨logical.frame, logical.live, logical.virtualValues, logical.movedFields⟩
    ⟨physical, bytes, [], [], {}, 0, false⟩ ranges

def MemoryFrame.backingRanges (frame : MemoryFrame language.operands.Val) :
    AList Hybrid.Place (Option Hybrid.Range) :=
  List.map (fun (place, provenance) => (provenance, AList.lookup frame.materialized place)) frame.addressIds

private def addressIdsMatch (memory : Memory lang profile)
    (source target : MemoryFrame language.operands.Val) : Prop :=
  source.addressIds = target.addressIds ∧
  ∀ place id, (place, id) ∈ source.addressIds →
    place ∈ source.live ∧
    (memory.zeroRoot target.frame place = true ∨ (AList.lookup target.materialized place).isSome)

def Memory.RepresentsCheckpoint (memory : Memory lang profile)
    (source target : MemoryRegisters language profile) : Prop :=
  let ranges := target.toMemoryFrame.backingRanges ++
    target.callers.flatMap (·.caller.backingRanges)
  memory.RepresentsActivation source.toMemoryFrame target.toMemoryFrame target.bytes ranges ∧
  source.callers.length = target.callers.length ∧
  (∀ pair ∈ source.callers.zip target.callers,
    memory.RepresentsActivation pair.1.caller pair.2.caller target.bytes ranges ∧
      addressIdsMatch memory pair.1.caller pair.2.caller) ∧
  addressIdsMatch memory source.toMemoryFrame target.toMemoryFrame ∧
  (List.map Prod.fst ranges).Nodup ∧ source.nextAddressId = target.nextAddressId ∧
  source.ownership = target.ownership

/-- Execute the two exact graphs and relate their reachable representations. -/
def LowersMemory (mir : MIR lang profile) (memory : Memory lang profile) : Prop :=
  memory.project = mir.project ∧ memory.entry = mir.entry ∧
  PreservesOrigins mir.origins memory.origins ∧ LayoutsMatch language.layout memory ∧
  (memory.signatures = mir.signatures ∧ memory.declarations = mir.declarations) ∧
  PromotionSound (language := language) mir memory ∧
  RetainsTypedRoots mir memory ∧ memory.LayoutsWF ∧
  memory.TablesWF ∧ mir.logicalView.TablesWF ∧
  let source := MIR.Execution (language := language)
  let target := Memory.Execution (language := language)
  BehaviorRefinement source target source.result (Memory.MatchingResult (language := language)) mir memory ∧
  (ProgramSafe source mir →
    ∃ sim : CheckpointSimulation source target source.result (Memory.MatchingResult (language := language)) mir memory,
      ∀ p q, source.finite mir p → target.finite memory q → sim.related p q →
        memory.RepresentsCheckpoint p q ∧ p.effects = q.effects)

theorem LowersMemory.retainsTypedRoots {mir : MIR lang profile} {memory : Memory lang profile}
    (lowered : LowersMemory (language := language) mir memory) :
    RetainsTypedRoots mir memory :=
  lowered.2.2.2.2.2.2.1

theorem LowersMemory.layoutsWF {mir : MIR lang profile} {memory : Memory lang profile}
    (lowered : LowersMemory (language := language) mir memory) :
    memory.LayoutsWF :=
  lowered.2.2.2.2.2.2.2.1

theorem MIR.logicalView_inventories_eq (mir : MIR lang profile) :
    List.map (fun e => (e.1, e.2.1)) (mir.logicalView (language := language)).functions =
      List.map (fun e => (e.1, e.2.1)) mir.functions := by
  simp only [MIR.logicalView, AList.map, List.map_map]
  rfl

/-- Bridge theorem for production `prepareGraph` / `lowerGraph`: any lowering whose
`(fn, locals)` inventory matches `mir.logicalView.functions` under `mir.logicalView.TablesWF`
satisfies bidirectional `RetainsTypedRoots mir memory`. -/
theorem RetainsTypedRoots.of_logicalView_inventories_eq
    {mir : MIR lang profile} {memory : Memory lang profile}
    (hInv : List.map (fun e => (e.1, e.2.1)) memory.functions =
      List.map (fun e => (e.1, e.2.1)) (mir.logicalView (language := language)).functions)
    (hSrc : (mir.logicalView (language := language)).TablesWF) :
    RetainsTypedRoots mir memory := by
  have hInvMir : List.map (fun e => (e.1, e.2.1)) memory.functions =
      List.map (fun e => (e.1, e.2.1)) mir.functions :=
    hInv.trans (MIR.logicalView_inventories_eq (language := language) mir)
  have hSrcFns : (List.map Prod.fst mir.functions).Nodup := by
    have hKeys : List.map Prod.fst (mir.logicalView (language := language)).functions =
        List.map Prod.fst mir.functions := by
      have h := congrArg (List.map Prod.fst) (MIR.logicalView_inventories_eq (language := language) mir)
      simpa only [List.map_map, Function.comp_def] using h
    exact hKeys ▸ hSrc.1
  have hSrcLocals : ∀ fn locals graph, (fn, locals, graph) ∈ mir.functions →
      (List.map Prod.fst locals).Nodup := by
    intro fn locals graph hMem
    have hPair : (fn, locals) ∈ List.map (fun e => (e.1, e.2.1)) (mir.logicalView (language := language)).functions := by
      rw [MIR.logicalView_inventories_eq (language := language) mir]
      exact List.mem_map.mpr ⟨(fn, locals, graph), hMem, rfl⟩
    obtain ⟨⟨logFn, logLocals, logicalGraph⟩, hLogMem, hEq⟩ := List.mem_map.mp hPair
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj hEq
    exact (hSrc.2.2.2.2.2.2 logFn logLocals logicalGraph hLogMem).1
  exact RetainsTypedRoots.of_inventories_eq hInvMir hSrcFns hSrcLocals

theorem RetainsTypedRoots.logicalView {mir : MIR lang profile}
    (hSrc : (mir.logicalView (language := language)).TablesWF) :
    RetainsTypedRoots mir (mir.logicalView (language := language)) :=
  RetainsTypedRoots.of_logicalView_inventories_eq rfl hSrc

/-- Any MIR that lowers validly to `Memory` is `Materializable` on `profile`: retaining
the exact function-keyed typed root inventory in both directions, matching layouts, and
enforcing `Layout.WellFormed` guarantees every non-scalar or escaping root admits a
well-formed target layout in `mir.declarations`. -/
theorem LowersMemory.materializable {mir : MIR lang profile} {memory : Memory lang profile}
    (lowered : LowersMemory (language := language) mir memory) :
    Materializable (language := language) mir := by
  intro fn srcLocals srcGraph hFn place ty hLocal hNeed
  obtain ⟨dstLocals, dstGraph, hDstFn, hRetained⟩ := lowered.retainsTypedRoots.forward hFn
  obtain ⟨classes, storage, hStorageFn, hStoragePlace, hVirtualIff⟩ :=
    lowered.2.2.2.2.2.1.1 fn srcLocals srcGraph hFn place ty hLocal
  have hMat : storage = .materialized := by
    cases storage with
    | materialized => rfl
    | virtual =>
      obtain ⟨hScalar, hNoEsc⟩ := hVirtualIff.mp rfl
      rcases hNeed with hNone | hEsc
      · cases hs : language.scalarType ty <;> simp [hs] at hScalar hNone
      · exact absurd hEsc hNoEsc
  obtain ⟨classes', hStorageFn', hCase⟩ :=
    lowered.2.2.2.1.1 fn dstLocals dstGraph hDstFn place ty ((hRetained place ty).mp hLocal)
  rw [hStorageFn] at hStorageFn'
  cases Option.some.inj hStorageFn'
  rcases hCase with hVirt | ⟨_, slots, d, hSlots, hDesc, hLayout⟩
  · rw [hStoragePlace, hMat] at hVirt
    cases hVirt
  · refine ⟨d, lowered.2.2.2.2.1.2 ▸ hLayout, ?_⟩
    exact lowered.layoutsWF (fn, slots) (alist_lookup_mem_local hSlots)
      (place, d) (alist_lookup_mem_local hDesc)

/-- Backward root accountability of `LowersMemory`: every function and typed local root
present in `memory` was declared with the exact same type in `mir`. -/
theorem LowersMemory.memoryRoot_declared {mir : MIR lang profile} {memory : Memory lang profile}
    (lowered : LowersMemory (language := language) mir memory)
    {fn dstLocals dstGraph} (hDstFn : (fn, dstLocals, dstGraph) ∈ memory.functions)
    {place ty} (hLocal : (place, ty) ∈ dstLocals) :
    ∃ srcLocals srcGraph, (fn, srcLocals, srcGraph) ∈ mir.functions ∧ (place, ty) ∈ srcLocals := by
  obtain ⟨srcLocals, srcGraph, hSrcFn, hEquiv⟩ := lowered.retainsTypedRoots.backward hDstFn
  exact ⟨srcLocals, srcGraph, hSrcFn, (hEquiv place ty).mp hLocal⟩

def BIR.TrapsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  ∀ fn ∈ ssa.functions, ∀ block ∈ fn.2.2.blocks.toList, ∀ kind origin,
    block.ending = .trap kind origin →
    ∃ after ∈ bir.functions, after.1 = fn.1 ∧
      ∃ next ∈ after.2.2.blocks.toList, next.id = block.id ∧
        ∃ slot, next.ending = .trap slot ∧
          ∃ entry, bir.sites[slot.index]? = some entry ∧ entry.kind = kind ∧ entry.origin = origin

def LCBInput.RuntimeMatches (output : LCBInput profile) (project : Project) : Prop :=
  ∃ diagnostics,
    output.sites.toList.mapM (fun entry => entry.diagnosticLine project) = some diagnostics ∧
    output.runtimeInput = (Backend.Runtime.qbeDiagnostics diagnostics).toUTF8

private def LCBInput.runtimeResult (project : Project) (output : LCBInput profile) :
    LeanCompilerBackend.Verified.Source.Value → Option Concrete.Contracts.Observation
  | .word word => do
    if word.toNat < 256 then
      return .returned (UInt8.ofNat word.toNat) ByteArray.empty
    else
      let entry ← output.sites[word.toNat - 256]?
      let site ← runtimeSite project entry.origin
      return .trapped 134 entry.kind site (renderTrapLine entry.kind site).toUTF8
  | _ => none

def LCBInput.Execution (E : Type) (project : Project) : Semantics (LCBInput profile) E :=
  Backend.lcbSemanticsFor LCBInput.body (LCBInput.runtimeResult project)

namespace BIR

private def register (base : Nat) {ty : language.operands.Ty}
    (value : SSAValue _ ty) : MemoryPlace _ ty :=
  ⟨⟨⟨base + value.id⟩, []⟩, .virtual⟩

private def materialized {ty : language.operands.Ty} (place : Place _ ty) : MemoryPlace _ ty :=
  ⟨place, .materialized⟩

private def registerRead (base : Nat) {ty : language.operands.Ty} :
    SSARead _ lang.literal ty → MemoryRead _ lang.literal ty
  | .literal literal => .literal literal
  | .value value =>
    if language.operands.isCopy ty then .copy (register base value) else .move (register base value)

private def operand (base : Nat) (arg : Sigma (SSARead _ lang.literal)) :
    Sigma (MemoryRead language.operands.Ty lang.literal) :=
  ⟨arg.1, registerRead base arg.2⟩

private def arguments (base : Nat) : {types : List language.operands.Ty} →
    Args (SSARead _ lang.literal) types → Args (MemoryRead _ lang.literal) types
  | _, .nil => .nil
  | _, .cons arg rest => .cons (registerRead base arg) (arguments base rest)

private def instruction (base : Nat) : SSAInst lang → MemoryInst lang
  | .op dst op args addresses =>
    .op (register base dst) op (arguments base args) [] (addresses.map fun p => ⟨p.1, materialized p.2⟩)
  | .addWithOverflow t dst flag lhs rhs =>
    .addWithOverflow t (register base dst) (register base flag)
      (registerRead base lhs) (registerRead base rhs)
  | .load (ty := ty) dst src =>
    .bind (register base dst) (if language.operands.isCopy ty then .copy (materialized src)
      else .move (materialized src))
  | .destructure src bindings =>
    .destructure (match src with
      | .register src => register base src
      | .materialized src => materialized src)
      (bindings.map fun (field, dst) => (field, ⟨dst.1, register base dst.2⟩))
  | .store dst src => .assign (materialized dst) (registerRead base src)
  | .beginLifetime place => .beginLifetime place
  | .endLifetime place => .endLifetime place
  | .call fn dst args => .call fn (dst.map fun p => ⟨p.1, register base p.2⟩)
      (args.map (operand base)) []

private def registerDeclarations (base : Nat) : SSAInst lang → LocalTypes language.operands.Ty
  | .op (ty := ty) dst .. | .load (ty := ty) dst .. => [((register base dst).logical.root, ty)]
  | .addWithOverflow t dst flag .. =>
    [((register base dst).logical.root, lang.intTy t),
     ((register base flag).logical.root, lang.bool)]
  | .destructure _ bindings =>
    bindings.map fun (_, dst) => ((register base dst.2).logical.root, dst.1)
  | .call _ (some ⟨ty, dst⟩) _ => [((register base dst).logical.root, ty)]
  | _ => []

private theorem registerDeclarations_length_eq_slotCount (base : Nat) (inst : SSAInst lang) :
    (registerDeclarations (language := language) (lang := lang) base inst).length = inst.slotCount := by
  cases inst with
  | op _ _ _ _ => rfl
  | addWithOverflow _ _ _ _ _ => rfl
  | load _ _ => rfl
  | destructure _ bindings => simp [registerDeclarations, SSAInst.slotCount]
  | store _ _ => rfl
  | beginLifetime _ => rfl
  | endLifetime _ => rfl
  | call _ dst _ => cases dst <;> rfl

private theorem nodup_of_append_right {α : Type} {xs ys : List α} (h : (xs ++ ys).Nodup) :
    ys.Nodup := by
  induction xs with
  | nil => exact h
  | cons _ rest ih => exact ih (List.nodup_cons.mp h).2

private theorem nodup_of_flatMap_mem {α β : Type} {xs : List α} {f : α → List β}
    (h : (xs.flatMap f).Nodup) {x : α} (hx : x ∈ xs) : (f x).Nodup := by
  induction xs with
  | nil => cases hx
  | cons a rest ih =>
    rw [List.flatMap_cons] at h
    rcases List.mem_cons.mp hx with rfl | hrest
    · exact (List.nodup_append.mp h).1
    · exact ih (List.nodup_append.mp h).2.1 hrest

private def terminator (bir : BIR lang profile) (base : Nat) :
    BIRTerm (Sigma (SSARead _ lang.literal)) (SSARead _ lang.literal lang.bool) → MemoryTerm lang
  | .ret value => .ret (value.map (operand base))
  | .jump edge => .jump ⟨edge.target, edge.arguments.map (operand base)⟩
  | .branch test yes no => .branch (registerRead base test)
      ⟨yes.target, yes.arguments.map (operand base)⟩ ⟨no.target, no.arguments.map (operand base)⟩
  | .trap slot => match bir.sites[slot.index]? with
    | some entry => .trap entry.kind entry.origin
    | none => .invalid

/-- Interpret the exact BIR graph with the shared CFG rules. Register IDs and
byte-backed roots occupy disjoint namespaces. Trap slots resolve in this graph's table. -/
private def registerView {T : Type} (project : Project) (entry : FnId)
    (signatures : AList FnId (FunctionSignature language.operands.Ty))
    (functions : AList FnId (LocalTypes language.operands.Ty ×
      CFG (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang)) T))
    (layouts : AList FnId (AList PlaceId Layout)) (mutableLocals : AList FnId (List PlaceId))
    (declarations : Declarations) (lowerTerm : Nat → T → MemoryTerm lang) :
    Memory lang profile :=
  let views := List.map (fun (fn, locals, graph) =>
    let declaredLimit := List.foldl (fun limit (root, _) => max limit (root.id + 1)) 0 locals
    -- Even an undeclared physical lifetime operand must not alias a virtual register.
    let base := graph.blocks.foldl (fun limit block =>
      block.body.foldl (fun limit op =>
        match op.instruction with
        | .beginLifetime root | .endLifetime root => max limit (root.id + 1)
        | _ => limit) limit) declaredLimit
    let registers := graph.blocks.toList.flatMap fun block =>
      (block.params.map fun p => ((register base p.2).logical.root, p.1)) ++
      (block.body.flatMap fun op => registerDeclarations base op.instruction)
    let body : MemoryGraph lang := ⟨graph.entry, graph.blocks.map fun block =>
      ⟨block.id, block.params.map (fun p => ⟨p.1, register base p.2⟩),
        block.body.map (fun op => ⟨op.origin, instruction base op.instruction⟩),
        lowerTerm base block.ending, block.boundaryOrigins⟩⟩
    ((fn, locals ++ registers, body),
      (fn, List.map (fun p => (p.1, StorageClass.materialized)) locals ++
        List.map (fun p => (p.1, StorageClass.virtual)) registers),
      (fn, registers.map Prod.fst))) functions
  -- Register definitions recur on loop iterations. Their synthetic backing
  -- slots permit replacement of Copy values without granting source mutation.
  let mutableLocals := views.foldl (fun table (_, _, fn, registers) =>
    if registers.isEmpty then table
    else (fn, registers ++ (AList.lookup table fn).getD []) :: AList.erase table fn) mutableLocals
  ⟨project, entry, signatures, views.map Prod.fst, layouts, views.map (·.2.1), mutableLocals, declarations⟩

private theorem registerView_addWithOverflow_aliased_not_tablesWF {T : Type}
    {project : Project} {entry : FnId}
    {signatures : AList FnId (FunctionSignature language.operands.Ty)}
    {functions : AList FnId (LocalTypes language.operands.Ty ×
      CFG (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang)) T)}
    {layouts : AList FnId (AList PlaceId Layout)} {mutableLocals : AList FnId (List PlaceId)}
    {declarations : Declarations} {lowerTerm : Nat → T → MemoryTerm lang}
    {fn : FnId} {locals : LocalTypes language.operands.Ty}
    {graph : CFG (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang)) T}
    {block : Block (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang)) T}
    {origin : Origin} {t : IntTy} {dst : SSAValue _ (lang.intTy t)}
    {flag : SSAValue _ lang.bool} {lhs rhs : SSARead _ lang.literal (lang.intTy t)}
    (hFn : (fn, locals, graph) ∈ functions)
    (hBlock : block ∈ graph.blocks.toList)
    (hOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ block.body)
    (hAlias : dst.id = flag.id) :
    ¬ (registerView (language := language) (lang := lang) (profile := profile)
        project entry signatures functions layouts mutableLocals declarations lowerTerm).TablesWF := by
  intro wf
  have hMemFn : ∃ regs body, (fn, locals ++ regs, body) ∈
      (registerView (language := language) (lang := lang) (profile := profile)
        project entry signatures functions layouts mutableLocals declarations lowerTerm).functions ∧
      ∃ base, regs = graph.blocks.toList.flatMap fun b =>
        (b.params.map fun p => ((register base p.2).logical.root, p.1)) ++
        (b.body.flatMap fun op => registerDeclarations (language := language) (lang := lang) base op.instruction) := by
    unfold registerView
    dsimp only
    rw [List.map_map]
    refine ⟨_, _, List.mem_map.mpr ⟨(fn, locals, graph), hFn, rfl⟩, _, rfl⟩
  obtain ⟨regs, body, hViewFn, base, hRegs⟩ := hMemFn
  have hLocalsNodup : (List.map Prod.fst (locals ++ regs)).Nodup :=
    (wf.2.2.2.2.2.2 fn (locals ++ regs) body hViewFn).1
  rw [List.map_append] at hLocalsNodup
  have hRegsNodup : (List.map Prod.fst regs).Nodup := nodup_of_append_right hLocalsNodup
  rw [hRegs, List.map_flatMap] at hRegsNodup
  have hBlockNodup := nodup_of_flatMap_mem hRegsNodup hBlock
  rw [List.map_append] at hBlockNodup
  have hBodyNodup := nodup_of_append_right hBlockNodup
  rw [List.map_flatMap] at hBodyNodup
  have hOpNodup := nodup_of_flatMap_mem hBodyNodup hOp
  simp only [registerDeclarations, register, hAlias, List.map_cons, List.map_nil,
    List.nodup_cons, List.mem_singleton] at hOpNodup
  exact hOpNodup.1 trivial


def memoryView (bir : BIR lang profile) : Memory lang profile :=
  registerView bir.project bir.entry bir.signatures bir.functions bir.layouts bir.mutableLocals bir.declarations (terminator bir)

def entryState (bir : BIR lang profile) : MemoryRegisters language profile :=
  entryStateFor (bir.memoryView (language := language))

private def stepFor (memory : Memory lang profile) (state next : MemoryRegisters language profile) : Prop :=
  memory.Step state next (startDefinitions := true)

def step (bir : BIR lang profile) (state next : MemoryRegisters language profile) : Prop :=
  stepFor (bir.memoryView (language := language)) state next

def hostedResult (bir : BIR lang profile) (state : MemoryRegisters language profile) :
    Option Concrete.Contracts.Observation :=
  hostedResultFor (bir.memoryView (language := language)) state

private def registerExecution : Semantics (Memory lang profile) language.operands.Event :=
  graphExecution stepFor (fun memory => entryStateFor memory) hostedResultFor

def Execution : Semantics (BIR lang profile) language.operands.Event :=
  registerExecution.onInput (memoryView (language := language))

/-- An aliased two-result `addWithOverflow` (`dst.id = flag.id`) breaks `TablesWF` in
the decoded `BIR.memoryView`. -/
theorem memoryView_addWithOverflow_aliased_not_tablesWF {bir : BIR lang profile}
    {fn : FnId} {locals : LocalTypes language.operands.Ty} {graph : BIRGraph lang}
    {block : Block (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang))
      (BIRTerm (Sigma (SSARead _ lang.literal)) (SSARead _ lang.literal lang.bool))}
    {origin : Origin} {t : IntTy} {dst : SSAValue _ (lang.intTy t)}
    {flag : SSAValue _ lang.bool} {lhs rhs : SSARead _ lang.literal (lang.intTy t)}
    (hFn : (fn, locals, graph) ∈ bir.functions)
    (hBlock : block ∈ graph.blocks.toList)
    (hOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ block.body)
    (hAlias : dst.id = flag.id) :
    ¬ (bir.memoryView (language := language)).TablesWF :=
  registerView_addWithOverflow_aliased_not_tablesWF hFn hBlock hOp hAlias

end BIR

/-- Canonical LLVM return type of the hosted entry body (`i32`), matching the call
`call i32 @concrete_body()` inside `Backend.Runtime.llvmDiagnostics`. -/
def llvmBodyReturnType : Backend.LLVM.TypeCode := .integer .u32

/-- Canonical LLVM global symbol of the hosted entry body (`@concrete_body`), matching
the call inside `Backend.Runtime.llvmDiagnostics`. -/
def llvmBodyGlobal : Backend.LLVM.Global := .body

/-- Canonical LLVM parameter list of the hosted entry body (`()`), matching the
0-argument call inside `Backend.Runtime.llvmDiagnostics`. -/
def llvmBodyParams : List (Backend.LLVM.TypeCode × Backend.LLVM.Local) := []

/-- External LLVM definition header of `@concrete_body` required in the component module. -/
def llvmBodyDefinition : Backend.LLVM.Item :=
  .define false llvmBodyReturnType llvmBodyGlobal llvmBodyParams

/-- Matching LLVM `declare` item for `@concrete_body` prepended to the standalone
launcher module, using the exact same return type, global symbol, and parameter types as
`llvmBodyDefinition`. -/
def llvmBodyDeclaration : Backend.LLVM.Item :=
  .declare llvmBodyReturnType llvmBodyGlobal (llvmBodyParams.map Prod.fst)

/-- Whether an LLVM item declares or defines the hosted entry body symbol `@concrete_body`. -/
def isLLVMBodyHeader : Backend.LLVM.Item → Bool
  | .define _ _ .body _ => true
  | .declare _ .body _ => true
  | _ => false

/-- Exact LLVM assembly items of a standalone two-module launcher: the matching external
body declaration (`llvmBodyDeclaration`) followed by
`Backend.Runtime.llvmDiagnostics diagnostics`. Used by both `LauncherMatches` and
`EmitLLVM.launcherModule` as a single source of truth. -/
def launcherItems (diagnostics : List String) : List Backend.LLVM.Item :=
  llvmBodyDeclaration :: Backend.Runtime.llvmDiagnostics diagnostics

/-- The two-module LLVM launcher contract:
1. `bir.sites` resolves in `bir.project` via `TrapEntry.diagnosticLine` to `diagnostics`;
2. `output.component.items` has `llvmBodyDefinition` (`define i32 @concrete_body()`) as its
   unique `@concrete_body` header (non-internal, matching symbol, return type, and parameters);
3. `output.launcher.items` equals `launcherItems diagnostics` — the matching declaration
   `llvmBodyDeclaration` (`declare i32 @concrete_body()`) followed by
   `Backend.Runtime.llvmDiagnostics diagnostics`. -/
def LauncherMatches {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  ∃ diagnostics,
    bir.sites.toList.mapM (fun entry => entry.diagnosticLine bir.project) = some diagnostics ∧
    output.component.items.filter isLLVMBodyHeader = [llvmBodyDefinition] ∧
    output.launcher.items = launcherItems diagnostics

theorem LauncherMatches.launcher_unique {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile} {left right : LLVMInput profile}
    (hLeft : LauncherMatches bir left) (hRight : LauncherMatches bir right) :
    left.launcher = right.launcher := by
  obtain ⟨d₁, hMap₁, _, hItems₁⟩ := hLeft
  obtain ⟨d₂, hMap₂, _, hItems₂⟩ := hRight
  have hDiag : d₁ = d₂ := Option.some.inj (hMap₁.symm.trans hMap₂)
  subst hDiag
  cases left; cases right
  simp only at hItems₁ hItems₂
  exact congrArg LLVMModule.mk (hItems₁.trans hItems₂.symm)

theorem LauncherMatches.launcher_items_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile} {diagnostics : List String}
    (hMatch : LauncherMatches bir output)
    (hDiag : bir.sites.toList.mapM (fun entry => entry.diagnosticLine bir.project) = some diagnostics) :
    output.launcher.items = launcherItems diagnostics := by
  obtain ⟨d, hMap, _, hItems⟩ := hMatch
  have hEq : d = diagnostics := Option.some.inj (hMap.symm.trans hDiag)
  subst hEq
  exact hItems

/-- Every `LauncherMatches` handoff has `llvmBodyDefinition` as the unique `@concrete_body`
header in `output.component.items`. -/
theorem LauncherMatches.component_body_header_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (hMatch : LauncherMatches bir output) :
    output.component.items.filter isLLVMBodyHeader = [llvmBodyDefinition] :=
  let ⟨_, _, hComp, _⟩ := hMatch
  hComp

/-- Every `LauncherMatches` handoff externally defines `@concrete_body` in `output.component`. -/
theorem LauncherMatches.component_defines_body {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (hMatch : LauncherMatches bir output) :
    llvmBodyDefinition ∈ output.component.items := by
  obtain ⟨_, _, hComp, _⟩ := hMatch
  have hMem : llvmBodyDefinition ∈ output.component.items.filter isLLVMBodyHeader := by
    rw [hComp]; exact List.mem_cons_self
  exact (List.mem_filter.mp hMem).1

/-- The declaration at the head of `output.launcher.items` matches `output.component`'s
unique external body definition in return type, global symbol, and parameter types. -/
theorem LauncherMatches.body_decl_matches_define {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (hMatch : LauncherMatches bir output) :
    ∃ result fn params rest,
      output.component.items.filter isLLVMBodyHeader = [.define false result fn params] ∧
      output.launcher.items = .declare result fn (params.map Prod.fst) :: rest := by
  obtain ⟨diagnostics, _, hComp, hLaunch⟩ := hMatch
  exact ⟨llvmBodyReturnType, llvmBodyGlobal, llvmBodyParams,
    Backend.Runtime.llvmDiagnostics diagnostics, hComp, hLaunch⟩

/-- Structural obligations of the two-module LLVM handoff. The target datalayout and
triple headers are rendered unconditionally from `profile` inside `LLVMInput.bytesAt`,
and semantic correctness of the exact rendered component and launcher bytes is the
trusted-base assumption `P_print`. -/
def LLVMMetadataMatches {Ty : Type} {lang : LoweringLanguage Ty}
    (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  profile.backend = .llvm ∧ output.sites = bir.sites ∧ LauncherMatches bir output

theorem LLVMMetadataMatches.launcherMatches {Ty : Type} {lang : LoweringLanguage Ty}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (h : LLVMMetadataMatches bir output) : LauncherMatches bir output :=
  h.2.2

def SSA.memoryView (ssa : SSA lang profile) : Memory lang profile :=
  BIR.registerView ssa.project ssa.entry ssa.signatures ssa.functions ssa.layouts ssa.mutableLocals ssa.declarations fun base ending =>
    match ending with
    | .ret value => .ret (value.map (BIR.operand base))
    | .jump edge => .jump ⟨edge.target, edge.arguments.map (BIR.operand base)⟩
    | .branch test yes no => .branch (BIR.registerRead base test)
        ⟨yes.target, yes.arguments.map (BIR.operand base)⟩
        ⟨no.target, no.arguments.map (BIR.operand base)⟩
    | .trap kind origin => .trap kind origin
    | .invalid => .invalid

/-- An aliased two-result `addWithOverflow` (`dst.id = flag.id`) breaks `TablesWF` in
the decoded `SSA.memoryView`. -/
theorem SSA.memoryView_addWithOverflow_aliased_not_tablesWF {ssa : SSA lang profile}
    {fn : FnId} {locals : LocalTypes language.operands.Ty} {graph : SSAGraph lang}
    {block : Block (Sigma (SSAValue language.operands.Ty)) (LocatedOp (SSAInst lang))
      (Terminator (Sigma (SSARead _ lang.literal)) (SSARead _ lang.literal lang.bool))}
    {origin : Origin} {t : IntTy} {dst : SSAValue _ (lang.intTy t)}
    {flag : SSAValue _ lang.bool} {lhs rhs : SSARead _ lang.literal (lang.intTy t)}
    (hFn : (fn, locals, graph) ∈ ssa.functions)
    (hBlock : block ∈ graph.blocks.toList)
    (hOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ block.body)
    (hAlias : dst.id = flag.id) :
    ¬ (ssa.memoryView (language := language)).TablesWF :=
  BIR.registerView_addWithOverflow_aliased_not_tablesWF hFn hBlock hOp hAlias

def SSA.Execution : Semantics (SSA lang profile) language.operands.Event :=
  BIR.registerExecution.onInput (SSA.memoryView (language := language))

/-- Abbreviation for an SSA CFG block. -/
abbrev SSABlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

/-- A Memory instruction is a residual plain addition of width `t` writing `dst` from
operands `lhs` and `rhs` when it performs `.arithmetic (.plain .add)` on
`[lang.intTy t, lang.intTy t] → lang.intTy t` with at least one residual `.overflow` check. -/
inductive MemoryInst.ResidualPlainAdd {Ty : Type} {lang : LoweringLanguage Ty}
    (t : IntTy) (dst : MemoryPlace Ty (lang.intTy t))
    (lhs rhs : MemoryRead Ty lang.literal (lang.intTy t)) : MemoryInst lang → Prop where
  | mk {op : lang.operation (.arithmetic (.plain .add)) [lang.intTy t, lang.intTy t] (lang.intTy t)}
      {checks : List OperationCheck} {addresses : List (Sigma (MemoryPlace Ty))}
      (hRes : ∃ c ∈ checks, c.disposition = .residual ∧ c.trap = .overflow) :
      MemoryInst.ResidualPlainAdd t dst lhs rhs
        (.op (opcode := .arithmetic (.plain .add))
          (types := [lang.intTy t, lang.intTy t]) (ty := lang.intTy t)
          dst op (.cons lhs (.cons rhs .nil)) checks addresses)

/-- A Memory instruction is a residual plain addition of width `t`. -/
def MemoryInst.IsResidualPlainAdd {Ty : Type} {lang : LoweringLanguage Ty}
    (t : IntTy) (inst : MemoryInst lang) : Prop :=
  ∃ dst lhs rhs, inst.ResidualPlainAdd t dst lhs rhs

/-- A Memory instruction is a residual plain addition at any width. -/
def MemoryInst.HasResidualAdd {Ty : Type} {lang : LoweringLanguage Ty} :
    MemoryInst lang → Prop
  | @MemoryInst.op _ _ (.arithmetic (.plain .add)) _ _ _ _ _ checks _ =>
    ∃ c ∈ checks, c.disposition = .residual ∧ c.trap = .overflow
  | _ => False

theorem MemoryInst.ResidualPlainAdd.hasResidualAdd {Ty : Type} {lang : LoweringLanguage Ty}
    {t : IntTy} {dst : MemoryPlace Ty (lang.intTy t)}
    {lhs rhs : MemoryRead Ty lang.literal (lang.intTy t)} {inst : MemoryInst lang}
    (h : inst.ResidualPlainAdd t dst lhs rhs) : inst.HasResidualAdd := by
  cases h with
  | mk hRes => exact hRes

theorem MemoryInst.ResidualPlainAdd.isResidualPlainAdd {Ty : Type} {lang : LoweringLanguage Ty}
    {t : IntTy} {dst : MemoryPlace Ty (lang.intTy t)}
    {lhs rhs : MemoryRead Ty lang.literal (lang.intTy t)} {inst : MemoryInst lang}
    (h : inst.ResidualPlainAdd t dst lhs rhs) : inst.IsResidualPlainAdd t :=
  ⟨dst, lhs, rhs, h⟩

/-- A Memory CFG contains at least one residual plain addition. -/
def MemoryGraph.HasResidualAdd {Ty : Type} {lang : LoweringLanguage Ty}
    (graph : MemoryGraph lang) : Prop :=
  ∃ block ∈ graph.blocks.toList, ∃ op ∈ block.body, op.instruction.HasResidualAdd

/-- Virtual local renaming map from Memory `PlaceId`s to their current SSA read image. -/
abbrev SSAEnv (Ty : Type) (lang : LoweringLanguage Ty) :=
  PlaceId → Option (Sigma (SSARead Ty lang.literal))

def SSAEnv.empty {Ty : Type} {lang : LoweringLanguage Ty} : SSAEnv Ty lang :=
  fun _ => none

def SSAEnv.set {Ty : Type} {lang : LoweringLanguage Ty}
    (env : SSAEnv Ty lang) (root : PlaceId) (val : Sigma (SSARead Ty lang.literal)) :
    SSAEnv Ty lang :=
  fun p => if p = root then some val else env p

/-- Parallel update of virtual local bindings from Memory block parameters and their
corresponding SSA reads. Materialized parameters do not enter the virtual renaming. -/
def SSAEnv.bindParams {Ty : Type} {lang : LoweringLanguage Ty}
    (base : SSAEnv Ty lang) :
    List (Sigma (MemoryPlace Ty)) → List (Sigma (SSARead Ty lang.literal)) → SSAEnv Ty lang
  | [], _ => base
  | _, [] => base
  | ⟨_, mParam⟩ :: mRest, sArg :: sRest =>
    let rest := SSAEnv.bindParams base mRest sRest
    match mParam.storage with
    | .virtual => rest.set mParam.logical.root sArg
    | .materialized => rest

/-- Every instruction in a read-preparation prefix is an `SSAInst.load`. -/
def AllLoads {Ty : Type} {lang : LoweringLanguage Ty} :
    List (LocatedOp (SSAInst lang)) → Prop
  | [] => True
  | op :: rest =>
    (∃ (ty : Ty) (dst : SSAValue Ty ty) (src : Place Ty ty), op.instruction = .load dst src) ∧
      AllLoads rest

/-- `sRead` is the SSA image of Memory operand `mRead` under virtual renaming `env` and
preceding operand loads `opLoads`:
- a literal maps to the exact same literal;
- a virtual read (`path = []`) resolves through `env` at its root with the exact type;
- a materialized read resolves to an SSA value loaded from its exact logical place in `opLoads`. -/
def ReadImage {Ty : Type} {lang : LoweringLanguage Ty}
    (env : SSAEnv Ty lang) (opLoads : List (LocatedOp (SSAInst lang)))
    {ty : Ty} (mRead : MemoryRead Ty lang.literal ty) (sRead : SSARead Ty lang.literal ty) : Prop :=
  match mRead with
  | .literal lit => sRead = .literal lit
  | .copy place | .move place =>
    match place.storage with
    | .virtual => place.logical.path = [] ∧ env place.logical.root = some ⟨ty, sRead⟩
    | .materialized =>
      ∃ v, sRead = .value v ∧ ∃ lo ∈ opLoads, lo.instruction = .load v place.logical

/-- Pointwise lift of `ReadImage` to typed operator argument lists. -/
inductive ArgsImage {Ty : Type} {lang : LoweringLanguage Ty}
    (env : SSAEnv Ty lang) (opLoads : List (LocatedOp (SSAInst lang))) :
    {types : List Ty} → Args (MemoryRead Ty lang.literal) types →
    Args (SSARead Ty lang.literal) types → Prop where
  | nil : ArgsImage env opLoads .nil .nil
  | cons {ty : Ty} {types : List Ty}
      {mArg : MemoryRead Ty lang.literal ty} {sArg : SSARead Ty lang.literal ty}
      {mRest : Args (MemoryRead Ty lang.literal) types}
      {sRest : Args (SSARead Ty lang.literal) types} :
      ReadImage env opLoads mArg sArg →
      ArgsImage env opLoads mRest sRest →
      ArgsImage env opLoads (.cons mArg mRest) (.cons sArg sRest)

/-- Pointwise lift of `ReadImage` to sigma-typed operand lists (calls and CFG edges). -/
inductive OperandsImage {Ty : Type} {lang : LoweringLanguage Ty}
    (env : SSAEnv Ty lang) (opLoads : List (LocatedOp (SSAInst lang))) :
    List (Sigma (MemoryRead Ty lang.literal)) →
    List (Sigma (SSARead Ty lang.literal)) → Prop where
  | nil : OperandsImage env opLoads [] []
  | cons {ty : Ty}
      {mArg : MemoryRead Ty lang.literal ty} {sArg : SSARead Ty lang.literal ty}
      {mRest : List (Sigma (MemoryRead Ty lang.literal))}
      {sRest : List (Sigma (SSARead Ty lang.literal))} :
      ReadImage env opLoads mArg sArg →
      OperandsImage env opLoads mRest sRest →
      OperandsImage env opLoads (⟨ty, mArg⟩ :: mRest) (⟨ty, sArg⟩ :: sRest)

/-- Lowering a destination write of `sRead` into `mDst` at `origin`:
- a virtual destination (`path = []`) updates `env` with no store;
- a materialized destination emits `⟨origin, .store mDst.logical sRead⟩` and leaves `env` unchanged. -/
def WriteDest {Ty : Type} {lang : LoweringLanguage Ty}
    (origin : Origin) {ty : Ty} (mDst : MemoryPlace Ty ty) (sRead : SSARead Ty lang.literal ty)
    (env env' : SSAEnv Ty lang) (stores : List (LocatedOp (SSAInst lang))) : Prop :=
  match mDst.storage with
  | .virtual =>
    mDst.logical.path = [] ∧ stores = [] ∧ env' = env.set mDst.logical.root ⟨ty, sRead⟩
  | .materialized =>
    stores = [⟨origin, .store mDst.logical sRead⟩] ∧ env' = env

/-- Lowering destructure field destination bindings in order. -/
inductive WriteBindings {Ty : Type} {lang : LoweringLanguage Ty} (origin : Origin) :
    List (FieldId × Sigma (MemoryPlace Ty)) →
    List (FieldId × Sigma (SSAValue Ty)) →
    SSAEnv Ty lang → SSAEnv Ty lang → List (LocatedOp (SSAInst lang)) → Prop where
  | nil (env : SSAEnv Ty lang) : WriteBindings origin [] [] env env []
  | cons {field : FieldId} {ty : Ty} {mDst : MemoryPlace Ty ty} {sVal : SSAValue Ty ty}
      {mRest : List (FieldId × Sigma (MemoryPlace Ty))}
      {sRest : List (FieldId × Sigma (SSAValue Ty))}
      {env envMid env' : SSAEnv Ty lang}
      {s1 s2 : List (LocatedOp (SSAInst lang))} :
      WriteDest origin mDst (.value sVal) env envMid s1 →
      WriteBindings origin mRest sRest envMid env' s2 →
      WriteBindings origin ((field, ⟨ty, mDst⟩) :: mRest) ((field, ⟨ty, sVal⟩) :: sRest)
        env env' (s1 ++ s2)

/-- Lowering parameter writes for materialized block or function parameters. -/
inductive ParamStores {Ty : Type} {lang : LoweringLanguage Ty} :
    List (Sigma (MemoryPlace Ty)) →
    List (Sigma (SSARead Ty lang.literal)) →
    List (LocatedOp (SSAInst lang)) → Prop where
  | nil : ParamStores [] [] []
  | consVirtual {ty : Ty} {mParam : MemoryPlace Ty ty} {sArg : SSARead Ty lang.literal ty}
      {mRest : List (Sigma (MemoryPlace Ty))} {sRest : List (Sigma (SSARead Ty lang.literal))}
      {stores : List (LocatedOp (SSAInst lang))} :
      mParam.storage = .virtual →
      ParamStores mRest sRest stores →
      ParamStores (⟨ty, mParam⟩ :: mRest) (⟨ty, sArg⟩ :: sRest) stores
  | consMaterialized {ty : Ty} {mParam : MemoryPlace Ty ty} {sArg : SSARead Ty lang.literal ty}
      {mRest : List (Sigma (MemoryPlace Ty))} {sRest : List (Sigma (SSARead Ty lang.literal))}
      {origin : Origin} {prefixOps stores : List (LocatedOp (SSAInst lang))} :
      mParam.storage = .materialized →
      (prefixOps = [⟨origin, .store mParam.logical sArg⟩] ∨
       prefixOps = [⟨origin, .beginLifetime mParam.logical.root⟩, ⟨origin, .store mParam.logical sArg⟩]) →
      ParamStores mRest sRest stores →
      ParamStores (⟨ty, mParam⟩ :: mRest) (⟨ty, sArg⟩ :: sRest) (prefixOps ++ stores)

/-- An SSA CFG edge `sEdge` to `sTarget` transfers virtual environment `envAfter` into
`envTarget`: argument types match `sTarget.params`, and every root bound in `envTarget`
either comes from a matching `(sEdge.arguments[idx], sTarget.params[idx])` transfer slot
whose edge argument matches `envAfter`, or stays unchanged from `envAfter`. -/
def EdgeTransfersEnv {Ty : Type} {lang : LoweringLanguage Ty}
    (envAfter : SSAEnv Ty lang)
    (sEdge : CFGEdge (Sigma (SSARead Ty lang.literal)))
    (sTarget : SSABlock lang)
    (envTarget : SSAEnv Ty lang) : Prop :=
  sTarget.id = sEdge.target ∧
  sEdge.arguments.map Sigma.fst = sTarget.params.map Sigma.fst ∧
  ∀ root ty targetRead, envTarget root = some ⟨ty, targetRead⟩ →
    (∃ (idx : Nat) (paramVal : SSAValue Ty ty) (argRead : SSARead Ty lang.literal ty),
      targetRead = .value paramVal ∧
      sTarget.params[idx]? = some (⟨ty, paramVal⟩ : Sigma (SSAValue Ty)) ∧
      sEdge.arguments[idx]? = some (⟨ty, argRead⟩ : Sigma (SSARead Ty lang.literal)) ∧
      envAfter root = some ⟨ty, argRead⟩) ∨
    envAfter root = some ⟨ty, targetRead⟩

/-- Lowering a Memory CFG edge `mEdge` to an SSA edge `sEdge` under `blockMap` and `envAt`. -/
def EdgeLowers {Ty : Type} {lang : LoweringLanguage Ty}
    (mGraph : MemoryGraph lang) (sGraph : SSAGraph lang)
    (blockMap : BlockId → BlockId) (envAt : BlockId → SSAEnv Ty lang)
    (env : SSAEnv Ty lang) (sPrefix : List (LocatedOp (SSAInst lang)))
    (mEdge : CFGEdge (Sigma (MemoryRead Ty lang.literal)))
    (sEdge : CFGEdge (Sigma (SSARead Ty lang.literal))) : Prop :=
  ∃ mTarget ∈ mGraph.blocks.toList, mTarget.id = mEdge.target ∧
    ∃ opLoads stores sArgReads,
      sPrefix = opLoads ++ stores ∧
      AllLoads opLoads ∧
      OperandsImage env opLoads mEdge.arguments sArgReads ∧
      ParamStores mTarget.params sArgReads stores ∧
      ∃ sTarget ∈ sGraph.blocks.toList, sTarget.id = blockMap mEdge.target ∧
        EdgeTransfersEnv (SSAEnv.bindParams env mTarget.params sArgReads) sEdge sTarget (envAt mEdge.target)

/-- Lowering a Memory block terminator `mEnding` at the end of an SSA block segment. -/
def TermLowers {Ty : Type} {lang : LoweringLanguage Ty}
    (mGraph : MemoryGraph lang) (sGraph : SSAGraph lang)
    (blockMap : BlockId → BlockId) (envAt : BlockId → SSAEnv Ty lang)
    (env : SSAEnv Ty lang) (sRem : List (LocatedOp (SSAInst lang)))
    (mEnding : MemoryTerm lang)
    (sEnding : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) : Prop :=
  match mEnding with
  | .ret none => sRem = [] ∧ sEnding = .ret none
  | .ret (some ⟨ty, mRead⟩) =>
    ∃ (sRead : SSARead Ty lang.literal ty),
      AllLoads sRem ∧ ReadImage env sRem mRead sRead ∧ sEnding = .ret (some ⟨ty, sRead⟩)
  | .trap kind origin => sRem = [] ∧ sEnding = .trap kind origin
  | .invalid => sRem = [] ∧ sEnding = .invalid
  | .jump mEdge =>
    ∃ sEdge, sEnding = .jump sEdge ∧
      EdgeLowers mGraph sGraph blockMap envAt env sRem mEdge sEdge
  | .branch mTest mYes mNo =>
    ∃ sTest sYes sNo testLoads yesPrefix noPrefix,
      sRem = testLoads ++ yesPrefix ++ noPrefix ∧
      AllLoads testLoads ∧ ReadImage env testLoads mTest sTest ∧
      sEnding = .branch sTest sYes sNo ∧
      EdgeLowers mGraph sGraph blockMap envAt env yesPrefix mYes sYes ∧
      EdgeLowers mGraph sGraph blockMap envAt env noPrefix mNo sNo

/-- A specific SSA block `block` guards an `addWithOverflow` of width `t` at `origin`
producing distinct `(dst, flag)` from operands `(lhs, rhs)`, branching on `flag` to an
empty `.trap .overflow origin` block on `true` and supplying the wrapped value `.value dst`
along `no` to continuation block `contBlock` as parameter `contVal` on `false`. -/
def SSAGraph.BlockGuardsAddWithOverflow {Ty : Type} {lang : LoweringLanguage Ty}
    (graph : SSAGraph lang) (block : SSABlock lang)
    (t : IntTy) (origin : Origin)
    (dst : SSAValue Ty (lang.intTy t)) (flag : SSAValue Ty lang.bool)
    (lhs rhs : SSARead Ty lang.literal (lang.intTy t))
    (no : CFGEdge (Sigma (SSARead Ty lang.literal)))
    (contBlock : SSABlock lang) (contVal : SSAValue Ty (lang.intTy t)) : Prop :=
  dst.id ≠ flag.id ∧
  block.body.getLast? = some ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∧
  ∃ yes trapBlock,
    block.ending = .branch (.value flag) yes no ∧
    yes.arguments = [] ∧
    trapBlock ∈ graph.blocks.toList ∧ trapBlock.id = yes.target ∧
    trapBlock.params = [] ∧ trapBlock.body = [] ∧
    trapBlock.ending = .trap .overflow origin ∧
    contBlock ∈ graph.blocks.toList ∧ contBlock.id = no.target ∧
    contBlock.id ≠ trapBlock.id ∧
    no.arguments.map Sigma.fst = contBlock.params.map Sigma.fst ∧
    ∃ idx : Nat,
      no.arguments[idx]? = some (⟨lang.intTy t, .value dst⟩ : Sigma (SSARead Ty lang.literal)) ∧
      contBlock.params[idx]? = some (⟨lang.intTy t, contVal⟩ : Sigma (SSAValue Ty))

/-- Some block in `graph` guards an `addWithOverflow` of width `t` at `origin` on the
given SSA operands `lhs` and `rhs`, supplying the wrapped value along `no` to `contBlock`
as parameter `contVal` and branching to an empty `.overflow` trap block at `origin` on `true`. -/
def SSAGraph.GuardsAddWithOverflow {Ty : Type} {lang : LoweringLanguage Ty}
    (graph : SSAGraph lang) (t : IntTy) (origin : Origin)
    (lhs rhs : SSARead Ty lang.literal (lang.intTy t))
    (no : CFGEdge (Sigma (SSARead Ty lang.literal)))
    (contBlock : SSABlock lang) (contVal : SSAValue Ty (lang.intTy t)) : Prop :=
  ∃ block ∈ graph.blocks.toList, ∃ dst flag,
    graph.BlockGuardsAddWithOverflow block t origin dst flag lhs rhs no contBlock contVal

/-- Structural lowering of a Memory block tail `(mBody, mEnding)` into `sGraph` starting
from SSA block `sBlock`, remaining SSA instructions `sRem`, and virtual renaming `env`,
recording every emitted `(guardBlock, addWithOverflowOp)` occurrence in `emitted`. -/
inductive SSAGraph.InstsLower {Ty : Type} {lang : LoweringLanguage Ty}
    (mGraph : MemoryGraph lang) (sGraph : SSAGraph lang)
    (blockMap : BlockId → BlockId) (envAt : BlockId → SSAEnv Ty lang) :
    SSABlock lang → SSAEnv Ty lang →
    List (LocatedOp (MemoryInst lang)) → MemoryTerm lang →
    List (LocatedOp (SSAInst lang)) →
    List (SSABlock lang × LocatedOp (SSAInst lang)) → Prop where
  | nil {sBlock env mEnding sRem} :
      TermLowers mGraph sGraph blockMap envAt env sRem mEnding sBlock.ending →
      InstsLower mGraph sGraph blockMap envAt sBlock env [] mEnding sRem []
  | bind {mOrigin ty} {mDst : MemoryPlace Ty ty} {mSrc : MemoryRead Ty lang.literal ty}
      {mPost mEnding sBlock opLoads stores sRest} {sRead : SSARead Ty lang.literal ty}
      {env env' emitted} :
      AllLoads opLoads →
      ReadImage env opLoads mSrc sRead →
      WriteDest mOrigin mDst sRead env env' stores →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .bind mDst mSrc⟩ :: mPost) mEnding (opLoads ++ stores ++ sRest) emitted
  | assign {mOrigin ty} {mDst : MemoryPlace Ty ty} {mSrc : MemoryRead Ty lang.literal ty}
      {mPost mEnding sBlock opLoads stores sRest} {sRead : SSARead Ty lang.literal ty}
      {env env' emitted} :
      AllLoads opLoads →
      ReadImage env opLoads mSrc sRead →
      WriteDest mOrigin mDst sRead env env' stores →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .assign mDst mSrc⟩ :: mPost) mEnding (opLoads ++ stores ++ sRest) emitted
  | op {mOrigin opcode types ty} {mDst : MemoryPlace Ty ty}
      {op : lang.operation opcode types ty} {mArgs : Args (MemoryRead Ty lang.literal) types}
      {checks addresses mPost mEnding sBlock opLoads stores sRest}
      {sDst : SSAValue Ty ty} {sArgs : Args (SSARead Ty lang.literal) types}
      {env env' emitted} :
      ¬ (MemoryInst.op (opcode := opcode) mDst op mArgs checks addresses).HasResidualAdd →
      AllLoads opLoads →
      ArgsImage env opLoads mArgs sArgs →
      WriteDest mOrigin mDst (.value sDst) env env' stores →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .op mDst op mArgs checks addresses⟩ :: mPost) mEnding
        (opLoads ++ [⟨mOrigin, .op sDst op sArgs (addresses.map fun p => ⟨p.1, p.2.logical⟩)⟩] ++
          stores ++ sRest) emitted
  | residualAdd {mOrigin t} {mDst : MemoryPlace Ty (lang.intTy t)}
      {mLhs mRhs : MemoryRead Ty lang.literal (lang.intTy t)} {mInst : MemoryInst lang}
      {mPost mEnding sBlock contBlock opLoads contStores restContBody}
      {sDst : SSAValue Ty (lang.intTy t)} {sFlag : SSAValue Ty lang.bool}
      {sLhs sRhs : SSARead Ty lang.literal (lang.intTy t)}
      {no : CFGEdge (Sigma (SSARead Ty lang.literal))}
      {contVal : SSAValue Ty (lang.intTy t)}
      {env envAfter envCont emitted} :
      mInst.ResidualPlainAdd t mDst mLhs mRhs →
      AllLoads opLoads →
      ReadImage env opLoads mLhs sLhs →
      ReadImage env opLoads mRhs sRhs →
      sGraph.BlockGuardsAddWithOverflow sBlock t mOrigin sDst sFlag sLhs sRhs no contBlock contVal →
      (match mDst.storage with
       | .virtual =>
         mDst.logical.path = [] ∧
         envAfter = env.set mDst.logical.root ⟨lang.intTy t, .value sDst⟩ ∧
         EdgeTransfersEnv envAfter no contBlock envCont ∧
         envCont mDst.logical.root = some ⟨lang.intTy t, .value contVal⟩ ∧
         contStores = []
       | .materialized =>
         envAfter = env ∧
         EdgeTransfersEnv envAfter no contBlock envCont ∧
         contStores = [⟨mOrigin, .store mDst.logical (.value contVal)⟩]) →
      contBlock.body = contStores ++ restContBody →
      InstsLower mGraph sGraph blockMap envAt contBlock envCont mPost mEnding restContBody emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, mInst⟩ :: mPost) mEnding
        (opLoads ++ [⟨mOrigin, .addWithOverflow t sDst sFlag sLhs sRhs⟩])
        ((sBlock, ⟨mOrigin, .addWithOverflow t sDst sFlag sLhs sRhs⟩) :: emitted)
  | addWithOverflow {mOrigin t} {mDst : MemoryPlace Ty (lang.intTy t)}
      {mFlag : MemoryPlace Ty lang.bool}
      {mLhs mRhs : MemoryRead Ty lang.literal (lang.intTy t)}
      {mPost mEnding sBlock opLoads dstStores flagStores sRest}
      {sDst : SSAValue Ty (lang.intTy t)} {sFlag : SSAValue Ty lang.bool}
      {sLhs sRhs : SSARead Ty lang.literal (lang.intTy t)}
      {env envMid env' emitted} :
      sDst.id ≠ sFlag.id →
      AllLoads opLoads →
      ReadImage env opLoads mLhs sLhs →
      ReadImage env opLoads mRhs sRhs →
      WriteDest mOrigin mDst (.value sDst) env envMid dstStores →
      WriteDest mOrigin mFlag (.value sFlag) envMid env' flagStores →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .addWithOverflow t mDst mFlag mLhs mRhs⟩ :: mPost) mEnding
        (opLoads ++ [⟨mOrigin, .addWithOverflow t sDst sFlag sLhs sRhs⟩] ++
          dstStores ++ flagStores ++ sRest)
        ((sBlock, ⟨mOrigin, .addWithOverflow t sDst sFlag sLhs sRhs⟩) :: emitted)
  | beginLifetime {mOrigin place mPost mEnding sBlock sPrefix sRest env emitted} :
      (sPrefix = [] ∨ sPrefix = [⟨mOrigin, .beginLifetime place⟩]) →
      InstsLower mGraph sGraph blockMap envAt sBlock env mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .beginLifetime place⟩ :: mPost) mEnding (sPrefix ++ sRest) emitted
  | endLifetime {mOrigin place mPost mEnding sBlock sPrefix sRest env emitted} :
      (sPrefix = [] ∨ sPrefix = [⟨mOrigin, .endLifetime place⟩]) →
      InstsLower mGraph sGraph blockMap envAt sBlock
        (fun p => if p = place then none else env p) mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .endLifetime place⟩ :: mPost) mEnding (sPrefix ++ sRest) emitted
  | destructure {mOrigin ty} {mSrc : MemoryPlace Ty ty}
      {mBindings : List (FieldId × Sigma (MemoryPlace Ty))}
      {sAgg : SSAAggregate Ty ty} {sBindings : List (FieldId × Sigma (SSAValue Ty))}
      {mPost mEnding sBlock stores sRest env env' emitted} :
      (match mSrc.storage with
       | .virtual =>
         mSrc.logical.path = [] ∧
         ∃ v, env mSrc.logical.root = some ⟨ty, .value v⟩ ∧ sAgg = .register v
       | .materialized => sAgg = .materialized mSrc.logical) →
      WriteBindings mOrigin mBindings sBindings env env' stores →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .destructure mSrc mBindings⟩ :: mPost) mEnding
        (⟨mOrigin, .destructure sAgg sBindings⟩ :: (stores ++ sRest)) emitted
  | call {mOrigin callee mDst mArgs checks mPost mEnding sBlock opLoads stores sRest}
      {sDst : Option (Sigma (SSAValue Ty))} {sArgs : List (Sigma (SSARead Ty lang.literal))}
      {env env' emitted} :
      AllLoads opLoads →
      OperandsImage env opLoads mArgs sArgs →
      (match mDst with
       | none => sDst = none ∧ stores = [] ∧ env' = env
       | some ⟨ty, dstPlace⟩ =>
         ∃ sVal : SSAValue Ty ty, sDst = some ⟨ty, sVal⟩ ∧
           WriteDest mOrigin dstPlace (.value sVal) env env' stores) →
      InstsLower mGraph sGraph blockMap envAt sBlock env' mPost mEnding sRest emitted →
      InstsLower mGraph sGraph blockMap envAt sBlock env
        (⟨mOrigin, .call callee mDst mArgs checks⟩ :: mPost) mEnding
        (opLoads ++ [⟨mOrigin, .call callee sDst sArgs⟩] ++ stores ++ sRest) emitted

/-- Every residual plain addition in `mBody` lowered by `InstsLower` produces a matching
guarded `addWithOverflow` block in `sGraph` on the SSA images of its operands, supplying
the wrapped value along `no` to its continuation block `contBlock` as `contVal`, and is
recorded in `emitted`. -/
theorem SSAGraph.InstsLower.residualAdd_guarded {Ty : Type} {lang : LoweringLanguage Ty}
    {mGraph : MemoryGraph lang} {sGraph : SSAGraph lang}
    {blockMap : BlockId → BlockId} {envAt : BlockId → SSAEnv Ty lang}
    {sBlock : SSABlock lang} {env : SSAEnv Ty lang}
    {mBody : List (LocatedOp (MemoryInst lang))} {mEnding : MemoryTerm lang}
    {sRem : List (LocatedOp (SSAInst lang))}
    {emitted : List (SSABlock lang × LocatedOp (SSAInst lang))}
    (hLower : SSAGraph.InstsLower mGraph sGraph blockMap envAt sBlock env mBody mEnding sRem emitted)
    (hSBlock : sBlock ∈ sGraph.blocks.toList)
    {mOp : LocatedOp (MemoryInst lang)} (hMem : mOp ∈ mBody)
    (hRes : mOp.instruction.HasResidualAdd) :
    ∃ t mDst mLhs mRhs guardBlock sDst sFlag sLhs sRhs no contBlock contVal envHere opLoads,
      mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
      AllLoads opLoads ∧
      ReadImage envHere opLoads mLhs sLhs ∧
      ReadImage envHere opLoads mRhs sRhs ∧
      guardBlock ∈ sGraph.blocks.toList ∧
      sGraph.BlockGuardsAddWithOverflow guardBlock t mOp.origin sDst sFlag sLhs sRhs no contBlock contVal ∧
      (guardBlock, ⟨mOp.origin, .addWithOverflow t sDst sFlag sLhs sRhs⟩) ∈ emitted := by
  induction hLower with
  | nil _ => cases hMem
  | bind _ _ _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail
  | assign _ _ _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail
  | op hNoRes _ _ _ _ ih =>
    cases hMem with
    | head => exact absurd hRes hNoRes
    | tail _ hTail => exact ih hSBlock hTail
  | @residualAdd mOrigin t mDst mLhs mRhs mInst mPost mEnding sBlock' contBlock
      opLoads contStores restContBody sDst sFlag sLhs sRhs no contVal env' envAfter envCont
      emitted' hPlain hLoads hLhsImg hRhsImg hGuard _ _ _ ih =>
    cases hMem with
    | head =>
      exact ⟨t, mDst, mLhs, mRhs, sBlock', sDst, sFlag, sLhs, sRhs, no, contBlock, contVal,
        env', opLoads, hPlain, hLoads, hLhsImg, hRhsImg, hSBlock, hGuard, List.mem_cons_self⟩
    | tail _ hTail =>
      have hContMem : contBlock ∈ sGraph.blocks.toList := by
        obtain ⟨_, _, _, _, _, _, _, _, _, _, _, hCM, _⟩ := hGuard
        exact hCM
      obtain ⟨t', mDst', mLhs', mRhs', gb, sd, sf, sl, sr, no', cb, cv, eh, ol,
        hp, hl, hli, hri, hgb, hbg, hem⟩ := ih hContMem hTail
      exact ⟨t', mDst', mLhs', mRhs', gb, sd, sf, sl, sr, no', cb, cv, eh, ol,
        hp, hl, hli, hri, hgb, hbg, List.mem_cons_of_mem _ hem⟩
  | addWithOverflow _ _ _ _ _ _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail =>
      obtain ⟨t', mDst', mLhs', mRhs', gb, sd, sf, sl, sr, no', cb, cv, eh, ol,
        hp, hl, hli, hri, hgb, hbg, hem⟩ := ih hSBlock hTail
      exact ⟨t', mDst', mLhs', mRhs', gb, sd, sf, sl, sr, no', cb, cv, eh, ol,
        hp, hl, hli, hri, hgb, hbg, List.mem_cons_of_mem _ hem⟩
  | beginLifetime _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail
  | endLifetime _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail
  | destructure _ _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail
  | call _ _ _ _ ih =>
    cases hMem with
    | head => cases hRes
    | tail _ hTail => exact ih hSBlock hTail

/-- Every `(sBlock, sOp)` recorded in `emitted` by `InstsLower` is a distinct-destination
`SSAInst.addWithOverflow` that traces to either a matching guarded residual plain addition
(with operand images and continuation) or an existing `MemoryInst.addWithOverflow` in `mBody`. -/
theorem SSAGraph.InstsLower.emitted_sound {Ty : Type} {lang : LoweringLanguage Ty}
    {mGraph : MemoryGraph lang} {sGraph : SSAGraph lang}
    {blockMap : BlockId → BlockId} {envAt : BlockId → SSAEnv Ty lang}
    {sBlock : SSABlock lang} {env : SSAEnv Ty lang}
    {mBody : List (LocatedOp (MemoryInst lang))} {mEnding : MemoryTerm lang}
    {sRem : List (LocatedOp (SSAInst lang))}
    {emitted : List (SSABlock lang × LocatedOp (SSAInst lang))}
    (hLower : SSAGraph.InstsLower mGraph sGraph blockMap envAt sBlock env mBody mEnding sRem emitted)
    {emBlock : SSABlock lang} {sOp : LocatedOp (SSAInst lang)}
    (hEm : (emBlock, sOp) ∈ emitted) :
    ∃ t sDst sFlag sLhs sRhs mOp,
      mOp ∈ mBody ∧
      sOp = ⟨mOp.origin, .addWithOverflow t sDst sFlag sLhs sRhs⟩ ∧
      sDst.id ≠ sFlag.id ∧
      ((∃ mDst mLhs mRhs no contBlock contVal envHere opLoads,
          mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
          AllLoads opLoads ∧
          ReadImage envHere opLoads mLhs sLhs ∧
          ReadImage envHere opLoads mRhs sRhs ∧
          sGraph.BlockGuardsAddWithOverflow emBlock t mOp.origin sDst sFlag sLhs sRhs no contBlock contVal) ∨
        (∃ mDst mFlag mLhs mRhs envHere opLoads,
          mOp.instruction = .addWithOverflow t mDst mFlag mLhs mRhs ∧
          AllLoads opLoads ∧
          ReadImage envHere opLoads mLhs sLhs ∧
          ReadImage envHere opLoads mRhs sRhs)) := by
  induction hLower with
  | nil _ => cases hEm
  | bind _ _ _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | assign _ _ _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | op _ _ _ _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | @residualAdd mOrigin t mDst mLhs mRhs mInst mPost mEnding sBlock' contBlock
      opLoads contStores restContBody sDst sFlag sLhs sRhs no contVal env' envAfter envCont
      emitted' hPlain hLoads hLhsImg hRhsImg hGuard _ _ _ ih =>
    cases hEm with
    | head =>
      exact ⟨t, sDst, sFlag, sLhs, sRhs, ⟨mOrigin, mInst⟩, List.mem_cons_self, rfl, hGuard.1,
        .inl ⟨mDst, mLhs, mRhs, no, contBlock, contVal, env', opLoads,
          hPlain, hLoads, hLhsImg, hRhsImg, hGuard⟩⟩
    | tail _ hTail =>
      obtain ⟨t', sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hTail
      exact ⟨t', sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | @addWithOverflow mOrigin t mDst mFlag mLhs mRhs mPost mEnding sBlock' opLoads
      dstStores flagStores sRest sDst sFlag sLhs sRhs env' envMid env'' emitted'
      hNe hLoads hLhsImg hRhsImg _ _ _ ih =>
    cases hEm with
    | head =>
      exact ⟨t, sDst, sFlag, sLhs, sRhs, ⟨mOrigin, .addWithOverflow t mDst mFlag mLhs mRhs⟩,
        List.mem_cons_self, rfl, hNe,
        .inr ⟨mDst, mFlag, mLhs, mRhs, env', opLoads, rfl, hLoads, hLhsImg, hRhsImg⟩⟩
    | tail _ hTail =>
      obtain ⟨t', sd, sf, sl, sr, mOp, hM, hEq, hNe', hCase⟩ := ih hTail
      exact ⟨t', sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe', hCase⟩
  | beginLifetime _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | endLifetime _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | destructure _ _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩
  | call _ _ _ _ ih =>
    obtain ⟨t, sd, sf, sl, sr, mOp, hM, hEq, hNe, hCase⟩ := ih hEm
    exact ⟨t, sd, sf, sl, sr, mOp, List.mem_cons_of_mem _ hM, hEq, hNe, hCase⟩

/-- Full CFG lowering correspondence for a function containing residual plain additions:
every Memory block lowers instruction-by-instruction into `sGraph` under a grounded SSA
renaming `envAt` (initialized from `mGraph.entry`'s parameters and propagated across every
CFG edge via `EdgeTransfersEnv`), lowering every residual plain addition to a guarded
`addWithOverflow` on the SSA images of its operands whose false edge supplies the wrapped
value to the continuation lowering the remainder of the Memory block, and accounting for
every `SSAInst.addWithOverflow` anywhere in `sGraph`. -/
def SSAGraph.LowersFrom {Ty : Type} {lang : LoweringLanguage Ty}
    (mGraph : MemoryGraph lang) (sGraph : SSAGraph lang) : Prop :=
  ∃ (blockMap : BlockId → BlockId) (envAt : BlockId → SSAEnv Ty lang)
    (emittedBy : BlockId → List (SSABlock lang × LocatedOp (SSAInst lang))),
    blockMap mGraph.entry = sGraph.entry ∧
    (∀ mBlock ∈ mGraph.blocks.toList,
      ∃ sBlock ∈ sGraph.blocks.toList, sBlock.id = blockMap mBlock.id ∧
        (mBlock.id = mGraph.entry →
          mBlock.params.map Sigma.fst = sBlock.params.map Sigma.fst ∧
          envAt mBlock.id =
            SSAEnv.bindParams SSAEnv.empty mBlock.params
              (sBlock.params.map fun p => ⟨p.1, .value p.2⟩) ∧
          ∃ entryStores restBody,
            ParamStores mBlock.params (sBlock.params.map fun p => ⟨p.1, .value p.2⟩) entryStores ∧
            sBlock.body = entryStores ++ restBody ∧
            SSAGraph.InstsLower mGraph sGraph blockMap envAt sBlock (envAt mBlock.id)
              mBlock.body mBlock.ending restBody (emittedBy mBlock.id)) ∧
        (mBlock.id ≠ mGraph.entry →
          (∃ slot : Nat → Nat,
            (∀ i j, i < mBlock.params.length → j < mBlock.params.length → slot i = slot j → i = j) ∧
            ∀ i ty (mParam : MemoryPlace Ty ty),
              mBlock.params[i]? = some ⟨ty, mParam⟩ → mParam.storage = .virtual →
              mParam.logical.path = [] ∧
              ∃ v : SSAValue Ty ty, sBlock.params[slot i]? = some ⟨ty, v⟩ ∧
                envAt mBlock.id mParam.logical.root = some ⟨ty, .value v⟩) ∧
          ((∀ b ∈ mGraph.blocks.toList, ∀ e ∈ b.ending.successors, e.target ≠ mBlock.id) →
            ∀ root val, envAt mBlock.id root = some val →
              ∃ p ∈ mBlock.params, p.2.storage = .virtual ∧ p.2.logical.root = root) ∧
          SSAGraph.InstsLower mGraph sGraph blockMap envAt sBlock (envAt mBlock.id)
            mBlock.body mBlock.ending sBlock.body (emittedBy mBlock.id))) ∧
    (∀ sBlock ∈ sGraph.blocks.toList, ∀ sOp ∈ sBlock.body,
      match sOp.instruction with
      | .addWithOverflow .. =>
        ∃ mBlock ∈ mGraph.blocks.toList, (sBlock, sOp) ∈ emittedBy mBlock.id
      | _ => True)

theorem SSAGraph.LowersFrom.residualAdd_guarded {Ty : Type} {lang : LoweringLanguage Ty}
    {mGraph : MemoryGraph lang} {sGraph : SSAGraph lang}
    (hLower : sGraph.LowersFrom mGraph)
    {mBlock : MemoryBlock lang} (hMBlock : mBlock ∈ mGraph.blocks.toList)
    {mOp : LocatedOp (MemoryInst lang)} (hMOp : mOp ∈ mBlock.body)
    (hRes : mOp.instruction.HasResidualAdd) :
    ∃ t mDst mLhs mRhs sLhs sRhs no contBlock contVal envHere opLoads,
      mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
      AllLoads opLoads ∧
      ReadImage envHere opLoads mLhs sLhs ∧
      ReadImage envHere opLoads mRhs sRhs ∧
      sGraph.GuardsAddWithOverflow t mOp.origin sLhs sRhs no contBlock contVal := by
  obtain ⟨blockMap, envAt, emittedBy, _, hBlocks, _⟩ := hLower
  obtain ⟨sBlock, hSBlock, _, hEntry, hNonEntry⟩ := hBlocks mBlock hMBlock
  have hInst : ∃ sRem, SSAGraph.InstsLower mGraph sGraph blockMap envAt sBlock (envAt mBlock.id)
      mBlock.body mBlock.ending sRem (emittedBy mBlock.id) := by
    by_cases hEq : mBlock.id = mGraph.entry
    · obtain ⟨_, _, _, restBody, _, _, hIL⟩ := hEntry hEq
      exact ⟨restBody, hIL⟩
    · exact ⟨sBlock.body, (hNonEntry hEq).2.2⟩
  obtain ⟨sRem, hIL⟩ := hInst
  obtain ⟨t, mDst, mLhs, mRhs, guardBlock, sDst, sFlag, sLhs, sRhs, no, contBlock, contVal,
    envHere, opLoads, hPlain, hLoads, hLhsImg, hRhsImg, hGuardMem, hGuard, _⟩ :=
    hIL.residualAdd_guarded hSBlock hMOp hRes
  exact ⟨t, mDst, mLhs, mRhs, sLhs, sRhs, no, contBlock, contVal, envHere, opLoads,
    hPlain, hLoads, hLhsImg, hRhsImg, ⟨guardBlock, hGuardMem, sDst, sFlag, hGuard⟩⟩

/-- Every `SSAInst.addWithOverflow` in `sGraph` lowered via `LowersFrom mGraph` has distinct
destinations and traces to either a matching guarded residual plain addition (with operand
images and continuation) or an existing `MemoryInst.addWithOverflow` in `mGraph`. -/
theorem SSAGraph.LowersFrom.addWithOverflow_accounted {Ty : Type} {lang : LoweringLanguage Ty}
    {mGraph : MemoryGraph lang} {sGraph : SSAGraph lang}
    (hLower : sGraph.LowersFrom mGraph)
    {sBlock : SSABlock lang} (hSBlock : sBlock ∈ sGraph.blocks.toList)
    {origin : Origin} {t : IntTy} {dst : SSAValue Ty (lang.intTy t)}
    {flag : SSAValue Ty lang.bool} {lhs rhs : SSARead Ty lang.literal (lang.intTy t)}
    (hSOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ sBlock.body) :
    dst.id ≠ flag.id ∧
    ∃ mBlock ∈ mGraph.blocks.toList, ∃ mOp ∈ mBlock.body, mOp.origin = origin ∧
      ((∃ mDst mLhs mRhs no contBlock contVal envHere opLoads,
          mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
          AllLoads opLoads ∧
          ReadImage envHere opLoads mLhs lhs ∧
          ReadImage envHere opLoads mRhs rhs ∧
          sGraph.BlockGuardsAddWithOverflow sBlock t origin dst flag lhs rhs no contBlock contVal) ∨
        (∃ mDst mFlag mLhs mRhs envHere opLoads,
          mOp.instruction = .addWithOverflow t mDst mFlag mLhs mRhs ∧
          AllLoads opLoads ∧
          ReadImage envHere opLoads mLhs lhs ∧
          ReadImage envHere opLoads mRhs rhs)) := by
  obtain ⟨blockMap, envAt, emittedBy, _, hBlocks, hAccounted⟩ := hLower
  obtain ⟨mBlock, hMBlock, hInEm⟩ :=
    hAccounted sBlock hSBlock ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ hSOp
  obtain ⟨sEntry, _, _, hEntry, hNonEntry⟩ := hBlocks mBlock hMBlock
  have hInst : ∃ sRem, SSAGraph.InstsLower mGraph sGraph blockMap envAt sEntry (envAt mBlock.id)
      mBlock.body mBlock.ending sRem (emittedBy mBlock.id) := by
    by_cases hEq : mBlock.id = mGraph.entry
    · obtain ⟨_, _, _, restBody, _, _, hIL⟩ := hEntry hEq
      exact ⟨restBody, hIL⟩
    · exact ⟨sEntry.body, (hNonEntry hEq).2.2⟩
  obtain ⟨sRem, hIL⟩ := hInst
  obtain ⟨t', sDst', sFlag', sLhs', sRhs', mOp, hMOp, hOpEq, hNe, hCase⟩ :=
    hIL.emitted_sound hInEm
  have hParts := LocatedOp.mk.inj hOpEq
  have hOrigEq : origin = mOp.origin := hParts.1
  have hInstEq := hParts.2
  cases hOrigEq
  cases hInstEq
  exact ⟨hNe, mBlock, hMBlock, mOp, hMOp, rfl, hCase⟩

/-- Every residual plain addition in `memory` lowers in the same function of `ssa` through
a structural SSA correspondence (`SSAGraph.LowersFrom`):
- the guard block's last instruction is an `addWithOverflow` at the exact same origin and
  width with distinct destinations, operating on the SSA images of the Memory op's arguments;
- the true edge branches with no arguments to an empty `.trap .overflow` block at that origin;
- the false edge supplies the wrapped value to the continuation block, which lowers the
  remaining instructions and terminator of the Memory block;
- no unguarded plain `SSAInst.op (.arithmetic (.plain .add))` appears at that origin; and
- every `SSAInst.addWithOverflow` in `ssa` has distinct destinations and traces either to
  that guarded lowering of a residual plain addition or to an existing `MemoryInst.addWithOverflow`
  of the same width at the same origin in `memory` (preventing spurious runtime overflow guards
  on discharged plain additions). -/
def ResidualAddsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (memory : Memory lang profile) (ssa : SSA lang profile) : Prop :=
  (∀ fn mLocals mGraph, (fn, mLocals, mGraph) ∈ memory.functions →
    mGraph.HasResidualAdd →
      ∃ sLocals sGraph, (fn, sLocals, sGraph) ∈ ssa.functions ∧
        sGraph.LowersFrom mGraph ∧
        ∀ mBlock ∈ mGraph.blocks.toList, ∀ mOp ∈ mBlock.body,
          mOp.instruction.HasResidualAdd →
            ∀ sBlock ∈ sGraph.blocks.toList, ∀ sOp ∈ sBlock.body,
              sOp.origin = mOp.origin →
                match sOp.instruction with
                | @SSAInst.op _ _ (.arithmetic (.plain .add)) .. => False
                | _ => True) ∧
  ∀ fn sLocals sGraph, (fn, sLocals, sGraph) ∈ ssa.functions →
    ∀ sBlock ∈ sGraph.blocks.toList, ∀ sOp ∈ sBlock.body,
      match sOp.instruction with
      | .addWithOverflow t dst flag _ _ =>
        dst.id ≠ flag.id ∧
        ∃ mLocals mGraph, (fn, mLocals, mGraph) ∈ memory.functions ∧
          ((mGraph.HasResidualAdd ∧ sGraph.LowersFrom mGraph) ∨
            (¬ mGraph.HasResidualAdd ∧
              ∃ mBlock ∈ mGraph.blocks.toList, ∃ mOp ∈ mBlock.body,
                mOp.origin = sOp.origin ∧
                match mOp.instruction with
                | .addWithOverflow mt .. => mt = t
                | _ => False))
      | _ => True

/-- Every residual plain addition in `memory` has a matching SSA guard bound to the SSA
images of its arguments, its overflow trap block, and its continuation receiving the wrapped value. -/
theorem ResidualAddsMatch.guards_of_residualAdd {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {memory : Memory lang profile} {ssa : SSA lang profile}
    (hMatch : ResidualAddsMatch memory ssa)
    {fn mLocals mGraph} (hFn : (fn, mLocals, mGraph) ∈ memory.functions)
    {mBlock : MemoryBlock lang} (hMBlock : mBlock ∈ mGraph.blocks.toList)
    {mOp : LocatedOp (MemoryInst lang)} (hMOp : mOp ∈ mBlock.body)
    (hRes : mOp.instruction.HasResidualAdd) :
    ∃ t mDst mLhs mRhs sLocals sGraph sLhs sRhs no contBlock contVal envHere opLoads,
      mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
      (fn, sLocals, sGraph) ∈ ssa.functions ∧
      AllLoads opLoads ∧
      ReadImage envHere opLoads mLhs sLhs ∧
      ReadImage envHere opLoads mRhs sRhs ∧
      sGraph.GuardsAddWithOverflow t mOp.origin sLhs sRhs no contBlock contVal ∧
      ∀ sBlock ∈ sGraph.blocks.toList, ∀ sOp ∈ sBlock.body,
        sOp.origin = mOp.origin →
          match sOp.instruction with
          | @SSAInst.op _ _ (.arithmetic (.plain .add)) .. => False
          | _ => True := by
  obtain ⟨sLocals, sGraph, hSFn, hLower, hNoPlain⟩ :=
    hMatch.1 fn mLocals mGraph hFn ⟨mBlock, hMBlock, mOp, hMOp, hRes⟩
  obtain ⟨t, mDst, mLhs, mRhs, sLhs, sRhs, no, contBlock, contVal, envHere, opLoads,
    hPlain, hLoads, hLhsImg, hRhsImg, hGuard⟩ :=
    hLower.residualAdd_guarded hMBlock hMOp hRes
  exact ⟨t, mDst, mLhs, mRhs, sLocals, sGraph, sLhs, sRhs, no, contBlock, contVal,
    envHere, opLoads, hPlain, hSFn, hLoads, hLhsImg, hRhsImg, hGuard,
    hNoPlain mBlock hMBlock mOp hMOp hRes⟩

/-- Every `SSAInst.addWithOverflow` in an SSA graph satisfying `ResidualAddsMatch` has
distinct result and overflow-flag destinations. -/
theorem ResidualAddsMatch.addWithOverflow_distinct {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {memory : Memory lang profile} {ssa : SSA lang profile}
    (hMatch : ResidualAddsMatch memory ssa)
    {fn sLocals sGraph} (hFn : (fn, sLocals, sGraph) ∈ ssa.functions)
    {sBlock : SSABlock lang} (hBlock : sBlock ∈ sGraph.blocks.toList)
    {origin : Origin} {t : IntTy} {dst : SSAValue Ty (lang.intTy t)}
    {flag : SSAValue Ty lang.bool} {lhs rhs : SSARead Ty lang.literal (lang.intTy t)}
    (hOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ sBlock.body) :
    dst.id ≠ flag.id :=
  (hMatch.2 fn sLocals sGraph hFn sBlock hBlock ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ hOp).1

/-- Backward accountability of `ResidualAddsMatch`: every `SSAInst.addWithOverflow` in `ssa`
has distinct destinations and traces in the same function of `memory` to either a matching
guarded residual plain addition (with operand images and continuation) or an existing
`MemoryInst.addWithOverflow` of the same width at the same origin. -/
theorem ResidualAddsMatch.addWithOverflow_accounted {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {memory : Memory lang profile} {ssa : SSA lang profile}
    (hMatch : ResidualAddsMatch memory ssa)
    {fn sLocals sGraph} (hFn : (fn, sLocals, sGraph) ∈ ssa.functions)
    {sBlock : SSABlock lang} (hBlock : sBlock ∈ sGraph.blocks.toList)
    {origin : Origin} {t : IntTy} {dst : SSAValue Ty (lang.intTy t)}
    {flag : SSAValue Ty lang.bool} {lhs rhs : SSARead Ty lang.literal (lang.intTy t)}
    (hOp : ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ ∈ sBlock.body) :
    dst.id ≠ flag.id ∧
    ∃ mLocals mGraph, (fn, mLocals, mGraph) ∈ memory.functions ∧
      ∃ mBlock ∈ mGraph.blocks.toList, ∃ mOp ∈ mBlock.body, mOp.origin = origin ∧
        ((∃ mDst mLhs mRhs no contBlock contVal envHere opLoads,
            mOp.instruction.ResidualPlainAdd t mDst mLhs mRhs ∧
            AllLoads opLoads ∧
            ReadImage envHere opLoads mLhs lhs ∧
            ReadImage envHere opLoads mRhs rhs ∧
            sGraph.BlockGuardsAddWithOverflow sBlock t origin dst flag lhs rhs no contBlock contVal) ∨
          match mOp.instruction with
          | .addWithOverflow mt .. => mt = t
          | _ => False) := by
  obtain ⟨hNe, mLocals, mGraph, hMFn, hCases⟩ :=
    hMatch.2 fn sLocals sGraph hFn sBlock hBlock ⟨origin, .addWithOverflow t dst flag lhs rhs⟩ hOp
  refine ⟨hNe, mLocals, mGraph, hMFn, ?_⟩
  rcases hCases with ⟨_, hLower⟩ | ⟨_, mBlock, hMB, mOp, hMO, hOrig, hInst⟩
  · obtain ⟨_, mBlock, hMB, mOp, hMO, hOrig, hCase⟩ :=
      hLower.addWithOverflow_accounted hBlock hOp
    refine ⟨mBlock, hMB, mOp, hMO, hOrig, ?_⟩
    rcases hCase with hRes | ⟨mDst, mFlag, mLhs, mRhs, _, _, hEq, _, _, _⟩
    · exact .inl hRes
    · rw [hEq]; exact .inr rfl
  · exact ⟨mBlock, hMB, mOp, hMO, hOrig, .inr hInst⟩

def BuildsSSA
    (memory : Memory lang profile) (ssa : SSA lang profile) : Prop :=
  ssa.project = memory.project ∧ ssa.entry = memory.entry ∧ ssa.layouts = memory.layouts ∧
  (ssa.signatures = memory.signatures ∧ ssa.declarations = memory.declarations) ∧
  PreservesOrigins memory.origins ssa.origins ∧
  ResidualAddsMatch memory ssa ∧
  BehaviorRefinement (Memory.Execution (language := language)) (SSA.Execution (language := language))
    (Memory.Execution (language := language)).result (SSA.Execution (language := language)).result memory ssa

theorem BuildsSSA.residualAddsMatch {memory : Memory lang profile} {ssa : SSA lang profile}
    (built : BuildsSSA (language := language) memory ssa) :
    ResidualAddsMatch memory ssa :=
  built.2.2.2.2.2.1

/-- BIR formation re-encodes the entry graph. Linked behavior is checked
separately, so unreachable helper bodies can differ. -/
def BIR.PreservesProgram (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  BehaviorRefinement (SSA.Execution (language := language)) (BIR.Execution (language := language))
    (SSA.Execution (language := language)).result (BIR.Execution (language := language)).result ssa bir

def BIR.EntryMatches (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.entry = ssa.entry ∧ bir.declarations = ssa.declarations ∧
  (ssa.memoryView (language := language)).graphAt ssa.entry =
    (bir.memoryView (language := language)).graphAt bir.entry

theorem BIR.refinesOfViewEq {ssa : SSA lang profile} {bir : BIR lang profile}
    (same : ssa.memoryView (language := language) = bir.memoryView (language := language)) :
    BIR.PreservesProgram (language := language) ssa bir := by
  let exec := BIR.registerExecution (language := language) (lang := lang) (profile := profile)
  change BehaviorRefinement exec exec exec.result exec.result
    (ssa.memoryView (language := language)) (bir.memoryView (language := language))
  rw [same]
  intro safe
  exact ⟨fun state _ reached result => ⟨state, reached, result, rfl⟩,
    fun run infinite => ⟨run, infinite, fun _ => Iff.rfl⟩, safe⟩

def BuildsBIR (ssa : SSA lang profile) (bir : BIR lang profile) : Prop :=
  bir.project = ssa.project ∧ bir.TrapSlotsWF ∧ bir.TrapsMatch ssa ∧ PreservesOrigins ssa.origins bir.origins ∧
  bir.frameSlots ≤ profile.maxBIRSlots ∧ BIR.EntryMatches (language := language) ssa bir ∧
  BIR.PreservesProgram (language := language) ssa bir

theorem BuildsBIR.refines {ssa : SSA lang profile} {bir : BIR lang profile}
    (built : BuildsBIR (language := language) ssa bir) :
    BIR.PreservesProgram (language := language) ssa bir :=
  built.2.2.2.2.2.2

/-- Visit only CFG successors of the entry. Each distinct block is expanded at
most once. Fuel covers the entry plus at most two queued edges per block. -/
private def reachableCall (graph : BIRGraph lang) :
    Nat → List BlockId → List BlockId → Bool
  | 0, _, _ | _, [], _ => false
  | fuel + 1, id :: pending, visited =>
    if visited.contains id then reachableCall graph fuel pending visited
    else
      match graph.blocks.find? (·.id == id) with
      | none => reachableCall graph fuel pending visited
      | some block =>
        let hasCall := block.body.any fun op =>
          match op.instruction with
          | .call .. => true
          | _ => false
        let successors := match block.ending with
          | .jump edge => [edge.target]
          | .branch _ yes no => [yes.target, no.target]
          | .ret _ | .trap _ => []
        hasCall || reachableCall graph fuel (successors ++ pending) (id :: visited)

def BIR.requiresLCBLinking (bir : BIR lang profile) : Bool :=
  match AList.lookup bir.functions bir.entry with
  | none => false
  | some (_, graph) => reachableCall graph (2 * graph.blocks.size + 1) [graph.entry] []

def LCBEncoding (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  ((Backend.LCBJob.lcbFunctionJob output.body :: List.map Prod.snd output.additionalJobs).map
    (fun job => job.code.symbol.name)).Nodup ∧
  output.sites = bir.sites ∧ output.RuntimeMatches bir.project ∧
  BehaviorRefinement (BIR.Execution (language := language)) (LCBInput.Execution _ bir.project)
    (BIR.Execution (language := language)).result (LCBInput.Execution _ bir.project).result bir output ∧
  (List.map Prod.fst bir.functions).Nodup ∧
  (bir.entry :: List.map Prod.fst output.additionalJobs).Nodup ∧
  (∀ fn, fn ∈ List.map Prod.fst bir.functions ↔ fn ∈ bir.entry :: List.map Prod.fst output.additionalJobs) ∧
  bir.requiresLCBLinking = false ∧
  bir.TrapSlotsWF

def EmitsLCB (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) (output : LCBInput profile) : Prop :=
  profile.backend = .lcb ∧
  LeanCompilerBackend.Verified.validateJob (Backend.LCBJob.lcbFunctionJob output.body) = .ok () ∧
  (∀ pair ∈ output.additionalJobs, LeanCompilerBackend.Verified.validateJob pair.2 = .ok ()) ∧
  LCBEncoding language bir output

end Concrete.Interfaces
