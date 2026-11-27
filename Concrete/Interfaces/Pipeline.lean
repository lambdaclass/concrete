import Concrete.Interfaces.Contracts
import ConcreteSpec.StageGrammar

namespace Concrete.Interfaces

open ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Concrete.Contracts (EvidenceKind Graph)

variable {count : Nat} {grammar : SharedGrammar count} {language : CoreLanguage}
  {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

/-- Executable signatures are collected separately from the declarative spec and models.
The registry below builds its graphs from these values. -/
structure PassFunctions {count : Nat} (grammar : SharedGrammar count) (language : CoreLanguage)
    (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  lex : Source → Except LexRefusal TokenStream
  elaborate : Parsed (grammar.value grammar.start) → Except ElabRefusal (AdmissionInput language)
  typeCore : CoreInput language → Except StageFailure Unit
  checkCore : CoreInput language → Except AdmissionFailure (Discharges language)
  lowerCore : Admitted language → Except StageFailure (MIR lang profile)
  lowerUnchecked : CoreInput language → Except StageFailure (MIR lang profile)
  lowerMemory : MIR lang profile → Except StageFailure (Memory lang profile)
  buildSSA : Memory lang profile → Except StageFailure (SSA lang profile)
  buildBIR : SSA lang profile → Except StageFailure (BIR lang profile)
  emitLLVM : BIR lang profile → Except StageFailure (LLVMInput profile)
  emitLCB : BIR lang profile → Except StageFailure (LCBInput profile)

def Source.WF (source : Source) : Prop :=
  (source.project.fileAt source.entryFile).isSome = true

def PassFunctions.lexer (passes : PassFunctions grammar language lang profile) : Stage Source LexRefusal TokenStream where
  run := passes.lex
  kind := .directProof
  inputInvariant := Source.WF
  outputInvariant stream := stream.parserView.WF
  success := Lexes
  refusal := LexRefuses

def parserEntry (parser : ParserEngine grammar) :
    Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start)) where
  run stream := (parser.run stream.parserView).2.getD
    (.error (.malformed parser.malformedRule ⟨stream.source.entryFile, 0, 0, 1, 1⟩))
  kind := .directProof
  inputInvariant stream := stream.parserView.WF
  outputInvariant ast := Source.WF ast.source
  success stream output := (parser.run stream.parserView).2 = some (.ok output) ∧
    ParsesWith parser stream.parserView output ∧
    (parser.run stream.parserView).1 ≤ parser.stepsPerToken * (stream.parserView.tokens.size + 1)
  refusal stream failure := (parser.run stream.parserView).2 = some (.error failure) ∧
    Refuses grammar stream.parserView failure ∧
    (parser.run stream.parserView).1 ≤ parser.stepsPerToken * (stream.parserView.tokens.size + 1)

/-- The fixed grammar supplies a surface tree. Any successful canonical
elaboration must retain locations valid in the issuing project. -/
def Parsed.ElaborationDomain (slice : Slice)
    (ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) : Prop :=
  Source.WF ast.source ∧ ast.source.slice = slice ∧
  (∃ stream : ParserView, stream.WF ∧ ((sliceParser slice).run stream).2 = some (.ok ast)) ∧
  (∃ result, SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) result) ∧
  ∀ input, SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.ok input) →
    input.Ready slice

/-- Resolve representable source before unchecked Core typing. The slice is the
one selected by the registry, independently of the implementation language. -/
def eligibilityEntry (language : CoreLanguage) (slice : Slice) :
    Stage (AdmissionInput language) StageFailure (CoreInput language) where
  run input := if input.slice = slice then
      (input.prepare .eligibility).mapError (fun _ => .unpreparable)
    else .error .unpreparable
  kind := .directProof
  inputInvariant input := input.Ready slice
  outputInvariant input := input.slice = slice ∧ input.WF ∧ input.InFragment
  success input output := input.slice = slice ∧ input.prepare .eligibility = .ok output
  refusal input failure := failure = .unpreparable ∧
    (input.slice ≠ slice ∨ ∃ r, input.prepare .eligibility = .error r)

theorem eligibilityEntry_validated (language : CoreLanguage) (slice : Slice) :
    (eligibilityEntry language slice).Validated := by
  constructor
  · intro input output ready ran
    change (if input.slice = slice then
      (input.prepare .eligibility).mapError (fun _ => StageFailure.unpreparable) else .error .unpreparable) = .ok output at ran
    rw [ite_eq_left ready.1] at ran
    have domain := ready.2 .eligibility
    cases prepared : input.prepare .eligibility with
    | error reason => simp [prepared, Except.mapError] at ran
    | ok i =>
      simp only [prepared, Except.mapError, Except.ok.injEq] at ran
      subst output
      rw [prepared] at domain
      exact ⟨⟨(input.preparedSlice prepared).trans ready.1, domain⟩, ready.1, prepared⟩
  · intro input failure ready ran
    change (if input.slice = slice then
      (input.prepare .eligibility).mapError (fun _ => StageFailure.unpreparable) else .error .unpreparable) = .error failure at ran
    rw [ite_eq_left ready.1] at ran
    cases prepared : input.prepare .eligibility <;> simp [prepared, Except.mapError] at ran
    exact ⟨ran.symm, .inr ⟨_, prepared⟩⟩

def PassFunctions.typingEntry (passes : PassFunctions grammar language lang profile) (slice : Slice) :
    Stage (CoreInput language) StageFailure (CoreInput language) where
  run input := (passes.typeCore input).map fun _ => input
  kind := .directProof
  inputInvariant input := input.slice = slice ∧ input.WF ∧ input.InFragment
  outputInvariant input := input.WF ∧ input.StaticallyTyped
  success input output := input = output
  refusal input failure := failure = .invalidInput ⟨``CoreInput.StaticallyTyped, 1⟩ ∧ ¬ input.StaticallyTyped

def PassFunctions.admissionEntry (passes : PassFunctions grammar language lang profile) (slice : Slice) :
    Stage (AdmissionInput language) AdmissionFailure (Admitted language) where
  run input := do
    unless input.slice = slice do throw (.fragment (.sliceMismatch slice input.slice))
    let prepared ← input.prepare.mapError AdmissionFailure.refused
    (passes.checkCore prepared).map fun discharges => ⟨prepared, discharges⟩
  kind := .directProof
  inputInvariant input := input.Ready slice
  outputInvariant := Admitted.Valid
  success input output := input.slice = slice ∧ ∃ prepared, input.prepare = .ok prepared ∧ Admits prepared output
  refusal input
    | .refused diagnostic => input.slice = slice ∧ input.CheckerRefuses diagnostic
    | .fragment (.sliceMismatch expected actual) => expected = slice ∧ actual = input.slice ∧ actual ≠ expected
    | _ => False

def Memory.HandoffWF {Ty : Type} {lowering : LoweringLanguage Ty}
    (memory : Memory lowering profile) : Prop :=
  memory.TablesWF ∧ memory.entry ∈ List.map Prod.fst memory.functions ∧
  ∀ origin ∈ memory.origins, ∃ span, origin.lexical.Binds memory.project span

def SSA.WF (ssa : SSA lang profile) : Prop :=
  (ssa.memoryView (language := language)).HandoffWF

def BIR.WF (bir : BIR lang profile) : Prop :=
  bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots ∧
  (List.map Prod.fst bir.functions).Nodup ∧ bir.entry ∈ List.map Prod.fst bir.functions ∧
  (∀ site ∈ bir.sites.toList, ∃ span, site.origin.lexical.Binds bir.project span) ∧
  (bir.memoryView (language := language)).TablesWF

private def Profile.Refuses (profile : Profile) (backend : BackendKind) : StageFailure → Prop
  | .unsupportedProfile rule =>
    profile.backend ≠ backend ∧ rule = ⟨``Profile.backend, 1⟩
  | _ => False

/-- Maximal observations of the external LLVM execution. The event type is
fixed by the source language at this edge. -/
inductive Behavior (E : Type) where
  | terminates (result : Concrete.Contracts.Observation) (trace : List (Concrete.Contracts.ProcessEvent E))
  | diverges (trace : Nat → List (Concrete.Contracts.ProcessEvent E))
  | wrong

abbrev LLVMModuleBytes := ByteArray

/-- Trusted interpretation boundary for the exact emitted bytes. Lean does not
interpret LLVM here. The opaque default only supplies an inhabitant. -/
opaque llvmBehaviors {E : Type} : LLVMModuleBytes → Behavior E → Prop := default

def P_print (bir : BIR lang profile) (output : LLVMInput profile) : Prop :=
  let source := BIR.Execution (language := language)
  ProgramSafe source bir →
    ∀ behavior : Behavior language.operands.Event, llvmBehaviors output.moduleBytes behavior →
      match behavior with
      | .terminates result trace =>
        ∃ p, source.finite bir p ∧ source.result bir p result ∧ source.trace bir p = trace
      | .diverges trace =>
        ∃ run, source.diverges bir run ∧
          Concrete.Contracts.SameInfiniteTrace (fun i => source.trace bir (run i)) trace
      | .wrong => False

private def LCBInput.validateJobs (output : LCBInput profile) :
    Except LeanCompilerBackend.Verified.CompileError Unit := do
  LeanCompilerBackend.Verified.validateJob (Backend.LCB.job output.body)
  output.additionalJobs.forM fun pair => LeanCompilerBackend.Verified.validateJob pair.2

/-- The hosted LCB word reserves 0 through 255 for ordinary exits. -/
def LCBInput.trapCapacity : Nat := 2 ^ 32 - 256

def LCBInput.trapCapacityRefusal (sites : Nat) : ResourceRefusal :=
  ⟨⟨``LCBInput.trapCapacity, 1⟩, sites, LCBInput.trapCapacity⟩

/-- Check hosted ABI limits and validate the exact jobs prepared by the emitter. -/
def PassFunctions.emitSingleLCB (passes : PassFunctions grammar language lang profile)
    (bir : BIR lang profile) : Except StageFailure (LCBInput profile) :=
  if LCBInput.trapCapacity < bir.sites.size then
    .error (.resource (LCBInput.trapCapacityRefusal bir.sites.size))
  else do
    if bir.requiresLCBLinking then throw .lcbLinkingUnavailable
    let output ← passes.emitLCB bir
    output.validateJobs.mapError StageFailure.invalidLCBJob
    return output

def PassFunctions.lowerUncheckedEntry (passes : PassFunctions grammar language lang profile) :
    Stage (CoreInput language) StageFailure (MIR lang profile) :=
  ⟨passes.lowerUnchecked, .directProof,
    fun input => input.WF ∧ input.StaticallyTyped,
    fun mir => mir.logicalView.HandoffWF, LowersUnchecked (language := language), fun _ _ => False, fun _ _ => True⟩

def PassFunctions.emitLLVMEntry (passes : PassFunctions grammar language lang profile) :
    Stage (BIR lang profile) StageFailure (LLVMInput profile) :=
  ⟨passes.emitLLVM, .namedPremise "P_print", BIR.WF (language := language),
    fun output => ∃ bir : BIR lang profile, LLVMMetadataMatches bir output, LLVMMetadataMatches,
    fun _ => profile.Refuses .llvm, P_print (language := language)⟩

def PassFunctions.emitLCBEntry (passes : PassFunctions grammar language lang profile) :
    Stage (BIR lang profile) StageFailure (LCBInput profile) :=
  ⟨passes.emitSingleLCB, .directProof, BIR.WF (language := language),
    fun output => ∃ bir : BIR lang profile, EmitsLCB language bir output, EmitsLCB language, fun bir failure =>
      match failure with
      | .resource bound =>
        bound = LCBInput.trapCapacityRefusal bir.sites.size ∧ LCBInput.trapCapacity < bir.sites.size
      | .lcbLinkingUnavailable => bir.requiresLCBLinking = true
      | .invalidLCBJob reason =>
        profile.backend = .lcb ∧ ∃ output, passes.emitLCB bir = .ok output ∧
          LCBEncoding language bir output ∧ output.validateJobs = .error reason
      | _ => profile.Refuses .lcb failure,
      fun _ _ => True⟩

/-- Only pass implementations vary at the public boundary. Their semantic and
grammar indices are selected by the repository. -/
abbrev Passes (slice : Slice) (profile : Profile) :=
  PassFunctions (sliceGrammar slice) (sliceLanguage slice) (sliceLoweringLanguage slice) profile

/-- Each field is a stage at the repository-selected slice. Construction is
  confined to the registry definition, which installs each fixed contract. -/
structure Registry (slice : Slice) (profile : Profile) where
  private mk ::
  lexer : Stage Source LexRefusal TokenStream
  parser : Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start))
  elaborator : Stage (Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)) ElabRefusal (AdmissionInput (sliceLanguage slice))
  eligibility : Stage (AdmissionInput (sliceLanguage slice)) StageFailure (CoreInput (sliceLanguage slice))
  typing : Stage (CoreInput (sliceLanguage slice)) StageFailure (CoreInput (sliceLanguage slice))
  admission : Stage (AdmissionInput (sliceLanguage slice)) AdmissionFailure (Admitted (sliceLanguage slice))
  lowerCore : Stage (Admitted (sliceLanguage slice)) StageFailure (MIR (sliceLoweringLanguage slice) profile)
  lowerUnchecked : Stage (CoreInput (sliceLanguage slice)) StageFailure (MIR (sliceLoweringLanguage slice) profile)
  lowerMemory : Stage (MIR (sliceLoweringLanguage slice) profile) StageFailure (Memory (sliceLoweringLanguage slice) profile)
  buildSSA : Stage (Memory (sliceLoweringLanguage slice) profile) StageFailure (SSA (sliceLoweringLanguage slice) profile)
  buildBIR : Stage (SSA (sliceLoweringLanguage slice) profile) StageFailure (BIR (sliceLoweringLanguage slice) profile)
  emitLLVM : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LLVMInput profile)
  emitLCB : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LCBInput profile)

def registry {slice : Slice} (passes : Passes slice profile) : Registry slice profile where
  lexer := { passes.lexer with
    inputInvariant := fun input => Source.WF input ∧ input.slice = slice
    outputInvariant := fun stream => stream.parserView.WF ∧ stream.source.slice = slice }
  parser := { parserEntry (sliceParser slice) with
    inputInvariant := fun stream => stream.parserView.WF ∧ stream.source.slice = slice
    outputInvariant := Parsed.ElaborationDomain slice }
  elaborator :=
    { run := passes.elaborate
      kind := .directProof
      inputInvariant := Parsed.ElaborationDomain slice
      outputInvariant := (·.Ready slice)
      success := fun ast input =>
        SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.ok input)
      refusal := fun ast failure =>
        SliceElaboration slice ast.source.project (cast (sliceGrammarStartShape slice) ast.tree) (.error failure) }
  eligibility := eligibilityEntry _ slice
  typing := passes.typingEntry slice
  admission := passes.admissionEntry slice
  lowerCore := ⟨passes.lowerCore, .directProof, Admitted.Valid,
    fun mir => mir.logicalView.HandoffWF, LowersCore (language := sliceLanguage slice), fun _ _ => False, fun _ _ => True⟩
  lowerUnchecked := passes.lowerUncheckedEntry
  lowerMemory := ⟨passes.lowerMemory, .directProof, fun mir => mir.logicalView.HandoffWF,
    Memory.HandoffWF, (fun mir memory =>
      LowersMemory (language := sliceLanguage slice) mir memory ∧ memory.mutableLocals = mir.mutableLocals),
    fun _ _ => False, fun _ _ => True⟩
  buildSSA := ⟨passes.buildSSA, .provedValidator, Memory.HandoffWF,
    SSA.WF (language := sliceLanguage slice), (fun memory ssa =>
      BuildsSSA (language := sliceLanguage slice) memory ssa ∧ ssa.mutableLocals = memory.mutableLocals),
    fun _ _ => False, fun _ _ => True⟩
  buildBIR := ⟨passes.buildBIR, .provedValidator, SSA.WF (language := sliceLanguage slice),
    BIR.WF (language := sliceLanguage slice), (fun ssa bir =>
      BuildsBIR (language := sliceLanguage slice) ssa bir ∧ bir.mutableLocals = ssa.mutableLocals),
    fun ssa failure =>
      match failure with
      | .resource bound =>
        bound.rule = ⟨``BIR.frameSlots, 1⟩ ∧ bound.limit = profile.maxBIRSlots ∧
          bound.measured = ssa.frameSlots ∧ bound.limit < bound.measured
      | _ => False,
    fun _ _ => True⟩
  emitLLVM := passes.emitLLVMEntry
  emitLCB := passes.emitLCBEntry

inductive Edge where
  | lexer | parser | elaborator | eligibility | typing | admission | lowerCore | lowerUnchecked
  | lowerMemory | buildSSA | buildBIR | emitLLVM | emitLCB

/-- Soundness refers to the selected repository contract for the executable edge. -/
def Stage.Sound {slice : Slice} (passes : Passes slice profile) : Edge → Prop
  | .lexer => (registry passes).lexer.Checks
  | .parser => (registry passes).parser.Checks
  | .elaborator => (registry passes).elaborator.Checks
  | .eligibility => (registry passes).eligibility.Checks
  | .typing => (registry passes).typing.Checks
  | .admission => (registry passes).admission.Checks
  | .lowerCore => (registry passes).lowerCore.Checks
  | .lowerUnchecked => (registry passes).lowerUnchecked.Checks
  | .lowerMemory => (registry passes).lowerMemory.Checks
  | .buildSSA => (registry passes).buildSSA.Checks
  | .buildBIR => (registry passes).buildBIR.Checks
  | .emitLLVM => (registry passes).emitLLVM.Checks
  | .emitLCB => (registry passes).emitLCB.Checks

private def Registry.chain {slice : Slice} {Output : Type} (r : Registry slice profile) (unchecked : Bool)
    (backend : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure Output) :
    Pipeline Source Output :=
  let lowering := Pipeline.next r.lowerMemory <| .next r.buildSSA <| .next r.buildBIR <|
    .next backend .done
  let core := if unchecked then Pipeline.next r.eligibility (.next r.typing (.next r.lowerUnchecked lowering))
    else .next r.admission (.next r.lowerCore lowering)
  .next r.lexer (.next r.parser (.next r.elaborator core))

def Registry.llvmPipeline {slice : Slice} (r : Registry slice profile) (unchecked : Bool) :
    Pipeline Source (LLVMInput profile) :=
  r.chain unchecked r.emitLLVM

def Registry.lcbPipeline {slice : Slice} (r : Registry slice profile) (unchecked : Bool) :
    Pipeline Source (LCBInput profile) :=
  r.chain unchecked r.emitLCB

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

private theorem compose {A B : Type} {pipeline : Pipeline A B}
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

private def backendOutput (profile : Profile) : BackendKind → Type
  | .llvm => LLVMInput profile
  | .lcb => LCBInput profile

def Registry.Output (profile : Profile) : Type :=
  backendOutput profile profile.backend

private def selectBackend {profile : Profile} (llvm : Pipeline Source (LLVMInput profile))
    (lcb : Pipeline Source (LCBInput profile)) :
    (backend : BackendKind) → Pipeline Source (backendOutput profile backend)
  | .llvm => llvm
  | .lcb => lcb

def Registry.pipeline {slice : Slice} (passes : Passes slice profile) (unchecked : Bool) :
    Pipeline Source (Registry.Output profile) :=
  let entries := registry passes
  selectBackend (entries.llvmPipeline unchecked) (entries.lcbPipeline unchecked) profile.backend

theorem Pipeline.endToEnd {slice : Slice} (passes : Passes slice profile) (unchecked : Bool)
    {input : Source} {output : Registry.Output profile}
    (validated : (Registry.pipeline passes unchecked).Validated)
    (trusted : (Registry.pipeline passes unchecked).Trusted)
    (ready : (Registry.pipeline passes unchecked).Ready input)
    (ran : (Registry.pipeline passes unchecked).run input = .ok output) :
    (Registry.pipeline passes unchecked).Corresponds input output :=
  compose validated trusted ready ran

/-- The guarded LCB pipeline composes from validation alone. Any program
that needs linked execution is refused by the registered backend edge. -/
theorem registeredLCBEndToEnd {slice : Slice} (passes : Passes slice profile)
    (unchecked : Bool)
    {input : Source} {output : LCBInput profile}
    (validated : ((registry passes).lcbPipeline unchecked).Validated)
    (ready : ((registry passes).lcbPipeline unchecked).Ready input)
    (ran : ((registry passes).lcbPipeline unchecked).run input = .ok output) :
    ((registry passes).lcbPipeline unchecked).Corresponds input output := by
  refine compose validated ?_ ready ran
  cases unchecked <;> repeat' first | apply And.intro | exact True.intro | intro

end Concrete.Interfaces
