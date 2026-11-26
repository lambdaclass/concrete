import Concrete.Interfaces.Contracts
import ConcreteSpec.StageGrammar

namespace Concrete.Interfaces

open ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Concrete.Contracts (EvidenceKind Graph)

variable {count : Nat} {grammar : SharedGrammar count} {language : CoreLanguage}
  {lang : LoweringLanguage language.operands.Ty} {profile : Profile}

/-- Executable signatures are collected separately from the declarative spec and models.
The registry below builds its graphs from these values. -/
structure Passes {count : Nat} (grammar : SharedGrammar count) (language : CoreLanguage)
    (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  lex : Source → Except LexRefusal TokenStream
  parser : Parser grammar
  elaborate : Parsed (grammar.value grammar.start) → Except ElabRefusal (CoreInput language)
  lowerCore : Admitted language → Except StageFailure (MIR lang profile)
  lowerUnchecked : CoreInput language → Except StageFailure (MIR lang profile)
  lowerMemory : MIR lang profile → Except StageFailure (Memory lang profile)
  buildSSA : Memory lang profile → Except StageFailure (SSA lang profile)
  buildBIR : SSA lang profile → Except StageFailure (BIR lang profile)
  emitLLVM : BIR lang profile → Except StageFailure (LLVMInput profile)
  emitLCB : BIR lang profile → Except StageFailure (LCBInput profile)

structure FrontendInvariants (Ast : Type) (language : CoreLanguage) where
  source : Source → Prop
  ast : Parsed Ast → Prop
  conformance : CoreInput language → Prop

def Passes.lexer (passes : Passes grammar language lang profile) (spec : FrontendSpec (grammar.value grammar.start) language)
    (inv : FrontendInvariants (grammar.value grammar.start) language) : Stage Source LexRefusal TokenStream where
  run := passes.lex
  kind := .directProof
  inputInvariant := inv.source
  outputInvariant stream := stream.parserView.WF
  success := Lexes spec
  refusal source
    | .malformed rule span => spec.malformedToken source rule span
    | .resource failure => spec.lexLimit source failure ∧ failure.limit < failure.measured

def Passes.parserEntry (passes : Passes grammar language lang profile)
    (inv : FrontendInvariants (grammar.value grammar.start) language) :
    Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start)) where
  run stream := (passes.parser.run stream.parserView).2.getD
    (.error (.malformed passes.parser.malformedRule ⟨stream.source.entryFile, 0, 0, 1, 1⟩))
  kind := .directProof
  inputInvariant stream := stream.parserView.WF
  outputInvariant := inv.ast
  success stream output := (passes.parser.run stream.parserView).2 = some (.ok output) ∧
    Parses passes.parser stream.parserView output ∧
    (passes.parser.run stream.parserView).1 ≤ passes.parser.bound stream.parserView
  refusal stream failure := (passes.parser.run stream.parserView).2 = some (.error failure) ∧
    Refuses grammar stream.parserView failure ∧
    (passes.parser.run stream.parserView).1 ≤ passes.parser.bound stream.parserView

def elaborationEntry {Ast : Type}
    (run : Parsed Ast → Except ElabRefusal (CoreInput language))
    (spec : FrontendSpec Ast language) (admission : Admission language)
    (inv : FrontendInvariants Ast language) : Stage (Parsed Ast) ElabRefusal (CoreInput language) where
  run := run
  kind := .directProof
  inputInvariant := inv.ast
  outputInvariant input := input.WF ∧ admission.fragment input.slice input.payload ∧
    inv.conformance input
  success := Elaborates
  refusal := spec.elabRefusal

def Passes.elaborator (passes : Passes grammar language lang profile)
    (spec : FrontendSpec (grammar.value grammar.start) language)
    (admission : Admission language) (inv : FrontendInvariants (grammar.value grammar.start) language) :
    Stage (Parsed (grammar.value grammar.start)) ElabRefusal (CoreInput language) :=
  elaborationEntry passes.elaborate spec admission inv

def Admitted.Valid (admission : Admission language) (admitted : Admitted language) : Prop :=
  admitted.core.WF ∧ admission.fragment admitted.core.slice admitted.core.payload ∧
  admission.accepted admitted.core ∧ admission.statics admitted.core ∧
  ValidDischarges admitted.core admitted.discharges

def admissionEntry (admission : Admission language) :
    Stage (CoreInput language) CheckerRefusal (Admitted language) where
  run := check admission
  kind := .directProof
  inputInvariant input := input.WF ∧ admission.fragment input.slice input.payload
  outputInvariant := Admitted.Valid admission
  success := Admits admission
  refusal := admission.rejects

/-- Typed refusal obligations belong to the edge on the exact input and profile. -/
structure LoweringFailures (language : CoreLanguage) (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  core : Admitted language → StageFailure → Prop
  unchecked : CoreInput language → StageFailure → Prop
  mir : MIR lang profile → StageFailure → Prop
  memory : Memory lang profile → StageFailure → Prop
  ssa : SSA lang profile → StageFailure → Prop
  llvm : BIR lang profile → StageFailure → Prop
  lcb : BIR lang profile → StageFailure → Prop

structure LoweringInvariants (lang : LoweringLanguage language.operands.Ty)
    (profile : Profile) where
  mir : MIR lang profile → Prop
  memory : Memory lang profile → Prop
  ssa : SSA lang profile → Prop
  bir : BIR lang profile → Prop

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

structure Registry {count : Nat} (grammar : SharedGrammar count) (language : CoreLanguage)
    (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  lexer : Stage Source LexRefusal TokenStream
  parser : Stage TokenStream ParseRefusal (Parsed (grammar.value grammar.start))
  elaborator : Stage (Parsed (grammar.value grammar.start)) ElabRefusal (CoreInput language)
  admission : Stage (CoreInput language) CheckerRefusal (Admitted language)
  lowerCore : Stage (Admitted language) StageFailure (MIR lang profile)
  lowerUnchecked : Stage (CoreInput language) StageFailure (MIR lang profile)
  lowerMemory : Stage (MIR lang profile) StageFailure (Memory lang profile)
  buildSSA : Stage (Memory lang profile) StageFailure (SSA lang profile)
  buildBIR : Stage (SSA lang profile) StageFailure (BIR lang profile)
  emitLLVM : Stage (BIR lang profile) StageFailure (LLVMInput profile)
  emitLCB : Stage (BIR lang profile) StageFailure (LCBInput profile)

/-- Check for calls before invoking the single-job backend. -/
def Passes.emitSingleLCB (passes : Passes grammar language lang profile)
    (bir : BIR lang profile) : Except StageFailure (LCBInput profile) :=
  if bir.requiresLCBLinking then .error .lcbLinkingUnavailable else passes.emitLCB bir

def registry (passes : Passes grammar language lang profile) (front : FrontendSpec (grammar.value grammar.start) language)
    (inv : FrontendInvariants (grammar.value grammar.start) language) (admission : Admission language)
    (lower : LoweringInvariants (language := language) lang profile)
    (fails : LoweringFailures language lang profile) :
    Registry grammar language lang profile where
  lexer := passes.lexer front inv
  parser := passes.parserEntry inv
  elaborator := passes.elaborator front admission inv
  admission := admissionEntry admission
  lowerCore := ⟨passes.lowerCore, .directProof, Admitted.Valid admission,
    lower.mir, LowersCore (language := language), fails.core, fun _ _ => True⟩
  lowerUnchecked := ⟨passes.lowerUnchecked, .directProof,
    fun input => input.WF ∧ inv.conformance input,
    lower.mir, LowersUnchecked (language := language), fails.unchecked, fun _ _ => True⟩
  lowerMemory := ⟨passes.lowerMemory, .directProof, lower.mir,
    lower.memory, LowersMemory (language := language), fails.mir, fun _ _ => True⟩
  buildSSA := ⟨passes.buildSSA, .provedValidator, lower.memory,
    lower.ssa, BuildsSSA (language := language), fails.memory, fun _ _ => True⟩
  buildBIR := ⟨passes.buildBIR, .provedValidator, lower.ssa,
    fun bir => lower.bir bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    BuildsBIR (language := language), fails.ssa, fun _ _ => True⟩
  emitLLVM := ⟨passes.emitLLVM, .namedPremise "P_print",
    fun bir => lower.bir bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    fun output => ∃ bir : BIR lang profile, LLVMMetadataMatches bir output, LLVMMetadataMatches, fails.llvm, P_print (language := language)⟩
  emitLCB := ⟨passes.emitSingleLCB, .directProof,
    fun bir => lower.bir bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    fun output => ∃ bir : BIR lang profile, EmitsLCB language bir output, EmitsLCB language, fun bir failure =>
      match failure with
      | .lcbLinkingUnavailable => bir.requiresLCBLinking = true
      | _ => fails.lcb bir failure,
      fun _ _ => True⟩

private def Registry.pipeline {Output : Type} (r : Registry grammar language lang profile) (unchecked : Bool)
    (backend : Stage (BIR lang profile) StageFailure Output) :
    Pipeline Source Output :=
  let lowering := Pipeline.next r.lowerMemory <| .next r.buildSSA <| .next r.buildBIR <|
    .next backend .done
  let core := if unchecked then Pipeline.next r.lowerUnchecked lowering
    else .next r.admission (.next r.lowerCore lowering)
  .next r.lexer (.next r.parser (.next r.elaborator core))

def Registry.llvmPipeline (r : Registry grammar language lang profile) (unchecked : Bool) :
    Pipeline Source (LLVMInput profile) :=
  r.pipeline unchecked r.emitLLVM

def Registry.lcbPipeline (r : Registry grammar language lang profile) (unchecked : Bool) :
    Pipeline Source (LCBInput profile) :=
  r.pipeline unchecked r.emitLCB

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

theorem Pipeline.endToEnd {A B : Type} {pipeline : Pipeline A B}
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

/-- The guarded LCB pipeline composes from validation alone. Any program
that needs linked execution is refused by the registered backend edge. -/
theorem registeredLCBEndToEnd (passes : Passes grammar language lang profile)
    (front : FrontendSpec (grammar.value grammar.start) language)
    (inv : FrontendInvariants (grammar.value grammar.start) language)
    (admission : Admission language) (lower : LoweringInvariants (language := language) lang profile)
    (fails : LoweringFailures language lang profile) (unchecked : Bool)
    {input : Source} {output : LCBInput profile}
    (validated : ((registry passes front inv admission lower fails).lcbPipeline unchecked).Validated)
    (ready : ((registry passes front inv admission lower fails).lcbPipeline unchecked).Ready input)
    (ran : ((registry passes front inv admission lower fails).lcbPipeline unchecked).run input = .ok output) :
    ((registry passes front inv admission lower fails).lcbPipeline unchecked).Corresponds input output := by
  refine Pipeline.endToEnd validated ?_ ready ran
  cases unchecked <;> repeat' first | apply And.intro | exact True.intro | intro

end Concrete.Interfaces
