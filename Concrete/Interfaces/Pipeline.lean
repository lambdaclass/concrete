import Concrete.Interfaces.Contracts
import ConcreteSpec.StageGrammar

namespace Concrete.Interfaces

open ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Concrete.Contracts (EvidenceKind)

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

def Passes.elaborator (passes : Passes grammar language lang profile) (spec : FrontendSpec (grammar.value grammar.start) language)
    (admission : Admission language) (inv : FrontendInvariants (grammar.value grammar.start) language) :
    Stage (Parsed (grammar.value grammar.start)) ElabRefusal (CoreInput language) where
  run := passes.elaborate
  kind := .directProof
  inputInvariant := inv.ast
  outputInvariant input := input.WF ∧ admission.fragment input.slice input.payload ∧
    inv.conformance input
  success := Elaborates spec
  refusal := spec.elabRefusal

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

structure Registry {count : Nat} (grammar : SharedGrammar count) (language : CoreLanguage)
    (lang : LoweringLanguage language.operands.Ty) (profile : Profile) where
  coreModel : CoreModel language
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

def registry (passes : Passes grammar language lang profile) (front : FrontendSpec (grammar.value grammar.start) language)
    (inv : FrontendInvariants (grammar.value grammar.start) language) (admission : Admission language)
    (m : LoweringModels language lang profile) (fails : LoweringFailures language lang profile) :
    Registry grammar language lang profile where
  coreModel := m.core
  lexer := passes.lexer front inv
  parser := passes.parserEntry inv
  elaborator := passes.elaborator front admission inv
  admission := admissionEntry admission
  lowerCore := ⟨passes.lowerCore, .directProof, Admitted.Valid admission,
    m.mirInvariant, LowersCore m, fails.core⟩
  lowerUnchecked := ⟨passes.lowerUnchecked, .directProof,
    fun input => input.WF ∧ inv.conformance input,
    m.mirInvariant, LowersUnchecked m, fails.unchecked⟩
  lowerMemory := ⟨passes.lowerMemory, .directProof, m.mirInvariant,
    m.memoryInvariant, LowersMemory m, fails.mir⟩
  buildSSA := ⟨passes.buildSSA, .provedValidator, m.memoryInvariant,
    m.ssaInvariant, BuildsSSA m, fails.memory⟩
  buildBIR := ⟨passes.buildBIR, .provedValidator, m.ssaInvariant,
    fun bir => m.birInvariant bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    BuildsBIR m, fails.ssa⟩
  emitLLVM := ⟨passes.emitLLVM, .namedPremise "LLVM printer",
    fun bir => m.birInvariant bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    fun output => ∃ bir, EmitsLLVM m bir output, EmitsLLVM m, fails.llvm⟩
  emitLCB := ⟨passes.emitLCB, .directProof,
    fun bir => m.birInvariant bir ∧ bir.TrapSlotsWF ∧ bir.frameSlots ≤ profile.maxBIRSlots,
    fun output => ∃ bir, EmitsLCB m bir output, EmitsLCB m, fails.lcb⟩

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

inductive Pipeline.Sound : {A B : Type} → Pipeline A B → Prop where
  | done {A : Type} : Sound (.done : Pipeline A A)
  | last {A R B : Type} {edge : Stage A R B} : edge.Sound → Sound (.next edge .done)
  | next {A R B S C D : Type} {edge : Stage A R B} {after : Stage B S C}
      {rest : Pipeline C D} : edge.Sound → edge.Compatible after →
      Sound (.next after rest) → Sound (.next edge (.next after rest))

end Concrete.Interfaces
