import Concrete.IntegratedCore
import Concrete.Admission
import Concrete.Parse
import ConcreteSpec.IntegratedCore
import ConcreteSpec.Admission.Early

namespace Concrete.IntegratedCore
open Concrete.Interfaces
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageGrammar
open ConcreteSpec.StageInterface

private def source : Source :=
  ⟨Project.singleton mainSourcePath
      (SourceText.ofBytes r"fn main() -> u8 { return 42; }".toUTF8.data.toList), ⟨0⟩, slice0⟩

private def returnTokens : ParserView :=
  let token := fun start stop value => (⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, value⟩ : SurfaceToken)
  ⟨source, #[
    token 0 2 (.fixed .fn rfl), token 3 7 (.name "main"), token 7 8 (.fixed .lparen rfl),
    token 8 9 (.fixed .rparen rfl), token 10 12 (.fixed .arrow rfl), token 13 15 (.fixed .u8 rfl),
    token 16 17 (.fixed .lbrace rfl), token 18 24 (.fixed .return_ rfl),
    token 25 27 (.integer 42 .decimal none), token 27 28 (.fixed .semi rfl),
    token 29 30 (.fixed .rbrace rfl)]⟩

private def expected : CoreInput (sliceLanguage slice0) :=
  ⟨source.project, slice0, .straightLine (.zero ⟨[], .lit 42, ⟨18, 28, 1, 19, ⟨0⟩⟩, ⟨25, 27, 1, 26, ⟨0⟩⟩⟩) rfl⟩

-- The source locals 9 and 4 occupy operation positions 0 and 1. C9 indexes their unnamed siblings.
example :
    let span : ConcreteSpec.Surface0.Span := ⟨25, 27, 1, 26, ⟨0⟩⟩
    let program : EarlyProgram := ⟨[⟨⟨⟨9⟩, .atom (.lit 1)⟩, span, span, (), .unnamed none⟩,
      ⟨⟨⟨4⟩, .atom (.lit 2)⟩, span, span, (), .unnamed none⟩], .var ⟨4⟩, span, span⟩
    (AList.map Origin.evidenceKey (AdmissionView.zero program).execution.origins) =
      [(⟨0⟩, ⟨.function earlyEntry, ⟨[⟨.binding, .ordinal 0⟩]⟩⟩),
       (⟨1⟩, ⟨.function earlyEntry, ⟨[⟨.binding, .ordinal 1⟩]⟩⟩),
       (⟨2⟩, ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩)] := by decide

private def sourceKeys (sourceText : String) : Option SiteKeyTable := do
  let tokens ← (Lex.lex sourceText.toUTF8).toOption
  let ast ← (Parse.parse (Parse.eofSpan sourceText.toUTF8) tokens).toOption
  let program ← (SurfacePreparation.prepareSurface0 ast).toOption
  return AList.map Origin.evidenceKey (AdmissionView.zero program).execution.origins

-- Inserting y changes x's lexical site. Both source programs issue the spec's full table,
-- and looking up x at either site yields the very same named key.
example :
    let old := sourceKeys r"fn main() -> u8 { let x: u8 = 1; return x; }"
    let new := sourceKeys r"fn main() -> u8 { let y: u8 = 1; let x: u8 = 1; return x; }"
    let bind := fun (n : String) => (⟨.binding, .named ⟨n⟩, []⟩ : KeyConstruct)
    let ret : KeyConstruct := ⟨.return, .unnamed none, []⟩
    let x : StableKey := ⟨.function earlyEntry, ⟨[⟨.binding, .named ⟨"x"⟩⟩]⟩⟩
    old = some (KeyConstruct.assignFunctionTable earlyEntry 0 [bind "x", ret]) ∧
    new = some (KeyConstruct.assignFunctionTable earlyEntry 0 [bind "y", bind "x", ret]) ∧
    (old.bind (AList.lookup · ⟨0⟩)) = some x ∧ (new.bind (AList.lookup · ⟨1⟩)) = some x := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

-- A nested addition creates a temporary before y. The return addition creates another
-- after y. Only those temporaries occupy binding ordinals.
example :
    let src := r"fn main() -> u8 { let x: u8 = 1; let y: u8 = (x + 1) + 1; return y + x; }"
    let temp : KeyConstruct := ⟨.binding, .unnamed none, []⟩
    sourceKeys src = some (KeyConstruct.assignFunctionTable earlyEntry 0
      [⟨.binding, .named ⟨"x"⟩, []⟩, temp, ⟨.binding, .named ⟨"y"⟩, []⟩, temp,
       ⟨.return, .unnamed none, []⟩]) := by
  with_unfolding_all rfl

private def parseAndElaborate (tokens : ParserView) : Option (CoreInput (sliceLanguage slice0)) := do
  let parsed ← ((sliceParser slice0).run tokens).2
  let ast ← parsed.toOption
  let surface ← (elaborate slice0 ast).toOption
  surface.prepare.toOption

-- Exercise the registered parser implementation and exact elaboration function.
example : parseAndElaborate returnTokens = some expected := by
  with_unfolding_all rfl

-- A parser-view token can have the same payload and still carry an invalid end offset.
example :
    let bad := {returnTokens with tokens := returnTokens.tokens.modify 8 (
      fun token => {token with span := {token.span with endOffset := 31}})}
    ((sliceParser slice0).run bad).2 = some (.error (.invalidStream ⟨⟨0⟩, 25, 31, 1, 26⟩)) := by
  with_unfolding_all rfl

example :
    elaborate slice0 ⟨source, .inl (.node .body [])⟩ =
      .error (.diagnostic ⟨``Elab.surfaceProgram, 1⟩ ⟨⟨0⟩, 0, 0, 1, 1⟩ []) := rfl

-- An empty body crosses elaboration intact and receives its refusal at preparation.
example :
    ((elaborate slice0 ⟨source, .inl (.node .program [
      .leaf ⟨⟨⟨0⟩, 3, 7, 1, 4⟩, .name "main"⟩, .node .body []])⟩).toOption.map (·.prepare)) =
      some (.error (Surface0Issue.fallthrough ⟨3, 7, 1, 4, ⟨0⟩⟩).render) := rfl

example :
    elaborate slice0 ⟨source, .inr true⟩ =
      .error (.diagnostic ⟨``SliceElaboration, 1⟩ ⟨⟨0⟩, 0, 0, 1, 1⟩ []) := rfl

private def slice1 : Slice := ⟨⟨1, by decide⟩⟩

example :
    elaborate slice1 ⟨{source with slice := slice1}, .inr true⟩ =
      .error (.unsupportedSlice slice1) := rfl

example :
    elaborate slice0 ⟨{source with slice := slice1}, .inr true⟩ =
      .error (.diagnostic ⟨``Parsed.ElaborationDomain, 1⟩ ⟨⟨0⟩, 0, 0, 1, 1⟩ []) := rfl

private def profile : Profile := ⟨.lcb, .x86_64Linux, 3, 4096⟩

-- Keep the real elaborator and checker, and the shared static traversal.
private def fragmentPasses (slice : Slice) : Passes slice profile where
  lex input := .error (.missingFile input.entryFile)
  elaborate := elaborate slice
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  lowerUnchecked _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  lowerMemory _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  buildSSA _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  buildBIR _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  emitLLVM _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  emitLCB _ := .error (.invalidInput ⟨``elaborate, 1⟩)

private abbrev elaborationPasses := fragmentPasses slice0

example : (registry elaborationPasses).elaborator.Validated :=
  validated elaborationPasses rfl

open LeanCompilerBackend.Verified

private def wordJob (word : BitVec 32) (sites : TrapTable) : LCBInput profile :=
  ⟨⟨.word, #[], #[], #[⟨#[], #[], .ret (.literal (.word word))⟩]⟩, [], sites, ByteArray.empty⟩

private def entryState : LeanCompilerBackend.Verified.Source.State := ⟨0, 0, #[]⟩

example :
    (LCBInput.Execution (profile := profile) Empty source.project).result (wordJob 42 #[]) (some entryState)
      (.returned 42 ByteArray.empty) :=
  lcb_return_word source.project (wordJob 42 #[]) entryState 42 (by with_unfolding_all rfl)

private def overflow : TrapEntry :=
  ⟨.overflow, ⟨⟨9⟩, ⟨⟨0⟩, 25, 27, 1, 26⟩, ⟨.function earlyEntry, ⟨[⟨.return, .ordinal 0⟩]⟩⟩⟩⟩

private def trapJob : LCBInput profile :=
  wordJob 257 #[{overflow with kind := .abort}, overflow]

example :
    (LCBInput.Execution (profile := profile) Empty source.project).result trapJob (some entryState)
      (.trapped 134 .overflow ⟨⟨9⟩, ⟨"main.con", 1, 26⟩⟩
        "concrete: trap: overflow at main.con:1:26 (site 9)\n".toUTF8) := by
  with_unfolding_all
    exact lcb_trap_word source.project trapJob entryState ⟨1⟩ overflow _ (by decide) rfl rfl rfl

-- The stable source operation number is used in the rendered UTF-8 diagnostic.
example :
    trapDiagnostic source.project .overflow ⟨9⟩ ⟨⟨0⟩, 25, 27, 1, 26⟩ =
      "concrete: trap: overflow at main.con:1:26 (site 9)\n".toUTF8 := by
  with_unfolding_all rfl


private def tokenName : ConcreteSpec.Name := ⟨"Token"⟩
private def tokenId : DeclId := ⟨earlyEntry.module, tokenName⟩
private def binderSpan : ConcreteSpec.Surface0.Span := ⟨3, 7, 1, 4, ⟨0⟩⟩
private def useSpan : ConcreteSpec.Surface0.Span := ⟨25, 27, 1, 26, ⟨0⟩⟩
private def introToken : LocatedStraightBinding :=
  ⟨.bind (.name ⟨0⟩) (.nominal tokenName) (.construct tokenName), useSpan, binderSpan, .unnamed none⟩
private def takeToken : LocatedStraightBinding :=
  ⟨.destructure tokenName (.move ⟨0⟩), useSpan, useSpan, .unnamed none⟩
private def tokenView (bindings : List LocatedStraightBinding) : AdmissionView :=
  .one ⟨[(tokenName, binderSpan)], bindings, .ret (.lit 42), useSpan⟩
private def tokenInput (bindings : List LocatedStraightBinding) : CoreInput (sliceLanguage sliceOne) :=
  ⟨source.project, sliceOne, .straightLine (tokenView bindings) rfl⟩

-- Normal execution constructs and consumes the logical owner. A second
-- destructure fails in both the checker and the instrumented machine.
private def runTokens (bindings : List LocatedStraightBinding) :=
  let payload := (tokenInput bindings).machineProgram
  Exec.run payload.body (fun _ p => nomatch p) 32 payload.initial

example : (ConcreteSpec.Admission.admitInput (tokenInput [introToken, takeToken])).isOk = true ∧
    (runTokens [introToken, takeToken]).ending = .final (.returned (.scalar (.int .u8 42))) ∧
    ConcreteSpec.checkProgram (tokenView [introToken, takeToken, takeToken]).program =
      .error (.atPosition 2 (.movedBinding ⟨0⟩)) ∧
    (runTokens [introToken, takeToken, takeToken]).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

private def wildToken : LocatedStraightBinding :=
  ⟨.bind .wild (.nominal tokenName) (.construct tokenName), useSpan, binderSpan, .unnamed none⟩

private def wildByte : LocatedStraightBinding :=
  ⟨.bind .wild .u8 (.scalar (.atom (.lit 1))), useSpan, binderSpan, .unnamed none⟩

-- Retain the source declarations and both operation origins in a one-block
-- candidate whose only executable operation returns the chosen byte.
private def wildcardMIR (status : UInt8) : MIR (sliceLoweringLanguage sliceOne) profile :=
  let core := tokenInput [wildToken]
  let u8 : (sliceLanguage sliceOne).operands.Ty := .inl (.scalar (.int .u8))
  let block : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8, .literal ⟨[status]⟩⟩),
    List.map Prod.snd core.machineProgram.origins⟩
  ⟨core.project, earlyEntry, [(earlyEntry, ⟨[], some u8⟩)],
    [(earlyEntry, [], ⟨⟨0⟩, #[block]⟩)], [], core.machineProgram.declarations⟩

private def inputOf (v : AdmissionView) : CoreInput (sliceLanguage v.slice) :=
  ⟨source.project, v.slice, .straightLine v rfl⟩

/-- Check the first offending operation and rule, and exclude *every* MIR.
These programs satisfy the independent value-typing judgment. -/
private def RefusedStatic (v : AdmissionView) (pos : Nat) (why : ConcreteSpec.Admission.Reason)
    (rule : RuleId) : Prop :=
  let core := inputOf v
  let edge := (registry (fragmentPasses v.slice)).typing
  (core.StaticallyTyped ∧ rule ∈ registeredRules) ∧
  ConcreteSpec.checkProgramWith .runtime v.program = .error (.atPosition pos why) ∧
  edge.run core = .error (.invalidInput rule) ∧ edge.refusal core (.invalidInput rule) ∧
  ∀ {target : Profile} (mir : MIR (sliceLoweringLanguage v.slice) target), ¬ LowersUnchecked core mir

private theorem refusedStatic {v pos why rule} (typed : ConcreteSpec.Admission.StaticallyTyped v.program)
    (failed : ConcreteSpec.checkProgramWith .runtime v.program = .error (.atPosition pos why))
    (cited : v.staticRule (.atPosition pos why) = rule) (registered : rule ∈ registeredRules := by decide) : RefusedStatic v pos why rule := by
  have rejected : (inputOf v).checkStatic = .error rule := by
    simp [CoreInput.checkStatic, inputOf, CoreInput.decode, failed, cited, Except.mapError,
      Except.bind, Bind.bind]
  refine ⟨⟨⟨v, ⟨rfl, rfl, rfl⟩, typed⟩, registered⟩, failed, ?_, ?_, ?_⟩
  · change (if v.slice = v.slice then _ else _) = Except.error (StageFailure.invalidInput rule)
    simp [rejected, Except.map, Except.mapError]
  · change (if v.slice = v.slice then _ else _)
    rw [ite_eq_left rfl]
    exact ⟨rule, rejected, rfl⟩
  · intro target mir lowered
    have ok := (ConcreteSpec.Admission.checkStatic_iff lowered.1.1).2 lowered.1
    rw [rejected] at ok
    contradiction

private def byteBinding (x value : Nat) : LocatedStraightBinding :=
  ⟨.bind (.name ⟨x⟩) .u8 (.scalar (.atom (.lit value))), useSpan, binderSpan, .unnamed none⟩

private def moveByte : LocatedStraightBinding :=
  ⟨.bind (.name ⟨1⟩) .u8 (.move ⟨0⟩), useSpan, binderSpan, .unnamed none⟩

private def duplicateView : AdmissionView :=
  let bind (n : Nat) : LocatedBinding := ⟨⟨⟨0⟩, .atom (.lit n)⟩, useSpan, binderSpan, (), .unnamed none⟩
  .zero ⟨[bind 1, bind 42], .var ⟨0⟩, useSpan, useSpan⟩

private def converseView : AdmissionView :=
  .one ⟨[(tokenName, binderSpan)], [introToken, takeToken, byteBinding 0 42, moveByte],
    .ret (.var ⟨1⟩), useSpan⟩

private def copyToken : LocatedStraightBinding :=
  ⟨.bind (.name ⟨1⟩) (.nominal tokenName) (.scalar (.atom (.var ⟨0⟩))), useSpan, binderSpan, .unnamed none⟩

private theorem typedTokenConstruct :
    ConcreteSpec.HasType [⟨tokenName, false, []⟩] [] (.mk tokenName []) (.struct tokenName) :=
  .mk rfl ⟨.nil, nofun, fun _ _ => nofun⟩

example : RefusedStatic (tokenView [introToken]) 1 (.liveLinearBinding ⟨0⟩) ⟨``ConcreteSpec.Core.OwnershipRule.ReturnDischarged, 1⟩ :=
  refusedStatic ⟨by decide, .letName typedTokenConstruct (.ret (.int rfl))⟩ rfl rfl

example : RefusedStatic duplicateView 1 (.duplicateBinding ⟨0⟩) ⟨``ConcreteSpec.Core.OwnershipRule.FreshBinder, 1⟩ :=
  refusedStatic ⟨by decide, .letName (.int rfl) (.letName (.int rfl) (.ret (.copy rfl)))⟩ rfl rfl

private theorem tokenPattern : ConcreteSpec.PatOk ⟨tokenName, false, []⟩ [] :=
  ⟨.nil, nofun, fun _ _ => nofun⟩

example : RefusedStatic converseView 2 (.duplicateBinding ⟨0⟩) ⟨``ConcreteSpec.Core.OwnershipRule.FreshBinder, 1⟩ :=
  refusedStatic ⟨by decide, .letName typedTokenConstruct (.letStruct (.move rfl) rfl tokenPattern
    (.letName (.int rfl) (.letName (.move rfl) (.ret (.copy rfl)))))⟩ rfl rfl

example : RefusedStatic (tokenView [introToken, takeToken, takeToken]) 2 (.movedBinding ⟨0⟩)
    ⟨``ConcreteSpec.Core.OwnershipRule.ReadAllowed, 1⟩ :=
  refusedStatic ⟨by decide, .letName typedTokenConstruct (.letStruct (.move rfl) rfl tokenPattern
    (.letStruct (.move rfl) rfl tokenPattern (.ret (.int rfl))))⟩ rfl rfl

example : RefusedStatic (tokenView [byteBinding 0 42, moveByte]) 1 (.moveOfCopy ⟨0⟩)
    ⟨``ConcreteSpec.Core.OwnershipRule.ReadAllowed, 1⟩ :=
  refusedStatic ⟨by decide, .letName (.int rfl) (.letName (.move rfl) (.ret (.int rfl)))⟩ rfl rfl

example : RefusedStatic (tokenView [introToken, copyToken]) 1 (.copyOfLinear ⟨0⟩)
    ⟨``ConcreteSpec.Core.OwnershipRule.ReadAllowed, 1⟩ :=
  refusedStatic ⟨by decide, .letName typedTokenConstruct (.letName (.copy rfl) (.ret (.int rfl)))⟩ rfl rfl

private theorem wildcardRefused : RefusedStatic (tokenView [wildToken]) 0 (.discardLinear (.struct tokenName))
    ⟨``ConcreteSpec.WildcardPermitted, 1⟩ :=
  refusedStatic ⟨by decide, .letWild typedTokenConstruct (.ret (.int rfl))⟩ rfl rfl

private theorem exitSeven :
    (MIR.Execution (language := sliceLanguage sliceOne)).observes (wildcardMIR 7)
      (.returned 7 ByteArray.empty) := by
  exact ⟨_, ⟨_, rfl, .nil _⟩, by with_unfolding_all rfl⟩

example : ¬ (registry (fragmentPasses sliceOne)).lowerUnchecked.success
    (tokenInput [wildToken]) (wildcardMIR 7) := wildcardRefused.2.2.2.2 _

-- On an eligible source the behavioral obligation itself excludes the wrong byte.
example : ¬ LowersUnchecked (tokenInput [wildByte]) (wildcardMIR 7) := by
  intro lowered
  obtain ⟨_, o, agrees, only⟩ := Agreement.unchecked_agreement lowered.1
    (tokenView [wildByte]) ⟨rfl, rfl, rfl⟩
  have correct : o = .returned 42 ByteArray.empty := by with_unfolding_all exact agrees
  have wrong := (only _).1 ((Agreement.lowered_observations lowered).2 _ exitSeven)
  rw [correct] at wrong
  cases wrong

-- A Copy wildcard crosses typing and both executions return 42.
example (candidate : MIR (sliceLoweringLanguage sliceOne) profile) :
    let core := tokenInput [wildByte]
    let stages := registry {fragmentPasses sliceOne with lowerUnchecked := fun _ => .ok candidate}
    stages.typing.run core = .ok core ∧ stages.lowerUnchecked.run core = .ok candidate ∧
      ConcreteSpec.run (tokenView [wildByte]).program 10 = .exit 42 ∧
      (runTokens [wildByte]).ending = .final (.returned (.scalar (.int .u8 42))) := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

-- The terminator and the variable's declaration have distinct coordinates.
example :
    let why := ConcreteSpec.Admission.Refusal.atPosition 1 (.liveLinearBinding ⟨0⟩)
    ConcreteSpec.checkProgram (tokenView [introToken]).program = .error why ∧
    (tokenView [introToken]).render why = some ⟨⟨``OwnershipRule.ReturnDischarged, 1⟩,
      surfaceFileSpan useSpan, [(⟨"binding"⟩, surfaceFileSpan binderSpan)], none, .core why⟩ :=
  ⟨rfl, rfl⟩

example :
    let later : Slice := ⟨⟨2, by decide⟩⟩
    ({tokenInput [introToken] with slice := later}).decode = .error (.unsupportedSlice later) := rfl

private def sourceReturn (e : ConcreteSpec.Surface0.Expr) : AdmissionInput (sliceLanguage slice0) :=
  .pending source.project (.zero ⟨"main", binderSpan, [.ret e useSpan]⟩) rfl

example :
    let valid := sourceReturn (.integer 42 none useSpan)
    let forged := (Surface0Issue.unknownLocal "absent" useSpan).render
    ¬ valid.CheckerRefuses forged := by
  intro _ _ refusal
  obtain ⟨_, v, reason, represents, checked, _⟩ := refusal
  have actual := (CoreInput.decode_iff _ v).2 represents
  change Except.ok (.zero ⟨[], .lit 42, useSpan, useSpan⟩) = Except.ok v at actual
  cases Except.ok.inj actual
  cases checked

example :
    let unknown := sourceReturn (.local "absent" useSpan)
    let issue := (Surface0Issue.unknownLocal "absent" useSpan).render
    (registry elaborationPasses).admission.run unknown = .error (.refused issue) ∧
    (registry elaborationPasses).admission.refusal unknown (.refused issue) ∧
    (registry elaborationPasses).eligibility.run unknown = .error .unpreparable ∧
    (registry elaborationPasses).eligibility.refusal unknown .unpreparable :=
  ⟨rfl, ⟨rfl, rfl⟩, rfl, rfl, .inr ⟨_, rfl⟩⟩

private def binding (name : String) : ConcreteSpec.Surface0.Statement :=
  .bind ⟨name, binderSpan, .integer 42 none useSpan⟩ useSpan

private def sourceBody (body : List ConcreteSpec.Surface0.Statement) : AdmissionInput (sliceLanguage slice0) :=
  .pending source.project (.zero ⟨"main", binderSpan, body⟩) rfl

-- The same located syntax crosses elaboration in every case. Body shape
-- precedes name resolution, and duplicate/reserved binders have distinct rules.
example :
    let ret := ConcreteSpec.Surface0.Statement.ret (.integer 42 none useSpan) useSpan
    (sourceBody [binding "x"]).prepare = .error (.fallthrough useSpan : Surface0Issue).render ∧
    (sourceBody [ret, .ret (.local "missing" binderSpan) binderSpan]).prepare =
      .error (.unreachable binderSpan : Surface0Issue).render ∧
    (sourceReturn (.integer 42 (some .u8) useSpan)).prepare =
      .error (.integerSuffix useSpan : Surface0Issue).render ∧
    (sourceBody [binding "x", binding "x", ret]).prepare =
      .error (.duplicateBinder "x" binderSpan : Surface0Issue).render ∧
    (sourceBody [binding "main", ret]).prepare =
      .error (.reservedBinder "main" binderSpan : Surface0Issue).render :=
  ⟨rfl, rfl, rfl, rfl, rfl⟩

-- Reject forged coordinates even when the underlying source really is invalid.
example :
    ¬ (sourceReturn (.local "absent" useSpan)).CheckerRefuses
      (.unknownLocal "absent" binderSpan : Surface0Issue).render := by
  intro h
  cases h

example :
    let reason := ConcreteSpec.Admission.Refusal.atPosition 0 (.literalOutOfRange .u8 256)
    let failure := CheckerRefusal.mk ⟨``Slice0Rules.LiteralTyped, 1⟩ (surfaceFileSpan useSpan) [] none (.core reason)
    (registry elaborationPasses).admission.run (sourceReturn (.integer 256 none useSpan)) =
      .error (.refused failure) := rfl

private abbrev nominalLang := sliceLanguage sliceOne
private abbrev nominalTy : nominalLang.operands.Ty := .inl (.nominal tokenId)
private def root : MemoryPlace _ nominalTy := ⟨⟨⟨0⟩, []⟩, .materialized⟩
private def memory : Memory (sliceLoweringLanguage sliceOne) profile :=
  { project := source.project, entry := earlyEntry, signatures := []
    functions := [(earlyEntry, [(root.logical.root, nominalTy)], ⟨⟨0⟩, #[]⟩)]
    layouts := [(earlyEntry, [(root.logical.root, ⟨0, 1⟩)])]
    storage := [(earlyEntry, [(root.logical.root, .materialized)])]
    declarations := (tokenInput [introToken]).machineProgram.declarations }

private def started : MemoryRegisters nominalLang profile :=
  { frame := earlyEntry, block := ⟨0⟩, instruction := 0, virtualValues := [], materialized := [],
    live := [root.logical.root], initialized := [], bytes := Mem.Memory.empty }
private def stored : MemoryRegisters nominalLang profile :=
  {started with initialized := [root.logical.root], zeroValues := [(root.logical.root, .other tokenId)]}
private def constructed : MemoryRegisters nominalLang profile := stored.advance
private def consumed : MemoryRegisters nominalLang profile :=
  {constructed with initialized := [], zeroValues := []}

-- The physical operation writes the nominal value without allocating bytes.
-- Destructuring consumes the root and leaves a subsequent move unavailable.
example :
    memory.InstructionStep started constructed false
      (.op (opcode := .named ⟨"constructEmpty"⟩) root () .nil []) ∧
    memory.InstructionStep constructed consumed.advance false (.destructure root []) ∧
    memory.readValue consumed.advance (.move root) = none := by
  with_unfolding_all
    refine ⟨?_, ?_, rfl⟩
    · refine ⟨[], started, [], .other tokenId, [], Memory.operationStore started false, stored,
        rfl, rfl, rfl, ⟨rfl, rfl, rfl, rfl⟩, rfl, ?_, rfl⟩
      exact ⟨⟨rfl, nofun⟩, fun _ => ⟨⟨0, 1⟩, rfl, ⟨fun _ => rfl, fun _ => rfl⟩⟩, rfl, rfl, fun _ => .inl rfl⟩
    · exact ⟨.other tokenId, consumed, [], consumed, rfl,
        ⟨.nil, [], rfl, nofun, nofun⟩, rfl, .nil _, rfl⟩

example :
    let wrong := {memory with declarations := [(tokenId, ⟨tokenName, true, []⟩)]}
    nominalLang.layout wrong.declarations profile.memoryTarget nominalTy = none ∧
    ∀ next, ¬ wrong.InstructionStep started next false
      (.op (opcode := .named ⟨"constructEmpty"⟩) root () .nil []) := by
  refine ⟨rfl, ?_⟩
  rintro next ⟨_, _, _, _, _, _, _, reads, _, addresses, operation, _⟩
  cases Option.some.inj reads
  cases Option.some.inj addresses
  cases operation.2.1

-- Even an independently admissible abort cannot cross into another registry.
private def abortOnly : CoreInput (sliceLanguage sliceOne) :=
  ⟨source.project, sliceOne, .straightLine (.one ⟨[], [], .abort, useSpan⟩) rfl⟩

private def slice2 : Slice := ⟨⟨2, by decide⟩⟩

example :
    ((registry (fragmentPasses sliceOne)).admission.run (.core abortOnly)).isOk = true ∧
    (registry (fragmentPasses slice0)).admission.run (.core abortOnly) =
      .error (.fragment (.sliceMismatch slice0 sliceOne)) ∧
    (registry (fragmentPasses slice2)).admission.run (.core abortOnly) =
      .error (.fragment (.sliceMismatch slice2 sliceOne)) ∧
    ¬ (registry (fragmentPasses slice0)).admission.inputInvariant (.core abortOnly) ∧
    ¬ (registry (fragmentPasses slice2)).admission.inputInvariant (.core abortOnly) := by
  refine ⟨rfl, rfl, rfl, ?_, ?_⟩ <;> intro h <;> cases h.1

-- A direct typing call also rejects an off-slice Core.
example :
    let typing := (registry (fragmentPasses slice0)).typing
    let failure := StageFailure.invalidInput ⟨``CoreInput.slice, 1⟩
    typing.run abortOnly = .error failure ∧ typing.refusal abortOnly failure ∧
    ¬ typing.outputInvariant abortOnly := by
  exact ⟨rfl, rfl, fun h => by cases h.1⟩

example :
    ({abortOnly with slice := slice0}).decode = .error .outsideFragment ∧
    ({abortOnly with payload := .integrated abortOnly.machineProgram}).decode = .error .outsideFragment ∧
    ({expected with payload := .integrated expected.machineProgram}).decode = .error .outsideFragment :=
  ⟨rfl, rfl, rfl⟩

example (p : EarlyProgram) : (AdmissionView.zero p).program = ConcreteSpec.Slice0.toCore p.eraseLocations := rfl

example :
    let badEntry : AdmissionInput (sliceLanguage slice0) :=
      .pending source.project (.zero ⟨"other", binderSpan, []⟩) rfl
    let ret := ConcreteSpec.Surface0.Statement.ret (.integer 42 none useSpan) useSpan
    badEntry.prepare = .error (Surface0Issue.unexpectedEntry binderSpan).render ∧
    (sourceBody [binding "_", ret]).prepare = .error (Surface0Issue.wildcard binderSpan).render :=
  ⟨rfl, rfl⟩

-- This unbound return names the ID a wildcard used to acquire.
example :
    let wildcard : LocatedStraightBinding :=
      ⟨.bind .wild .u8 (.scalar (.atom (.lit 42))), binderSpan, binderSpan, .unnamed none⟩
    let view := AdmissionView.one ⟨[], [wildcard], .ret (.var ⟨0⟩), useSpan⟩
    let machine := view.execution
    ConcreteSpec.run view.program 8 = .stuck .unbound ∧
    (Exec.run machine.body (fun _ prim => nomatch prim) 32 machine.initial).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- A later RHS can also read an unbound ID. The terminator here contains no variables.
example :
    let wildcard : LocatedStraightBinding :=
      ⟨.bind .wild .u8 (.scalar (.atom (.lit 42))), binderSpan, binderSpan, .unnamed none⟩
    let useMissing : LocatedStraightBinding :=
      ⟨.bind .wild .u8 (.scalar (.add (.lit 0) (.var ⟨0⟩))), useSpan, useSpan, .unnamed none⟩
    let view := AdmissionView.one ⟨[], [wildcard, useMissing], .ret (.lit 42), useSpan⟩
    let machine := view.execution
    ConcreteSpec.run view.program 8 = .stuck .unbound ∧
    (Exec.run machine.body (fun _ prim => nomatch prim) 32 machine.initial).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl⟩

private def parseSource (text : String) := (Lex.lex text.toUTF8).bind (Parse.parseForDriver (Parse.eofSpan text.toUTF8))

example :
    ((parseSource r"fn main() -> u8 { return 42; let Option: u8 = 1; }").bind
      (Admission.prepareParsed .admission)) =
      .error (.checker "unreachable code (STMT-14) at 1:30, bytes 29..48") := by
  with_unfolding_all rfl

private def pendingSource (text : String) : Option (AdmissionInput (sliceLanguage slice0)) := do
  let ast ← (parseSource text).toOption
  return .pending (Project.singleton mainSourcePath (SourceText.ofBytes text.toUTF8.data.toList))
    (.zero ast) rfl

-- These spellings and suffixes must be checked before Core erases them.
example (purpose : SurfacePreparation.Purpose) :
    let ret := ConcreteSpec.Surface0.Statement.ret (.integer 42 none useSpan) useSpan
    (sourceBody [binding "main", ret]).prepare purpose = .error (.reservedBinder "main" binderSpan : Surface0Issue).render ∧
    (sourceBody [binding "x", binding "x", ret]).prepare purpose =
      .error (.duplicateBinder "x" binderSpan : Surface0Issue).render ∧
    (sourceReturn (.integer 42 (some .u8) useSpan)).prepare purpose =
      .error (.integerSuffix useSpan : Surface0Issue).render := by
  with_unfolding_all exact ⟨rfl, rfl, rfl⟩

private def sourceDiagnostic (text : String) : Option CheckerRefusal := do
  let input ← pendingSource text
  let .error (.refused diagnostic) := (registry elaborationPasses).admission.run input | none
  return diagnostic

example :
    let reason := ConcreteSpec.Admission.Refusal.atPosition 0
      (.additionOperandType (.int .u8 256) (.atomFailure (.outOfRange .u8 256)))
    let literalAt := fun start =>
      some (CheckerRefusal.mk ⟨``Slice0Rules.LiteralTyped, 1⟩ ⟨⟨0⟩, start, start + 3, 1, start + 1⟩ [] none (.core reason))
    sourceDiagnostic r"fn main() -> u8 { return 256 + 1; }" = literalAt 25 ∧
    sourceDiagnostic r"fn main() -> u8 { return 1 + 256; }" = literalAt 29 ∧
    sourceDiagnostic r"fn main() -> u8 { return 256 + 256; }" = literalAt 25 := by
  with_unfolding_all exact ⟨rfl, rfl, rfl⟩

example :
    let why := ConcreteSpec.Admission.Refusal.atPosition 2 (.unusedBinding ⟨0⟩)
    sourceDiagnostic r"fn main() -> u8 { let x: u8 = 1; return 20 + 22; }" =
      some ⟨⟨``Slice0Rules.LocalsUsed, 1⟩, ⟨⟨0⟩, 33, 48, 1, 34⟩,
        [(⟨"binding"⟩, ⟨⟨0⟩, 22, 23, 1, 23⟩)], none, .core why⟩ := by
  with_unfolding_all rfl

example :
    let otherName : ConcreteSpec.Name := ⟨"Other"⟩
    let other : DeclId := ⟨earlyEntry.module, otherName⟩
    let env := (tokenId, ⟨tokenName, false, []⟩) :: [(other, ⟨otherName, false, []⟩)]
    nominalLang.activeFields env nominalTy (.other other) = none ∧
    nominalLang.evalIn env [] ⟨[], Mem.Memory.empty⟩
      (.inl (.consumeEmpty tokenId (.constructEmpty other))) = .stuck ∧
    nominalLang.activeFields env nominalTy (.other tokenId) = some [] ∧
    nominalLang.evalIn env [] ⟨[], Mem.Memory.empty⟩
      (.inl (.consumeEmpty other (.constructEmpty other))) = .ok (.scalar .unit, ⟨[], Mem.Memory.empty⟩) :=
  ⟨rfl, rfl, rfl, rfl⟩

-- The same two initialized octets go through the materialized byte-range load.
example :
    let big := {linuxX8664 with byteOrder := .big}
    let ty : nominalLang.operands.Ty := .inl (.scalar (.int .i16))
    let place : MemoryPlace _ ty := ⟨⟨⟨0⟩, []⟩, .materialized⟩
    let mem : Memory (sliceLoweringLanguage sliceOne) profile := {memory with
      functions := [(earlyEntry, [(⟨0⟩, ty)], ⟨⟨0⟩, #[]⟩)],
      layouts := [(earlyEntry, [(⟨0⟩, ⟨2, 2⟩)])]}
    let block := {Mem.candidate ⟨0⟩ 2 2 2 with bytes := [.init 254 none, .init 255 none]}
    let state : MemoryRegisters nominalLang profile := {started with
      initialized := [⟨0⟩],
      materialized := [(⟨0⟩, ⟨⟨⟨0⟩, 2, some ⟨0⟩⟩, 2, 2⟩)], bytes := ⟨[(⟨0⟩, block)], 1⟩}
    nominalLang.layout [] big ty = some ⟨2, 2⟩ ∧
    nominalLang.literal ty [254, 255] = some (.scalar (.int .i16 (-2))) ∧
    mem.loadMaterialized state place big = some (.scalar (.int .i16 (-257))) ∧
    mem.loadValue state place = some (.scalar (.int .i16 (-2))) := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

-- 255 + 1 reaches the residual add instruction at site zero. The independent
-- Core evaluator, hosted dispatcher and MIR all retain the defined overflow.
private def overflowSource := r"fn main() -> u8 { return 255 + 1; }"
private def overflowProject := Project.singleton mainSourcePath
  (SourceText.ofBytes overflowSource.toUTF8.data.toList)
private def plus : ConcreteSpec.Surface0.Span := ⟨29, 30, 1, 30, ⟨0⟩⟩
private def overflowView : AdmissionView := .zero
  ⟨[⟨⟨⟨0⟩, .add (.lit 255) (.lit 1)⟩, plus, plus, (⟨25, 28, 1, 26, ⟨0⟩⟩, ⟨31, 32, 1, 32, ⟨0⟩⟩), .unnamed none⟩],
    .var ⟨0⟩, ⟨18, 33, 1, 19, ⟨0⟩⟩, plus⟩
private def overflowCore : CoreInput (sliceLanguage slice0) := ⟨overflowProject, slice0, .straightLine overflowView rfl⟩
private def plusOrigin : Origin := overflowView.located.bindingOrigins[0].2
private def returnOrigin := overflowView.located.endingOrigin.2
private abbrev byteTy : (sliceLanguage slice0).operands.Ty := .inl (.scalar (.int .u8))
private abbrev overflowMachine := overflowCore.machineProgram
private def nextConfig (q : Exec.Config overflowMachine.machine) :=
  match Exec.step overflowMachine.body q with
  | some (.next next) => next
  | _ => q

private def overflowMIR : MIR (sliceLoweringLanguage slice0) profile :=
  let dst : Place _ byteTy := ⟨⟨0⟩, []⟩
  let op : MIRInst (sliceLoweringLanguage slice0) :=
    .op (opcode := .arithmetic (.plain .add)) (types := [byteTy, byteTy]) dst ()
      (.cons (.literal ⟨[255]⟩) (.cons (.literal ⟨[1]⟩) .nil)) [⟨0, .overflow, .residual⟩]
  let block : Block _ _ _ := ⟨⟨0⟩, [], [⟨plusOrigin, op⟩], .ret (some ⟨byteTy, .copy dst⟩), [returnOrigin]⟩
  ⟨overflowProject, earlyEntry, [(earlyEntry, ⟨[], some byteTy⟩)],
    [(earlyEntry, [(⟨0⟩, byteTy)], ⟨⟨0⟩, #[block]⟩)], [], []⟩

private abbrev overflowAnswer : Concrete.Contracts.Observation :=
  .trapped 134 .overflow ⟨⟨0⟩, ⟨"main.con", 1, 30⟩⟩
    "concrete: trap: overflow at main.con:1:30 (site 0)\n".toUTF8

private def addOp : CoreOperation (sliceLanguage slice0).operands :=
  .expression (.inl (.add .u8 (.literal (.int .u8 255)) (.literal (.int .u8 1))))
private def retOp : CoreOperation (sliceLanguage slice0).operands := .expression (.inl (.read (.copy ⟨0⟩)))

private theorem overflowWF : overflowCore.WF := by
  have plusBinds : ∃ span, plusOrigin.lexical.Binds overflowProject span :=
    ⟨⟨⟨0⟩, 29, 30, by decide, 1, 30, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩
  have retBinds : ∃ span, returnOrigin.lexical.Binds overflowProject span :=
    ⟨⟨⟨0⟩, 18, 33, by decide, 1, 19, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩
  refine ⟨⟨by decide, by decide, by decide, ?_, ?_, ?_⟩, ?_, ?_⟩
  · intro site op member
    change (site, op) ∈ [(⟨0⟩, addOp), (⟨1⟩, retOp)] at member
    rcases List.mem_cons.mp member with h | h
    · cases h; exact ⟨plusOrigin, rfl⟩
    · cases List.mem_singleton.mp h; exact ⟨returnOrigin, rfl⟩
  · intro state events kind site path trapped
    have peel {p : Exec.Config overflowMachine.machine}
        (tick : Exec.step overflowMachine.body p = some (.next (nextConfig p))) (active : p.final? = none)
        {ev} (path : Exec.Steps overflowMachine.body p ev state) :
        ∃ ev', Exec.Steps overflowMachine.body (nextConfig p) ev' state := by
      cases path with
      | refl => rw [active] at trapped; contradiction
      | step step rest =>
        cases step with
        | silent h => rw [tick] at h; cases Option.some.inj h; exact ⟨_, rest⟩
        | prim h => rw [tick] at h; contradiction
    let p := overflowMachine.initial
    obtain ⟨_, p1⟩ := peel (p := p) rfl rfl path
    obtain ⟨_, p2⟩ := peel (p := nextConfig p) rfl rfl p1
    have endStep : Exec.step overflowMachine.body (nextConfig (nextConfig p)) = none := by with_unfolding_all rfl
    cases p2 with
    | refl =>
      change some (.trapped .overflow ⟨0⟩) = some (Exec.Final.trapped kind site) at trapped
      cases trapped
      exact ⟨_, rfl⟩
    | step tick _ => cases tick <;> rename_i h _ <;> first | contradiction | (simp_all)
  · intro site org mem
    change (site, org) ∈ [(⟨0⟩, plusOrigin), (⟨1⟩, returnOrigin)] at mem
    rcases List.mem_cons.mp mem with eq | mem
    · cases eq; exact ⟨rfl, plusBinds⟩
    · cases List.mem_singleton.mp mem; exact ⟨rfl, retBinds⟩
  · intro site op mem
    change (site, op) ∈ [(⟨0⟩, addOp), (⟨1⟩, retOp)] at mem
    rcases List.mem_cons.mp mem with eq | mem
    · cases eq; trivial
    · cases List.mem_singleton.mp mem; trivial
  · intro span mem
    change span ∈ [plus, plus, ⟨25, 28, 1, 26, ⟨0⟩⟩, ⟨31, 32, 1, 32, ⟨0⟩⟩] at mem
    simp only [List.mem_cons, List.not_mem_nil, or_false] at mem
    rcases mem with rfl | rfl | rfl | rfl
    · exact plusBinds
    · exact plusBinds
    · exact ⟨⟨⟨0⟩, 25, 28, by decide, 1, 26, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩
    · exact ⟨⟨⟨0⟩, 31, 32, by decide, 1, 32, by decide, by decide⟩, _, rfl, rfl, by decide, rfl, rfl, rfl, rfl⟩

private theorem overflowHandoff : overflowMIR.logicalView.HandoffWF := by
  refine ⟨?_, List.mem_singleton_self _, ?_⟩
  · with_unfolding_all
      refine ⟨.cons nofun .nil, .cons nofun .nil, .cons nofun .nil, .nil, ?_, nofun, ?_⟩
      · intro fn classes member
        cases List.mem_singleton.mp member
        exact .cons nofun .nil
      · intro fn locals graph member
        cases List.mem_singleton.mp member
        refine ⟨.cons nofun .nil, ⟨.cons nofun .nil, ⟨_, List.mem_singleton_self _, rfl⟩, ?_⟩,
          ⟨_, rfl, ⟨_, List.mem_singleton_self _, rfl, rfl⟩, ?_⟩⟩
        · intro block member edge follows
          cases List.mem_singleton.mp member
          cases follows
        · intro block member
          cases List.mem_singleton.mp member
          refine ⟨rfl, ?_⟩
          intro op member
          cases List.mem_singleton.mp member
          trivial
  · intro org member
    have origins : org ∈ [plusOrigin, returnOrigin] := by with_unfolding_all exact member
    rcases List.mem_cons.mp origins with rfl | last
    · exact (overflowWF.1.2.2.2.2.2 _ _ (.head _)).2
    · cases List.mem_singleton.mp last
      exact (overflowWF.1.2.2.2.2.2 _ _ (.tail _ (.head _))).2

private theorem overflowConsumes : ConsumesEvidence overflowCore [] overflowMIR := by
  refine ⟨by decide, ?_, ?_, ?_, overflowWF.2.1⟩
  · rintro site opcode conditions ⟨op, mem, _, rfl, rfl⟩ nonempty
    change (site, op) ∈ [(⟨0⟩, addOp), (⟨1⟩, retOp)] at mem
    rcases List.mem_cons.mp mem with eq | mem
    · cases eq; exact ⟨_, List.mem_singleton_self _, rfl, rfl⟩
    · cases List.mem_singleton.mp mem; exact (nonempty rfl).elim
  · intro op member
    cases List.mem_singleton.mp member
    refine ⟨_, ⟨addOp, .head _, trivial, rfl, rfl⟩, by decide, rfl, ?_, ?_⟩
    · exact fun _ => nofun
    · intro check mem
      cases List.mem_singleton.mp mem
      exact nofun
  · rintro _ _ ⟨_, h, _⟩; cases h

private abbrev overflowSem := MIR.Execution (language := sliceLanguage slice0)
  (lang := sliceLoweringLanguage slice0) (profile := profile)

private theorem overflowLowers : LowersUnchecked overflowCore overflowMIR := by
  have eligible := (ConcreteSpec.Admission.checkStatic_iff overflowWF).1 (by rfl)
  refine ⟨eligible, rfl, rfl, ⟨fun _ h => h, fun _ h => h⟩, overflowConsumes, ⟨⟨?_, nofun⟩, rfl⟩, ?_⟩
  · intro fn ls graph member root ty found
    cases List.mem_singleton.mp member
    cases List.mem_singleton.mp found
    rfl
  · intro _
    let S := CoreInput.Execution (language := sliceLanguage slice0)
    let finish := nextConfig (nextConfig overflowMachine.initial)
    have path : Concrete.Contracts.Steps (fun p q => Exec.step overflowMachine.body p = some (.next q))
        overflowMachine.initial finish := .cons rfl (.cons rfl (.nil _))
    have actual := CoreInput.safe_of_silent_path (core := overflowCore) (q := finish)
      (f := .trapped .overflow ⟨0⟩) (hosted := .trapped 134 .overflow ⟨0⟩) (result := overflowAnswer)
      path rfl rfl rfl
    obtain ⟨p, finite, result⟩ := (actual.2 _).2 rfl
    obtain ⟨entry, initial⟩ := overflowSem.initialized overflowMIR
    have observed : overflowSem.result overflowMIR entry overflowAnswer := by
      cases initial; with_unfolding_all rfl
    have here {q} (h : overflowSem.finite overflowMIR q) : q = entry := by
      obtain ⟨start, starts, reach⟩ := h
      have eq : start = entry := starts.trans initial.symm
      rw [eq] at reach
      cases reach with
      | nil => rfl
      | cons step _ => exact (overflowSem.terminal _ _ _ observed _ step).elim
    refine ⟨?_, ?_, ?_⟩
    · intro q o reached value
      obtain rfl := here reached
      cases Option.some.inj (value.symm.trans observed)
      refine ⟨p, finite, result, ?_⟩
      change p.2.map _ ++ (Option.map _ _).getD [] = _
      rw [show _ = some overflowAnswer from result]
      have silent : p.2 = [] := by cases p.2 with | nil => rfl | cons e _ => exact nomatch e
      rw [silent]
      cases initial; with_unfolding_all rfl
    · intro run inf
      exact (overflowSem.terminal _ _ _ observed _ ((inf.1.trans initial.symm) ▸ inf.2 0)).elim
    · intro q stuck
      obtain rfl := here stuck.1
      exact stuck.2.2 _ observed

example :
    let r := registry {fragmentPasses slice0 with lowerUnchecked := fun _ => .ok overflowMIR}
    ConcreteSpec.checkProgram overflowView.program = .error (.atPosition 0 .additionOverflowUnproven) ∧
    ConcreteSpec.checkProgramWith .runtime overflowView.program = .ok [] ∧
    r.typing.run overflowCore = .ok overflowCore ∧ r.lowerUnchecked.run overflowCore = .ok overflowMIR ∧
    r.lowerUnchecked.outputInvariant overflowMIR ∧
    r.lowerUnchecked.success overflowCore overflowMIR ∧ overflowSem.observes overflowMIR overflowAnswer := by
  exact ⟨rfl, rfl, rfl, rfl, overflowHandoff, overflowLowers, _, ⟨_, rfl, .nil _⟩, by with_unfolding_all rfl⟩

end Concrete.IntegratedCore
