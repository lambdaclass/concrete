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

-- Keep the real elaborator and checker, with a type pass that always succeeds.
private def fragmentPasses (slice : Slice) : Passes slice profile where
  lex input := .error (.missingFile input.entryFile)
  elaborate := elaborate slice
  typeCore _ := .ok ()
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
  ⟨.bind (.name ⟨0⟩) (.nominal tokenName) (.construct tokenName), useSpan, binderSpan⟩
private def takeToken : LocatedStraightBinding :=
  ⟨.destructure tokenName (.move ⟨0⟩), useSpan, useSpan⟩
private def tokenView (tail : List LocatedStraightBinding) : AdmissionView :=
  .one ⟨[(tokenName, binderSpan)], introToken :: tail, .ret (.lit 42), useSpan⟩
private def tokenInput (tail : List LocatedStraightBinding) : CoreInput (sliceLanguage sliceOne) :=
  ⟨source.project, sliceOne, .straightLine (tokenView tail) rfl⟩

-- Normal execution constructs and consumes the logical owner. A second
-- destructure fails in both the checker and the instrumented machine.
private def runTokens (tail : List LocatedStraightBinding) :=
  let payload := (tokenInput tail).machineProgram
  Exec.run payload.body (fun _ p => nomatch p) 32 payload.initial

example : (ConcreteSpec.Admission.admitInput (tokenInput [takeToken])).isOk = true ∧
    (runTokens [takeToken]).ending = .final (.returned (.scalar (.int .u8 42))) ∧
    ConcreteSpec.checkProgram (tokenView [takeToken, takeToken]).program =
      .error (.atPosition 2 (.movedBinding ⟨0⟩)) ∧
    (runTokens [takeToken, takeToken]).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl, rfl, rfl⟩

-- The terminator and the variable's declaration have distinct coordinates.
example :
    let why := ConcreteSpec.Admission.Refusal.atPosition 1 (.liveLinearBinding ⟨0⟩)
    ConcreteSpec.checkProgram (tokenView []).program = .error why ∧
    (tokenView []).render why = some ⟨⟨``ConcreteSpec.ProgramPermitted, 1⟩,
      surfaceFileSpan useSpan, [(⟨"binding"⟩, surfaceFileSpan binderSpan)], none, .core why⟩ :=
  ⟨rfl, rfl⟩

example :
    let later : Slice := ⟨⟨2, by decide⟩⟩
    ({tokenInput [] with slice := later}).decode = .error (.unsupportedSlice later) := rfl

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
    declarations := (tokenInput []).machineProgram.declarations }

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
      ⟨.bind .wild .u8 (.scalar (.atom (.lit 42))), binderSpan, binderSpan⟩
    let view := AdmissionView.one ⟨[], [wildcard], .ret (.var ⟨0⟩), useSpan⟩
    let machine := view.execution
    ConcreteSpec.run view.program 8 = .stuck .unbound ∧
    (Exec.run machine.body (fun _ prim => nomatch prim) 32 machine.initial).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl⟩

-- A later RHS can also read an unbound ID. The terminator here contains no variables.
example :
    let wildcard : LocatedStraightBinding :=
      ⟨.bind .wild .u8 (.scalar (.atom (.lit 42))), binderSpan, binderSpan⟩
    let useMissing : LocatedStraightBinding :=
      ⟨.bind .wild .u8 (.scalar (.add (.lit 0) (.var ⟨0⟩))), useSpan, useSpan⟩
    let view := AdmissionView.one ⟨[], [wildcard, useMissing], .ret (.lit 42), useSpan⟩
    let machine := view.execution
    ConcreteSpec.run view.program 8 = .stuck .unbound ∧
    (Exec.run machine.body (fun _ prim => nomatch prim) 32 machine.initial).ending = .stuck := by
  with_unfolding_all exact ⟨rfl, rfl⟩

private def parseSource (text : String) := (Lex.lex text.toUTF8).bind Parse.parse

example :
    ((parseSource r"fn main() -> u8 { return 42; let _: u8 = 1; }").bind
      (fun ast => Admission.prepareParsed ast)) =
      .error (.checker "unreachable code (STMT-14) at 1:30, bytes 29..43") := by
  with_unfolding_all rfl

private def pendingSource (text : String) : Option (AdmissionInput (sliceLanguage slice0)) := do
  let ast ← (parseSource text).toOption
  return .pending (Project.singleton mainSourcePath (SourceText.ofBytes text.toUTF8.data.toList))
    (.zero ast) rfl

-- A source spelling erased by resolution cannot affect the unchecked Core type pass.
example :
    let parsed := pendingSource r"fn main() -> u8 { let main: u8 = 42; return main; }"
    let r := registry elaborationPasses
    (parsed.bind fun input => (r.eligibility.run input).toOption.bind (fun i =>
      (r.typing.run i).toOption.map (fun out => out.decode.toOption.map AdmissionView.program))) =
      some (some (ConcreteSpec.Slice0.toCore ⟨[⟨⟨0⟩, .atom (.lit 42)⟩], .var ⟨0⟩⟩)) ∧
    (parsed.map fun input => r.admission.run input) = some (.error (.refused
      (Surface0Issue.reservedBinder "main" ⟨22, 26, 1, 23, ⟨0⟩⟩).render)) := by
  with_unfolding_all exact ⟨rfl, rfl⟩

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

end Concrete.IntegratedCore
