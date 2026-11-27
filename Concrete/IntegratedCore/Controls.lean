import Concrete.IntegratedCore
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
  ⟨source.project, slice0, .straightLine (.zero ⟨[], .lit 42, ⟨25, 27, 1, 26, ⟨0⟩⟩⟩) rfl⟩

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
      .leaf ⟨⟨⟨0⟩, 3, 7, 1, 4⟩, .name "main"⟩, .node .body []])⟩).toOption.map AdmissionInput.prepare) =
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

-- The remaining edges refuse because this bundle exercises elaboration only.
private def elaborationPasses : Passes slice0 profile where
  lex input := .error (.missingFile input.entryFile)
  elaborate := elaborate slice0
  typeCore _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  lowerUnchecked _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  lowerMemory _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  buildSSA _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  buildBIR _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  emitLLVM _ := .error (.invalidInput ⟨``elaborate, 1⟩)
  emitLCB _ := .error (.invalidInput ⟨``elaborate, 1⟩)

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


-- A signed multi-byte literal distinguishes Core order from big-endian storage.
example :
    let bytes := ([254, 255] : List UInt8)
    (sliceLanguage slice0).literal (.inl (.scalar (.int .i16))) bytes = some (.scalar (.int .i16 (-2))) ∧
    Hybrid.decode {linuxX8664 with byteOrder := .big} (.int .i16)
      (bytes.map fun byte => .init byte.toFin none) = some (.int .i16 (-257)) :=
  ⟨rfl, rfl⟩

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
  change Except.ok (.zero ⟨[], .lit 42, useSpan⟩) = Except.ok v at actual
  cases Except.ok.inj actual
  cases checked

example :
    let unknown := sourceReturn (.local "absent" useSpan)
    let issue := (Surface0Issue.unknownLocal "absent" useSpan).render
    (registry elaborationPasses).admission.run unknown = .error (.refused issue) ∧
    (registry elaborationPasses).admission.refusal unknown (.refused issue) ∧
    (registry elaborationPasses).typing.run unknown = .error .unpreparable ∧
    (registry elaborationPasses).typing.refusal unknown .unpreparable :=
  ⟨rfl, rfl, rfl, rfl⟩

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

example :
    let big := {linuxX8664 with byteOrder := .big}
    let ty : (sliceLanguage slice0).operands.Ty := .inl (.scalar (.int .i16))
    (sliceLanguage slice0).layout [] big ty = some ⟨2, 2⟩ ∧
    (sliceLanguage slice0).literal ty [254, 255] = some (.scalar (.int .i16 (-2))) ∧
    Hybrid.decode big (.int .i16) [.init 254 none, .init 255 none] = some (.int .i16 (-257)) :=
  ⟨rfl, rfl, rfl⟩

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
    ¬ wrong.InstructionStep started constructed false
      (.op (opcode := .named ⟨"constructEmpty"⟩) root () .nil []) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨_, _, _, _, _, _, _, reads, _, addresses, operation, _⟩
  cases Option.some.inj reads
  cases Option.some.inj addresses
  cases operation.2.1

end Concrete.IntegratedCore
