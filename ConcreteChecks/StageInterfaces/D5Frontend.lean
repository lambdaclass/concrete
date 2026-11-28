import Concrete.Frontend.Validate
import Concrete.Parse
import Concrete.Interfaces.Pipeline
import Concrete.Lex
import Concrete.IntegratedCore
import Concrete.IntegratedCore.Agreement
import ConcreteSpec.StageGrammar
import ConcreteSpec.StageInterface
import ConcreteSpec.Admission.Early

/-! # D5 front-end witnesses and rejected wrong-compiler mutants (`ISS-0113`)

Standalone controls for the three verified front-end edges on the fixed Slice 0
and Slice 1 registry instances (`Lexer`, `Parser`, `Elaborator`):

1. Universal `Stage.Sound` and `(frontendPipeline passes).Sound` on fixed `slice0`
   and `sliceOne`.
2. Reachable whole-program witnesses exercising the exact registered stages
   (using `Concrete.Parse.KernelEval.sliceParser_run_eq_sparse_for` for fast kernel
   evaluation without unfolding `earlyProposal`), including:
   - `fn main() -> u8 { return (42); }` (grouped return through Lex → Parse → Elaborate
     → Core admission + `Exec.run` returning `42`);
   - `fn main() -> u8 { let x: u8 = 20; return x + 22; }` (multi-statement `let` +
     checked `u8` addition through Lex → Parse → Elaborate → Core admission + `Exec.run`
     returning `42`);
   - `fn main() -> u8 { }` (empty-body fallthrough passing Lex → Parse → Elaborate
     with `AdmissionInput.Ready slice0` and `prepare` refusing at `closeBrace` with `Returns@1`);
   - Stage-order refusal precedence witnesses:
     - Lex-before-parse (`fn main() -> u8 { return ; @ }`, `Lexed@2` at `@`);
     - Parse malformed (`fn main() -> u8 { return missing }`, `Parsed@1` at `}`);
     - Parse-before-checker (`fn main() -> u8 { let x: u8 = 256; return x }`, `Parsed@1` at `}`);
     - Parse unsupported Slice 1 production on `slice0`
       (`fn main() -> u8 { let Option {} = 1; return 0; }`, `AdmittedProduction@3` at `{`);
     - Elaborator refusal on fixed `sliceOne`
       (`fn main() -> u8 { let Option {} = 1; return 0; }` accepted by `sliceParser sliceOne`,
       refused by `elaborator` with `.unsupportedSlice sliceOne`).
3. Rejected wrong-compiler mutants on every verified front-end edge:
   - Lexer mutants: value corruption (`42 → 43`) preserving bytes/spans, dropped trailing
     trivia preserving `parserView`, and spurious refusal on valid source;
   - Parser mutants: under-budgeted engine (`stepsPerToken := 1`) exhaustion, AST-corrupting
     stage, spurious-refusal stage, and the load-bearing `TokensOrdered` counterexample
     (`swappedStream`: coordinate-valid `view.WF` stream with swapped `return`/`;` spans
     that `sliceParser slice0` accepts yet violates `Parsed.ElaborationDomain slice0`,
     proving `¬ parserWithoutOrder.Validated`);
   - Elaborator mutants: value corruption (`42 → 43`), spurious refusal on valid Slice 0
     AST, `closeBrace`-span corruption on empty body, and wrong refusal on fixed `sliceOne`.
-/

namespace ConcreteChecks.StageInterfaces.D5Frontend

open Concrete
open Concrete.Interfaces
open Concrete.Frontend.Validate
open Concrete.Parse.KernelEval
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open ConcreteSpec.StageGrammar
open ConcreteSpec.Surface0
open ConcreteSpec.GrammarData

/-! ## Section 1: Fixed Slice 0 and Slice 1 pass bundles and universal soundness -/

def d5Profile : Profile :=
  ⟨.lcb, .x86_64Linux, 8, 4096⟩

private def stubRule : RuleId :=
  ⟨`ConcreteSpec.Core.Slice0Rules.Parsed, 1⟩

def frontendPassesAt (slice : Slice) : Passes slice d5Profile where
  lex := Concrete.Lex.lexStream
  elaborate := Concrete.IntegratedCore.elaborate slice
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput stubRule)
  lowerUnchecked _ := .error (.invalidInput stubRule)
  lowerMemory _ := .error (.invalidInput stubRule)
  buildSSA _ := .error (.invalidInput stubRule)
  buildBIR _ := .error (.invalidInput stubRule)
  emit _ := .error (.invalidInput stubRule)

def frontendPasses : Passes slice0 d5Profile :=
  frontendPassesAt slice0

theorem d5_frontend_sound_slice0 :
    Stage.Sound frontendPasses .lexer ∧
    Stage.Sound frontendPasses .parser ∧
    Stage.Sound frontendPasses .elaborator ∧
    (frontendPipeline frontendPasses).Sound :=
  ⟨lexer_sound frontendPasses rfl,
   parser_sound frontendPasses,
   elaborator_sound frontendPasses rfl,
   frontendPipeline_sound frontendPasses rfl rfl⟩

theorem d5_frontend_sound_slice1 :
    Stage.Sound (frontendPassesAt sliceOne) .lexer ∧
    Stage.Sound (frontendPassesAt sliceOne) .parser ∧
    Stage.Sound (frontendPassesAt sliceOne) .elaborator ∧
    (frontendPipeline (frontendPassesAt sliceOne)).Sound :=
  ⟨lexer_sound (frontendPassesAt sliceOne) rfl,
   parser_sound (frontendPassesAt sliceOne),
   elaborator_sound (frontendPassesAt sliceOne) rfl,
   frontendPipeline_sound (frontendPassesAt sliceOne) rfl rfl⟩

/-! ## Section 2: Helpers for closed kernel witnesses -/

private def mkSource (slice : Slice) (bytes : List UInt8) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes bytes), ⟨0⟩, slice⟩

private def dummyStream (src : Source) : TokenStream :=
  ⟨src, #[], #[#[]]⟩

private def dummyAst (slice : Slice) (src : Source) :
    Parsed ((sliceGrammar slice).value (sliceGrammar slice).start) :=
  ⟨src, .inl (.node .program [])⟩

private def dummyCore (src : Source) : CoreInput (sliceLanguage slice0) :=
  ⟨src.project, slice0,
    .straightLine (.zero ⟨[], .lit 0, ⟨0, 0, 1, 1, ⟨0⟩⟩, ⟨0, 0, 1, 1, ⟨0⟩⟩⟩) rfl⟩

private theorem lexStream_eq_ok_of_isOk (src : Source)
    (h : (Concrete.Lex.lexStream src).isOk = true) :
    Concrete.Lex.lexStream src = .ok
      (match Concrete.Lex.lexStream src with | .ok s => s | .error _ => dummyStream src) := by
  cases hlex : Concrete.Lex.lexStream src with
  | error _ => rw [hlex] at h; cases h
  | ok s => rfl

private theorem sparse_eq_ok_of_isSome (slice : Slice) (stream : TokenStream)
    (fallback : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start))
    (h : ((runEarlySparse stream.parserView).2.bind (·.toOption)).isSome = true) :
    (runEarlySparse stream.parserView).2 = some (.ok
      (match (runEarlySparse stream.parserView).2 with
       | some (.ok ast) => ast
       | _ => fallback)) := by
  cases hp : (runEarlySparse stream.parserView).2 with
  | none => rw [hp] at h; cases h
  | some res =>
    cases res with
    | error _ => rw [hp] at h; cases h
    | ok ast => rfl

private theorem pipeline_run_eq {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) {src : Source} {stream : TokenStream}
    {ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)}
    {input : AdmissionInput (sliceLanguage slice)}
    (hLex : (registry passes).lexer.run src = .ok stream)
    (hParse : (registry passes).parser.run stream = .ok ast)
    (hElab : (registry passes).elaborator.run ast = .ok input) :
    (frontendPipeline passes).run src = .ok input := by
  unfold frontendPipeline Pipeline.run
  rw [hLex]
  dsimp only
  unfold Pipeline.run
  rw [hParse]
  dsimp only
  unfold Pipeline.run
  rw [hElab]
  rfl

private def execExitU8 (core : CoreInput (sliceLanguage slice0)) (fuel : Nat) : Option Int :=
  match (Exec.run core.machineProgram.body (fun _ _ _ _ _ => none) fuel core.machineProgram.initial).ending with
  | .final (.returned (.scalar (.int .u8 n))) => some n
  | _ => none

/-! ## Section 3: Reachable whole-program witnesses on fixed `slice0` and `sliceOne` -/

/-! ### Witness 1: `fn main() -> u8 { return (42); }` -/

def w1Source : Source :=
  mkSource slice0 "fn main() -> u8 { return (42); }".toUTF8.toList

def w1Stream : TokenStream :=
  match Concrete.Lex.lexStream w1Source with
  | .ok s => s
  | .error _ => dummyStream w1Source

def w1Ast : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
  match (runEarlySparse w1Stream.parserView).2 with
  | some (.ok ast) => ast
  | _ => dummyAst slice0 w1Source

def w1Input : AdmissionInput (sliceLanguage slice0) :=
  match Concrete.IntegratedCore.elaborate slice0 w1Ast with
  | .ok inp => inp
  | .error _ => .pending w1Source.project (.zero ⟨"main", ⟨0, 0, 1, 1, ⟨0⟩⟩, [], ⟨0, 0, 1, 1, ⟨0⟩⟩⟩) rfl

def w1Core : CoreInput (sliceLanguage slice0) :=
  match w1Input.prepare .admission with
  | .ok c => c
  | .error _ => dummyCore w1Source

def w1Discharges : Discharges (sliceLanguage slice0) :=
  match ConcreteSpec.Admission.checkInput w1Core with
  | .ok d => d
  | .error _ => []

theorem w1_ready : Source.WF w1Source ∧ w1Source.slice = slice0 :=
  ⟨rfl, rfl⟩

theorem w1_lex_ok : (registry frontendPasses).lexer.run w1Source = .ok w1Stream :=
  lexStream_eq_ok_of_isOk w1Source (by decide +kernel)

theorem w1_parse_sparse_ok : (runEarlySparse w1Stream.parserView).2 = some (.ok w1Ast) :=
  sparse_eq_ok_of_isSome slice0 w1Stream (dummyAst slice0 w1Source) (by decide +kernel)

theorem w1_parse_ok : (registry frontendPasses).parser.run w1Stream = .ok w1Ast :=
  parserEntry_run_of_sparse (slice := slice0) w1_parse_sparse_ok

theorem w1_elab_ok : (registry frontendPasses).elaborator.run w1Ast = .ok w1Input := by
  have h : (Concrete.IntegratedCore.elaborate slice0 w1Ast).isOk = true := by decide +kernel
  change Concrete.IntegratedCore.elaborate slice0 w1Ast = .ok w1Input
  unfold w1Input
  cases he : Concrete.IntegratedCore.elaborate slice0 w1Ast with
  | error _ => rw [he] at h; cases h
  | ok inp => rfl

theorem w1_pipeline_ok : (frontendPipeline frontendPasses).run w1Source = .ok w1Input :=
  pipeline_run_eq frontendPasses w1_lex_ok w1_parse_ok w1_elab_ok

theorem w1_pipeline_corresponds :
    (frontendPipeline frontendPasses).Corresponds w1Source w1Input :=
  frontendPipeline_corresponds_of_ok frontendPasses rfl rfl w1_ready w1_pipeline_ok

theorem w1_edge_contracts :
    Lexes w1Source w1Stream ∧
    w1Stream.parserView.WF ∧
    TokensOrdered w1Stream.parserView ∧
    ParsesWith (sliceParser slice0) w1Stream.parserView w1Ast ∧
    Parsed.ElaborationDomain slice0 w1Ast ∧
    w1Input.Ready slice0 := by
  have hLexOut := (lexer_validated frontendPasses rfl).1 w1Source w1Stream w1_ready w1_lex_ok
  have hParseOut := (parser_validated frontendPasses).1 w1Stream w1Ast hLexOut.1 w1_parse_ok
  have hElabOut := (elaborator_validated frontendPasses rfl).1 w1Ast w1Input hParseOut.1 w1_elab_ok
  exact ⟨hLexOut.2, hLexOut.1.1, hLexOut.1.2.1, hParseOut.2.2.1, hParseOut.1, hElabOut.1⟩

theorem w1_prepare_ok : w1Input.prepare .admission = .ok w1Core := by
  have h : (w1Input.prepare .admission).isOk = true := by decide +kernel
  unfold w1Core
  cases hp : w1Input.prepare .admission with
  | error _ => rw [hp] at h; cases h
  | ok c => rfl

theorem w1_checkInput_ok : ConcreteSpec.Admission.checkInput w1Core = .ok w1Discharges := by
  have h : (ConcreteSpec.Admission.checkInput w1Core).isOk = true := by decide +kernel
  unfold w1Discharges
  cases hc : ConcreteSpec.Admission.checkInput w1Core with
  | error _ => rw [hc] at h; cases h
  | ok d => rfl

theorem w1_core_admitted_and_exits_42 :
    w1Core.WF ∧
    w1Core.Accepted ∧
    ValidDischarges w1Core w1Discharges ∧
    execExitU8 w1Core 16 = some 42 := by
  obtain ⟨hwf, hfrag⟩ := w1_edge_contracts.2.2.2.2.2.prepared w1_prepare_ok
  have hadm := Concrete.IntegratedCore.Agreement.admissionSound w1Core hwf hfrag
  refine ⟨hwf, hadm.1.mpr ⟨w1Discharges, w1_checkInput_ok⟩,
    (hadm.2.1 w1Discharges w1_checkInput_ok).2, ?_⟩
  decide +kernel

/-! ### Witness 2: `fn main() -> u8 { let x: u8 = 20; return x + 22; }` -/

def w2Source : Source :=
  mkSource slice0 "fn main() -> u8 { let x: u8 = 20; return x + 22; }".toUTF8.toList

def w2Stream : TokenStream :=
  match Concrete.Lex.lexStream w2Source with
  | .ok s => s
  | .error _ => dummyStream w2Source

def w2Ast : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
  match (runEarlySparse w2Stream.parserView).2 with
  | some (.ok ast) => ast
  | _ => dummyAst slice0 w2Source

def w2Input : AdmissionInput (sliceLanguage slice0) :=
  match Concrete.IntegratedCore.elaborate slice0 w2Ast with
  | .ok inp => inp
  | .error _ => .pending w2Source.project (.zero ⟨"main", ⟨0, 0, 1, 1, ⟨0⟩⟩, [], ⟨0, 0, 1, 1, ⟨0⟩⟩⟩) rfl

def w2Core : CoreInput (sliceLanguage slice0) :=
  match w2Input.prepare .admission with
  | .ok c => c
  | .error _ => dummyCore w2Source

def w2Discharges : Discharges (sliceLanguage slice0) :=
  match ConcreteSpec.Admission.checkInput w2Core with
  | .ok d => d
  | .error _ => []

theorem w2_ready : Source.WF w2Source ∧ w2Source.slice = slice0 :=
  ⟨rfl, rfl⟩

theorem w2_lex_ok : (registry frontendPasses).lexer.run w2Source = .ok w2Stream :=
  lexStream_eq_ok_of_isOk w2Source (by decide +kernel)

theorem w2_parse_ok : (registry frontendPasses).parser.run w2Stream = .ok w2Ast :=
  parserEntry_run_of_sparse (slice := slice0)
    (sparse_eq_ok_of_isSome slice0 w2Stream (dummyAst slice0 w2Source) (by decide +kernel))

theorem w2_elab_ok : (registry frontendPasses).elaborator.run w2Ast = .ok w2Input := by
  have h : (Concrete.IntegratedCore.elaborate slice0 w2Ast).isOk = true := by decide +kernel
  change Concrete.IntegratedCore.elaborate slice0 w2Ast = .ok w2Input
  unfold w2Input
  cases he : Concrete.IntegratedCore.elaborate slice0 w2Ast with
  | error _ => rw [he] at h; cases h
  | ok inp => rfl

theorem w2_pipeline_corresponds_and_exits_42 :
    (frontendPipeline frontendPasses).Corresponds w2Source w2Input ∧
    w2Input.Ready slice0 ∧
    w2Core.WF ∧
    w2Core.Accepted ∧
    execExitU8 w2Core 32 = some 42 := by
  have hPipe : (frontendPipeline frontendPasses).run w2Source = .ok w2Input :=
    pipeline_run_eq frontendPasses w2_lex_ok w2_parse_ok w2_elab_ok
  have hCorr := frontendPipeline_corresponds_of_ok frontendPasses rfl rfl w2_ready hPipe
  have hLexOut := (lexer_validated frontendPasses rfl).1 w2Source w2Stream w2_ready w2_lex_ok
  have hParseOut := (parser_validated frontendPasses).1 w2Stream w2Ast hLexOut.1 w2_parse_ok
  have hElabOut := (elaborator_validated frontendPasses rfl).1 w2Ast w2Input hParseOut.1 w2_elab_ok
  have hPrep : w2Input.prepare .admission = .ok w2Core := by
    have h : (w2Input.prepare .admission).isOk = true := by decide +kernel
    unfold w2Core
    cases hp : w2Input.prepare .admission with
    | error _ => rw [hp] at h; cases h
    | ok c => rfl
  obtain ⟨hwf, hfrag⟩ := hElabOut.1.prepared hPrep
  have hChk : ConcreteSpec.Admission.checkInput w2Core = .ok w2Discharges := by
    have h : (ConcreteSpec.Admission.checkInput w2Core).isOk = true := by decide +kernel
    unfold w2Discharges
    cases hc : ConcreteSpec.Admission.checkInput w2Core with
    | error _ => rw [hc] at h; cases h
    | ok d => rfl
  have hadm := Concrete.IntegratedCore.Agreement.admissionSound w2Core hwf hfrag
  exact ⟨hCorr, hElabOut.1, hwf, hadm.1.mpr ⟨w2Discharges, hChk⟩, by decide +kernel⟩

/-! ### Witness 3: `fn main() -> u8 { }` (empty-body fallthrough at `closeBrace`) -/

def w6Source : Source :=
  mkSource slice0 "fn main() -> u8 { }".toUTF8.toList

def w6Stream : TokenStream :=
  match Concrete.Lex.lexStream w6Source with
  | .ok s => s
  | .error _ => dummyStream w6Source

def w6Ast : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
  match (runEarlySparse w6Stream.parserView).2 with
  | some (.ok ast) => ast
  | _ => dummyAst slice0 w6Source

def w6Input : AdmissionInput (sliceLanguage slice0) :=
  match Concrete.IntegratedCore.elaborate slice0 w6Ast with
  | .ok inp => inp
  | .error _ => .pending w6Source.project (.zero ⟨"main", ⟨0, 0, 1, 1, ⟨0⟩⟩, [], ⟨0, 0, 1, 1, ⟨0⟩⟩⟩) rfl

theorem w6_ready : Source.WF w6Source ∧ w6Source.slice = slice0 :=
  ⟨rfl, rfl⟩

theorem w6_lex_ok : (registry frontendPasses).lexer.run w6Source = .ok w6Stream :=
  lexStream_eq_ok_of_isOk w6Source (by decide +kernel)

theorem w6_parse_ok : (registry frontendPasses).parser.run w6Stream = .ok w6Ast :=
  parserEntry_run_of_sparse (slice := slice0)
    (sparse_eq_ok_of_isSome slice0 w6Stream (dummyAst slice0 w6Source) (by decide +kernel))

theorem w6_elab_ok : (registry frontendPasses).elaborator.run w6Ast = .ok w6Input := by
  have h : (Concrete.IntegratedCore.elaborate slice0 w6Ast).isOk = true := by decide +kernel
  change Concrete.IntegratedCore.elaborate slice0 w6Ast = .ok w6Input
  unfold w6Input
  cases he : Concrete.IntegratedCore.elaborate slice0 w6Ast with
  | error _ => rw [he] at h; cases h
  | ok inp => rfl

private def prepErrorCause (inp : AdmissionInput (sliceLanguage slice0)) : Option RefusalCause :=
  match inp.prepare .admission with
  | .error c => some c
  | .ok _ => none

theorem w6_empty_body_ready_and_refuses_at_closeBrace :
    (frontendPipeline frontendPasses).Corresponds w6Source w6Input ∧
    w6Input.Ready slice0 ∧
    prepErrorCause w6Input = some (.surface (.fallthrough ⟨18, 19, 1, 19, ⟨0⟩⟩)) := by
  have hPipe : (frontendPipeline frontendPasses).run w6Source = .ok w6Input :=
    pipeline_run_eq frontendPasses w6_lex_ok w6_parse_ok w6_elab_ok
  have hLexOut := (lexer_validated frontendPasses rfl).1 w6Source w6Stream w6_ready w6_lex_ok
  have hParseOut := (parser_validated frontendPasses).1 w6Stream w6Ast hLexOut.1 w6_parse_ok
  have hElabOut := (elaborator_validated frontendPasses rfl).1 w6Ast w6Input hParseOut.1 w6_elab_ok
  refine ⟨frontendPipeline_corresponds_of_ok frontendPasses rfl rfl w6_ready hPipe, hElabOut.1, ?_⟩
  decide +kernel

/-! ### Stage-order refusal precedence witnesses -/

/-- Stage-order witness (lex before parse): `return ;` is malformed syntax at offset 25,
but the lexer refuses `@` at `[27, 28)` with `Lexed@2` before the parser runs. -/
def w3Source : Source :=
  mkSource slice0 "fn main() -> u8 { return ; @ }".toUTF8.toList

theorem w3_lex_before_parse :
    ∃ cause : RefusalCause,
      (registry frontendPasses).lexer.run w3Source = .error (.source cause) ∧
      (registry frontendPasses).lexer.refusal w3Source (.source cause) ∧
      cause.rule = ⟨`ConcreteSpec.Core.Slice0Rules.Lexed, 2⟩ ∧
      cause.primary = ⟨⟨0⟩, 27, 28, 1, 28⟩ := by
  have h : (match Concrete.Lex.lexStream w3Source with
      | .error (.source c) =>
        decide (c.rule = ⟨`ConcreteSpec.Core.Slice0Rules.Lexed, 2⟩ ∧
          c.primary = ⟨⟨0⟩, 27, 28, 1, 28⟩)
      | _ => false) = true := by
    decide +kernel
  cases hlex : Concrete.Lex.lexStream w3Source with
  | ok _ => rw [hlex] at h; cases h
  | error e =>
    cases e with
    | missingFile _ => rw [hlex] at h; cases h
    | source cause =>
      rw [hlex] at h
      have hprops := of_decide_eq_true h
      have href := (lexer_validated frontendPasses rfl).2 w3Source (.source cause) ⟨rfl, rfl⟩ hlex
      exact ⟨cause, hlex, href, hprops.1, hprops.2⟩

private theorem sparse_eq_error_of_eq (stream : TokenStream) (expected : ParseRefusal)
    (h : (match (runEarlySparse stream.parserView).2 with
          | some (.error r) => decide (r = expected)
          | _ => false) = true) :
    (runEarlySparse stream.parserView).2 = some (.error expected) := by
  cases hp : (runEarlySparse stream.parserView).2 with
  | none => rw [hp] at h; cases h
  | some res =>
    cases res with
    | ok _ => rw [hp] at h; cases h
    | error r =>
      rw [hp] at h
      rw [of_decide_eq_true h]

/-- Parser malformed witness (`Parsed@1` at `}` `[33, 34)`): missing `;` after `return missing`. -/
def w4Source : Source :=
  mkSource slice0 "fn main() -> u8 { return missing }".toUTF8.toList

def w4Stream : TokenStream :=
  match Concrete.Lex.lexStream w4Source with
  | .ok s => s
  | .error _ => dummyStream w4Source

theorem w4_parse_malformed :
    let expected : ParseRefusal :=
      .malformed (sliceParser slice0).malformedRule .unexpectedToken ⟨⟨0⟩, 33, 34, 1, 34⟩
    (registry frontendPasses).lexer.run w4Source = .ok w4Stream ∧
    (registry frontendPasses).parser.run w4Stream = .error expected ∧
    (registry frontendPasses).parser.refusal w4Stream expected := by
  have hLex : (registry frontendPasses).lexer.run w4Source = .ok w4Stream :=
    lexStream_eq_ok_of_isOk w4Source (by decide +kernel)
  have hLexOut := (lexer_validated frontendPasses rfl).1 w4Source w4Stream ⟨rfl, rfl⟩ hLex
  let expected : ParseRefusal :=
    .malformed (sliceParser slice0).malformedRule .unexpectedToken ⟨⟨0⟩, 33, 34, 1, 34⟩
  have hSp : (runEarlySparse w4Stream.parserView).2 = some (.error expected) :=
    sparse_eq_error_of_eq w4Stream expected (by decide +kernel)
  have hParse : (registry frontendPasses).parser.run w4Stream = .error expected :=
    parserEntry_run_of_sparse (slice := slice0) hSp
  exact ⟨hLex, hParse, (parser_validated frontendPasses).2 w4Stream expected hLexOut.1 hParse⟩

/-- Stage-order witness (parse before checker): `256` overflows `u8`, yet missing `;`
before `}` at `[44, 45)` is refused by the parser with `Parsed@1` before checker admission. -/
def w8Source : Source :=
  mkSource slice0 "fn main() -> u8 { let x: u8 = 256; return x }".toUTF8.toList

def w8Stream : TokenStream :=
  match Concrete.Lex.lexStream w8Source with
  | .ok s => s
  | .error _ => dummyStream w8Source

theorem w8_parse_before_checker :
    let expected : ParseRefusal :=
      .malformed (sliceParser slice0).malformedRule .unexpectedToken ⟨⟨0⟩, 44, 45, 1, 45⟩
    (registry frontendPasses).lexer.run w8Source = .ok w8Stream ∧
    (registry frontendPasses).parser.run w8Stream = .error expected ∧
    (registry frontendPasses).parser.refusal w8Stream expected := by
  have hLex : (registry frontendPasses).lexer.run w8Source = .ok w8Stream :=
    lexStream_eq_ok_of_isOk w8Source (by decide +kernel)
  have hLexOut := (lexer_validated frontendPasses rfl).1 w8Source w8Stream ⟨rfl, rfl⟩ hLex
  let expected : ParseRefusal :=
    .malformed (sliceParser slice0).malformedRule .unexpectedToken ⟨⟨0⟩, 44, 45, 1, 45⟩
  have hSp : (runEarlySparse w8Stream.parserView).2 = some (.error expected) :=
    sparse_eq_error_of_eq w8Stream expected (by decide +kernel)
  have hParse : (registry frontendPasses).parser.run w8Stream = .error expected :=
    parserEntry_run_of_sparse (slice := slice0) hSp
  exact ⟨hLex, hParse, (parser_validated frontendPasses).2 w8Stream expected hLexOut.1 hParse⟩

/-- Parser unsupported-slice witness on fixed `slice0`: Slice 1 struct destructuring
is refused at `{` `[29, 30)` with `AdmittedProduction@3` and `required = sliceOne`. -/
def w5Source : Source :=
  mkSource slice0 "fn main() -> u8 { let Option {} = 1; return 0; }".toUTF8.toList

def w5Stream : TokenStream :=
  match Concrete.Lex.lexStream w5Source with
  | .ok s => s
  | .error _ => dummyStream w5Source

theorem w5_parse_unsupported_slice1 :
    let expected : ParseRefusal :=
      .unsupported (sliceParser slice0).unsupportedProductionRule
        (.production .destructure) ⟨⟨0⟩, 29, 30, 1, 30⟩ sliceOne
    (registry frontendPasses).lexer.run w5Source = .ok w5Stream ∧
    (registry frontendPasses).parser.run w5Stream = .error expected ∧
    (registry frontendPasses).parser.refusal w5Stream expected := by
  have hLex : (registry frontendPasses).lexer.run w5Source = .ok w5Stream :=
    lexStream_eq_ok_of_isOk w5Source (by decide +kernel)
  have hLexOut := (lexer_validated frontendPasses rfl).1 w5Source w5Stream ⟨rfl, rfl⟩ hLex
  let expected : ParseRefusal :=
    .unsupported (sliceParser slice0).unsupportedProductionRule
      (.production .destructure) ⟨⟨0⟩, 29, 30, 1, 30⟩ sliceOne
  have hSp : (runEarlySparse w5Stream.parserView).2 = some (.error expected) :=
    sparse_eq_error_of_eq w5Stream expected (by decide +kernel)
  have hParse : (registry frontendPasses).parser.run w5Stream = .error expected :=
    parserEntry_run_of_sparse (slice := slice0) hSp
  exact ⟨hLex, hParse, (parser_validated frontendPasses).2 w5Stream expected hLexOut.1 hParse⟩

/-- Fixed `sliceOne` witness: the exact same struct-destructuring program is accepted by
`sliceParser sliceOne`, enters `Parsed.ElaborationDomain sliceOne`, and is explicitly
refused at the registered `elaborator` edge with `.unsupportedSlice sliceOne`. -/
def w7Source : Source :=
  mkSource sliceOne "fn main() -> u8 { let Option {} = 1; return 0; }".toUTF8.toList

def w7Stream : TokenStream :=
  match Concrete.Lex.lexStream w7Source with
  | .ok s => s
  | .error _ => dummyStream w7Source

def w7Ast : Parsed ((sliceGrammar sliceOne).value (sliceGrammar sliceOne).start) :=
  match (runEarlySparse w7Stream.parserView).2 with
  | some (.ok ast) => ast
  | _ => dummyAst sliceOne w7Source

theorem w7_slice1_parse_ok_and_elab_refuses :
    let passes1 := frontendPassesAt sliceOne
    (registry passes1).lexer.run w7Source = .ok w7Stream ∧
    (registry passes1).parser.run w7Stream = .ok w7Ast ∧
    Parsed.ElaborationDomain sliceOne w7Ast ∧
    (registry passes1).elaborator.run w7Ast = .error (.unsupportedSlice sliceOne) ∧
    (registry passes1).elaborator.refusal w7Ast (.unsupportedSlice sliceOne) := by
  let passes1 := frontendPassesAt sliceOne
  have hReady : Source.WF w7Source ∧ w7Source.slice = sliceOne := ⟨rfl, rfl⟩
  have hLex : (registry passes1).lexer.run w7Source = .ok w7Stream :=
    lexStream_eq_ok_of_isOk w7Source (by decide +kernel)
  have hLexOut := (lexer_validated passes1 rfl).1 w7Source w7Stream hReady hLex
  have hSp : (runEarlySparse w7Stream.parserView).2 = some (.ok w7Ast) :=
    sparse_eq_ok_of_isSome sliceOne w7Stream (dummyAst sliceOne w7Source) (by decide +kernel)
  have hParse : (registry passes1).parser.run w7Stream = .ok w7Ast :=
    parserEntry_run_of_sparse (slice := sliceOne) hSp
  have hParseOut := (parser_validated passes1).1 w7Stream w7Ast hLexOut.1 hParse
  have hElab : (registry passes1).elaborator.run w7Ast = .error (.unsupportedSlice sliceOne) := by
    have hne : sliceOne ≠ slice0 := by decide
    have hcorr : SliceElaboration sliceOne w7Ast.source.project
        (cast (sliceGrammarStartShape sliceOne) w7Ast.tree)
        (Concrete.IntegratedCore.elaborate sliceOne w7Ast) := by
      cases hrun : Concrete.IntegratedCore.elaborate sliceOne w7Ast with
      | ok inp => exact ((elaborator_validated passes1 rfl).1 w7Ast inp hParseOut.1 hrun).2
      | error err => exact (elaborator_validated passes1 rfl).2 w7Ast err hParseOut.1 hrun
    unfold SliceElaboration at hcorr
    rw [dite_eq_right hne] at hcorr
    exact hcorr
  have hElabRef := (elaborator_validated passes1 rfl).2 w7Ast (.unsupportedSlice sliceOne) hParseOut.1 hElab
  exact ⟨hLex, hParse, hParseOut.1, hElab, hElabRef⟩

/-! ## Section 4: Rejected wrong-compiler mutants on every verified front-end edge (`ISS-0113`) -/

/-! ### Lexer mutants on fixed `slice0` -/

/-- Mutant L1: corrupts the decoded value of token `42` to `43` while keeping identical
raw bytes, spans, `reconstruct`, and `parserView.WF`. -/
def w1CorruptValueStream : TokenStream :=
  { w1Stream with
    tokens := w1Stream.tokens.map fun tok =>
      match tok.decoded.value with
      | .integer 42 r s => ⟨⟨tok.decoded.span, .integer 43 r s⟩, tok.spelling⟩
      | _ => tok }

def mutantL1 : Passes slice0 d5Profile :=
  { frontendPasses with lex := fun _ => .ok w1CorruptValueStream }

theorem mutantL1_preserves_reconstruct_yet_rejected :
    (reconstruct w1CorruptValueStream.source.entryFile w1CorruptValueStream.tokens w1CorruptValueStream.trivia).map (·.data.toList) =
      (reconstruct w1Stream.source.entryFile w1Stream.tokens w1Stream.trivia).map (·.data.toList) ∧
    w1CorruptValueStream.parserView.WF ∧
    ¬ Stage.Sound mutantL1 .lexer := by
  refine ⟨by decide +kernel, by change w1CorruptValueStream.parserView.invalidLocation = none; decide +kernel, ?_⟩
  intro hSound
  have hLexes := (hSound.1.1 w1Source w1CorruptValueStream w1_ready rfl).2
  have hGoodLexes := w1_edge_contracts.1
  obtain ⟨_, _, f1, hlk1, b1, hrec1, hb1, r1, hsc1, hp1⟩ := hLexes
  obtain ⟨_, _, f2, hlk2, b2, hrec2, hb2, r2, hsc2, hp2⟩ := hGoodLexes
  have hf : f1 = f2 := Option.some.inj (hlk1.symm.trans hlk2)
  subst hf
  have hbytes : b1 = b2 := by
    cases b1; cases b2
    exact congrArg ByteArray.mk (Array.ext' (hb1.trans hb2.symm))
  subst hbytes
  have href : r1 = r2 := Except.ok.inj (hsc1.symm.trans hsc2)
  subst href
  let extractInts : List (FileSpan × Option ConcreteSpec.StageInterface.TokenValue) → List Nat :=
    List.filterMap fun p => match p.2 with | some (.integer n _ _) => some n | _ => none
  have hIntsEq := congrArg extractInts (Option.some.inj (hp1.symm.trans hp2))
  exact absurd hIntsEq (by decide +kernel)

/-- Mutant L2: drops trailing EOF trivia on padded source, keeping `parserView` identical
yet failing `reconstruct` byte-exactness under `Lexes`. -/
def w1PaddedSource : Source :=
  mkSource slice0 "fn main() -> u8 { return (42); } ".toUTF8.toList

def w1PaddedStream : TokenStream :=
  match Concrete.Lex.lexStream w1PaddedSource with
  | .ok s => s
  | .error _ => dummyStream w1PaddedSource

def w1DroppedTriviaStream : TokenStream :=
  { w1PaddedStream with trivia := w1PaddedStream.trivia.setIfInBounds (w1PaddedStream.trivia.size - 1) #[] }

def mutantL2 : Passes slice0 d5Profile :=
  { frontendPasses with lex := fun _ => .ok w1DroppedTriviaStream }

theorem mutantL2_preserves_parserView_yet_rejected :
    w1DroppedTriviaStream.parserView = w1PaddedStream.parserView ∧
    ¬ Stage.Sound mutantL2 .lexer := by
  refine ⟨rfl, ?_⟩
  intro hSound
  have hReady : Source.WF w1PaddedSource ∧ w1PaddedSource.slice = slice0 := ⟨rfl, rfl⟩
  have hGoodRun : (registry frontendPasses).lexer.run w1PaddedSource = .ok w1PaddedStream :=
    lexStream_eq_ok_of_isOk w1PaddedSource (by decide +kernel)
  have hGoodLexes := ((lexer_validated frontendPasses rfl).1 w1PaddedSource w1PaddedStream hReady hGoodRun).2
  have hBadLexes := (hSound.1.1 w1PaddedSource w1DroppedTriviaStream hReady rfl).2
  obtain ⟨_, _, f1, hlk1, b1, hrec1, hb1, _⟩ := hBadLexes
  obtain ⟨_, _, f2, hlk2, b2, hrec2, hb2, _⟩ := hGoodLexes
  have hf : f1 = f2 := Option.some.inj (hlk1.symm.trans hlk2)
  subst hf
  have hbytes : b1 = b2 := by
    cases b1; cases b2
    exact congrArg ByteArray.mk (Array.ext' (hb1.trans hb2.symm))
  subst hbytes
  have hrecEq : (reconstruct w1PaddedSource.entryFile
        w1DroppedTriviaStream.tokens w1DroppedTriviaStream.trivia).map (·.data.toList) =
      (reconstruct w1PaddedSource.entryFile
        w1PaddedStream.tokens w1PaddedStream.trivia).map (·.data.toList) :=
    congrArg (Option.map (·.data.toList)) (hrec1.trans hrec2.symm)
  have hrecNe : (reconstruct w1PaddedSource.entryFile
        w1DroppedTriviaStream.tokens w1DroppedTriviaStream.trivia).map (·.data.toList) ≠
      (reconstruct w1PaddedSource.entryFile
        w1PaddedStream.tokens w1PaddedStream.trivia).map (·.data.toList) := by
    decide +kernel
  exact hrecNe hrecEq

/-- Mutant L3: spuriously refuses valid `w1Source`. -/
def mutantL3 : Passes slice0 d5Profile :=
  { frontendPasses with lex := fun _ => .error (.missingFile ⟨0⟩) }

theorem mutantL3_spurious_refusal_rejected :
    ¬ Stage.Sound mutantL3 .lexer := by
  intro hSound
  have hRef : LexRefuses w1Source (.missingFile ⟨0⟩) :=
    hSound.1.2 w1Source (.missingFile ⟨0⟩) w1_ready rfl
  exact hRef

/-! ### Parser mutants on fixed `slice0` -/

/-- Mutant P1: under-budgeted parser engine (`stepsPerToken := 1`) exhausts fuel (`none`)
on reachable `w1Stream`, failing the totality/budget requirement of `parserEntry`. -/
theorem mutantP1_under_budget_exhausts :
    ¬ ∀ input : ParserView,
      (ParserEngine.run (g := earlySharedGrammar)
        { sliceParser slice0 with stepsPerToken := 1 } input).2 ≠ none := by
  intro hall
  have hEq := parserEngine_run_eq_sparse_with
    { sliceParser slice0 with stepsPerToken := 1 } 1
    sliceParser_table_lookup_eq rfl rfl rfl rfl w1Stream.parserView
  have hNone : (runEarlySparseWith 1 w1Stream.parserView).2.isNone = true := by decide +kernel
  cases hsp : (runEarlySparseWith 1 w1Stream.parserView).2 with
  | some _ => rw [hsp] at hNone; cases hNone
  | none =>
    exact hall w1Stream.parserView ((congrArg Prod.snd hEq).trans hsp)

/-- Mutant P2: AST-corrupting parser stage returning `43`'s AST on `w1Stream` (`return (42);`)
against the exact registered parser contract. -/
def w1WrongSource : Source :=
  mkSource slice0 "fn main() -> u8 { return (43); }".toUTF8.toList

def w1WrongStream : TokenStream :=
  match Concrete.Lex.lexStream w1WrongSource with
  | .ok s => s
  | .error _ => dummyStream w1WrongSource

def w1WrongAst : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
  match (runEarlySparse w1WrongStream.parserView).2 with
  | some (.ok ast) => ⟨w1Source, ast.tree⟩
  | _ => dummyAst slice0 w1Source

def mutantP2Stage :
    Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start)) :=
  { (registry frontendPasses).parser with run := fun _ => .ok w1WrongAst }

private def astReturnExitU8 (ast : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start)) :
    Option Int :=
  match Concrete.IntegratedCore.elaborate slice0 ast with
  | .ok inp =>
    match inp.prepare .admission with
    | .ok core => execExitU8 core 16
    | .error _ => none
  | .error _ => none

theorem mutantP2_corrupt_ast_rejected : ¬ mutantP2Stage.Checks := by
  intro hChecks
  have hIn : mutantP2Stage.inputInvariant w1Stream :=
    ⟨w1_edge_contracts.2.1, w1_edge_contracts.2.2.1, w1_edge_contracts.1.1 ▸ rfl⟩
  have hOut := (hChecks.1.1 w1Stream w1WrongAst hIn rfl).2.1
  have hGood : ((sliceParser slice0).run w1Stream.parserView).2 = some (.ok w1Ast) :=
    (congrArg Prod.snd (sliceParser_run_eq_sparse_for slice0 w1Stream.parserView)).trans w1_parse_sparse_ok
  have hAstEq : w1Ast = w1WrongAst := Except.ok.inj (Option.some.inj (hGood.symm.trans hOut))
  have hValNe : astReturnExitU8 w1Ast ≠ astReturnExitU8 w1WrongAst := by decide +kernel
  exact hValNe (congrArg astReturnExitU8 hAstEq)

/-- Mutant P3: spurious-refusal parser stage refusing valid `w1Stream` against the exact
registered parser contract. -/
def mutantP3Stage :
    Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start)) :=
  { (registry frontendPasses).parser with
    run := fun _ => .error (.malformed (sliceParser slice0).malformedRule .unexpectedEnd ⟨⟨0⟩, 0, 0, 1, 1⟩) }

theorem mutantP3_spurious_refusal_rejected : ¬ mutantP3Stage.Checks := by
  intro hChecks
  have hIn : mutantP3Stage.inputInvariant w1Stream :=
    ⟨w1_edge_contracts.2.1, w1_edge_contracts.2.2.1, w1_edge_contracts.1.1 ▸ rfl⟩
  have hRef := (hChecks.1.2 w1Stream _ hIn rfl).1
  have hGood : ((sliceParser slice0).run w1Stream.parserView).2 = some (.ok w1Ast) :=
    (congrArg Prod.snd (sliceParser_run_eq_sparse_for slice0 w1Stream.parserView)).trans w1_parse_sparse_ok
  cases Option.some.inj (hGood.symm.trans hRef)

/-- Mutant P4 (load-bearing `TokensOrdered` counterexample): swapping the byte spans of
`return` (`[18, 24)`) and `;` (`[27, 28)`) in `fn main() -> u8 { return 42; }` preserves
per-token `ParserView.WF` and succeeds under `sliceParser slice0`, yet produces a reversed
`returnSpan` (`[27, 24)`) that fails `CoreInput.WF` and `Parsed.ElaborationDomain slice0`.
Consequently, dropping `TokensOrdered` from `registeredParserStage slice0` makes
`Validated` FALSE. -/
def swapBaseSource : Source :=
  mkSource slice0 "fn main() -> u8 { return 42; }".toUTF8.toList

def swapBaseStream : TokenStream :=
  match Concrete.Lex.lexStream swapBaseSource with
  | .ok s => s
  | .error _ => dummyStream swapBaseSource

def swappedStream : TokenStream :=
  match swapBaseStream.tokens[7]?, swapBaseStream.tokens[9]? with
  | some a, some b =>
    { swapBaseStream with
      tokens := (swapBaseStream.tokens.setIfInBounds 7 ⟨⟨b.decoded.span, a.decoded.value⟩, a.spelling⟩)
        |>.setIfInBounds 9 ⟨⟨a.decoded.span, b.decoded.value⟩, b.spelling⟩ }
  | _, _ => swapBaseStream

def swappedAst : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
  match (runEarlySparse swappedStream.parserView).2 with
  | some (.ok ast) => ast
  | _ => dummyAst slice0 swapBaseSource

def parserWithoutOrder :
    Stage TokenStream ParseRefusal (Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start)) :=
  { registeredParserStage slice0 with
    inputInvariant := fun stream => stream.parserView.WF ∧ stream.source.slice = slice0 }

theorem mutantP4_tokensOrdered_load_bearing :
    swappedStream.parserView.WF ∧
    ¬ TokensOrdered swappedStream.parserView ∧
    parserWithoutOrder.run swappedStream = .ok swappedAst ∧
    ¬ Parsed.ElaborationDomain slice0 swappedAst ∧
    ¬ parserWithoutOrder.Validated := by
  have hwf : swappedStream.parserView.WF := by
    change swappedStream.parserView.invalidLocation = none
    decide +kernel
  have hNotOrd : ¬ TokensOrdered swappedStream.parserView := by
    intro hord
    have h7 : swappedStream.parserView.tokens.toList[7]?.map Located.span =
        some ⟨⟨0⟩, 27, 28, 1, 28⟩ := by decide +kernel
    have h9 : swappedStream.parserView.tokens.toList[9]?.map Located.span =
        some ⟨⟨0⟩, 18, 24, 1, 19⟩ := by decide +kernel
    have hlen : 9 < swappedStream.parserView.tokens.toList.length := by decide +kernel
    have hpair := List.pairwise_iff_getElem.mp hord 7 9 (by omega) hlen (by omega)
    rw [List.getElem?_eq_getElem (by omega)] at h7
    rw [List.getElem?_eq_getElem hlen] at h9
    have hs7 : (swappedStream.parserView.tokens.toList[7]'(by omega)).span = ⟨⟨0⟩, 27, 28, 1, 28⟩ :=
      Option.some.inj h7
    have hs9 : (swappedStream.parserView.tokens.toList[9]'hlen).span = ⟨⟨0⟩, 18, 24, 1, 19⟩ :=
      Option.some.inj h9
    have hle := hpair.2
    rw [hs7, hs9] at hle
    exact (show ¬ (27 : Nat) ≤ 24 by decide) hle
  have hSp : (runEarlySparse swappedStream.parserView).2 = some (.ok swappedAst) :=
    sparse_eq_ok_of_isSome slice0 swappedStream (dummyAst slice0 swapBaseSource) (by decide +kernel)
  have hRun : parserWithoutOrder.run swappedStream = .ok swappedAst :=
    parserEntry_run_of_sparse (slice := slice0) hSp
  have hNotDom : ¬ Parsed.ElaborationDomain slice0 swappedAst := by
    intro hDom
    have hElabOk : (Concrete.IntegratedCore.elaborate slice0 swappedAst).isOk = true := by decide +kernel
    cases hElab : Concrete.IntegratedCore.elaborate slice0 swappedAst with
    | error _ => rw [hElab] at hElabOk; cases hElabOk
    | ok inp =>
      have hReady : inp.Ready slice0 :=
        ((registeredElaboratorStage_validated slice0).1 swappedAst inp hDom hElab).1
      have hPrepOk : (inp.prepare .admission).isOk = true := by
        have h : (match Concrete.IntegratedCore.elaborate slice0 swappedAst with
            | .ok x => (x.prepare .admission).isOk
            | .error _ => false) = true := by decide +kernel
        rw [hElab] at h
        exact h
      cases hPrep : inp.prepare .admission with
      | error _ => rw [hPrep] at hPrepOk; cases hPrepOk
      | ok core =>
        have hCoreWF : core.WF := (hReady.prepared hPrep).1
        have hBadOrigin : ∃ site origin, (site, origin) ∈ core.machineProgram.origins ∧
            origin.lexical.startOffset > origin.lexical.endOffset := by
          have h : (match Concrete.IntegratedCore.elaborate slice0 swappedAst with
              | .ok x =>
                match x.prepare .admission with
                | .ok c => c.machineProgram.origins.any (fun e => decide (e.2.lexical.startOffset > e.2.lexical.endOffset))
                | .error _ => false
              | .error _ => false) = true := by decide +kernel
          rw [hElab] at h
          dsimp only at h
          rw [hPrep] at h
          dsimp only at h
          obtain ⟨⟨site, origin⟩, hmem, hgt⟩ := List.any_eq_true.mp h
          exact ⟨site, origin, hmem, of_decide_eq_true hgt⟩
        obtain ⟨site, origin, hmem, hgt⟩ := hBadOrigin
        obtain ⟨_, span, hbinds⟩ := hCoreWF.1.2.2.2.2.2 site origin hmem
        obtain ⟨_, _, _, _, hstart, hend, _, _⟩ := hbinds
        have hle : origin.lexical.startOffset ≤ origin.lexical.endOffset :=
          hstart ▸ hend ▸ span.range_ok
        omega
  refine ⟨hwf, hNotOrd, hRun, hNotDom, ?_⟩
  intro hVal
  have hIn : parserWithoutOrder.inputInvariant swappedStream := ⟨hwf, by decide +kernel⟩
  exact hNotDom (hVal.1 swappedStream swappedAst hIn hRun).1

/-! ### Elaborator mutants on fixed `slice0` and `sliceOne` -/

/-- Mutant E1: value-corrupting elaborator returning `43`'s pending program for `w1Ast` (`42`). -/
def w1WrongInput : AdmissionInput (sliceLanguage slice0) :=
  match Concrete.IntegratedCore.elaborate slice0 w1WrongAst with
  | .ok inp => inp
  | .error _ => w1Input

def mutantE1 : Passes slice0 d5Profile :=
  { frontendPasses with elaborate := fun _ => .ok w1WrongInput }

private def inputReturnExitU8 (inp : AdmissionInput (sliceLanguage slice0)) : Option Int :=
  match inp.prepare .admission with
  | .ok core => execExitU8 core 16
  | .error _ => none

theorem mutantE1_value_corrupt_rejected : ¬ Stage.Sound mutantE1 .elaborator := by
  intro hSound
  have hWrong : SliceElaboration slice0 w1Ast.source.project
      (cast (sliceGrammarStartShape slice0) w1Ast.tree) (.ok w1WrongInput) :=
    (hSound.1.1 w1Ast w1WrongInput w1_edge_contracts.2.2.2.2.1 rfl).2
  have hGood : SliceElaboration slice0 w1Ast.source.project
      (cast (sliceGrammarStartShape slice0) w1Ast.tree) (.ok w1Input) :=
    ((elaborator_validated frontendPasses rfl).1 w1Ast w1Input w1_edge_contracts.2.2.2.2.1 w1_elab_ok).2
  unfold SliceElaboration at hWrong hGood
  rw [dite_eq_left rfl] at hWrong hGood
  cases htree : cast (sliceGrammarStartShape slice0) w1Ast.tree with
  | inr _ => rw [htree] at hGood; exact False.elim hGood
  | inl t =>
    rw [htree] at hWrong hGood
    obtain ⟨p1, hd1, heq1⟩ := hWrong
    obtain ⟨p2, hd2, heq2⟩ := hGood
    have hp : p1 = p2 := Option.some.inj (hd1.symm.trans hd2)
    subst hp
    have hinEq : w1WrongInput = w1Input := Except.ok.inj (heq1.symm.trans heq2)
    have hValNe : inputReturnExitU8 w1WrongInput ≠ inputReturnExitU8 w1Input := by decide +kernel
    exact hValNe (congrArg inputReturnExitU8 hinEq)

/-- Mutant E2: spurious-refusal elaborator refusing valid `w1Ast` on fixed `slice0`. -/
def mutantE2 : Passes slice0 d5Profile :=
  { frontendPasses with elaborate := fun _ => .error (.invalidTree ⟨0⟩) }

theorem mutantE2_spurious_refusal_rejected : ¬ Stage.Sound mutantE2 .elaborator := by
  intro hSound
  have hRef : SliceElaboration slice0 w1Ast.source.project
      (cast (sliceGrammarStartShape slice0) w1Ast.tree) (.error (.invalidTree ⟨0⟩)) :=
    hSound.1.2 w1Ast (.invalidTree ⟨0⟩) w1_edge_contracts.2.2.2.2.1 rfl
  have hGood : SliceElaboration slice0 w1Ast.source.project
      (cast (sliceGrammarStartShape slice0) w1Ast.tree) (.ok w1Input) :=
    ((elaborator_validated frontendPasses rfl).1 w1Ast w1Input w1_edge_contracts.2.2.2.2.1 w1_elab_ok).2
  unfold SliceElaboration at hRef hGood
  rw [dite_eq_left rfl] at hRef hGood
  cases htree : cast (sliceGrammarStartShape slice0) w1Ast.tree with
  | inr _ => rw [htree] at hGood; exact False.elim hGood
  | inl t =>
    rw [htree] at hRef hGood
    obtain ⟨p1, hd1, heq1⟩ := hRef
    obtain ⟨p2, hd2, heq2⟩ := hGood
    have hp : p1 = p2 := Option.some.inj (hd1.symm.trans hd2)
    subst hp
    cases heq1.symm.trans heq2

/-- Mutant E3: `closeBrace`-corrupting elaborator on empty body `fn main() -> u8 { }`,
changing only `Program.closeBrace` from `[18, 19)` (`}`) to `[16, 17)` (`{`). -/
def w6WrongCloseBraceInput : AdmissionInput (sliceLanguage slice0) :=
  .pending w6Source.project (.zero ⟨"main", ⟨3, 7, 1, 4, ⟨0⟩⟩, [], ⟨16, 17, 1, 17, ⟨0⟩⟩⟩) rfl

def mutantE3 : Passes slice0 d5Profile :=
  { frontendPasses with elaborate := fun _ => .ok w6WrongCloseBraceInput }

theorem mutantE3_closeBrace_corrupt_rejected : ¬ Stage.Sound mutantE3 .elaborator := by
  intro hSound
  have hLexOut := (lexer_validated frontendPasses rfl).1 w6Source w6Stream w6_ready w6_lex_ok
  have hParseOut := (parser_validated frontendPasses).1 w6Stream w6Ast hLexOut.1 w6_parse_ok
  have hWrong : SliceElaboration slice0 w6Ast.source.project
      (cast (sliceGrammarStartShape slice0) w6Ast.tree) (.ok w6WrongCloseBraceInput) :=
    (hSound.1.1 w6Ast w6WrongCloseBraceInput hParseOut.1 rfl).2
  have hGood : SliceElaboration slice0 w6Ast.source.project
      (cast (sliceGrammarStartShape slice0) w6Ast.tree) (.ok w6Input) :=
    ((elaborator_validated frontendPasses rfl).1 w6Ast w6Input hParseOut.1 w6_elab_ok).2
  unfold SliceElaboration at hWrong hGood
  rw [dite_eq_left rfl] at hWrong hGood
  cases htree : cast (sliceGrammarStartShape slice0) w6Ast.tree with
  | inr _ => rw [htree] at hGood; exact False.elim hGood
  | inl t =>
    rw [htree] at hWrong hGood
    obtain ⟨p1, hd1, heq1⟩ := hWrong
    obtain ⟨p2, hd2, heq2⟩ := hGood
    have hp : p1 = p2 := Option.some.inj (hd1.symm.trans hd2)
    subst hp
    have hinEq : w6WrongCloseBraceInput = w6Input := Except.ok.inj (heq1.symm.trans heq2)
    have hErrNe : prepErrorCause w6WrongCloseBraceInput ≠ prepErrorCause w6Input := by
      decide +kernel
    exact hErrNe (congrArg prepErrorCause hinEq)

/-- Mutant E4: wrong-refusal elaborator on fixed `sliceOne`, returning `.invalidTree`
instead of `.unsupportedSlice sliceOne` on valid `w7Ast`. -/
def mutantE4 : Passes sliceOne d5Profile :=
  { frontendPassesAt sliceOne with elaborate := fun _ => .error (.invalidTree ⟨0⟩) }

theorem mutantE4_slice1_wrong_refusal_rejected : ¬ Stage.Sound mutantE4 .elaborator := by
  intro hSound
  have hDom : Parsed.ElaborationDomain sliceOne w7Ast := w7_slice1_parse_ok_and_elab_refuses.2.2.1
  have hRef : SliceElaboration sliceOne w7Ast.source.project
      (cast (sliceGrammarStartShape sliceOne) w7Ast.tree) (.error (.invalidTree ⟨0⟩)) :=
    hSound.1.2 w7Ast (.invalidTree ⟨0⟩) hDom rfl
  unfold SliceElaboration at hRef
  rw [dite_eq_right (show sliceOne ≠ slice0 by decide)] at hRef
  cases hRef

end ConcreteChecks.StageInterfaces.D5Frontend
