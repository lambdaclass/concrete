import ConcreteSpec.StageInterface
import ConcreteSpec.StageGrammar
import ConcreteSpec.Spec.SliceGrammar
import ConcreteSpec.Spec.Slice0Rules
import ConcreteSpec.Surface1
import ConcreteSpec.Proofs.SliceGrammar
import ConcreteSpec.Proofs.StageParser
import Concrete.Lex
import Concrete.Lex.StreamProofs
import Concrete.Parse
import Concrete.Stages
import Concrete.Refusal
import Concrete.Interfaces.Pipeline
import Concrete.Interfaces.SurfaceElaboration

/-! Standalone freeze controls for front-end refusal precedence, certified parser
totality/scaling, zero-fuel exhaustion, decoder interruptions, and the `.ifStmt`
scaffolding-row carry-over fix.

Imports only frozen/merged `ConcreteSpec` and `Concrete` modules (no dependency
on `ConcreteChecks/StageInterfaces/Fixtures.lean`). -/

namespace ConcreteChecks.StageInterfaces.Frontend.PrecedenceControls

open Concrete
open Concrete.Interfaces
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open ConcreteSpec.GrammarData (ProductionId)

/-! ## Shared standalone front-end runners and sparse-lookup bridges -/

private def sliceTwo : Slice := ⟨⟨2, by decide⟩⟩
private def sliceThree : Slice := ⟨⟨3, by decide⟩⟩
private def sliceEleven : Slice := ⟨⟨11, by decide⟩⟩

private def mkSource (slice : Slice) (bytes : ByteArray) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes bytes.data.toList), ⟨0⟩, slice⟩

private theorem sliceParser_run_eq_sparse_any (s : Slice) (input : ParserView) :
    (sliceParser s).run input = Parse.KernelEval.runEarlySparse input :=
  Parse.KernelEval.sliceParser_run_eq_sparse input

/-- Result of running the certified lexer + registered `sliceParser s` front end. -/
private inductive FrontOutcome where
  | lexError (failure : LexRefusal)
  | parseError (refusal : ParseRefusal)
  | parseExhausted
  | parsed (steps : Nat) (tokens : Nat) (ast : (sliceGrammar slice0).value (sliceGrammar slice0).start)

private def runFront (s : Slice) (bytes : ByteArray) : FrontOutcome :=
  match Lex.lexStream (mkSource s bytes) with
  | .error e => .lexError e
  | .ok stream =>
    let view := stream.parserView
    let (steps, res) := (sliceParser s).run view
    match res with
    | none => .parseExhausted
    | some (.error r) => .parseError r
    | some (.ok out) => .parsed steps view.tokens.size out.tree

private def runFrontSparse (s : Slice) (bytes : ByteArray) : FrontOutcome :=
  match Lex.lexStream (mkSource s bytes) with
  | .error e => .lexError e
  | .ok stream =>
    let view := stream.parserView
    let (steps, res) := Parse.KernelEval.runEarlySparse view
    match res with
    | none => .parseExhausted
    | some (.error r) => .parseError r
    | some (.ok out) => .parsed steps view.tokens.size out.tree

private theorem runFront_eq_sparse (s : Slice) (bytes : ByteArray) :
    runFront s bytes = runFrontSparse s bytes := by
  unfold runFront runFrontSparse
  cases Lex.lexStream (mkSource s bytes) with
  | error _ => rfl
  | ok stream =>
    dsimp only
    rw [sliceParser_run_eq_sparse_any s stream.parserView]
    cases Parse.KernelEval.runEarlySparse stream.parserView with
    | mk _ res =>
      cases res with
      | none => rfl
      | some r => cases r <;> rfl

private def frontParseRefusal? : FrontOutcome → Option ParseRefusal
  | .parseError r => some r
  | _ => none

private def frontLexCause? : FrontOutcome → Option RefusalCause
  | .lexError (.source c) => some c
  | _ => none

private theorem front_lex_of_sparse {s : Slice} {bytes : ByteArray} {cause : RefusalCause}
    (h : frontLexCause? (runFrontSparse s bytes) = some cause) :
    Lex.lexStream (mkSource s bytes) = .error (.source cause) := by
  unfold runFrontSparse frontLexCause? at h
  cases hlex : Lex.lexStream (mkSource s bytes) with
  | ok stream =>
    rw [hlex] at h
    dsimp only at h
    cases hrun : Parse.KernelEval.runEarlySparse stream.parserView with
    | mk _ res =>
      rw [hrun] at h
      cases res with
      | none => cases h
      | some r => cases r <;> cases h
  | error e =>
    rw [hlex] at h
    cases e with
    | missingFile _ => cases h
    | source c =>
      cases Option.some.inj h
      rfl

private theorem front_parse_of_sparse {s : Slice} {bytes : ByteArray} {r : ParseRefusal}
    (h : frontParseRefusal? (runFrontSparse s bytes) = some r) :
    runFront s bytes = .parseError r := by
  rw [runFront_eq_sparse]
  cases hres : runFrontSparse s bytes with
  | parseError r' =>
    rw [hres] at h
    cases Option.some.inj h
    rfl
  | lexError _ | parseExhausted | parsed _ _ _ =>
    rw [hres] at h
    cases h

/-- Assert that both Slice 0 and Slice 1 lexer runs and both Slice 0 checked/unchecked
driver routes report the exact same lexical cause and registered rule. -/
private def LexRefusalPrecedence (bytes : ByteArray) (cause : RefusalCause) (rule : RuleId) : Prop :=
  Lex.lexStream (mkSource slice0 bytes) = .error (.source cause) ∧
  Lex.lexStream (mkSource sliceOne bytes) = .error (.source cause) ∧
  ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
  ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
  cause.rule = rule

private theorem lexRefusalPrecedence_of_sparse {bytes : ByteArray} {cause : RefusalCause}
    {rule : RuleId}
    (h0 : frontLexCause? (runFrontSparse slice0 bytes) = some cause)
    (h1 : frontLexCause? (runFrontSparse sliceOne bytes) = some cause)
    (hc : Stages.KernelEval.loadCompileErrorSparse "main.con" bytes = some (.source cause))
    (hu : Stages.KernelEval.loadUncheckedErrorSparse "main.con" bytes = some (.source cause))
    (hr : cause.rule = rule) :
    LexRefusalPrecedence bytes cause rule :=
  ⟨front_lex_of_sparse h0, front_lex_of_sparse h1,
   Stages.KernelEval.load_compile_eq_error_of_sparse hc,
   Stages.KernelEval.load_unchecked_eq_error_of_sparse hu, hr⟩

/-- Assert that both Slice 0 and Slice 1 registered parsers report the exact same
`ParseRefusal`, and both Slice 0 checked/unchecked driver routes transport it verbatim. -/
private def MalformedPrecedence (bytes : ByteArray) (refusal : ParseRefusal)
    (cause : RefusalCause) (rule : RuleId) : Prop :=
  runFront slice0 bytes = .parseError refusal ∧
  runFront sliceOne bytes = .parseError refusal ∧
  ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
  ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
  refusal.ruleId = rule ∧
  cause.rule = rule

private theorem malformedPrecedence_of_sparse {bytes : ByteArray} {refusal : ParseRefusal}
    {cause : RefusalCause} {rule : RuleId}
    (h0 : frontParseRefusal? (runFrontSparse slice0 bytes) = some refusal)
    (h1 : frontParseRefusal? (runFrontSparse sliceOne bytes) = some refusal)
    (hc : Stages.KernelEval.loadCompileErrorSparse "main.con" bytes = some (.source cause))
    (hu : Stages.KernelEval.loadUncheckedErrorSparse "main.con" bytes = some (.source cause))
    (hr1 : refusal.ruleId = rule) (hr2 : cause.rule = rule) :
    MalformedPrecedence bytes refusal cause rule :=
  ⟨front_parse_of_sparse h0, front_parse_of_sparse h1,
   Stages.KernelEval.load_compile_eq_error_of_sparse hc,
   Stages.KernelEval.load_unchecked_eq_error_of_sparse hu, hr1, hr2⟩

/-- Run Slice 1 front end through `Lex.lexStream`, `sliceParser sliceOne`, `decodeSurface1`,
and `ConcreteSpec.Surface1.elaborateChecked`. -/
private def runSlice1Elaboration (bytes : ByteArray) :
    Option (Except ConcreteSpec.Surface1.ElaborationRefusal ConcreteSpec.Surface1.Elaborated) :=
  match runFront sliceOne bytes with
  | .parsed _ _ (.inl tree) => (decodeSurface1 tree).map ConcreteSpec.Surface1.elaborateChecked
  | _ => none

private def runSlice1ElaborationSparse (bytes : ByteArray) :
    Option (Except ConcreteSpec.Surface1.ElaborationRefusal ConcreteSpec.Surface1.Elaborated) :=
  match runFrontSparse sliceOne bytes with
  | .parsed _ _ (.inl tree) => (decodeSurface1 tree).map ConcreteSpec.Surface1.elaborateChecked
  | _ => none

private theorem runSlice1Elaboration_eq_sparse (bytes : ByteArray) :
    runSlice1Elaboration bytes = runSlice1ElaborationSparse bytes := by
  unfold runSlice1Elaboration runSlice1ElaborationSparse
  rw [runFront_eq_sparse]

private def slice1ElabError? :
    Option (Except ConcreteSpec.Surface1.ElaborationRefusal ConcreteSpec.Surface1.Elaborated) →
      Option ConcreteSpec.Surface1.ElaborationRefusal
  | some (.error e) => some e
  | _ => none

private theorem slice1Elab_eq_error_of_sparse {bytes : ByteArray}
    {err : ConcreteSpec.Surface1.ElaborationRefusal}
    (h : slice1ElabError? (runSlice1ElaborationSparse bytes) = some err) :
    runSlice1Elaboration bytes = some (.error err) := by
  rw [runSlice1Elaboration_eq_sparse]
  unfold slice1ElabError? at h
  cases hr : runSlice1ElaborationSparse bytes with
  | none => rw [hr] at h; cases h
  | some res =>
    rw [hr] at h
    cases res with
    | ok _ => cases h
    | error e =>
      cases Option.some.inj h
      rfl

/-! ## Carry-over #2 definitional unity check for `TokensOrdered` -/

theorem tokensOrdered_abbrev_eq (input : ParserView) :
    Concrete.Interfaces.TokensOrdered input =
      ConcreteSpec.StageGrammar.TokensOrdered input.tokens.toList :=
  rfl

/-! ## All 5 tie-break precedence fixtures (`_review/tie-codex.md:21-27`) and neighbors -/

-- Fixture 1: `fn main() -> u8 { return ; @ }` -> Lexed@2 at `@` [27,28) on both slices & routes.
theorem fixture1_lex_beats_parse :
    LexRefusalPrecedence r"fn main() -> u8 { return ; @ }".toUTF8
      (.lexical .character ⟨27, 28, 1, 28, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- Neighbor 1a: removing `@` exposes Parsed@1 at `;` [25,26) on both slices & routes.
theorem fixture1_neighbor_parse :
    MalformedPrecedence r"fn main() -> u8 { return ; }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 25, 26, 1, 26⟩)
      (.parser .unexpectedToken ⟨25, 26, 1, 26, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Fixture 2: `fn main() -> u8 { return missing }` -> Parsed@1 at `}` [33,34) on both slices & routes.
theorem fixture2_parse_missing_semi :
    MalformedPrecedence r"fn main() -> u8 { return missing }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 33, 34, 1, 34⟩)
      (.parser .unexpectedToken ⟨33, 34, 1, 34, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 2a: `;`-terminated `return missing;` advances to surface preparation (`LocalBound@1` at `missing` [25,32)).
theorem fixture2_neighbor_complete_localBound :
    let bytes := r"fn main() -> u8 { return missing; }".toUTF8
    let cause : RefusalCause := .surface (.unknownLocal "missing" ⟨25, 32, 1, 26, ⟨0⟩⟩)
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.LocalBound, 1⟩ :=
  ⟨Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

-- Neighbor 2b: `return missing @ }` stops at lexer (`Lexed@2` at `@` [33,34)).
theorem fixture2_neighbor_lex :
    LexRefusalPrecedence r"fn main() -> u8 { return missing @ }".toUTF8
      (.lexical .character ⟨33, 34, 1, 34, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- Fixture 3: `fn main() -> u8 { return 255 + 1 @ }` -> Lexed@2 at `@` [33,34), never arithmetic/trap.
theorem fixture3_lex_beats_overflow :
    LexRefusalPrecedence r"fn main() -> u8 { return 255 + 1 @ }".toUTF8
      (.lexical .character ⟨33, 34, 1, 34, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- Neighbor 3a: without `@`, missing `;` reports Parsed@1 at `}` [33,34) on both slices & routes.
theorem fixture3_neighbor_missing_semi :
    MalformedPrecedence r"fn main() -> u8 { return 255 + 1 }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 33, 34, 1, 34⟩)
      (.parser .unexpectedToken ⟨33, 34, 1, 34, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 3b: missing `)` in grouped overflow reports Parsed@1 at `}` [34,35).
theorem fixture3_neighbor_missing_rparen :
    MalformedPrecedence r"fn main() -> u8 { return (255 + 1 }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 34, 35, 1, 35⟩)
      (.parser .unexpectedToken ⟨34, 35, 1, 35, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 3c: missing `;` between binding overflow and next `let` reports Parsed@1 at `let` [38,41).
theorem fixture3_neighbor_missing_semi_between_lets :
    MalformedPrecedence r"fn main() -> u8 { let x: u8 = 255 + 1 let y: u8 = 2; return x; }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 38, 41, 1, 39⟩)
      (.parser .unexpectedToken ⟨38, 41, 1, 39, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 3d: complete `return 255 + 1;` refuses `AdditionFits@1` at `+` [29,30) on checked route,
-- and compiles (for runtime trap) on unchecked route.
theorem fixture3_neighbor_complete_checked_vs_unchecked :
    let bytes := r"fn main() -> u8 { return 255 + 1; }".toUTF8
    let cause : RefusalCause :=
      .core .slice0 ⟨⟨0⟩, 29, 30, 1, 30⟩ [] (.atPosition 0 .additionOverflowUnproven)
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.AdditionFits, 1⟩ ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked).isOk = true := by
  refine ⟨Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel), rfl, ?_⟩
  rw [Stages.KernelEval.load_unchecked_eq_sparse]
  decide +kernel

-- Fixture 4 (Slice 0): `fn main() -> u8 { let Option {} = 1; return 0; }`
-- reports AdmittedProduction@3, cause `.production .destructure`, at `{` [29,30), required = sliceOne.
theorem fixture4_slice0_destructure_unsupported :
    let bytes := r"fn main() -> u8 { let Option {} = 1; return 0; }".toUTF8
    let refusal : ParseRefusal :=
      .unsupported earlyUnsupportedProductionRule (.production .destructure)
        ⟨⟨0⟩, 29, 30, 1, 30⟩ sliceOne
    let cause : RefusalCause := .parser .outsideProduction ⟨29, 30, 1, 30, ⟨0⟩⟩
    runFront slice0 bytes = .parseError refusal ∧
    refusal.ruleId = ⟨``Slice0Rules.AdmittedProduction, 3⟩ ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.AdmittedProduction, 3⟩ :=
  ⟨front_parse_of_sparse (by decide +kernel), rfl,
   Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

-- Fixture 4 (Slice 1): passes `sliceParser sliceOne` and `decodeSurface1`, then Slice 1 surface
-- elaboration refuses undeclared struct `Option` at [22,28) citing `StructDeclared@1`.
theorem fixture4_slice1_unknown_destructured_struct :
    let bytes := r"fn main() -> u8 { let Option {} = 1; return 0; }".toUTF8
    let err : ConcreteSpec.Surface1.ElaborationRefusal :=
      .unknownDestructuredStruct "Option" ⟨22, 28, 1, 23, ⟨0⟩⟩
    runSlice1Elaboration bytes = some (.error err) ∧
    err.ruleId = some ConcreteSpec.Surface1.structDeclaredRule ∧
    err.span? = some ⟨22, 28, 1, 23, ⟨0⟩⟩ :=
  ⟨slice1Elab_eq_error_of_sparse (by decide +kernel), rfl, rfl⟩

-- Neighbor 4a: colon-annotated `let Option: u8 = 1; return Option;` parses at Slice 0 and
-- fails surface preparation with `UnreservedLocal@2` at `Option` [22,28).
theorem fixture4_neighbor_colon_unreserved :
    let bytes := r"fn main() -> u8 { let Option: u8 = 1; return Option; }".toUTF8
    let cause : RefusalCause := .surface (.reservedBinder "Option" ⟨22, 28, 1, 23, ⟨0⟩⟩)
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.UnreservedLocal, 2⟩ :=
  ⟨Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

-- Neighbor 4b: truncated after `1` -> Parsed@1 at EOF [35,35) on both Slice 0 and Slice 1.
theorem fixture4_neighbor_eof_malformed :
    MalformedPrecedence r"fn main() -> u8 { let Option {} = 1".toUTF8
      (.malformed earlyMalformedRule .unexpectedEnd ⟨⟨0⟩, 35, 35, 1, 36⟩)
      (.parser .unexpectedEnd ⟨35, 35, 1, 36, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 4c: ending with `1 }` -> Parsed@1 at `}` [36,37) on both Slice 0 and Slice 1.
theorem fixture4_neighbor_missing_semi_malformed :
    MalformedPrecedence r"fn main() -> u8 { let Option {} = 1 }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 36, 37, 1, 37⟩)
      (.parser .unexpectedToken ⟨36, 37, 1, 37, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Neighbor 4d: bare `let Option {} = 1` -> Parsed@1 at `let` [0,3) on both Slice 0 and Slice 1.
theorem fixture4_neighbor_bare_let_malformed :
    MalformedPrecedence r"let Option {} = 1".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 0, 3, 1, 1⟩)
      (.parser .unexpectedToken ⟨0, 3, 1, 1, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Fixture 5: `fn main() -> u8 { return (42); }`
-- Exact AST spans (literal [26,28), return statement [18,30), closing brace [31,32)),
-- registry/driver parse parity, and acceptance on both checked and unchecked routes.
private def groupedAnswerExpected : ConcreteSpec.Surface0.Program :=
  ⟨"main", ⟨3, 7, 1, 4, ⟨0⟩⟩,
    [.ret (.integer 42 none ⟨26, 28, 1, 27, ⟨0⟩⟩) ⟨18, 30, 1, 19, ⟨0⟩⟩],
    ⟨31, 32, 1, 32, ⟨0⟩⟩⟩

theorem fixture5_grouped_return_parity :
    let bytes := r"fn main() -> u8 { return (42); }".toUTF8
    Parse.parseBytes bytes = .ok groupedAnswerExpected ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.compile).isOk = true ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked).isOk = true := by
  refine ⟨Parse.KernelEval.parseBytes_eq_ok_of_sparse (by decide +kernel),
    Stages.KernelEval.load_compile_isOk_of_sparse (by decide +kernel), ?_⟩
  rw [Stages.KernelEval.load_unchecked_eq_sparse]
  decide +kernel

/-! ## Surface preparation span controls (Codex 5 & agy note 2) -/

-- Codex 5: empty body `fn main() -> u8 { }` records `closeBrace` at `}` [18,19) and
-- refuses `Returns@1` at `}` on both checked and unchecked routes.
theorem codex5_empty_body_closeBrace_fallthrough :
    let bytes := r"fn main() -> u8 { }".toUTF8
    let expectedAst : ConcreteSpec.Surface0.Program :=
      ⟨"main", ⟨3, 7, 1, 4, ⟨0⟩⟩, [], ⟨18, 19, 1, 19, ⟨0⟩⟩⟩
    let cause : RefusalCause := .surface (.fallthrough ⟨18, 19, 1, 19, ⟨0⟩⟩)
    Parse.parseBytes bytes = .ok expectedAst ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.Returns, 1⟩ :=
  ⟨Parse.KernelEval.parseBytes_eq_ok_of_sparse (by decide +kernel),
   Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

-- agy note 2: unreachable statement after `return` cites `ReachableBody@1` with full
-- statement span `[29, 43)` covering `let x: u8 = 1;`.
theorem agy_note2_unreachable_full_statement_span :
    let bytes := r"fn main() -> u8 { return 42; let x: u8 = 1; }".toUTF8
    let cause : RefusalCause := .surface (.unreachable ⟨29, 43, 1, 30, ⟨0⟩⟩)
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.ReachableBody, 1⟩ :=
  ⟨Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

/-! ## Carry-over #1 control: `if` cites `.conditional`, never scaffolding `.ifStmt` -/

-- Concrete Slice 0 & Slice 1 controls: `if` reports `.production .conditional` at `if` [18,20)
-- with whole-file `required = slice 3`, never `.production .ifStmt`.
theorem carryover_if_cites_conditional_not_ifStmt :
    let bytes := r"fn main() -> u8 { if 1 { return 0; } return 0; }".toUTF8
    let refusal : ParseRefusal :=
      .unsupported earlyUnsupportedProductionRule (.production .conditional)
        ⟨⟨0⟩, 18, 20, 1, 19⟩ sliceThree
    let cause : RefusalCause := .parser .outsideProduction ⟨18, 20, 1, 19, ⟨0⟩⟩
    runFront slice0 bytes = .parseError refusal ∧
    runFront sliceOne bytes = .parseError refusal ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.compile) = .error (.source cause) ∧
    ((Stages.Input.load "main.con" bytes).bind Stages.unchecked) = .error (.source cause) ∧
    cause.rule = ⟨``Slice0Rules.AdmittedProduction, 3⟩ :=
  ⟨front_parse_of_sparse (by decide +kernel),
   front_parse_of_sparse (by decide +kernel),
   Stages.KernelEval.load_compile_eq_error_of_sparse (by decide +kernel),
   Stages.KernelEval.load_unchecked_eq_error_of_sparse (by decide +kernel), rfl⟩

-- Malformed `if` stays malformed (`Parsed@1` at `}` [21,22)), never `.unsupported`.
theorem carryover_malformed_if_stays_malformed :
    MalformedPrecedence r"fn main() -> u8 { if }".toUTF8
      (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 21, 22, 1, 22⟩)
      (.parser .unexpectedToken ⟨21, 22, 1, 22, ⟨0⟩⟩)
      ⟨``Slice0Rules.Parsed, 1⟩ :=
  malformedPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl rfl

-- Two-violation `if` with a Slice 11 string literal in the condition head:
-- first witness stays `.production .conditional` at `if` [18,20), whole-file `required = slice 11`.
theorem carryover_if_two_violation_preserves_conditional_witness :
    let bytes := r#"fn main() -> u8 { if "hi" { return 0; } return 0; }"#.toUTF8
    runFront slice0 bytes = .parseError
      (.unsupported earlyUnsupportedProductionRule (.production .conditional)
        ⟨⟨0⟩, 18, 20, 1, 19⟩ sliceEleven) :=
  front_parse_of_sparse (by decide +kernel)

private theorem earlyProduction_ifStmt_unique (p : Production earlyValue)
    (hp : p ∈ earlySharedGrammar.productions.toList) :
    p.id = .ifStmt ↔ p = earlyProduction .ifStmt := by
  have hids := earlySharedGrammar_productionIds_eq
  unfold earlySharedGrammar earlyProductions at hp
  simp only [List.mem_map] at hp
  obtain ⟨id, _, rfl⟩ := hp
  constructor
  · intro hid
    change id = .ifStmt at hid
    rw [hid]
  · intro heq
    exact congrArg Production.id heq

private theorem earlyDerivesTrace_ifStmt_slice0 {slice : Slice} {eof : FileSpan}
    {n : Fin ConcreteSpec.GrammarData.Nonterminal.count} {pos : Nat}
    {tokens rest : List SurfaceToken} {val : earlySharedGrammar.value n}
    {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace earlySharedGrammar slice eof n pos tokens rest val steps events) :
    ∀ ev ∈ events, ev.cause = .production .ifStmt → ev.slice = slice0 :=
  DerivesTrace.rec
    (motive_1 := fun _ _ _ _ _ _ evs _ =>
      ∀ ev ∈ evs, ev.cause = .production .ifStmt → ev.slice = slice0)
    (motive_2 := fun prodId rhs _ _ _ _ _ evs _ =>
      (prodId = .ifStmt → ∀ s ∈ rhs, match s with | .t _ => False | .n _ => True) →
      ∀ ev ∈ evs, ev.cause = .production .ifStmt → ev.slice = slice0)
    (fun p _ _ _ _ _ _ hmem _ _ ih ev hev hcause => by
      rcases List.mem_cons.mp hev with rfl | hrest
      · have hpid : p.id = .ifStmt := UnsupportedCause.production.inj hcause
        have hp : p = earlyProduction .ifStmt := (earlyProduction_ifStmt_unique p hmem).mp hpid
        rw [hp]; rfl
      · refine ih (fun hpid => ?_) ev hrest hcause
        have hp : p = earlyProduction .ifStmt := (earlyProduction_ifStmt_unique p hmem).mp hpid
        rw [hp]
        intro s hs
        simp only [earlyProduction, earlyRhs, ProductionId.row, List.map,
          List.mem_cons, List.not_mem_nil, or_false] at hs
        rcases hs with rfl | rfl <;> trivial)
    (fun _ _ _ _ _ h => nomatch h)
    (fun {prodId kind tok rhs _ _ _ _ _ _} _ _ _ ih hinv ev hev hcause => by
      rcases List.mem_cons.mp hev with rfl | hrest
      · cases htok : tok.value with
        | integer _ radix _ =>
          cases radix with
          | decimal =>
            rw [htok] at hcause
            have hpid : prodId = .ifStmt := UnsupportedCause.production.inj hcause
            exact False.elim (hinv hpid (.t kind) List.mem_cons_self)
          | binary | octal | hexadecimal =>
            rw [htok] at hcause
            cases hcause
        | fixed _ _ | name _ | stringLiteral _ | byteLiteral _ | label _ =>
          rw [htok] at hcause
          have hpid : prodId = .ifStmt := UnsupportedCause.production.inj hcause
          exact False.elim (hinv hpid (.t kind) List.mem_cons_self)
      · exact ih (fun hpid s hs => hinv hpid s (List.mem_cons_of_mem _ hs)) ev hrest hcause)
    (fun _ _ ihL ihR hinv ev hev hcause => by
      rcases List.mem_append.mp hev with hL | hR
      · exact ihL ev hL hcause
      · exact ihR (fun hpid s hs => hinv hpid s (List.mem_cons_of_mem _ hs)) ev hR hcause)
    h

/-- Universal scaffolding exclusion: no unsupported refusal of `sliceParser s` at any slice
on any `ParserView` ever cites the scaffolding row `.production .ifStmt`. -/
theorem sliceParser_unsupported_ne_ifStmt (s : Slice) (input : ParserView)
    (rule : RegisteredRule) (cause : UnsupportedCause) (span : FileSpan) (required : Slice)
    (hrun : ((sliceParser s).run input).2 = some (.error (.unsupported rule cause span required))) :
    cause ≠ .production .ifStmt := by
  intro heq
  subst heq
  obtain ⟨_, _, _, ⟨tree, steps, events, htr⟩, hall⟩ :=
    parserEngine_unsupported_first_witness (sliceParser s) input rule (.production .ifStmt)
      span required hrun
  obtain ⟨_, _, _, pre, hit, post, hevEq, _, hhitLt, _, hcs, _, _⟩ := hall _ _ _ _ htr
  have hmem : hit ∈ events := by
    rw [hevEq]
    exact List.mem_append_right _ List.mem_cons_self
  have hslice0 : hit.slice = slice0 := earlyDerivesTrace_ifStmt_slice0 htr hit hmem hcs
  rw [hslice0] at hhitLt
  exact Nat.not_lt_zero _ hhitLt

/-! ## Zero-fuel control with distinct lookahead and EOF spans -/

private def zeroFuelSource : Source :=
  mkSource slice0 r"fn main() -> u8 { return (42); }".toUTF8

private def zeroFuelFirstTok : SurfaceToken :=
  ⟨⟨⟨0⟩, 0, 2, 1, 1⟩, .fixed .fn rfl⟩

private def zeroFuelView : ParserView :=
  ⟨zeroFuelSource, #[zeroFuelFirstTok]⟩

private def zeroFuelParser : ParserEngine (sliceGrammar slice0) :=
  { sliceParser slice0 with stepsPerToken := 0 }

-- Distinct lookahead span [0,2) and EOF span [32,32): zero fuel reports exhaustion (`none`),
-- never fabricating a malformed/unsupported refusal; the certified `sliceParser` never exhausts.
theorem zero_fuel_distinct_eof_and_lookahead :
    zeroFuelView.WF ∧
    zeroFuelFirstTok.span ≠ eofFileSpan zeroFuelSource ∧
    eofFileSpan zeroFuelSource = ⟨⟨0⟩, 32, 32, 1, 33⟩ ∧
    zeroFuelParser.run zeroFuelView = (1, none) ∧
    ((sliceParser slice0).run zeroFuelView).2.isSome = true := by
  refine ⟨by unfold ParserView.WF; decide, by decide, by decide, ?_, sliceParser_nonExhausted slice0 zeroFuelView⟩
  unfold zeroFuelParser ParserEngine.run
  have hwf : zeroFuelView.invalidLocation = none := by decide
  simp only [hwf, Nat.zero_mul]
  rfl

/-! ## 200-binding malformed-return linear-work scaling control (Codex 8)

Instead of replaying 1,410 tokens by kernel reduction, we construct the parametric
`n`-binding family `fn main() -> u8 { let x: u8 = 0; ... return ; }`, certify its
byte/span parity with `Lex.lexStream` at `n = 3`, prove `(scalingView n).WF` and the
exact linear step recurrence `25 + 77 * n` by induction on `n` over `sliceParser`,
and instantiate the certified bound `sliceParser_steps_le` and totality
`sliceParser_nonExhausted` at `n = 200` (`1410` tokens). -/

private def scalingPrefixBytes : List UInt8 := r"fn main() -> u8 { ".toUTF8.data.toList
private def scalingBindingBytes : List UInt8 := r"let x: u8 = 0; ".toUTF8.data.toList
private def scalingSuffixBytes : List UInt8 := r"return ; }".toUTF8.data.toList

private def repeatBytes : Nat → List UInt8 → List UInt8
  | 0, _ => []
  | k + 1, bs => bs ++ repeatBytes k bs

private def scalingByteList (n : Nat) : List UInt8 :=
  scalingPrefixBytes ++ repeatBytes n scalingBindingBytes ++ scalingSuffixBytes

private def scalingSource (n : Nat) : Source :=
  ⟨Project.singleton mainSourcePath (SourceText.ofBytes (scalingByteList n)), ⟨0⟩, slice0⟩

private def mkSpan1 (start stop : Nat) : FileSpan :=
  ⟨⟨0⟩, start, stop, 1, start + 1⟩

private def scalingPrefixTokens : List SurfaceToken :=
  [ ⟨mkSpan1 0 2, .fixed .fn rfl⟩,
    ⟨mkSpan1 3 7, .name "main"⟩,
    ⟨mkSpan1 7 8, .fixed .lparen rfl⟩,
    ⟨mkSpan1 8 9, .fixed .rparen rfl⟩,
    ⟨mkSpan1 10 12, .fixed .arrow rfl⟩,
    ⟨mkSpan1 13 15, .fixed .u8 rfl⟩,
    ⟨mkSpan1 16 17, .fixed .lbrace rfl⟩ ]

private def scalingBindingTokens (base : Nat) : List SurfaceToken :=
  [ ⟨mkSpan1 base (base + 3), .fixed .let_ rfl⟩,
    ⟨mkSpan1 (base + 4) (base + 5), .name "x"⟩,
    ⟨mkSpan1 (base + 5) (base + 6), .fixed .colon rfl⟩,
    ⟨mkSpan1 (base + 7) (base + 9), .fixed .u8 rfl⟩,
    ⟨mkSpan1 (base + 10) (base + 11), .fixed .equal rfl⟩,
    ⟨mkSpan1 (base + 12) (base + 13), .integer 0 .decimal none⟩,
    ⟨mkSpan1 (base + 13) (base + 14), .fixed .semi rfl⟩ ]

private def scalingBindingsTokens : Nat → Nat → List SurfaceToken
  | 0, _ => []
  | k + 1, base => scalingBindingTokens base ++ scalingBindingsTokens k (base + 15)

private def scalingSuffixTokens (tail : Nat) : List SurfaceToken :=
  [ ⟨mkSpan1 tail (tail + 6), .fixed .return_ rfl⟩,
    ⟨mkSpan1 (tail + 7) (tail + 8), .fixed .semi rfl⟩,
    ⟨mkSpan1 (tail + 9) (tail + 10), .fixed .rbrace rfl⟩ ]

private def scalingTokenList (n : Nat) : List SurfaceToken :=
  scalingPrefixTokens ++ scalingBindingsTokens n 18 ++ scalingSuffixTokens (18 + 15 * n)

private def scalingView (n : Nat) : ParserView :=
  ⟨scalingSource n, (scalingTokenList n).toArray⟩

private theorem repeatBytes_length (n : Nat) (bs : List UInt8) :
    (repeatBytes n bs).length = n * bs.length := by
  induction n with
  | zero => simp [repeatBytes]
  | succ k ih =>
    simp only [repeatBytes, List.length_append, ih, Nat.succ_mul]
    omega

private theorem scalingByteList_length (n : Nat) :
    (scalingByteList n).length = 15 * n + 28 := by
  unfold scalingByteList
  simp only [List.length_append, repeatBytes_length]
  have hp : scalingPrefixBytes.length = 18 := by decide
  have hb : scalingBindingBytes.length = 15 := by decide
  have hs : scalingSuffixBytes.length = 10 := by decide
  rw [hp, hb, hs]
  omega

private theorem scalingByteList_no_eol (n : Nat) :
    ∀ b ∈ scalingByteList n, b.toNat ≠ 13 ∧ b.toNat ≠ 10 := by
  have hp : ∀ b ∈ scalingPrefixBytes, b.toNat ≠ 13 ∧ b.toNat ≠ 10 := by decide
  have hb : ∀ b ∈ scalingBindingBytes, b.toNat ≠ 13 ∧ b.toNat ≠ 10 := by decide
  have hs : ∀ b ∈ scalingSuffixBytes, b.toNat ≠ 13 ∧ b.toNat ≠ 10 := by decide
  have hrep : ∀ k b, b ∈ repeatBytes k scalingBindingBytes → b.toNat ≠ 13 ∧ b.toNat ≠ 10 := by
    intro k
    induction k with
    | zero => intro _ h; cases h
    | succ k ih =>
      intro b h
      rcases List.mem_append.mp h with h1 | h2
      · exact hb b h1
      · exact ih b h2
  intro b h
  unfold scalingByteList at h
  rcases List.mem_append.mp h with h1 | h3
  · rcases List.mem_append.mp h1 with h1a | h1b
    · exact hp b h1a
    · exact hrep n b h1b
  · exact hs b h3

private theorem scanl_advanceByte_no_eol :
    ∀ (bs : List Nat) (p : Position),
      (∀ b ∈ bs, b ≠ 13 ∧ b ≠ 10) →
      ∀ k ≤ bs.length,
        ((bs.scanl Position.advanceByte p).map fun q => (q.line, q.column))[k]? =
          some (p.line, p.column + k)
  | [], p, _, k, hk => by
    simp only [List.length_nil] at hk
    have hk0 : k = 0 := by omega
    subst hk0
    rfl
  | b :: rest, p, hno, k, hk => by
    cases k with
    | zero =>
      simp only [List.scanl_cons, List.map_cons, List.getElem?_cons_zero, Nat.add_zero]
    | succ k' =>
      have hb := hno b List.mem_cons_self
      have hstep : Position.advanceByte p b = ⟨p.byte + 1, p.line, p.column + 1, false⟩ := by
        unfold Position.advanceByte Position.step
        have h13 : (b == 13) = false := beq_false_of_ne hb.1
        have h10 : (b == 10) = false := beq_false_of_ne hb.2
        simp only [h13, h10, Bool.false_eq_true, ↓reduceIte]
      have ih := scanl_advanceByte_no_eol rest (Position.advanceByte p b)
        (fun x hx => hno x (List.mem_cons_of_mem _ hx)) k' (by simp only [List.length_cons] at hk; omega)
      simp only [List.scanl_cons, List.map_cons, List.getElem?_cons_succ]
      rw [ih, hstep]
      dsimp only
      have hcol : p.column + 1 + k' = p.column + (k' + 1) := by omega
      rw [hcol]

private theorem scalingBindingsTokens_length (k base : Nat) :
    (scalingBindingsTokens k base).length = 7 * k := by
  induction k generalizing base with
  | zero => rfl
  | succ k ih =>
    simp only [scalingBindingsTokens, List.length_append, ih]
    change 7 + 7 * k = 7 * (k + 1)
    omega

private theorem scalingTokenList_length (n : Nat) :
    (scalingTokenList n).length = 7 * n + 10 := by
  unfold scalingTokenList
  simp only [List.length_append, scalingBindingsTokens_length]
  change 7 + 7 * n + 3 = 7 * n + 10
  omega

private theorem scalingView_tokens_size (n : Nat) :
    (scalingView n).tokens.size = 7 * n + 10 := by
  simp only [scalingView, List.size_toArray, scalingTokenList_length]

private theorem scalingTokenList_spans_ok (n : Nat) :
    ∀ tok ∈ scalingTokenList n,
      tok.span.file = ⟨0⟩ ∧
      tok.span.startOffset ≤ tok.span.endOffset ∧
      tok.span.endOffset ≤ 15 * n + 28 ∧
      tok.span.line = 1 ∧
      tok.span.col = tok.span.startOffset + 1 := by
  have hbind : ∀ k base, base + 15 * k ≤ 18 + 15 * n →
      ∀ tok ∈ scalingBindingsTokens k base,
        tok.span.file = ⟨0⟩ ∧
        tok.span.startOffset ≤ tok.span.endOffset ∧
        tok.span.endOffset ≤ 15 * n + 28 ∧
        tok.span.line = 1 ∧
        tok.span.col = tok.span.startOffset + 1 := by
    intro k
    induction k with
    | zero => intro _ _ _ h; cases h
    | succ k ih =>
      intro base hle tok hmem
      rcases List.mem_append.mp hmem with h1 | h2
      · simp only [scalingBindingTokens, List.mem_cons, List.not_mem_nil, or_false] at h1
        rcases h1 with rfl | rfl | rfl | rfl | rfl | rfl | rfl <;>
          exact ⟨rfl, by dsimp [mkSpan1]; omega, by dsimp [mkSpan1]; omega, rfl, rfl⟩
      · exact ih (base + 15) (by omega) tok h2
  intro tok hmem
  unfold scalingTokenList at hmem
  rcases List.mem_append.mp hmem with h1 | h3
  · rcases List.mem_append.mp h1 with h1a | h1b
    · simp only [scalingPrefixTokens, List.mem_cons, List.not_mem_nil, or_false] at h1a
      rcases h1a with rfl | rfl | rfl | rfl | rfl | rfl | rfl <;>
        exact ⟨rfl, by dsimp [mkSpan1]; omega, by dsimp [mkSpan1]; omega, rfl, rfl⟩
    · exact hbind n 18 (by omega) tok h1b
  · simp only [scalingSuffixTokens, List.mem_cons, List.not_mem_nil, or_false] at h3
    rcases h3 with rfl | rfl | rfl <;>
      exact ⟨rfl, by dsimp [mkSpan1]; omega, by dsimp [mkSpan1]; omega, rfl, rfl⟩

private theorem scalingView_wf (n : Nat) : (scalingView n).WF := by
  unfold ParserView.WF ParserView.invalidLocation ParserView.validateCoordinates
  simp only [scalingView, scalingSource, Project.singleton, Project.load, Project.files,
    File.load, SourceText.ofBytes, Function.comp_apply, List.getElem?_toArray,
    List.map_cons, List.map_nil, List.getElem?_cons_zero, Bind.bind, Except.bind,
    Pure.pure, Except.pure, throw, throwThe, MonadExceptOf.throw]
  let bs := (scalingByteList n).map UInt8.toNat
  let positions := ((bs.scanl Position.advanceByte {}).map fun p => (p.line, p.column)).toArray
  have hno : ∀ b ∈ bs, b ≠ 13 ∧ b ≠ 10 := by
    intro b hb
    obtain ⟨u, hu, rfl⟩ := List.mem_map.mp hb
    exact scalingByteList_no_eol n u hu
  have hbsLen : bs.length = 15 * n + 28 := by
    simp only [bs, List.length_map, scalingByteList_length]
  have hposSize : positions.size = 15 * n + 29 := by
    simp only [positions, List.size_toArray, List.length_map, List.length_scanl, hbsLen]
  have hback : positions.back?.isSome = true := by
    simp only [Array.back?, hposSize]
    have hlt : 15 * n + 29 - 1 < positions.size := by omega
    rw [Array.getElem?_eq_getElem hlt]
    rfl
  split
  · rename_i bad heq
    split at heq
    · split at heq
      · rename_i badPair hsome
        rw [Array.findSome?_eq_none_iff.mpr (by
          intro tok htok
          simp only [List.mem_toArray] at htok
          obtain ⟨hfile, hle, hend, hline, hcol⟩ := scalingTokenList_spans_ok n tok htok
          have hendLt : tok.span.endOffset < positions.size := by omega
          have hstartLe : tok.span.startOffset ≤ bs.length := by omega
          have hpos : positions[tok.span.startOffset]? = some (1, tok.span.startOffset + 1) := by
            simp only [positions, List.getElem?_toArray]
            have h := scanl_advanceByte_no_eol bs {} hno tok.span.startOffset hstartLe
            simpa [Nat.add_comm] using h
          let files : Array (Array (Nat × Nat)) := #[positions]
          have hfiles : files[tok.span.file.index]? = some positions := by
            rw [hfile]; rfl
          change Option.map (fun reason => (tok.span, reason))
            (match files[tok.span.file.index]? with
             | none => some LocationRefusal.unknownFile
             | some table =>
               if !(tok.span.startOffset ≤ tok.span.endOffset) then some .reversedRange
               else if !(tok.span.endOffset < table.size) then some (.pastEnd (table.size - 1))
               else match table[tok.span.startOffset]? with
                 | none => some (.pastEnd (table.size - 1))
                 | some (line, col) =>
                   if tok.span.line != line then some (.wrongLine line)
                   else if tok.span.col != col then some (.wrongColumn col)
                   else none) = none
          simp [hfiles, hle, hendLt, hpos, hline, hcol])] at hsome
        contradiction
      · contradiction
    · rename_i hnone
      have hnone' : positions.back? = none := by
        cases hb : positions.back? with
        | none => rfl
        | some pair => exact (hnone pair.1 pair.2 hb).elim
      simp [hnone'] at hback
  · rfl

private def tokenValueEq : TokenValue → TokenValue → Bool
  | .fixed k1 _, .fixed k2 _ => k1 == k2
  | .name s1, .name s2 => s1 == s2
  | .integer v1 r1 suf1, .integer v2 r2 suf2 => v1 == v2 && r1 == r2 && suf1 == suf2
  | .stringLiteral b1, .stringLiteral b2 => b1 == b2
  | .byteLiteral b1, .byteLiteral b2 => b1 == b2
  | .label s1, .label s2 => s1 == s2
  | _, _ => false

private theorem tokenValueEq_sound : ∀ {a b : TokenValue}, tokenValueEq a b = true → a = b
  | .fixed _ _, .fixed _ _, h => by
    have hk : _ = _ := eq_of_beq h
    subst hk; rfl
  | .name _, .name _, h => congrArg TokenValue.name (eq_of_beq h)
  | .integer .., .integer .., h => by
    simp only [tokenValueEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨⟨rfl, rfl⟩, rfl⟩; rfl
  | .stringLiteral _, .stringLiteral _, h => congrArg TokenValue.stringLiteral (eq_of_beq h)
  | .byteLiteral _, .byteLiteral _, h => congrArg TokenValue.byteLiteral (eq_of_beq h)
  | .label _, .label _, h => congrArg TokenValue.label (eq_of_beq h)
  | .fixed .., .name .., h | .fixed .., .integer .., h | .fixed .., .stringLiteral .., h
  | .fixed .., .byteLiteral .., h | .fixed .., .label .., h
  | .name .., .fixed .., h | .name .., .integer .., h | .name .., .stringLiteral .., h
  | .name .., .byteLiteral .., h | .name .., .label .., h
  | .integer .., .fixed .., h | .integer .., .name .., h | .integer .., .stringLiteral .., h
  | .integer .., .byteLiteral .., h | .integer .., .label .., h
  | .stringLiteral .., .fixed .., h | .stringLiteral .., .name .., h | .stringLiteral .., .integer .., h
  | .stringLiteral .., .byteLiteral .., h | .stringLiteral .., .label .., h
  | .byteLiteral .., .fixed .., h | .byteLiteral .., .name .., h | .byteLiteral .., .integer .., h
  | .byteLiteral .., .stringLiteral .., h | .byteLiteral .., .label .., h
  | .label .., .fixed .., h | .label .., .name .., h | .label .., .integer .., h
  | .label .., .stringLiteral .., h | .label .., .byteLiteral .., h => by cases h

private def surfaceTokensEq : List SurfaceToken → List SurfaceToken → Bool
  | [], [] => true
  | x :: xs, y :: ys => x.span == y.span && tokenValueEq x.value y.value && surfaceTokensEq xs ys
  | _, _ => false

private theorem surfaceTokensEq_sound :
    ∀ {xs ys : List SurfaceToken}, surfaceTokensEq xs ys = true → xs = ys
  | [], [], _ => rfl
  | ⟨s1, v1⟩ :: xs, ⟨s2, v2⟩ :: ys, h => by
    simp only [surfaceTokensEq, Bool.and_eq_true, beq_iff_eq] at h
    rcases h with ⟨⟨rfl, hv⟩, hrest⟩
    rw [tokenValueEq_sound hv, surfaceTokensEq_sound hrest]
  | [], _ :: _, h | _ :: _, [], h => by cases h

-- Certify that the parametric `scalingView` matches real `Lex.lexStream` output at `n = 3`.
theorem scaling_3_bindings_lex_parity :
    ∃ stream,
      Lex.lexStream (scalingSource 3) = .ok stream ∧
      stream.parserView.tokens = (scalingView 3).tokens := by
  have hok : (match Lex.lexStream (scalingSource 3) with
      | .ok stream => surfaceTokensEq stream.parserView.tokens.toList (scalingView 3).tokens.toList
      | .error _ => false) = true := by decide +kernel
  cases hlex : Lex.lexStream (scalingSource 3) with
  | error _ => rw [hlex] at hok; cases hok
  | ok stream =>
    rw [hlex] at hok
    have hlist := surfaceTokensEq_sound hok
    exact ⟨stream, rfl, Array.ext' hlist⟩

-- Constant sparse table lookups exercised along `scalingView n`.
private theorem lk_program_fn :
    Parse.KernelEval.earlyTreeLookup (earlyNt .program) (some .fn) = some ProductionId.program.ctorIdx := by
  decide +kernel
private theorem lk_structs_fn :
    Parse.KernelEval.earlyTreeLookup (earlyNt .structs) (some .fn) = some ProductionId.endDecls.ctorIdx := by
  decide +kernel
private theorem lk_returnType_u8 :
    Parse.KernelEval.earlyTreeLookup (earlyNt .returnType) (some .u8) = some ProductionId.retU8.ctorIdx := by
  decide +kernel
private theorem lk_body_let :
    Parse.KernelEval.earlyTreeLookup (earlyNt .body) (some .let_) = some ProductionId.binding.ctorIdx := by
  decide +kernel
private theorem lk_letHead_ident :
    Parse.KernelEval.earlyTreeLookup (earlyNt .letHead) (some .ident) = some ProductionId.named.ctorIdx := by
  decide +kernel
private theorem lk_letTail_colon :
    Parse.KernelEval.earlyTreeLookup (earlyNt .letTail) (some .colon) = some ProductionId.annotated.ctorIdx := by
  decide +kernel
private theorem lk_localType_u8 :
    Parse.KernelEval.earlyTreeLookup (earlyNt .localType) (some .u8) = some ProductionId.localU8.ctorIdx := by
  decide +kernel
private theorem lk_expr_int :
    Parse.KernelEval.earlyTreeLookup (earlyNt .expr) (some .integer) = some ProductionId.expr.ctorIdx := by
  decide +kernel
private theorem lk_product_int :
    Parse.KernelEval.earlyTreeLookup (earlyNt .product) (some .integer) = some ProductionId.product.ctorIdx := by
  decide +kernel
private theorem lk_atom_int :
    Parse.KernelEval.earlyTreeLookup (earlyNt .atom) (some .integer) = some ProductionId.integer.ctorIdx := by
  decide +kernel
private theorem lk_productTail_semi :
    Parse.KernelEval.earlyTreeLookup (earlyNt .productTail) (some .semi) = some ProductionId.endProduct.ctorIdx := by
  decide +kernel
private theorem lk_sumTail_semi :
    Parse.KernelEval.earlyTreeLookup (earlyNt .sumTail) (some .semi) = some ProductionId.endSum.ctorIdx := by
  decide +kernel
private theorem lk_body_return :
    Parse.KernelEval.earlyTreeLookup (earlyNt .body) (some .return_) = some ProductionId.retStmt.ctorIdx := by
  decide +kernel
private theorem lk_expr_semi :
    Parse.KernelEval.earlyTreeLookup (earlyNt .expr) (some .semi) = none := by
  decide +kernel

private theorem earlyProductions_getElem? (id : ProductionId) :
    earlySharedGrammar.productions[id.ctorIdx]? = some (earlyProduction id) := by
  cases id <;> rfl

private theorem require_slice0 (c : ParseCursor) (cause : UnsupportedCause) (span : FileSpan) :
    c.require slice0 slice0 cause span = c := by
  cases c with
  | mk off req hit steps =>
    unfold ParseCursor.require
    have hlt : ¬ (slice0.val < slice0.val) := Nat.lt_irrefl _
    have hle : ¬ (req.val < slice0.val) := Nat.not_lt_zero _
    cases hit <;> simp only [hlt, hle, ↓reduceIte]

private theorem parseSymbolSparse_step_ok (tokens : Array SurfaceToken) (eof : FileSpan)
    (id : ProductionId) (nt : ConcreteSpec.GrammarData.Nonterminal) (hlhs : id.row.lhs = nt)
    (off s off' s' sFinal fuel fuel' : Nat) (hfuel : fuel = fuel' + 1)
    (hlk : Parse.KernelEval.earlyTreeLookup (earlyNt nt)
      (tokens[off]?.map fun t => t.value.kind) = some id.ctorIdx)
    (hfs : earlyProductionFirstSlice id = slice0)
    (args : ActionArgs earlySharedGrammar.value (earlyRhs id))
    (hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof id fuel' (earlyRhs id)
      ⟨off, slice0, none, s + 1⟩ = (⟨off', slice0, none, s'⟩, .ok args))
    (hs : s' + (earlyAction id).cost = sFinal) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt nt)
        ⟨off, slice0, none, s⟩ fuel = (⟨off', slice0, none, sFinal⟩, .ok v) := by
  subst hfuel
  subst hlhs
  subst hs
  refine ⟨((earlyProduction id).action.eval args).2, ?_⟩
  conv => lhs; unfold Parse.KernelEval.parseSymbolSparse
  dsimp only
  rw [hlk]
  dsimp only
  rw [earlyProductions_getElem? id]
  dsimp only [earlyProduction]
  rw [hfs, require_slice0]
  simp only [↓reduceDIte]
  rw [hargs]
  dsimp only
  exact congrArg
    (fun k => (ParseCursor.mk off' slice0 none (s' + k),
      Except.ok ((earlyAction id).eval args).2))
    (Action.eval_cost args (earlyAction id))

private theorem parseSymbolSparse_step_err (tokens : Array SurfaceToken) (eof : FileSpan)
    (id : ProductionId) (nt : ConcreteSpec.GrammarData.Nonterminal) (hlhs : id.row.lhs = nt)
    (off s : Nat) (c' : ParseCursor) (fuel fuel' : Nat) (hfuel : fuel = fuel' + 1)
    (hlk : Parse.KernelEval.earlyTreeLookup (earlyNt nt)
      (tokens[off]?.map fun t => t.value.kind) = some id.ctorIdx)
    (hfs : earlyProductionFirstSlice id = slice0)
    (stop : ParseStop)
    (hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof id fuel' (earlyRhs id)
      ⟨off, slice0, none, s + 1⟩ = (c', .error stop)) :
    Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt nt)
      ⟨off, slice0, none, s⟩ fuel = (c', .error stop) := by
  subst hfuel
  subst hlhs
  conv => lhs; unfold Parse.KernelEval.parseSymbolSparse
  dsimp only
  rw [hlk]
  dsimp only
  rw [earlyProductions_getElem? id]
  dsimp only [earlyProduction]
  rw [hfs, require_slice0]
  simp only [↓reduceDIte]
  rw [hargs]

private theorem parseArgsSparse_tok (tokens : Array SurfaceToken) (eof : FileSpan)
    (prodId : ProductionId) (kind : Terminal)
    (rest : List (Concrete.LL1.Sym Terminal (Fin ConcreteSpec.GrammarData.Nonterminal.count)))
    (off s fuel fuel' : Nat) (hfuel : fuel = fuel' + 1) {tok : SurfaceToken}
    (htok : tokens[off]? = some tok) (hkind : tok.value.kind = kind)
    (hreq : earlySharedGrammar.required tok.value = slice0) :
    Parse.KernelEval.parseArgsSparse tokens slice0 eof prodId fuel (.t kind :: rest)
      ⟨off, slice0, none, s⟩ =
      let (c', args) := Parse.KernelEval.parseArgsSparse tokens slice0 eof prodId fuel' rest
        ⟨off + 1, slice0, none, s + 1⟩
      (c', args.map fun args => (tok, args)) := by
  subst hfuel
  conv => lhs; unfold Parse.KernelEval.parseArgsSparse
  dsimp only
  rw [htok]
  dsimp only
  simp only [hkind, ne_eq, not_true_eq_false, ↓reduceIte, hreq, require_slice0]

private theorem parseArgsSparse_nt_ok (tokens : Array SurfaceToken) (eof : FileSpan)
    (prodId : ProductionId) (n : Fin ConcreteSpec.GrammarData.Nonterminal.count)
    (rest : List (Concrete.LL1.Sym Terminal (Fin ConcreteSpec.GrammarData.Nonterminal.count)))
    (off s : Nat) (c₁ : ParseCursor) (fuel fuel' : Nat) (hfuel : fuel = fuel' + 1)
    (child : earlySharedGrammar.value n)
    (hchild : Parse.KernelEval.parseSymbolSparse tokens slice0 eof n
      ⟨off, slice0, none, s + 1⟩ fuel' = (c₁, .ok child)) :
    Parse.KernelEval.parseArgsSparse tokens slice0 eof prodId fuel (.n n :: rest)
      ⟨off, slice0, none, s⟩ =
      let (c₂, args) := Parse.KernelEval.parseArgsSparse tokens slice0 eof prodId fuel' rest c₁
      (c₂, args.map fun args => (child, args)) := by
  subst hfuel
  conv => lhs; unfold Parse.KernelEval.parseArgsSparse
  dsimp only
  rw [hchild]

private theorem parseArgsSparse_nt_err (tokens : Array SurfaceToken) (eof : FileSpan)
    (prodId : ProductionId) (n : Fin ConcreteSpec.GrammarData.Nonterminal.count)
    (rest : List (Concrete.LL1.Sym Terminal (Fin ConcreteSpec.GrammarData.Nonterminal.count)))
    (off s : Nat) (c₁ : ParseCursor) (fuel fuel' : Nat) (hfuel : fuel = fuel' + 1) (stop : ParseStop)
    (hchild : Parse.KernelEval.parseSymbolSparse tokens slice0 eof n
      ⟨off, slice0, none, s + 1⟩ fuel' = (c₁, .error stop)) :
    Parse.KernelEval.parseArgsSparse tokens slice0 eof prodId fuel (.n n :: rest)
      ⟨off, slice0, none, s⟩ = (c₁, .error stop) := by
  subst hfuel
  conv => lhs; unfold Parse.KernelEval.parseArgsSparse
  dsimp only
  rw [hchild]

private theorem parseStructs_endDecls (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {tok : SurfaceToken} (h0 : tokens[off]? = some tok)
    (hk0 : tok.value.kind = .fn) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .structs)
        ⟨off, slice0, none, s⟩ (f + 2) = (⟨off, slice0, none, s + 4⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .endDecls (f + 1) (earlyRhs .endDecls)
      ⟨off, slice0, none, s + 1⟩ = (⟨off, slice0, none, s + 1 + 1⟩, .ok ()) := rfl
  exact parseSymbolSparse_step_ok tokens eof .endDecls .structs rfl
    off s off (s + 1 + 1) (s + 4) (f + 2) (f + 1) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_structs_fn) rfl () hargs
    (by change s + 1 + 1 + 2 = s + 4; omega)

private theorem parseReturnType_u8 (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {tok : SurfaceToken} (h0 : tokens[off]? = some tok)
    (hk0 : tok.value.kind = .u8) (hreq : earlySharedGrammar.required tok.value = slice0) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .returnType)
        ⟨off, slice0, none, s⟩ (f + 3) = (⟨off + 1, slice0, none, s + 6⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .retU8 (f + 2) (earlyRhs .retU8)
      ⟨off, slice0, none, s + 1⟩ = (⟨off + 1, slice0, none, s + 1 + 1 + 1⟩, .ok (tok, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .retU8 (f + 2) [.t .u8] _ = _
    rw [parseArgsSparse_tok tokens eof .retU8 .u8 [] off (s + 1) (f + 2) (f + 1) rfl h0 hk0 hreq]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .retU8 .returnType rfl
    off s (off + 1) (s + 1 + 1 + 1) (s + 6) (f + 3) (f + 2) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_returnType_u8) rfl (tok, ()) hargs
    (by change s + 1 + 1 + 1 + 3 = s + 6; omega)

private theorem parseLocalType_u8 (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {tok : SurfaceToken} (h0 : tokens[off]? = some tok)
    (hk0 : tok.value.kind = .u8) (hreq : earlySharedGrammar.required tok.value = slice0) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .localType)
        ⟨off, slice0, none, s⟩ (f + 3) = (⟨off + 1, slice0, none, s + 6⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .localU8 (f + 2) (earlyRhs .localU8)
      ⟨off, slice0, none, s + 1⟩ = (⟨off + 1, slice0, none, s + 1 + 1 + 1⟩, .ok (tok, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .localU8 (f + 2) [.t .u8] _ = _
    rw [parseArgsSparse_tok tokens eof .localU8 .u8 [] off (s + 1) (f + 2) (f + 1) rfl h0 hk0 hreq]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .localU8 .localType rfl
    off s (off + 1) (s + 1 + 1 + 1) (s + 6) (f + 3) (f + 2) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_localType_u8) rfl (tok, ()) hargs
    (by change s + 1 + 1 + 1 + 3 = s + 6; omega)

private theorem parseLetTail_u8 (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 t1 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .colon)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .u8)
    (hr1 : earlySharedGrammar.required t1.value = slice0) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .letTail)
        ⟨off, slice0, none, s⟩ (f + 6) = (⟨off + 2, slice0, none, s + 16⟩, .ok v) := by
  obtain ⟨vLoc, hvLoc⟩ := parseLocalType_u8 tokens eof (off + 1) (s + 1 + 1 + 1) f h1 hk1 hr1
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .annotated (f + 5) (earlyRhs .annotated)
      ⟨off, slice0, none, s + 1⟩ =
      (⟨off + 2, slice0, none, s + 1 + 1 + 1 + 6 + 1⟩, .ok (t0, vLoc, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .annotated (f + 5)
      [.t .colon, .n (earlyNt .localType)] _ = _
    rw [parseArgsSparse_tok tokens eof .annotated .colon _ off (s + 1) (f + 5) (f + 4) rfl h0 hk0 hr0,
      parseArgsSparse_nt_ok tokens eof .annotated (earlyNt .localType) [] (off + 1) (s + 1 + 1) _ (f + 4) (f + 3) rfl vLoc hvLoc]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .annotated .letTail rfl
    off s (off + 2) (s + 1 + 1 + 1 + 6 + 1) (s + 16) (f + 6) (f + 5) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_letTail_colon) rfl (t0, vLoc, ()) hargs
    (by change s + 1 + 1 + 1 + 6 + 1 + 6 = s + 16; omega)

private theorem parseLetHead_u8 (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 t1 t2 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .ident)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .colon)
    (hr1 : earlySharedGrammar.required t1.value = slice0)
    (h2 : tokens[off + 2]? = some t2) (hk2 : t2.value.kind = .u8)
    (hr2 : earlySharedGrammar.required t2.value = slice0) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .letHead)
        ⟨off, slice0, none, s⟩ (f + 9) = (⟨off + 3, slice0, none, s + 30⟩, .ok v) := by
  obtain ⟨vTail, hvTail⟩ := parseLetTail_u8 tokens eof (off + 1) (s + 1 + 1 + 1) f
    h1 hk1 hr1 h2 hk2 hr2
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .named (f + 8) (earlyRhs .named)
      ⟨off, slice0, none, s + 1⟩ =
      (⟨off + 3, slice0, none, s + 1 + 1 + 1 + 16 + 1⟩, .ok (t0, vTail, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .named (f + 8)
      [.t .ident, .n (earlyNt .letTail)] _ = _
    rw [parseArgsSparse_tok tokens eof .named .ident _ off (s + 1) (f + 8) (f + 7) rfl h0 hk0 hr0,
      parseArgsSparse_nt_ok tokens eof .named (earlyNt .letTail) [] (off + 1) (s + 1 + 1) _ (f + 7) (f + 6) rfl vTail hvTail]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .named .letHead rfl
    off s (off + 3) (s + 1 + 1 + 1 + 16 + 1) (s + 30) (f + 9) (f + 8) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_letHead_ident) rfl (t0, vTail, ()) hargs
    (by change s + 1 + 1 + 1 + 16 + 1 + 10 = s + 30; omega)

private theorem parseAtom_int (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .integer)
    (hr0 : earlySharedGrammar.required t0.value = slice0) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .atom)
        ⟨off, slice0, none, s⟩ (f + 3) = (⟨off + 1, slice0, none, s + 6⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .integer (f + 2) (earlyRhs .integer)
      ⟨off, slice0, none, s + 1⟩ = (⟨off + 1, slice0, none, s + 1 + 1 + 1⟩, .ok (t0, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .integer (f + 2) [.t .integer] _ = _
    rw [parseArgsSparse_tok tokens eof .integer .integer [] off (s + 1) (f + 2) (f + 1) rfl h0 hk0 hr0]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .integer .atom rfl
    off s (off + 1) (s + 1 + 1 + 1) (s + 6) (f + 3) (f + 2) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_atom_int) rfl (t0, ()) hargs
    (by change s + 1 + 1 + 1 + 3 = s + 6; omega)

private theorem parseProductTail_semi (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .semi) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .productTail)
        ⟨off, slice0, none, s⟩ (f + 2) = (⟨off, slice0, none, s + 4⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .endProduct (f + 1) (earlyRhs .endProduct)
      ⟨off, slice0, none, s + 1⟩ = (⟨off, slice0, none, s + 1 + 1⟩, .ok ()) := rfl
  exact parseSymbolSparse_step_ok tokens eof .endProduct .productTail rfl
    off s off (s + 1 + 1) (s + 4) (f + 2) (f + 1) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_productTail_semi) rfl () hargs
    (by change s + 1 + 1 + 2 = s + 4; omega)

private theorem parseProduct_int_semi (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 t1 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .integer)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .semi) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .product)
        ⟨off, slice0, none, s⟩ (f + 5) = (⟨off + 1, slice0, none, s + 23⟩, .ok v) := by
  obtain ⟨vAtom, hvAtom⟩ := parseAtom_int tokens eof off (s + 1 + 1) f h0 hk0 hr0
  obtain ⟨vPT, hvPT⟩ := parseProductTail_semi tokens eof (off + 1) (s + 1 + 1 + 6 + 1) f h1 hk1
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .product (f + 4) (earlyRhs .product)
      ⟨off, slice0, none, s + 1⟩ =
      (⟨off + 1, slice0, none, s + 1 + 1 + 6 + 1 + 4 + 1⟩, .ok (vAtom, vPT, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .product (f + 4)
      [.n (earlyNt .atom), .n (earlyNt .productTail)] _ = _
    rw [parseArgsSparse_nt_ok tokens eof .product (earlyNt .atom) _ off (s + 1) _ (f + 4) (f + 3) rfl vAtom hvAtom,
      parseArgsSparse_nt_ok tokens eof .product (earlyNt .productTail) [] (off + 1) (s + 1 + 1 + 6) _ (f + 3) (f + 2) rfl vPT hvPT]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .product .product rfl
    off s (off + 1) (s + 1 + 1 + 6 + 1 + 4 + 1) (s + 23) (f + 5) (f + 4) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_product_int) rfl (vAtom, vPT, ()) hargs
    (by change s + 1 + 1 + 6 + 1 + 4 + 1 + 9 = s + 23; omega)

private theorem parseSumTail_semi (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .semi) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .sumTail)
        ⟨off, slice0, none, s⟩ (f + 2) = (⟨off, slice0, none, s + 4⟩, .ok v) := by
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .endSum (f + 1) (earlyRhs .endSum)
      ⟨off, slice0, none, s + 1⟩ = (⟨off, slice0, none, s + 1 + 1⟩, .ok ()) := rfl
  exact parseSymbolSparse_step_ok tokens eof .endSum .sumTail rfl
    off s off (s + 1 + 1) (s + 4) (f + 2) (f + 1) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_sumTail_semi) rfl () hargs
    (by change s + 1 + 1 + 2 = s + 4; omega)

private theorem parseExpr_int_semi (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 t1 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .integer)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .semi) :
    ∃ v, Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .expr)
        ⟨off, slice0, none, s⟩ (f + 7) = (⟨off + 1, slice0, none, s + 40⟩, .ok v) := by
  obtain ⟨vProd, hvProd⟩ := parseProduct_int_semi tokens eof off (s + 1 + 1) f h0 hk0 hr0 h1 hk1
  obtain ⟨vST, hvST⟩ := parseSumTail_semi tokens eof (off + 1) (s + 1 + 1 + 23 + 1) (f + 2) h1 hk1
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .expr (f + 6) (earlyRhs .expr)
      ⟨off, slice0, none, s + 1⟩ =
      (⟨off + 1, slice0, none, s + 1 + 1 + 23 + 1 + 4 + 1⟩, .ok (vProd, vST, ())) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .expr (f + 6)
      [.n (earlyNt .product), .n (earlyNt .sumTail)] _ = _
    rw [parseArgsSparse_nt_ok tokens eof .expr (earlyNt .product) _ off (s + 1) _ (f + 6) (f + 5) rfl vProd hvProd,
      parseArgsSparse_nt_ok tokens eof .expr (earlyNt .sumTail) [] (off + 1) (s + 1 + 1 + 23) _ (f + 5) (f + 4) rfl vST hvST]
    rfl
  exact parseSymbolSparse_step_ok tokens eof .expr .expr rfl
    off s (off + 1) (s + 1 + 1 + 23 + 1 + 4 + 1) (s + 40) (f + 7) (f + 6) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_expr_int) rfl (vProd, vST, ()) hargs
    (by change s + 1 + 1 + 23 + 1 + 4 + 1 + 9 = s + 40; omega)

private theorem parseBody_binding_malformed (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) (cEnd : ParseCursor)
    {t0 t1 t2 t3 t4 t5 t6 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .let_)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .ident)
    (hr1 : earlySharedGrammar.required t1.value = slice0)
    (h2 : tokens[off + 2]? = some t2) (hk2 : t2.value.kind = .colon)
    (hr2 : earlySharedGrammar.required t2.value = slice0)
    (h3 : tokens[off + 3]? = some t3) (hk3 : t3.value.kind = .u8)
    (hr3 : earlySharedGrammar.required t3.value = slice0)
    (h4 : tokens[off + 4]? = some t4) (hk4 : t4.value.kind = .equal)
    (hr4 : earlySharedGrammar.required t4.value = slice0)
    (h5 : tokens[off + 5]? = some t5) (hk5 : t5.value.kind = .integer)
    (hr5 : earlySharedGrammar.required t5.value = slice0)
    (h6 : tokens[off + 6]? = some t6) (hk6 : t6.value.kind = .semi)
    (hr6 : earlySharedGrammar.required t6.value = slice0)
    (htail : Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .body)
      ⟨off + 7, slice0, none, s + 77⟩ (f + 5) = (cEnd, .error .malformed)) :
    Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .body)
      ⟨off, slice0, none, s⟩ (f + 12) = (cEnd, .error .malformed) := by
  obtain ⟨vHead, hvHead⟩ := parseLetHead_u8 tokens eof (off + 1) (s + 1 + 1 + 1) f
    h1 hk1 hr1 h2 hk2 hr2 h3 hk3 hr3
  obtain ⟨vExpr, hvExpr⟩ := parseExpr_int_semi tokens eof (off + 1 + 3 + 1)
    (s + 1 + 1 + 1 + 30 + 1 + 1) f
    (show tokens[off + 1 + 3 + 1]? = some t5 by simpa [show off + 1 + 3 + 1 = off + 5 by omega] using h5)
    hk5 hr5
    (show tokens[off + 1 + 3 + 1 + 1]? = some t6 by simpa [show off + 1 + 3 + 1 + 1 = off + 6 by omega] using h6)
    hk6
  have hcur : ParseCursor.mk (off + 1 + 3 + 1 + 1 + 1) slice0 none
      (s + 1 + 1 + 1 + 30 + 1 + 1 + 40 + 1 + 1) =
      ⟨off + 7, slice0, none, s + 77⟩ := by congr 1 <;> omega
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .binding (f + 11) (earlyRhs .binding)
      ⟨off, slice0, none, s + 1⟩ = (cEnd, .error .malformed) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .binding (f + 11)
      [.t .let_, .n (earlyNt .letHead), .t .equal, .n (earlyNt .expr), .t .semi, .n (earlyNt .body)] _ = _
    rw [parseArgsSparse_tok tokens eof .binding .let_ _ off (s + 1) (f + 11) (f + 10) rfl h0 hk0 hr0,
      parseArgsSparse_nt_ok tokens eof .binding (earlyNt .letHead) _ (off + 1) (s + 1 + 1) _ (f + 10) (f + 9) rfl vHead hvHead,
      parseArgsSparse_tok tokens eof .binding .equal _ (off + 1 + 3) (s + 1 + 1 + 1 + 30) (f + 9) (f + 8) rfl
        (show tokens[off + 1 + 3]? = some t4 by simpa [show off + 1 + 3 = off + 4 by omega] using h4)
        hk4 hr4,
      parseArgsSparse_nt_ok tokens eof .binding (earlyNt .expr) _ (off + 1 + 3 + 1) (s + 1 + 1 + 1 + 30 + 1) _ (f + 8) (f + 7) rfl vExpr hvExpr,
      parseArgsSparse_tok tokens eof .binding .semi _ (off + 1 + 3 + 1 + 1) (s + 1 + 1 + 1 + 30 + 1 + 1 + 40) (f + 7) (f + 6) rfl
        (show tokens[off + 1 + 3 + 1 + 1]? = some t6 by simpa [show off + 1 + 3 + 1 + 1 = off + 6 by omega] using h6)
        hk6 hr6,
      parseArgsSparse_nt_err tokens eof .binding (earlyNt .body) [] (off + 1 + 3 + 1 + 1 + 1) (s + 1 + 1 + 1 + 30 + 1 + 1 + 40 + 1) cEnd (f + 6) (f + 5) rfl .malformed
        (by rw [hcur]; exact htail)]
    rfl
  exact parseSymbolSparse_step_err tokens eof .binding .body rfl
    off s cEnd (f + 12) (f + 11) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_body_let) rfl .malformed hargs

private theorem parseBody_retSemi_malformed (tokens : Array SurfaceToken) (eof : FileSpan)
    (off s f : Nat) {t0 t1 : SurfaceToken}
    (h0 : tokens[off]? = some t0) (hk0 : t0.value.kind = .return_)
    (hr0 : earlySharedGrammar.required t0.value = slice0)
    (h1 : tokens[off + 1]? = some t1) (hk1 : t1.value.kind = .semi) :
    Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .body)
      ⟨off, slice0, none, s⟩ (f + 4) = (⟨off + 1, slice0, none, s + 4⟩, .error .malformed) := by
  have hexpr : Parse.KernelEval.parseSymbolSparse tokens slice0 eof (earlyNt .expr)
      ⟨off + 1, slice0, none, s + 1 + 1 + 1⟩ (f + 1) =
      (⟨off + 1, slice0, none, s + 4⟩, .error .malformed) := by
    unfold Parse.KernelEval.parseSymbolSparse
    simp only [h1, Option.map_some, hk1, lk_expr_semi]
  have hargs : Parse.KernelEval.parseArgsSparse tokens slice0 eof .retStmt (f + 3) (earlyRhs .retStmt)
      ⟨off, slice0, none, s + 1⟩ = (⟨off + 1, slice0, none, s + 4⟩, .error .malformed) := by
    change Parse.KernelEval.parseArgsSparse tokens slice0 eof .retStmt (f + 3)
      [.t .return_, .n (earlyNt .expr), .t .semi, .n (earlyNt .body)] _ = _
    rw [parseArgsSparse_tok tokens eof .retStmt .return_ _ off (s + 1) (f + 3) (f + 2) rfl h0 hk0 hr0,
      parseArgsSparse_nt_err tokens eof .retStmt (earlyNt .expr) _ (off + 1) (s + 1 + 1) _ (f + 2) (f + 1) rfl .malformed hexpr]
    rfl
  exact parseSymbolSparse_step_err tokens eof .retStmt .body rfl
    off s ⟨off + 1, slice0, none, s + 4⟩ (f + 4) (f + 3) rfl
    (by simp only [h0, Option.map_some, hk0]; exact lk_body_return) rfl .malformed hargs

private theorem list_getElem?_append_right_eq {α : Type} (xs ys : List α) (k : Nat) :
    (xs ++ ys)[xs.length + k]? = ys[k]? := by
  rw [List.getElem?_append_right (Nat.le_add_right _ _)]
  simp only [Nat.add_sub_cancel_left]

private theorem scalingBindingsTokens_getElem? (k base j : Nat) (hj : j < k) :
    ∃ b,
      (scalingBindingsTokens k base)[7 * j]? = some ⟨mkSpan1 b (b + 3), .fixed .let_ rfl⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 1]? = some ⟨mkSpan1 (b + 4) (b + 5), .name "x"⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 2]? = some ⟨mkSpan1 (b + 5) (b + 6), .fixed .colon rfl⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 3]? = some ⟨mkSpan1 (b + 7) (b + 9), .fixed .u8 rfl⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 4]? = some ⟨mkSpan1 (b + 10) (b + 11), .fixed .equal rfl⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 5]? = some ⟨mkSpan1 (b + 12) (b + 13), .integer 0 .decimal none⟩ ∧
      (scalingBindingsTokens k base)[7 * j + 6]? = some ⟨mkSpan1 (b + 13) (b + 14), .fixed .semi rfl⟩ := by
  induction k generalizing base j with
  | zero => exact absurd hj (Nat.not_lt_zero _)
  | succ k ih =>
    cases j with
    | zero =>
      refine ⟨base, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;> rfl
    | succ j' =>
      obtain ⟨b, h0, h1, h2, h3, h4, h5, h6⟩ := ih (base + 15) j' (by omega)
      refine ⟨b, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) = (scalingBindingTokens base).length + 7 * j' from by
              change 7 * (j' + 1) = 7 + 7 * j'; omega,
            list_getElem?_append_right_eq]; exact h0
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 1 = (scalingBindingTokens base).length + (7 * j' + 1) from by
              change 7 * (j' + 1) + 1 = 7 + (7 * j' + 1); omega,
            list_getElem?_append_right_eq]; exact h1
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 2 = (scalingBindingTokens base).length + (7 * j' + 2) from by
              change 7 * (j' + 1) + 2 = 7 + (7 * j' + 2); omega,
            list_getElem?_append_right_eq]; exact h2
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 3 = (scalingBindingTokens base).length + (7 * j' + 3) from by
              change 7 * (j' + 1) + 3 = 7 + (7 * j' + 3); omega,
            list_getElem?_append_right_eq]; exact h3
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 4 = (scalingBindingTokens base).length + (7 * j' + 4) from by
              change 7 * (j' + 1) + 4 = 7 + (7 * j' + 4); omega,
            list_getElem?_append_right_eq]; exact h4
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 5 = (scalingBindingTokens base).length + (7 * j' + 5) from by
              change 7 * (j' + 1) + 5 = 7 + (7 * j' + 5); omega,
            list_getElem?_append_right_eq]; exact h5
      · simp only [scalingBindingsTokens]
        rw [show 7 * (j' + 1) + 6 = (scalingBindingTokens base).length + (7 * j' + 6) from by
              change 7 * (j' + 1) + 6 = 7 + (7 * j' + 6); omega,
            list_getElem?_append_right_eq]; exact h6

private theorem scalingView_binding_at (n j : Nat) (hj : j < n) :
    ∃ b,
      (scalingView n).tokens[7 + 7 * j]? = some ⟨mkSpan1 b (b + 3), .fixed .let_ rfl⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 1]? = some ⟨mkSpan1 (b + 4) (b + 5), .name "x"⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 2]? = some ⟨mkSpan1 (b + 5) (b + 6), .fixed .colon rfl⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 3]? = some ⟨mkSpan1 (b + 7) (b + 9), .fixed .u8 rfl⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 4]? = some ⟨mkSpan1 (b + 10) (b + 11), .fixed .equal rfl⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 5]? = some ⟨mkSpan1 (b + 12) (b + 13), .integer 0 .decimal none⟩ ∧
      (scalingView n).tokens[7 + 7 * j + 6]? = some ⟨mkSpan1 (b + 13) (b + 14), .fixed .semi rfl⟩ := by
  obtain ⟨b, h0, h1, h2, h3, h4, h5, h6⟩ := scalingBindingsTokens_getElem? n 18 j hj
  have hlen : (scalingPrefixTokens ++ scalingBindingsTokens n 18).length = 7 + 7 * n := by
    simp only [List.length_append, scalingBindingsTokens_length]; rfl
  refine ⟨b, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    simp only [scalingView, scalingTokenList, List.getElem?_toArray]
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j = scalingPrefixTokens.length + 7 * j from rfl,
      list_getElem?_append_right_eq]; exact h0
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 1 = scalingPrefixTokens.length + (7 * j + 1) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h1
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 2 = scalingPrefixTokens.length + (7 * j + 2) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h2
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 3 = scalingPrefixTokens.length + (7 * j + 3) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h3
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 4 = scalingPrefixTokens.length + (7 * j + 4) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h4
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 5 = scalingPrefixTokens.length + (7 * j + 5) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h5
  · rw [List.getElem?_append_left (by omega),
      show 7 + 7 * j + 6 = scalingPrefixTokens.length + (7 * j + 6) by change _ = 7 + _; omega,
      list_getElem?_append_right_eq]; exact h6

private theorem scalingView_suffix_at (n : Nat) :
    (scalingView n).tokens[7 + 7 * n]? =
      some ⟨mkSpan1 (18 + 15 * n) (18 + 15 * n + 6), .fixed .return_ rfl⟩ ∧
    (scalingView n).tokens[7 + 7 * n + 1]? =
      some ⟨mkSpan1 (18 + 15 * n + 7) (18 + 15 * n + 8), .fixed .semi rfl⟩ := by
  have hlen : (scalingPrefixTokens ++ scalingBindingsTokens n 18).length = 7 + 7 * n := by
    simp only [List.length_append, scalingBindingsTokens_length]; rfl
  constructor
  · simp only [scalingView, scalingTokenList, List.getElem?_toArray]
    rw [show 7 + 7 * n = (scalingPrefixTokens ++ scalingBindingsTokens n 18).length + 0 by omega,
      list_getElem?_append_right_eq]; rfl
  · simp only [scalingView, scalingTokenList, List.getElem?_toArray]
    rw [show 7 + 7 * n + 1 = (scalingPrefixTokens ++ scalingBindingsTokens n 18).length + 1 by omega,
      list_getElem?_append_right_eq]; rfl

private theorem req_let : earlySharedGrammar.required (.fixed .let_ rfl) = slice0 := by decide
private theorem req_ident_x : earlySharedGrammar.required (.name "x") = slice0 := by decide
private theorem req_colon : earlySharedGrammar.required (.fixed .colon rfl) = slice0 := by decide
private theorem req_u8 : earlySharedGrammar.required (.fixed .u8 rfl) = slice0 := by decide
private theorem req_equal : earlySharedGrammar.required (.fixed .equal rfl) = slice0 := by decide
private theorem req_int0 : earlySharedGrammar.required (.integer 0 .decimal none) = slice0 := by decide
private theorem req_semi : earlySharedGrammar.required (.fixed .semi rfl) = slice0 := by decide
private theorem req_return : earlySharedGrammar.required (.fixed .return_ rfl) = slice0 := by decide

private theorem parseBody_scaling_loop (n : Nat) (eof : FileSpan) :
    ∀ k j s f, j + k = n →
      Parse.KernelEval.parseSymbolSparse (scalingView n).tokens slice0 eof (earlyNt .body)
        ⟨7 + 7 * j, slice0, none, s⟩ (f + 7 * k + 5) =
        (⟨7 + 7 * n + 1, slice0, none, s + 77 * k + 4⟩, .error .malformed) := by
  intro k
  induction k with
  | zero =>
    intro j s f hj
    have hjn : j = n := by omega
    rw [hjn]
    obtain ⟨hret, hsemi⟩ := scalingView_suffix_at n
    have h := parseBody_retSemi_malformed (scalingView n).tokens eof (7 + 7 * n) s (f + 1)
      hret rfl req_return hsemi rfl
    simpa only [Nat.mul_zero, Nat.add_zero] using h
  | succ k ih =>
    intro j s f hj
    obtain ⟨b, h0, h1, h2, h3, h4, h5, h6⟩ := scalingView_binding_at n j (by omega)
    have htail := ih (j + 1) (s + 77) f (by omega)
    have hstep := parseBody_binding_malformed (scalingView n).tokens eof (7 + 7 * j) s (f + 7 * k)
      ⟨7 + 7 * n + 1, slice0, none, s + 77 + 77 * k + 4⟩
      h0 rfl req_let h1 rfl req_ident_x h2 rfl req_colon h3 rfl req_u8
      h4 rfl req_equal h5 rfl req_int0 h6 rfl req_semi
      (by simpa only [show 7 + 7 * (j + 1) = 7 + 7 * j + 7 by omega] using htail)
    have hfuel : f + 7 * (k + 1) + 5 = f + 7 * k + 12 := by omega
    have hsteps : s + 77 + 77 * k + 4 = s + 77 * (k + 1) + 4 := by omega
    rw [hfuel, hstep, hsteps]

private theorem parseProgram_scaling (n : Nat) (eof : FileSpan) (f : Nat) :
    Parse.KernelEval.parseSymbolSparse (scalingView n).tokens slice0 eof (earlyNt .program)
      ⟨0, slice0, none, 0⟩ (f + 7 * n + 15) =
      (⟨7 + 7 * n + 1, slice0, none, 24 + 77 * n⟩, .error .malformed) := by
  have h0 : (scalingView n).tokens[0]? = some ⟨mkSpan1 0 2, .fixed .fn rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h1 : (scalingView n).tokens[1]? = some ⟨mkSpan1 3 7, .name "main"⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h2 : (scalingView n).tokens[2]? = some ⟨mkSpan1 7 8, .fixed .lparen rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h3 : (scalingView n).tokens[3]? = some ⟨mkSpan1 8 9, .fixed .rparen rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h4 : (scalingView n).tokens[4]? = some ⟨mkSpan1 10 12, .fixed .arrow rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h5 : (scalingView n).tokens[5]? = some ⟨mkSpan1 13 15, .fixed .u8 rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  have h6 : (scalingView n).tokens[6]? = some ⟨mkSpan1 16 17, .fixed .lbrace rfl⟩ := by
    simp [scalingView, scalingTokenList, scalingPrefixTokens]
  obtain ⟨vStr, hvStr⟩ := parseStructs_endDecls (scalingView n).tokens eof 0 2 (f + 7 * n + 11) h0 rfl
  obtain ⟨vRet, hvRet⟩ := parseReturnType_u8 (scalingView n).tokens eof 5 12 (f + 7 * n + 4) h5 rfl req_u8
  have hbody := parseBody_scaling_loop n eof n 0 20 f (Nat.zero_add n)
  have hargs : Parse.KernelEval.parseArgsSparse (scalingView n).tokens slice0 eof .program (f + 7 * n + 14)
      (earlyRhs .program) ⟨0, slice0, none, 1⟩ =
      (⟨7 + 7 * n + 1, slice0, none, 24 + 77 * n⟩, .error .malformed) := by
    change Parse.KernelEval.parseArgsSparse (scalingView n).tokens slice0 eof .program (f + 7 * n + 14)
      [.n (earlyNt .structs), .t .fn, .t .ident, .t .lparen, .t .rparen, .t .arrow,
       .n (earlyNt .returnType), .t .lbrace, .n (earlyNt .body), .t .rbrace] _ = _
    rw [parseArgsSparse_nt_ok (scalingView n).tokens eof .program (earlyNt .structs) _ 0 1 _ (f + 7 * n + 14) (f + 7 * n + 13) rfl vStr hvStr,
      parseArgsSparse_tok (scalingView n).tokens eof .program .fn _ 0 6 (f + 7 * n + 13) (f + 7 * n + 12) rfl h0 rfl (by decide),
      parseArgsSparse_tok (scalingView n).tokens eof .program .ident _ 1 7 (f + 7 * n + 12) (f + 7 * n + 11) rfl h1 rfl (by decide),
      parseArgsSparse_tok (scalingView n).tokens eof .program .lparen _ 2 8 (f + 7 * n + 11) (f + 7 * n + 10) rfl h2 rfl (by decide),
      parseArgsSparse_tok (scalingView n).tokens eof .program .rparen _ 3 9 (f + 7 * n + 10) (f + 7 * n + 9) rfl h3 rfl (by decide),
      parseArgsSparse_tok (scalingView n).tokens eof .program .arrow _ 4 10 (f + 7 * n + 9) (f + 7 * n + 8) rfl h4 rfl (by decide),
      parseArgsSparse_nt_ok (scalingView n).tokens eof .program (earlyNt .returnType) _ 5 11 _ (f + 7 * n + 8) (f + 7 * n + 7) rfl vRet hvRet,
      parseArgsSparse_tok (scalingView n).tokens eof .program .lbrace _ 6 18 (f + 7 * n + 7) (f + 7 * n + 6) rfl h6 rfl (by decide),
      parseArgsSparse_nt_err (scalingView n).tokens eof .program (earlyNt .body) _ 7 19 _ (f + 7 * n + 6) (f + 7 * n + 5) rfl .malformed
        (by simpa only [Nat.mul_zero, Nat.add_zero, show 20 + 77 * n + 4 = 24 + 77 * n by omega] using hbody)]
    rfl
  exact parseSymbolSparse_step_err (scalingView n).tokens eof .program .program rfl
    0 0 ⟨7 + 7 * n + 1, slice0, none, 24 + 77 * n⟩
    (f + 7 * n + 15) (f + 7 * n + 14) rfl
    (by simp only [h0, Option.map_some]; exact lk_program_fn) rfl .malformed hargs

private theorem foldl_max_ge_loc {α : Type 1} (f : α → Nat) (xs : List α) (acc : Nat) :
    acc ≤ xs.foldl (fun a x => max a (f x)) acc := by
  induction xs generalizing acc with
  | nil => exact Nat.le_refl acc
  | cons x xs ih =>
    simp only [List.foldl_cons]
    exact Nat.le_trans (Nat.le_max_left acc (f x)) (ih (max acc (f x)))

private theorem foldl_max_mem_loc {α : Type 1} (f : α → Nat) (xs : List α) (acc : Nat)
    {x : α} (hx : x ∈ xs) :
    f x ≤ xs.foldl (fun a y => max a (f y)) acc := by
  induction xs generalizing acc with
  | nil => cases hx
  | cons y ys ih =>
    simp only [List.foldl_cons]
    rcases List.mem_cons.mp hx with rfl | hmem
    · exact Nat.le_trans (Nat.le_max_right acc (f x)) (foldl_max_ge_loc f ys _)
    · exact ih (max acc (f y)) hmem

private theorem earlyStepsPerToken_ge_15 : 15 ≤ earlyStepsPerToken := by
  have hmem : earlyProduction .program ∈ earlySharedGrammar.productions.toList := by
    unfold earlySharedGrammar earlyProductions
    simp only [List.mem_map]
    exact ⟨.program, by decide, rfl⟩
  have hfc : (earlyProduction .program).frameCost ≤ earlySharedGrammar.maxFrameCost := by
    unfold SharedGrammar.maxFrameCost
    rw [← Array.foldl_toList]
    exact foldl_max_mem_loc Production.frameCost earlySharedGrammar.productions.toList 1 hmem
  have hprog : 4 ≤ (earlyProduction .program).frameCost := by
    unfold Production.frameCost
    change 4 ≤ 1 + (10 + 1) + (earlyAction .program).cost
    omega
  unfold earlyStepsPerToken linearStepsPerToken earlyMaxRank
  dsimp only
  omega

-- Exact parametric linear scaling theorem for ALL `n : Nat` bindings.
theorem scaling_n_bindings_linear_budget (n : Nat) :
    (scalingView n).WF ∧
    (scalingView n).tokens.size = 7 * n + 10 ∧
    (sliceParser slice0).run (scalingView n) =
      (25 + 77 * n,
        some (.error (.malformed earlyMalformedRule .unexpectedToken
          (mkSpan1 (15 * n + 25) (15 * n + 26))))) ∧
    ((sliceParser slice0).run (scalingView n)).1 ≤
      (sliceParser slice0).stepsPerToken * ((scalingView n).tokens.size + 1) := by
  have hwf := scalingView_wf n
  have hsz := scalingView_tokens_size n
  have hrun : (sliceParser slice0).run (scalingView n) =
      (25 + 77 * n,
        some (.error (.malformed earlyMalformedRule .unexpectedToken
          (mkSpan1 (15 * n + 25) (15 * n + 26))))) := by
    rw [sliceParser_run_eq_sparse_any slice0 (scalingView n)]
    unfold Parse.KernelEval.runEarlySparse Parse.KernelEval.runEarlySparseWith
    have hinv : (scalingView n).invalidLocation = none := hwf
    rw [hinv]
    dsimp only
    rw [hsz]
    have hmul : 15 * (7 * n + 10 + 1) ≤ earlyStepsPerToken * (7 * n + 10 + 1) :=
      Nat.mul_le_mul_right (7 * n + 10 + 1) earlyStepsPerToken_ge_15
    have hbudget : earlyStepsPerToken * (7 * n + 10 + 1) =
        (earlyStepsPerToken * (7 * n + 10 + 1) - (7 * n + 15)) + 7 * n + 15 := by omega
    rw [hbudget]
    have hprog : Parse.KernelEval.parseSymbolSparse (scalingView n).tokens (scalingView n).source.slice
        (eofFileSpan (scalingView n).source) earlySharedGrammar.start
        ⟨0, slice0, none, 0⟩
        (earlyStepsPerToken * (7 * n + 10 + 1) - (7 * n + 15) + 7 * n + 15) =
        (⟨7 + 7 * n + 1, slice0, none, 24 + 77 * n⟩, .error .malformed) :=
      parseProgram_scaling n (eofFileSpan (scalingView n).source)
        (earlyStepsPerToken * (7 * n + 10 + 1) - (7 * n + 15))
    rw [hprog]
    dsimp only
    have hlt : 7 + 7 * n + 1 < 7 * n + 10 := by omega
    have hsemi := (scalingView_suffix_at n).2
    simp only [hsemi, Option.map_some, Option.getD_some, hlt, ↓reduceIte,
      show 18 + 15 * n + 7 = 15 * n + 25 by omega,
      show 18 + 15 * n + 8 = 15 * n + 26 by omega,
      show 24 + 77 * n + 1 = 25 + 77 * n by omega]
    rfl
  exact ⟨hwf, hsz, hrun, sliceParser_steps_le slice0 (scalingView n)⟩

-- Instantiation at `n = 200` bindings (`1410` tokens): exact `Parsed@1 .unexpectedToken` at `;`
-- `[3025, 3026)` (1:3026), exact linear step count `15425`, strictly within
-- `earlyStepsPerToken * (1410 + 1)`, with zero exhaustion (`sliceParser_nonExhausted`).
theorem scaling_200_bindings_linear_budget :
    (scalingView 200).WF ∧
    (scalingView 200).tokens.size = 1410 ∧
    (sliceParser slice0).run (scalingView 200) =
      (15425,
        some (.error (.malformed earlyMalformedRule .unexpectedToken ⟨⟨0⟩, 3025, 3026, 1, 3026⟩))) ∧
    ((sliceParser slice0).run (scalingView 200)).1 ≤
      (sliceParser slice0).stepsPerToken * ((scalingView 200).tokens.size + 1) ∧
    ((sliceParser slice0).run (scalingView 200)).2.isSome = true := by
  obtain ⟨hwf, hsz, hrun, hbound⟩ := scaling_n_bindings_linear_budget 200
  exact ⟨hwf, hsz, hrun, hbound, sliceParser_nonExhausted slice0 (scalingView 200)⟩

/-! ## Decoder-interruption controls (agy 1: all 8 `DecodeReason` variants + leading/embedded BOM & NUL) -/

private def bytesWith (prefixStr : String) (bad : List UInt8) (suffixStr : String) : ByteArray :=
  ⟨prefixStr.toUTF8.data ++ bad.toArray ++ suffixStr.toUTF8.data⟩

-- 1. Leading UTF-8 BOM before a valid program -> `Lexed@2` (`.encoding .bom`) at [0,1) 1:1.
theorem decoder_interruption_1_leading_bom :
    LexRefusalPrecedence (bytesWith "" [0xEF, 0xBB, 0xBF] "fn main() -> u8 { return 42; }")
      (.lexical (.encoding .bom) ⟨0, 1, 1, 1, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 2. Embedded UTF-8 BOM after syntax error `;` -> `Lexed@2` (`.encoding .bom`) at [27,28) 1:28.
theorem decoder_interruption_2_embedded_bom_beats_parse :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xEF, 0xBB, 0xBF] " }")
      (.lexical (.encoding .bom) ⟨27, 28, 1, 28, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 3. Embedded NUL after syntax error `;` -> `Lexed@2` (`.encoding .nul`) at [27,28) 1:28.
theorem decoder_interruption_3_embedded_nul_beats_parse :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0x00] " }")
      (.lexical (.encoding .nul) ⟨27, 28, 1, 28, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 4. Embedded NUL inside comment after overflow `255 + 1;` -> `Lexed@2` (`.encoding .nul`) at [37,38),
-- beating Core `AdditionFits@1`.
theorem decoder_interruption_4_nul_in_comment_beats_core :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return 255 + 1; // " [0x00] "\n}")
      (.lexical (.encoding .nul) ⟨37, 38, 1, 38, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 5. Unexpected continuation byte `0x80` after `;` -> `Lexed@2` (`.encoding .unexpectedContinuation`) at [27,28).
theorem decoder_interruption_5_unexpected_continuation :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0x80] " }")
      (.lexical (.encoding .unexpectedContinuation) ⟨27, 28, 1, 28, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 6. Overlong UTF-8 `0xC0 0x80` after `;` -> `Lexed@2` (`.encoding .overlong`) at [27,28).
theorem decoder_interruption_6_overlong :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xC0, 0x80] " }")
      (.lexical (.encoding .overlong) ⟨27, 28, 1, 28, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 7. Surrogate UTF-8 `0xED 0xA0 0x80` after `;` -> `Lexed@2` (`.encoding .surrogate`) at [28,29).
theorem decoder_interruption_7_surrogate :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xED, 0xA0, 0x80] " }")
      (.lexical (.encoding .surrogate) ⟨28, 29, 1, 29, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 8. Code point > U+10FFFF (`0xF4 0x90 0x80 0x80`) -> `Lexed@2` (`.encoding .tooLarge`) at [28,29).
theorem decoder_interruption_8_too_large :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xF4, 0x90, 0x80, 0x80] " }")
      (.lexical (.encoding .tooLarge) ⟨28, 29, 1, 29, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 9. Invalid continuation `0xC2 0x41` -> `Lexed@2` (`.encoding .invalidContinuation`) at [28,29).
theorem decoder_interruption_9_invalid_continuation :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xC2, 0x41] " }")
      (.lexical (.encoding .invalidContinuation) ⟨28, 29, 1, 29, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

-- 10. Truncated multi-byte UTF-8 `0xE2 0x82` at EOF -> `Lexed@2` (`.encoding .truncated`) at EOF [29,29).
theorem decoder_interruption_10_truncated_eof :
    LexRefusalPrecedence (bytesWith "fn main() -> u8 { return ; " [0xE2, 0x82] "")
      (.lexical (.encoding .truncated) ⟨29, 29, 1, 30, ⟨0⟩⟩)
      ⟨``Slice0Rules.Lexed, 2⟩ :=
  lexRefusalPrecedence_of_sparse
    (by decide +kernel) (by decide +kernel) (by decide +kernel) (by decide +kernel) rfl

end ConcreteChecks.StageInterfaces.Frontend.PrecedenceControls
