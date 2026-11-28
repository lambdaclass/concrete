import ConcreteChecks.StageInterfaces.Frontend.Literals

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1

namespace StageInterfaceControls

-- The verifier accepts the fixed proposal and rejects a changed prediction row.
set_option maxRecDepth 4096 in
example :
    let inputs := sliceVerifierInputs slice0
    let budget := (sliceGrammar slice0).verificationBudget
    let changed := inputs.proposal.set! (terminalColumn (some .fn)) 0
    (verifyRun inputs.program ⟨0, inputs.proposal⟩ budget).2 = some true ∧
    (verifyRun inputs.program ⟨0, changed⟩ budget).2 = some false :=
  ⟨(sliceVerifierInputs slice0).accepted, by with_unfolding_all rfl⟩

def returnPasses (elaborate : Parsed (SyntaxTree ⊕ Bool) → Except ElabRefusal (AdmissionInput earlyFixtureLanguage)) :
    Passes slice0 testProfile where
  lex _ := .ok returnStream
  elaborate := elaborate
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput rule)
  lowerUnchecked _ := .error (.invalidInput rule)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emitLLVM _ := .error (.invalidInput rule)
  emitLCB _ := .error (.invalidInput rule)

set_option maxRecDepth 4096 in
theorem parsedReturnDomain : Parsed.ElaborationDomain slice0 parsedReturn := by
  refine ⟨rfl, rfl, ⟨returnView, by with_unfolding_all rfl, by with_unfolding_all rfl⟩,
    ⟨.ok (pendingReturn), returnProgram, rfl, rfl⟩, ?_⟩
  intro payload matching
  obtain ⟨program, decoded, matching⟩ := matching
  have same : returnProgram = program := Option.some.inj (show some returnProgram = some program from decoded)
  subst program
  have eq : pendingReturn = payload := Except.ok.inj matching
  subst payload
  refine ⟨rfl, ?_⟩
  intro purpose
  cases purpose <;> exact returnedCoreWF 42

private theorem firstTokenConsumes {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {rhs tokens args steps} (parsed : DerivesArgs g slice rhs tokens args steps) :
    (∃ terminal tail, rhs = .t terminal :: tail) → tokens ≠ [] := by
  cases parsed with
  | nil =>
    rintro ⟨_, _, impossible⟩
    cases impossible
  | token => exact fun _ => List.cons_ne_nil _ _
  | child =>
    rintro ⟨_, _, impossible⟩
    cases impossible

private theorem firstTokenDerivation {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {n tokens tree steps} (parsed : Derives g slice n tokens tree steps)
    (first : ∀ p ∈ g.productions.toList, ∃ terminal tail, p.rhs = .t terminal :: tail) :
    tokens ≠ [] := by
  cases parsed with
  | apply p tokens args member _ parsed => exact firstTokenConsumes parsed (first p member)

-- A malformed stream has an admissible parser refusal. A fabricated tree
-- cannot be passed on to the elaborator as a successful parser output.
set_option maxRecDepth 4096 in
example :
    let edge := (registry (returnPasses fixtureElaborate)).parser
    let empty := {returnStream with tokens := #[]}
    edge.inputInvariant empty ∧
      ∃ failure, edge.run empty = .error failure ∧ edge.refusal empty failure := by
  dsimp only
  let failure : ParseRefusal := .malformed (sliceParser slice0).malformedRule ⟨⟨0⟩, 0, 0, 1, 1⟩
  refine ⟨⟨by with_unfolding_all rfl, rfl⟩, failure, by with_unfolding_all rfl,
    by with_unfolding_all rfl, ?_, by with_unfolding_all decide⟩
  rintro ⟨tree, slice, steps, derives⟩
  have first (p : Production (sliceGrammar slice0).value)
      (member : p ∈ (sliceGrammar slice0).productions.toList) :
      ∃ terminal tail, p.rhs = .t terminal :: tail := by
    have same := List.mem_singleton.mp member
    subst p
    exact ⟨.fn, _, rfl⟩
  exact (firstTokenDerivation derives first) rfl

set_option maxRecDepth 4096 in
example :
    let registered := registry (returnPasses fixtureElaborate)
    registered.parser.Compatible registered.elaborator ∧
    ¬ registered.elaborator.inputInvariant {parsedReturn with tree := .inl (.node .body [])} ∧
    registered.elaborator.Compatible registered.eligibility ∧
    registered.eligibility.Compatible registered.typing ∧
    registered.typing.Compatible registered.lowerUnchecked := by
  refine ⟨fun _ h => h, ?_, fun _ h => h, fun _ h => h, fun _ h => h.2⟩
  rintro ⟨_, _, _, ⟨result, matching⟩, _⟩
  obtain ⟨_, impossible, _⟩ := matching
  cases impossible

set_option maxRecDepth 4096 in
example :
    let registered := registry (returnPasses fixtureElaborate)
    registered.typing.outputInvariant (elaboratedReturn 42) := by
  exact ⟨rfl, (ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl⟩

set_option maxRecDepth 4096 in
theorem return256IllTyped : ¬ (elaboratedReturn 256).StaticallyTyped := by
  rintro ⟨v, represented, _, typed⟩
  cases v.unique represented (representsReturn 256)
  cases typed with
  | ret atom => cases atom with | int fits => cases fits

set_option maxRecDepth 4096 in
example :
    let failure : StageFailure := .admission (.refused (.core .slice0 (surfaceFileSpan resultSpan) []
      (.atPosition 0 (.literalOutOfRange .u8 256))))
    let passes := returnPasses fixtureElaborate
    let edge := (registry passes).typing
    edge.inputInvariant (elaboratedReturn 256) ∧
    edge.run (elaboratedReturn 256) = .error failure ∧
    edge.refusal (elaboratedReturn 256) failure ∧
    ¬ edge.refusal (elaboratedReturn 42) failure := by
  refine ⟨⟨rfl, returnedCoreWF 256, ⟨_, representsReturn 256⟩⟩,
    rfl, ⟨_, rfl, rfl⟩, ?_⟩
  rintro ⟨_, h, _⟩
  cases h

-- Exercise the exact registered parse and elaboration calls before the unchecked
-- typing refusal. A literal-only checker suffices for this fixture.
set_option maxRecDepth 4096 in
example :
    let failure : StageFailure := .admission (.refused (.core .slice0 ⟨⟨0⟩, 25, 28, 1, 26⟩ []
      (.atPosition 0 (.literalOutOfRange .u8 256))))
    let passes := {returnPasses fixtureElaborate with lex := fun _ => .ok (returnStream 256)}
    ((registry passes).lcbPipeline true).run (literalSource 256) = .error ⟨StageFailure, failure⟩ := by
  with_unfolding_all rfl

-- The unchecked type judgment permits a defined arithmetic trap.
set_option maxRecDepth 4096 in
example :
    let program : EarlyProgram :=
      ⟨[⟨⟨⟨0⟩, .add (.lit 255) (.lit 1)⟩, resultSpan, resultSpan, (resultSpan, resultSpan), .unnamed none⟩], .var ⟨0⟩, resultSpan, resultSpan⟩
    let core : CoreInput earlyFixtureLanguage :=
      ⟨elaborationSource.project, slice0,  .straightLine (.zero program) rfl⟩
    let typing := (registry (returnPasses fixtureElaborate)).typing
    typing.run core = .ok core ∧
    ConcreteSpec.run (AdmissionView.zero program).program 3 = .trap .arithmeticOverflow ∧
    ∀ failure, ¬ typing.refusal core failure := by
  refine ⟨rfl, rfl, ?_⟩
  rintro _ ⟨_, impossible, _⟩
  cases impossible

set_option maxRecDepth 4096 in
private theorem fixtureElaboratorValidated : (registry (returnPasses fixtureElaborate)).elaborator.Validated :=
  IntegratedCore.validated _ rfl

set_option maxRecDepth 4096 in
theorem wrongLiteralElaboratorInvalid (passes : Passes slice0 testProfile)
    (wrong : passes.elaborate parsedReturn = .ok (pendingLiteral 43)) :
    ¬ (registry passes).elaborator.Validated := by
  intro validated
  have matching := (validated.1 parsedReturn _ parsedReturnDomain wrong).2
  change SliceElaboration slice0 elaborationSource.project (.inl (returnSyntax 42))
    (.ok (pendingLiteral 43)) at matching
  obtain ⟨program, decoded, matching⟩ := matching
  obtain rfl := Option.some.inj (show some returnProgram = some program from decoded)
  cases matching

set_option maxRecDepth 4096 in
theorem returnLexerContract :
    Lexes elaborationSource returnStream := by
  refine ⟨rfl, by with_unfolding_all rfl, _, rfl, elaborationSourceText.toUTF8, ?_, rfl, _, rfl, ?_⟩
  all_goals with_unfolding_all rfl

set_option maxRecDepth 4096 in
theorem returnParserContract :
    (registry (returnPasses fixtureElaborate)).parser.success returnStream parsedReturn := by
  let g := sliceGrammar slice0
  let p := g.productions[0]'(by with_unfolding_all decide)
  let ts := returnView.tokens
  let args : ActionArgs g.value p.rhs :=
    ⟨ts[0]'(by decide), ts[1]'(by decide), ts[2]'(by decide), ts[3]'(by decide),
      ts[4]'(by decide), ts[5]'(by decide), ts[6]'(by decide), ts[7]'(by decide),
      ts[8]'(by decide), ts[9]'(by decide), ts[10]'(by decide), ()⟩
  let steps := 11 + 1 + (p.action.eval args).1
  refine ⟨by with_unfolding_all rfl, ⟨rfl, steps, ?_, ?_, by with_unfolding_all decide,
    returnLexerContract.2.1⟩, by with_unfolding_all decide⟩
  · with_unfolding_all
      exact Derives.apply p ts.toList args (List.mem_singleton_self _) (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) .nil)))))))))))
  · with_unfolding_all
      exact Predictive.apply p 0 ts.toList [] args rfl rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
        (.token rfl (by decide) (.token rfl (by decide) (.nil []))))))))))))

-- Read the literal and locations from the grammar's token leaves. The registered
-- elaborator consumes the AST returned by the same checked parser.
set_option maxRecDepth 4096 in
example :
    let good := registry (returnPasses fixtureElaborate)
    let mutant := registry (returnPasses (fun _ => .ok (pendingLiteral 43)))
    let refusing := registry (returnPasses (fun _ => .error (.source (.parser .unexpectedToken span.surface))))
    good.lexer.success elaborationSource returnStream ∧
    good.lexer.run elaborationSource = .ok returnStream ∧
    good.parser.run returnStream = .ok parsedReturn ∧
    good.elaborator.run parsedReturn = .ok pendingReturn ∧
    good.elaborator.success parsedReturn pendingReturn ∧
    ¬ mutant.elaborator.Validated ∧ ¬ refusing.elaborator.Validated := by
  refine ⟨?_, rfl, by with_unfolding_all rfl, rfl,
    ⟨returnProgram, rfl, rfl⟩, ?_, ?_⟩
  · exact returnLexerContract
  · exact wrongLiteralElaboratorInvalid _ rfl
  · intro validated
    have matching := validated.2 parsedReturn _ parsedReturnDomain rfl
    change SliceElaboration slice0 elaborationSource.project (.inl (returnSyntax 42)) (.error (.source (.parser .unexpectedToken span.surface))) at matching
    obtain ⟨program, decoded, matching⟩ := matching
    obtain rfl := Option.some.inj (show some returnProgram = some program from decoded)
    cases matching

-- The spec admits the reachable return program with no operation discharges.
-- The same fixed judgment rules out an unconditional checker refusal.
set_option maxRecDepth 2048 in
theorem returnAdmissionContract :
    Admits (elaboratedReturn 42) ⟨elaboratedReturn 42, []⟩ ∧
    ∀ failure, ¬ (elaboratedReturn 42).CheckerRefuses failure := by
  have fragment := representsReturn 42
  have accepted : (elaboratedReturn 42).Accepted := by
    refine ⟨(ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl, _, fragment, ?_⟩
    exact (ConcreteSpec.check_iff _).mp rfl
  refine ⟨⟨rfl, accepted, rfl, by simp, ?_, ?_⟩, ?_⟩
  · simp
  · rintro _ _ ⟨_, h, _⟩
    simp at h
  · exact fun _ refusal => refusal.1 accepted

end StageInterfaceControls
