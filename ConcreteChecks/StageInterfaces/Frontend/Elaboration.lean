import ConcreteChecks.StageInterfaces.Frontend.Literals

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1

namespace StageInterfaceControls

-- The verifier accepts the fixed 78-row proposal and rejects a changed prediction row.
example :
    let inputs := sliceVerifierInputs slice0
    let budget := (sliceGrammar slice0).verificationBudget
    let changed := inputs.proposal.set! (terminalColumn (some .fn)) 0
    (verifyRun inputs.program ⟨0, inputs.proposal⟩ budget).2 = some true ∧
    (verifyRun inputs.program ⟨0, changed⟩ budget).2 = some false := by
  dsimp only
  let inputs := sliceVerifierInputs slice0
  let budget := (sliceGrammar slice0).verificationBudget
  let col := terminalColumn (some .fn)
  let changed := inputs.proposal.set! col 0
  let progNt : Fin (sliceGrammarCount slice0) := ⟨0, by decide⟩
  refine ⟨inputs.accepted, ?_⟩
  have hSize : changed.size = inputs.verifier.words := by
    simp [changed, inputs.proposalSize]
  have hComp : (verifyRun inputs.program ⟨0, changed⟩ budget).2.isSome = true :=
    inputs.verifier.completes changed hSize
  cases hRes : (verifyRun inputs.program ⟨0, changed⟩ budget).2 with
  | none =>
    rw [hRes] at hComp
    cases hComp
  | some ok =>
    cases ok with
    | false => rfl
    | true =>
      exfalso
      obtain ⟨table, _, _, hCells⟩ := inputs.verifier.sound changed hSize hRes
      have hColLt : col < table.cells.size := by
        rw [table.dimensions]
        decide
      obtain ⟨word, hMem, hCell⟩ := hCells ⟨col, hColLt⟩
      have hColInChanged : col < changed.size := by
        cases Nat.lt_or_ge col changed.size with
        | inl h => exact h
        | inr h =>
          have : changed[col]? = none := Array.getElem?_eq_none h
          rw [this] at hMem; cases hMem
      have hColInProp : col < inputs.proposal.size := by
        simpa [changed] using hColInChanged
      have hWordZero : word = 0 := by
        have hGet : changed[col]? = some 0 := by
          simp only [changed, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds,
            hColInProp, ↓reduceIte]
        exact Option.some.inj (hMem.symm.trans hGet)
      subst hWordZero
      have hNone : table.lookup progNt (some .fn) = none := by
        unfold CheckedTable.lookup tableLookup
        change (table.cells[col]?).join = none
        rw [hCell]
        rfl
      have hPred : (sliceParser slice0).table.lookup progNt (some .fn) = some 0 := by
        change (sliceParser slice0).table.lookup ⟨0, by decide⟩ (some .fn) = some 0
        rw [Concrete.Parse.KernelEval.sliceParser_table_lookup_eq]
        decide
      have hSome : table.lookup progNt (some .fn) = some 0 :=
        (table.predicts progNt (some .fn) 0).mpr
          (((sliceParser slice0).table.predicts progNt (some .fn) 0).mp hPred)
      rw [hNone] at hSome
      cases hSome

def returnPasses (elaborate : Parsed (SyntaxTree ⊕ Empty) → Except ElabRefusal (AdmissionInput earlyFixtureLanguage)) :
    Passes slice0 testProfile where
  lex _ := .ok returnStream
  elaborate := elaborate
  checkCore := ConcreteSpec.Admission.checkInput
  lowerCore _ := .error (.invalidInput rule)
  lowerUnchecked _ := .error (.invalidInput rule)
  lowerMemory _ := .error (.invalidInput rule)
  buildSSA _ := .error (.invalidInput rule)
  buildBIR _ := .error (.invalidInput rule)
  emit _ := .error (.invalidInput rule)

theorem registry_parser_run {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) (stream : TokenStream) :
    (registry passes).parser.run stream =
      (parserEntry (sliceParser slice) (sliceParser_nonExhausted slice)).run stream :=
  rfl

theorem registry_pipeline_unchecked_run_to_typing {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) (src : Source)
    {stream : TokenStream}
    {ast : Parsed ((sliceGrammar slice).value (sliceGrammar slice).start)}
    {elabIn : AdmissionInput (sliceLanguage slice)}
    {core : CoreInput (sliceLanguage slice)}
    {failure : StageFailure}
    (hlex : (registry passes).lexer.run src = .ok stream)
    (hparse : (registry passes).parser.run stream = .ok ast)
    (helab : (registry passes).elaborator.run ast = .ok elabIn)
    (helig : (registry passes).eligibility.run elabIn = .ok core)
    (htyp : (registry passes).typing.run core = .error failure) :
    (Registry.pipeline passes .unchecked).run src = .error ⟨StageFailure, failure⟩ := by
  show (Pipeline.next (registry passes).lexer
    (.next (registry passes).parser
      (.next (registry passes).elaborator
        (.next (registry passes).eligibility
          (.next (registry passes).typing _))))).run src = _
  simp only [Pipeline.run, hlex, hparse, helab, helig, htyp]

mutual
  private def syntaxTreeEq : SyntaxTree → SyntaxTree → Bool
    | .leaf t1, .leaf t2 => t1.span == t2.span && t1.value == t2.value
    | .node k1 xs1, .node k2 xs2 => k1 == k2 && syntaxTreeListEq xs1 xs2
    | _, _ => false

  private def syntaxTreeListEq : List SyntaxTree → List SyntaxTree → Bool
    | [], [] => true
    | x :: xs, y :: ys => syntaxTreeEq x y && syntaxTreeListEq xs ys
    | _, _ => false
end

mutual
  private theorem syntaxTreeEq_sound : ∀ {a b : SyntaxTree}, syntaxTreeEq a b = true → a = b
    | .leaf ⟨s1, v1⟩, .leaf ⟨s2, v2⟩, h => by
      simp only [syntaxTreeEq, Bool.and_eq_true, beq_iff_eq] at h
      rcases h with ⟨rfl, rfl⟩; rfl
    | .node k1 xs1, .node k2 xs2, h => by
      simp only [syntaxTreeEq, Bool.and_eq_true, beq_iff_eq] at h
      rcases h with ⟨rfl, hxs⟩
      rw [syntaxTreeListEq_sound hxs]
    | .leaf _, .node _ _, h | .node _ _, .leaf _, h => by cases h

  private theorem syntaxTreeListEq_sound : ∀ {xs ys : List SyntaxTree}, syntaxTreeListEq xs ys = true → xs = ys
    | [], [], _ => rfl
    | x :: xs, y :: ys, h => by
      simp only [syntaxTreeListEq, Bool.and_eq_true] at h
      rw [syntaxTreeEq_sound h.1, syntaxTreeListEq_sound h.2]
    | [], _ :: _, h | _ :: _, [], h => by cases h
end

private def matchesSparseTree (input : ParserView) (expected : SyntaxTree) : Bool :=
  match (Concrete.Parse.KernelEval.runEarlySparse input).2 with
  | some (.ok ⟨_, .inl tree⟩) => syntaxTreeEq tree expected
  | _ => false

private theorem runEarlySparse_ok_of_matches {input : ParserView} {expected : SyntaxTree}
    (h : matchesSparseTree input expected = true) :
    (Concrete.Parse.KernelEval.runEarlySparse input).2 = some (.ok ⟨input.source, .inl expected⟩) := by
  unfold matchesSparseTree at h
  cases hrun : (Concrete.Parse.KernelEval.runEarlySparse input).2 with
  | none => rw [hrun] at h; cases h
  | some res =>
    rw [hrun] at h
    cases res with
    | error _ => cases h
    | ok parsed =>
      rcases parsed with ⟨src, ast⟩
      cases ast with
      | inr e => exact nomatch e
      | inl tree =>
        have hsrc : src = input.source := by
          have hps := congrArg Prod.snd (Concrete.Parse.KernelEval.sliceParser_run_eq_sparse input)
          have hok : ((sliceParser slice0).run input).2 = some (.ok ⟨src, .inl tree⟩) :=
            hps.trans hrun
          exact ((sliceParser_ok_iff slice0 input ⟨src, .inl tree⟩).mp hok).1
        subst hsrc
        rw [syntaxTreeEq_sound h]

private theorem sliceParser_ok_of_matches {input : ParserView} {expected : SyntaxTree}
    (h : matchesSparseTree input expected = true) :
    ((sliceParser slice0).run input).2 = some (.ok ⟨input.source, .inl expected⟩) := by
  have hsp : (ParserEngine.run (g := earlySharedGrammar) (sliceParser slice0) input).2 =
      (Concrete.Parse.KernelEval.runEarlySparse input).2 :=
    congrArg Prod.snd (Concrete.Parse.KernelEval.sliceParser_run_eq_sparse input)
  exact hsp.trans (runEarlySparse_ok_of_matches h)

private def sparseErrorOf (input : ParserView) : Option ParseRefusal :=
  match (Concrete.Parse.KernelEval.runEarlySparse input).2 with
  | some (.error e) => some e
  | _ => none

private theorem sliceParser_error_of_sparse {input : ParserView} {refusal : ParseRefusal}
    (h : sparseErrorOf input = some refusal) :
    ((sliceParser slice0).run input).2 = some (.error refusal) := by
  have hsp : (ParserEngine.run (g := earlySharedGrammar) (sliceParser slice0) input).2 =
      (Concrete.Parse.KernelEval.runEarlySparse input).2 :=
    congrArg Prod.snd (Concrete.Parse.KernelEval.sliceParser_run_eq_sparse input)
  unfold sparseErrorOf at h
  cases hrun : (Concrete.Parse.KernelEval.runEarlySparse input).2 with
  | none => rw [hrun] at h; cases h
  | some res =>
    rw [hrun] at h
    cases res with
    | ok _ => cases h
    | error e =>
      cases Option.some.inj h
      exact hsp.trans hrun

theorem returnParserRun42 :
    ((sliceParser slice0).run (returnView 42)).2 = some (.ok parsedReturn) :=
  sliceParser_ok_of_matches (by decide +kernel)

theorem returnParserRun256 :
    ((sliceParser slice0).run (returnView 256)).2 =
      some (.ok ⟨literalSource 256, .inl (returnSyntax 256)⟩) :=
  sliceParser_ok_of_matches (by decide +kernel)

theorem returnStream_parserView (n : Nat) : (returnStream n).parserView = returnView n := by
  unfold TokenStream.parserView returnStream
  simp [Array.toList_map, List.map_map, Function.comp_def]

theorem parsedReturnDomain : Parsed.ElaborationDomain slice0 parsedReturn := by
  have hOrd : Concrete.Interfaces.TokensOrdered returnView := by
    unfold Concrete.Interfaces.TokensOrdered ConcreteSpec.StageGrammar.TokensOrdered
    decide
  refine ⟨rfl, rfl, ⟨returnView, by with_unfolding_all rfl, hOrd, returnParserRun42⟩,
    ⟨.ok pendingReturn, returnProgram, rfl, rfl⟩, ?_⟩
  intro payload matching
  obtain ⟨program, decoded, matching⟩ := matching
  have same : returnProgram = program := Option.some.inj (show some returnProgram = some program from decoded)
  subst program
  have eq : pendingReturn = payload := Except.ok.inj matching
  subst payload
  refine ⟨rfl, ?_⟩
  intro purpose
  cases purpose <;> exact returnedCoreWF 42

-- A malformed stream has an admissible parser refusal. A fabricated tree
-- cannot be passed on to the elaborator as a successful parser output.
example :
    let edge := (registry (returnPasses fixtureElaborate)).parser
    let empty := {returnStream with tokens := #[]}
    edge.inputInvariant empty ∧
      ∃ failure, edge.run empty = .error failure ∧ edge.refusal empty failure := by
  dsimp only
  let empty := {returnStream with tokens := #[]}
  let failure : ParseRefusal :=
    .malformed (sliceParser slice0).malformedRule .unexpectedEnd (eofFileSpan elaborationSource)
  have hwf : empty.parserView.WF := by with_unfolding_all rfl
  have hrun : ((sliceParser slice0).run empty.parserView).2 = some (.error failure) :=
    sliceParser_error_of_sparse (by decide +kernel)
  have hmal : Refuses (sliceGrammar slice0) empty.parserView failure :=
    (sliceParser_malformed_iff slice0 empty.parserView hwf).mp ⟨_, _, _, hrun⟩
  have hord : Concrete.Interfaces.TokensOrdered empty.parserView := by
    simp [Concrete.Interfaces.TokensOrdered, ConcreteSpec.StageGrammar.TokensOrdered, TokenStream.parserView, empty]
  have hEdgeRun : (registry (returnPasses fixtureElaborate)).parser.run empty = .error failure := by
    rw [registry_parser_run]
    exact (parserEntry_run_eq_error_iff (sliceParser slice0) (sliceParser_nonExhausted slice0)
      empty failure).mpr hrun
  refine ⟨⟨hwf, hord, rfl⟩, failure, hEdgeRun,
    hrun, hmal, sliceParser_steps_le slice0 empty.parserView⟩

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

example :
    let registered := registry (returnPasses fixtureElaborate)
    registered.typing.outputInvariant (elaboratedReturn 42) := by
  exact ⟨rfl, (ConcreteSpec.Admission.checkStatic_iff (returnedCoreWF 42)).1 rfl⟩

theorem return256IllTyped : ¬ (elaboratedReturn 256).StaticallyTyped := by
  rintro ⟨v, represented, _, typed⟩
  cases v.unique represented (representsReturn 256)
  cases typed with
  | ret atom => cases atom with | int fits => cases fits

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
    rfl, ⟨_, rfl, ConcreteSpec.Admission.checkStatic_refused rfl, rfl⟩, ?_⟩
  rintro ⟨_, h, _⟩
  cases h

-- Exercise the exact registered parse and elaboration calls before the unchecked
-- typing refusal. A literal-only checker suffices for this fixture.
example :
    let failure : StageFailure := .admission (.refused (.core .slice0 ⟨⟨0⟩, 25, 28, 1, 26⟩ []
      (.atPosition 0 (.literalOutOfRange .u8 256))))
    let passes := {returnPasses fixtureElaborate with lex := fun _ => .ok (returnStream 256)}
    (Registry.pipeline passes .unchecked).run (literalSource 256) = .error ⟨StageFailure, failure⟩ := by
  dsimp only
  let passes := {returnPasses fixtureElaborate with lex := fun _ => .ok (returnStream 256)}
  have hparse : (registry passes).parser.run (returnStream 256) =
      .ok ⟨literalSource 256, .inl (returnSyntax 256)⟩ := by
    rw [registry_parser_run]
    apply (parserEntry_run_eq_ok_iff (sliceParser slice0) (sliceParser_nonExhausted slice0)
      (returnStream 256) ⟨literalSource 256, .inl (returnSyntax 256)⟩).mpr
    rw [returnStream_parserView]
    exact returnParserRun256
  let ast256 : Parsed ((sliceGrammar slice0).value (sliceGrammar slice0).start) :=
    ⟨literalSource 256, .inl (returnSyntax 256)⟩
  let prog256 : Surface0.Program :=
    ⟨"main", ⟨3, 7, 1, 4, ⟨0⟩⟩,
      [.ret (.integer 256 none ⟨25, 28, 1, 26, ⟨0⟩⟩) ⟨18, 29, 1, 19, ⟨0⟩⟩],
      ⟨30, 31, 1, 31, ⟨0⟩⟩⟩
  let pending256 : AdmissionInput earlyFixtureLanguage :=
    .pending (literalSource 256).project (.zero prog256) rfl
  let core256 : CoreInput earlyFixtureLanguage :=
    ⟨(literalSource 256).project, slice0,
      .straightLine (.zero ⟨[], .lit 256, ⟨18, 29, 1, 19, ⟨0⟩⟩, ⟨25, 28, 1, 26, ⟨0⟩⟩⟩) rfl⟩
  exact registry_pipeline_unchecked_run_to_typing passes (literalSource 256)
    (stream := returnStream 256) (ast := ast256) (elabIn := pending256) (core := core256)
    rfl hparse rfl rfl rfl

-- The unchecked type judgment permits a defined arithmetic trap.
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

private theorem fixtureElaboratorValidated : (registry (returnPasses fixtureElaborate)).elaborator.Validated :=
  IntegratedCore.validated _ rfl

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

theorem returnLexerContract :
    Lexes elaborationSource returnStream := by
  refine ⟨rfl, by with_unfolding_all rfl, _, rfl, elaborationSourceText.toUTF8, ?_, rfl, _, rfl, ?_⟩
  all_goals with_unfolding_all rfl

theorem returnParserContract :
    (registry (returnPasses fixtureElaborate)).parser.success returnStream parsedReturn := by
  have hrun : ((sliceParser slice0).run returnStream.parserView).2 = some (.ok parsedReturn) := by
    rw [returnStream_parserView]
    exact returnParserRun42
  exact ⟨hrun, (sliceParser_ok_iff slice0 returnStream.parserView parsedReturn).mp hrun,
    sliceParser_steps_le slice0 returnStream.parserView⟩

-- Read the literal and locations from the grammar's token leaves. The registered
-- elaborator consumes the AST returned by the same checked parser.
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
  refine ⟨?_, rfl, ?_, rfl,
    ⟨returnProgram, rfl, rfl⟩, ?_, ?_⟩
  · exact returnLexerContract
  · rw [registry_parser_run]
    apply (parserEntry_run_eq_ok_iff (sliceParser slice0) (sliceParser_nonExhausted slice0)
      returnStream parsedReturn).mpr
    rw [returnStream_parserView]
    exact returnParserRun42
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
