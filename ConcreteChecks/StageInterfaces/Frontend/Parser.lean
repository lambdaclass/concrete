import ConcreteChecks.StageInterfaces.Fixtures

open Concrete
open Concrete.Interfaces
open ConcreteSpec Core Core.SourcePathSpan StageInterface StageGrammar
open Concrete.LL1

namespace StageInterfaceControls

-- A fixed parser entry also fixes the entire refusal, including its rule and span.
example {count : Nat} {g : SharedGrammar count} (engine : ParserEngine g) (stream : TokenStream)
    (first second : ParseRefusal) (h₁ : (parserEntry engine).refusal stream first)
    (h₂ : (parserEntry engine).refusal stream second) : first = second :=
  Except.error.inj (Option.some.inj (h₁.1.symm.trans h₂.1))

section
variable (verifier : TableVerifier grammar) (size : proposal.size = verifier.words)
  (accept : (verifier.run proposal).2 = some true)

local notation "parser" => testParser verifier size accept

-- The parser sees all decoded payloads without consulting the raw source.
example (source : Source) :
    let view := TokenStream.parserView ⟨source, #[⟨⟨span, .stringLiteral [65, 0]⟩, ByteArray.empty⟩,
      ⟨⟨span, .byteLiteral 255⟩, ByteArray.empty⟩,
      ⟨⟨span, .label ⟨"next"⟩⟩, ByteArray.empty⟩], #[#[], #[], #[], #[]]⟩
    view.tokens[0]?.map Located.value = some (.stringLiteral [65, 0]) ∧
      view.tokens[1]?.map Located.value = some (.byteLiteral 255) ∧
      view.tokens[2]?.map Located.value = some (.label ⟨"next"⟩) :=
  ⟨rfl, rfl, rfl⟩

private def spacedStream : TokenStream :=
  let source : Source := ⟨Project.singleton mainSourcePath (SourceText.ofBytes [52, 50, 32, 13, 10]), ⟨0⟩, early⟩
  ⟨source, #[⟨⟨{span with endOffset := 2}, .integer 42 .decimal none⟩, "42".toUTF8⟩],
    #[#[], #[⟨{span with startOffset := 2, endOffset := 5, col := 3}, " \r\n".toUTF8⟩]]⟩

-- The mutant retains the same token and the same number of trivia groups.
-- Only reconstruction of the retained pieces can distinguish it.
example :
    let dropped := {spacedStream with trivia := #[#[], #[]]}
    let mutant := ({testPasses (profile := testProfile) [] with
      lex := fun _ => .ok dropped}).lexer
    dropped.parserView = spacedStream.parserView ∧
    mutant.success spacedStream.source spacedStream ∧
    ¬ mutant.Validated := by
  refine ⟨rfl, ⟨rfl, rfl, _, rfl, _, rfl, rfl, _, rfl, rfl⟩, ?_⟩
  intro validated
  obtain ⟨_, _, file, lookup, bytes, rebuilt, exactBytes, _⟩ :=
    (validated.1 spacedStream.source _ rfl rfl).2
  have short : some "42".toUTF8 = some bytes := rebuilt
  cases Option.some.inj short
  have found : some (File.load mainSourcePath (SourceText.ofBytes [52, 50, 32, 13, 10])) = some file := lookup
  obtain rfl := Option.some.inj found
  exact (show ¬ ([52, 50] : List UInt8) = [52, 50, 32, 13, 10] by decide) exactBytes

-- The retained bytes and coordinates are identical. Only the reported value changes.
example :
    let wrong := {spacedStream with tokens :=
      #[⟨⟨{span with endOffset := 2}, .integer 43 .decimal none⟩, "42".toUTF8⟩]}
    let mutant := ({testPasses (profile := testProfile) [] with
      lex := fun _ => .ok wrong}).lexer
    reconstruct wrong.source.entryFile wrong.tokens wrong.trivia =
      reconstruct spacedStream.source.entryFile spacedStream.tokens spacedStream.trivia ∧
    wrong.parserView.WF ∧ ¬ mutant.Validated := by
  refine ⟨rfl, rfl, ?_⟩
  intro valid
  obtain ⟨_, _, _, _, bytes, restored, _, reference, scanned, pieces⟩ :=
    (valid.1 spacedStream.source _ rfl rfl).2
  obtain rfl := Option.some.inj (show some "42 \r\n".toUTF8 = some bytes from restored)
  have expected : Concrete.Lex.lexLossless "42 \r\n".toUTF8 ⟨0⟩ = .ok _ := rfl
  obtain rfl := Except.ok.inj (expected.symm.trans scanned)
  cases pieces

-- A fabricated AST from the registered parser's success channel fails the exact run check.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, .inr false⟩ := by
  intro h
  have run := congrArg (fun x => x.map (fun y => y.toOption.map Parsed.tree)) h.1
  have wrong : true = false := Sum.inr.inj (Option.some.inj (Option.some.inj run))
  cases wrong

-- The same registry returns 42's AST for decimal, refuses hexadecimal at Slice 0,
-- and accepts the hexadecimal token when that production's spelling is available.
example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.run (tokenStream .decimal) =
      .ok ⟨(tokenStream .decimal).source, .inr true⟩ ∧
    (r m parser).parser.run (tokenStream .hexadecimal) =
      .error (.unsupported rule span later) ∧
    (r m parser).parser.run (tokenStream .hexadecimal later) =
      .ok ⟨(tokenStream .hexadecimal later).source, .inr true⟩ :=
  ⟨rfl, rfl, rfl⟩

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.success (tokenStream .decimal)
      ⟨(tokenStream .decimal).source, .inr true⟩ := by
  refine ⟨rfl, ⟨rfl, 3, ?_, ?_, by change 3 ≤ 5; decide, rfl⟩, by change 5 ≤ 8; decide⟩
  · exact Derives.apply (g := grammar) production _ _
      (List.mem_cons_self ..) (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) .nil)
  · exact Predictive.apply (table := table) production 0 _ [] _
      rfl rfl (Nat.le_refl _) (.token rfl (by change 0 ≤ 0; decide) (.nil []))

example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).parser.refusal (tokenStream .hexadecimal) (.unsupported rule span later) := by
  refine ⟨rfl, ⟨by change 0 < 2; decide, ⟨.inr true, 3, Derives.apply (g := grammar) production _ _
    (List.mem_cons_self ..) (by decide) (.token rfl (by change 2 ≤ 2; decide) .nil)⟩, ?_⟩, by change 5 ≤ 8; decide⟩
  intro earlier tooEarly
  rintro ⟨tree, steps, derives⟩
  have required := Derives.tokensEligible derives _ (List.mem_cons_self ..)
  exact Nat.not_lt_of_ge required tooEarly

-- A correct AST does not make a run with more than the permitted transitions sound.
-- The machine spends five transitions; a two-step-per-token profile permits four.
example (m : LoweringModels testLanguage testLowering testProfile) :
    ¬ (r m (testParser verifier size accept 2)).parser.Checks := by
  intro sound
  have h := sound.1.1 (tokenStream .decimal)
    ⟨(tokenStream .decimal).source, .inr true⟩ rfl rfl
  exact (show ¬ 5 ≤ 4 from by decide) h.2.2.2

private def corruptTokenStream (location : FileSpan) : TokenStream :=
  {tokenStream .decimal with tokens := #[⟨⟨location, .integer 42 .decimal none⟩, "a".toUTF8⟩]}

-- Valid and corrupt streams have the same grammar projection. The registered
-- parser rejects both a fabricated file and an out-of-bounds byte range.
example (m : LoweringModels testLanguage testLowering testProfile) :
    let edge := (r m parser).parser
    let wrongFile := {span with file := ⟨1⟩}
    let wrongEnd := {span with endOffset := 4}
    let wrongLine := {span with line := 2}
    edge.inputInvariant (tokenStream .decimal) ∧
    (∀ location ∈ [wrongFile, wrongEnd, wrongLine],
      ¬ edge.inputInvariant (corruptTokenStream location) ∧
      edge.run (corruptTokenStream location) = .error (.invalidStream location) ∧
      edge.refusal (corruptTokenStream location) (.invalidStream location)) := by
  refine ⟨rfl, ?_⟩
  intro location member
  simp only [List.mem_cons, List.not_mem_nil, or_false] at member
  rcases member with rfl | rfl | rfl <;>
    exact ⟨fun h => by contradiction, rfl, rfl, rfl, by change 1 ≤ 8; decide⟩

-- The checked lexer/parser link has no model-selected token invariant.
example (m : LoweringModels testLanguage testLowering testProfile) :
    (r m parser).lexer.Compatible (r m parser).parser := fun _ h => h

-- Extra proposal columns cannot be counted as terminals to inflate the budget.
example (g : SharedGrammar 1) : g.terminalColumns ≤ Surface0.Terminal.count + 1 := by
  apply Nat.le_of_not_gt
  intro oversized
  have room : Surface0.Terminal.count < g.terminalSlices.size := by
    change Surface0.Terminal.count + 1 < g.terminalSlices.size + 1 at oversized
    omega
  obtain ⟨kind, index⟩ := g.terminalSliceExact ⟨_, room⟩
  exact (Nat.ne_of_lt (Surface0.Terminal.index_lt_count kind)) index

-- An accepting callback could hide work. Here even a loop with no changing
-- memory reaches the limit without producing a verifier verdict.
example : verifyRun #[.ifEqual 0 0 0 0] ⟨0, #[0]⟩ 8 = (8, none) := rfl

end

end StageInterfaceControls
