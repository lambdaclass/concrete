import ConcreteSpec.StageGrammarCore
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.SliceGrammar
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Proofs.SliceGrammar
import ConcreteSpec.Proofs.StageParser

/-! Closed grammar and parser selectors at the stage boundary.

Every executable datum (`earlySharedGrammar`, `compileTableVerifier`,
`earlyProposal`, `acceptedCells`, `acceptedRank`, `earlyStepsPerToken`, and the
registered parser refusal rules) comes strictly from pinned specification
modules. Only `Prop`-valued proof fields and theorems are imported from
`ConcreteSpec.Proofs.SliceGrammar` and `ConcreteSpec.Proofs.StageParser`, so
proof-file edits cannot alter any executable grammar, table, or parser behavior. -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.GrammarData
open ConcreteSpec.GrammarVerification

abbrev sliceGrammarCount (_slice : Slice) : Nat := Nonterminal.count

def sliceASTExtra (_slice : Slice) : Type := Empty

def sliceGrammar (_slice : Slice) : SharedGrammar (sliceGrammarCount _slice) :=
  earlySharedGrammar

theorem sliceGrammarStartShape (slice : Slice) :
    (sliceGrammar slice).value (sliceGrammar slice).start = SurfaceAST sliceASTExtra slice := rfl

private def earlyVerifier : TableVerifier earlySharedGrammar where
  program := (compileTableVerifier earlySharedGrammar.rawGrammar).program
  words := (compileTableVerifier earlySharedGrammar.rawGrammar).words
  spaceBound := earlyVerifier_spaceBound
  completes := earlyVerifier_completes
  sound := earlyVerifier_sound

private def earlyTable : CheckedTable earlySharedGrammar where
  cells := acceptedCells earlyProposal earlySharedGrammar
  dimensions := earlyTable_dimensions
  predicts := earlyTable_predicts
  rank := acceptedRank earlyProposal earlySharedGrammar
  acyclic := earlyTable_acyclic

private def earlyVerification : VerificationInputs earlySharedGrammar where
  verifier := earlyVerifier
  verifierCompiled := ⟨rfl, rfl⟩
  proposal := earlyProposal
  proposalSize := earlyVerification_proposalSize
  accepted := earlyVerification_accepted
  engine := ⟨earlyTable, earlyStepsPerToken, earlyMalformedRule,
    earlyUnsupportedProductionRule, earlyUnsupportedLiteralRule⟩
  engineTableEq := rfl

def sliceVerifierInputs (slice : Slice) : VerificationInputs (sliceGrammar slice) :=
  earlyVerification

def sliceParser (slice : Slice) : ParserEngine (sliceGrammar slice) :=
  (sliceVerifierInputs slice).engine

/-- Certified nonterminal rank bound for the registered slice parser, bridged directly to
`earlyTable_rank_le_max` without unfolding `earlyProposal`. -/
theorem sliceParser_rank_le (s : Slice) (n : Fin (sliceGrammarCount s)) :
    (sliceParser s).table.rank n ≤ earlyMaxRank := by
  dsimp only [sliceParser, sliceVerifierInputs, earlyVerification, earlyTable]
  exact earlyTable_rank_le_max n

/-- The registered slice parser's step-per-token multiplier meets the linear-work
threshold for `earlyMaxRank`. -/
theorem sliceParser_stepsPerToken_ge (s : Slice) :
    linearStepsPerToken (sliceGrammar s).maxFrameCost (sliceGrammar s).maxNonterminals earlyMaxRank ≤
      (sliceParser s).stepsPerToken :=
  Nat.le_refl _

/-- Totality of the registered slice parser: `run` never exhausts fuel on any `ParserView`. -/
theorem sliceParser_nonExhausted (s : Slice) (input : ParserView) :
    ((sliceParser s).run input).2.isSome = true :=
  parserEngine_nonExhausted (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input

/-- Exact linear step budget of the registered slice parser. -/
theorem sliceParser_steps_le (s : Slice) (input : ParserView) :
    ((sliceParser s).run input).1 ≤ (sliceParser s).stepsPerToken * (input.tokens.size + 1) :=
  parserEngine_steps_le (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input

/-- Acceptance equivalence of the registered slice parser with `Derives`. -/
theorem sliceParser_ok_derives_iff (s : Slice) (input : ParserView)
    (tree : (sliceGrammar s).value (sliceGrammar s).start) :
    ((sliceParser s).run input).2 = some (.ok ⟨input.source, tree⟩) ↔
      input.WF ∧ ∃ steps, Derives (sliceGrammar s) input.source.slice (sliceGrammar s).start
        input.tokens.toList tree steps :=
  parserEngine_ok_derives_iff (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input tree

/-- Acceptance equivalence of the registered slice parser with `ParsesWith`. -/
theorem sliceParser_ok_iff (s : Slice) (input : ParserView)
    (output : Parsed ((sliceGrammar s).value (sliceGrammar s).start)) :
    ((sliceParser s).run input).2 = some (.ok output) ↔
      ParsesWith (sliceParser s) input output :=
  parserEngine_ok_iff (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input output

/-- Unsupported-slice refusal equivalence for the registered slice parser. -/
theorem sliceParser_unsupported_iff (s : Slice) (input : ParserView)
    (hwf : input.WF) (required : Slice) :
    (∃ rule cause span,
        ((sliceParser s).run input).2 = some (.error (.unsupported rule cause span required))) ↔
      input.source.slice.val < required.val ∧
      (∃ tree steps, Derives (sliceGrammar s) required (sliceGrammar s).start
        input.tokens.toList tree steps) ∧
      ∀ earlier, earlier.val < required.val →
        ¬ ∃ tree steps, Derives (sliceGrammar s) earlier (sliceGrammar s).start
          input.tokens.toList tree steps :=
  parserEngine_unsupported_iff (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input hwf required

/-- Malformed refusal equivalence for the registered slice parser. -/
theorem sliceParser_malformed_iff (s : Slice) (input : ParserView) (hwf : input.WF) :
    (∃ rule cause span,
        ((sliceParser s).run input).2 = some (.error (.malformed rule cause span))) ↔
      ¬ ∃ tree required steps, Derives (sliceGrammar s) required (sliceGrammar s).start
        input.tokens.toList tree steps :=
  parserEngine_malformed_iff (sliceParser s) earlyMaxRank
    (sliceParser_rank_le s) (sliceParser_stepsPerToken_ge s) input hwf

/-- No unsupported refusal produced by the registered slice parser cites the default `.program` id. -/
theorem sliceParser_unsupported_ne_program (s : Slice) (input : ParserView)
    (rule : Core.RegisteredRule) (cause : UnsupportedCause)
    (span : FileSpan) (required : Slice)
    (hrun : ((sliceParser s).run input).2 = some (.error (.unsupported rule cause span required))) :
    cause ≠ .production .program :=
  earlyParser_unsupported_ne_program (sliceParser s) input rule cause span required hrun

end ConcreteSpec.StageGrammar
