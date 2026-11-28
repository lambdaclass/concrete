import ConcreteSpec.StageGrammarCore
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.SliceGrammar
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Proofs.SliceGrammar

/-! Closed grammar and parser selectors at the stage boundary.

Every executable datum (`earlySharedGrammar`, `compileTableVerifier`,
`earlyProposal`, `acceptedCells`, `acceptedRank`, `earlyStepsPerToken`, and the
registered parser refusal rules) comes strictly from pinned specification
modules. Only `Prop`-valued proof fields are imported from
`ConcreteSpec.Proofs.SliceGrammar`, so proof-file edits cannot alter any
executable grammar, table, or parser behavior. -/

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

end ConcreteSpec.StageGrammar
