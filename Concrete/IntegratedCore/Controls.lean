import Concrete.IntegratedCore
import ConcreteSpec.IntegratedCore

namespace Concrete.IntegratedCore
open Concrete.Interfaces
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageGrammar
open ConcreteSpec.StageInterface

private def source : Source :=
  ⟨Project.singleton mainSourcePath
      (SourceText.ofBytes "fn main() -> u8 { return 42; }".toUTF8.data.toList), ⟨0⟩, slice0⟩

private def returnTokens : ParserView :=
  let token := fun start stop value => (⟨⟨⟨0⟩, start, stop, 1, start + 1⟩, value⟩ : SurfaceToken)
  ⟨source, #[
    token 0 2 (.fixed .fn rfl), token 3 7 (.name "main"), token 7 8 (.fixed .lparen rfl),
    token 8 9 (.fixed .rparen rfl), token 10 12 (.fixed .arrow rfl), token 13 15 (.fixed .u8 rfl),
    token 16 17 (.fixed .lbrace rfl), token 18 24 (.fixed .return_ rfl),
    token 25 27 (.integer 42 .decimal none), token 27 28 (.fixed .semi rfl),
    token 29 30 (.fixed .rbrace rfl)]⟩

private def expected : CoreInput (sliceLanguage slice0) :=
  ⟨source.project, slice0, earlyPayload (sliceEarlyCore slice0)
    ⟨[], .lit 42, ⟨25, 27, 1, 26, ⟨0⟩⟩⟩⟩

private def parseAndElaborate (tokens : ParserView) : Option (CoreInput (sliceLanguage slice0)) := do
  let parsed ← ((sliceParser slice0).run tokens).2
  let ast ← parsed.toOption
  (elaborate slice0 ast).toOption

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

private def slice1 : Slice := ⟨⟨1, by decide⟩⟩

example :
    elaborate slice1 ⟨{source with slice := slice1}, .inr true⟩ =
      .error (.unsupportedSlice slice1) := rfl

example :
    elaborate slice0 ⟨{source with slice := slice1}, .inr true⟩ =
      .error (.diagnostic ⟨``Parsed.ElaborationDomain, 1⟩ ⟨⟨0⟩, 0, 0, 1, 1⟩ []) := rfl

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

end Concrete.IntegratedCore
