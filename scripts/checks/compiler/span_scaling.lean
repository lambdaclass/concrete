import Concrete.Lex
import ConcreteSpec.Spec.Rules

open ConcreteSpec.Core SourcePathSpan

private def loadProject (bytes : List UInt8) : Project :=
  ⟨[File.load mainSourcePath (.ofBytes bytes)], by simp [SourcePath.Project.Reference.Admitted]⟩

private def checkLexer : IO Unit := do
  let src := "a\r\nb\rc\nd".toUTF8
  let .ok tokens := Concrete.Lex.lex src
    | throw (IO.userError "lexer refused the line-ending control")
  let observed := tokens.map fun t => (t.span.start, t.span.stop, t.span.line, t.span.column)
  unless observed == [(0, 1, 1, 1), (3, 4, 2, 1), (5, 6, 3, 1), (7, 8, 4, 1)] do
    throw (IO.userError s!"lexer reported unexpected coordinates: {observed}")

private def checkRevisions : IO Unit := do
  for name in [``FilePresent, ``OrderedOffsets, ``EndInSource, ``LineMatches, ``ColumnMatches] do
    unless (registeredRules.find? (fun rule => rule.name == name)).map (·.rev) == some 2 do
      throw (IO.userError s!"expected revision 2 for {name}")

private def checkSpan (size : Nat) (rescan : Bool) : IO Unit := do
  let project := loadProject (List.replicate size 97)
  for offset in [:size] do
    let some span := projectSpan project mainSourcePath offset (offset + 1)
      | throw (IO.userError s!"missing span at byte {offset}")
    let expected : Span :=
      ⟨⟨0⟩, offset, offset + 1, Nat.le_succ _, 1, offset + 1, Nat.zero_lt_succ _, Nat.zero_lt_succ _⟩
    unless decide (span = ⟨expected⟩) do
      throw (IO.userError s!"incorrect span at byte {offset}")
    -- Plant the former prefix-rescan cost on the same workload.
    if rescan then
      let some file := project.fileAt span.fileId
        | throw (IO.userError "missing file for prefix-rescan control")
      unless file.source.columnAt offset == span.col do
        throw (IO.userError s!"prefix scan disagreed at byte {offset}")

private def measureSpanWork (size samples : Nat) (rescan : Bool) : IO Nat := do
  let mut best : Option Nat := none
  for _ in [:samples] do
    let start ← IO.monoNanosNow
    checkSpan size rescan
    let elapsed := (← IO.monoNanosNow) - start
    best := some (min elapsed (best.getD elapsed))
  return best.getD 0

def main : IO Unit := do
  checkLexer
  checkRevisions
  let some small := projectSpan (loadProject [97]) mainSourcePath 0 1
    | throw (IO.userError "missing span in the short file")
  let some large := projectSpan (loadProject (List.replicate 32768 97)) mainSourcePath 0 1
    | throw (IO.userError "missing span in the long file")
  unless decide (small.toSpan = large.toSpan) &&
      reprStr small == reprStr large do
    throw (IO.userError "the span payload changed when the file grew")
  let fastSmall ← measureSpanWork 2048 3 false
  let fastLarge ← measureSpanWork 16384 3 false
  let slowSmall ← measureSpanWork 512 2 true
  let slowLarge ← measureSpanWork 4096 2 true
  IO.println s!"span construction and comparison (2048 → 16384): {fastSmall / 1000} → {fastLarge / 1000} µs"
  IO.println s!"planted prefix rescans (512 → 4096): {slowSmall / 1000} → {slowLarge / 1000} µs"
  -- Eight times the work allows a factor of three for allocation and scheduling.
  unless fastLarge ≤ 24 * fastSmall + 5000000 do
    throw (IO.userError "span construction and comparison exceeded the linear scaling budget")
  unless slowLarge > 24 * slowSmall + 5000000 do
    throw (IO.userError "the scaling budget accepted the planted prefix rescans")
