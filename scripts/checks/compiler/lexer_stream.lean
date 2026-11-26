import Concrete.Lex
import ConcreteSpec.Proofs.Ident

open Concrete ConcreteSpec.Surface0
open ConcreteSpec.Core (FileId SourcePath SourceText)
open ConcreteSpec.Core.SourcePathSpan

-- Production must keep a space and a second slash pair inside the comment.
example : (Lex.lexLossless "//a //b".toUTF8).map Lex.LosslessStream.pieces =
    .ok [.lineComment ⟨0, 7, 1, 1, ⟨0⟩⟩] := rfl

-- Even if the ranges cover every byte, the first comment in the split is invalid.
example : ¬ Lex.PieceMeaning ⟨0⟩ {} false [47, 47, 97] [32, 47, 47, 98]
    (.lineComment ⟨0, 3, 1, 1, ⟨0⟩⟩) := by
  intro meaning
  cases meaning with
  | comment _ closed =>
    have comment := closed rfl
    cases comment with
    | intro body =>
      have maximal : CommentBody [97, 32, 47, 47, 98] [] := by
        repeat first | exact .eof | apply CommentBody.cons <;> try decide
      exact (by decide : [32, 47, 47, 98] ≠ []) (commentBody_unique body maximal)

-- Slash is a punctuation token, but a comment opener takes precedence.
example : ¬ Lex.PieceMeaning ⟨0⟩ {} false [47] [47, 97]
    (.token (.parsed (.fixed .slash)) ⟨0, 1, 1, 1, ⟨0⟩⟩) := by
  intro meaning
  cases meaning with
  | token stopped _ _ =>
    have skipped : Trivia [47, 47, 97] [] := Lex.skipTrivia_sound rfl
    cases Lex.trivia_unique stopped skipped

example : ¬ (Lex.LosslessValue.parsed (.fixed .stringLiteral)).Payload [34, 34] [34, 34] [] := by
  intro payload
  exact payload.1 rfl

private def path (name : String) : IO SourcePath :=
  match SourcePath.parse (.ofBytes name.toUTF8.data.toList) with
  | .ok value => pure value
  | .error _ => throw (IO.userError s!"invalid lexer fixture path {name}")

private def project (paths : List SourcePath) (source : SourceText) : IO Project :=
  match checked : SourcePath.Project.check paths with
  | .error _ => throw (IO.userError "invalid lexer fixture project")
  | .ok () => pure <| Project.load (paths.map (File.load · source)).toArray (by
      simpa [List.map_map, Function.comp_def, File.load] using
        SourcePath.Project.check_safety paths checked)

private def firstPiece (origin : Project) (id : FileId) : IO (Lex.Piece × ScopedSpan origin) := do
  let .ok stream := Lex.lexProject origin id | throw (IO.userError "project lexer refused a fixture")
  match h : stream.pieces with
  | [] => throw (IO.userError "project lexer dropped the first piece")
  | piece :: _ =>
    let some issued := stream.scopePiece piece (by rw [h]; simp)
      | throw (IO.userError "failed to scope the first piece")
    pure (piece, issued)

def main : IO Unit := do
  let first ← path "a.con"
  let second ← path "b.con"
  let source := SourceText.ofBytes "//a //b\r\nword".toUTF8.data.toList
  let origin ← project [first, second] source
  let otherName ← project [second] source
  let reordered ← project [second, first] source
  let edited ← project [first] (.ofBytes "//z //b\r\nword".toUTF8.data.toList)
  let (a, aSpan) ← firstPiece origin ⟨0⟩
  let (b, _) ← firstPiece origin ⟨1⟩
  unless a.span.fileId == ⟨0⟩ && b.span.fileId == ⟨1⟩ && a != b do
    throw (IO.userError "identical bytes permitted substituting a piece from another file")
  unless (bindSource origin origin aSpan).isSome &&
      (bindSource origin reordered aSpan).isSome &&
      (bindSource origin otherName aSpan).isNone &&
      (bindSource origin edited aSpan).isNone do
    throw (IO.userError "lexer piece did not bind to its exact path and bytes")
