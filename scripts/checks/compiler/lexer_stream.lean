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

private def bridgeControl (filePath : SourcePath) : IO Unit := do
  let names := wordTokens.map Prod.fst
  let identifiers := names.map (· ++ "x") ++ intrinsicNames ++
    ["Option", "Result", "Heap", "Vec", "String", "math", "self", "as", "vec_pop", "alloc_array", "free_array", "string_length"]
  let spellings := names ++ identifiers
  let source := " //é\r\n" ++ String.intercalate " " spellings ++
    " 42u8 0x2au16 \"a\\n\" '\\t' 'loop //tail\r\n"
  let bytes := source.toUTF8
  let files ← project [filePath, ← path "tokens.con"] (.ofBytes bytes.data.toList)
  let input : ConcreteSpec.StageInterface.Source := ⟨files, ⟨1⟩, ConcreteSpec.StageInterface.slice0⟩
  let .ok stream := Lex.lexStream input | throw (IO.userError "refused bridge fixture")
  unless stream.parserView.invalidSpan.isNone && stream.source.entryFile == input.entryFile &&
      (ConcreteSpec.StageInterface.reconstruct input.entryFile stream.tokens stream.trivia).any
        (·.data == bytes.data) do throw (IO.userError "entry, coordinates or lossless replay changed")
  unless stream.tokens.size == spellings.length + 5 &&
      (stream.trivia[0]?.getD #[]).toList.map (·.value.data) == [" ".toUTF8.data, "//é".toUTF8.data, "\r\n".toUTF8.data] &&
      (stream.trivia.back?.getD #[]).toList.map (·.value.data) == [" ".toUTF8.data, "//tail".toUTF8.data, "\r\n".toUTF8.data] do
    throw (IO.userError "changed token count or trivia boundaries")
  for ((spelling, terminal), i) in wordTokens.zipIdx do
    let some token := stream.tokens[i]? | throw (IO.userError spelling)
    unless token.decoded.value.kind == terminal && token.spelling.data == spelling.toUTF8.data do
      throw (IO.userError s!"lost reservation or spelling for {spelling}")
  for (spelling, i) in identifiers.zipIdx do
    let some token := stream.tokens[names.length + i]? | throw (IO.userError spelling)
    unless token.decoded.value == .name spelling do throw (IO.userError s!"split or reserved {spelling}")
  let payloads := (stream.tokens.toList.drop spellings.length).map (·.decoded.value)
  unless payloads == [.integer 42 .decimal (some .u8), .integer 42 .hexadecimal (some .u16),
      .stringLiteral [97,10], .byteLiteral 9, .label ⟨"loop"⟩] do
    throw (IO.userError "changed decoded integer, string, byte or label")
  let .error (.missingFile ⟨2⟩) := Lex.lexStream {input with entryFile := ⟨2⟩}
    | throw (IO.userError "missing entry did not retain its file ID")
  for spelling in ["", " //é\r\n"] do
    let emptyFile ← project [filePath] (.ofBytes spelling.toUTF8.data.toList)
    let .ok empty := Lex.lexStream {input with project := emptyFile, entryFile := ⟨0⟩}
      | throw (IO.userError "refused empty or trivia-only source")
    unless empty.tokens.isEmpty && empty.trivia.size == 1 &&
        (ConcreteSpec.StageInterface.reconstruct ⟨0⟩ empty.tokens empty.trivia).any
          (·.data == spelling.toUTF8.data) do throw (IO.userError "lost EOF trivia")
  let malformed ← project [filePath] (.ofBytes "return ; @".toUTF8.data.toList)
  let .error (.malformed "unexpected character at byte 9") :=
    Lex.lexStream {input with project := malformed, entryFile := ⟨0⟩}
    | throw (IO.userError "scanner failure lost its location or precedence")
  for prefixText in ["return ident", "return -", "return \"é"] do
    let prefixBytes := prefixText.toUTF8.data.toList
    let invalid ← project [filePath] (.ofBytes (prefixBytes ++ [0]))
    let .error (.malformed diagnostic) := Lex.lexStream {input with project := invalid, entryFile := ⟨0⟩}
      | throw (IO.userError "accepted NUL")
    unless diagnostic.startsWith s!"invalid source at byte {prefixBytes.length}: " do
      throw (IO.userError s!"wrong NUL offset: {diagnostic}")
  let large ← project [filePath] (.ofBytes (" " ++ String.ofList (List.replicate (Lex.integerDigitLimit+1) '9')).toUTF8.data.toList)
  let .error (.digitLimit 1 limit) := Lex.lexStream {input with project := large, entryFile := ⟨0⟩}
    | throw (IO.userError "digit limit became a syntax error")
  unless limit == Lex.integerDigitLimit do throw (IO.userError "wrong digit limit")

def main : IO Unit := do
  let first ← path "a.con"
  bridgeControl first
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
