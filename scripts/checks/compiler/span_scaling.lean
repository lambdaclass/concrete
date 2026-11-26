import Concrete.Stages
import ConcreteSpec.Proofs.Ident

open ConcreteSpec.Core SourcePathSpan

private def loadProject (bytes : List UInt8) : Project :=
  ⟨#[File.load mainSourcePath (.ofBytes bytes)], by simp [SourcePath.Project.Reference.Admitted]⟩

private def editedProject := loadProject [97, 9, 99]

-- The former public constructor accepted this payload under the edited project.
/--
error: Invalid `⟨...⟩` notation: Constructor for `ConcreteSpec.Core.SourcePathSpan.ScopedSpan` is marked as private
-/
#guard_msgs in
example : ScopedSpan editedProject := ⟨scopedTabSpan.toSpan⟩

example : bindSource tabProject editedProject scopedTabSpan = none := by decide

private def pathFor (index : Nat) : IO SourcePath :=
  match SourcePath.parse (.ofBytes s!"file{index}.con".toUTF8.data.toList) with
  | .ok path => pure path
  | .error _ => throw (IO.userError s!"invalid fixture path for slot {index}")

private def loadFiles (count : Nat) (source : SourceText) : IO Project := do
  let paths ← (List.range count).mapM pathFor
  match checked : SourcePath.Project.check paths with
  | .error error => throw (IO.userError s!"fixture project failed admission: {repr error}")
  | .ok () => return ⟨(paths.map (File.load · source)).toArray,
      by simpa [List.map_map, Function.comp_def, File.load] using
        SourcePath.Project.check_safety paths checked⟩

private def checkLexer : IO Unit := do
  let src := "a\r\nb\rc\nd".toUTF8
  let fileId : FileId := ⟨3⟩
  let .ok tokens := Concrete.Lex.lex src fileId
    | throw (IO.userError "lexer refused the line-ending control")
  let observed := tokens.map fun t => (t.span.start, t.span.stop, t.span.line, t.span.column)
  unless observed == [(0, 1, 1, 1), (3, 4, 2, 1), (5, 6, 3, 1), (7, 8, 4, 1)] &&
      tokens.all (fun t => decide (t.span.fileId = fileId)) do
    throw (IO.userError s!"lexer reported unexpected spans: {repr tokens}")

private def checkCompilerFiles : IO Unit := do
  let source := SourceText.ofBytes "fn main() -> u8 { return 20 + 22; }".toUTF8.data.toList
  let project ← loadFiles 2 source
  let mut sites := []
  for index in [:2] do
    let fileId : FileId := ⟨index⟩
    let .ok out := Concrete.Stages.compileFile project fileId
      | throw (IO.userError s!"compiler refused file {index}")
    let [site] := out.core.sites
      | throw (IO.userError s!"expected one addition site in file {index}")
    let [.ret (.add _ _ parsed) statement] := out.ast.body
      | throw (IO.userError s!"expected one return addition in file {index}")
    let some token := out.tokens.find? (fun t => t.value.kind == .plus)
      | throw (IO.userError s!"missing addition token in file {index}")
    unless decide (token.span = parsed ∧ parsed = site.operator ∧ statement.fileId = fileId) do
      throw (IO.userError "parser or elaborator changed the source span")
    let op := site.operator
    let some projected := projectSpanAt project op.fileId op.start op.stop
      | throw (IO.userError "operator range could not be projected")
    let some bound := bindSource project project projected
      | throw (IO.userError "operator range could not be bound")
    let expected ← pathFor index
    unless decide (op.fileId = fileId ∧ bound.path = expected ∧
        op.line = projected.line ∧ op.column = projected.col) do
      throw (IO.userError "operator site did not resolve to its issuing file and coordinates")
    sites := op :: sites
  let [second, first] := sites | throw (IO.userError "missing operator sites")
  unless decide (first ≠ second ∧ first.start = second.start ∧ first.stop = second.stop ∧
      first.line = second.line ∧ first.column = second.column) do
    throw (IO.userError "identical text in different files must have distinct operator spans")

private def checkSpan (project : Project) (id : FileId) (offset : Nat) : IO (ScopedSpan project) := do
  let some span := projectSpanAt project id offset (offset + 1)
    | throw (IO.userError s!"missing span at file {id.index}, byte {offset}")
  let expected : Span :=
    ⟨id, offset, offset + 1, Nat.le_succ _, 1, offset + 1, Nat.zero_lt_succ _, Nat.zero_lt_succ _⟩
  unless decide (span.toSpan = expected) do
    throw (IO.userError s!"incorrect span at file {id.index}, byte {offset}")
  return span

private def spanWork (project : Project) (size : Nat) (rescan : Bool) : IO Unit := do
  for offset in [:size] do
    let span ← checkSpan project ⟨0⟩ offset
    if rescan then
      let some file := project.fileAt span.fileId
        | throw (IO.userError "missing file for prefix-rescan control")
      unless file.source.columnAt offset == span.col do
        throw (IO.userError "prefix scan disagreed")

private def projectWork (project : Project) (paths : Array SourcePath) (files : List File)
    (scan : Bool) : IO Unit := do
  for _ in [:32] do
    for index in [:paths.size] do
      let id : FileId := ⟨index⟩
      if scan then
        -- Replay both passes of the former per-span path lookup.
        let some path := paths[index]? | throw (IO.userError "missing fixture path")
        let matchPath := fun file : File => decide (file.path = path)
        let some found := files.findIdx? matchPath
          | throw (IO.userError "path search missed the issuing file")
        let some file := files[found]?
          | throw (IO.userError "list lookup missed the issuing file")
        unless found == index && decide (file.path = path) do
          throw (IO.userError "path search found the wrong file")
      discard (checkSpan project id 0)

private def elapsed (work : IO Unit) : IO Nat := do
  let start ← IO.monoNanosNow
  work
  return (← IO.monoNanosNow) - start

private def checkGrowth (label : String) (small large : IO Unit) (planted : Bool) : IO Unit := do
  let mut samples := #[]
  for _ in [:3] do
    let smallTime ← elapsed small
    let largeTime ← elapsed large
    samples := samples.push (smallTime, largeTime)
  let (smallTime, largeTime) := (samples.qsort fun a b => a.2 * b.1 < b.2 * a.1)[1]!
  IO.println s!"{label}: {smallTime / 1000} → {largeTime / 1000} µs"
  -- Eight times the work allows fifty percent overhead in the median paired ratio.
  unless (largeTime ≤ 12 * smallTime) != planted do
    throw (IO.userError s!"{label} did not distinguish indexed access from rescanning")

def main : IO Unit := do
  checkLexer
  checkCompilerFiles
  let short := loadProject [97]
  let long := loadProject (List.replicate 32768 97)
  let small ← checkSpan short ⟨0⟩ 0
  let large ← checkSpan long ⟨0⟩ 0
  unless decide (small.toSpan = large.toSpan) && reprStr small == reprStr large do
    throw (IO.userError "the span payload changed when the file grew")
  checkGrowth "span construction and comparison (2048 → 16384 spans)"
    (spanWork long 2048 false) (spanWork long 16384 false) false
  checkGrowth "planted byte-prefix scans (512 → 4096 spans)"
    (spanWork long 512 true) (spanWork long 4096 true) true
  let few ← loadFiles 128 (.ofBytes [97])
  let many ← loadFiles 1024 (.ofBytes [97])
  checkGrowth "project projection (128 → 1024 files, 32 spans per file)"
    (projectWork few (few.entries.map File.path) [] false)
    (projectWork many (many.entries.map File.path) [] false) false
  let smallFiles := few.files.take 32
  let largeFiles := many.files.take 256
  checkGrowth "planted path searches and list indexing (32 → 256 files, 32 spans per file)"
    (projectWork few (smallFiles.map File.path).toArray smallFiles true)
    (projectWork many (largeFiles.map File.path).toArray largeFiles true) true
