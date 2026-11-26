import Concrete.Stages
import ConcreteSpecEvidence.Ident
import Lean.Elab.Command
import Lean.Util.CollectAxioms

open ConcreteSpec.Core SourcePathSpan

private def loadProject (bytes : List UInt8) : Project :=
  Project.singleton mainSourcePath (.ofBytes bytes)

private def editedProject := loadProject [97, 9, 99]

-- The former public constructor accepted this payload under the edited project.
/--
error: Invalid `⟨...⟩` notation: Constructor for `ConcreteSpec.Core.SourcePathSpan.ScopedSpan` is marked as private
-/
#guard_msgs in
example : ScopedSpan editedProject := ⟨scopedTabSpan.toSpan⟩

-- The constructor tactic can still be used for valid issuance, but must
-- establish the originating file's location rules as well as its presence.
example : ScopedSpan editedProject := by
  constructor
  case toSpan => exact scopedTabSpan.toSpan
  case issued =>
    fail_if_success
      exact (show scopedTabSpan.fileId.index < editedProject.entries.size from by decide)
    fail_if_success decide
    exact ⟨File.load mainSourcePath (.ofBytes [97, 9, 99]), by decide, rfl⟩

-- The former normative module exported an unrestricted past-end issuer.
example : True := by
  fail_if_success
    exact (fun (_ : (project : Project) → SourcePath → Nat → Nat → Option (ScopedSpan project)) =>
      True.intro) Controls.projectPastEnd
  trivial

example : bindSource tabProject editedProject scopedTabSpan = none := by decide

example (span : ScopedSpan tabProject)
    (raw : span.toSpan = Controls.pastEnd) : False := by
  have issued := span.issued
  rw [raw] at issued
  obtain ⟨file, valid, found⟩ := issued
  have fileEq : file = File.load mainSourcePath tabSource := Option.some.inj found.symm
  subst file
  exact (by decide : ¬ Controls.pastEnd.LocationOk tabSource) valid

example :
    let file := File.load mainSourcePath tabSource
    file.matchesLocation Controls.pastEnd = false ∧
      file.matchesLocation Controls.wrongLine = false ∧
      file.matchesLocation Controls.wrongColumn = false := by decide

open Lean in
run_cmd do
  for theoremName in [``Controls.bindPastEnd_violates_correct,
      ``Controls.bindIgnoringLine_violates_correct, ``Controls.bindIgnoringColumn_violates_correct,
      ``Project.findId_eq, ``projectSpan_correct, ``projectSpan_progress, ``projectSpan_safety,
      ``bindSource_correct] do
    if (← Lean.collectAxioms theoremName).contains ``Classical.choice then
      logError m!"{theoremName} depends on Classical.choice"

private def fixturePath (name : String) : IO SourcePath :=
  match SourcePath.parse (.ofBytes name.toUTF8.data.toList) with
  | .ok path => pure path
  | .error _ => throw (IO.userError s!"invalid fixture path {name}")

private def pathFor (index : Nat) : IO SourcePath :=
  fixturePath s!"file{index}.con"

private def loadFiles (count : Nat) (source : SourceText) (last : List SourcePath := []) : IO Project := do
  let files ← (List.range count).mapM pathFor
  let paths := files ++ last
  match checked : SourcePath.Project.check paths with
  | .error error => throw (IO.userError s!"fixture project failed admission: {repr error}")
  | .ok () => return Project.load (paths.map (File.load · source)).toArray (by
      simpa [List.map_map, Function.comp_def, File.load] using
        SourcePath.Project.check_safety paths checked)

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
  let edited := SourceText.ofBytes "fn main() -> u8 { return 20 + 23; }".toUTF8.data.toList
  let editedProject ← loadFiles 2 edited
  if (Concrete.Stages.compileFile project ⟨2⟩).isOk then
    throw (IO.userError "compiler accepted an out-of-range file ID")
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
    let some projected := out.operatorSpan 0
      | throw (IO.userError "operator range could not be projected")
    let some bound := bindSource out.project project projected
      | throw (IO.userError "operator range could not be bound")
    -- Reissuing the raw range in the edited table reproduces the old false acceptance.
    let some reissued := projectSpanAt editedProject op.fileId op.start op.stop
      | throw (IO.userError "edited fixture changed the operator range")
    unless decide (reissued.toSpan = projected.toSpan) &&
        (bindSource editedProject editedProject reissued).isSome do
      throw (IO.userError "edited fixture did not reproduce raw-range rebinding")
    unless (bindSource out.project editedProject projected).isNone do
      throw (IO.userError "compiled operator span rebound to changed bytes at the same path")
    let expected ← pathFor index
    unless decide (op.fileId = fileId ∧ bound.path = expected ∧
        op.line = projected.line ∧ op.column = projected.col) do
      throw (IO.userError "operator site did not resolve to its issuing file and coordinates")
    sites := op :: sites
  let [second, first] := sites | throw (IO.userError "missing operator sites")
  unless decide (first ≠ second ∧ first.start = second.start ∧ first.stop = second.stop ∧
      first.line = second.line ∧ first.column = second.column) do
    throw (IO.userError "identical text in different files must have distinct operator spans")

-- Rebuilding an output with an edited project and the original payload must fail.
example (_out : Concrete.Stages.Output) (_replacement : Concrete.Stages.Input) : True := by
  fail_if_success
    have _swapped : Concrete.Stages.Output := by
      constructor
      case project => exact _replacement.project
      case fileId => exact _out.fileId
      case tokens => exact _out.tokens
      case ast => exact _out.ast
      case core => exact _out.core
      case evidence => exact _out.evidence
      case mir => exact _out.mir
      case memory => exact _out.memory
      case bir => exact _out.bir
      all_goals first | exact _out.provenance | trivial
    trivial
  trivial

private def checkSingleFile : IO Unit := do
  let source := "fn main() -> u8 { return 20 + 22; }".toUTF8
  let edited := "fn main() -> u8 { return 20 + 23; }".toUTF8
  let .ok input := Concrete.Stages.Input.load "main.con" source
    | throw (IO.userError "single-file loader refused the source")
  let .ok replacement := Concrete.Stages.Input.load "main.con" edited
    | throw (IO.userError "single-file loader refused the edited source")
  let .ok out := Concrete.Stages.compile input
    | throw (IO.userError "compiler refused the single-file source")
  let some span := out.operatorSpan 0
    | throw (IO.userError "single-file compile lost the elaborated operator site")
  unless (bindSource out.project input.project span).isSome &&
      (bindSource out.project replacement.project span).isNone do
    throw (IO.userError "single-file compile lost the issuing source bytes")

private def checkSpan {project : Project} (id : FileId) (offset : Nat)
    (result : Option (ScopedSpan project)) : IO (ScopedSpan project) := do
  let some span := result
    | throw (IO.userError s!"missing span at file {id.index}, byte {offset}")
  let expected : Span :=
    ⟨id, offset, offset + 1, Nat.le_succ _, 1, offset + 1, Nat.zero_lt_succ _, Nat.zero_lt_succ _⟩
  unless decide (span.toSpan = expected) do
    throw (IO.userError s!"incorrect span at file {id.index}, byte {offset}")
  return span

private instance : MonadTick (StateM Nat) where
  tick := modify (· + 1)

-- The counting and production instantiations use the same definitions.
example (file : File) (offset : Nat) (bounded : offset ≤ file.length) :
    file.positionAt offset bounded =
      (File.indexedCoordinate (m := Id) file offset bounded).val := rfl

example (project : Project) (path : SourcePath) :
    project.findId path = project.findIdM (m := Id) path := rfl

example (project : Project) (id : FileId) (start stop : Nat) :
    projectSpanAt project id start stop = projectSpanAtM (m := Id) project id start stop := rfl

example (project : Project) (path : SourcePath) (start stop : Nat) :
    projectSpan project path start stop = projectSpanM (m := Id) project path start stop := rfl

-- Both indexed and planted runs use the same per-span budget predicate.
private def checkWork {project : Project} (id : FileId) (offset budget : Nat)
    (result : Option (ScopedSpan project) × Nat) : IO Bool := do
  discard (checkSpan id offset result.1)
  return result.2 ≤ budget

private def spanWork (project : Project) (size : Nat) (planted : Bool) : IO Unit := do
  let mut total := 0
  let mut rejected := 0
  for offset in [:size] do
    -- Substitute the counted reference fold at the production issuer’s coordinate access.
    let issue : StateM Nat (Option (ScopedSpan project)) := if planted then
      projectSpanAtM project ⟨0⟩ offset (offset + 1) File.prefixCoordinate
    else
      projectSpanAtM project ⟨0⟩ offset (offset + 1)
    let result := issue.run 0
    if planted then
      let expected := offset + 1
      unless result.2 == expected do
        throw (IO.userError s!"prefix scan charged {result.2} reads at {offset}, expected {expected}")
    unless ← checkWork ⟨0⟩ offset 2 result do
      rejected := rejected + 1
    total := total + result.2
  let expectedRejected := if planted then size - 2 else 0
  unless rejected == expectedRejected do
    throw (IO.userError s!"span work budget rejected {rejected}/{size}, expected {expectedRejected}")
  unless planted || total == 2 * size do
    throw (IO.userError s!"indexed span issuance charged {total} reads, expected {2 * size}")
  IO.println s!"span work (planted={planted}, {size} spans): {total} operations, {rejected} over budget"

-- Replay the former first-match lookup through the same indexed read primitive.
private def scanProject {m : Type → Type} [Monad m] [MonadTick m]
    (project : Project) (path : SourcePath) (index : Nat) : Nat → m (Option FileId)
  | 0 => pure none
  | fuel + 1 => do
    if bounded : index < project.entries.size then
      let entry ← readArray project.entries index bounded
      if entry.val.path = path then return some ⟨index⟩
      scanProject project path (index + 1) fuel
    else return none

private def projectWork (project : Project) (paths : Array SourcePath) (planted : Bool) : IO Unit := do
  let mut total := 0
  let mut rejected := 0
  for index in [:paths.size] do
    let some path := paths[index]? | throw (IO.userError "missing fixture path")
    let issue : StateM Nat (Option (ScopedSpan project)) := if planted then
      projectSpanM project path 0 1 (fun query => scanProject project query 0 project.entries.size)
    else
      projectSpanM project path 0 1
    let result := issue.run 0
    -- For file<n>.con, the seven letters each have one edge, with a possible
    -- second root edge. Each remaining byte has at most ten digits and a dot
    -- to compare. Also charge nodes, including the terminal, and both table reads.
    let bytes := path.raw.bytes.length
    let budget := bytes + 8 + 11 * (bytes - 7) + 3
    unless ← checkWork ⟨index⟩ 0 budget result do
      rejected := rejected + 1
    total := total + result.2
  let controlRejected := paths.size / 2 < rejected
  unless (if planted then controlRejected else rejected == 0) do
    throw (IO.userError s!"path work budget rejected {rejected}/{paths.size} (planted={planted})")
  IO.println s!"path work (planted={planted}, {paths.size} spans): {total} operations, {rejected} over budget"

private def checkDistinctPrefix (project : Project) (id : FileId) (last missing : SourcePath) : IO Unit := do
  let result := (projectSpanM (m := StateM Nat) project last 0 1).run 0
  -- Two root edges and one edge for each remaining byte.
  let budget := 2 * last.raw.bytes.length + 4
  unless ← checkWork id 0 budget result do
    throw (IO.userError s!"distinct-prefix lookup charged {result.2}, budget {budget}")
  let (absent, work) := (project.findIdM (m := StateM Nat) missing).run 0
  unless absent.isNone && work == 3 do
    throw (IO.userError s!"absent-prefix lookup charged {work}, expected a root visit and two comparisons")
  IO.println s!"distinct/absent prefix: {result.2}/{work} operations with {project.entries.size} files"

-- Plant the root list produced when insertion keeps replaced edges.
private def checkDuplicateEdges (count : Nat) : IO Unit := do
  let edges := List.replicate count ('f'.toNat, 0) ++ [('z'.toNat, count)]
  let (slot, work) := (lookupEdgeM (m := StateM Nat) 'z'.toNat edges).run 0
  unless slot == some count && work == count + 1 && work > 2 do
    throw (IO.userError s!"duplicate-prefix edge scan charged {work}, expected {count + 1}")

def main : IO Unit := do
  checkLexer
  checkCompilerFiles
  checkSingleFile
  let short := loadProject [97]
  let long := loadProject (List.replicate 32768 97)
  let small ← checkSpan ⟨0⟩ 0 (projectSpanAt short ⟨0⟩ 0 1)
  let large ← checkSpan ⟨0⟩ 0 (projectSpanAt long ⟨0⟩ 0 1)
  unless decide (small.toSpan = large.toSpan) && reprStr small == reprStr large do
    throw (IO.userError "the span payload changed when the file grew")
  spanWork long 2048 false
  spanWork long 16384 false
  spanWork long 512 true
  spanWork long 4096 true
  let last ← fixturePath "z.con"
  let missing ← fixturePath "missing.con"
  let few ← loadFiles 128 (.ofBytes [97]) [last]
  let many ← loadFiles 1024 (.ofBytes [97]) [last]
  projectWork few ((few.entries.extract 0 128).map File.path) false
  projectWork many ((many.entries.extract 0 1024).map File.path) false
  projectWork few ((few.entries.extract 0 128).map File.path) true
  projectWork many ((many.entries.extract 0 256).map File.path) true
  checkDistinctPrefix few ⟨128⟩ last missing
  checkDistinctPrefix many ⟨1024⟩ last missing
  checkDuplicateEdges 1024
