import Concrete

/-!
Emitter for the manifest-driven test gate (`scripts/checks/compiler/tests.sh`). PROVISIONAL.

`lake env lean --run scripts/checks/compiler/slice0_bir.lean list` prints the hand-written BIR examples;
`... emit NAME DIR TARGET EXPECT...` takes the expectation from `tests/manifest.tsv` and checks the
example in Lean (well formed; its BIR run gives that outcome; LCB's own evaluator on the adapted
body returns the same result word; a trap site's kind is the expected kind) and writes
`DIR/llvm.ll` (body + runtime entry), `DIR/lcb_body.s` (the adapted body, assembled by LCB in
process for TARGET: `x86_64-linux`, `aarch64-linux` or `aarch64-macos`), `DIR/lcb_rt.ssa` (the
runtime entry as QBE text, for LCB's command line), `DIR/lcb_body.ssa` (a debug dump only), and the
observation the executables must show: `DIR/status` (the exit status) and `DIR/stderr` (the exact
bytes on descriptor 2; descriptor 1 must stay empty).

Two controls exercise the gate itself: `... loop DIR TARGET` emits a well-formed BIR body that
never returns (`DIR/status` is `incomplete`), and `... invalid DIR TARGET` emits a body returning a
word that encodes nothing, which the runtime must refuse (`Runtime.invalidLine`, then abort).
-/

open Concrete.BIR Concrete.Backend

-- The source and candidate agree on the complete checked-add site entry.
private def sumSource (a b : Nat) : Concrete.Memory.Program :=
  ⟨[⟨⟨⟨0⟩, Concrete.Memory.u8Layout⟩,
    .add (.lit a) (.lit b) ⟨⟨0⟩, ⟨59, 64, 3, 12, ⟨0⟩⟩⟩ none⟩], .var ⟨0⟩⟩

private def sumPath := "tests/slice0/sum.con"
private def sumWitness : Concrete.SSA.Witness := [[⟨0⟩]]
private def correspondenceError : Except Concrete.Refusal Program :=
  .error (.internal "SSA correspondence check failed")

example : Concrete.SSA.validate sumPath (sumSource 20 22) Examples.answer sumWitness =
    .ok Examples.answer := by cbv

-- A different return value and a site with the right kind but wrong provenance are refused.
example : Concrete.SSA.validate sumPath { sumSource 20 22 with result := .lit 41 }
    Examples.answer sumWitness = correspondenceError := by cbv

example : Concrete.SSA.validate "other.con" (sumSource 20 22) Examples.answer sumWitness =
    correspondenceError := by cbv

-- Every parameter/argument pair needs its witness slot, and every emitted block is consumed.
example : Concrete.SSA.validate sumPath (sumSource 20 22) Examples.answer [[]] =
    correspondenceError := by cbv

example : Concrete.SSA.validate sumPath (sumSource 20 22)
    { Examples.answer with blocks := Examples.answer.blocks ++ [⟨[], [], .ret (.imm .u8 42)⟩] }
    sumWitness = correspondenceError := by cbv

example : Concrete.SSA.validate sumPath (sumSource 20 22)
    { Examples.answer with sites := [] } sumWitness = correspondenceError := by cbv

-- Even a continuation the closed input cannot reach must return the checked operand.
private def brokenContinuation : Program :=
  let candidate := Examples.checkedSum 200 56
  { candidate with blocks := candidate.blocks.take 2 ++ [⟨[(2, .int .u8)], [], .ret (.imm .u8 0)⟩] }

example : run brokenContinuation 4 = .trapped 0 := by cbv

example : Concrete.SSA.validate sumPath (sumSource 200 56)
    brokenContinuation sumWitness = correspondenceError := by cbv

private def twoSlots : Concrete.Memory.Program :=
  let sum := sumSource 20 22
  { sum with stores := sum.stores ++ [⟨⟨⟨1⟩, Concrete.Memory.u8Layout⟩,
      .add (.lit 2) (.lit 3) ⟨⟨1⟩, ⟨65, 70, 4, 3, ⟨0⟩⟩⟩ none⟩] }

example :
    (match Concrete.SSA.lower sumPath twoSlots with
     | .ok bir => run bir (bir.blocks.length + 1)
     | .error _ => .stuck) = .exited 42 := by cbv

-- The second success edge carries the new result before the earlier slot.
example :
    (do
      let bir ← Concrete.SSA.lower sumPath twoSlots
      Concrete.SSA.validate sumPath twoSlots bir [[⟨0⟩], [⟨0⟩, ⟨1⟩]]) =
    correspondenceError := by cbv

inductive Expectation where
  | exit (status : Nat) (stderr : ByteArray)
  | trap (kind : String) (index : Nat) (stderr : ByteArray)

def Expectation.stderr : Expectation → ByteArray
  | .exit _ bytes | .trap _ _ bytes => bytes

def hexDigit (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else none

def decodeStderr (text : String) : Option ByteArray :=
  if text == "-" then some ByteArray.empty
  else if text.isEmpty then none
  else go text.toList ByteArray.empty
where
  go : List Char → ByteArray → Option ByteArray
    | [], bytes => some bytes
    | hi :: lo :: rest, bytes => do
      let value := 16 * (← hexDigit hi) + (← hexDigit lo)
      go rest (bytes.push (UInt8.ofNat value))
    | _, _ => none

def parseExpectation : List String → Option Expectation
  | ["exit", status, stderr] => do
    let n ← status.toNat?
    if n ≤ 255 then return .exit n (← decodeStderr stderr) else none
  | ["trap", kind, site, stderr] => do
    let bytes ← decodeStderr stderr
    if bytes.isEmpty then none else return .trap kind (← site.toNat?) bytes
  | _ => none

def writeExpectedBytes (dir : System.FilePath) (status : String) (stderr : ByteArray) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeBinFile (dir / "stderr") stderr

def writeExpected (dir : System.FilePath) (status stderr : String) : IO Unit :=
  writeExpectedBytes dir status stderr.toUTF8

-- Retire the old fixture label together with the 4(e) observation interface.
def fixtureTrapKind : ConcreteSpec.Trap → String
  | .arithmeticOverflow => "arithmeticOverflow"

-- For planted fixture controls, these exit codes distinguish a rejected observation from a
-- schema error, compilation failure, timeout or harness failure.
def checkObservation (expected : Expectation) (got : Outcome) (sites : SiteTable) :
    Except UInt32 Nat :=
  match expected, got with
  | .exit status stderr, .exited actual => do
    if status != actual then throw 10
    unless stderr.isEmpty do throw 16
    return status
  | .trap kind expectedIndex stderr, .trapped index => do
    if index != expectedIndex then throw 11
    let some entry := sites[index]? | .error 10
    if fixtureTrapKind entry.kind != kind then throw 12
    if (Runtime.trapLine entry ++ "\n").toUTF8.data != stderr.data then throw 16
    return 134
  | _, _ => .error 10

def lcbProfileStatus (job : LeanCompilerBackend.Verified.FunctionJob)
    (defaultCode : UInt32) : UInt32 :=
  match LeanCompilerBackend.Verified.validateJob job with
  | .error (.admission (.unsupported _))
  | .error (.admission (.unsupportedHighMultiplyWidth ..))
  | .error (.unsupported _) | .error (.unsupportedTarget _) => 17
  | _ => defaultCode

/-- Write the LCB leg: the body assembled in process, the runtime entry as QBE text. -/
def writeLcb (dir : System.FilePath) (target : LCB.Target)
    (emitted : LeanCompilerBackend.Verified.Source.Function × SiteTable) : IO Bool := do
  let (f, sites) := emitted
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job := LCB.job f
  IO.FS.writeFile (dir / "lcb_job.txt") (reprStr job ++ "\n")
  -- On every host, LCB's verified first path (the `aarch64-macos` target) must admit the body.
  if let .error msg := LCB.assemble .aarch64Macos f then
    IO.eprintln s!"{dir}: LCB first path: {msg.message}"; return false
  match LCB.assemble target f with
  | .error msg => IO.eprintln s!"{dir}: LCB: {msg.message}"; return false
  | .ok asm =>
    IO.FS.writeFile (dir / "lcb_body.s") asm
    IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe sites)
    IO.FS.writeFile (dir / "lcb_body.ssa") (LCB.render f)
    return true

def writeBackends (dir : System.FilePath) (target : LCB.Target) (p : Program) (llvm : String)
    (emitted : LeanCompilerBackend.Verified.Source.Function × SiteTable) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (dir / "llvm.ll") llvm
  writeLcb dir target emitted

def emitArtifact (name : String) (p : Program) (llvm : String)
    (emitted : LeanCompilerBackend.Verified.Source.Function × SiteTable) (dir : System.FilePath)
    (expectation : List String) (target : LCB.Target) : IO UInt32 := do
  let some expected := parseExpectation expectation
    | IO.eprintln s!"{name}: invalid BIR expectation; use exit STATUS STDERR or trap KIND BIR_INDEX STDERR"; return 2
  unless wf p do IO.eprintln s!"{name}: not well formed"; return 1
  let got := run p 1000
  let (f, tbl) := emitted
  let observation := checkObservation expected got tbl
  let .ok status := observation | do
    IO.eprintln s!"{name}: expectation mismatch for BIR outcome {repr got}"
    return match observation with | .error code => code | .ok _ => 1
  let some word := encode got | IO.eprintln s!"{name}: no result word"; return 1
  match LCB.evalBody f 100000 with
  | some w =>
    unless w == word do
      IO.eprintln s!"{name}: LCB's evaluator returns {w}, BIR's result word is {word}"; return 1
  | none => IO.eprintln s!"{name}: LCB's evaluator does not return"; return 1
  unless ← writeBackends dir target p llvm (f, tbl) do
    return lcbProfileStatus (LCB.job f) 1
  writeExpectedBytes dir (toString status) expected.stderr
  return 0

-- A well-formed BIR program can contain an unreachable cycle. LCB admission
-- rejects it even though the adapter can represent it.
private def unreachableCycle : Program :=
  { blocks := [
      { params := [], insts := [], term := .ret (.imm .u8 42) },
      { params := [], insts := [], term := .br 2 [] },
      { params := [], insts := [], term := .br 1 [] }],
    sites := [] }

example : WF unreachableCycle := by decide

private def checkBackendProfile : IO UInt32 := do
  let checks := [
    ("prepareLCB", (Concrete.Stages.prepareLCB unreachableCycle).map fun _ => ()),
    ("aarch64-macos", (LCB.artifact .aarch64Macos unreachableCycle).map fun _ => ()),
    ("aarch64-linux", (LCB.artifact .aarch64Linux unreachableCycle).map fun _ => ()),
    ("x86_64-linux", (LCB.artifact .x86_64Linux unreachableCycle).map fun _ => ())]
  for (stage, result) in checks do
    match result with
    | .ok _ =>
      IO.eprintln s!"{stage}: unreachable cycle passed LCB admission"
      return 1
    | .error e =>
      unless e.exitCode == 2 && e.code == "BACKEND-PROFILE" &&
          e.message == "incomplete (LCB backend: unreachable blocks)" do
        IO.eprintln s!"{stage}: expected BACKEND-PROFILE (2), got {e.code} ({e.exitCode}): {e.message}"
        return 1
  IO.println "ok   LCB admission: BACKEND-PROFILE (2) across all targets"
  for (op, name) in [(LeanCompilerBackend.Verified.Source.Binary.mulHighUnsigned, "mulHighUnsigned"),
      (.mulHighSigned, "mulHighSigned")] do
    let body (width : LeanCompilerBackend.Width) : LeanCompilerBackend.Verified.Source.Function :=
      { resultWidth := width, parameters := #[width, width], valueWidths := #[width, width, width]
        blocks := #[{ phis := #[], terminator := .ret (.local 2)
                      instructions := #[{ destination := 2, width
                                          operation := .binary op (.local 0) (.local 1) }] }] }
    for target in [LCB.Target.x86_64Linux, .aarch64Linux, .aarch64Macos] do
      match LCB.assemble target (body .word) with
      | .error (.backendProfile "LCB" _) => pure ()
      | _ =>
        IO.eprintln s!"{name}: expected BACKEND-PROFILE (2) for word-width high multiply"
        return 1
    for target in [LCB.Target.x86_64Linux, .aarch64Linux] do
      match LCB.assemble target (body .long) with
      | .ok _ =>
        IO.eprintln s!"{name}: Linux compatibility reification emitted assembly"
        return 1
      | .error e =>
        unless e.exitCode == 2 && e.code == "BACKEND-PROFILE" &&
            e.message == s!"incomplete (LCB backend: compatibility reification does not support {name})" do
          IO.eprintln s!"{name}: expected BACKEND-PROFILE (2), got {e.code} ({e.exitCode}): {e.message}"
          return 1
  IO.println "ok   LCB high multiplies: word width refused on all targets, long width refused on Linux"
  return 0

def emitProgram (name : String) (p : Program) (dir : System.FilePath)
    (expectation : List String) (target : LCB.Target) : IO UInt32 := do
  match LCB.emit p with
  | .error (.backendProfile ..) => return 17
  | .error msg => IO.eprintln s!"{name}: adapter: {msg.message}"; return 1
  | .ok emitted => emitArtifact name p (LLVM.module p) emitted dir expectation target

def emit (name : String) (dir : System.FilePath) (expectation : List String)
    (target : LCB.Target) : IO UInt32 := do
  let some (_, p) := Examples.all.find? (·.1 == name)
    | IO.eprintln s!"unknown example {name}"; return 2
  emitProgram name p dir expectation target

def pendingRefusalMatches (kind : String) (refusal : Concrete.Refusal) : Bool :=
  match kind with
  | "syntax" => refusal.code == "SYNTAX"
  | "unsupported_form" => refusal.code == "SLICE-0"
  | "checker" => refusal.code == "CORE-CHECK"
  | _ => false

open ConcreteSpec.Surface0 renaming Span → SourceSpan

structure RefusalExpectation where
  kind : String
  rule : String
  revision : Nat
  primary : SourceSpan
  related : List (String × SourceSpan)

def parseOrigin (fileId : ConcreteSpec.Core.FileId) (text : String) : Option SourceSpan := do
  let [start, stop, line, column] := text.splitOn ":" | none
  let span : SourceSpan := ⟨← start.toNat?, ← stop.toNat?, ← line.toNat?, ← column.toNat?, fileId⟩
  if span.start ≤ span.stop && span.line > 0 && span.column > 0 then some span else none

def parseRefusal (fileId : ConcreteSpec.Core.FileId) (kind rule primary related : String) : Option RefusalExpectation := do
  let [name, revision] := rule.splitOn "@" | none
  let origins ← if related == "-" then some [] else
    (related.splitOn ",").mapM fun item => do
      let [label, location] := item.splitOn "=" | none
      return (label, ← parseOrigin fileId location)
  let rev ← revision.toNat?
  if rev == 0 || (origins.map Prod.fst).eraseDups.length != origins.length then none
  else return ⟨kind, name, rev, ← parseOrigin fileId primary, origins⟩

def checkSourceRefusal (input : Concrete.Stages.Input) (expected : RefusalExpectation)
    (actual : Concrete.Refusal) :
    IO UInt32 := do
  let .source cause := actual | return 13
  let some name := (match expected.kind, expected.rule with
    | "reserved_binder", "UnreservedLocal" => some ``ConcreteSpec.Core.Slice0Rules.UnreservedLocal
    | "unreachable_statement", "STMT-14" => some ``ConcreteSpec.Core.Slice0Rules.ReachableBody
    | _, _ => none) | return 13
  unless (actual.diagnosticAt ⟨name, expected.revision⟩).isSome do return 13
  let .error unchecked := Concrete.Stages.unchecked input | return 13
  if unchecked != actual then return 18
  if cause.primary.surface != expected.primary ||
      cause.related.map (fun (label, span) => (label.text, span.surface)) != expected.related then return 14
  return 0

def emitSource (path actual : String) (dir : System.FilePath) (expectation : List String)
    (target : LCB.Target) : IO UInt32 := do
  let .ok input := Concrete.Stages.Input.load path (← IO.FS.readBinFile actual)
    | IO.eprintln s!"invalid project path: {path}"; return 1
  match expectation, Concrete.Stages.compile input with
  | ["pending-refusal", status, kind], .error refusal =>
    if toString refusal.exitCode == status && pendingRefusalMatches kind refusal then
      IO.println s!"pending diagnostic fields: {path} ({kind})"
      return 0
    return 13
  | ["refuse", status, kind, rule, primary, related], .error (actual@(.source _)) =>
    unless toString actual.exitCode == status do return 13
    let some expected := parseRefusal input.fileId kind rule primary related | return 2
    checkSourceRefusal input expected actual
  | "refuse" :: _, _ | "pending-refusal" :: _, _ => return 13
  | ["llvm", "exit", status, "lcb", "resource", resource, bound, measurement], .ok out =>
    let p := Concrete.Core.toCore out.core.program
    unless status.toNat?.any (fun n => Concrete.Core.run p (p.body.length + 1) == .exit n) do
      return 10
    match Concrete.Stages.prepareLCB out.bir with
    | .error (refusal@(.backendResource "LCB" resourceFound measured limit)) =>
      unless refusal.exitCode == 2 && resource == resourceFound && bound.toNat? == some limit &&
          measurement.toNat? == some measured && measured > limit do return 15
      IO.println s!"ok   {path}: LCB {resourceFound}: {measured} exceeds {limit}"
      return 0
    | .error (.backendProfile ..) => return 17
    | _ => return 15
  | ["trap", kind, _, _], .error (.source (.core ..)) =>
    let .ok tokens := Concrete.Lex.lex input.source input.fileId | IO.eprintln "lex failed"; return 1
    let .ok ast := Concrete.Parse.parse input.eof tokens | IO.eprintln "parse failed"; return 1
    let .ok core := Concrete.Admission.prepareParsed .eligibility ast | IO.eprintln "elaboration failed"; return 1
    let p := Concrete.Core.toCore core.program
    let .trap actualKind := Concrete.Core.run p (p.body.length + 1)
      | IO.eprintln s!"{path}: Core evaluator did not trap"; return 10
    unless fixtureTrapKind actualKind == kind do
      IO.eprintln s!"{path}: Core evaluator trap kind mismatch"; return 12
    match Concrete.Stages.unchecked input with
    | .ok bir => emitProgram path bir dir expectation target
    | .error e => IO.eprintln s!"{path}: unchecked lowering: {e.message}"; return 1
  | "exit" :: _, .ok out =>
    let p := Concrete.Core.toCore out.core.program
    let some expected := parseExpectation expectation | IO.eprintln "invalid source expectation"; return 2
    let .exit n stderr := expected | IO.eprintln "expected source exit"; return 2
    unless stderr.isEmpty do return 16
    unless Concrete.Core.run p (p.body.length + 1) == .exit n do
      IO.eprintln s!"{path}: Core evaluator did not exit with {n}"; return 10
    unless Concrete.Elab.eval out.ast == .exit n && Concrete.Memory.run out.memory == .exit n do
      IO.eprintln s!"{path}: AST or memory evaluator did not exit with {n}"; return 10
    unless out.bir.sites.isEmpty do
      IO.eprintln s!"{path}: admitted slice-0 additions retained trap sites"; return 1
    match Concrete.Stages.prepareLCB out.bir with
    | .ok lcb => emitArtifact path out.bir (LLVM.module out.bir) (lcb, out.bir.sites) dir expectation target
    | .error (.backendProfile ..) => return 17
    | .error e => IO.eprintln s!"{path}: {e.code}: {e.message}"; return 1
  | _, .error e =>
    IO.eprintln s!"{path}: unexpected refusal {e.code}: {e.message}"; return 1
  | _, .ok _ =>
    IO.eprintln s!"{path}: invalid expectation or trapping source passed admission"; return 1

/-- A body that branches back to itself forever. -/
def loopBody : Program :=
  { blocks := [{ params := [], insts := [], term := .br 1 [] },
               { params := [], insts := [], term := .br 1 [] }], sites := [] }

def emitLoop (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  unless wf loopBody && run loopBody 1000 == .outOfFuel do
    IO.eprintln "loop: not a well-formed diverging body"; return 1
  match LCB.emit loopBody with
  | .error msg => IO.eprintln s!"loop: adapter: {msg.message}"; return 1
  | .ok (f, tbl) =>
    if (LCB.evalBody f 10000).isSome then IO.eprintln "loop: LCB's evaluator returns"; return 1
    unless ← writeBackends dir target loopBody (LLVM.module loopBody) (f, tbl) do
      return lcbProfileStatus (LCB.job f) 1
    writeExpected dir "incomplete" ""
    return 0

/-- One site, and a body returning `256 + 1`: past the table, so the word encodes nothing. -/
def emitInvalid (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let tbl : SiteTable := [Examples.plusSite]
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "llvm.ll")
    ("define i32 @concrete_body() {\nentry:\n  ret i32 257\n}\n\n" ++ Runtime.llvm tbl)
  let f : LeanCompilerBackend.Verified.Source.Function :=
    { resultWidth := .word, parameters := #[], valueWidths := #[],
      blocks := #[{ phis := #[], instructions := #[], terminator := .ret (.literal (LCB.word 257)) }] }
  unless ← writeLcb dir target (f, tbl) do return lcbProfileStatus (LCB.job f) 1
  writeExpected dir "134" "concrete: runtime: invalid result word\n"
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unsupported profile target {name}"; return 17

-- Reorder the two entries and remap operands so the runtime line is unchanged.
private def reorderTrapTable (p : Program) : Program :=
  { p with
    sites := p.sites.reverse
    blocks := p.blocks.map fun block =>
      match block.term with
      | .trap index => { block with term := .trap (1 - index) }
      | _ => block }

def main (args : List String) : IO UInt32 := do
  match args with
  | ["list"] => do
    for (n, _) in Examples.all do IO.println n
    return 0
  | "emit" :: name :: dir :: t :: expectation => withTarget t (emit name dir expectation)
  | "source" :: path :: actual :: dir :: t :: expectation =>
    withTarget t (emitSource path actual dir expectation)
  | ["backend-profile"] => checkBackendProfile
  | ["control-unsupported-lcb-profile"] => do
    let some (_, p) := Examples.all.find? (·.1 == "answer") | return 1
    let .ok (f, _) := LCB.emit p | return 1
    let job := LCB.job f
    if lcbProfileStatus job 15 != 15 then return 1
    else return lcbProfileStatus { job with code := { job.code with version := 0 } } 15
  | "control-reordered-traps" :: name :: dir :: t :: expectation =>
    withTarget t fun target => do
      let some (_, p) := Examples.all.find? (·.1 == name) | return 2
      unless p.sites.length == 2 do return 2
      emitProgram name (reorderTrapTable p) dir expectation target
  | ["control-wrong-compiler-origin", path, actual] => do
    let .ok input := Concrete.Stages.Input.load path (← IO.FS.readBinFile actual)
      | IO.eprintln s!"invalid project path: {path}"; return 1
    let some expected := parseRefusal input.fileId "unreachable_statement" "STMT-14@1" "28:37:1:29" "-"
      | return 2
    checkSourceRefusal input expected (.source (.surface (.unreachable ⟨18, 27, 1, 19, ⟨0⟩⟩)))
  | ["loop", dir, t] => withTarget t (emitLoop dir)
  | ["invalid", dir, t] => withTarget t (emitInvalid dir)
  | _ => IO.eprintln "usage: slice0_bir.lean list | backend-profile | emit NAME DIR TARGET EXPECT... | source PATH FILE DIR TARGET EXPECT... | (loop | invalid) DIR TARGET"; return 2

-- A checked-add continuation must carry the newest write to a reused slot.
private def overwrittenSlot : Concrete.Memory.Program :=
  ⟨[⟨⟨⟨0⟩, Concrete.Memory.u8Layout⟩, .copy (.lit 1)⟩,
    ⟨⟨⟨0⟩, Concrete.Memory.u8Layout⟩, .copy (.lit 2)⟩,
    ⟨⟨⟨1⟩, Concrete.Memory.u8Layout⟩, .add (.lit 1) (.lit 1) ⟨⟨1⟩, ⟨0, 1, 1, 1, ⟨0⟩⟩⟩ none⟩],
    .var ⟨0⟩⟩

example : Concrete.Memory.run overwrittenSlot = .exit 2 := by rfl

example :
    (match Concrete.SSA.lower "a.con" overwrittenSlot with
     | .ok p => Concrete.BIR.run p (p.blocks.length + 1)
     | .error _ => .stuck) = .exited 2 := by cbv
