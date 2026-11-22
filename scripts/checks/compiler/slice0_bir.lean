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
open ConcreteSpec (Trap)

structure Expectation where
  outcome : Outcome
  trapKind : Option Trap
  trapKindName : String := ""

def trapOfName : String → Option Trap
  | "arithmeticOverflow" => some .arithmeticOverflow
  | _ => none

def parseExpectation : List String → Option Expectation
  | ["exit", c] => match c.toNat? with
    | some n => if n ≤ 255 then some ⟨.exited n, none, ""⟩ else none
    | none => none
  | ["trap", kind, "site", s] => match s.toNat? with
    | some n => if !kind.isEmpty then some ⟨.trapped n, trapOfName kind, kind⟩ else none
    | none => none
  | _ => none

def writeExpected (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeFile (dir / "stderr") stderr

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
    IO.eprintln s!"{dir}: LCB first path: {msg}"; return false
  match LCB.assemble target f with
  | .error msg => IO.eprintln s!"{dir}: LCB: {msg}"; return false
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
    | IO.eprintln s!"{name}: invalid BIR expectation; use exit N or trap KIND site N"; return 2
  unless wf p do IO.eprintln s!"{name}: not well formed"; return 1
  let got := run p 1000
  unless got == expected.outcome do
    IO.eprintln s!"{name}: expectation mismatch: BIR run gives {repr got}, expected {repr expected.outcome}"; return 1
  let some word := encode expected.outcome | IO.eprintln s!"{name}: no result word"; return 1
  let (f, tbl) := emitted
  match LCB.evalBody f 100000 with
  | some w =>
    unless w == word do
      IO.eprintln s!"{name}: LCB's evaluator returns {w}, BIR's result word is {word}"; return 1
  | none => IO.eprintln s!"{name}: LCB's evaluator does not return"; return 1
  unless ← writeBackends dir target p llvm (f, tbl) do return 1
  match expected.outcome with
  | .exited c => writeExpected dir (toString c) ""
  | .trapped s => match tbl[s]? with
    | some e =>
      unless expected.trapKind == some e.kind do
        IO.eprintln s!"{name}: expectation mismatch: trap site {s} has kind {repr e.kind}, expected {expected.trapKindName}"; return 1
      writeExpected dir "134" (Runtime.trapLine e ++ "\n")
    | none => IO.eprintln s!"{name}: no site {s}"; return 1
  | _ => IO.eprintln s!"{name}: no observation"; return 1
  return 0

def emitProgram (name : String) (p : Program) (dir : System.FilePath)
    (expectation : List String) (target : LCB.Target) : IO UInt32 := do
  match LCB.emit p with
  | .error msg => IO.eprintln s!"{name}: adapter: {msg}"; return 1
  | .ok emitted => emitArtifact name p (LLVM.module p) emitted dir expectation target

def emit (name : String) (dir : System.FilePath) (expectation : List String)
    (target : LCB.Target) : IO UInt32 := do
  let some (_, p) := Examples.all.find? (·.1 == name)
    | IO.eprintln s!"unknown example {name}"; return 2
  emitProgram name p dir expectation target

def emitSource (path actual : String) (dir : System.FilePath) (expectation : List String)
    (target : LCB.Target) : IO UInt32 := do
  let input : Concrete.Stages.Input := ⟨path, ← IO.FS.readBinFile actual⟩
  match expectation, Concrete.Stages.compile input with
  | ["refuse", rule], .error e =>
    if e.code == rule then
      IO.println s!"ok   {path}: {e.code}: {e.message}"
      return 0
    IO.eprintln s!"{path}: expected refusal {rule}, got {e.code}: {e.message}"
    return 1
  | ["refuse", rule], .ok _ =>
    IO.eprintln s!"{path}: expected refusal {rule}, but compilation succeeded"
    return 1
  | "trap" :: _, .error (.checker _) =>
    let .ok tokens := Concrete.Lex.lex input.source | IO.eprintln "lex failed"; return 1
    let .ok ast := Concrete.Parse.parse tokens | IO.eprintln "parse failed"; return 1
    let .ok core := Concrete.Elab.elaborate ast | IO.eprintln "elaboration failed"; return 1
    let p := Concrete.Core.toCore core.program
    unless Concrete.Core.run p (p.body.length + 1) == .trap .arithmeticOverflow do
      IO.eprintln s!"{path}: Core evaluator did not trap on overflow"; return 1
    match Concrete.Stages.unchecked input with
    | .ok bir => emitProgram path bir dir expectation target
    | .error e => IO.eprintln s!"{path}: unchecked lowering: {e.message}"; return 1
  | "exit" :: _, .ok out =>
    let p := Concrete.Core.toCore out.core.program
    let some expected := parseExpectation expectation | IO.eprintln "invalid source expectation"; return 2
    let .exited n := expected.outcome | IO.eprintln "expected source exit"; return 2
    unless Concrete.Core.run p (p.body.length + 1) == .exit n do
      IO.eprintln s!"{path}: Core evaluator did not exit with {n}"; return 1
    unless Concrete.Elab.eval out.ast == .exit n && Concrete.Memory.run out.memory == .exit n do
      IO.eprintln s!"{path}: AST or memory evaluator did not exit with {n}"; return 1
    unless out.bir.sites.isEmpty do
      IO.eprintln s!"{path}: admitted slice-0 additions retained trap sites"; return 1
    emitArtifact path out.bir out.llvm (out.lcb, out.bir.sites) dir expectation target
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
  | .error msg => IO.eprintln s!"loop: adapter: {msg}"; return 1
  | .ok (f, tbl) =>
    if (LCB.evalBody f 10000).isSome then IO.eprintln "loop: LCB's evaluator returns"; return 1
    unless ← writeBackends dir target loopBody (LLVM.module loopBody) (f, tbl) do return 1
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
  unless ← writeLcb dir target (f, tbl) do return 1
  writeExpected dir "134" (Runtime.invalidLine ++ "\n")
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unknown target {name}"; return 2

def main (args : List String) : IO UInt32 := do
  match args with
  | ["list"] => do
    for (n, _) in Examples.all do IO.println n
    return 0
  | "emit" :: name :: dir :: t :: expectation => withTarget t (emit name dir expectation)
  | "source" :: path :: actual :: dir :: t :: expectation =>
    withTarget t (emitSource path actual dir expectation)
  | ["loop", dir, t] => withTarget t (emitLoop dir)
  | ["invalid", dir, t] => withTarget t (emitInvalid dir)
  | _ => IO.eprintln "usage: slice0_bir.lean list | emit NAME DIR TARGET EXPECT... | source PATH FILE DIR TARGET EXPECT... | (loop | invalid) DIR TARGET"; return 2
