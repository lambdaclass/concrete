import Concrete

/-!
Emitter for the slice-0 backend gate (`scripts/checks/slice0_backends.sh`). PROVISIONAL.

`lake env lean --run scripts/checks/slice0_bir.lean list` prints the hand-written BIR examples;
`... emit NAME DIR TARGET` checks the example in Lean (well formed; its BIR run gives the expected
outcome; LCB's own evaluator on the adapted body returns the same result word) and writes
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

def writeExpected (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeFile (dir / "stderr") stderr

/-- Write the LCB leg: the body assembled in process, the runtime entry as QBE text. -/
def writeLcb (dir : System.FilePath) (target : LCB.Target) (sites : SiteTable)
    (f : LeanCompilerBackend.Verified.Source.Function) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job : LeanCompilerBackend.Verified.FunctionJob :=
    { code := { symbol := LCB.bodySymbol, body := f,
                abi := LeanCompilerBackend.Verified.planAbi f,
                layout := LeanCompilerBackend.Verified.planFrame f } }
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

def writeBackends (dir : System.FilePath) (target : LCB.Target) (p : Program)
    (f : LeanCompilerBackend.Verified.Source.Function) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
  writeLcb dir target p.sites f

def emit (name : String) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let some (_, p, expected) := Examples.all.find? (·.1 == name)
    | IO.eprintln s!"unknown example {name}"; return 2
  unless wf p do IO.eprintln s!"{name}: not well formed"; return 1
  let got := run p 1000
  unless got == expected do
    IO.eprintln s!"{name}: BIR run gives {repr got}, expected {repr expected}"; return 1
  let some word := encode expected | IO.eprintln s!"{name}: no result word"; return 1
  match LCB.adapt p with
  | .error msg => IO.eprintln s!"{name}: adapter: {msg}"; return 1
  | .ok f =>
    match LCB.evalBody f 100000 with
    | some w =>
      unless w == word do
        IO.eprintln s!"{name}: LCB's evaluator returns {w}, BIR's result word is {word}"; return 1
    | none => IO.eprintln s!"{name}: LCB's evaluator does not return"; return 1
    unless ← writeBackends dir target p f do return 1
    match expected with
    | .exited c => writeExpected dir (toString c) ""
    | .trapped s => match p.sites[s]? with
      | some e => writeExpected dir "134" (Runtime.trapLine e ++ "\n")
      | none => IO.eprintln s!"{name}: no site {s}"; return 1
    | _ => IO.eprintln s!"{name}: no observation"; return 1
    return 0

/-- A body that branches back to itself forever. -/
def loopBody : Program :=
  { blocks := [{ params := [], insts := [], term := .br 1 [] },
               { params := [], insts := [], term := .br 1 [] }], sites := [] }

def emitLoop (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  unless wf loopBody && run loopBody 1000 == .outOfFuel do
    IO.eprintln "loop: not a well-formed diverging body"; return 1
  match LCB.adapt loopBody with
  | .error msg => IO.eprintln s!"loop: adapter: {msg}"; return 1
  | .ok f =>
    if (LCB.evalBody f 10000).isSome then IO.eprintln "loop: LCB's evaluator returns"; return 1
    unless ← writeBackends dir target loopBody f do return 1
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
  unless ← writeLcb dir target tbl f do return 1
  writeExpected dir "134" (Runtime.invalidLine ++ "\n")
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unknown target {name}"; return 2

def main (args : List String) : IO UInt32 := do
  match args with
  | ["list"] => do
    for (n, _, _) in Examples.all do IO.println n
    return 0
  | ["emit", name, dir, t] => withTarget t (emit name dir)
  | ["loop", dir, t] => withTarget t (emitLoop dir)
  | ["invalid", dir, t] => withTarget t (emitInvalid dir)
  | _ => IO.eprintln "usage: slice0_bir.lean list | (emit NAME | loop | invalid) DIR TARGET"; return 2
